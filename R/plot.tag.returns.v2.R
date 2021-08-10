# Tagging plots

#' Plot the observed and predicted tag recaptures.
#' 
#' Plot the observed and predicted tag recaptures by tag recapture group.
#' Tags caught during the mixing period are not counted.
#' The plot is either a time series of the difference between the observed and predicted, or a time series of the recaptures.
#' A loess smoother is put through the differences.
#' @param tagrelease.list A list, or an individual data.frame, of tag release data created by the \code{tag.release.data.preparation()} function.
#' @param tagrep.list A list, or an individual data.frame, of tag returns from the temporary_tag_returns file. The output from the FLR4MFCL function \code{read.temporary_tag_report()}.
#' @param fishery.map A data.frame that describes which fishery is fishing in which region and with which gear. The columns are: fishery_name, region, gear, fishery (the reference number), tag_recapture_group and tag_recapture_name. The map should contain entries for all fisheries to avoid data being inadvertently dropped.
#' @param names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the tagrelease.list (if available) or generated automatically.
#' @param recapture.groups A vector of the reference numbers of the tag recapture groups you want to plot.
#' @param plot.diff Do you want to plot the difference between the observed and predicted, or a time series of recaptures? TRUE (default) or FALSE.
#' @param scale.diff If TRUE, the difference between observed and predicted is scaled by the mean of observed returns.
#' @param show.legend Do you want to show the plot legend, TRUE (default) or FALSE.
#' @param show.points Do you want to show points as well as the smoother for the difference plots? Default is FALSE.
#' @param palette.func A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @param ... Passes extra arguments to the palette function. Use the argument all.model.names to ensure consistency of model colours when plotting a subset of models.
#' @export
#' @rdname plot.tag.returns.time.v2
#' @name Plot tag returns time
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom data.table data.table
#' @importFrom data.table rbindlist
#' @importFrom ggthemes theme_few
#' @importFrom ggplot2 geom_blank
# 
# 
# Without tagdat.list but with individual elements
plot.tag.returns.time.v2 <- function(tagrelease.list, tagrep.list, names=NULL, fishery.map, recapture.groups, plot.diff=TRUE, scale.diff=TRUE, show.legend=TRUE, show.points=FALSE, palette.func=default.model.colours, save.dir, save.name, ...){
  
  # Argument checking etc.
  tagrelease.list <- check.tagrelease.args(tagrelease.list, names) 
  tagrep.list <- check.tagrep.args(tagrep.list, names) 
  
  # If plotting time series of actuals, can only plot one model at a time
  if(plot.diff == FALSE & length(tagrelease.list) != 1){
    stop("If plotting actual observed and predicted attrition of (not the difference between them) you can only plot one model at a time. Try subsetting your tagrelease list.")
  }
  
  # Two stages with two functions:
  # Prepare the data
  pdat <- prepare.tag.returns.time.v2(tagrelease.list = tagrelease.list, tagrep.list = tagrep.list, names=names, fishery.map = fishery.map)
  
  # Generate plot
  p <- generate.plot.tag.returns.time.v2(pdat=pdat, model_names=names(tagrelease.list), recapture.groups=recapture.groups, plot.diff=plot.diff, scale.diff=scale.diff, show.legend=show.legend, show.points=show.points, palette.func=palette.func, save.dir=save.dir, save.name=save.name, ...)
  
  return(p)
  
}

#' @export
#' @rdname plot.tag.returns.time.v2
#' @name Plot tag returns time
prepare.tag.returns.time.v2 <- function(tagrelease.list, tagrep.list, names=NULL, fishery.map, plot.diff=TRUE, scale.diff=TRUE, show.legend=TRUE, show.points=FALSE, palette.func=default.model.colours, save.dir, save.name, ...){
  
  tagrelease.list <- check.tagrelease.args(tagrelease.list, names) 
  tagrep.list <- check.tagrep.args(tagrep.list, names) 
  
  # Collapse recaptures into a single data.table
  tagrep <- data.table::rbindlist(tagrep.list, idcol="Model")
  # Bring in tag_recapture_group and name
  colnames(fishery.map)[colnames(fishery.map)=="fishery"] <- "recap.fishery"
  tagrep <- merge(tagrep, fishery.map[,c("recap.fishery", "tag_recapture_group", "tag_recapture_name")])
  # Drop recapture groups we don't want - do in the engine plot
  # tagrep <- tagrep[tag_recapture_group %in% recapture.groups,]
  # Make a timestep column
  tagrep$recap.ts <- tagrep$recap.year + (tagrep$recap.month-1)/12 + 1/24
  # Drop columns we don't need for space
  tagrep[,c("recap.year", "recap.month", "recap.fishery"):=NULL]
  
  # Get releases
  tagrel <- data.table::rbindlist(tagrelease.list, idcol="Model")
  tagrel$rel.ts.after.mix <- tagrel$rel.ts + tagrel$mixing_period_years
  # Bring mixing period into recapture
  tagrep <- merge(tagrep, tagrel[,c("Model", "rel.group", "rel.ts.after.mix")])
  # Drop observations that are within the mixing period  
  tagrep <- tagrep[!(recap.ts < rel.ts.after.mix),]
  
  # Summarise returns by recapture group
  pdat <- tagrep[,.(recap.pred = sum(recap.pred, na.rm=TRUE), recap.obs = sum(recap.obs, na.rm=TRUE)), by=.(tag_recapture_group, tag_recapture_name, recap.ts, Model)]  
  
  # To ensure plotting is OK we need each fishery to have a full complement of time series
  # observations, even if NA.
  # This is a right pain in the bum - must be an easier way
  # Need to pad out time series
  no_seasons <- length(unique(tagrelease.list[[1]]$rel.month)) # unsafe, how best to get no seasons?
  padts <- expand.grid(recap.ts = seq(from=min(pdat$recap.ts), to=max(pdat$recap.ts), by= 1 / no_seasons), tag_recapture_name = sort(unique(pdat$tag_recapture_name)), Model=sort(unique(pdat$Model)))
  # Bring in recapture name and group
  padts <- merge(padts, fishery.map[c("tag_recapture_group", "tag_recapture_name")])
  pdat <- merge(pdat, padts, all=TRUE, by=colnames(padts))
  
  return(pdat)
}
  
  
#' @param pdat The output from calling prepare.plot.tag.returns.time.v2
#' @param model_names Names of the models to be plotted and ordered by.
#' @export
#' @rdname plot.tag.returns.time.v2
#' @name Plot tag returns time
generate.plot.tag.returns.time.v2 <- function(pdat, model_names=unique(pdat$Model), recapture.groups, plot.diff=TRUE, scale.diff=TRUE, show.legend=TRUE, show.points=FALSE, palette.func=default.model.colours, save.dir, save.name, ...){
  
  # If not plotting the difference don't scale it
  if(plot.diff == FALSE){
    scale.diff <- FALSE
  }
  
  # If plotting time series of actuals, can only plot one model at a time
  if(plot.diff == FALSE & length(model_names) != 1){
    stop("If plotting actual observed and predicted attrition of (not the difference between them) you can only plot one model at a time. Try subsetting your tagrelease list.")
  }
  
  # Subset recapture.groups and models
  pdat <- pdat[(tag_recapture_group %in% recapture.groups) & (Model %in% model_names),]
  
  # Want pdat to have Model names in a specific order - important for plotting order
  pdat[,Model:=factor(Model, levels=model_names)]
  
  # Mathew's plot. Time series of predicted and observed
  if(plot.diff == FALSE){
    # For the observed and predicted recaptures, NA is essentially 0,
    # i.e. there were no recaptures, so set to 0
    pdat[is.na(pdat$recap.pred), "recap.pred"] <- 0
    pdat[is.na(pdat$recap.obs), "recap.obs"] <- 0
    p <- ggplot2::ggplot(pdat, ggplot2::aes(x=recap.ts, y=recap.obs))
    p <- p + ggplot2::geom_point(colour="red", na.rm=TRUE)
    p <- p + ggplot2::geom_line(ggplot2::aes(y=recap.pred), na.rm=TRUE)
    p <- p + ggplot2::facet_wrap(~tag_recapture_name, scales="free")
    p <- p + ggplot2::xlab("Time") + ggplot2::ylab("Tag recaptures")
    p <- p + ggthemes::theme_few()
  }
  
  
  if(plot.diff == TRUE){
    colour_values <- palette.func(selected.model.names = model_names, ...)
    # Or plot the difference - need to scale by number of recaptures?
    pdat$diff <- pdat$recap.obs - pdat$recap.pred
    ylab <- "Observed - predicted recaptures"
    # Normalise
    if(scale.diff == TRUE){
      # Rescale the diffs by the mean recaptures in that group
      pdat <- pdat[,.(Model=Model, recap.ts=recap.ts, diff = diff / mean(recap.obs, na.rm=TRUE)), by=.(tag_recapture_name)]
      ylab <- "Obs. - pred. recaptures (scaled)"
    }
    # Spoof up approriate y ranges for each facet using geom_blank()
    dummydat <- pdat[,.(y = c(max(abs(diff), na.rm=T), -max(abs(diff), na.rm=T))), by=.(tag_recapture_name)]
    dummydat$x <- rep(c(min(pdat$recap.ts), max(pdat$recap.ts)), nrow(dummydat)/2)
    
    p <- ggplot2::ggplot(pdat, ggplot2::aes(x=recap.ts, y=diff))
    # If showing points, also make y axes symmetrical
    # If you want symetrical points with just the smoother look at
    # https://stackoverflow.com/questions/9789871/method-to-extract-stat-smooth-line-fit 
    if(show.points==TRUE){
      p <- p + ggplot2::geom_point(aes(colour=Model), na.rm=TRUE)
      p <- p + ggplot2::geom_blank(data=dummydat, aes(x=x, y=y))
    }
    p <- p + ggplot2::geom_smooth(aes(colour=Model), method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
    p <- p + ggplot2::scale_color_manual("Model",values=colour_values)
    p <- p + ggplot2::facet_wrap(~tag_recapture_name, scales="free")
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0.0), linetype=2)
    p <- p + ggthemes::theme_few()
    p <- p + ggplot2::xlab("Time") + ggplot2::ylab(ylab)
    if (show.legend==FALSE){
      p <- p + theme(legend.position="none") 
    }
  }
  
  save_plot(save.dir, save.name, plot=p)
  
  return(p)
}





