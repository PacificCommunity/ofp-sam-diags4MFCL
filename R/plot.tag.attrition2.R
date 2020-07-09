# Attrition rate by tagging program
# Similar to Figs 28 and 29 in SKJ

#' Plot the observed and predicted tag attrition.
#' 
#' Plot the observed and predicted tag recaptures against time at liberty by tagging program, or all tagging programs combined.
#' The plot is either a time series of the difference between the observed and predicted, or a time series of the recaptures.
#' A loess smoother is put through the differences.
#' @param tagrelease.list A list, or an individual data.frame, of tag release data created by the \code{tag.release.data.preparation()} function.
#' @param tagrep.list A list, or an individual data.frame, of tag returns from the temporary_tag_returns file. The output from the FLR4MFCL function \code{read.temporary_tag_report()}.
#' @param names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the tagrelease.list (if available) or generated automatically.
#' @param facet What variable fo you want to group by: "none" (no grouping), "program" (by tagging program - default), "region" (by recapture region).
#' @param plot.diff Do you want to plot the difference between the observed and predicted, or a time series of recaptures? TRUE (default) or FALSE.
#' @param scale.diff If TRUE, the difference between observed and predicted is scaled by the mean number of observed returns.
#' @param show.legend Do you want to show the plot legend, TRUE (default) or FALSE.
#' @param show.points Do you want to show points as well as the smoother for the difference plots? Default is FALSE.
#' @param palette.func A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @param ... Passes extra arguments to the palette function. Use the argument all.model.names to ensure consistency of model colours when plotting a subset of models.
#' @export
#' @rdname plot.tag.attrition.v2
#' @name Plot tag attrition
#' @import FLR4MFCL
#' @import magrittr
plot.tag.attrition.v2 <- function(tagrelease.list, tagrep.list, fishery.map, names=NULL, facet="program", plot.diff=TRUE, scale.diff=TRUE, show.legend=TRUE, show.points=FALSE, palette.func=default.model.colours, save.dir, save.name, ...){
  
  # Check names and lists etc
  tagrelease.list <- check.tagrelease.args(tagrelease.list, names) 
  tagrep.list <- check.tagrep.args(tagrep.list, names) 
  
  # If plotting time series of actuals, can only plot one model at a time
  if(plot.diff == FALSE & length(tagrelease.list) != 1){
    stop("If plotting actual observed and predicted attrition of (not the difference between them) you can only plot one model at a time.")
  }
  
  # Check facet arguments
  if (!(facet %in% c("none", "program", "region"))){
    stop("facet argument must be 'none', 'program' or 'region'")
  }
  
 pdat <- prepare.tag.attrition.v2(tagrelease.list=tagrelease.list, tagrep.list=tagrep.list, names=names, fishery.map=fishery.map)
  p <- generate.plot.tag.attrition.v2(pdat=pdat, facet=facet, model_names=names(tagrelease.list), plot.diff=plot.diff, scale.diff=scale.diff, show.legend=show.legend, show.points=show.points, palette.func=palette.func, save.dir=save.dir, save.name=save.name, ...)
  return(p)
}

#' @export
#' @rdname plot.tag.attrition.v2
#' @name Plot tag attrition
prepare.tag.attrition.v2 <- function(tagrelease.list, tagrep.list, names=NULL, fishery.map){
  # Check names and lists etc
  tagrelease.list <- check.tagrelease.args(tagrelease.list, names) 
  tagrep.list <- check.tagrep.args(tagrep.list, names) 
  
  # Collapse into a single data.table
  tagrep <- data.table::rbindlist(tagrep.list, idcol="Model")
  # Bring recapture region
  colnames(fishery.map)[colnames(fishery.map)=="fishery"] <- "recap.fishery"
  tagrep <- merge(tagrep, fishery.map[,c("recap.fishery", "region")])
  tagrep$recap.ts <- tagrep$recap.year + (tagrep$recap.month-1)/12 + 1/24
  # Drop columns we don't need for space
  tagrep[,c("recap.year", "recap.month", "recap.fishery"):=NULL]
  # Sort out releases  
  tagrel <- data.table::rbindlist(tagrelease.list, idcol="Model")
  # Bring in tagging program, and rel.ts
  tagrep <- merge(tagrep, tagrel[,c("Model", "rel.group","program", "rel.ts")])
  # Add period at liberty
  no_seasons <- length(unique(tagrel$rel.month)) # this is unsafe, how best to get no seasons?
  tagrep$period_at_liberty <- ((tagrep$recap.ts - tagrep$rel.ts) * no_seasons) + 1
  
  # Attrition loses the time step - so group over all possible facets and sum
  grouping_names <- c("Model", "period_at_liberty", "region","program")
  pdat <- tagrep[, .(recap.obs=sum(recap.obs, na.rm=TRUE), recap.pred=sum(recap.pred, na.rm=TRUE)) ,by=mget(grouping_names)]
  
  return(pdat)
}
  
  
#' @param pdat The output from calling prepare.plot.tag.returns.time.v2
#' @param model_names Names of the models to be plotted and ordered by.
#' @export  
#' @rdname plot.tag.attrition.v2
#' @name Plot tag attrition
generate.plot.tag.attrition.v2 <- function(pdat, facet="program", model_names=unique(pdat$Model), plot.diff=TRUE, scale.diff=TRUE, show.legend=TRUE, show.points=FALSE, palette.func=default.model.colours, save.dir, save.name, ...){
  
  # If not plotting the difference don't scale it
  if(plot.diff == FALSE){
    scale.diff <- FALSE
  }
  
  # Subset models
  pdat <- pdat[Model %in% model_names,]
  
  # If plotting time series of actuals, can only plot one model at a time
  if(plot.diff == FALSE & length(model_names) != 1){
    stop("If plotting actual observed and predicted attrition of (not the difference between them) you can only plot one model at a time.")
  }
  
  # Y lab for the difference plot without scaling - overwritten if scaled
  ylab <- "Observed - predicted recaptures"
  
  # Sum number of tags by period at liberty
  # Depends if we want to process by tag program
  if (facet=="none"){
    grouping_names <- c("Model", "period_at_liberty")
  }
  if (facet=="program"){
    grouping_names <- c("Model", "period_at_liberty", "program")
  }
  if (facet=="region"){
    grouping_names <- c("Model", "period_at_liberty", "region")
  }
  
  # Do some grouping again
  pdat <- pdat[, .(recap.obs=sum(recap.obs, na.rm=TRUE), recap.pred=sum(recap.pred, na.rm=TRUE)) ,by=mget(grouping_names)]
  pdat$diff <- pdat$recap.obs - pdat$recap.pred
  if(scale.diff == TRUE){
    # Don't group by period at liberty - keep other choices
    grouping_names <- grouping_names[grouping_names!="period_at_liberty"] 
    mean_recaptured <- pdat[,.(mean_obs_recap=mean(recap.obs, na.rm=TRUE)), by=mget(grouping_names)]
    pdat <- merge(pdat, mean_recaptured)
    pdat$diff <- pdat$diff / pdat$mean_obs_recap
    ylab <- "Obs. - pred. recaptures (scaled)"
    }
  if (facet %in% c("none", "region")){
    pdat$program <- "All programs"
  }
  if (facet %in% c("none", "program")){
    pdat$region <- "All recapture regions"
  }
  
  # Need to pad out time series to avoid missing missing periods at liberty
  padts <- expand.grid(period_at_liberty = seq(from=min(pdat$period_at_liberty), to=max(pdat$period_at_liberty), by= 1), program = sort(unique(pdat$program)), region = sort(unique(pdat$region)))
  pdat <- merge(pdat, padts, by=colnames(padts), all=TRUE)
  
  # Want pdat to have Model names in the original order - important for plotting order
  pdat[,Model:=factor(Model, levels=model_names)]
  
  # Time series
  if(plot.diff == FALSE){
    # For the observed and predicted recaptures, NA is essentially 0,
    # i.e. there were no recaptures, so set to 0
    pdat[is.na(pdat$recap.pred), "recap.pred"] <- 0
    pdat[is.na(pdat$recap.obs), "recap.obs"] <- 0
    p <- ggplot2::ggplot(pdat, ggplot2::aes(x=period_at_liberty))
    p <- p + ggplot2::geom_point(ggplot2::aes(y=recap.obs), colour="red")
    p <- p + ggplot2::geom_line(ggplot2::aes(y=recap.pred))
    if(facet=="program"){
      p <- p + ggplot2::facet_wrap(~program, scales="free")
    }
    if(facet=="region"){
      p <- p + ggplot2::facet_wrap(~region, scales="free")
    }
    p <- p + ggplot2::xlab("Periods at liberty (quarters)")
    p <- p + ggplot2::ylab("Number of tag returns")
    p <- p + ggplot2::ylim(c(0,NA))
    p <- p + ggthemes::theme_few()
  }
  
  # Residuals - can by multiple models
  if(plot.diff == TRUE){
    colour_values <- palette.func(selected.model.names = model_names, ...)
    # Get dummy data to set nice looking symmetrical ylims
    # Drop Model from grouping_name
    grouping_names <- grouping_names[grouping_names!="Model"] 
    dummydat <- pdat[,.(y = c(max(abs(diff), na.rm=T), -max(abs(diff), na.rm=T))), mget(grouping_names)]
    dummydat$x <- rep(c(min(pdat$period_at_liberty), max(pdat$period_at_liberty)), nrow(dummydat)/2)
    # Start...
    p <- ggplot2::ggplot(pdat, aes(x=period_at_liberty, y=diff))
    # If only 1 model, draw points and turn off legend
    # Ensure symetrical y-axes only works when not showing points
    # https://stackoverflow.com/questions/9789871/method-to-extract-stat-smooth-line-fit 
    if (show.points==TRUE){
      p <- p + ggplot2::geom_point(aes(colour=Model))
      p <- p + ggplot2::geom_blank(data=dummydat, aes(x=x, y=y))
    }
    p <- p + ggplot2::geom_smooth(aes(colour=Model), method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
    p <- p + ggplot2::scale_color_manual("Model",values=colour_values)
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0.0), linetype=2)
    if(facet=="program"){
      p <- p + ggplot2::facet_wrap(~program, scales="free")
    }
    if(facet=="region"){
      p <- p + ggplot2::facet_wrap(~region, scales="free")
    }
    p <- p + ggplot2::ylab(ylab)
    p <- p + ggplot2::xlab("Periods at liberty (quarters)")
    p <- p + ggthemes::theme_few()
    if (show.legend==FALSE){
      p <- p + theme(legend.position="none") 
    }
  }
  
  save_plot(save.dir, save.name, plot=p)
  
  return(p)
}
