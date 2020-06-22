# Tagging plots

#' Plot the observed and predicted tag recaptures.
#' 
#' Plot the observed and predicted tag recaptures by tag recapture group.
#' Tags caught during the mixing period are not counted.
#' The plot is either a time series of the difference between the observed and predicted, or a time series of the recaptures.
#' A loess smoother is put through the differences.
#' @param tagdat Tagging data created by the \code{tag.data.preparation()} function.
#' @param recapture.groups A vector of the reference numbers of the tag recapture groups you want to plot.
#' @param plot.diff Do you want to plot the difference between the observed and predicted, or a time series of recaptures? TRUE (default) or FALSE.
#' @param scale.diff If TRUE, the difference between observed and predicted is scaled by the number of observed returns.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom data.table data.table
#' @importFrom data.table rbindlist
#' @importFrom ggthemes theme_few
#' @importFrom ggplot2 geom_blank
plot.tag.returns.time <- function(tagdat, recapture.groups, plot.diff=TRUE, scale.diff=TRUE, save.dir, save.name){
  
  # ID observations that are within the mixing period  
  mixing_rows <- tagdat$recap.ts < (tagdat$rel.ts + tagdat$mixing_period_quarters) 
  # And drop them
  tagdat <- tagdat[!mixing_rows,]
  
  # Try to subset the recapture groups beforehand but we ended making a mess
  # when trying to pad timeseries
  
  # Prepare data for plotting
  # Sum predicted and observed recaptures in each timestep for each tag recapture group
  pdat <- aggregate(list(recap.pred = tagdat$recap.pred, recap.obs=tagdat$recap.obs),
                    list(tag_recapture_group = tagdat$tag_recapture_group,
                         tag_recapture_name = tagdat$tag_recapture_name,
                         recap.ts = tagdat$recap.ts), sum, na.rm=TRUE)
  # Reorder so it's helpful for debugging
  #pdat <- pdat[order(pdat$tag_recapture_group, pdat$recap.ts),]
  
  # Subset the recapture groups - do earlier to save time?
  pdat <- subset(pdat, tag_recapture_group %in% recapture.groups)
  
  # To ensure plotting is OK we need each fishery to have a full complement of time series
  # observations, even if NA.
  # This is a right pain in the bum - must be an easier way
  # Need to pad out time series
  no_seasons <- length(unique(tagdat$recap.month)) # this is unsafe, how best to get no seasons?
  padts <- expand.grid(recap.ts = seq(from=min(pdat$recap.ts), to=max(pdat$recap.ts), by= 1 / no_seasons), tag_recapture_group = sort(unique(pdat$tag_recapture_group)))
  # Bring in the recapture group name
  padts <- merge(padts, unique(pdat[,c("tag_recapture_group","tag_recapture_name")]))
  pdat <- merge(pdat, padts, all=TRUE)
  
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
    # Or plot the difference - need to scale by number of recaptures?
    pdat$diff <- pdat$recap.obs - pdat$recap.pred
    # Alt. - problem with obs = 0, leads to infinite difference
    # pdat$diff <- log(pdat$recap.pred / pdat$recap.obs)
    # Normalise so maximum diff = 1
    # Get total recaptured observed
    ylab <- "Observed - predicted recaptures"
    # Normalise
    if(scale.diff == TRUE){
      total_recaptured <- aggregate(list(total_obs_recap=pdat$recap.obs), list(tag_recapture_group = pdat$tag_recapture_group), sum, na.rm=TRUE)
      pdat <- merge(pdat, total_recaptured)
      pdat$diff <- pdat$diff / pdat$total_obs_recap
      ylab <- "Obs. - pred. recaptures (scaled)"
    }
    # Spoof up approriate y ranges for each facet using geom_blank()
    no_grps <- length(unique(pdat$tag_recapture_name))
    ylims <- tapply(pdat$diff, pdat$tag_recapture_name, function(x) c(max(abs(x), na.rm=T), -max(abs(x), na.rm=T)))
    dummydat <- data.frame(y=unlist(ylims), x=rep(c(min(pdat$recap.ts), max(pdat$recap.ts)), no_grps), tag_recapture_name = rep(names(ylims), each=2))
    p <- ggplot2::ggplot(pdat, ggplot2::aes(x=recap.ts, y=diff))
    p <- p + ggplot2::geom_point(na.rm=TRUE)
    p <- p + ggplot2::geom_smooth(method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
    p <- p + ggplot2::facet_wrap(~tag_recapture_name, scales="free")
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0.0), linetype=2)
    p <- p + ggthemes::theme_few()
    p <- p + ggplot2::xlab("Time") + ggplot2::ylab(ylab)
    p <- p + ggplot2::geom_blank(data=dummydat, aes(x=x, y=y))
  }
  
  save_plot(save.dir, save.name, plot=p)
  
  return(p)
}

## Similar to Figs 28 and 29 in SKJ
## Attrition rate by tagging program
#' Plot the observed and predicted tag attrition.
#' 
#' Plot the observed and predicted tag recaptures against time at liberty by tagging program, or all tagging programs combined.
#' The plot is either a time series of the difference between the observed and predicted, or a time series of the recaptures.
#' A loess smoother is put through the differences.
#' @param tagdat.list A list, or an individual data.frame, of tagging data created by the \code{tag.data.preparation()} function.
#' @param tagdat.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the tagdat.list (if available) or generated automatically.
#' @param facet.program Do you want to process and plot by tagging program, or combine the tagging programs. TRUE (default) or FALSE.
#' @param plot.diff Do you want to plot the difference between the observed and predicted, or a time series of recaptures? TRUE (default) or FALSE.
#' @param scale.diff If TRUE, the difference between observed and predicted is scaled by the number of observed returns.
#' @param show.legend Do you want to show the plot legend, TRUE (default) or FALSE.
#' @param palette.func A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @param ... Passes extra arguments to the palette function. Use the argument all.model.names to ensure consistency of model colours when plotting a subset of models.
#' @export
#' @import FLR4MFCL
#' @import magrittr
plot.tag.attrition <- function(tagdat.list, tagdat.names=NULL, facet.program=TRUE, plot.diff=TRUE, scale.diff=TRUE, show.legend=TRUE, palette.func=default.model.colours, save.dir, save.name, ...){
  
  # If not plotting the difference don't scale it
  if(plot.diff == FALSE){
    scale.diff <- FALSE
  }
  
  # Sort out the list of inputs
  tagdat.list <- check.tagdat.args(tagdat.list, tagdat.names) 
  # If plotting time series of actuals, can only plot one model at a time
  if(plot.diff == FALSE & length(tagdat.list) != 1){
    stop("If plotting actual observed and predicted attrition of (not the difference between them) you can only plot one model at a time. Try subsetting your tagdat list.")
  }
  # Collapse into a single data.frame
  tagdat <- as.data.frame(data.table::rbindlist(tagdat.list, idcol="Model"))
  #tagdat <- data.table::rbindlist(tagdat.list, idcol="Model")
  
  #browser()
  
  # Y lab for the difference plot without scaling - overwritten if scaled
  ylab <- "Observed - predicted recaptures"
  # Sum number of tags by period at liberty
  # Depends if we want to process by tag program
  
  
  if (facet.program==FALSE){
    #grouping_names <- c("Model", "period_at_liberty")
    
    pdat <- aggregate(list(recap.obs = tagdat$recap.obs, recap.pred = tagdat$recap.pred),
                 list(Model= tagdat$Model, period_at_liberty = tagdat$period_at_liberty),
                 sum, na.rm=TRUE)
    
    
    #pdat <- tagdat[, .(recap.obs=sum(recap.obs, na.rm=TRUE), recap.pred=sum(recap.pred, na.rm=TRUE)) ,by=.(Model, period_at_liberty)]
    #pdat <- tagdat[, .(recap.obs=sum(recap.obs, na.rm=TRUE), recap.pred=sum(recap.pred, na.rm=TRUE)) ,by=mget(grouping_names)]
    
    
    pdat$diff <- pdat$recap.obs - pdat$recap.pred
    if(scale.diff == TRUE){
      total_recaptured <- aggregate(list(total_obs_recap=pdat$recap.obs), list(Model=pdat$Model), sum, na.rm=TRUE)
      pdat <- merge(pdat, total_recaptured)
      pdat$diff <- pdat$diff / sum(pdat$recap.obs, na.rm=TRUE)
      ylab <- "Obs. - pred. recaptures (scaled)"
    }
  # Need to pad out time series
    padts <- expand.grid(period_at_liberty = seq(from=min(pdat$period_at_liberty), to=max(pdat$period_at_liberty), by= 1))
    pdat <- merge(pdat, padts, all=TRUE)
    pdat$program <- "All programs" # Dropped later, but needed for convenience
  }
  
  # Same again but keep the program info
  if (facet.program==TRUE){
    pdat <- aggregate(list(recap.obs = tagdat$recap.obs, recap.pred = tagdat$recap.pred),
                 list(Model= tagdat$Model, period_at_liberty = tagdat$period_at_liberty, program=tagdat$program),
                 sum, na.rm=TRUE)
    pdat$diff <- pdat$recap.obs - pdat$recap.pred
    # Scale by number of tags in each program if needed
    if(scale.diff == TRUE){
      total_recaptured <- aggregate(list(total_obs_recap=pdat$recap.obs), list(Model=pdat$Model, program=pdat$program), sum, na.rm=TRUE)
      pdat <- merge(pdat, total_recaptured)
      pdat$diff <- pdat$diff / sum(pdat$recap.obs, na.rm=TRUE)
      ylab <- "Obs. - pred. recaptures (scaled)"
    }
    # Need to pad out time series
    padts <- expand.grid(period_at_liberty = seq(from=min(pdat$period_at_liberty), to=max(pdat$period_at_liberty), by= 1), program = sort(unique(pdat$program)))
    pdat <- merge(pdat, padts, all=TRUE)
  }
  
  # Time series
  if(plot.diff == FALSE){
    # For the observed and predicted recaptures, NA is essentially 0,
    # i.e. there were no recaptures, so set to 0
    pdat[is.na(pdat$recap.pred), "recap.pred"] <- 0
    pdat[is.na(pdat$recap.obs), "recap.obs"] <- 0
    p <- ggplot2::ggplot(pdat, ggplot2::aes(x=period_at_liberty))
    p <- p + ggplot2::geom_point(ggplot2::aes(y=recap.obs), colour="red")
    p <- p + ggplot2::geom_line(ggplot2::aes(y=recap.pred))
    p <- p + ggplot2::facet_wrap(~program, scales="free")
    p <- p + ggplot2::xlab("Periods at liberty (quarters)")
    p <- p + ggplot2::ylab("Number of tag returns")
    p <- p + ggplot2::ylim(c(0,NA))
    p <- p + ggthemes::theme_few()
  }
  
  # Residuals
  if(plot.diff == TRUE){
    colour_values <- palette.func(selected.model.names = names(tagdat.list), ...)
    # Get dummy data to set nice looking ylims
    no_progs <- length(unique(pdat$program))
    ylims <- tapply(pdat$diff, pdat$program, function(x) c(max(abs(x), na.rm=T), -max(abs(x), na.rm=T)))
    dummydat <- data.frame(y=unlist(ylims), x=rep(c(min(pdat$period_at_liberty), max(pdat$period_at_liberty)), no_progs), program = rep(names(ylims), each=2))
    p <- ggplot2::ggplot(pdat, aes(x=period_at_liberty, y=diff))
    # If only 1 model, draw points and turn off legend
    if (length(tagdat.list)==1){
      p <- p + ggplot2::geom_point()
    }
    p <- p + ggplot2::geom_smooth(aes(colour=Model), method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
    p <- p + ggplot2::scale_color_manual("Model",values=colour_values)
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0.0), linetype=2)
    p <- p + ggplot2::facet_wrap(~program, scales="free")
    p <- p + ggplot2::ylab(ylab)
    p <- p + ggplot2::xlab("Periods at liberty (quarters)")
    p <- p + ggthemes::theme_few()
    p <- p + ggplot2::geom_blank(data=dummydat, aes(x=x, y=y))
    if (show.legend==FALSE){
      p <- p + theme(legend.position="none") 
    }
    
  }
  
  save_plot(save.dir, save.name, plot=p)
  
  return(p)
}





#' Plot proportions of tag returns by region
#' 
#' Plot the difference between the predicted and observed proportions of tag returns by region.
#' Experimental at the moment.
#' @param tagdat An data.frame of tagging data created by the \code{tag.data.preparation()} function.
#' @param plot.type What type of plot: 'point' (default) or 'bar'. 
#' @param save.dir Path to the directory where the outputs will be saved.
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory.
#' @export
#' @import FLR4MFCL
#' @import magrittr
plot.tag.return.proportion <- function(tagdat, plot.type="point", save.dir, save.name){
  
  # Could use data.table here
  # Get sum of recaptures by release and recapture region and by recapture quarter (month)
  recap_reg <- aggregate(list(recap.pred=tagdat$recap.pred, recap.obs=tagdat$recap.obs),
    list(rel.region = tagdat$rel.region, recap.region=tagdat$recap.region, recap.month=tagdat$recap.month),
    sum, na.rm=TRUE)

  # Get total recaptures by release region and month
  recap_reg_sum <- aggregate(list(recap.pred.sum = tagdat$recap.pred, recap.obs.sum = tagdat$recap.obs),
    list(rel.region = tagdat$rel.region, recap.month = tagdat$recap.month),
    sum, na.rm=TRUE)

  # Merge together and get the proportion of recaptures 
  # i.e. the total number of tags from region 1 that were recaptured, found out the proportion that was recaptured in each region
  recap_reg <- merge(recap_reg, recap_reg_sum)
  recap_reg$pred.prop <- recap_reg$recap.pred / recap_reg$recap.pred.sum
  recap_reg$obs.prop <- recap_reg$recap.obs/ recap_reg$recap.obs.sum

  # Sanity check that it looks like data in Fig 33 of SKJ 2019 assessment report
  # Total released from 1 that were recaught in 1 quarter
  #subset(recap_reg_sum, recap.month==2 & rel.region==1)
  #subset(recap_reg, recap.month==2 & rel.region==1)

  # Bare bones of Fig 33 of SKJ 2019 assessment report
  # Not continued as we should try something else
  #p <- ggplot(recap_reg, aes(x=rel.region, y=recap.region))
  #p <- p + geom_raster(aes(fill=obs.prop))
  #p <- p + facet_wrap(~recap.month)
  #p

  # Plot of the difference between predicted and observed proportion of tags returned by region of release
  recap_reg$diff.prop <- recap_reg$obs.prop - recap_reg$pred.prop
  #recap_reg$diff.prop2 <- log(recap_reg$pred.prop / recap_reg$obs.prop)
  recap_reg$rel.region.name <- paste("Release region ", recap_reg$rel.region, sep="")
  recap_reg$Quarter <- as.factor((recap_reg$recap.month+1) / 3)

  # Plot attempt 1 - Facet for each release region, plot difference in proportion
  # by recapture region, coloured by quarter
  
  # Stuff for dummy data
  no_grps <- length(unique(recap_reg$rel.region.name))
  ylims <- tapply(recap_reg$diff.prop, recap_reg$rel.region.name, function(x) c(max(abs(x), na.rm=T), -max(abs(x), na.rm=T)))
  dummydat <- data.frame(y=unlist(ylims), x=rep(c(min(recap_reg$recap.region), max(recap_reg$recap.region)), no_grps), rel.region.name = rep(names(ylims), each=2))
  dummydat$y <- dummydat$y * 1.1
  
  # Point plot
  if(plot.type == "point"){
    p <- ggplot2::ggplot(recap_reg, ggplot2::aes(x=as.factor(recap.region), y=diff.prop))
    p <- p + ggplot2::geom_point(aes(colour = Quarter), size=4)
    p <- p + ggplot2::facet_wrap(~rel.region.name, ncol=2, scales="free")
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0.0), linetype=2)
    p <- p + ggthemes::theme_few()
    p <- p + ggplot2::xlab("Recapture region")
    p <- p + ggplot2::ylab("Observed proportion - predicted proportion")
    p <- p + ggplot2::geom_blank(data=dummydat, ggplot2::aes(x=x, y=y))
  }

  # Bar plot
  if(plot.type == "bar"){
    p <- ggplot2::ggplot(recap_reg, ggplot2::aes(x=as.factor(recap.region), y=diff.prop))
    p <- p + ggplot2::geom_bar(stat="identity", ggplot2::aes(fill = Quarter), colour="black", position=ggplot2::position_dodge())
    p <- p + ggplot2::facet_wrap(~rel.region.name, ncol=2, scales="free")
    p <- p + ggplot2::xlab("Recapture region")
    p <- p + ggplot2::geom_hline(aes(yintercept=0.0))
    p <- p + ggplot2::ylab("Observed proportion - predicted proportion")
    p <- p + ggthemes::theme_few()
    p <- p + ggplot2::geom_blank(data=dummydat, ggplot2::aes(x=x, y=y))
  }
  
  save_plot(save.dir, save.name, plot=p)
  
  return(p)
}








