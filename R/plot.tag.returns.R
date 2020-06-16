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
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom data.table data.table
#' @importFrom data.table rbindlist
#' @importFrom ggthemes theme_few
#' @importFrom ggplot2 geom_blank
plot.tag.returns.time <- function(tagdat, recapture.groups, plot.diff=TRUE, save.dir, save.name){
  
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
    pdat$diff <- pdat$recap.pred - pdat$recap.obs
    # Alt. - problem with obs = 0, leads to infinite difference
    # pdat$diff <- log(pdat$recap.pred / pdat$recap.obs)
    # Spoof up approriate y ranges for each facet using geom_blank()
    no_grps <- length(unique(pdat$tag_recapture_name))
    ylims <- tapply(pdat$diff, pdat$tag_recapture_name, function(x) c(max(abs(x), na.rm=T), -max(abs(x), na.rm=T)))
    dummydat <- data.frame(y=unlist(ylims), x=rep(c(min(pdat$recap.ts), max(pdat$recap.ts)), no_grps), tag_recapture_name = rep(names(ylims), each=2))
    p <- ggplot2::ggplot(pdat, ggplot2::aes(x=recap.ts, y=diff))
    p <- p + ggplot2::geom_point(na.rm=TRUE)
    p <- p + ggplot2::geom_smooth(method = 'loess', formula = 'y~x', na.rm=TRUE)
    p <- p + ggplot2::facet_wrap(~tag_recapture_name, scales="free")
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0.0), linetype=2)
    p <- p + ggthemes::theme_few()
    p <- p + ggplot2::xlab("Time") + ggplot2::ylab("Predicted - observed recaptures")
    #p <- p + ggplot2::xlab("Time") + ggplot2::ylab("log(Predicted / observed) recaptures")
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
#' @param tagdat Tagging data created by the \code{tag.data.preparation()} function.
#' @param facet.program Do you want to process and plot by tagging program, or combine the tagging programs. TRUE (default) or FALSE.
#' @param plot.diff Do you want to plot the difference between the observed and predicted, or a time series of recaptures? TRUE (default) or FALSE.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' @import FLR4MFCL
#' @import magrittr
plot.tag.attrition <- function(tagdat, facet.program=TRUE, plot.diff=TRUE, save.dir, save.name){
  
  # Sum number of tags by period at liberty
  # Depends if we want to process by tag program
  if (facet.program==FALSE){
  pdat <- aggregate(list(recap.obs = tagdat$recap.obs, recap.pred = tagdat$recap.pred),
                 list(period_at_liberty = tagdat$period_at_liberty),
                 sum, na.rm=TRUE)
  pdat$diff <- pdat$recap.pred - pdat$recap.obs
  # Need to pad out time series
  padts <- expand.grid(period_at_liberty = seq(from=min(pdat$period_at_liberty), to=max(pdat$period_at_liberty), by= 1))
  pdat <- merge(pdat, padts, all=TRUE)
  }
  # Same again but keep the program info
  if (facet.program==TRUE){
  pdat <- aggregate(list(recap.obs = tagdat$recap.obs, recap.pred = tagdat$recap.pred),
                 list(period_at_liberty = tagdat$period_at_liberty, program=tagdat$program),
                 sum, na.rm=TRUE)
  pdat$diff <- pdat$recap.pred - pdat$recap.obs
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
    if(facet.program == TRUE){
      p <- p + ggplot2::facet_wrap(~program, scales="free")
    }
    p <- p + ggplot2::xlab("Periods at liberty (quarters)")
    p <- p + ggplot2::ylab("Number of tag returns")
    p <- p + ggplot2::ylim(c(0,NA))
    p <- p + ggthemes::theme_few()
  }
  
  # Residuals
  if(plot.diff == TRUE){
    # Get dummy data to set nice looking ylims
    no_progs <- length(unique(pdat$program))
    ylims <- tapply(pdat$diff, pdat$program, function(x) c(max(abs(x), na.rm=T), -max(abs(x), na.rm=T)))
    dummydat <- data.frame(y=unlist(ylims), x=rep(c(min(pdat$period_at_liberty), max(pdat$period_at_liberty)), no_progs), program = rep(names(ylims), each=2))
    p <- ggplot2::ggplot(pdat, aes(x=period_at_liberty, y=diff))
    p <- p + ggplot2::geom_point()
    p <- p + ggplot2::geom_smooth(method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0.0), linetype=2)
    if(facet.program == TRUE){
      p <- p + ggplot2::facet_wrap(~program, scales="free")
    }
    p <- p + ggplot2::ylab("Predicted - observed recaptures")
    p <- p + ggplot2::xlab("Periods at liberty (quarters)")
    p <- p + ggthemes::theme_few()
    p <- p + ggplot2::geom_blank(data=dummydat, aes(x=x, y=y))
    
  }
  
  save_plot(save.dir, save.name, plot=p)
  
  return(p)
}
