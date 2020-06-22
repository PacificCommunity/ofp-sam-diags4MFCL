# Attrition rate by tagging program
# Similar to Figs 28 and 29 in SKJ

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
  # Collapse into a single data.table
  tagdat <- data.table::rbindlist(tagdat.list, idcol="Model")
  
  # Y lab for the difference plot without scaling - overwritten if scaled
  ylab <- "Observed - predicted recaptures"
  
  # Sum number of tags by period at liberty
  # Depends if we want to process by tag program
  if (facet.program==FALSE){
    grouping_names <- c("Model", "period_at_liberty")
  }
  if (facet.program==TRUE){
    grouping_names <- c("Model", "period_at_liberty", "program")
  }
  
  pdat <- tagdat[, .(recap.obs=sum(recap.obs, na.rm=TRUE), recap.pred=sum(recap.pred, na.rm=TRUE)) ,by=mget(grouping_names)]
  pdat$diff <- pdat$recap.obs - pdat$recap.pred
  if(scale.diff == TRUE){
    # Don't group by period at liberty
    grouping_names <- grouping_names[grouping_names!="period_at_liberty"] 
    total_recaptured <- pdat[,.(total_obs_recap=sum(recap.obs, na.rm=TRUE)), by=mget(grouping_names)]
    pdat <- merge(pdat, total_recaptured)
    pdat$diff <- pdat$diff / pdat$total_obs_recap
    ylab <- "Obs. - pred. recaptures (scaled)"
    }
  if (facet.program==FALSE){
    pdat$program <- "All programs"
  }
  # Need to pad out time series - merge has stopped working with data.table
  padts <- expand.grid(period_at_liberty = seq(from=min(pdat$period_at_liberty), to=max(pdat$period_at_liberty), by= 1), program = sort(unique(pdat$program)))
  pdat <- merge(padts, pdat, all=TRUE)
  
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
