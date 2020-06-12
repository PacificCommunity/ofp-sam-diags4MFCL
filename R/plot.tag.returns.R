# Tagging plots

#' Plot the observed and predicted tag recaptures.
#' 
#' Plot the observed and predicted tag recaptures by tag recapture group.
#' Tags caught during the mixing period are not counted.
#' The plot is either a time series of the difference between the observed and predicted, or a time series of the recaptures.
#' A loess smoother is put through the differences.
#' @param tagrep A data.frame of tag returns from the temporary_tag_returns file. The output from the FLR4MFCL function \code{read.temporary_tag_report()}.
#' @param tagobs An object of type MFCLTag.
#' @param par An object of MFCLPar that contains the effort deviations.
#' @param fishery.map A data.frame that describes which fishery is fishing in which region and with which gear. The columns are: fishery_name, region, gear, fishery (the reference number), tag_recapture_group and tag_recapture_name. Only tag groups in the fishery.map will be plotted.
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
plot.tag.returns.time <- function(tagrep, tagobs, par, fishery.map, plot.diff=TRUE, save.dir, save.name){
  if (class(par) != "MFCLPar"){
    stop("par argument must of type of type 'MFCLPar'.")
  }
  if (class(tagobs) != "MFCLTag"){
    stop("obs argument must of type of type 'MFCLTag'.")
  }
  
  # The predicted and observed recapture data is in the tagrep object
  # However, if you want to remove the tags that are within the mixing period
  # we need additional information:
  # * The mixing period per release (in the par object)
  # * The release date per release (in the tagobs object)
  
  # Optional stuff: par argument could be a vector of mixing instead of full par object
  # Better to do all this data processing and hacking externally and just pass in the
  # complete data set with all the above info
  
  # Need the par to get mixing periods by release group
  # Or get number of releases from the tag file
  tag_releases <- data.frame(rel.group = sort(unique(tagrep$rel.group)))
  # Get the mixing periods from the tag flags
  tag_releases$mixing_period <- flagval(par, (-10000 - tag_releases$rel.group + 1),1)$value
  tagrep$mixing_period <- tag_releases$mixing_period[tagrep$rel.group]
  
  # Also need the release dates
  # Could sum over length to get total release numbers by release event
  releases <- unique(releases(tagobs)[,c("rel.group","region","year","month")])
  setnames(releases, c("rel.group", "rel.region", "rel.year", "rel.month"))
  tagdat <- merge(tagrep, releases, all=TRUE)
  
  # Sort out the release and recapture timestep
  no_seasons <- length(unique(tagdat$recap.month)) # this is unsafe, how best to get no seasons?
  tagdat$rel.ts <- tagdat$rel.year + (tagdat$rel.month-1)/12 + 1/24
  tagdat$recap.ts <- tagdat$recap.year + (tagdat$recap.month-1)/12 + 1/24
  tagdat$mixing_period_quarters <- tagdat$mixing_period / no_seasons
  
  # ID observations that are within the mixing period  
  mixing_rows <- tagdat$recap.ts < (tagdat$rel.ts + tagdat$mixing_period_quarters) 
  # And drop them
  tagdat <- tagdat[!mixing_rows,]
  
  # Bring in recapture groups
  colnames(fishery.map)[colnames(fishery.map) == "fishery"] <- "recap.fishery"
  tagdat <- merge(tagdat, fishery.map[,c("recap.fishery", "tag_recapture_group")])
  
  # Prepare data for plotting
  # Sum predicted and observed recaptures in each timestep for each tag recapture group
  pdat <- aggregate(list(recap.pred = tagdat$recap.pred, recap.obs=tagdat$recap.obs),
                    list(tag_recapture_group = tagdat$tag_recapture_group,
                         recap.ts = tagdat$recap.ts), sum, na.rm=TRUE)
  # Reorder so it's helpful for debugging
  pdat <- pdat[order(pdat$tag_recapture_group, pdat$recap.ts),]
  
  # To ensure plotting is OK we need each fishery to have a full complement of time series
  # observations, even if NA.
  # Need to pad out time series
  padts <- expand.grid(recap.ts = seq(from=min(pdat$recap.ts), to=max(pdat$recap.ts), by= 1 / no_seasons), tag_recapture_group = sort(unique(pdat$tag_recapture_group)))
  pdat <- merge(pdat, padts, all=TRUE)
  
  
  # Now bring in the names - tried to do above with the original merge with the 
  # fishery.map but it ended up with loads of missing values in the name column
  # after padding the data set
  pdat <- merge(pdat, unique(fishery.map[,c("tag_recapture_name", "tag_recapture_group")]))
  
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
    # Or plot the difference
    pdat$diff <- pdat$recap.obs - pdat$recap.pred
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
    p <- p + ggplot2::xlab("Time") + ggplot2::ylab("Observed - predicted recaptures")
    p <- p + ggplot2::geom_blank(data=dummydat, aes(x=x, y=y))
  }
  
  save_plot(save.dir, save.name, plot=p)
  
  return(p)
}

