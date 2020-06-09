# Plot effort devs

#' Plot effort deviations over time by fishery
#' 
#' The effort deviations are in the 'par' object. The 'frq' object is needed to get the time series of fishing realisations.
#' Only effort deviations that have a corresponding effort in the 'frq' object are plotted.
#' A loess smoothed fit is shown.
#' 
#' @param frq An object of type MFCLFrq that contains the observed effort data.
#' @param par An object of MFCLPar that contains the effort deviations.
#' @param fisheries The numbers of the fisheries to plot.
#' @param fishery_names The names of the fisheries to plot.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom data.table data.table
#' @importFrom data.table rbindlist
#' @importFrom ggthemes theme_few
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 scale_color_gradient
#' @importFrom ggplot2 scale_y_continuous
#' 

plot.effort.devs <- function(frq, par, fisheries, fishery_names, save.dir, save.name){
  # Check input types - or write as a method - so could just pass in edevs instead of whole par object
  if (class(par) != "MFCLPar"){
    stop("par argument must of type of type 'MFCLPar'.")
  }
  if (class(frq) != "MFCLFrq"){
    stop("frq argument must of type of type 'MFCLFrq'.")
  }
  # Each fishery has different number edevs - based on realisations
  # Time steps given by frq file
  # This is repeated from the plot.pred.obs.cpue() plot - could farm out to function?
  # Extract the fishing realisations with observed catch, effort and penalty data
  frqreal <- realisations(frq)
  # Add timestep column for plotting - ignoring week
  frqreal$ts <- frqreal$year + (frqreal$month-1)/12 + 1/24  # month is mid-month
  # Tidy up missing values
  frqreal$effort[frqreal$effort < 0] <- NA
  frqreal$catch[frqreal$catch< 0] <- NA
  # Get the devs. A list, each element are edevs though time
  edevs <- effort_dev_coffs(par)
  # Check that we have all the obs
  if (length(unlist(edevs)) != dim(frqreal)[1]){
    stop("Length of effort devs does not match nrows of fishing realisations.")
  }
  # Force the order to be same as the effort devs so we can just unlist edevs in
  frqreal <- frqreal[order(frqreal$fishery, frqreal$ts), ]
  frqreal$edev <- unlist(edevs)
  # Subset the chosen fisheries
  # Only include edevs where you have a corresponding effort
  frqreal[is.na(frqreal$effort),"edev"] <- NA
  pdat <- subset(frqreal, fishery %in% fisheries)
  # Add in fishery names
  fishery_names_df <- data.frame(fishery = fisheries, fishery_names = fishery_names)
  pdat <- merge(pdat, fishery_names_df)
  # Plot it up
  # Get yrange
  ymax <- max(abs(pdat$edev), na.rm=TRUE)
  # Round it up to nearest 0.5
  ymax <- ceiling(ymax*2)/2
  
  p <- ggplot2::ggplot(pdat, ggplot2::aes(x=ts, y=edev))
  p <- p + ggplot2::geom_point(na.rm=TRUE)
  p <- p + ggplot2::geom_smooth(method = 'loess', formula = 'y~x', na.rm=TRUE)
  p <- p + ggplot2::facet_wrap(~fishery_names)
  p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0.0), linetype=2)
  p <- p + ggplot2::ylim(c(-ymax,ymax))
  p <- p + ggthemes::theme_few()
  p <- p + ggplot2::xlab("Time") + ggplot2::ylab("Effort deviation")
  
	# write.out
	if(!missing(save.dir))
	{
		if(missing(save.name))
		{
			stop("How can you save the output if you haven't specified the directory? Please specify save.dir.")
		} else {
			if (! dir.exists(save.dir))dir.create(save.dir,recursive=TRUE)
			ggplot2::ggsave(paste0(save.name,".png"),plot=p, device = "png", path = save.dir,scale = 1, width = 9, height = 9, units = c("in"))
		}
	} 
		
	return(p)
}
