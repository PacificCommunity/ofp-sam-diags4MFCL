# Plot the observed and predicted CPUE of the index fisheries through time


# Add some check that frq is not the Mathew frq

#' Plot time series of the predicted and observed CPUE for selected fisheries
#' 
#' Plot observed (colored points) and model-predicted (black lines) CPUE for selected fisheries 
#' The color of the point indicates the penalty applied to the observed CPUE where brighter colors indicate a larger penalty.
#' 
#' The observed CPUE is calculated from the observed catches and effort in the 'freq' file.
#' The model-predicted CPUE is calculated from the observed catches in the 'freq' file and the model-estimated effort (from applying the estimated effort devs to the observed effort in the 'freq' file .)
#' The CPUE series are normalised by dividing by the mean before plotting.
#' (The difference between the predicted and observed time series is therefore just the effort devs).
#' @param frq An object of type MFCLFrq that contains the observed catch, effort and penalty data.
#' @param par An object of MFCLPar that contains the effort devs.
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
#' @importFrom ggplot2 scale_color_gradient
#' @importFrom ggplot2 scale_y_continuous
#' 
plot.pred.obs.cpue <- function(frq = frq, par = par, fisheries, fishery_names, save.dir, save.name){
  # Check input types - or write as a method - so could just pass in edevs instead of whole par object
  if (class(par) != "MFCLPar"){
    stop("par argument must of type of type 'MFCLPar'.")
  }
  if (class(frq) != "MFCLFrq"){
    stop("frq argument must of type of type 'MFCLFrq'.")
  }
  
  # Extract the fishing realisations with observed catch, effort and penalty data
  frqreal <- realisations(frq)
  # Add timestep column for plotting - ignoring week
  frqreal$ts <- frqreal$year + (frqreal$month-1)/12 + 1/24  # month is mid-month
  # Tidy up missing values
  frqreal$effort[frqreal$effort < 0] <- NA
  frqreal$catch[frqreal$catch< 0] <- NA
  # Get observed CPUE - simple
  frqreal$obs_cpue <- frqreal$catch / frqreal$effort
  # Get model-predicted CPUE
  # Get effort devs from the par file and apply them to the observed effort
  # model predicted CPUE = effort * exp(edevs)
  edevs <- effort_dev_coffs(par)
  # Bring edevs into freq - requires dims to be the same as the fishing realisations
  if (length(unlist(edevs)) != dim(frqreal)[1]){
    stop("Length of effort devs does not match nrows of fishing realisations.")
  }
  # Force the order to be same as the effort devs so we can just unlist edevs in
  frqreal <- frqreal[order(frqreal$fishery, frqreal$ts), ]
  frqreal$edev <- unlist(edevs)
  frqreal$est_effort <- frqreal$effort * exp(frqreal$edev)
  frqreal$est_cpue <- frqreal$catch / frqreal$est_effort

  # Normalise the CPUE
  # Could probably redo some of the above with data.table too
  frqrealdt <- data.table(frqreal)
  # Add by normalisation columns by reference
  frqrealdt[, c("norm_obs_cpue", "norm_est_cpue") := .(obs_cpue / mean(obs_cpue, na.rm=TRUE), est_cpue / mean(est_cpue, na.rm=TRUE)), by=fishery]
  
  # Subset out the required fisheries
  pdat <- frqrealdt[fishery %in% fisheries,]
  # Name the fisheries
  if(length(fisheries) != length(fishery_names)){
    stop("fisheries should be the same length as fishery_names")
  }
  fishery_names <- data.frame(fishery = fisheries, fishery_names = fishery_names)
  pdat <- merge(pdat, fishery_names)

  # Plot similar to the SKJ 2019 assessment Figure 18
  p <- ggplot2::ggplot(pdat, aes(x=ts))
  p <- p + ggplot2::geom_point(aes(y=norm_obs_cpue, colour=penalty), na.rm=TRUE)
  p <- p + ggplot2::geom_line(aes(y=norm_est_cpue), na.rm=TRUE)
  p <- p + ggplot2::facet_wrap(~fishery_names, scales="free")
  p <- p + ggplot2::ylim(c(0,NA))
  p <- p + ggplot2::scale_colour_gradient(low = 'royalblue', high = 'lightskyblue1')
  p <- p + ggthemes::theme_few()
  p <- p + ggplot2::xlab("Time") + ggplot2::ylab("Normalised CPUE")
  p <- p + ggplot2::theme(legend.position = "none")

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


