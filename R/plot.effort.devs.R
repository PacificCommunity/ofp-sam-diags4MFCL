# Plot effort devs

#' Plot effort deviations over time by fishery
#' 
#' The effort deviations are in the 'par' object. The 'frq' object is needed to get the time series of fishing realisations.
#' Only effort deviations that have a corresponding effort in the 'frq' object are plotted.
#' A loess smoothed fit is shown.
#' 
#' @param frqreal.list A list of, or single instance of, data.frames, each one from calling realisations() on an MFCLFrq.
#' @param par.list A list of MFCLPar objects or a single MFCLFrq that contain the effort deviations.
#' @param model.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the frqreal.list (if available) or generated automatically.
#' @param fisheries The numbers of the fisheries to plot.
#' @param fishery_names The names of the fisheries to plot. If not supplied, the fishery numbers from the fisheries argument is used.
#' @param show.legend Do you want to show the plot legend, TRUE (default) or FALSE.
#' @param show.points Do you want to show points as well as the smoother for the difference plots? Default is FALSE.
#' @param palette.func A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @param ... Passes extra arguments to the palette function. Use the argument all.model.names to ensure consistency of model colours when plotting a subset of models.
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

plot.effort.devs <- function(frqreal.list, par.list, model.names=NULL, fisheries, fishery.names=as.character(fisheries), show.legend=TRUE, show.points=FALSE, palette.func=default.model.colours, save.dir, save.name, ...){
  # Check input types
  frqreal.list <- check.frqreal.args(frqreal=frqreal.list, frqreal.names=model.names)
  frq.names <- names(frqreal.list)
  par.list <- check.par.args(par=par.list, par.names=model.names)
  par.names <- names(par.list)
  if(length(par.list) != length(frqreal.list)){
    stop("frqreal.list must be the same length as par.list")
  }
  
  # Each fishery has different number edevs - based on realisations
  # Time steps given by frq file
  # This is repeated from the plot.pred.obs.cpue() plot - could farm out to function?
  # Extract the fishing realisations with observed catch, effort and penalty data
  frqreal <- data.table::rbindlist(frqreal.list, idcol="Model")
  # Add timestep column for plotting - ignoring week
  frqreal$ts <- frqreal$year + (frqreal$month-1)/12 + 1/24  # month is mid-month
  # Tidy up missing values
  frqreal$effort[frqreal$effort < 0] <- NA
  frqreal$catch[frqreal$catch< 0] <- NA
  
  # Get the devs. A list, each element are edevs though time
  # Pull out the effort coffs from each par in the list
  edevs_list_all <- lapply(par.list, function(x) (effort_dev_coffs(x)))
  # We now have a list, where each element is a list of coffs
  # Turn each list element into a vector of coffs
  edevs_list <- lapply(edevs_list_all, unlist)
  # Sanity check that the position is OK
  #lapply(effort_dev_coffs(par.list[[10]]),length)
  #effort_dev_coffs(par.list[[10]])[[4]][1:10]
  #edevs[[10]][565:574]
  # We now have a list of vectors. Collapse into a single vector
  edevs <- unlist(edevs_list)
  # Sanity check that the order is right
  #edevs_list[[2]][[1]][1:10]
  #test1[5449:5455] 
  
  # Check that we have all the obs
  if (length(unlist(edevs)) != dim(frqreal)[1]){
    stop("Length of effort devs does not match nrows of fishing realisations.")
  }
  # Force the order to be same as the effort devs so we can just unlist edevs in
  # But I don't want to change the original model order
  # (important for colouring - the last model needs to be visible in black)
  # Set as factor in the actual plot
  data.table::setorder(frqreal, Model, fishery, ts)
  # Unlist and hope the order is right!
  frqreal$edev <- unlist(edevs)
  # Sanity check
  #subset(frqreal, Model=="A2B0C1D0E0" & fishery==12)
  #edevs_list_all[["A2B0C1D0E0"]][[12]][1:5]
  
  # Subset the chosen fisheries
  # Only include edevs where you have a corresponding effort
  #frqreal[is.na(frqreal$effort),"edev"] <- NA
  #frqreal[is.na(frqreal$effort),.(edev, effort)]
  frqreal[is.na(frqreal$effort),edev:=NA]
  pdat <- frqreal[fishery %in% fisheries]
  # Add in fishery names
  fishery_names_df <- data.frame(fishery = fisheries, fishery_names = fishery.names)
  pdat <- merge(pdat, fishery_names_df)
  # Plot it up
  # Get yrange
  ymax <- max(abs(pdat$edev), na.rm=TRUE)
  # Round it up to nearest 0.5
  ymax <- ceiling(ymax*2)/2
  
  # Want pdat to have Model names in the original order - important for plotting order
  pdat[,Model:=factor(Model, levels=names(frqreal.list))]
  
  colour_values <- palette.func(selected.model.names = names(frqreal.list), ...)
  p <- ggplot2::ggplot(pdat, ggplot2::aes(x=ts, y=edev))
  if(show.points==TRUE){
    p <- p + ggplot2::geom_point(aes(colour=Model), na.rm=TRUE, alpha=0.6)
    p <- p + ggplot2::ylim(c(-ymax,ymax))
  }
  # If just one model - colour the smoother red
  if(length(frqreal.list)==1){
    p <- p + ggplot2::geom_smooth(colour="red", method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
  }
  # Otherwise colour the smoother by model
  if(length(frqreal.list)>1){
    p <- p + ggplot2::geom_smooth(aes(colour=Model), method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
  }
  p <- p + ggplot2::scale_color_manual("Model",values=colour_values)
  p <- p + ggplot2::facet_wrap(~fishery_names)
  p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0.0), linetype=2)
  p <- p + ggthemes::theme_few()
  p <- p + ggplot2::xlab("Time") + ggplot2::ylab("Effort deviation")
  if (show.legend==FALSE){
    p <- p + theme(legend.position="none") 
  }
  
  save_plot(save.dir, save.name, plot=p)
  
	return(p)
}
