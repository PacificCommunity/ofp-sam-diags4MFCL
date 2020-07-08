# Predicted vs observed catch plot

#' Plot difference between observed and predicted catches
#' 
#' Plot difference between observed and predicted catches.
#' The differences can be scaled by the total catch of each fishery.
#' Multiple models can be passed in.
#' @param rep.list A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.
#' @param rep.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.
#' @param fisheries The numbers of the fisheries to plot.
#' @param fishery_names The names of the fisheries to plot.
#' @param show.legend Do you want to show the plot legend, TRUE (default) or FALSE.
#' @param show.points Do you want to show points as well as the smoother for the difference plots? Default is FALSE.
#' @param palette.func A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @param ... Passes extra arguments to the palette function. Use the argument all.model.colours to ensure consistency of model colours when plotting a subset of models.
#' @export
#' @import FLR4MFCL
#' @import magrittr
plot.pred.obs.catch <- function(rep.list, rep.names=NULL, fisheries, fishery_names, scale.diff=TRUE, show.legend=TRUE, show.points=FALSE, palette.func=default.model.colours, save.dir, save.name, ...){
  # Check and sanitise input MFCLRep arguments and names
  rep.list <- check.rep.args(rep=rep.list, rep.names=rep.names)
  rep.names <- names(rep.list)
  
  # Pull out catch_obs and catch_pred from rep file
  catch_obs_list <- lapply(rep.list, function(x) as.data.frame(catch_obs(x)))
  catch_obs <- data.table::rbindlist(catch_obs_list, idcol="Model")
  setnames(catch_obs, "data", "catch_obs")
  catch_pred_list <- lapply(rep.list, function(x) as.data.frame(catch_pred(x)))
  catch_pred <- data.table::rbindlist(catch_pred_list, idcol="Model")
  setnames(catch_pred, "data", "catch_pred")
  pdat <- merge(catch_obs, catch_pred)
  pdat$season <- as.numeric(as.character(pdat$season))
  pdat$ts <- pdat$year + (pdat$season-1)/4 + 1/8 # Hard wired hack
  pdat$diff <- pdat$catch_obs - pdat$catch_pred
  
  ylab <- "Observed - predicted catch"
  # Scale by total catch by fishery and Model
  if(scale.diff == TRUE){
    pdat <- pdat[,.(ts=ts, diff = diff / mean(catch_obs, na.rm=TRUE)), by=.(Model, unit)]
    ylab <- "Obs. - pred. catch (scaled)"
  }
  
  # Add in fishery names
  setnames(pdat, "unit", "fishery")
  pdat$fishery<- as.numeric(as.character(pdat$fishery))
  fishery_names_df <- data.frame(fishery = fisheries, fishery_names = fishery_names)
  # Why do I need to specify by?
  pdat <- merge(pdat, fishery_names_df, by="fishery")
  
  # Want pdat to have Model names in the original order - important for plotting order
  pdat[,Model:=factor(Model, levels=names(rep.list))]
  
  # Plot it
  colour_values <- palette.func(selected.model.names = names(rep.list), ...)
  p <- ggplot2::ggplot(pdat, ggplot2::aes(x=ts, y=diff))
  if(show.points==TRUE){
    p <- p + ggplot2::geom_point(ggplot2::aes(colour=Model), na.rm=TRUE, alpha=0.6)
  }
  p <- p + ggplot2::scale_color_manual("Model",values=colour_values)
  # If just one model - colour the smoother red
  if(length(rep.list)==1){
    p <- p + ggplot2::geom_smooth(colour="red", method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
  }
  # Otherwise colour the smoother by model
  if(length(rep.list)>1){
    p <- p + ggplot2::geom_smooth(aes(colour=Model), method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
  }
  p <- p + ggplot2::xlab("Time") + ggplot2::ylab(ylab)  
  p <- p + ggplot2::facet_wrap(~fishery_names, scales="free")
  p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0.0), linetype=2)
  #p <- p + ggplot2::ylim(c(0,NA))
  p <- p + ggthemes::theme_few()
  if(show.legend==FALSE){
    p <- p + ggplot2::theme(legend.position = "none") 
  }
  
  save_plot(save.dir, save.name, plot=p)
  
  return(p)
}