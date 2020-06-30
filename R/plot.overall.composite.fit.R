#' Plot composite catch-at-length data for each fishery.
#' 
#' Plot composite (all time periods combined) observed and predicted catch-at-length for each fishery.
#' The table comes from the length.fit file and has already been processed using the
#' \code{length.fit.preparation()} function (which is based on Yukio's code).
#' This may also work with weight.fit data but has not been tested yet.
#' 
#' @param lfit A data.frame of observed and predicted composite catch-at-length data (generated using \code{length.fit.preparation()}).
#' @param fisheries The numbers of the fisheries to plot.
#' @param fishery_names The names of the fisheries to plot.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' @import FLR4MFCL
#' @import magrittr
plot.overall.composition.fit <- function(lfit, fisheries, fishery_names, save.dir, save.name){
  
  # Subset out the desired fisheris
  pdat <- subset(lfit, fishery %in% fisheries)
  
  # Bring in the fishery names
  fishery_names_df <- data.frame(fishery = fisheries, fishery_names = fishery_names)
  pdat <- merge(pdat, fishery_names_df)
  
  bar_width <- pdat$length[2] - pdat$length[1]
  # Plot up
  p <- ggplot(pdat, aes(x=length))
  # Observed as barchart
  p <- p + geom_bar(aes(y=obs), fill="blue", colour="blue", stat="identity", width=bar_width)
  # Predicted as red line
  p <- p + geom_line(aes(y=pred), colour="red", size=2)
  p <- p + facet_wrap(~fishery_names, scales="free", ncol=2)
  p <- p + xlab("Length (cm)") + ylab("Samples")
  p <- p + ggthemes::theme_few()
  # Tighten the axes
  p <- p + scale_y_continuous(expand = c(0, 0))
  p
  
  save_plot(save.dir, save.name, plot=p)
  
  return(p)
}