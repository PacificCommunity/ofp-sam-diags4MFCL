#' Plot the distribution an age class by region and quarter
#'
#' Plot the distribution of proportions of a chosen age class across year between quarters and region from a single model.
#' @param rep a MFCLRep object
#' @param ages The age class that you want to plot the distribution of values across
#' @param year_range The year range to plot the distribution of the recruitment over. Default is all years
#' @param plot_type can be "violin" for a violin plot or "box" for a boxplot. Default is violin plot
#' @param overlay_data Do you want to overlay the original data on the distribution? TRUE or FALSE (default).
#' @param palette.func a function to determine the colours of each area.  It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param save.dir Path to the directory where the output will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @param ... Passes extra arguements to the palette function. Use the aurgument all.model.colours to ensure consistency of model colours when plottin a subset of models.
#' @export
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_violin

plot.age.dist <- function(rep,ages,year_range=(as.numeric(range(rep)["minyear"])):as.numeric(range(rep)["maxyear"]), plot_type="violin", overlay_data=FALSE, palette.func=default.model.colours, save.dir, save.name, ...){
  # Check and sanitise input MFCLRep
  rep.list <- check.rep.args(rep=rep, rep.names=NULL)
  dat <- as.data.frame(popN(rep)[ages,ac(year_range)])
  dat$total_rec <- c(areaSums(seasonSums(popN(rep)[ages,ac(year_range)])))
  dat$prop_rec <- dat$data / dat$total_rec


  # Tidy up data

  no_seasons <- length(unique(dat$season))
  no_areas <- length(unique(dat$area))
  dat$area_name <- paste("Region ", dat$area, sep="")
  dat$age_name <- factor(paste("Age",dat$age),unique(paste("Age",dat$age)))

  # And plot
  # Colour by area - not sure it's a great idea
  colour_values <- palette.func(selected.model.names = unique(dat$area_name), ...)
  p <- ggplot2::ggplot(dat, ggplot2::aes(x=season, y=prop_rec))
  p <- p + geom_hline(yintercept=0,color='grey85',size=0.8)
  if(plot_type=="violin"){
    p <- p + ggplot2::geom_violin(aes(fill=area_name))
  }
  else {
    p <- p + ggplot2::geom_boxplot(aes(fill=area_name))
  }
  if (overlay_data){
    p <- p + ggplot2::geom_point(alpha=0.25) # Maybe overlay the original data?
  }
  p <- p + ggplot2::facet_grid(age_name~area_name)
  p <- p + ggplot2::xlab("Quarter") + ggplot2::ylab("Proportion of total recruitment")
  p <- p + ggthemes::theme_few()
  p <- p + ggplot2::theme(legend.position = "none")
	p <- p + ggplot2::scale_fill_manual("Model",values=colour_values)

  save_plot(save.dir, save.name, plot=p)

  return(p)
}

