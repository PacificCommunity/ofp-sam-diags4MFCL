# Plots based on recruitment
# Based on code by Rob Scott 2020

# Plotting stock-recruitment relationships

#' Plot the stock-recruitment relationship
#' 
#' For each model, the fitted Beverton-Holt stock-recruiment relationship is plotted.
#' Estimated adult biomass and recruitment is also plotted.
#' @param frq An object of type MFCLFrq that contains the observed effort data.
#' @param palette.func A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @param ... Passes extra arguments to the palette function. Use the argument all.model.colours to ensure consistency of model colours when plotting a subset of models.
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
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' 
plot.srr <- function(rep.list, rep.names=NULL, palette.func=default.model.colours, save.dir, save.name, ...){
  # Check and sanitise input MFCLRep arguments and names
  rep.list <- check.rep.args(rep=rep.list, rep.names=rep.names)
  rep.names <- names(rep.list)
  
  adult_biomass <- as.data.frame(FLQuants(lapply(rep.list, function(x) {areaSums(adultBiomass(x))})))[,c("year","season","data", "qname")]
  recruitment <- as.data.frame(FLQuants(lapply(rep.list, function(x) {areaSums(popN(x)[1,])})))[,c("year","season","data", "qname")]
  colnames(adult_biomass)[colnames(adult_biomass)=="data"] <- "sb"
  colnames(recruitment)[colnames(recruitment)=="data"] <- "rec"
  # Basically plotting year to year 
  # This might not be strictly correct as the recruitment function is fitted using
  # some kind of rolling window of SB (see that report on bias correction)
  pdat <- merge(adult_biomass, recruitment)
  
  # Data for the BH shape
  max_sb <- max(adult_biomass$sb) * 1.2 # Just add another 20% on
  sb <- seq(0, max_sb, length=100)
  # Extract the BH params and make data.frame of predicted recruitment
  params <- lapply(rep.list, function(x) c(srr(x)[c("a","b")]))
  bhdat <- data.frame(rec = unlist(lapply(params, function(x) (sb * x[1]/4) / (x[2]+sb))), sb=sb, qname = rep(rep.names, each=length(sb)))
  
	colour_values <- palette.func(selected.model.names = names(rep.list), ...)
  # Plot everything
  p <- ggplot2::ggplot(pdat, aes(x=sb, y=rec))
  p <- p + ggplot2::geom_point(aes(colour=qname))
  p <- p + ggplot2::ylim(c(0,NA))
  p <- p + ggplot2::geom_line(data=bhdat, aes(x=sb, y=rec, colour=qname), size=1.2)
  p <- p + ggplot2::xlab("Adult biomass") + ggplot2::ylab("Recruitment")
	p <- p + ggplot2::scale_color_manual("Model",values=colour_values)
	p <- p + ggthemes::theme_few()
	
  save_plot(save.dir, save.name, plot=p)
  
  return(p)
}

#' Plot recruitment distribution proprotions
#' 
#' Plot distribution of proportion of total recruitment (averaged over year range) between quarters and regions across models.
#' @param rep.list A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.
#' @param rep.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.
#' @param year_range The year range to average the total recruitment over. Default is the last 10 years.
#' @param plot_type Can be "violin" for a violin plot or "box" for a boxplot. The default is boxplot.
#' @param overlay_data Do you want to overlay the original data on the distribution? TRUE or FALSE (default).
#' @param palette.func A function to determine the colours of each area. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @param ... Passes extra arguments to the palette function. Use the argument all.model.colours to ensure consistency of model colours when plotting a subset of models.
#' @export
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_violin
plot.rec.dist <- function(rep.list, rep.names=NULL, year_range = (as.numeric(range(rep.list[[1]])["maxyear"])-9):as.numeric(range(rep.list[[1]])["maxyear"]), plot_type="violin", overlay_data=FALSE, palette.func=default.model.colours, save.dir, save.name, ...){
  # Check and sanitise input MFCLRep arguments and names
  rep.list <- check.rep.args(rep=rep.list, rep.names=rep.names)
  rep.names <- names(rep.list)
  
  dat <- lapply(rep.list, function(x){
    out <- as.data.frame(yearMeans(popN(x)[1,ac(year_range)]))
    out$total_rec  <- c(areaSums(seasonSums(yearMeans(popN(x)[1,ac(year_range)]))))
    out$prop_rec <- out$data / out$total_rec
    return(out)
    }
  )
  dat <- do.call("rbind", dat)
  # Tidy up data
  dat$model <- rep(rep.names, each=dim(dat)[1] / length(rep.names))
  no_seasons <- length(unique(dat$season))
  no_areas <- length(unique(dat$area))
  dat$area_name <- paste("Region ", dat$area, sep="")
  
  # And plot
  # Colour by area - not sure it's a great idea
  colour_values <- palette.func(selected.model.names = unique(dat$area_name), ...)
  p <- ggplot2::ggplot(dat, ggplot2::aes(x=season, y=prop_rec))
  if(plot_type=="violin"){
    p <- p + ggplot2::geom_violin(aes(fill=area_name))
  }
  else {
    p <- p + ggplot2::geom_boxplot(aes(fill=area_name))
  }
  if (overlay_data){
    p <- p + ggplot2::geom_point(alpha=0.25) # Maybe overlay the original data?
  }
  p <- p + ggplot2::facet_wrap(~area_name, ncol = no_areas)
  p <- p + ggplot2::xlab("Quarter") + ggplot2::ylab("Proportion of total recruitment")
  p <- p + ggthemes::theme_few()
  p <- p + ggplot2::theme(legend.position = "none") 
	p <- p + ggplot2::scale_fill_manual("Model",values=colour_values)
  
  save_plot(save.dir, save.name, plot=p)
  
  return(p)
}

#' Plot estimated recruitment deviates
#' 
#' Plot estimated recruitment deviates for a single model, by quarter and region.
#' A loess smoothed fit is shown.
#' @param par.list A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.
#' @param par.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.
#' @param show.legend Do you want to show the plot legend, TRUE (default) or FALSE.
#' @param show.points Do you want to show points as well as the smoother for the difference plots? Default is FALSE.
#' @param palette.func A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @param ... Passes extra arguments to the palette function. Use the argument all.model.names to ensure consistency of model colours when plotting a subset of models.
#' @export
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom ggplot2 scale_x_continuous
plot.rec.devs <- function(par.list, par.names=NULL, show.legend=TRUE, show.points=FALSE, palette.func=default.model.colours, save.dir, save.name, ...){
  
  # Check args
  par.list <- check.par.args(par=par.list, par.names=par.names)
  par.names <- names(par.list)
  # Grab the region_rec_var per Model
  recvars <- lapply(par.list, function(x) as.data.frame(region_rec_var(x)))
  pdat <-  data.table::rbindlist(recvars, idcol="Model")
  pdat$area_name <- paste("Region ", pdat$area, sep="")
  pdat$season_name <- paste("Quarter ", pdat$season, sep="")
  
  year_axis_breaks <- seq(10*floor(min(pdat$year)/10), 10*ceiling(max(pdat$year)/10) , 20)
  
  colour_values <- palette.func(selected.model.names = names(par.list), ...)
  p <- ggplot2::ggplot(pdat, ggplot2::aes(x=year, y=data))
  if(show.points==TRUE){
    p <- p + ggplot2::geom_point(aes(colour=Model))
  }
  if(length(par.list)==1){
    p <- p + ggplot2::geom_smooth(colour="red", method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
  }
  # Otherwise colour the smoother by model
  if(length(par.list)>1){
    p <- p + ggplot2::geom_smooth(aes(colour=Model), method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
  }
  p <- p + ggplot2::facet_grid(season_name ~ area_name, scales="free")
  p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0.0), linetype=2)
  p <- p + ggplot2::scale_color_manual("Model",values=colour_values)
  p <- p + ggplot2::xlab("Year") + ggplot2::ylab("Recruitment deviate")
  p <- p + ggplot2::scale_x_continuous(breaks=year_axis_breaks) 
  p <- p + ggthemes::theme_few()
  if (show.legend==FALSE){
    p <- p + theme(legend.position="none") 
  }
  
  save_plot(save.dir, save.name, plot=p)
  
	return(p)
}




