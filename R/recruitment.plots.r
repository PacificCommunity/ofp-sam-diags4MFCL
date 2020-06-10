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

# Plot recruitment distributions across the models
plot.rec.dist <- function(rep.list, rep.names=NULL, palette.func=default.model.colours, save.dir, save.name, ...){
  # Check and sanitise input MFCLRep arguments and names
  rep.list <- check.rep.args(rep=rep.list, rep.names=rep.names)
  rep.names <- names(rep.list)
  
  
  rec_dist  <- rbind(rec_dist,   c(yearMeans(trim(seasonSums(popN(reps[[rr]])[1,]), year=1982:2018)))/sum(yearMeans(trim(seasonSums(popN(reps[[rr]])[1,]), year=1982:2018))))
#  rec_dist_q<- rbind(rec_dist_q, c(yearMeans(trim(popN(reps[[rr]])[1,], year=1982:2018)))/sum(yearMeans(trim(seasonSums(popN(reps[[rr]])[1,]), year=1982:2018))))
  
year_range <- 1982:2018  
  
  rec_dist <- lapply(rep.list, function(x){
    out <- c(yearMeans(trim(seasonSums(popN(x)[1,]), year=year_range))) / sum(yearMeans(trim(seasonSums(popN(x)[1,]), year=year_range)))
    return(out)
  }
  
  rec_dist_q<- lapply(rep.list, function(x){
    out <-  c(yearMeans(trim(popN(x)[1,], year=year_range)))/sum(yearMeans(trim(seasonSums(popN(x)[1,]), year=year_range)))
    return(out)
  }


)
  
  



#rec_dist <- rec_dist_q <- NULL
#for(rr in 1:length(runnames2)){
#  rec_dist  <- rbind(rec_dist,   c(yearMeans(trim(seasonSums(popN(reps[[rr]])[1,]), year=1982:2018)))/sum(yearMeans(trim(seasonSums(popN(reps[[rr]])[1,]), year=1982:2018))))
#  rec_dist_q<- rbind(rec_dist_q, c(yearMeans(trim(popN(reps[[rr]])[1,], year=1982:2018)))/sum(yearMeans(trim(seasonSums(popN(reps[[rr]])[1,]), year=1982:2018))))
#}
#rec_dist.df   <- data.frame(run=runnames2, region=rep(1:8, each=length(runnames2)), propn=c(rec_dist))
#rec_dist_q.df <- data.frame(run=runnames2, region=rep(1:8, each=length(runnames2)*4), qtr=rep(1:4, each=length(runnames2)), propn=c(rec_dist_q))
#rec_dist_q.df$id <- paste(rec_dist_q.df$region, rec_dist_q.df$qtr, sep="_")
#
#qtrlySRRrep <- reps[[1]]
#qtrlySRR_red_dist <- c(yearMeans(trim(popN(qtrlySRRrep)[1,], year=1982:2015)))/sum(yearMeans(trim(seasonSums(popN(qtrlySRRrep)[1,]), year=1982:2015)))
#
#png("/home/rob/MSE/ofp-sam-skipjack_MSE/reports/OM_cond/2019/figures/srr_distbn.png", width=800, height=600)
#boxplot(propn~id, data=rec_dist_q.df, col=rep(c("burlywood", "cornsilk", "bisque3", "beige", "cornsilk3","seashell2","wheat4","khaki3"), each=4),
#        names=rep(as.character(1:4), 8), xlab="Quarter", ylab="Proportion of Total Recruitment")
#abline(v=0.5+seq(4,length=7, by=4), col="grey")
#
##segments(seq(0.5,31.5), qtrlySRR_red_dist, seq(1.5, 32.5), qtrlySRR_red_dist, col="red", lwd=2)
#mtext(paste("Region", 1:8), side=3, line=1, at=c(2.5, 6.5, 10.5, 14.5, 18.5, 22.5, 26.5, 30.5))
#dev.off()





