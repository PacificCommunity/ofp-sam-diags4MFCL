
#' Plot proportions of tag returns by region
#' 
#' Plot the difference between the predicted and observed proportions of tag returns by region.
#' Experimental at the moment.
#' @param tagdat A data.frame of tagging data created by the \code{tag.data.preparation()} function.
#' @param plot.type What type of plot: 'point' (default) or 'bar'. 
#' @param save.dir Path to the directory where the outputs will be saved.
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory.
#' @export
#' @rdname plot.tag.return.proportion.v2
#' @name Plot tag return proportion
#' @import FLR4MFCL
#' @import magrittr
plot.tag.return.proportion.v2 <- function(tagrelease, tagrep, fishery.map, plot.type="point", save.dir, save.name){
  pdat <- prepare.tag.return.proportion.v2(tagrelease=tagrelease, tagrep=tagrep, fishery.map=fishery.map)
  p <- generate.tag.return.proportion.v2(pdat=pdat, plot.type=plot.type, save.dir=save.dir, save.name=save.name)
  return(p)
}
  
#' @export
#' @rdname plot.tag.return.proportion.v2
#' @name Plot tag return proportion
prepare.tag.return.proportion.v2 <- function(tagrelease, tagrep, fishery.map){
  
  colnames(fishery.map)[colnames(fishery.map)=="fishery"] <- "recap.fishery"
  tagrep <- merge(tagrep, fishery.map[,c("recap.fishery", "region")])
  # Merge the rep and release
  tdat <- data.table(merge(tagrep, tagrelease))
  recap_reg <- tdat[,.(recap.pred = sum(recap.pred, na.rm=TRUE), recap.obs=sum(recap.obs, na.rm=TRUE)), by=c("rel.region", "region", "recap.month")]
  recap_reg_sum <- tdat[,.(recap.pred.sum = sum(recap.pred, na.rm=TRUE), recap.obs.sum=sum(recap.obs, na.rm=TRUE)), by=c("rel.region", "recap.month")]
  
  # Merge together and get the proportion of recaptures 
  # i.e. the total number of tags from region 1 that were recaptured, found out the proportion that was recaptured in each region
  recap_reg <- merge(recap_reg, recap_reg_sum)
  recap_reg$pred.prop <- recap_reg$recap.pred / recap_reg$recap.pred.sum
  recap_reg$obs.prop <- recap_reg$recap.obs/ recap_reg$recap.obs.sum

  # Sanity check that it looks like data in Fig 33 of SKJ 2019 assessment report
  # Total released from 1 that were recaught in 1 quarter
  #subset(recap_reg_sum, recap.month==2 & rel.region==1)
  #subset(recap_reg, recap.month==2 & rel.region==1)

  # Plot of the difference between predicted and observed proportion of tags returned by region of release
  recap_reg$diff.prop <- recap_reg$obs.prop - recap_reg$pred.prop
  #recap_reg$diff.prop2 <- log(recap_reg$pred.prop / recap_reg$obs.prop)
  recap_reg$rel.region.name <- paste("Release region ", recap_reg$rel.region, sep="")
  recap_reg$Quarter <- as.factor((recap_reg$recap.month+1) / 3)
  
  return(recap_reg)
}


#' @param pdat Data from calling \code{prepare.tag.return.proportion.v2()}
#' @export
#' @rdname plot.tag.return.proportion.v2
#' @name Plot tag return proportion
generate.tag.return.proportion.v2 <- function(pdat, plot.type="point", save.dir, save.name){
  
  # Plot attempt 1 - Facet for each release region, plot difference in proportion
  # by recapture region, coloured by quarter
  
  # Stuff for dummy data
  no_grps <- length(unique(pdat$rel.region.name))
  ylims <- tapply(pdat$diff.prop, pdat$rel.region.name, function(x) c(max(abs(x), na.rm=T), -max(abs(x), na.rm=T)))
  dummydat <- data.frame(y=unlist(ylims), x=rep(c(min(pdat$region), max(pdat$region)), no_grps), rel.region.name = rep(names(ylims), each=2))
  dummydat$y <- dummydat$y * 1.1
  
  # Point plot
  if(plot.type == "point"){
    p <- ggplot2::ggplot(pdat, ggplot2::aes(x=as.factor(region), y=diff.prop))
    p <- p + ggplot2::geom_point(aes(colour = Quarter), size=4)
    p <- p + ggplot2::facet_wrap(~rel.region.name, ncol=2, scales="free")
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0.0), linetype=2)
    p <- p + ggthemes::theme_few()
    p <- p + ggplot2::xlab("Recapture region")
    p <- p + ggplot2::ylab("Observed proportion - predicted proportion")
    p <- p + ggplot2::geom_blank(data=dummydat, ggplot2::aes(x=x, y=y))
  }

  # Bar plot
  if(plot.type == "bar"){
    p <- ggplot2::ggplot(pdat, ggplot2::aes(x=as.factor(region), y=diff.prop))
    p <- p + ggplot2::geom_bar(stat="identity", ggplot2::aes(fill = Quarter), colour="black", position=ggplot2::position_dodge())
    p <- p + ggplot2::facet_wrap(~rel.region.name, ncol=2, scales="free")
    p <- p + ggplot2::xlab("Recapture region")
    p <- p + ggplot2::geom_hline(aes(yintercept=0.0))
    p <- p + ggplot2::ylab("Observed proportion - predicted proportion")
    p <- p + ggthemes::theme_few()
    p <- p + ggplot2::geom_blank(data=dummydat, ggplot2::aes(x=x, y=y))
  }
  
  save_plot(save.dir, save.name, plot=p)
  
  return(p)
}


