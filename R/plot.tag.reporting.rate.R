#' Plot tag reporting rates
#' 
#' The tag reporting rates are reported by group. A fishery may belong to multiple groups, depending on the tag release event.
#' Plots a box or violing plot of the reporting rates for chosen reporting rate groups for all models in the input the list.
#' Note that this plot assumes the reporting rates groups are the same for each model in the list.
#' 
#' @param par.list A list of MFCLPar objects or a single MFCLFrq that contain the effort deviations.
#' @param model.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the frqreal.list (if available) or generated automatically. Not really used here.
#' @param reporting.rate.groups A numerical vector of the reporting rate groups you want plotted. The vector can be named for more informative plot labels.
#' @param plot_type 'violin' or 'box'.
#' @param fill_colour The fill colour.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' @import FLR4MFCL
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_violin
plot.tag.reporting.rate <- function(par.list, model.names=NULL, reporting.rate.groups=NULL, plot_type="violin", fill_colour="lightblue", save.dir, save.name){
  
  # Argument check
  par.list <- check.par.args(par=par.list, par.names=model.names)
  
  # If no reporting groups supplied, use all of them
  if(is.null(reporting.rate.groups)){
    reporting.rate.groups <- sort(unique(c(tag_fish_rep_grp(par.list[[1]]))))
  }
  
  # If no reporting groups supplied, use numerical names
  if(is.null(names(reporting.rate.groups))){
    names(reporting.rate.groups) <- as.character(reporting.rate.groups)
  }
  
  # This assumes that each model has the same number of reporting rate groups between models
  # And that the names of the groups are the same between models
  no_grps <- length(unique(c(tag_fish_rep_grp(par.list[[1]]))))
  # Nested sapply for the win!
  # Sapply over the par list
  rates <- sapply(par.list, function(x){
    # Get the first value of each group
    tag_fish_rep_rate(x)[unlist(sapply(reporting.rate.groups, function(y) which(tag_fish_rep_grp(x)==y)[1]))]
  })
  rates <- as.data.table(rates)
  rates$taggrp <- factor(names(reporting.rate.groups), levels=names(reporting.rate.groups))
  # Reshape
  dat <- melt(rates, id.vars = "taggrp", variable.name = "model")
  
  # Order the grps - need to think about how to do this
  #dat$taggrp <- factor(dat$taggrp, levels=repgrp$Name)
  
  
  p <- ggplot2::ggplot(dat, ggplot2::aes(value, taggrp))
  if (plot_type=="box"){
    p <- p + ggplot2::geom_boxplot(fill=fill_colour)
  }
  if (plot_type=="violin"){
    p <- p + ggplot2::geom_violin(fill=fill_colour, scale="width") # Scale by width as it looks better
  }
  p <- p + ggthemes::theme_few()
  p <- p + ggplot2::ylab("Tag reporting rate group")
  p <- p + ggplot2::xlab("Reporting rate")
  p <- p + xlim(c(0,NA)) # Just fix minimum
  
  save_plot(save.dir, save.name, plot=p)
  
  return(p)
}
