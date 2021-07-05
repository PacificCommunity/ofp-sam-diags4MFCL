# Natural mortality plot - simple

#' Plot natural mortality of each model
#'
#' Plots the natural mortality of each age class of each model
#' @param rep.list A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.
#' @param rep.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.
#' @param show.legend Do you want to show the plot legend, TRUE (default) or FALSE.
#' @param palette.func A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param linesize The size of the lines. Default is 1.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @param ... Passes extra arguments to the palette function. Use the argument all.model.colours to ensure consistency of model colours when plotting a subset of models.
#' @export
#' @import FLR4MFCL
#' @import magrittr
plot.nat.mort <- function(rep.list, rep.names=NULL, show.legend=TRUE, linesize = 1, palette.func=default.model.colours,  save.dir, save.name, ...){
  # Check and sanitise input MFCLRep arguments and names
  rep.list <- check.rep.args(rep=rep.list, rep.names=rep.names)
  rep.names <- names(rep.list)

  pdat <- lapply(1:length(rep.list), function(x){
    m_at_age <- m_at_age(rep.list[[x]])
    m_at_age <- as.data.frame(FLQuant(m_at_age, dimnames=list(age=1:length(m_at_age))))[,c("age","data")]
    m_at_age$model <- names(rep.list)[x]
    return(m_at_age)
  })

  pdat <- do.call("rbind", pdat)
  # Tidy up data
  ## pdat$model <- rep(rep.names, each=dim(pdat)[1] / length(rep.names))

  # Want pdat to have Model names in the original order - important for plotting order
  pdat$model <- factor(pdat$model, levels=names(rep.list))

  # Plot it
  colour_values <- palette.func(selected.model.names = names(rep.list), ...)
  p <- ggplot2::ggplot(pdat, ggplot2::aes(x=age, y=data))
  p <- p + ggplot2::geom_line(ggplot2::aes(colour=model), size=linesize)
  p <- p + ggplot2::scale_color_manual("Model",values=colour_values)
  p <- p + ggplot2::xlab("Age class") + ggplot2::ylab("Natural mortality")
  p <- p + ggplot2::ylim(c(0,NA))
  p <- p + ggthemes::theme_few()
  if(show.legend==FALSE){
    p <- p + ggplot2::theme(legend.position = "none")
  }

  save_plot(save.dir, save.name, plot=p)

  return(p)
}
