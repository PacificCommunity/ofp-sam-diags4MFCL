## plot.maturity.par Plots the maturity-at-age function as printed out in the par file. Requires Rob Scott's package FLR4MFCl to run and uses the read.MFCLBio function.
## written by Matthew Vincent 03/2019

#' Plot maturity for each model give choice of by length or age
#'
#' Plots the natural mortality by length bin or age class for each mode
#' @param pars a list of MFCLPar objects or a single MFCLPar object. The reference model should be the first in the list.
#' @param par.names an optional vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the par.list (if available) or generated automatically.
#' @param Length a boolean to determine whether to plot maturity at age (default) or maturity at length
#' @param LnBins a vector of length bin values
#' @param show.legend Do you want to show the plot legend, TRUE (default) or FALSE.
#' @param palette.func A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param linesize Size of line.
#' @param xlab Name to display on the x axis label
#' @param ylab Name to display on the y axis label
#' @param LegLon legend location if show.legen is TRUE
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @param ... Passes extra arguments to the palette function. Use the argument all.model.colours to ensure consistency of model colours when plotting a subset of models.
#' @export
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom ggthemes theme_few
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 theme

plot.maturity <- function (pars, par.names=NULL, Length=FALSE,LnBins, show.legend=TRUE, palette.func=default.model.colours , xlab="Age class",ylab="Reproductive output", LegLoc="bottomright", linesize=1, save.dir, save.name, ...){
    pars <- check.par.args(par=pars, par.names=par.names)
    par.names <- names(pars)

    if (Length) {
        if(missing(LnBins)) stop("You need to supply the length bins (LnBins) to serve as the x-axis")
        dat <-  lapply(1:length(pars), function(x){
            mat=mat_at_length(pars[[x]])
            if(length(LnBins)!=length(mat)) stop("The length of Lnbins does not match the length of the maturity at length")
            dat=data.frame(var=LnBins,mat=mat,model=par.names[x])
            return(dat)
        })
    } else{
        dat <-lapply(1:length(pars), function(x){
            mat=mat(pars[[x]])
            dat=data.frame(var=1:length(mat),mat=mat,model=par.names[[x]])
            return(dat)
        })
    }
    dat <- do.call("rbind", dat)
    
    # Want dat to have Model names in the original order - important for plotting order
    dat$model <- factor(dat$model, levels=names(pars))
    
    # Plot it
    colour_values <- palette.func(selected.model.names = names(pars), ...)
    p <- ggplot2::ggplot(dat, ggplot2::aes(x=var, y=mat))
    p <- p + ggplot2::geom_line(ggplot2::aes(colour=model), size=linesize)
    p <- p + ggplot2::scale_color_manual("Model",values=colour_values)
    p <- p + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)
    p <- p + ggplot2::coord_cartesian(ylim=c(0,1.05))
    p <- p + ggthemes::theme_few()
    if(show.legend==FALSE){
        p <- p + ggplot2::theme(legend.position = "none")
    } else {
        p <- p + ggplot2::theme(legend.position = "LegLoc")
    }

    save_plot(save.dir, save.name, plot=p)

    return(p)
}
