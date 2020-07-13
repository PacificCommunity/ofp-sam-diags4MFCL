# Likelihood profile plots

#' Plot the likelihood profiles
#' 
#' Plot the likelihood profiles using previously generated data for a single model.
#' The data generation is hidden away in other repos awaiting clean up.
#' It's based on Rob and Matt's code.
#' @param llmodel A data.frame of profile information for one model.
#' @param xrange Minimum and maximum of xrange. Default is to be set automatically.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' @import FLR4MFCL
#' @import magrittr
plot.likelihood.profiles <- function(llmodel, xrange=NULL, save.dir, save.name){
  
  pdat <- as.data.table(llmodel)
  pdatm <- melt(pdat, id.vars = c("Model", "depletion"))
  rep_dep <- pdat$sbsbf0latest[1]

  # Profiles to plot with better names
  profiles <- c("scl.tot.lik", "scl.eff.lik", "scl.len.lik", "scl.tag.lik", "scl.cat.lik")
  profile_df <- data.frame(variable = profiles, Variable=c("Total","CPUE","Length","Tag","Catch"))
  pdatm <- pdatm[variable %in% profiles]
  pdatm <- merge(pdatm, profile_df)
  
  p <- ggplot(pdatm, aes(x=depletion, y=value))
  p <- p + geom_line(aes(colour=Variable), size=2)
  p <- p + geom_vline(aes(xintercept=rep_dep), linetype=2)
  p <- p + xlab("SB/SBF=0") + ylab("Relative likelihood")
  if(!is.null(xrange)){
    p <- p + xlim(c(xrange[1], xrange[2]))
  }
  p <- p + ggthemes::theme_few()
  
  save_plot(save.dir, save.name, plot=p)
  
  return(p)
}