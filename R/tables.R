# Likelihood table
# Developed by R Scott 06 2020

#' Table of likelihood components
#' 
#' Returns a data.frame of the likelihood components. The likelihoods are taken from the 'test_plot_output' file which is output as part of the model fitting process.
#' The output can be processed with the \code{xtable} package to make a table for RMarkdown or Latex reports.
#' @param likelihood.list A named list of MFCLLikelihood objects (from reading in the 'test_plot_output' files), one for each model.
#' @param par.list A named list of MFCLPar objects, the same length and names as the likelihoods argument. If this argument is not supplied, the 'grad' column is not included in the returned data.frame.
#' @param npars Boolean, if TRUE also returns the number of parameters. Default is FALSE
#' @export
#' @import FLR4MFCL
likelihood.table <- function(likelihood.list, par.list,npars=FALSE){
  # Need to add some safety checks here for the object types
  
  # Scrape the likelihoods out of the list of likelihoods
  dfs <- lapply(likelihood.list, function(x){
    lls <- summary(x)
    out <- matrix(lls$likelihood, nrow=1, dimnames=list(NULL,lls$component))
    out <- as.data.frame(out)
    return(out)
  })
  lldf <- do.call("rbind", dfs)
  lldf$Model <- names(likelihood.list)
  
  # Rename and tidy up
  colnames(lldf)[colnames(lldf)=="bhsteep"] <- "BH Steepness"
  colnames(lldf)[colnames(lldf)=="effort_dev"] <- "Effort devs"
  colnames(lldf)[colnames(lldf)=="catchability_dev"] <- "Catchability devs"
  colnames(lldf)[colnames(lldf)=="length_comp"] <- "Length composition"
  colnames(lldf)[colnames(lldf)=="weight_comp"] <- "Weight composition"
  colnames(lldf)[colnames(lldf)=="tag_data"] <- "Tag data"
  colnames(lldf)[colnames(lldf)=="total"] <- "Total"
  lldf <- lldf[,c("Model", "BH Steepness", "Effort devs", "Catchability devs", "Length composition", "Weight composition", "Tag data", "Total")]
  
  # Add in the grad column if available
  if(!missing(par.list)){
    max_grads <- unlist(lapply(par.list, max_grad))
    if(npars)
    {
      npars <- unlist(lapply(par.list, n_pars))
      # Cannot assume order of the par list is the same as the likelihood list so safer to merge
      max_grad_df <- data.frame(Model=names(max_grads), maxgrad = max_grads, npar = npars)
      colnames(max_grad_df)[colnames(max_grad_df)=="maxgrad"] <- "Max Gradient"
      colnames(max_grad_df)[colnames(max_grad_df)=="npar"] <- "Parameters"
    } else {
        # Cannot assume order of the par list is the same as the likelihood list so safer to merge
      max_grad_df <- data.frame(Model=names(max_grads), maxgrad = max_grads)
      colnames(max_grad_df)[colnames(max_grad_df)=="maxgrad"] <- "Max Gradient"
    }
    
    lldf <- merge(lldf, max_grad_df)
  }
  return(lldf)
}

#' Table of stock status metrics
#' 
#' Returns a data.frame of terminal year stock status metrics for each model in the \code{rep.list}.
#' Metrics include SBSBF0latest, MSY, BSMY and FMSY.
#' The output can be processed with the \code{xtable} package to make a table for RMarkdown or Latex reports.
#' @param rep.list A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.
#' @param rep.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.
#' @export
#' @import FLR4MFCL
status.table <- function(rep.list,rep.names=NULL){
	  # Check and sanitise input MFCLRep arguments and names
    rep.list <- check.rep.args(rep=rep.list, rep.names=rep.names)
    rep.names <- names(rep.list)
    # Pull out the bits of interest
    df <- lapply(rep.list, function(x){
      sbsbf0 <- SBSBF0latest(x)
      # Pull out final year
      final_sbsbf0 <- c(sbsbf0[,dim(sbsbf0)[2]])
      out <- data.frame(SBSBF0 = final_sbsbf0, MSY = MSY(x), BMSY=BMSY(x), FMSY=FMSY(x))
      return(out)
    })
    df <- do.call("rbind", df)
    df <- cbind(data.frame(Model=rep.names), df)
    return(df)
}
