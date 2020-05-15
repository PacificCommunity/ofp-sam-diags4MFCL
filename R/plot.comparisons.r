#' Wrapper function for plotting model comparisons
#' 
#' @param rep.list A list of MFCLRep objects. The reference model should be listed first.
#' @param rep.names A vector of character strings naming the models for plotting purposes
#' @param agg.years TRUE or FALSE. Should model outputs be aggregated to an annual time step.
#' @param agg.regions TRUE or FALSE. Should model outputs be aggregated across all regions are kept separate.
#' @param biomass.type Character string denoting the type of biomass plotted, 'SSB' or 'Total'
#' @param palette.cols A vector of character strings giving the colors to form a palette for differentiating between models. If wishing to use the exact colors give a vector of (length(rep.list) - 1) colors as the reference model is black by default.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' 

plot.comparisons = function(plot.name,rep.list,rep.names,agg.years = TRUE,agg.regions=TRUE,biomass.type = "SSB", palette.cols = c("royalblue3","deepskyblue1","gold","orange1","indianred1","firebrick2","#AC2020"),save.dir,save.name)
{
	if(!missing(save.dir))
	{
		if(missing(save.name))
		{
			stop("How can you save the output if you haven't specified the directory? Please specify save.dir.")
		} else {
			if (! dir.exists(save.dir))dir.create(save.dir,recursive=TRUE)
			g1 = plot.depletion(rep.list,rep.names,agg.years,agg.regions,biomass.type,palette.cols,save.dir,save.name=paste0("dep-",save.name))
			g2 = plot.biomass(rep.list,rep.names,agg.years,agg.regions,biomass.type,palette.cols,save.dir,save.name=paste0("bio-",save.name))
		}
	} else {
		stop("Must provide 'save.dir'.")
	} 
}