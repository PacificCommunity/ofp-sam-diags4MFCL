#' Wrapper function for plotting model comparisons
#' 
#' @param rep.list A list of MFCLRep objects. The reference model should be listed first.
#' @param model.names A vector of character strings naming the models for plotting purposes
#' @param agg.years TRUE or FALSE. Should model outputs be aggregated to an annual time step.
#' @param agg.regions TRUE or FALSE. Should model outputs be aggregated across all regions are kept separate.
#' @param sel.basis A character string indicating if selectivity at age ('AGE') or length ('Length') should be plotted
#' @param biomass.type Character string denoting the type of biomass plotted, 'SSB' or 'Total'
#' @param biomass.units Integer number denoting how many MT to divide the biomass by. Default is 1000.
#' @param yaxis.free TRUE or FALSE. Default is FALSE. If TRUE and agg.regions is also TRUE than the y-axis scales will be independent across regions, otherwise they will be shared so regional scaling will be apparent.  
#' @param palette.func A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param ... Passes extra arguments to the palette function. Use the argument all.model.colours to ensure consistency of model colours when plotting a subset of models.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' 

plot.comparisons = function(plot.name,rep.list,model.names,agg.years = TRUE,agg.regions=TRUE,sel.basis="AGE",biomass.type = "SSB",biomass.units=1000,yaxis.free=FALSE,palette.func=default.model.colours,save.dir,save.name,...)
{
	if(!missing(save.dir))
	{
		if(missing(save.name))
		{
			stop("How can you save the output if you haven't specified the name? Please specify 'save.name'.")
		} else {
			if (! dir.exists(save.dir))dir.create(save.dir,recursive=TRUE)
			g1 = plot.depletion(rep.list,model.names,agg.years,agg.regions,biomass.type,palette.func,save.dir,save.name=paste0("dep-",save.name),...)
			g2 = plot.biomass(rep.list,model.names,agg.years,agg.regions,biomass.type,biomass.units,yaxis.free,palette.func,save.dir,save.name=paste0("bio-",save.name),...)
			g3 = plot.selectivity(rep.list,model.names,sel.basis,palette.func,save.dir,save.name=paste0("sel-",tolower(sel.basis),"-",save.name),...)
			g4 = plot.growth(rep.list,model.names,palette.func,save.dir,save.name=paste0("gro-",save.name),...)
		}
	} else {
		stop("Must provide 'save.dir'.")
	} 
}