#' Wrapper function for plotting model comparisons
#' 
#' @param rep.list A list of MFCLRep objects. The reference model should be listed first.
#' @param frq.list A list of MFCLFrq objects
#' @param par.list A list of MFCLPar objects
#' @param model.names A vector of character strings naming the models for plotting purposes
#' @param agg.years TRUE or FALSE. Should model outputs be aggregated to an annual time step.
#' @param agg.regions TRUE or FALSE. Should model outputs be aggregated across all regions are kept separate.
#' @param biomass.type Character string denoting the type of biomass plotted, 'SSB' or 'Total'
#' @param biomass.units Integer number denoting how many MT to divide the biomass by. Default is 1000.
#' @param fisheries.edev A vector of fishery indices to plot for the effort devs
#' @param recdist.year_range The year range to average the total recruitment over. Default is the last 10 years.
#' @param yaxis.free TRUE or FALSE. Default is FALSE. If TRUE and agg.regions is also TRUE than the y-axis scales will be independent across regions, otherwise they will be shared so regional scaling will be apparent.  
#' @param LRP Limit reference point. If not specified it is not plotted.
#' @param TRP Target reference point. If not specified it is not plotted.
#' @param palette.func A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param ... Passes extra arguments to the palette function. Use the argument all.model.colours to ensure consistency of model colours when plotting a subset of models.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' @import FLR4MFCL
#' 

plot.comparisons = function(rep.list,frq.list,par.list,model.names,agg.years = TRUE,agg.regions=TRUE,biomass.type = "SSB",biomass.units=1000,fisheries.edev=NULL,recdist.year_range=NULL,yaxis.free=FALSE,LRP=NULL, TRP=NULL,palette.func=default.model.colours,save.dir,save.name,...)
{
	if(!missing(save.dir))
	{
		if(missing(save.name))
		{
			stop("How can you save the output if you haven't specified the name? Please specify 'save.name'.")
		} else {
			if (! dir.exists(save.dir))dir.create(save.dir,recursive=TRUE)
			g1 = plot.depletion(rep.list,model.names,agg.years,agg.regions=FALSE,biomass.type=biomass.type,LRP=LRP,TRP=TRP,palette.func=palette.func,save.dir=save.dir,save.name=paste0("dep.reg-",save.name),...)
			g2 = plot.biomass(rep.list,model.names,agg.years,agg.regions=FALSE,biomass.type,biomass.units,yaxis.free,palette.func,save.dir,save.name=paste0("bio.reg-",save.name),...)
			if(agg.regions)
			{
				g11 = plot.depletion(rep.list,model.names,agg.years,agg.regions=TRUE,biomass.type,palette.func,save.dir,save.name=paste0("dep-",save.name),...)
				g22 = plot.biomass(rep.list,model.names,agg.years,agg.regions=TRUE,biomass.type,biomass.units,yaxis.free,palette.func,save.dir,save.name=paste0("bio-",save.name),...)
			} 
			
			g3 = plot.selectivity(rep.list,model.names,sel.basis="AGE",palette.func,fisheries=1:max(sapply(rep.list,function(x)dimensions(x)["fisheries"])),save.dir,save.name=paste0("sel.age-",save.name),...)
			g3 = plot.selectivity(rep.list,model.names,sel.basis="LENGTH",palette.func,fisheries=1:max(sapply(rep.list,function(x)dimensions(x)["fisheries"])),save.dir,save.name=paste0("sel.len-",save.name),...)
			g4 = plot.growth(rep.list,model.names,palette.func,save.dir,save.name=paste0("gro-",save.name),...)
			if(is.null(fisheries.edev))
			{
				fisheries.edev = 1:max(sapply(rep.list,function(x)dimensions(x)["fisheries"]))
			}
			g5 = plot.effort.devs(lapply(frq.list,function(x)realisations(x)), par.list, model.names, fisheries=fisheries.edev, fishery.names=as.character(fisheries), show.legend=TRUE, show.points=FALSE, palette.func=palette.func, save.dir=save.dir,save.name=paste0("edev-",save.name), ...)
			g6 = plot.srr(rep.list, model.names, show.legend=TRUE, palette.func=palette.func, save.dir=save.dir,save.name=paste0("srr-",save.name), ...)
			if(is.null(recdist.year_range))
			{
				recdist.year_range = (as.numeric(range(rep.list[[1]])["maxyear"])-9):as.numeric(range(rep.list[[1]])["maxyear"])
			}
			g7 = plot.rec.dist(rep.list, model.names, year_range = recdist.year_range, plot_type="violin", overlay_data=FALSE, palette.func=palette.func, save.dir=save.dir,save.name=paste0("rec.dist-",save.name), ...)
			g8 = plot.rec.devs(par.list, model.names, show.legend=TRUE, show.points=FALSE, palette.func=palette.func, save.dir=save.dir,save.name=paste0("rec.dev-",save.name), ...)
			g9 = plot.nat.mort(rep.list, model.names, show.legend=TRUE, linesize = 1, palette.func=palette.func, save.dir=save.dir,save.name=paste0("nat.M-",save.name), ...)
			LnBins = seq(from=lf_range(frq.list[[1]])["LFFirst"],by=lf_range(frq.list[[1]])["LFWidth"]*lf_range(frq.list[[1]])["LFFactor"],length.out=lf_range(frq.list[[1]])["LFIntervals"])
			g10 = plot.maturity(par.list, model.names, Length=FALSE,LnBins=LnBins, show.legend=TRUE, palette.func=palette.func , xlab="Age class",ylab="Reproductive output", LegLoc="bottomright", linesize=1, save.dir=save.dir,save.name=paste0("mat.age-",save.name), ...)	
			g11 = plot.maturity(par.list, model.names, Length=TRUE,LnBins=LnBins, show.legend=TRUE, palette.func=palette.func , xlab="Length class",ylab="Reproductive output", LegLoc="bottomright", linesize=1, save.dir=save.dir,save.name=paste0("mat.len-",save.name), ...)	

		}
	} else {
		stop("Must provide 'save.dir'.")
	} 
}