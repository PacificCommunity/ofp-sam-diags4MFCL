

#' Compare the estimated biomass across different models.
#' 
#' @param rep.list A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.
#' @param rep.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.
#' @param agg.years TRUE or FALSE. Should model outputs be aggregated to an annual time step.
#' @param agg.regions TRUE or FALSE. Should model outputs be aggregated across all regions are kept separate.
#' @param biomass.type Character string denoting the type of biomass plotted, 'SSB' or 'Total'
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
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_y_continuous
#' 

plot.biomass = function(rep.list,rep.names=NULL,agg.years = TRUE,agg.regions=TRUE,biomass.type = "SSB", palette.func=default.model.colours, save.dir, save.name, ...)
{
	  # Check and sanitise input MFCLRep arguments and names
    rep.list <- check.rep.args(rep=rep.list, rep.names=rep.names)
    rep.names <- names(rep.list)
    
		dt.list = as.list(rep(NA,length(rep.list)))
		names(dt.list) = names(rep.list)

		for(i in 1:length(dt.list))
		{
			tmp.rep = rep.list[[i]]
			tmp.name = names(rep.list)[i]
			if(agg.years)
			{
				if(agg.regions)
				{
					if(biomass.type == "SSB")
					{
						ylab = "Spawning potential"
						tmp.bio.array = adultBiomass(tmp.rep)@.Data[1,,1,,,1]
						tmp.bio = apply(tmp.bio.array,1,function(x)sum(colMeans(x)))/1000
						dt.list[[i]] = data.table::data.table(time=as.numeric(names(tmp.bio)),bio=tmp.bio,model=rep(tmp.name,length(tmp.bio)),region=rep("All regions",length(tmp.bio)))
						rm(list=c("tmp.bio","tmp.bio.array"))
					} else if(biomass.type == "Total"){
						stop("Invalid biomass.type. Option for 'Total' hasn't been developed yet")
					} else {
						stop("Invalid biomass.type. Please use either 'SSB' or 'Total'")
					}
				} else {
					stop("Disaggregated regions has not been developed as an option yet")
				}
			} else {
				stop("Disaggregated years has not been developed as an option yet")
			}

			# clean-up
			rm(list=c("tmp.rep","tmp.name"))
		}
		
		# combine into single data.table
			plot.dt = data.table::rbindlist(dt.list) %>% .[,model:=factor(as.character(model),levels=rep.names)]

		# make plot
			# Get the colours - if all.model.names passed in using ... then it is passed to palette func
			colour_values <- palette.func(selected.model.names = names(rep.list), ...)
			g = plot.dt %>% 
			ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~region) +
			ggplot2::xlab("Year") +
			ggplot2::ggtitle("Estimated biomass (1000s mt)") +
			ggplot2::scale_y_continuous(name=ylab,breaks = pretty,limits=c(0,max(plot.dt$bio))) +
			ggplot2::geom_line(ggplot2::aes(x=time,y=bio,color=model),size=1.25) +
			ggplot2::scale_color_manual("Model",values=colour_values)
		

		# write.out
		if(!missing(save.dir))
		{
			if(missing(save.name))
			{
				stop("How can you save the output if you haven't specified the directory? Please specify save.dir.")
			} else {
				if (! dir.exists(save.dir))dir.create(save.dir,recursive=TRUE)
				ggplot2::ggsave(paste0(save.name,".png"),plot=g, device = "png", path = save.dir,scale = 1, width = 9, height = 9, units = c("in"))
			}
		} 
			
		return(g)

}