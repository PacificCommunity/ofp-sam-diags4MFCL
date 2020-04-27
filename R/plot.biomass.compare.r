

#' Compare the estimated biomass across different models.
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

plot.biomass.compare = function(rep.list,rep.names,agg.years = TRUE,agg.regions=TRUE,biomass.type = "SSB", palette.cols = c("royalblue3","deepskyblue1","gold","orange1","indianred1","firebrick2","#AC2020"),save.dir,save.name)
{
		names(rep.list) = rep.names

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
			g = plot.dt %>% 
			ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~region) +
			ggplot2::xlab("Year") +
			ggplot2::ggtitle("Estimated biomass (1000s mt)") +
			ggplot2::scale_y_continuous(name=ylab,breaks = pretty,limits=c(0,max(plot.dt$bio))) +
			ggplot2::geom_line(ggplot2::aes(x=time,y=bio,color=model),size=1.25) +
			ggplot2::scale_color_manual("Model",values=c("black",colorRampPalette(palette.cols)(length(rep.list)-1)))
		

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