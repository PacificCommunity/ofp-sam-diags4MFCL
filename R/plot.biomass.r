

#' Compare the estimated biomass across different models.
#' 
#' @param rep.list A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.
#' @param rep.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.
#' @param agg.years TRUE or FALSE. Should model outputs be aggregated to an annual time step.
#' @param agg.regions TRUE or FALSE. Should model outputs be aggregated across all regions are kept separate.
#' @param biomass.type Character string denoting the type of biomass plotted, 'SSB' or 'Total'
#' @param biomass.units Integer number denoting how many MT to divide the biomass by. Default is 1000.
#' @param yaxis.free TRUE or FALSE. Default is FALSE. If TRUE and agg.regions is also TRUE than the y-axis scales will be independent across regions, otherwise they will be shared so regional scaling will be apparent.  
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

plot.biomass = function(rep.list,rep.names=NULL,agg.years = TRUE,agg.regions=TRUE,biomass.type = "SSB",biomass.units = 1000,yaxis.free=FALSE, palette.func=default.model.colours, save.dir, save.name, ...)
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
				mlab = "Annual"
				if(agg.regions)
				{
					if(biomass.type == "SSB")
					{
						ylab = "Spawning Potential"

						tmp.bio = data.table::as.data.table(adultBiomass(tmp.rep)) %>% .[,.(bio=mean(value)/biomass.units),by=.(year,area)] %>% .[,.(bio=sum(bio)),by=.(year)] %>%
								  data.table::setnames(.,c("year"),c("time")) %>% .[,model:=tmp.name] %>% .[,region:="All regions"]
						tmp.bio_nf = data.table::as.data.table(adultBiomass_nofish(tmp.rep)) %>% .[,.(bio_nf=mean(value)/biomass.units),by=.(year,area)] %>% .[,.(bio_nf=sum(bio_nf)),by=.(year)] %>%
								  data.table::setnames(.,c("year"),c("time")) %>% .[,model:=tmp.name] %>% .[,region:="All regions"]
						dt.list[[i]] = merge(tmp.bio,tmp.bio_nf) %>% .[,dep:=bio/bio_nf] %>% .[,.(time,bio,bio_nf,dep,model,region)]  %>% .[,time:=as.numeric(time)]
						rm(list=c("tmp.bio","tmp.bio_nf"))
					} else if(biomass.type == "Total"){
						ylab = "Total Biomass"
						tmp.bio = data.table::as.data.table(totalBiomass(tmp.rep)) %>% .[,.(bio=mean(value)/biomass.units),by=.(year,area)] %>% .[,.(bio=sum(bio)),by=.(year)] %>%
								  data.table::setnames(.,c("year"),c("time")) %>% .[,model:=tmp.name] %>% .[,region:="All regions"]
						tmp.bio_nf = data.table::as.data.table(totalBiomass_nofish(tmp.rep)) %>% .[,.(bio_nf=mean(value)/biomass.units),by=.(year,area)] %>% .[,.(bio_nf=sum(bio_nf)),by=.(year)] %>%
								  data.table::setnames(.,c("year"),c("time")) %>% .[,model:=tmp.name] %>% .[,region:="All regions"]
						dt.list[[i]] = merge(tmp.bio,tmp.bio_nf) %>% .[,dep:=bio/bio_nf] %>% .[,.(time,bio,bio_nf,dep,model,region)]  %>% .[,time:=as.numeric(time)]
						rm(list=c("tmp.bio","tmp.bio_nf"))
					} else {
						stop("Invalid biomass.type. Please use either 'SSB' or 'Total'")
					}
				} else {
					if(biomass.type == "SSB")
					{
						ylab = "Spawning Potential"

						tmp.bio = data.table::as.data.table(adultBiomass(tmp.rep)) %>% .[,.(bio=mean(value)/biomass.units),by=.(year,area)] %>%
								  data.table::setnames(.,c("year","area"),c("time","region")) %>% .[,model:=tmp.name]
						tmp.bio_nf = data.table::as.data.table(adultBiomass_nofish(tmp.rep)) %>% .[,.(bio_nf=mean(value)/biomass.units),by=.(year,area)] %>%
								  data.table::setnames(.,c("year","area"),c("time","region")) %>% .[,model:=tmp.name]
						dt.list[[i]] = merge(tmp.bio,tmp.bio_nf) %>% .[,dep:=bio/bio_nf] %>% .[,.(time,bio,bio_nf,dep,model,region)]  %>% .[,time:=as.numeric(time)]

						rm(list=c("tmp.bio","tmp.bio_nf"))
					} else if(biomass.type == "Total"){
						ylab = "Total Biomass"
						tmp.bio = data.table::as.data.table(totalBiomass(tmp.rep)) %>% .[,.(bio=mean(value)/biomass.units),by=.(year,area)] %>%
								  data.table::setnames(.,c("year","area"),c("time","region")) %>% .[,model:=tmp.name]
						tmp.bio_nf = data.table::as.data.table(totalBiomass_nofish(tmp.rep)) %>% .[,.(bio_nf=mean(value)/biomass.units),by=.(year,area)] %>%
								  data.table::setnames(.,c("year","area"),c("time","region")) %>% .[,model:=tmp.name]
						dt.list[[i]] = merge(tmp.bio,tmp.bio_nf) %>% .[,dep:=bio/bio_nf] %>% .[,.(time,bio,bio_nf,dep,model,region)]  %>% .[,time:=as.numeric(time)]

						rm(list=c("tmp.bio","tmp.bio_nf"))
					} else {
						stop("Invalid biomass.type. Please use either 'SSB' or 'Total'")
					}
				}
			} else {
				mlab = "Seasonal"
				if(agg.regions)
				{
					if(biomass.type == "SSB")
					{
						ylab = "Spawning Potential"

						tmp.bio = data.table::as.data.table(adultBiomass(tmp.rep)) %>% .[,.(bio=sum(value)/biomass.units),by=.(year,season)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>%
								  .[,region:="All regions"] %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
						tmp.bio_nf = data.table::as.data.table(adultBiomass_nofish(tmp.rep)) %>% .[,.(bio_nf=sum(value)/biomass.units),by=.(year,season)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>%
								  .[,region:="All regions"] %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
						dt.list[[i]] = merge(tmp.bio,tmp.bio_nf) %>% .[,dep:=bio/bio_nf] %>% .[,.(time,bio,bio_nf,dep,model,region)]  %>% .[,time:=as.numeric(time)]

						rm(list=c("tmp.bio","tmp.bio_nf"))
					} else if(biomass.type == "Total"){
						ylab = "Total Biomass"

						tmp.bio = data.table::as.data.table(totalBiomass(tmp.rep)) %>% .[,.(bio=sum(value)/biomass.units),by=.(year,season)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>%
								  .[,region:="All regions"] %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
						tmp.bio_nf = data.table::as.data.table(totalBiomass_nofish(tmp.rep)) %>% .[,.(bio_nf=sum(value)/biomass.units),by=.(year,season)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>%
								  .[,region:="All regions"] %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
						dt.list[[i]] = merge(tmp.bio,tmp.bio_nf) %>% .[,dep:=bio/bio_nf] %>% .[,.(time,bio,bio_nf,dep,model,region)]  %>% .[,time:=as.numeric(time)]

						rm(list=c("tmp.bio","tmp.bio_nf"))
					} else {
						stop("Invalid biomass.type. Please use either 'SSB' or 'Total'")
					}
				} else {
					if(biomass.type == "SSB")
					{
						ylab = "Spawning Potential"

						tmp.bio = data.table::as.data.table(adultBiomass(tmp.rep)) %>% .[,.(bio=mean(value)/biomass.units),by=.(year,season,area)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>%
								  data.table::setnames(.,c("area"),c("region")) %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
						tmp.bio_nf = data.table::as.data.table(adultBiomass_nofish(tmp.rep)) %>% .[,.(bio_nf=mean(value)/biomass.units),by=.(year,season,area)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>%
								  data.table::setnames(.,c("area"),c("region")) %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
						dt.list[[i]] = merge(tmp.bio,tmp.bio_nf) %>% .[,dep:=bio/bio_nf] %>% .[,.(time,bio,bio_nf,dep,model,region)]  %>% .[,time:=as.numeric(time)]

						rm(list=c("tmp.bio","tmp.bio_nf"))
					} else if(biomass.type == "Total"){
						ylab = "Total Biomass"
						tmp.bio = data.table::as.data.table(totalBiomass(tmp.rep)) %>% .[,.(bio=mean(value)/biomass.units),by=.(year,season,area)] %>% .[,year:=as.numeric(year)] %>% .[,seasons:=as.numeric(seasons)] %>%
								  data.table::setnames(.,c("area"),c("region")) %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
						tmp.bio_nf = data.table::as.data.table(totalBiomass_nofish(tmp.rep)) %>% .[,.(bio_nf=mean(value)/biomass.units),by=.(year,season,area)] %>% .[,year:=as.numeric(year)] %>% .[,seasons:=as.numeric(seasons)] %>%
								  data.table::setnames(.,c("year","area"),c("time","region")) %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
						dt.list[[i]] = merge(tmp.bio,tmp.bio_nf) %>% .[,dep:=bio/bio_nf] %>% .[,.(time,bio,bio_nf,dep,model,region)]  %>% .[,time:=as.numeric(time)]


						rm(list=c("tmp.bio","tmp.bio_nf"))
					} else {
						stop("Invalid biomass.type. Please use either 'SSB' or 'Total'")
					}
				}
			}

			# clean-up
			rm(list=c("tmp.rep","tmp.name"))
		}
		
		# combine into single data.table
			plot.dt = data.table::rbindlist(dt.list) %>% .[,model:=factor(as.character(model),levels=rep.names)]

		# make plot
			# Get the colours - if all.model.names passed in using ... then it is passed to palette func
			colour_values <- palette.func(selected.model.names = names(rep.list), ...)
			if(yaxis.free)
			{
				g = plot.dt %>% 
				ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~region,scales="free_y") +
				ggplot2::xlab("Year") +
				ggplot2::ggtitle(paste0("Estimated biomass (",formatC(biomass.units, format="f", big.mark=",", digits=0),"s mt) - ",mlab)) +
				ggplot2::geom_line(ggplot2::aes(x=time,y=bio,color=model),size=1.25) +
				ggplot2::scale_color_manual("Model",values=colour_values)
			} else {
				g = plot.dt %>% 
				ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~region) +
				ggplot2::xlab("Year") +
				ggplot2::ggtitle(paste0("Estimated biomass (",formatC(biomass.units, format="f", big.mark=",", digits=0),"s mt) - ",mlab)) +
				ggplot2::scale_y_continuous(name=ylab,breaks = pretty,limits=c(0,max(plot.dt$bio))) +
				ggplot2::geom_line(ggplot2::aes(x=time,y=bio,color=model),size=1.25) +
				ggplot2::scale_color_manual("Model",values=colour_values)
			}


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