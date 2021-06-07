

#' Compare the estimated depletion across different models.
#' 
#' @param rep.list A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.
#' @param rep.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.
#' @param agg.years TRUE or FALSE. Should model outputs be aggregated to an annual time step.
#' @param agg.regions TRUE or FALSE. Should model outputs be aggregated across all regions are kept separate.
#' @param biomass.type Character string denoting the type of biomass plotted, 'SSB' or 'Total'
#' @param LRP Limit reference point. If not specified it is not plotted.
#' @param TRP Target reference point. If not specified it is not plotted.
#' @param palette.func A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @param ... Passes extra arguments to the palette function. Use the argument all.model.colours to ensure consistency of model colours when plotting a subset of models.
#' @export
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom data.table data.table
#' @importFrom data.table rbindlist
#' @importFrom data.table setnames
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
#' @importFrom ggplot2 geom_hline
#' 

	plot.depletion = function(rep.list,rep.names=NULL,agg.years = TRUE,agg.regions=TRUE,biomass.type = "SSB", LRP=NULL, TRP=NULL, palette.func=default.model.colours, save.dir,save.name, ...)
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
						ylab = expression("SB"/"SB"["F=0"])

						tmp.bio = data.table::as.data.table(adultBiomass(tmp.rep)) %>% .[,.(bio=mean(value)),by=.(year,area)] %>% .[,.(bio=sum(bio)),by=.(year)] %>%
								  data.table::setnames(.,c("year"),c("time")) %>% .[,model:=tmp.name] %>% .[,region:="All regions"]
						tmp.bio_nf = data.table::as.data.table(adultBiomass_nofish(tmp.rep)) %>% .[,.(bio_nf=mean(value)),by=.(year,area)] %>% .[,.(bio_nf=sum(bio_nf)),by=.(year)] %>%
								  data.table::setnames(.,c("year"),c("time")) %>% .[,model:=tmp.name] %>% .[,region:="All regions"]
						dt.list[[i]] = merge(tmp.bio,tmp.bio_nf) %>% .[,dep:=bio/bio_nf] %>% .[,.(time,bio,bio_nf,dep,model,region)]  %>% .[,time:=as.numeric(time)]
						rm(list=c("tmp.bio","tmp.bio_nf"))
					} else if(biomass.type == "Total"){
						ylab = expression("B"/"B"["F=0"])
						tmp.bio = data.table::as.data.table(totalBiomass(tmp.rep)) %>% .[,.(bio=mean(value)),by=.(year,area)] %>% .[,.(bio=sum(bio)),by=.(year)] %>%
								  data.table::setnames(.,c("year"),c("time")) %>% .[,model:=tmp.name] %>% .[,region:="All regions"]
						tmp.bio_nf = data.table::as.data.table(totalBiomass_nofish(tmp.rep)) %>% .[,.(bio_nf=mean(value)),by=.(year,area)] %>% .[,.(bio_nf=sum(bio_nf)),by=.(year)] %>%
								  data.table::setnames(.,c("year"),c("time")) %>% .[,model:=tmp.name] %>% .[,region:="All regions"]
						dt.list[[i]] = merge(tmp.bio,tmp.bio_nf) %>% .[,dep:=bio/bio_nf] %>% .[,.(time,bio,bio_nf,dep,model,region)]  %>% .[,time:=as.numeric(time)]
						rm(list=c("tmp.bio","tmp.bio_nf"))
					} else {
						stop("Invalid biomass.type. Please use either 'SSB' or 'Total'")
					}
				} else {
					if(biomass.type == "SSB")
					{
						ylab = expression("SB"/"SB"["F=0"])

						tmp.bio = data.table::as.data.table(adultBiomass(tmp.rep)) %>% .[,.(bio=mean(value)),by=.(year,area)] %>%
								  data.table::setnames(.,c("year","area"),c("time","region")) %>% .[,model:=tmp.name]
						tmp.bio_nf = data.table::as.data.table(adultBiomass_nofish(tmp.rep)) %>% .[,.(bio_nf=mean(value)),by=.(year,area)] %>%
								  data.table::setnames(.,c("year","area"),c("time","region")) %>% .[,model:=tmp.name]
						dt.list[[i]] = merge(tmp.bio,tmp.bio_nf) %>% .[,dep:=bio/bio_nf] %>% .[,.(time,bio,bio_nf,dep,model,region)]  %>% .[,time:=as.numeric(time)]

						rm(list=c("tmp.bio","tmp.bio_nf"))
					} else if(biomass.type == "Total"){
						ylab = expression("B"/"B"["F=0"])
						tmp.bio = data.table::as.data.table(totalBiomass(tmp.rep)) %>% .[,.(bio=mean(value)),by=.(year,area)] %>%
								  data.table::setnames(.,c("year","area"),c("time","region")) %>% .[,model:=tmp.name]
						tmp.bio_nf = data.table::as.data.table(totalBiomass_nofish(tmp.rep)) %>% .[,.(bio_nf=mean(value)),by=.(year,area)] %>%
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
						ylab = expression("SB"/"SB"["F=0"])

						tmp.bio = data.table::as.data.table(adultBiomass(tmp.rep)) %>% .[,.(bio=sum(value)),by=.(year,season)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>%
								  .[,region:="All regions"] %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
						tmp.bio_nf = data.table::as.data.table(adultBiomass_nofish(tmp.rep)) %>% .[,.(bio_nf=sum(value)),by=.(year,season)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>%
								  .[,region:="All regions"] %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
						dt.list[[i]] = merge(tmp.bio,tmp.bio_nf) %>% .[,dep:=bio/bio_nf] %>% .[,.(time,bio,bio_nf,dep,model,region)]  %>% .[,time:=as.numeric(time)]

						rm(list=c("tmp.bio","tmp.bio_nf"))
					} else if(biomass.type == "Total"){
						ylab = expression("B"/"B"["F=0"])

						tmp.bio = data.table::as.data.table(totalBiomass(tmp.rep)) %>% .[,.(bio=sum(value)),by=.(year,season)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>%
								  .[,region:="All regions"] %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
						tmp.bio_nf = data.table::as.data.table(totalBiomass_nofish(tmp.rep)) %>% .[,.(bio_nf=sum(value)),by=.(year,season)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>%
								  .[,region:="All regions"] %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
						dt.list[[i]] = merge(tmp.bio,tmp.bio_nf) %>% .[,dep:=bio/bio_nf] %>% .[,.(time,bio,bio_nf,dep,model,region)]  %>% .[,time:=as.numeric(time)]

						rm(list=c("tmp.bio","tmp.bio_nf"))
					} else {
						stop("Invalid biomass.type. Please use either 'SSB' or 'Total'")
					}
				} else {
					if(biomass.type == "SSB")
					{
						ylab = expression("SB"/"SB"["F=0"])

						tmp.bio = data.table::as.data.table(adultBiomass(tmp.rep)) %>% .[,.(bio=mean(value)),by=.(year,season,area)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>%
								  data.table::setnames(.,c("area"),c("region")) %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
						tmp.bio_nf = data.table::as.data.table(adultBiomass_nofish(tmp.rep)) %>% .[,.(bio_nf=mean(value)),by=.(year,season,area)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>%
								  data.table::setnames(.,c("area"),c("region")) %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
						dt.list[[i]] = merge(tmp.bio,tmp.bio_nf) %>% .[,dep:=bio/bio_nf] %>% .[,.(time,bio,bio_nf,dep,model,region)]  %>% .[,time:=as.numeric(time)]

						rm(list=c("tmp.bio","tmp.bio_nf"))
					} else if(biomass.type == "Total"){
						ylab = expression("B"/"B"["F=0"])
						tmp.bio = data.table::as.data.table(totalBiomass(tmp.rep)) %>% .[,.(bio=mean(value)),by=.(year,season,area)] %>% .[,year:=as.numeric(year)] %>% .[,seasons:=as.numeric(seasons)] %>%
								  data.table::setnames(.,c("area"),c("region")) %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
						tmp.bio_nf = data.table::as.data.table(totalBiomass_nofish(tmp.rep)) %>% .[,.(bio_nf=mean(value)),by=.(year,season,area)] %>% .[,year:=as.numeric(year)] %>% .[,seasons:=as.numeric(seasons)] %>%
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
			g = plot.dt %>% 
			ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~region) +
			#ggplot2::geom_hline(yintercept=c(0.2,0.4),color="gray70",linetype="longdash") +
			ggplot2::geom_hline(yintercept=0) +
			ggplot2::xlab("Year") +
			ggplot2::scale_y_continuous(name=ylab,breaks = seq(0, 1, by = 0.20),limits=c(0,1)) +
			ggplot2::ggtitle(paste0("Estimated depletion - ",mlab)) +
			ggplot2::geom_line(ggplot2::aes(x=time,y=dep,color=model),size=1.25) +
			ggplot2::scale_color_manual("Model",values=colour_values)
			if (!is.null(LRP)){
  			g <- g + ggplot2::geom_hline(yintercept=c(LRP),color="gray70",linetype="longdash")
			}
			if (!is.null(TRP)){
  			g <- g + ggplot2::geom_hline(yintercept=c(TRP),color="gray70",linetype="longdash")
			}
		
		# write.out
		if(!missing(save.dir))
		{
			if(missing(save.name))
			{
				stop("How can you save the output if you haven't specified the directory? Please specify save.dir.")
			} else {
				if (! dir.exists(save.dir))dir.create(save.dir,recursive=TRUE)
				ggplot2::ggsave(paste0(save.name,".png"),plot=g, device = "png", path = save.dir,scale = 1, width = 16, height = 9, units = c("in"))
			}
		} 
			
		return(g)

	}