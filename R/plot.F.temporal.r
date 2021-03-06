

#' Compare the fishing mortality across different models.
#' 
#' @param rep.list A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.
#' @param par.list Optional. A list of MFCLPar objects or a single MFCLPar object. Used for plotting juvenile and adult fishing mortality. If specified then agg.ages is ignored
#' @param rep.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.
#' @param agg.years TRUE or FALSE. Should model outputs be aggregated to an annual time step.
#' @param agg.regions TRUE or FALSE. Should model outputs be aggregated across all regions are kept separate.
#' @param agg.ages A vector of age classes to average F over. Default is all age classes.
#' @param yaxis.free TRUE or FALSE. Default is FALSE. If TRUE and agg.regions is also TRUE than the y-axis scales will be independent across regions, otherwise they will be shared so regional scaling will be apparent.  
#' @param palette.func A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @param ... Passes extra arguments to the palette function. Use the argument all.model.colours to ensure consistency of model colours when plotting a subset of models.
#' @export
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom data.table as.data.table
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
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 geom_hline
#' 

	plot.F.temporal = function(rep.list,par.list=NULL,rep.names=NULL,agg.years = TRUE,agg.regions=TRUE,agg.ages=NULL, yaxis.free = FALSE, palette.func=default.model.colours, save.dir,save.name, ...)
	{
	  
	  # Check and sanitise input MFCLRep arguments and names
    rep.list <- check.rep.args(rep=rep.list, rep.names=rep.names)
    rep.names <- names(rep.list)
	  
		dt.list = as.list(rep(NA,length(rep.list)))
		names(dt.list) = names(rep.list)

		if(is.null(par.list))
		{
			if(is.null(agg.ages))
			{
				agg.ages = 1:length(m_at_age(rep.list[[1]]))
			}


			for(i in 1:length(dt.list))
			{
				tmp.rep = rep.list[[i]]
				tmp.name = names(rep.list)[i]
				if(agg.years)
				{
					mlab = "Annual"
					if(agg.regions)
					{
						dt.list[[i]] = data.table::as.data.table(fm(tmp.rep)) %>% merge(., data.table::as.data.table(popN(tmp.rep)),by=c("age","year","unit","season","area","iter")) %>%
									  data.table::setnames(.,c("value.x","value.y"),c("f","N")) %>%
									  .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,area:=as.numeric(area)] %>%
									  .[,dead:=f*N] %>%
									  .[age %in% agg.ages,.(dead=sum(dead),N=sum(N)),by=.(year,season)] %>% .[,F:=(dead/N)] %>% .[,.(F=mean(F)*4),by=year] %>%
									  data.table::setnames(.,c("year"),c("time")) %>% .[,model:=tmp.name] %>% .[,region:="All regions"]
						
					} else {
						dt.list[[i]] = data.table::as.data.table(fm(tmp.rep)) %>% merge(., data.table::as.data.table(popN(tmp.rep)),by=c("age","year","unit","season","area","iter")) %>%
									  data.table::setnames(.,c("value.x","value.y"),c("f","N")) %>%
									  .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,area:=as.numeric(area)] %>%
									  .[,dead:=f*N] %>%
									  .[age %in% agg.ages,.(dead=sum(dead),N=sum(N)),by=.(year,season,area)] %>% .[,F:=(dead/N)] %>% .[,.(F=mean(F)*4),by=.(year,area)] %>%
									  data.table::setnames(.,c("year","area"),c("time","region")) %>% .[,model:=tmp.name]
					}
				} else {
					mlab = "Seasonal"
					if(agg.regions)
					{
						dt.list[[i]] = data.table::as.data.table(fm(tmp.rep)) %>% merge(., data.table::as.data.table(popN(tmp.rep)),by=c("age","year","unit","season","area","iter")) %>%
									  data.table::setnames(.,c("value.x","value.y"),c("f","N")) %>%
									  .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,area:=as.numeric(area)] %>%
									  .[,dead:=f*N] %>%
									  .[age %in% agg.ages,.(dead=sum(dead),N=sum(N)),by=.(year,season)] %>% .[,F:=(dead/N)] %>%
									  .[,model:=tmp.name] %>% .[,region:="All regions"] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]

						
					} else {
						dt.list[[i]] = data.table::as.data.table(fm(tmp.rep)) %>% merge(., data.table::as.data.table(popN(tmp.rep)),by=c("age","year","unit","season","area","iter")) %>%
									  data.table::setnames(.,c("value.x","value.y"),c("f","N")) %>%
									  .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,area:=as.numeric(area)] %>%
									  .[,dead:=f*N] %>%
									  .[age %in% agg.ages,.(dead=sum(dead),N=sum(N)),by=.(year,season,area)] %>% .[,F:=(dead/N)] %>%
									  data.table::setnames(.,c("area"),c("region")) %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]

						
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
				ggplot2::ggplot() + ggthemes::theme_few() + 
				ggplot2::geom_hline(yintercept=0) +
				ggplot2::xlab("Year") + ggplot2::ylab("Fishing mortality") + 
				ggplot2::ggtitle(paste0(mlab, " average F, age classes: ",paste0(range(agg.ages),collapse="-"))) +
				ggplot2::geom_line(ggplot2::aes(x=time,y=F,color=model),size=1) +
				ggplot2::scale_color_manual("Model",values=colour_values)
				if(yaxis.free)
				{
					g = g + ggplot2::facet_wrap(~region,scales="free_y")
				} else {
					g = g + ggplot2::facet_wrap(~region)
				}
		} else {
			if(!is.null(agg.ages))
			{
				print("You have supplied the par file so agg.ages will be ignored. Juvenile and adult F will be plotted instead.")
			}

			par.list <- check.par.args(par=par.list, par.names=rep.names)
    		par.names <- names(rep.list)

    					for(i in 1:length(dt.list))
						{
							tmp.rep = rep.list[[i]]
							tmp.par = par.list[[i]]
							tmp.name = names(rep.list)[i]

							prop.adult = mat(tmp.par) 
							# find the maximum and make the rest also = 1
 							prop.adult[rev(order(prop.adult))[1]:length(prop.adult)] =  1
 							prop.juv = 1-prop.adult


							if(agg.years)
							{
								mlab = "Annual"
								if(agg.regions)
								{
									dt.list[[i]] = data.table::as.data.table(fm(tmp.rep)) %>% merge(., data.table::as.data.table(popN(tmp.rep)),by=c("age","year","unit","season","area","iter")) %>%
												  data.table::setnames(.,c("value.x","value.y"),c("f","N")) %>%
												  .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,area:=as.numeric(area)] %>%
												  .[,juv:=prop.juv[age]*N] %>% .[,adult:=prop.adult[age]*N] %>% .[,dead.juv:=f*juv] %>% .[,dead.adult:=f*adult] %>%
												  .[,.(dead.juv=sum(dead.juv),juv=sum(juv),dead.adult=sum(dead.adult),adult=sum(adult)),by=.(year,season)] %>%
												  .[,F.juv:=(dead.juv/juv)] %>% .[,F.adult:=(dead.adult/adult)] %>% .[,.(F.juv=mean(F.juv)*4,F.adult=mean(F.adult)*4),by=year] %>%
												  data.table::setnames(.,c("year"),c("time")) %>% .[,model:=tmp.name] %>% .[,region:="All regions"]
									
								} else {
									dt.list[[i]] = data.table::as.data.table(fm(tmp.rep)) %>% merge(., data.table::as.data.table(popN(tmp.rep)),by=c("age","year","unit","season","area","iter")) %>%
												  data.table::setnames(.,c("value.x","value.y"),c("f","N")) %>%
												  .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,area:=as.numeric(area)] %>%
												  .[,juv:=prop.juv[age]*N] %>% .[,adult:=prop.adult[age]*N] %>% .[,dead.juv:=f*juv] %>% .[,dead.adult:=f*adult] %>%
												  .[,.(dead.juv=sum(dead.juv),juv=sum(juv),dead.adult=sum(dead.adult),adult=sum(adult)),by=.(year,season,area)] %>%
												  .[,F.juv:=(dead.juv/juv)] %>% .[,F.adult:=(dead.adult/adult)] %>% .[,.(F.juv=mean(F.juv)*4,F.adult=mean(F.adult)*4),by=.(year,area)] %>%
												  data.table::setnames(.,c("year","area"),c("time","region")) %>% .[,model:=tmp.name]
								}
							} else {
								mlab = "Seasonal"
								if(agg.regions)
								{
									dt.list[[i]] = data.table::as.data.table(fm(tmp.rep)) %>% merge(., data.table::as.data.table(popN(tmp.rep)),by=c("age","year","unit","season","area","iter")) %>%
												  data.table::setnames(.,c("value.x","value.y"),c("f","N")) %>%
												  .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,area:=as.numeric(area)] %>%
												  .[,juv:=prop.juv[age]*N] %>% .[,adult:=prop.adult[age]*N] %>% .[,dead.juv:=f*juv] %>% .[,dead.adult:=f*adult] %>%
												  .[,.(dead.juv=sum(dead.juv),juv=sum(juv),dead.adult=sum(dead.adult),adult=sum(adult)),by=.(year,season)] %>%
												  .[,F.juv:=(dead.juv/juv)] %>% .[,F.adult:=(dead.adult/adult)] %>%
												  .[,model:=tmp.name] %>% .[,region:="All regions"] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
									
								} else {
									dt.list[[i]] = data.table::as.data.table(fm(tmp.rep)) %>% merge(., data.table::as.data.table(popN(tmp.rep)),by=c("age","year","unit","season","area","iter")) %>%
												  data.table::setnames(.,c("value.x","value.y"),c("f","N")) %>%
												  .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,area:=as.numeric(area)] %>%
												  .[,juv:=prop.juv[age]*N] %>% .[,adult:=prop.adult[age]*N] %>% .[,dead.juv:=f*juv] %>% .[,dead.adult:=f*adult] %>%
												  .[,.(dead.juv=sum(dead.juv),juv=sum(juv),dead.adult=sum(dead.adult),adult=sum(adult)),by=.(year,season,area)] %>%
												  .[,F.juv:=(dead.juv/juv)] %>% .[,F.adult:=(dead.adult/adult)] %>%
												  data.table::setnames(.,c("area"),c("region")) %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]									
								}
							}

							# clean-up
							rm(list=c("tmp.rep","tmp.name","tmp.par","prop.adult","prop.juv"))
						}

						# combine into single data.table
							plot.dt = data.table::rbindlist(dt.list) %>% .[,model:=factor(as.character(model),levels=rep.names)]

						# make plot
							# Get the colours - if all.model.names passed in using ... then it is passed to palette func
							colour_values <- palette.func(selected.model.names = names(rep.list), ...)
							g = plot.dt %>% 
							ggplot2::ggplot() + ggthemes::theme_few() +
							ggplot2::geom_hline(yintercept=0) +
							ggplot2::xlab("Year") + ggplot2::ylab("Fishing mortality") + 
							ggplot2::ggtitle(paste0(mlab, " average adult (solid) and juvenile (dashed) F")) +
							ggplot2::geom_line(ggplot2::aes(x=time,y=F.adult,color=model),size=1) +
							ggplot2::geom_line(ggplot2::aes(x=time,y=F.juv,color=model),size=1,linetype="longdash") +
							ggplot2::scale_color_manual("Model",values=colour_values)
							if(yaxis.free)
							{
								g = g + ggplot2::facet_wrap(~region,scales="free_y")
							} else {
								g = g + ggplot2::facet_wrap(~region)
							}

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