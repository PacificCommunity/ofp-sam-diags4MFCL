

#' Kobe plot
#' 
#' @param rep An MFCLRep object.
#' @param rep.name A character string naming the models for plotting purposes
#' @param agg.years TRUE or FALSE. Should model outputs be aggregated to an annual time step.
#' @param agg.regions TRUE or FALSE. Should model outputs be aggregated across all regions are kept separate.
#' @param biomass.type Character string denoting the type of biomass plotted, 'SSB' or 'Total'
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
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 scale_fill_gradient
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_vline
#' 

	plot.kobe = function(rep,rep.name,agg.years = TRUE,agg.regions=TRUE,biomass.type = "SSB",save.dir,save.name)
	{
		warning("The Fmsy value used in this plot corresponds to the estimate for the period specified by par_flag(148). \nMSY is a time dynamic quantity and should be recalculated for each year in the model period. \nAs a result the 'true' trajectory of Fmsy may differ slightly from the one plotted here.")

			tmp.rep = rep
			if(agg.years)
			{
				if(agg.regions)
				{
					if(biomass.type == "SSB")
					{
						xlab = expression("SB"["MSY,t"]/"SB"["MSY,t"])
						tmp.bio.array = adultBiomass(tmp.rep)@.Data[1,,1,,,1]
						tmp.bio = apply(tmp.bio.array,1,function(x)sum(colMeans(x)))/1000
						tmp.bio_nf.array = adultBiomass_nofish(tmp.rep)@.Data[1,,1,,,1]
						tmp.bio_nf = apply(tmp.bio_nf.array,1,function(x)sum(colMeans(x)))/1000
						tmp.f_fmsy = rowMeans(FFMSY_ts(tmp.rep)@.Data[1,,1,,,1])
						tmp.sb_sbmsy = rowMeans(ABBMSY_ts(tmp.rep)@.Data[1,,1,,,1])
						plot.dt = data.table::data.table(time=as.numeric(names(tmp.bio)),bio=tmp.bio,bio_nf=tmp.bio_nf,f_fmsy=tmp.f_fmsy,sb_sbmsy=tmp.sb_sbmsy,dep=tmp.bio/tmp.bio_nf,region=rep("All regions",length(tmp.bio)))
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

		# make plot
			plot.dt = plot.dt %>% .[order(time)]
			g = plot.dt %>%
			ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~region) +
			ggplot2::scale_y_continuous(expression("F"["t"]/"F"["MSY,t"]),expand=c(0,0),limits=c(0,max(c(2,round(max(plot.dt$f_fmsy))))+0.5)) +
			ggplot2::scale_x_continuous(xlab,expand=c(0,0),limits=c(0,max(c(2,round(max(plot.dt$sb_sbmsy))))+0.5)) +
			ggplot2::geom_polygon(data=data.frame(x=c(max(c(2,round(max(plot.dt$sb_sbmsy))))+0.5,1,1,max(c(2,round(max(plot.dt$sb_sbmsy))))+0.5),y=c(1,1,max(c(2,round(max(plot.dt$f_fmsy))))+0.5,max(c(2,round(max(plot.dt$f_fmsy))))+0.5)),aes(x=x,y=y),fill="#ff8f00",alpha=0.75) +
			ggplot2::geom_polygon(data=data.frame(x=c(1,0,0,1),y=c(1,1,max(c(2,round(max(plot.dt$f_fmsy))))+0.5,max(c(2,round(max(plot.dt$f_fmsy))))+0.5)),aes(x=x,y=y),fill="#f44336",alpha=0.75) +
			ggplot2::geom_polygon(data=data.frame(x=c(max(c(2,round(max(plot.dt$sb_sbmsy))))+0.5,1,1,max(c(2,round(max(plot.dt$sb_sbmsy))))+0.5),y=c(0,0,1,1)),aes(x=x,y=y),fill="#00e676",alpha=0.75) +
			ggplot2::geom_polygon(data=data.frame(x=c(1,0,0,1),y=c(0,0,1,1)),aes(x=x,y=y),fill="#ffeb3b",alpha=0.75) +
			ggplot2::geom_hline(yintercept=1,color="black",size=1.5) +
			ggplot2::geom_vline(xintercept=1,size=1.5) +
			ggplot2::ggtitle(paste0("Kobe (time-dynamic): ",rep.name)) +
			ggplot2::geom_line(ggplot2::aes(x=sb_sbmsy,y=f_fmsy),size=0.75) +
			ggplot2::geom_point(ggplot2::aes(x=sb_sbmsy,y=f_fmsy,fill=time),size=5,shape=21, stroke=0) +
			ggplot2::geom_point(data=data.frame(sb_sbmsy=tail(plot.dt$sb_sbmsy,n=1),f_fmsy=tail(plot.dt$f_fmsy,n=1)),ggplot2::aes(x=sb_sbmsy,y=f_fmsy),fill="#42a5f5",size=5,shape=21, stroke=0) +
			ggplot2::geom_point(data=data.frame(sb_sbmsy=head(plot.dt$sb_sbmsy,n=1),f_fmsy=head(plot.dt$f_fmsy,n=1)),ggplot2::aes(x=sb_sbmsy,y=f_fmsy),fill="#66bb6a",size=5,shape=21, stroke=0) +
			ggplot2::scale_fill_gradient("Year",low = "black", high = "white")
		
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