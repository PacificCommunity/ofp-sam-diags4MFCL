
#' Plot the temporal change in median length and weight of vulnerable numbers by fishery
#'
#' @param par MFCLPar object
#' @param rep MFCLRep object
#' @param frq MFCLFrq object
#' @param max.wt Optional: The maximum weight value to plot. Defaults to the maximum weight in the model
#' @param max.len Optional: The maximum length value to plot. Defaults to the maximum length in the model
#' @param fdescloc Optional: The path to the fdesc file to define the fishery names
#' @param year.window Optional: A single value giving the temporal window used to calculate the change in median length and weight from the beginning to the end of the model period. Defaults to 25% of the model period.
#' @param save.dir Optional: Path to the directory where the outputs will be saved
#' @param save.name Optional: Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom data.table data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table setkey
#' @importFrom data.table setnames
#' @importFrom ggthemes theme_few
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 coord_cartesian




plot.len.wt.temporal = function(par,rep,frq,max.wt=NULL,max.len=NULL,fdescloc=NULL,year.window=NULL,save.dir,save.name)
{

	if(is.null(max.len))
	{
		max.len = max(seq(from=lf_range(frq)["LFFirst"],by=lf_range(frq)["LFWidth"]*lf_range(frq)["LFFactor"],length.out=lf_range(frq)["LFIntervals"]))
	}

	if(is.null(max.wt))
	{
		max.wt = max(seq(from=lf_range(frq)["WFFirst"],by=lf_range(frq)["WFWidth"]*lf_range(frq)["WFFactor"],length.out=lf_range(frq)["WFIntervals"]))
	}



	# define population
		avg_len = c(aperm(mean_laa(rep),c(4,1,2,3,5,6)))
		sd_len = c(aperm(sd_laa(rep),c(4,1,2,3,5,6)))
		lw_pars = season_growth_pars(par)[27:28]
		pop.dt = data.table::as.data.table(popN(rep)) %>% .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,time:=year+(season-1)/dimensions(rep)["seasons"]] %>%
				.[,N:=as.integer(round(value))] %>% .[,.(time,area,age,N)] %>% .[order(time,area,age)] %>% data.table::setnames(.,"area","region") %>% .[,region:=as.numeric(region)] %>%
				.[,avg_len:=avg_len[age]] %>%.[,sd_len:=sd_len[age]] # %>% .[,avg_wt:=lw_pars[1]*avg_len^lw_pars[2]]
		model.period = range(pop.dt$time)
		if(is.null(year.window))
		{
			year.window = diff(model.period)*0.25
		}
	# define selectivity
		region_fish = c(aperm(region_fish(frq),c(3,1,2,4,5,6)))
		sel.dt = data.table::as.data.table(sel(rep))  %>% .[,age:=as.numeric(age)] %>% .[,fishery:=as.numeric(unit)] %>% data.table::setnames(.,"value","selectivity") %>% .[,region:=region_fish[fishery]] %>%
				 .[,.(fishery,region,age,selectivity)] %>% .[order(fishery,age)]

	# merge
		vulN.dt = merge(pop.dt,sel.dt,by=c("region","age"),allow.cartesian=TRUE) %>% .[,vulN:=selectivity*N]
		fishery.dt = unique(vulN.dt[,.(fishery,time)]) %>% .[order(fishery,time)]
		data.table::setkey(vulN.dt,fishery,time)

	# bootstrap
		boot.mat = matrix(NA,nrow=nrow(fishery.dt),ncol=6)
		colnames(boot.mat) = c(paste0("len",c(25,50,75)),paste0("wt",c(25,50,75)))
		nsamp = 10000

		A = proc.time()
		for(i in 1:nrow(fishery.dt))
		# for(i in 1:1000)
		{
			tmp.dt = vulN.dt[.(fishery.dt$fishery[i],fishery.dt$time[i])] %>% .[,propVul := vulN/sum(vulN)] %>% .[,samples:=round(propVul*nsamp)]
			tmp.samp.len = rnorm(sum(tmp.dt$samples),rep(tmp.dt$avg_len,tmp.dt$samples),rep(tmp.dt$sd_len,tmp.dt$samples))
			tmp.samp.wt = lw_pars[1]*tmp.samp.len^lw_pars[2]
			boot.mat[i,] = c(quantile(tmp.samp.len,probs=c(0.25,0.5,0.75)),quantile(tmp.samp.wt,probs=c(0.25,0.5,0.75)))
			rm(list=c("tmp.dt","tmp.samp.len","tmp.samp.wt"))
		} 
		B = proc.time()
		# B - A # roughly 1.2 minutes for 2020 BET

	if(is.null(fdescloc))
	{
		fishery_names = 1:length(region_fish)
	} else {
		fdesc = read.table(fdescloc,header=TRUE)
		fishery_names = paste0(1:length(region_fish),". ",fdesc$code)
	}


	# plot weight
		wt.dt = cbind(fishery.dt,boot.mat[,4:6])
		frq.wt.dt = data.table::as.data.table(freq(frq)) %>% .[,time:=year+c(0,0,0,0.25,0.25,0.25,0.5,0.5,0.5,0.75,0.75,0.75)[month]]  %>% .[,freq:=round(freq)] %>% .[freq>0 & !is.na(weight)]
		frq.wt.dt = data.table::data.table(fishery=rep(frq.wt.dt$fishery,frq.wt.dt$freq),time=rep(frq.wt.dt$time,frq.wt.dt$freq),wt=rep(frq.wt.dt$weight,frq.wt.dt$freq)) %>%
					.[,time:=floor(time)+0.5] %>%
					.[,.(N=.N,obswt25 = quantile(wt,probs=0.25),obswt50 = quantile(wt,probs=0.5),obswt75 = quantile(wt,probs=0.75)),by=.(fishery,time)] %>% .[order(fishery,time)] %>% .[N>=30,.(fishery,time,obswt25,obswt50,obswt75)]

		wt.dt = merge(wt.dt,frq.wt.dt,by=c("fishery","time"),all.x=TRUE) %>% .[fishery %in% unique(frq.wt.dt$fishery)]  %>% .[,fishery:=factor(as.character(fishery),levels = as.character(sort(unique(fishery))),labels=fishery_names[sort(unique(fishery))])]
		wt.diff.dt = merge(wt.dt[time<model.period[1]+year.window,.(init = median(wt50)),by=fishery],wt.dt[time>model.period[2]-year.window,.(term = median(wt50)),by=fishery],by="fishery") %>%
					  .[,diff:=round(((term-init)/init)*100,digits=2)] %>% .[,time:=model.period[1]] %>% .[,ycoord:=max.wt] %>% .[,lab:=paste0(diff," %")]
		g.wt = wt.dt %>%
			ggplot2::ggplot() + ggthemes::theme_few() + 
				ggplot2::facet_wrap(~fishery) +
				ggplot2::xlab("Time") +
				ggplot2::ylab("Fish weight (kg)") +
				ggplot2::geom_ribbon(ggplot2::aes(x=time,ymin=wt25,ymax=wt75),fill="gray90") +
				ggplot2::geom_line(ggplot2::aes(x=time,y=wt50),size=1) +
				ggplot2::geom_segment(ggplot2::aes(x=time,y=obswt25,xend=time,yend=obswt75),col="red") +
				ggplot2::geom_point(ggplot2::aes(x=time,y=obswt50),col="red") +
				ggplot2::coord_cartesian(ylim=c(0,max.wt)) +
				ggplot2::geom_text(data=wt.diff.dt,ggplot2::aes(x=time,y=ycoord,label=lab),hjust=0,vjust=1)

	# plot length
		len.dt = cbind(fishery.dt,boot.mat[,1:3])
		frq.len.dt = data.table::as.data.table(freq(frq)) %>% .[,time:=year+c(0,0,0,0.25,0.25,0.25,0.5,0.5,0.5,0.75,0.75,0.75)[month]]  %>% .[,freq:=round(freq)] %>% .[freq>0 & !is.na(length)]
		frq.len.dt = data.table::data.table(fishery=rep(frq.len.dt$fishery,frq.len.dt$freq),time=rep(frq.len.dt$time,frq.len.dt$freq),len=rep(frq.len.dt$length,frq.len.dt$freq)) %>%
					.[,time:=floor(time)+0.5] %>%
					.[,.(N=.N,obslen25 = quantile(len,probs=0.25),obslen50 = quantile(len,probs=0.5),obslen75 = quantile(len,probs=0.75)),by=.(fishery,time)] %>% .[order(fishery,time)] %>% .[N>=30,.(fishery,time,obslen25,obslen50,obslen75)]

		len.dt = merge(len.dt,frq.len.dt,by=c("fishery","time"),all.x=TRUE) %>% .[fishery %in% unique(frq.len.dt$fishery)] %>% .[,fishery:=factor(as.character(fishery),levels = as.character(sort(unique(fishery))),labels=fishery_names[sort(unique(fishery))])]
		len.diff.dt = merge(len.dt[time<model.period[1]+year.window,.(init = median(len50)),by=fishery],len.dt[time>model.period[2]-year.window,.(term = median(len50)),by=fishery],by="fishery") %>%
					  .[,diff:=round(((term-init)/init)*100,digits=2)] %>% .[,time:=model.period[1]] %>% .[,ycoord:=max.len] %>% .[,lab:=paste0(diff," %")]
		g.len = len.dt %>%
			ggplot2::ggplot() + ggthemes::theme_few() + 
				ggplot2::facet_wrap(~fishery) +
				ggplot2::xlab("Time") +
				ggplot2::ylab("Fish length (cm)") +
				ggplot2::geom_ribbon(ggplot2::aes(x=time,ymin=len25,ymax=len75),fill="gray90") +
				ggplot2::geom_line(ggplot2::aes(x=time,y=len50),size=1) +
				ggplot2::geom_segment(ggplot2::aes(x=time,y=obslen25,xend=time,yend=obslen75),col="red") +
				ggplot2::geom_point(ggplot2::aes(x=time,y=obslen50),col="red") +
				ggplot2::coord_cartesian(ylim=c(0,max.len)) +
				ggplot2::geom_text(data=len.diff.dt,ggplot2::aes(x=time,y=ycoord,label=lab),hjust=0,vjust=1)

		# write.out
		if(!missing(save.dir))
		{
			if(missing(save.name))
			{
				stop("How can you save the output if you haven't specified the directory? Please specify save.dir.")
			} else {
				if (! dir.exists(save.dir))dir.create(save.dir,recursive=TRUE)
				ggplot2::ggsave(paste0("len.temp.",save.name,".png"),plot=g.len, device = "png", path = save.dir,scale = 1, width = 16, height = 9, units = c("in"))
				ggplot2::ggsave(paste0("wt.temp.",save.name,".png"),plot=g.wt, device = "png", path = save.dir,scale = 1, width = 16, height = 9, units = c("in"))
			}
		} 
}





