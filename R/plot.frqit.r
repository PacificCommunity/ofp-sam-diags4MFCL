

#' Plot a frqit::frq object
#' 
#' @param Frq.list A list of Frq objects or a single Frq object. The reference model should be listed first.
#' @param Frq.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.
#' @param fdesc A data.frame with 7 columns (num,gear_long,method,code,gear,flag,region) and n rows, where n is the number of defined fisheries.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' @import frqit
#' @import magrittr
#' @importFrom data.table as.data.table
#' @importFrom data.table melt
#' @importFrom ggthemes theme_few
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 scale_color_viridis_c
#' @importFrom ggplot2 scale_fill_gradientn

	plot.frqit = function(Frq.list,Frq.names=NULL,fdesc=NULL, save.dir,save.name)
	{
	  
	  # Check and sanitise input Frq arguments and names
	    frq.list = check.frqit.args(frq=Frq.list, frq.names=Frq.names)
	    frq.names = names(frq.list)
	  
		if(length(frq.list)>1)
		{
			dt.list = as.list(rep(NA,length(frq.list)))
			names(dt.list) = names(frq.list)
			stop("plot.frq is not defined for plotting multiple frq objects")
		} else {
			tmp.frq = frq.list[[1]]
			tmp.name = names(frq.list)[1]

			if(is.null(fdesc))
			{
				fdesc = data.frame(num=1:n_fisheries(tmp.frq),gear_long=NA,method=NA,code=NA,gear=NA,flag=NA,region=c(aperm(region_fish(tmp.frq),c(3,1,2,4,5,6))))
				fdesc$code = paste0(fdesc$num,".",fdesc$region)
				fdesc$name = fdesc$code
			} else {
				fdesc$name = paste0(fdesc$num,": ",fdesc$code)
			}

			  rainbow.cols = c("#f44336","#e91e63","#9c27b0","#673ab7","#3f51b5","#2196f3","#03a9f4","#00bcd4","#009688","#4caf50","#8bc34a","#cddc39","#ffeb3b","#ffc107","#ff9800")
			  tmp.fn = function(x,y){return(median(rep(x,round(ifelse(is.na(y),0,y))),na.rm=TRUE))}			


			#____________________________________________________________________________________________________________
			# Plot catch
				n.fishery = max(fdesc$num)
				fishery.data = data_flags(tmp.frq)[1,]
				fishery.reg = fdesc$region
				cep.dt = data.table::as.data.table(cateffpen(tmp.frq))

				g.catch = data.table::as.data.table(cateffpen(tmp.frq)) %>% .[,ts:=year+(month/12)+(week/48)] %>% .[,Region:=fishery.reg[fishery]] %>%
				.[,Region := factor(as.character(Region),levels=as.character(sort(unique(Region))))] %>% .[,Fishery := factor(fdesc$name[fishery],levels=fdesc$name)] %>%
				.[,Text:=ifelse(fishery.data[fishery]==0,"N","MT")] %>%
				ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~Fishery,scales="free_y",drop=FALSE) +
				ggplot2::xlim(tail(range(tmp.frq),n=2)[1],tail(range(tmp.frq),n=2)[2]+1) + 
				ggplot2::geom_hline(yintercept=0) +
				ggplot2::xlab("Year") +
				ggplot2::ylab("Catch") +
				ggplot2::ggtitle("Catch by fishery") +
				ggplot2::geom_bar(ggplot2::aes(x=ts,y=catch,fill=Region),stat="identity") +
				ggplot2::scale_fill_manual("Region",values=colorRampPalette(rainbow.cols)(length(unique(fishery.reg)))) + 
				ggplot2::geom_text(data=data.frame(Fishery=factor(fdesc$name,levels=fdesc$name),ts=min(cep.dt$year+(cep.dt$month/12)+(cep.dt$week/48)),catch=cep.dt[,.(catch=0.9*max(catch)),by=fishery][order(fishery)]$catch,text=ifelse(fishery.data==0,"N","MT")),ggplot2::aes(x=ts,y=catch,label=text),hjust=0,vjust=1)

			#____________________________________________________________________________________________________________
			# Plot cpue & penalty weight
				cep.dt = data.table::as.data.table(cateffpen(tmp.frq)) %>% .[,cpue :=catch/effort] %>% .[,cpue:=ifelse(cpue<0,NA,cpue)]
				g.cpue = data.table::as.data.table(cateffpen(tmp.frq)) %>% .[,ts:=year+(month/12)+(week/48)] %>% .[,Region:=fishery.reg[fishery]] %>%
				.[,Region := factor(as.character(Region),levels=as.character(sort(unique(Region))))] %>% .[,Fishery := factor(fdesc$name[fishery],levels=fdesc$name)] %>%
				.[,cpue :=catch/effort] %>% .[,cpue:=ifelse(cpue<0,NA,cpue)] %>% merge(.,cep.dt[,.(mean_cpue=mean(cpue,na.rm=TRUE)),by=fishery],by="fishery") %>%
				.[,cpue := cpue/mean_cpue] %>% .[,penalty:=ifelse(penalty<0,NA,penalty)] %>%
				ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~Fishery,scales="free_y",drop=FALSE) + ggplot2::ylim(0,NA) +
				ggplot2::xlim(tail(range(tmp.frq),n=2)[1],tail(range(tmp.frq),n=2)[2]+1) + 
				ggplot2::geom_hline(yintercept=1) +
				ggplot2::xlab("Year") +
				ggplot2::ylab("CPUE") +
				ggplot2::ggtitle("CPUE by fishery")
				if(length(unique(ifelse(cep.dt$penalty<0,NA,cep.dt$penalty)))>1)
				{
					g.cpue = g.cpue + ggplot2::geom_point(ggplot2::aes(x=ts,y=cpue,fill=penalty),size=2,shape=21) +
									  ggplot2::scale_fill_viridis_c("Penalty weight")

					g.pen = data.table::as.data.table(cateffpen(tmp.frq)) %>% .[,ts:=year+(month/12)+(week/48)] %>% .[,Region:=fishery.reg[fishery]] %>%
					.[,Region := factor(as.character(Region),levels=as.character(sort(unique(Region))))] %>% .[,Fishery := factor(fdesc$name[fishery],levels=fdesc$name)] %>%
					.[,penalty:=ifelse(penalty<0,NA,penalty)] %>%
					ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~Fishery,scales="free_y",drop=FALSE) + ggplot2::ylim(0,NA) +
					ggplot2::xlim(tail(range(tmp.frq),n=2)[1],tail(range(tmp.frq),n=2)[2]+1) + 
					ggplot2::geom_hline(yintercept=1) +
					ggplot2::xlab("Year") +
					ggplot2::ylab("CPUE") +
					ggplot2::ggtitle("CPUE by fishery") +
					ggplot2::geom_point(ggplot2::aes(x=ts,y=penalty,fill=penalty),size=2,shape=21) +
					ggplot2::scale_fill_viridis_c("Penalty weight")

					# write.out
					if(!missing(save.dir))
					{
						if(missing(save.name))
						{
							stop("How can you save the output if you haven't specified the directory? Please specify save.dir.")
						} else {
							if (! dir.exists(save.dir))dir.create(save.dir,recursive=TRUE)
							ggplot2::ggsave(paste0(save.name,".png"),plot=g.pen, device = "png", path = save.dir,scale = 1, width = 16, height = 9, units = c("in"))
						}
					} 
				} else{
					g.cpue = g.cpue + ggplot2::geom_point(ggplot2::aes(x=ts,y=cpue),fill="gray70",size=2,shape=21)
					g.pen = NULL
				}

			#____________________________________________________________________________________________________________
			# Plot ln data 
				ln.dt = data.table::as.data.table(lnfrq(tmp.frq))
				g.ln_hist_time = data.table::as.data.table(lnfrq(tmp.frq)) %>% .[,Fishery := factor(fdesc$name[fishery],levels=fdesc$name)] %>% .[,ts:=year+(month/12)+(week/48)] %>%
				 			data.table::melt(.,id.vars=c("ts","year","month","week","fishery","Fishery"),variable.name = "Length",value.name = "N") %>%
				 			.[,Length:=as.numeric(as.character(Length))] %>% .[N>0] %>%
				ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~Fishery,scales="free_y",drop = FALSE) +
				ggplot2::xlim(tail(range(tmp.frq),n=2)[1],tail(range(tmp.frq),n=2)[2]+1) + 
				ggplot2::geom_hline(yintercept=0) +
				ggplot2::xlab("Year") +
				ggplot2::ylab("Observations") +
				ggplot2::ggtitle("Temporal length composition availability by fishery") +
				ggplot2::geom_bar(ggplot2::aes(x=ts,y=N,fill=Length),stat="identity") +
				ggplot2::scale_fill_gradientn("Length",colors=colorRampPalette(rainbow.cols)(length(unique(fishery.reg))))

				ln.dt = data.table::as.data.table(lnfrq(tmp.frq))
				g.ln_hist = data.table::as.data.table(lnfrq(tmp.frq)) %>% .[,Fishery := factor(fdesc$name[fishery],levels=fdesc$name)] %>% .[,ts:=year+(month/12)+(week/48)] %>%
				 			data.table::melt(.,id.vars=c("ts","year","month","week","fishery","Fishery"),variable.name = "Length",value.name = "N") %>%
				 			.[,Length:=as.numeric(as.character(Length))] %>% .[N>0] %>% .[order(fishery,ts,Length)] %>%
				ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~Fishery,scales="free_y",drop = FALSE) +
				ggplot2::geom_hline(yintercept=0) +
				ggplot2::xlab("Length") +
				ggplot2::ylab("Observations") +
				ggplot2::ggtitle("Length histogram by fishery") +
				ggplot2::geom_bar(ggplot2::aes(x=Length,y=N,fill=ts),stat="identity") +
				ggplot2::scale_fill_gradientn("Year",colors=colorRampPalette(rainbow.cols)(length(unique(fishery.reg))))

				ln.dt = data.table::as.data.table(lnfrq(tmp.frq)) %>% .[,Fishery := factor(fdesc$name[fishery],levels=fdesc$name)] %>% .[,ts:=year+(month/12)+(week/48)] %>%
				 			data.table::melt(.,id.vars=c("ts","year","month","week","fishery","Fishery"),variable.name = "Length",value.name = "N") %>%
				 			.[,Length:=as.numeric(as.character(Length))] %>% .[N>0] %>% .[order(fishery,ts,Length)]


				g.ln_heat = data.table::as.data.table(lnfrq(tmp.frq)) %>% .[,Fishery := factor(fdesc$name[fishery],levels=fdesc$name)] %>% .[,ts:=year+(month/12)+(week/48)] %>%
				 			data.table::melt(.,id.vars=c("ts","year","month","week","fishery","Fishery"),variable.name = "Length",value.name = "N") %>%
				 			.[,Length:=as.numeric(as.character(Length))] %>% .[N>0] %>% .[order(fishery,ts,Length)] %>%
				 			merge(.,ln.dt[,.(maxN=sum(N)),by=.(fishery,ts)],by=c("fishery","ts")) %>% .[,N:=N/maxN] %>%
				ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~Fishery,scales="free_y",drop = FALSE) +
				ggplot2::xlim(tail(range(tmp.frq),n=2)[1],tail(range(tmp.frq),n=2)[2]+1) + 
				ggplot2::geom_hline(yintercept=0) +
				ggplot2::xlab("Year") +
				ggplot2::ylab("Length") +
				ggplot2::ggtitle("Mean length by fishery") +
				ggplot2::geom_tile(ggplot2::aes(x=ts,y=Length,color=N),stat="identity") +
				# ggplot2::geom_line(data=ln.dt[,.(num=sum(Length*N),denom=sum(N)),by=.(ts,Fishery)],ggplot2::aes(x=ts,y=num/denom),color="hotpink") +
				# ggplot2::geom_point(data=ln.dt[,.(num=sum(Length*N),denom=sum(N)),by=.(ts,Fishery)],ggplot2::aes(x=ts,y=num/denom),fill="hotpink",shape=21,size=1) +
				ggplot2::geom_line(data=ln.dt[,.(med=tmp.fn(Length,N)),by=.(ts,Fishery)],ggplot2::aes(x=ts,y=med),color="hotpink") +
				ggplot2::geom_point(data=ln.dt[,.(med=tmp.fn(Length,N)),by=.(ts,Fishery)],ggplot2::aes(x=ts,y=med),fill="hotpink",shape=21,size=1) +
				ggplot2::scale_color_viridis_c("Proportion of observations",trans="logit")

			#____________________________________________________________________________________________________________
			# Plot wt data 
				wt.dt = data.table::as.data.table(wtfrq(tmp.frq))
				g.wt_hist_time = data.table::as.data.table(wtfrq(tmp.frq)) %>% .[,Fishery := factor(fdesc$name[fishery],levels=fdesc$name)] %>% .[,ts:=year+(month/12)+(week/48)] %>%
				 			data.table::melt(.,id.vars=c("ts","year","month","week","fishery","Fishery"),variable.name = "Weight",value.name = "N") %>%
				 			.[,Weight:=as.numeric(as.character(Weight))] %>% .[N>0] %>%
				ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~Fishery,scales="free_y",drop = FALSE) +
				ggplot2::xlim(tail(range(tmp.frq),n=2)[1],tail(range(tmp.frq),n=2)[2]+1) + 
				ggplot2::geom_hline(yintercept=0) +
				ggplot2::xlab("Year") +
				ggplot2::ylab("Observations") +
				ggplot2::ggtitle("Temporal weight composition availability by fishery") +
				ggplot2::geom_bar(ggplot2::aes(x=ts,y=N,fill=Weight),stat="identity") +
				ggplot2::scale_fill_gradientn("Weight",colors=colorRampPalette(rainbow.cols)(length(unique(fishery.reg))))

				wt.dt = data.table::as.data.table(wtfrq(tmp.frq))
				g.wt_hist = data.table::as.data.table(wtfrq(tmp.frq)) %>% .[,Fishery := factor(fdesc$name[fishery],levels=fdesc$name)] %>% .[,ts:=year+(month/12)+(week/48)] %>%
				 			data.table::melt(.,id.vars=c("ts","year","month","week","fishery","Fishery"),variable.name = "Weight",value.name = "N") %>%
				 			.[,Weight:=as.numeric(as.character(Weight))] %>% .[N>0] %>% .[order(fishery,ts,Weight)] %>%
				ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~Fishery,scales="free_y",drop = FALSE) +
				ggplot2::geom_hline(yintercept=0) +
				ggplot2::xlab("Weight") +
				ggplot2::ylab("Observations") +
				ggplot2::ggtitle("Weight histogram by fishery") +
				ggplot2::geom_bar(ggplot2::aes(x=Weight,y=N,fill=ts),stat="identity") +
				ggplot2::scale_fill_gradientn("Year",colors=colorRampPalette(rainbow.cols)(length(unique(fishery.reg))))

				wt.dt = data.table::as.data.table(wtfrq(tmp.frq)) %>% .[,Fishery := factor(fdesc$name[fishery],levels=fdesc$name)] %>% .[,ts:=year+(month/12)+(week/48)] %>%
				 			data.table::melt(.,id.vars=c("ts","year","month","week","fishery","Fishery"),variable.name = "Weight",value.name = "N") %>%
				 			.[,Weight:=as.numeric(as.character(Weight))] %>% .[N>0] %>% .[order(fishery,ts,Weight)]


				g.wt_heat = data.table::as.data.table(wtfrq(tmp.frq)) %>% .[,Fishery := factor(fdesc$name[fishery],levels=fdesc$name)] %>% .[,ts:=year+(month/12)+(week/48)] %>%
				 			data.table::melt(.,id.vars=c("ts","year","month","week","fishery","Fishery"),variable.name = "Weight",value.name = "N") %>%
				 			.[,Weight:=as.numeric(as.character(Weight))] %>% .[N>0] %>% .[order(fishery,ts,Weight)] %>%
				 			merge(.,wt.dt[,.(maxN=sum(N)),by=.(fishery,ts)],by=c("fishery","ts")) %>% .[,N:=N/maxN] %>%
				ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~Fishery,scales="free_y",drop = FALSE) +
				ggplot2::xlim(tail(range(tmp.frq),n=2)[1],tail(range(tmp.frq),n=2)[2]+1) + 
				ggplot2::geom_hline(yintercept=0) +
				ggplot2::xlab("Year") +
				ggplot2::ylab("Weight") +
				ggplot2::ggtitle("Mean weight by fishery") +
				ggplot2::geom_tile(ggplot2::aes(x=ts,y=Weight,color=N),stat="identity") +
				# ggplot2::geom_line(data=wt.dt[,.(num=sum(Weight*N),denom=sum(N)),by=.(ts,Fishery)],ggplot2::aes(x=ts,y=num/denom),color="hotpink") +
				# ggplot2::geom_point(data=wt.dt[,.(num=sum(Weight*N),denom=sum(N)),by=.(ts,Fishery)],ggplot2::aes(x=ts,y=num/denom),fill="hotpink",shape=21,size=1) +
				ggplot2::geom_line(data=wt.dt[,.(med=tmp.fn(Weight,N)),by=.(ts,Fishery)],ggplot2::aes(x=ts,y=med),color="hotpink") +
				ggplot2::geom_point(data=wt.dt[,.(med=tmp.fn(Weight,N)),by=.(ts,Fishery)],ggplot2::aes(x=ts,y=med),fill="hotpink",shape=21,size=1) +
				ggplot2::scale_color_viridis_c("Proportion of observations",trans="logit")

			
		}



		
		# write.out
		if(!missing(save.dir))
		{
			if(missing(save.name))
			{
				stop("How can you save the output if you haven't specified the directory? Please specify save.dir.")
			} else {
				if (! dir.exists(save.dir))dir.create(save.dir,recursive=TRUE)
				ggplot2::ggsave(paste0(save.name,".png"),plot=g.catch, device = "png", path = save.dir,scale = 1, width = 16, height = 9, units = c("in"))
				ggplot2::ggsave(paste0(save.name,".png"),plot=g.cpue, device = "png", path = save.dir,scale = 1, width = 16, height = 9, units = c("in"))
				ggplot2::ggsave(paste0(save.name,".png"),plot=g.ln_hist_time, device = "png", path = save.dir,scale = 1, width = 16, height = 9, units = c("in"))
				ggplot2::ggsave(paste0(save.name,".png"),plot=g.ln_hist, device = "png", path = save.dir,scale = 1, width = 16, height = 9, units = c("in"))
				ggplot2::ggsave(paste0(save.name,".png"),plot=g.ln_heat, device = "png", path = save.dir,scale = 1, width = 16, height = 9, units = c("in"))
				ggplot2::ggsave(paste0(save.name,".png"),plot=g.wt_hist_time, device = "png", path = save.dir,scale = 1, width = 16, height = 9, units = c("in"))
				ggplot2::ggsave(paste0(save.name,".png"),plot=g.wt_hist, device = "png", path = save.dir,scale = 1, width = 16, height = 9, units = c("in"))
				ggplot2::ggsave(paste0(save.name,".png"),plot=g.wt_heat, device = "png", path = save.dir,scale = 1, width = 16, height = 9, units = c("in"))

			}
		} 
			
		return(list(catch=g.catch,cpue=g.cpue,pen=g.pen,ln_hist_time=g.ln_hist_time,ln_hist=g.ln_hist,ln_heat=g.ln_heat,
			wt_hist_time=g.wt_hist_time,wt_hist=g.wt_hist,wt_heat=g.wt_heat))

	}	