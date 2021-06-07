

#' Plot the contributions to biomass from each region
#' Original code from Rob Scott's FLR4MFCL package and refreshed for diags4mfcl
#' 
#' @param par MFCLPar object
#' @param rep MFCLRep object
#' @param Fmult A multiplier on the terminal year fishing mortality
#' @param proportion TRUE or FALSE. Plot raw biomass (shows scale between region) or plot proportion of biomass by source region 
#' @param cols A vector of colors for each region. If NULL then the default will be a vibrant rainbow.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' @import FLR4MFCL
#' @import FLCore
#' @import magrittr
#' @importFrom data.table setnames
#' @importFrom data.table data.table
#' @importFrom ggthemes theme_few
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 scale_fill_manual
#' 

plot.biomass.contributions = function(par, rep, Fmult=0, proportion=TRUE, cols=NULL,save.dir,save.name)
{
  	#____________________________________________________________________________________________________________
  	# Legacy code from Rob Scott FLR4MFCL::plotting.r
		nages   = dimensions(par)['agecls']
		ssns    = dimensions(par)['seasons']
		regions = dimensions(par)['regions']

		m   = m_at_age(rep)
		fec = mat(par)
		f   = fm(rep)[,as.character(range(rep)['maxyear'])]

		# calculate average recruitment by region and season for an appropriate period
		recdist = yearMeans(rec_region(rep))

		pop = FLQuant(0, dimnames=list(age=1:nages, year="all", unit="unique", season=1:ssns, area=1:regions, iter=1:regions))

		# seed the recruitment values
		for(rr in 1:regions)
		{
		  pop[1,,,,rr,rr]= recdist[,,,,rr,]
		}

		# calculate popn numbers
		for(ii in 1:regions){
			for(qq in 1:ssns){
				for(aa in 2:nages){
					pop[aa,,,qq,ii] = pop[aa-1,,,qq,ii] %*% diff_coffs_age_period(par)[,,aa,qq] * exp(-m[aa]-c(f[aa,,,qq,ii,]*Fmult))
				}
			}
		}


		pop_ab = seasonMeans(quantSums(sweep(pop, 1, waa(par)*fec, "*")))

		dat = aperm(pop_ab, c(6,5,1,2,3,4))[,,1,1,1,1]
	
	#____________________________________________________________________________________________________________
  	# Updated code for plotting
		dimnames(dat) = list(source=1:nrow(dat),region=1:ncol(dat))
		dt = data.table::as.data.table(as.data.frame(as.table(dat))) %>% data.table::setnames(.,"Freq","pop") %>% .[,source:=factor(as.character(source),levels=as.character(rev(sort(unique(source)))))]
		
		if(is.null(cols))
		{
			rainbow.cols = c("#f44336","#e91e63","#9c27b0","#673ab7","#3f51b5","#2196f3","#03a9f4","#00bcd4","#009688","#4caf50","#8bc34a","#cddc39","#ffeb3b","#ffc107","#ff9800")
		  	cols = colorRampPalette(rainbow.cols)(nrow(dat))
		}

		if(proportion)
		{
			g = dt %>%
			ggplot2::ggplot() + ggthemes::theme_few() +
			ggplot2::geom_hline(yintercept=0) +
			ggplot2::xlab("Region") +
			ggplot2::ylab("Proportion biomass by source region") +
			ggplot2::ggtitle("Regional contributions to biomass") +
			ggplot2::geom_bar(ggplot2::aes(x=region,y=pop,fill=source),stat="identity",position="fill") +
			ggplot2::scale_fill_manual("Source region",values=cols)
		} else {
			g = dt %>%
			ggplot2::ggplot() + ggthemes::theme_few() +
			ggplot2::geom_hline(yintercept=0) +
			ggplot2::xlab("Region") +
			ggplot2::ylab("Biomass by source region (MT)") +
			ggplot2::ggtitle("Regional contributions to biomass") +
			ggplot2::geom_bar(ggplot2::aes(x=region,y=pop,fill=source),stat="identity") +
			ggplot2::scale_fill_manual("Source region",values=cols)
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