

#' Compare the estimated selectivity across different models.
#' 
#' @param rep.list A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.
#' @param rep.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.
#' @param sel.basis A character string indicating if selectivity at age ('AGE') or length ('Length') should be plotted
#' @param palette.cols A vector of character strings giving the colors to form a palette for differentiating between models. If wishing to use the exact colors give a vector of (length(rep.list) - 1) colors as the reference model is black by default.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom data.table as.data.table
#' @importFrom data.table rbindlist
#' @importFrom ggthemes theme_few
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_y_continuous
#' 

plot.selectivity = function(rep.list,rep.names=NULL,sel.basis="AGE",palette.cols = c("royalblue3","deepskyblue1","gold","orange1","indianred1","firebrick2","#AC2020"),save.dir,save.name)
{
	  # Check and sanitise input MFCLRep arguments and names
    rep.list <- check.rep.args(rep=rep.list, rep.names=rep.names)
    rep.names <- names(rep.list)

    if(!(sel.basis %in% c("AGE","LENGTH")))
    {
    	stop("Incorrect sel.basis. Please use 'AGE' or 'Length'")
    }
    
		dt.list = as.list(rep(NA,length(rep.list)))
		names(dt.list) = names(rep.list)

		for(i in 1:length(dt.list))
		{
			tmp.rep = rep.list[[i]]
			tmp.name = names(rep.list)[i]
			tmp.sel = as.data.table(sel(tmp.rep)) %>% .[,.(age,unit,value)] %>% .[,age:=as.numeric(age)] %>% .[,unit:=as.numeric(unit)] %>% .[,model:=tmp.name] %>% setnames(.,"unit","fishery") %>% .[order(model,fishery,age),.(model,fishery,age,value)]
			tmp.laa = c(aperm(mean_laa(tmp.rep),c(4,1,2,3,5,6)))

			if(sel.basis == "AGE")
			{
				xlab = "Age"
				tmp.sel$x = tmp.sel$age
					
			} else {
				xlab = "Length"
				tmp.sel$x = tmp.laa[tmp.sel$age]
			}
			dt.list[[i]] = tmp.sel

			# clean-up
			rm(list=c("tmp.rep","tmp.name","tmp.sel","tmp.laa"))
		}
		
		# combine into single data.table
			plot.dt = data.table::rbindlist(dt.list) %>% .[,model:=factor(as.character(model),levels=rep.names)]

		# make plot
			g = plot.dt %>% 
			ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~fishery) +
			ggplot2::xlab(xlab) + ggplot2::ylab("Selectivity") +
			ggplot2::ggtitle("Estimated selectivity by fishery") +
			ggplot2::geom_line(ggplot2::aes(x=x,y=value,color=model),size=1.25) +
			ggplot2::scale_color_manual("Model",values=c("black",colorRampPalette(palette.cols)(length(rep.list)-1))[1:length(rep.list)])
		
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