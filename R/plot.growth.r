

#' Compare the estimated growth across different models. If plotting a single model the 95 percent confidence region around the growth curve from the estimated standard deviation at age is also plotted.
#' 
#' @param rep.list A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.
#' @param rep.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.
#' @param palette.func A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @param ... Passes extra arguments to the palette function. Use the argument all.model.colours to ensure consistency of model colours when plotting a subset of models.
#' @export
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom data.table setnames
#' @importFrom data.table data.table
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

plot.growth = function(rep.list,rep.names=NULL, palette.func=default.model.colours, save.dir,save.name, ...)
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
			tmp.laa = c(aperm(mean_laa(tmp.rep),c(4,1,2,3,5,6)))
			tmp.sd_laa = c(aperm(sd_laa(tmp.rep),c(4,1,2,3,5,6)))
			if(length(dt.list)==1)
			{
				tmp.lower = tmp.laa - 1.96*tmp.sd_laa
				tmp.upper = tmp.laa + 1.96*tmp.sd_laa
			} else {
				tmp.lower = tmp.laa 
				tmp.upper = tmp.laa 
			}


			dt.list[[i]] = data.table::data.table(model=rep(tmp.name,length(tmp.laa)),age=1:length(tmp.laa),length=tmp.laa,lower=tmp.lower,upper=tmp.upper)

			# clean-up
			rm(list=c("tmp.rep","tmp.name","tmp.sd_laa","tmp.laa","tmp.lower","tmp.upper"))
		}
		
		# combine into single data.table
			plot.dt = data.table::rbindlist(dt.list) %>% .[,model:=factor(as.character(model),levels=rep.names)]

		# make plot
			# Get the colours - if all.model.names passed in using ... then it is passed to palette func
			colour_values <- palette.func(selected.model.names = names(rep.list), ...)
			g = plot.dt %>% 
			ggplot2::ggplot() + ggthemes::theme_few() +
			ggplot2::xlab("Age") + ggplot2::ylab("Length") +
			ggplot2::ggtitle("Model growth") +
			ggplot2::geom_ribbon(ggplot2::aes(x=age,ymin=lower,ymax=upper,fill=model),alpha=0.2) +
			ggplot2::geom_line(ggplot2::aes(x=age,y=length,color=model),size=1.25) +
			ggplot2::scale_color_manual("Model",values=colour_values) +
			ggplot2::scale_fill_manual("Model",values=colour_values) 
		
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