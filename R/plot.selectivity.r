

#' Compare the estimated selectivity across different models.
#' 
#' @param rep.list A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.
#' @param rep.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.
#' @param sel.basis A character string indicating if selectivity at age ('AGE') or length ('Length') should be plotted
#' @param palette.func A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().
#' @param fisheries A vector giving the number of the fisheries to plot. Default is to plot everything.
#' @param fishery_names The names of the fisheries to plot. If not supplied, the fishery numbers from the fisheries argument is used.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @param ... Passes extra arguments to the palette function. Use the argument all.model.colours to ensure consistency of model colours when plotting a subset of models.
#' @export
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom data.table setnames
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

plot.selectivity = function(rep.list,rep.names=NULL,sel.basis="AGE", palette.func=default.model.colours,fisheries, fishery.names=as.character(fisheries), save.dir,save.name, ...)
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
			tmp.sel = data.table::as.data.table(sel(tmp.rep)) %>% .[,.(age,unit,value)] %>% .[,age:=as.numeric(age)] %>% .[,unit:=as.numeric(unit)] %>% .[,model:=tmp.name] %>% data.table::setnames(.,"unit","fishery") %>% .[order(model,fishery,age),.(model,fishery,age,value)]
			tmp.laa = c(aperm(mean_laa(tmp.rep),c(4,1,2,3,5,6)))

			if(sel.basis == "AGE")
			{
				xlab = "Age class"
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
			
		# only plot fisheries that are in the provided vector (if provided)
			if(!missing(fisheries))
			{
				plot.dt = plot.dt[fishery %in% fisheries]
			}
			
    # Want pdat to have Model names in the original order - important for plotting order
    plot.dt[,Model:=factor(model, levels=names(rep.list))]
    
    # Add in fishery names for facets
    fishery_names_df <- data.frame(fishery = fisheries, fishery_names = fishery.names)
    plot.dt <- merge(plot.dt, fishery_names_df)

		# make plot
			# Get the colours - if all.model.names passed in using ... then it is passed to palette func
			colour_values <- palette.func(selected.model.names = names(rep.list), ...)
			g = plot.dt %>% 
			ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_wrap(~fishery_names) +
			ggplot2::xlab(xlab) + ggplot2::ylab("Selectivity") +
			ggplot2::ggtitle("Estimated selectivity by fishery") +
			ggplot2::geom_line(ggplot2::aes(x=x,y=value,color=model),size=1.25) +
			ggplot2::scale_color_manual("Model",values=colour_values)
		
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