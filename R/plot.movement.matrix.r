#' Plot the movement esimated movement for a single MFCLPar or the difference in the estimated movements for two pars with the same spatial, seasonal, age structure.
#' 
#' @param pars a list of two MFCLPar objects or a single MFCLPar object. The reference model should be the first in the list.
#' @param par.names an optional vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the par.list (if available) or generated automatically.
#' @param age.vec An integer value or vector denoting the age class or classes to plot the movement for. If missing, the average movement across all ages is plotted.
#' @param season.vec An integer value or vector of integers denoting which seasons to plot the movement for. If missing then the average movement across seasons is plotted.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom data.table as.data.table
#' @importFrom data.table setnames
#' @importFrom ggthemes theme_few
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 labeller
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 label_both
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 scale_fill_gradient2
#' 

plot.movement.matrix = function(pars,par.names,age.vec,season.vec,save.dir,save.name)
{
	pars = check.par.args(par=pars, par.names=par.names)
    par.names = names(pars)
    if(length(pars)>2)
    {
    	stop("This function is not defined for comparing more than two models.")
    } else if(length(pars)==2){
    	 move.coef.1 = data.table::as.data.table(diff_coffs_age_period(pars[[1]]))
    	 move.coef.2 = data.table::as.data.table(diff_coffs_age_period(pars[[2]]))

		if(missing(age.vec))
		{
			if(missing(season.vec))
			{				
				move.coef.1 = move.coef.1[,.(value=mean(value)),by=.(from,to)] %>% .[,age:="All ages"] %>% .[,age:=factor(age)] %>% .[,period:="All seasons"] %>% .[,period:=factor(period)]
				move.coef.2 = move.coef.2[,.(value=mean(value)),by=.(from,to)] %>% .[,age:="All ages"] %>% .[,age:=factor(age)] %>% .[,period:="All seasons"] %>% .[,period:=factor(period)]
			} else {
				move.coef.1 = move.coef.1[,.(value=mean(value)),by=.(from,to,period)] %>% .[,age:="All ages"] %>% .[,age:=factor(age)] %>% .[,period:=factor(as.character(period),levels=sort(unique(period)))]
				move.coef.2 = move.coef.2[,.(value=mean(value)),by=.(from,to,period)] %>% .[,age:="All ages"] %>% .[,age:=factor(age)] %>% .[,period:=factor(as.character(period),levels=sort(unique(period)))]
			}
		} else {
			if(missing(season.vec))
			{
				move.coef.1 = move.coef.1[age %in%age.vec,.(value=mean(value)),by=.(from,to,age)] %>% .[,age:=factor(as.character(age),levels=sort(unique(age)))] %>% .[,period:="All seasons"] %>% .[,period:=factor(period)]
				move.coef.2 = move.coef.2[age %in%age.vec,.(value=mean(value)),by=.(from,to,age)] %>% .[,age:=factor(as.character(age),levels=sort(unique(age)))] %>% .[,period:="All seasons"] %>% .[,period:=factor(period)]
			} else {
				move.coef.1 = move.coef.1[age %in%age.vec,.(value=mean(value)),by=.(from,to,age,period)] %>% .[,age:=factor(as.character(age),levels=sort(unique(age)))] %>% .[,period:=factor(as.character(period),levels=sort(unique(period)))]
				move.coef.2 = move.coef.2[age %in%age.vec,.(value=mean(value)),by=.(from,to,age,period)] %>% .[,age:=factor(as.character(age),levels=sort(unique(age)))] %>% .[,period:=factor(as.character(period),levels=sort(unique(period)))]
			}
		}

		move.coef = merge(move.coef.1,move.coef.2,by=c("age","period","from","to")) %>% .[,diff:=value.x-value.y]

		g = move.coef %>% data.table::setnames(.,c("age","period","from","to"),c("Age","Season","From","To")) %>%
		ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_grid(Age~Season,labeller = ggplot2::labeller(Age = ggplot2::label_both, Season = ggplot2::label_both)) +
		ggplot2::ggtitle("Estimated regional movement (Model 1 - Model 2)") +
		ggplot2::geom_tile(ggplot2::aes(x=To,y=From,fill=diff)) +
		ggplot2::scale_fill_gradient2("Diffusion difference")
    } else if(length(pars)==1){
    	move.coef = data.table::as.data.table(diff_coffs_age_period(pars[[1]]))

		if(missing(age.vec))
		{
			if(missing(season.vec))
			{
				move.coef = move.coef[,.(value=mean(value)),by=.(from,to)] %>% .[,age:="All ages"] %>% .[,age:=factor(age)] %>% .[,period:="All seasons"] %>% .[,period:=factor(period)]
			} else {
				move.coef = move.coef[,.(value=mean(value)),by=.(from,to,period)] %>% .[,age:="All ages"] %>% .[,age:=factor(age)] %>% .[,period:=factor(as.character(period),levels=sort(unique(period)))]
			}
		} else {
			if(missing(season.vec))
			{
				move.coef = move.coef[age %in%age.vec,.(value=mean(value)),by=.(from,to,age)] %>% .[,age:=factor(as.character(age),levels=sort(unique(age)))] %>% .[,period:="All seasons"] %>% .[,period:=factor(period)]
			} else {
				move.coef = move.coef[age %in%age.vec,.(value=mean(value)),by=.(from,to,age,period)] %>% .[,age:=factor(as.character(age),levels=sort(unique(age)))] %>% .[,period:=factor(as.character(period),levels=sort(unique(period)))]
			}
		}

		g = move.coef %>% data.table::setnames(.,c("age","period","from","to"),c("Age","Season","From","To")) %>%
		ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::facet_grid(Age~Season,labeller = ggplot2::labeller(Age = ggplot2::label_both, Season = ggplot2::label_both)) +
		ggplot2::ggtitle("Estimated regional movement") +
		ggplot2::geom_tile(ggplot2::aes(x=To,y=From,fill=value)) +
		ggplot2::scale_fill_gradientn("Diffusion",colors=c("royalblue3","deepskyblue1","gold","orange1","indianred1","firebrick2","#AC2020"))
    } else {
    	stop("If you are getting this error you probably did something wrong...")
    }

	

			save_plot(save.dir, save.name, plot=g, width = 16, height = 9)

    return(g)
}