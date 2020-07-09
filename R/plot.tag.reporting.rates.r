#' Plot the tag reporting rates
#' @param par An object of MFCLPar that contains the effort deviations.
#' @param grp.names Optional. Provide a character vector of names for the groups.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom data.table as.data.table
#' @importFrom ggthemes theme_few
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_crossbar
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 geom_hline
#' 

plot.tag.reporting.rates = function(par, grp.names = NULL, save.dir, save.name)
{
    if (class(par) != "MFCLPar"){
    stop("par argument must of type of type 'MFCLPar'.")
  }

  tag.dt = unique(cbind(c(tag_fish_rep_grp(par)),c(tag_fish_rep_rate(par)),c(tag_fish_rep_target(par)/100),c(1/(2*sqrt(tag_fish_rep_pen(par))))))
  colnames(tag.dt) = c("group","rr","prior_mean","prior_cv")
  upper.bound = subset(flags(par),flagtype==1&flag==33)$value/100
  tag.dt = data.table::as.data.table(tag.dt) %>% .[order(group)] %>% .[,ub:=prior_mean+1.96*prior_mean*prior_cv] %>%
           .[,lb:=prior_mean-1.96*prior_mean*prior_cv] %>%
           .[,lb:=ifelse(lb<0,0,lb)] %>% .[,ub:=ifelse(ub>upper.bound,upper.bound,ub)]


  if(is.null(grp.names))
  {
    tag.dt$names = paste0("G",1:nrow(tag.dt))
  } else {
    if(length(grp.names)==nrow(tag.dt))
    {
      tag.dt$names = grp.names
    } else {
      stop("Length of grp.names needs to match the number of reporting rate groups.")
    }
  }

  tag.dt$names = factor(tag.dt$names,levels=tag.dt$names)


        g = tag.dt %>% 
        ggplot2::ggplot() + ggthemes::theme_few() + ggplot2::coord_cartesian(xlim=c(0.9,1.1),ylim=c(0,1)) +
        ggplot2::theme(axis.title.x=ggplot2::element_blank(),axis.text.x=ggplot2::element_blank(),axis.ticks.x=ggplot2::element_blank()) +
        ggplot2::xlab("Tag return group") + ggplot2::ylab("Reporting rate") + ggplot2::facet_wrap(~names) +
        ggplot2::ggtitle("Estimated tag reporting rate by group") +
        ggplot2::geom_crossbar(ggplot2::aes(x=1,y=prior_mean,ymin=lb,ymax=ub),fill="gray90",col="gray75",width = 0.1) +
        ggplot2::geom_hline(yintercept = upper.bound,col="red",size=2) +
        ggplot2::geom_point(ggplot2::aes(x=1,y=rr),col="blue",size=2,shape=16)

    
  save_plot(save.dir, save.name, plot=g)
  
  return(g)
}
