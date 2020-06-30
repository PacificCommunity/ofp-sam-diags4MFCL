# Behind the scenes the functions
# Some are internal only and are not exposed to the user


# Internal function for saving plots
# To save including in every plot function
save_plot <- function(save.dir, save.name, plot, width = 9, height = 9){
	# write.out
	if(!missing(save.dir))
	{
		if(missing(save.name))
		{
			stop("How can you save the output if you haven't specified the directory? Please specify save.dir.")
		} else {
			if (! dir.exists(save.dir))dir.create(save.dir,recursive=TRUE)
			ggplot2::ggsave(paste0(save.name,".png"),plot=plot, device = "png", path = save.dir,scale = 1, width = 9, height = 9, units = c("in"))
		}
	} 
  return(NULL)
}
		

# Internal function for checking the tagdat arguments
# Substitute for doing S4 methods
check.tagdat.args <- function(tagdat, tagdat.names=NULL){
  bad_argument_types_message <- "The function is expecting a tagdat data.frame, or a list of tagdat data.frames objects."
  # If just a single data.frame coerce to an unamed list
  if (class(tagdat) == "data.frame"){
    tagdat <- list(tagdat)
  }
  # If it is a list, check that all elements are data.frames, otherwise fail
  if (class(tagdat) == "list"){
    if(!all(lapply(tagdat, class)=="data.frame")){
      stop(bad_argument_types_message)
    }
  }
  # If it's not a list of data.frames, fail
  else {
      stop(bad_argument_types_message)
  }
  
  # At this point rep is a list of data.frame objects
  # If tagdat.names is supplied, then name the list - overwriting any existing names
  if (!is.null(tagdat.names)){
    # Check length of rep.names matches length of list
    if(length(tagdat.names)!=length(tagdat)){
      stop("Length of tagdat.names must match the number of data.frames in list.")
    }
    names(tagdat) <- tagdat.names
  }
  # If there are still no names, make some up
  if(is.null(names(tagdat))){
     fake_names <- paste("Model", seq(from=1, to=length(tagdat)), sep="") 
     names(tagdat) <- fake_names
  }
  return(tagdat)
}

# Internal function for checking lists of MFCLX arguments
check.list.args <- function(obj, obj.names=NULL, type){
  # Could extend to other types
  if (!(type %in% c("MFCLRep", "MFCLPar", "MFCLFrq"))){
    stop("type should be 'MFCLPar', 'MFCLRep' or 'MFCLFrq'")
  }
  
  bad_argument_types_message <- paste("The function is expecting an ", type, " object, or a list of ", type, " objects.", sep="")
  
  # If just a single object coerce to an unamed list
  if (class(obj) == type){
    obj <- list(obj)
  }
  # If it is a list, check that all elements are a type object, otherwise fail
  if (class(obj) == "list"){
    if(!all(lapply(obj, class)==type)){
      stop(bad_argument_types_message)
    }
  }
  # If it's not a list of type objects, fail
  else {
      stop(bad_argument_types_message)
  }
  
  # At this point obj is a list of type objects
  # If obj.names is supplied, then name the list - overwriting any existing names
  if (!is.null(obj.names)){
    # Check length of obj.names matches length of list
    if(length(obj.names)!=length(obj)){
      stop(paste("Length of obj.names must match the number of ", type, " objects.", sep=""))
    }
    names(obj) <- obj.names
  }
  # If there are still no names, make some up
  if(is.null(names(obj))){
     fake_names <- paste("Model", seq(from=1, to=length(obj)), sep="") 
     names(obj) <- fake_names
  }
  return(obj)
}

check.par.args <- function(par, par.names=NULL){
  out <- check.list.args(obj=par, obj.names = par.names, type="MFCLPar")
  return(out)
}

check.rep.args <- function(rep, rep.names=NULL){
  out <- check.list.args(obj=rep, obj.names = rep.names, type="MFCLRep")
  return(out)
}

check.frq.args <- function(frq, frq.names=NULL){
  out <- check.list.args(obj=frq, obj.names = frq.names, type="MFCLFrq")
  return(out)
}



# Some tests - could be added to unit tests if needed
#rep1 <- MFCLRep()
#rep2 <- MFCLRep()
#rep3 <- MFCLRep()
#par1 <- MFCLPar()
#par2 <- MFCLPar()
#
#rep_list_named <- list(rep1=rep1, rep2=rep2, rep3=rep3)
#rep_list_unnamed <- list(rep1, rep2, rep3)
#fake_list <- list(1,2,3)
#fake_list2 <- list(rep1,2,3)
#      
#check.rep.args(rep=fake_list) # Should fail
#check.rep.args(rep=fake_list2) # Should fail
#check.rep.args(rep=par1) # Should fail
#check.rep.args(rep=rep1)
#check.rep.args(rep=rep1, rep.names="Bob") # Should be OK
#check.rep.args(rep=rep1, rep.names=c("Bob","Carol")) # Should fail
#check.rep.args(rep=rep_list_unnamed, rep.names=c("Bob","Carol","Ted"))
#check.rep.args(rep=rep_list_named)

check.frqit.args <- function(frq, frq.names=NULL){
  bad_argument_types_message <- "The function is expecting a Frq object from the frqit package, or a list of Frq objects."
  # If just a single frq coerce to an unamed list
  if (class(frq) == "Frq"){
    frq <- list(frq)
  }
  # If it is a list, check that all elements are frq objects, otherwise fail
  if (class(frq) == "list"){
    if(!all(lapply(frq, class)=="Frq")){
      stop(bad_argument_types_message)
    }
  }
  # If it's not a list of frq objects, fail
  else {
      stop(bad_argument_types_message)
  }
  
  # At this point frq is a list of frq objects
  # If frq.names is supplied, then name the list - overwriting any existing names
  if (!is.null(frq.names)){
    # Check length of frq.names matches length of list
    if(length(frq.names)!=length(frq)){
      stop("Length of frq.names must match the number of frq objects.")
    }
    names(frq) <- frq.names
  }
  # If there are still no names, make some up
  if(is.null(names(frq))){
     fake_names <- paste("Model", seq(from=1, to=length(frq)), sep="") 
     names(frq) <- fake_names
  }
  return(frq)
}
      


# Palette functions - these can be passed into the plotting function to set the palette.
# The two name arguments are important because if you are only plotting a subset of the models
# you don't want the colour of the models to change.
# These need to be exported

#' Model palettes
#' 
#' These palettes can be passed to the plotting functions to change the model colours.
#' By passing in the \code{all.model.names} it is possible to ensure that the model colours are consistent when plotting subsets of the full model set.
#' You can even write your own function.
#' 
#' @param selected.model.names A character vector of the subset of model names you want to plot.
#' @param all.model.names A character vector of the all model names
#' @return A named vector of colours, the same length and names as the models to be plotted.
#' @name Colour palettes
#' @rdname colour.palettes
#' @examples 
#' all.model.names <- c("a","b","c","d")
#' # If you don't supply all.model.names, the colours for the models will not be consistent
#' default.model.colours(selected.model.names=all.model.names[1:2])
#' default.model.colours(selected.model.names=all.model.names[3:4])
#' # Colours for the models should not change when subsetting
#' # Supply all.model.names to make consistent
#' default.model.colours(selected.model.names=all.model.names, all.model.names=all.model.names)
#' default.model.colours(selected.model.names=all.model.names[1:2], all.model.names=all.model.names)
#' default.model.colours(selected.model.names=all.model.names[3:4], all.model.names=all.model.names)
#' @export
default.model.colours <- function(selected.model.names, all.model.names=selected.model.names){
  palette.cols <- c("royalblue3","deepskyblue1","gold","orange1","indianred1","firebrick2","#AC2020")
  out <- colorRampPalette(palette.cols)(length(all.model.names)-1)
  out <- c("black",out)[1:length(all.model.names)]
  names(out) <- all.model.names
  out <- out[selected.model.names]
  return(out)
}

# My colours function is called
#' @rdname colour.palettes
#' @importFrom RColorBrewer brewer.pal
#' @export
colourblind.model.colours <- function(selected.model.names, all.model.names=selected.model.names){
  out <- colorRampPalette(RColorBrewer::brewer.pal(12,"Paired"))(length(all.model.names))
  names(out) <- all.model.names
  out <- out[selected.model.names]
  return(out)
}

# Rainbow colors
#' @rdname colour.palettes
#' @export
rainbow.model.colours <- function(selected.model.names, all.model.names=selected.model.names){
  palette.cols <- c("#f44336","#e91e63","#9c27b0","#673ab7","#3f51b5","#2196f3","#03a9f4","#00bcd4","#009688","#4caf50","#8bc34a","#cddc39","#ffeb3b","#ffc107","#ff9800")
  out <- colorRampPalette(palette.cols)(length(all.model.names)-1)
  out <- c("black",out)[1:length(all.model.names)]
  names(out) <- all.model.names
  out <- out[selected.model.names]
  return(out)
}

# Internal function for checking the MFCLPar arguments
# Substitute for doing S4 methods
check.par.args <- function(par, par.names=NULL){
  bad_argument_types_message <- "The function is expecting an MFCLPar object, or a list of MFCLPar objects."
  # If just a single MFCLPar coerce to an unamed list
  if (class(par) == "MFCLPar"){
    par <- list(par)
  }
  # If it is a list, check that all elements are an MFCLPar object, otherwise fail
  if (class(par) == "list"){
    if(!all(lapply(par, class)=="MFCLPar")){
      stop(bad_argument_types_message)
    }
  }
  # If it's not a list of MFCLPar objects, fail
  else {
      stop(bad_argument_types_message)
  }

  # At this point par is a list of MFCLPar objects
  # If par.names is supplied, then name the list - overwriting any existing names
  if (!is.null(par.names)){
    # Check length of par.names matches length of list
    if(length(par.names)!=length(par)){
      stop("Length of par.names must match the number of MFCLPar objects.")
    }
    names(par) <- par.names
  }
  # If there are still no names, make some up
  if(is.null(names(par))){
     fake_names <- paste("Model", seq(from=1, to=length(par)), sep="")
     names(par) <- fake_names
  }
  return(par)
}
