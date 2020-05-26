# Behind the scenes the functions
# Some are internal only and are not exposed to the user

# Internal function for checking the MFCLRep arguments
# Substitute for doing S4 methods
check.rep.args <- function(rep, rep.names=NULL){
  bad_argument_types_message <- "The function is expecting an MFCLRep object, or a list of MFCLRep objects."
  # If just a single MFCLRep coerce to an unamed list
  if (class(rep) == "MFCLRep"){
    rep <- list(rep)
  }
  # If it is a list, check that all elements are an MFCLRep object, otherwise fail
  if (class(rep) == "list"){
    if(!all(lapply(rep, class)=="MFCLRep")){
      stop(bad_argument_types_message)
    }
  }
  # If it's not a list of MFCLRep objects, fail
  else {
      stop(bad_argument_types_message)
  }
  
  # At this point rep is a list of MFCLRep objects
  # If rep.names is supplied, then name the list - overwriting any existing names
  if (!is.null(rep.names)){
    # Check length of rep.names matches length of list
    if(length(rep.names)!=length(rep)){
      stop("Length of rep.names must match the number of MFCLRep objects.")
    }
    names(rep) <- rep.names
  }
  # If there are still no names, make some up
  if(is.null(names(rep))){
     fake_names <- paste("Model", seq(from=1, to=length(rep)), sep="") 
     names(rep) <- fake_names
  }
  return(rep)
}

check.frq.args <- function(frq, frq.names=NULL){
  bad_argument_types_message <- "The function is expecting a frq object from the frqit package, or a list of frq objects."
  # If just a single frq coerce to an unamed list
  if (class(frq) == "frq"){
    frq <- list(frq)
  }
  # If it is a list, check that all elements are frq objects, otherwise fail
  if (class(frq) == "list"){
    if(!all(lapply(frq, class)=="frq")){
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
      

## Some tests - could be added to unit tests if needed
#rep1 <- MFCLRep()
#rep2 <- MFCLRep()
#rep3 <- MFCLRep()
#par1 <- MFCLPar()
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
