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

