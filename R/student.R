Student<-setClass(
  "Student",
  slots=c(Name="character",
          Age="numeric",
          Gender="character"),
  prototype=list(Name=NULL,
                 Age=NULL,
                 Gender=NULL),
  # Make a function to test data consistency
  # Not called if an initialize function is defined
  validity=function(object) {
    if(object@Age>6)
    {
      return(TRUE)
    }else
    {
      return("Too young to!")
    }
  }
)
