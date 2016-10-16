#' Class otuBentoBox
#'
#' Class \code{otuBentoBox} defines an OTU Bento Box class
#'
#' @name otuBentoBox-class
#' @rdname otuBentoBox-class
#' @exportClass otuBentoBox
setClass("otuBentoBox",
         slots = c(otutable = "data.frame",
                   metadata = "data.frame",
                   taxonomy = "data.frame")
)

#' Constructor method of otuBentoBox Class.
#'
#' @name otuBentoBox
#' @rdname otuBentoBox-class
setMethod(f = "initialize",
          signature = "otuBentoBox",
          definition = function(.Object, otutable, metadata, taxonomy)  {

            print("Checking integrity...")

            # Check integrity
            otutable.numberofsamples = ncol(otutable)
            otutable.numberofotus = nrow(otutable)
            taxonomy.numberofotus = nrow(taxonomy)
            metadata.numberofsamples = nrow(metadata)

            # Output
            cat(paste("[otutable] Number of OTUs   :", dim(otutable)[1], "\n"))
            cat(paste("           Number of samples:", dim(otutable)[2], "\n"))
            cat(paste("[metadata] Number of samples:", dim(metadata)[1], "\n"))
            cat(paste("[taxonomy] Number of OTUs   :", dim(taxonomy)[1], "\n"))

            if (otutable.numberofsamples != metadata.numberofsamples) stop("The number of samples don't match.")
            if (otutable.numberofotus != taxonomy.numberofotus) stop("The number of OTUs don't match.")

            # Bung them
            .Object@otutable = otutable
            .Object@metadata = metadata
            .Object@taxonomy = taxonomy

            validObject(.Object)
            return(.Object)
          }
)


#' Wrapper function otuBentoBox.
#'
#' @name otuBentoBox
#' @rdname otuBentoBox-class
#' @export
otuBentoBox <- function (otutable, metadata, taxonomy) {
  print("yo")
  new("otuBentoBox", otutable, metadata, taxonomy)
}

