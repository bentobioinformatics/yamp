#' Class otuBentoBox
#'
#' Class \code{otuBentoBox} defines a gas sensor device.
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
          definition = function(.Object, f_otutable, f_filename)  {

            if (missing(f_otutable) || missing(f_filename)) stop("input missing.")

            # otutable will usually have taxonomy column, so it will have one more column than metaexpmt.
            otutable = read.delim(file = f_otutable, sep = "\t", header = T, check.names = F, row.names = 1, quote = "#")
            metadata = read.delim(file = f_filename, sep = "\t", header = T, check.names = F)
            # dim(otutable); dim(metadata)

            # Taxonomy
            taxonomy = data.frame(taxonomy = otutable$taxonomy, row.names = rownames(otutable));
            taxonomy$taxonomy = (gsub("; ", ";", taxonomy$taxonomy))
            taxonomy$taxonomy = (gsub("\\.", "", taxonomy$taxonomy))

            # Remove taxonomy from OTU Table.
            otutable$taxonomy <- NULL

            # Taxonomy into 7 ranks
            # install.packages(c("tidyr", "devtools"))
            library(tidyr)
            taxonomy = separate(taxonomy, taxonomy , into = c("k", "p", "c", "o", "f", "g", "s"), sep = ";", extra = "drop", fill = "right")

            .Object@otutable = otutable
            .Object@metadata = metadata
            .Object@taxonomy = taxonomy

            validObject(.Object)
            return(.Object)
          }
)


#' Wrapper function Sensor.
#'
#' @name otuBentoBox
#' @rdname otuBentoBox-class
#' @export
otuBentoBox <- function (otutable_filename, metadata_filename) {
  new("otuBentoBox", otutable_filename, metadata_filename)
}




