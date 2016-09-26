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
          definition = function(.Object, otutable_filename, metadata_filename)  {

            if (missing(otutable_filename) || missing(metadata_filename)) stop("input missing!.")

            # THE_otu_table will usually have taxonomy column, so it will have one more column than THE_meta_expmt.
            THE_otu_table  = read.delim(file = otutable_filename,  sep = "\t", header = T, check.names = F, row.names = 1, quote = "#")
            THE_meta_expmt = read.csv  (file = metadata_filename, sep = ",",  header = T, check.names = F)
            # dim(THE_otu_table); dim(THE_meta_expmt)

            # Taxonomy
            THE_taxonomy = data.frame(taxonomy = THE_otu_table$taxonomy, row.names = rownames(THE_otu_table));
            THE_taxonomy$taxonomy = (gsub("; ", ";", THE_taxonomy$taxonomy))
            THE_taxonomy$taxonomy = (gsub("\\.", "", THE_taxonomy$taxonomy))

            # Remove taxonomy from The OTU Table.
            THE_otu_table$taxonomy <- NULL

            # Taxonomy into 7 ranks
            # install.packages(c("tidyr", "devtools"))
            library(tidyr)
            THE_taxonomy = separate(THE_taxonomy, taxonomy , into = c("k", "p", "c", "o", "f", "g", "s"), sep = ";", extra = "drop")

            .Object@otu_table = THE_otu_table
            .Object@meta_data = THE_meta_expmt
            .Object@taxonomy = THE_taxonomy

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
  print("hi")
  new("otuBentoBox", otutable_filename, metadata_filename)
}




