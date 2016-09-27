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

            # Convert NA to unclassified
            taxonomy$p[is.na(taxonomy$p)] = "p__"
            taxonomy$c[is.na(taxonomy$c)] = "c__"
            taxonomy$o[is.na(taxonomy$o)] = "o__"
            taxonomy$f[is.na(taxonomy$f)] = "f__"
            taxonomy$g[is.na(taxonomy$g)] = "g__"
            taxonomy$s[is.na(taxonomy$s)] = "s__"

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
otuBentoBox <- function (otutable_filename, metadata_filename) {
  new("otuBentoBox", otutable_filename, metadata_filename)
}

# Test
# dataFile_otutable = c("~/Library/Mobile\ Documents/com~apple~CloudDocs/Project/ThamesSampling/04_Bioinformatics/ThamesWBS/16S/otu_table.txt")
# dataFile_metadata = c("~/Library/Mobile\ Documents/com~apple~CloudDocs/Project/ThamesSampling/05_Analysis/ThamesWBS/metadata_combined.txt")
# otuBentoBox_16S = otuBentoBox(dataFile_otutable, dataFile_metadata)






