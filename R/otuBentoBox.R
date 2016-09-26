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

            cat(paste("Numner of samples: ", otutable.numberofsamples, "\n"))
            cat(paste("Numner of OTUs:    ", otutable.numberofotus, "\n"))

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


#' Remove taxa from an OTU Bento Box
#'
#' @name otuBentoBox
#' @rdname otuBentoBox
#' @param bentoBox Your Bento Box
#' @param classificationLevel Your classificationLevel
#' @param nameToRemove Names to remove
#' @export
#' @examples
#' aNewBentoBox = removeTaxaFromBentoBox(aBentoBox, "k", "k__Bacteria")
removeTaxaFromBentoBox <- function (bentoBox, classificationLevel, nameToRemove) {
  OTUsToExclude = rownames(bentoBox@taxonomy[ bentoBox@taxonomy[[classificationLevel]] == nameToRemove, ])
  print(OTUsToExclude)
  bentoBox@otutable = bentoBox@otutable[!rownames(bentoBox@otutable) %in% OTUsToExclude, ]
  bentoBox@taxonomy = bentoBox@taxonomy[!rownames(bentoBox@taxonomy) %in% OTUsToExclude, ]
  return(bentoBox)
}


#' Retain taxa from an OTU Bento Box
#'
#' @name otuBentoBox
#' @rdname otuBentoBox
#' @param bentoBox Your Bento Box
#' @param classificationLevel Your classificationLevel
#' @param nameToRetain Names to remove
#' @export
#' @examples
#' aNewBentoBox = retainTaxaFromBentoBox(aBentoBox, "k", "k__Bacteria")
retainTaxaFromBentoBox <- function (bentoBox, classificationLevel, nameToRemove) {
  OTUsToExclude = rownames(bentoBox@taxonomy[ bentoBox@taxonomy[[classificationLevel]] != nameToRemove, ])
  bentoBox@otutable = bentoBox@otutable[!rownames(bentoBox@otutable) %in% OTUsToExclude, ]
  bentoBox@taxonomy = bentoBox@taxonomy[!rownames(bentoBox@taxonomy) %in% OTUsToExclude, ]
  return(bentoBox)
}

# Test
# otuBentoBox_16S_2 = removeTaxaFromBentoBox(otuBentoBox_16S, c, "c__Chloroplast")


#' Remove samples from an OTU Bento Box
#'
#' Remove whole samples from an OTU Bento Box
#' @name otuBentoBox
#' @rdname otuBentoBox
#' @param bentoBox Your Bento Box
#' @param samples Samples to remove from Your Bento Box
#' @export
#' @examples
#' aNewBentoBox = removeSamplesFromBentoBox(aBentoBox, c("Sample1", "Sample2"))
removeSamplesFromBentoBox <- function (bentoBox, samplesToRemove) {
  bentoBox@otutable = bentoBox@otutable[, !colnames(bentoBox@otutable) %in% samplesToRemove]
  bentoBox@metadata = bentoBox@metadata[!rownames(bentoBox@metadata) %in% samplesToRemove, ]
  return(bentoBox)
}


