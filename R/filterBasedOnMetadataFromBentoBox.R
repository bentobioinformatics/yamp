#' Filter entries based on user selected metadata
#'
#' Filter entries based on user selected metadata from an OTU Bento Box
#' @name otuBentoBox
#' @rdname otuBentoBox
#' @param bentoBox Your Bento Box
#' @param column Column in metadata
#' @param termsToRetain Terms to retain
#' @export
#' @examples
#' aNewBentoBox = filterBasedOnMetadataFromBentoBox(otuBentoBox_16S, "Habitat", c("Water_GFA", "Water_M", "Biofilm", "Sediment_T"))
filterBasedOnMetadataFromBentoBox <- function (bentoBox, column, termsToRetain, type = "remove") {

  .bentoBox = bentoBox

  # Filter
  if (type == "remove") {
    print("hi")
    .bentoBox@otutable = bentoBox@otutable[, bentoBox@metadata[[column]] %in% termsToRetain]
    .bentoBox@metadata = bentoBox@metadata[bentoBox@metadata[[column]] %in% termsToRetain, ]
  } else if (type == "retain") {
    .bentoBox@otutable = bentoBox@otutable[, !bentoBox@metadata[[column]] %in% termsToRetain]
    .bentoBox@metadata = bentoBox@metadata[!bentoBox@metadata[[column]] %in% termsToRetain, ]
  } else {
    stop("type needs to be either remove or retain")
  }

  # Remove zero-sum OTUs
  .zeroSumOTUs = which(!apply(.bentoBox@otutable, 1, FUN = function(x){ sum(x) == 0 }))
  .bentoBox@otutable = .bentoBox@otutable[.zeroSumOTUs, ]
  .bentoBox@taxonomy = .bentoBox@taxonomy[.zeroSumOTUs, ]

  # Drop levels
  .bentoBox@otutable = droplevels(.bentoBox@otutable)
  .bentoBox@metadata = droplevels(.bentoBox@metadata)
  .bentoBox@taxonomy = droplevels(.bentoBox@taxonomy)

  # Output
  cat(paste("[otutable] Number of OTUs   :", dim(.bentoBox@otutable)[1], "\n"))
  cat(paste("           Number of samples:", dim(.bentoBox@otutable)[2], "\n"))
  cat(paste("[metadata] Number of samples:", dim(.bentoBox@metadata)[1], "\n"))
  cat(paste("[taxonomy] Number of OTUs   :",    dim(.bentoBox@taxonomy)[1], "\n"))
  return(.bentoBox)

}


# Test
if (FALSE) {
  dataFile_otutable = c("~/Library/Mobile\ Documents/com~apple~CloudDocs/Project/ThamesSampling/04_Bioinformatics/ThamesWBS/16S/otu_table.txt")
  dataFile_metadata = c("~/Library/Mobile\ Documents/com~apple~CloudDocs/Project/ThamesSampling/05_Analysis/ThamesWBS/metadata_combined.txt")
  bentoBox = otuBentoBox(dataFile_otutable, dataFile_metadata)
  bentoBox_ = filterBasedOnMetadataFromBentoBox(bentoBox, "Habitat", c("Water_GFA", "Water_M", "Biofilm", "Sediment_T"), type = "remove")

  dim(bentoBox@otutable)
  dim(bentoBox@metadata)
  dim(bentoBox@taxonomy)

  dim(bentoBox_@otutable)
  dim(bentoBox_@metadata)
  dim(bentoBox_@taxonomy)

  a = bentoBox@otutable
  o = bentoBox@otutable[, bentoBox@metadata[["Habitat"]] %in% c("Water_GFA", "Water_M", "Biofilm", "Sediment_T"),]
  dim(o)
  m = bentoBox@metadata[bentoBox@metadata[["Habitat"]] %in% c("Water_GFA", "Water_M", "Biofilm", "Sediment_T"), ]
  dim(m)
  View(bentoBox@otutable[1:2])
}


