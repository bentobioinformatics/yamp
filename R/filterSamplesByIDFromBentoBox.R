#' Filter samples from an OTU Bento Box by ID
#'
#' This function filter samples from an OTU Bento Box
#' @param bentoBox Your Bento Box
#' @param samplesToRemove a vector of samples to remove from Your Bento Box
#' @param type Remove or retain samples. Optinos: "remove", "remain" (default = "remove")
#' @export
#' @examples
#' aNewBentoBox = filterSamplesByIDFromBentoBox(aBentoBox, c("Sample1", "Sample2"), type = "remove")

filterSamplesByIDFromBentoBox <- function (bentoBox, samplesToRemove, type = "remove") {

  print("filtering")

  .bentoBox = bentoBox

  # Filter
  if (type == "remove") {
    .bentoBox@otutable = bentoBox@otutable[, !colnames(bentoBox@otutable) %in% samplesToRemove, drop = FALSE]
    .bentoBox@metadata = bentoBox@metadata[!colnames(bentoBox@otutable) %in% samplesToRemove, , drop = FALSE]
  } else if (type == "retain") {
    .bentoBox@otutable = bentoBox@otutable[, which(colnames(bentoBox@otutable) %in% samplesToRemove), drop = FALSE]
    .bentoBox@metadata = bentoBox@metadata[which(colnames(bentoBox@otutable) %in% samplesToRemove), , drop = FALSE]
  } else {
    stop("Type needs to be either remove or retain")
  }

  # Remove zero-sum OTUs
  .zeroSumOTUs = which(!apply(.bentoBox@otutable, 1, FUN = function(x){ sum(x) == 0 }))
  .bentoBox@otutable = .bentoBox@otutable[.zeroSumOTUs, ]
  .bentoBox@taxonomy = .bentoBox@taxonomy[.zeroSumOTUs, ]

  # Clean
  .bentoBox@otutable = droplevels(.bentoBox@otutable)
  .bentoBox@metadata = droplevels(.bentoBox@metadata)

  # Output
  cat(paste("[otutable] Number of OTUs   :", dim(.bentoBox@otutable)[1], "\n"))
  cat(paste("           Number of samples:", dim(.bentoBox@otutable)[2], "\n"))
  cat(paste("[metadata] Number of samples:", dim(.bentoBox@metadata)[1], "\n"))
  cat(paste("[taxonomy] Number of OTUs   :", dim(.bentoBox@taxonomy)[1], "\n"))
  return(.bentoBox)

}

# Test
if (FALSE) {
dataFile_otutable = c("~/Library/Mobile\ Documents/com~apple~CloudDocs/Project/ThamesSampling/04_Bioinformatics/ThamesWBS/16S/otu_table.txt")
dataFile_metadata = c("~/Library/Mobile\ Documents/com~apple~CloudDocs/Project/ThamesSampling/05_Analysis/ThamesWBS/metadata_combined.txt")
bentoBox = otuBentoBox(dataFile_otutable, dataFile_metadata)
bentoBox_ = filterSamplesFromBentoBox(otuBentoBox_16S, c("EB01", "EB02", "EB03", "EB04", "EB05"), type = "retain")

dim(bentoBox@otutable)
dim(bentoBox@metadata)
dim(bentoBox@taxonomy)

dim(bentoBox_@otutable)
dim(bentoBox_@metadata)
dim(bentoBox_@taxonomy)
}
