#' Filter OTUs from an OTU Bento Box
#'
#' This function filters OTUs from an OTU Bento Box
#' @param bentoBox Your Bento Box
#' @param OTUsToRemove a vector of samples to remove from Your Bento Box
#' @param type Remove or retain samples. Optinos: "remove", "remain" (default = "remove")
#' @export
#' @examples
#' aNewBentoBox = filterOTUsFromBentoBox(aBentoBox, c("OTU39359", "OTU2365"), type = "remove")

filterOTUsFromBentoBox <- function (bentoBox, OTUsToRemove, type = "remove") {

  .bentoBox = bentoBox

  # Filter
  if (type == "remove") {
    .bentoBox@otutable = bentoBox@otutable[!rownames(bentoBox@otutable) %in% OTUsToRemove, , drop = FALSE]
    .bentoBox@taxonomy = bentoBox@taxonomy[!rownames(bentoBox@taxonomy) %in% OTUsToRemove, , drop = FALSE]
  } else if (type == "retain") {
    .bentoBox@otutable = bentoBox@otutable[rownames(bentoBox@otutable) %in% OTUsToRemove, , drop = FALSE]
    .bentoBox@taxonomy = bentoBox@taxonomy[rownames(bentoBox@taxonomy) %in% OTUsToRemove, , drop = FALSE]
  } else {
    stop("Type needs to be either remove or retain")
  }

  # Clean
  .bentoBox@otutable = droplevels(.bentoBox@otutable)
  .bentoBox@metadata = droplevels(.bentoBox@metadata)
  .bentoBox@taxonomy = droplevels(.bentoBox@taxonomy)

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
