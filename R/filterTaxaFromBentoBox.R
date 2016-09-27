#' Remove taxa from an OTU Bento Box
#'
#' This function removes taxa from an OTU Bento Box
#' @param bentoBox Your Bento Box
#' @param classificationLevel Your classificationLevel
#' @param nameToRemove Names to remove
#' @param remove Whether of not you want to remove or retain (default = TRUE)
#' @export
#' @examples
#' aNewBentoBox = filterTaxaFromBentoBox(aBentoBox, "k", "k__Bacteria", remove = TRUE)

filterTaxaFromBentoBox <- function (bentoBox, classificationLevel, nameToRemove, remove = TRUE) {

  .bentoBox = bentoBox

  # Filter
  OTUsToExclude = rownames(bentoBox@taxonomy[bentoBox@taxonomy[[classificationLevel]] == nameToRemove, ])

  if (remove == TRUE) {
    .bentoBox@otutable = bentoBox@otutable[!rownames(bentoBox@otutable) %in% OTUsToExclude, ]
    .bentoBox@taxonomy = bentoBox@taxonomy[!rownames(bentoBox@taxonomy) %in% OTUsToExclude, ]
  } else {
    .bentoBox@otutable = bentoBox@otutable[rownames(bentoBox@otutable) %in% OTUsToExclude, ]
    .bentoBox@taxonomy = bentoBox@taxonomy[rownames(bentoBox@taxonomy) %in% OTUsToExclude, ]
  }

  # Clean
  .bentoBox@otutable = droplevels(.bentoBox@otutable)
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
bentoBox_ = filterTaxaFromBentoBox(bentoBox, "c", "c__Chloroplast", remove = FALSE)

dim(bentoBox@otutable)
dim(bentoBox@metadata)
dim(bentoBox@taxonomy)

dim(bentoBox_@otutable)
dim(bentoBox_@metadata)
dim(bentoBox_@taxonomy)
}
