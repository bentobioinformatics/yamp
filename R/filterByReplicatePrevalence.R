#' Filter out OTUs with less than x\% prevalence
#'
#' Say you have collected 10 replicates.
#' Only 4 of 10 of the replicates have a particular OTU.
#' Is that OTU a good representative of your sample?
#' When would you say that OTU represents your sample?
#' Let's say you want to retain OTUs which are found in more than 75% of the replicates, then this function will deal with this situation.
#' Choose prv = 0.75 to achieve this.
#' @param bentoBox Your Bento Box
#' @param column Column in metadata to group replicates
#' @param threshold prv (default 0.75). So, 0.5 means an OTU which is present in less than 50\% of all replicates will be removed.
#' @export
#' @examples
#' aNewBentoBox = filterByReplicatePrevalence(bentoBox, column, prv = 0.5)
filterByReplicatePrevalence <- function (bentoBox, column, prv = 0.75) {

  .bentoBox = bentoBox

  for (.replicateGroup in levels(.bentoBox@metadata[[column]])) {
    cat(paste("processing:", .replicateGroup, "\n"))
    otutable_replicateGroup = .bentoBox@otutable[, .bentoBox@metadata$replicateGroup == .replicateGroup, drop = FALSE]
    if (!is.null(dim(otutable_replicateGroup))) {
      prevalence = apply(otutable_replicateGroup != 0, 1, mean)
      otutable_replicateGroup[prevalence < prv, ] = 0
      .bentoBox@otutable[, .bentoBox@metadata$replicateGroup == .replicateGroup] = otutable_replicateGroup
    }
  }

  .zeroSumOTUs = which(!apply(.bentoBox@otutable, 1, FUN = function(x){ sum(x) == 0 }))
  .bentoBox@otutable = .bentoBox@otutable[.zeroSumOTUs, , drop = FALSE]
  .bentoBox@taxonomy = .bentoBox@taxonomy[.zeroSumOTUs, , drop = FALSE]

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
#
#
# # Test
# if (FALSE) {
#   dataFile_otutable = c("~/Library/Mobile\ Documents/com~apple~CloudDocs/Project/ThamesSampling/04_Bioinformatics/ThamesWBS/16S/otu_table.txt")
#   dataFile_metadata = c("~/Library/Mobile\ Documents/com~apple~CloudDocs/Project/ThamesSampling/05_Analysis/ThamesWBS/metadata_combined.txt")
#   bentoBox = otuBentoBox(dataFile_otutable, dataFile_metadata)
#   bentoBox_ = filterBasedOnMetadataFromBentoBox(bentoBox, "Habitat", c("Water_GFA", "Water_M", "Biofilm", "Sediment_T"), type = "remove")
#
#   dim(bentoBox@otutable)
#   dim(bentoBox@metadata)
#   dim(bentoBox@taxonomy)
#
#   dim(bentoBox_@otutable)
#   dim(bentoBox_@metadata)
#   dim(bentoBox_@taxonomy)
#
#   a = bentoBox@otutable
#   o = bentoBox@otutable[, bentoBox@metadata[["Habitat"]] %in% c("Water_GFA", "Water_M", "Biofilm", "Sediment_T"),]
#   dim(o)
#   m = bentoBox@metadata[bentoBox@metadata[["Habitat"]] %in% c("Water_GFA", "Water_M", "Biofilm", "Sediment_T"), ]
#   dim(m)
#   View(bentoBox@otutable[1:2])
# }
