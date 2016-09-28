#' Filter out OTUs with less than x% prevalence
#'
#' Say you have collected 10 replicates. Only 5 out of 10 of the replicates have an OTU. This function filters out such OTUs. The idea is to have a dataset which is a good representative of the site.
#' @param bentoBox Your Bento Box
#' @param column Column in metadata to group replicates
#' @param prevalence prevalence. So, 0.75 means an OTU which is present in less than 75% of all replicates will be removed.
#' @export
#' @examples
#' aNewBentoBox =

filterByReplicatePrevalence <- function (bentoBox, column, prevalence = 0.75) {

  for (.replicateGroup in levels(bentoBox@metadata[[column]])) {
    print(.replicateGroup)
    # otutable_replicateGroup <- otutable[, metadata$replicateGroup == .replicateGroup]
    # if (!is.null(dim(otutable_replicateGroup))) {
    #   prevalence = apply(otutable_replicateGroup != 0, 1, mean)
    #   otutable_replicateGroup[prevalence < 0.75, ] = 0
    #   otutable[, metadata$replicateGroup == .replicateGroup] <- otutable_replicateGroup
    # }
  }
  #
  #
  # .bentoBox = bentoBox
  #
  # # Filter
  # if (type == "remove") {
  #   print("hi")
  #   .bentoBox@otutable = bentoBox@otutable[, !bentoBox@metadata[[column]] %in% termsToRetain]
  #   .bentoBox@metadata = bentoBox@metadata[!bentoBox@metadata[[column]] %in% termsToRetain, ]
  # } else if (type == "retain") {
  #   .bentoBox@otutable = bentoBox@otutable[, bentoBox@metadata[[column]] %in% termsToRetain]
  #   .bentoBox@metadata = bentoBox@metadata[bentoBox@metadata[[column]] %in% termsToRetain, ]
  # } else {
  #   stop("type needs to be either remove or retain")
  # }
  #
  # # Remove zero-sum OTUs
  # .zeroSumOTUs = which(!apply(.bentoBox@otutable, 1, FUN = function(x){ sum(x) == 0 }))
  # .bentoBox@otutable = .bentoBox@otutable[.zeroSumOTUs, ]
  # .bentoBox@taxonomy = .bentoBox@taxonomy[.zeroSumOTUs, ]
  #
  # # Drop levels
  # .bentoBox@otutable = droplevels(.bentoBox@otutable)
  # .bentoBox@metadata = droplevels(.bentoBox@metadata)
  # .bentoBox@taxonomy = droplevels(.bentoBox@taxonomy)
  #
  # # Output
  # cat(paste("[otutable] Number of OTUs   :", dim(.bentoBox@otutable)[1], "\n"))
  # cat(paste("           Number of samples:", dim(.bentoBox@otutable)[2], "\n"))
  # cat(paste("[metadata] Number of samples:", dim(.bentoBox@metadata)[1], "\n"))
  # cat(paste("[taxonomy] Number of OTUs   :",    dim(.bentoBox@taxonomy)[1], "\n"))
  # return(.bentoBox)

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


