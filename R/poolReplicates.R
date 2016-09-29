#' Pool replicates
#'
#' Resulting table is the mean count.
#' @param bentoBox Your Bento Box
#' @param column Column in metadata to group replicates
#' @export
#' @examples
#' aNewBentoBox = poolReplicates(bentoBox, "replicateGroup")
poolReplicates <- function (bentoBox, column) {

  .bentoBox = bentoBox

  .bentoBox@otutable = aggregate(t(.bentoBox@otutable), by=list(.bentoBox@metadata[[column]]), FUN=mean)
  row.names(.bentoBox@otutable) = .bentoBox@otutable$Group.1
  .bentoBox@otutable$Group.1 = NULL
  .bentoBox@otutable = as.data.frame(t(.bentoBox@otutable))

  library(plyr)
  .bentoBox@metadata = ddply(.bentoBox@metadata, "replicateGroup", function(z) head(z,1))
  rownames(.bentoBox@metadata) = colnames(.bentoBox@otutable)

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


# otuBentoBox_16S_pooled = aggregate(t(otuBentoBox_16S@otutable), by=list(otuBentoBox_16S@metadata$replicateGroup), FUN=mean)
# row.names(otuBentoBox_16S_pooled) = otuBentoBox_16S_pooled$Group.1
# otuBentoBox_16S_pooled$Group.1 = NULL
# otuBentoBox_16S_pooled = as.data.frame(t(otuBentoBox_16S_pooled))
#
# library(plyr)
# mon = ddply(otuBentoBox_16S@metadata, "replicateGroup", function(z) head(z,1))
# rownames(mon) = colnames(otuBentoBox_16S_pooled)

