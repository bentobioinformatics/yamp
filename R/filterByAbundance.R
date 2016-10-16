#' Filter out OTUs by abundance
#'
#' This function will remove OTUs below the total abundance of x as a fraction, i.e. 0.5 = 50\%
#' See http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3531572/ if interested in finding out which threshold to use
#' @param bentoBox Your Bento Box
#' @param abundance abundance (default = 0.0)
#' @export
#' @examples
#' aNewBentoBox = filterByAbundance(bentoBox, abundance = 0.5)
filterByAbundance <- function (bentoBox, abundance = 0.75) {

  .bentoBox = bentoBox

  countPerOtus = apply(.bentoBox@otutable, 1, sum)
  minimumThreshold = sum(countPerOtus)*abundance
  .bentoBox@otutable = .bentoBox@otutable[countPerOtus >= minimumThreshold, ]
  .bentoBox@taxonomy = .bentoBox@taxonomy[countPerOtus >= minimumThreshold, ]

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
