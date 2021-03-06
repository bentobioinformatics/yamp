#' createOtuBentoBoxFromFiles
#'
#' This function creates an OtuBentoBox from two file inputs: Otu table and a Meta data.
#' @param dataFile_otutable Otu table file
#' @param dataFile_metadata Meta data file
#' @export
#' @examples
#' aNewBentoBox = createOtuBentoBoxFromFiles(dataFile_otutable, dataFile_metadata)
createOtuBentoBoxFromFiles <- function (f_otutable, f_metadata) {

  cat("Starting createOtuBentoBoxFromFiles...\n")

  if (missing(f_otutable) || missing(f_metadata)) stop("input missing.")

  # otutable will usually have taxonomy column, so it will have one more column than metaexpmt.
  otutable = read.delim(file = f_otutable, sep = "\t", header = T, check.names = F, row.names = 1, quote = "\"", skip = 1)
  metadata = read.delim(file = f_metadata, sep = "\t", header = T, check.names = F, row.names = 1)
  # dim(otutable); dim(metadata)

  # Taxonomy
  taxonomy = data.frame(taxonomy = otutable$taxonomy, row.names = rownames(otutable));
  taxonomy$taxonomy = (gsub("; ", ";", taxonomy$taxonomy))
  taxonomy$taxonomy = (gsub("\\.", "", taxonomy$taxonomy))

  # Remove taxonomy from OTU Table.
  otutable$taxonomy <- NULL

  # order metadata by otu colnames
  metadata = metadata[colnames(otutable), ]

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

  # # Check integrity
  # otutable.numberofsamples = ncol(otutable)
  # otutable.numberofotus = nrow(otutable)
  # taxonomy.numberofotus = nrow(taxonomy)
  # metadata.numberofsamples = nrow(metadata)
  #
  # # Output
  # cat(paste("[otutable] Number of OTUs   :", dim(otutable)[1], "\n"))
  # cat(paste("           Number of samples:", dim(otutable)[2], "\n"))
  # cat(paste("[metadata] Number of samples:", dim(metadata)[1], "\n"))
  # cat(paste("[taxonomy] Number of OTUs   :", dim(taxonomy)[1], "\n"))
  #
  # if (otutable.numberofsamples != metadata.numberofsamples) stop("The number of samples don't match.")
  # if (otutable.numberofotus != taxonomy.numberofotus) stop("The number of OTUs don't match.")

  # Bung them
  newOtuBentoBox = otuBentoBox(otutable, metadata, taxonomy)
  return(newOtuBentoBox)

}


# Test
# dataFile_otutable = c("~/Library/Mobile\ Documents/com~apple~CloudDocs/Project/ThamesSampling/04_Bioinformatics/ThamesWBS/ITS/otu_table.txt")
# dataFile_metadata = c("~/Library/Mobile\ Documents/com~apple~CloudDocs/Project/ThamesSampling/04_Bioinformatics/ThamesWBS/ITS/metadata_combined.txt")
# otuBentoBox_ITS = otuBentoBox(dataFile_otutable, dataFile_metadata)
# colnames(otuBentoBox_ITS@otutable)
# rownames(otuBentoBox_ITS@metadata)
# View(otuBentoBox_16S@otutable)
# View(otuBentoBox_16S@metadata)
#
# otutable = read.delim(file = dataFile_otutable, sep = "\t", header = T, check.names = F, row.names = 1, quote = "#")
# metadata = read.delim(file = dataFile_metadata, sep = "\t", header = T, check.names = F, row.names = 1)




