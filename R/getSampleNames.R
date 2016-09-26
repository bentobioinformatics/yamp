#' getSampleNames
#'
#' This function retrieves sample names from the OTU table. The OTU table must be derived from biom.
#' @param otu_table OTU Table file
#' @keywords OTU sample names
#' @export
#' @examples
#' sampleNames = getSampleNames("otu_table.txt")
getSampleNames <- function (.f_otu_table) {
  print("reading...")
  header = scan(.f_otu_table, skip = 1, nlines = 1, what = character(), sep = "\t")
  header = header[2:(length(header)-1)]
  header
}
