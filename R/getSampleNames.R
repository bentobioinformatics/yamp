#' getSampleNames
#'
#' This function retrieves sample names from an OTU table.
#' @param file the name of the OTU table file which the data are to be read from. The OTU table must have derived from biom.
#' @keywords OTU sample names
#' @export
#' @examples
#' sampleNames = getSampleNames("otu_table.txt")

getSampleNames <- function (file) {
  header = scan(file, skip = 1, nlines = 1, what = character(), sep = "\t")
  header = header[2:(length(header)-1)]
  header
}
