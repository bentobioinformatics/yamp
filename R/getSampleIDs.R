#' getSampldIDs
#'
#' This function retrieves Sample IDs from the OTU table.
#' @name getSampldIDs
#' @param otu_table Your OTU Table.
#' @keywords OTU sampleIDs
#' @export
#' @examples
#' SampleIDs = getSampldIDs("otu_table.txt")
getSampldIDs <- function (otu_table) {
  print("reading...")
  header <- scan(otu_table, skip = 1, nlines = 1, what = character(), sep = "\t")
  header = header[2:(length(header)-1)]
  header
}




