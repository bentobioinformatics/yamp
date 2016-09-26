#' getSampldIDs
#'
#' @name getSampldIDs
#' @rdname getSampldIDs
#' @export
getSampldIDs <- function (otutable_filename) {
  header <- scan(dataFile_otu_table, skip = 1, nlines = 1, what = character(), sep = "\t")
  header = header[2:(length(header)-1)]
  header
}




