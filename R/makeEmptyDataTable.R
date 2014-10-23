#' Make an empty data.table from a vector of column names
#' 
#' @param header Vector with column names
#' @examples
#' makeEmptyDataTable(c("Column1", "Column2"))
makeEmptyDataTable <- function(header){
  tab <- data.table(matrix(NA, ncol=length(header)))
  setnames(tab, names(tab), header)
  tab <- tab[-1,]
  tab
}

