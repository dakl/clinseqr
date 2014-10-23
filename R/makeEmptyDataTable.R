#### Fcn to make an empty data.table
makeEmptyDataTable <- function(header){
  tab <- data.table(matrix(NA, ncol=length(header)))
  setnames(tab, names(tab), header)
  tab <- tab[-1,]
  tab
}

