#' Get isoform expression from sailfish output
#' 
#' @param reports data frame with reports
#' @param columnToUse Name of the column in sailfish output to use
#' @return A data frame with isoform quant from sailfish base on columnToUse
#' @examples
#' #dat <- getIsoformExprMatrix(reports)
#' #dat <- getIsoformExprMatrix(reports, columnToUse="TPM")
getIsoformExprMatrix <- function(reports, columnToUse="EstimatedNumKmers"){
  
  quant <- makeEmptyDataTable(header = c("ENST", "Length"))
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports$SailfishQuant[k],sep="/")
    if(file.exists(infile)){
      tb <- fread(infile)
      quant <- rbindlist(list(quant, data.table(tb$"# Transcript", tb$Length)))
      break
    }
  }
  
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports$SailfishQuant[k],sep="/")
    if(file.exists(infile)){
      tb <- fread(infile)
      quant[, eval(reports$DataReportID[k]):=tb[,columnToUse, with=FALSE]]
    }else{
      quant[, eval(reports$DataReportID[k]):=NA ]
    }
    dot(k, every=10)
  }
  quant
}

