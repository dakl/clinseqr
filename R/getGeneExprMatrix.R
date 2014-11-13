#' Get gene expression from htseq-count
#' 
#' @param reports data frame with reports
#' @return A data frame with counts from htseq-count
#' @examples
#' #dat <- getGeneExprMatrix(reports)
getGeneExprMatrix <- function(reports){
  quant <- makeEmptyDataTable(header = c("ENSG"))
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports$HtseqCountTable[k],sep="/")
    if(file.exists(infile)){
      tb <- fread(infile)
      quant <- rbindlist(list(quant, data.table(tb$V1)))
      break
    }
  }
  
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports$HtseqCountTable[k],sep="/")
    if(file.exists(infile)){
      tb <- fread(infile)
      quant[, eval(reports$DataReportID[k]):=tb$V2]
    }else{
      quant[, eval(reports$DataReportID[k]):=NA ]
    }
    dot(k, every=10)
  }
  quant
}



