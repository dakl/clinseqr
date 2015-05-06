#' Get gene expression from htseq-count
#' 
#' @param reports data frame with reports
#' @return A data frame with counts from htseq-count
#' @examples
#' #dat <- getGeneExprMatrix(reports)
getGeneExprMatrix <- function(reports){
  quant <- makeEmptyDataTable(header = c("ENSG"))
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports$RNASEQ_HTSEQ[k],sep="/")
    if(file.exists(infile)){
      tb <- fread(infile)
      quant <- rbindlist(list(quant, data.table(tb$V1)))
      break
    }
  }

  numWithNoData = 0
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports$RNASEQ_HTSEQ[k],sep="/")
    if(file.exists(infile)){
      tb <- fread(infile)
      quant[, eval(reports$REPORTID[k]):=tb$V2]
    }else{
      #quant[, eval(reports$REPORTID[k]):=NA ]
      numWithNoData = numWithNoData + 1
    }
    dot(k, every=10)
  }
  if(numWithNoData > 0){
    warning("No data could be found for ", numWithNoData, " sample(s).")
  }
  quant
}



