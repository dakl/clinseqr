#' Get metrics from sailfish
#' 
#' @param reports data frame with reports
#' @return A data frame with metrics from sailfish
#' @examples
#' #dat <- getSailfishMetrics(reports)
getSailfishMetrics <- function(reports){
  metrics <- makeEmptyDataTable(header = c("Metric"))
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports$RNASEQ_SAILFISH_METRICS[k],sep="/")
    if(file.exists(infile)){
      tb <- fread(infile)
      metrics <- rbindlist(list(metrics, data.table(tb$V1)))
      break
    }
  }
  
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports$RNASEQ_SAILFISH_METRICS[k],sep="/")
    if(file.exists(infile)){
      tb <- fread(infile)
      metrics[, eval(reports$REPORTID[k]):=tb$V2]
    }else{
      metrics[, eval(reports$REPORTID[k]):=NA ]
    }
    dot(k, every=10)
  }
  metrics
}

