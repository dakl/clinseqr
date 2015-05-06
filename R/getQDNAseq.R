#' Get segmented CNV data
#' 
#' @param reports data frame with reports
#' @param columnToUse column to use for getting values
#' @return A data frame with segmented value, or other value
#' @examples
#' #dat <- getReadcountPerChr(reports)
#' #dat <- getReadcountPerChr(reports, columnToUse="segmented")
getQDNAseq <- function(reports, columnToUse="segmented"){
  dat <- makeEmptyDataTable(header = c("chr", "start", "end", "gc", "mappability"))
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports$WGS_TUMOR_QDNASEQ[k],sep="/")
    if(file.exists(infile)){
      tb <- fread(infile)
      dat <- rbindlist(list(dat, data.table(tb$chromosome, tb$start, tb$end, tb$gc, tb$mappability)))
      break
    }
  }
  
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports$WGS_TUMOR_QDNASEQ[k],sep="/")
    if(file.exists(infile)){
      tb <- fread(infile)
      dat[, eval(reports$REPORTID[k]) := tb[,columnToUse, with=FALSE ] ]
    }else{
      dat[, eval(reports$REPORTID[k]):=NA ]
    }
    dot(k, every=10)
  }
  dat
}

