#' Get read count per chromsome
#' 
#' @param reports data frame with reports
#' @return A data frame with read counts per chr
#' @examples
#' #dat <- getReadcountPerChr(reports)
getReadcountPerChr <- function(reports){
  
  dat <- makeEmptyDataTable(header = c("chr"))
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports$ReadcountPerChr[k],sep="/")
    if(file.exists(infile)){
      tb <- fread(infile)
      dat <- rbindlist(list(dat, data.table(tb$V1)))
      break
    }
  }
  
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports$ReadcountPerChr[k],sep="/")
    if(file.exists(infile)){
      tb <- fread(infile)
      dat[, eval(reports$DataReportID[k]) := tb$V2]
    }else{
      dat[, eval(reports$DataReportID[k]):=NA ]
    }
    dot(k, every=10)
  }
  dat
}

