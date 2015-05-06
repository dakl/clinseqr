#' Get normalized gene body coverage for reports
#' 
#' @param reports data frame with reports
#' @return A data frame with normalized gene body coverage for all samples
#' @examples
#' # dat <- getGeneBodyCoverage(reports)
#' # ggplot(dat, aes(x=NORM_POS, y=NORM_COV, colour=REPORTID)) + geom_line()
getGeneBodyCoverage <- function(reports,src="RNASEQ_RNASEQ_METRICS"){
  dat <- makeEmptyDataTable(c("NORM_POS", "NORM_COV", "REPORTID"))
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports[k,src],sep="/")
    
    if(file.exists(infile)){
      cmd <- paste("grep -A 101 normalized_position", infile)
      tb <- read.table(pipe(cmd), header=TRUE, sep="\t", row.names=NULL, stringsAsFactors=FALSE)
      colnames(tb) <- c("NORM_POS", "NORM_COV")
      tb$REPORTID <- reports$REPORTID[k]
    } else {
      tb <- data.table(NORM_POS=0:100, NORM_COV=NA, REPORTID=reports$REPORTID[k])
    }
    dat <- rbindlist(list(dat, tb))  
  }
  dat
}

