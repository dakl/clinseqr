#' Get normalized gene body coverage for reports
#' 
#' @param reports data frame with reports (commonly from \code(getReports()) )
#' @return A data frame with normalized gene body coverage for all samples
#' @examples
#' dat <- getGeneBodyCoverage(reports)
#' ggplot(dat, aes(x=NORM_POS, y=NORM_COV, colour=DataReportID)) + geom_line()
getGeneBodyCoverage <- function(reports){
  dat <- makeEmptyDataTable(c("NORM_POS", "NORM_COV", "DataReportID"))
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports$StarRNASeqMetrics[k],sep="/")
    
    if(file.exists(infile)){
      cmd <- paste("grep -A 101 normalized_position", infile)
      tb <- read.table(pipe(cmd), header=TRUE, sep="\t", row.names=NULL, stringsAsFactors=FALSE)
      colnames(tb) <- c("NORM_POS", "NORM_COV")
      tb$RNADataID <- reports$DataReportID[k]
    } else {
      tb <- data.table(NORM_POS=0:100, NORM_COV=NA, DataReportID=reports$DataReportID[k])
    }
    dat <- rbindlist(list(dat, tb))  
  }
  dat
}
