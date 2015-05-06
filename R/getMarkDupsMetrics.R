#' Get MarkDups Metrics
#' 
#' @param reports data frame with reports
#' @param src string with name of column name to use (from reports)
#' @return A data table with metrics from markDuplicates
#' @examples
#' #dat <- getMarkDupsMetrics(reports, src="PANEL_MARKDUPS_METRICS")
#' #dat <- getMarkDupsMetrics(reports, src="WGS_MARKDUPS_METRICS")
getMarkDupsMetrics <- function(reports, src="PANEL_MARKDUPS_METRICS"){
  readM <- function(f){
    cmd <- paste("grep -A 2 UNPAIRED_READS_EXAMINED", f)
    dat <- read.table(pipe(cmd), header=TRUE, sep="\t", row.names=NULL, stringsAsFactors=FALSE)
    return(as.data.table(dat) )
  }
  mdHeader <- c("LIBRARY","UNPAIRED_READS_EXAMINED","READ_PAIRS_EXAMINED","UNMAPPED_READS","UNPAIRED_READ_DUPLICATES","READ_PAIR_DUPLICATES","READ_PAIR_OPTICAL_DUPLICATES","PERCENT_DUPLICATION","ESTIMATED_LIBRARY_SIZE", "DataReportID")
  
  dat <- makeEmptyDataTable(mdHeader)
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports[k, src],sep="/")
    
    if(file.exists(infile)){
      tb <- readM(infile)
    } else { 
      tb <- data.table(t(rep(NA, length(mdHeader))))
    }
    tb$REPORTID <- reports$REPORTID[k]
    dat <- rbindlist(list(dat, tb))  
  }
  dat
}

