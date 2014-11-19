#' Get HS Metrics
#' 
#' @param reports data frame with reports
#' @param src string with name of column name to use (from reports)
#' @return A data table with hs metrics
#' @examples
#' #dat <- getHSMetrics(reports, src="RNASeqCapHsMetrics")
#' #dat <- getHSMetrics(reports, src="TumorHsMetrics")
#' #dat <- getHSMetrics(reports, src="NormalHsMetrics")
getHSMetrics <- function(reports, src="RNASeqCapHsMetrics"){
  readM <- function(f){
    cmd <- paste("grep -A 2 BAIT_SET", f)
    dat <- read.table(pipe(cmd), header=TRUE, sep="\t", row.names=NULL, stringsAsFactors=FALSE)
    #dat <- dat[,-match(c("SAMPLE", "LIBRARY", "READ_GROUP"), colnames(dat))]
    #dat <- data.frame( METRIC=colnames(dat), VALUE=as.numeric( dat[1,] ) )
    return(as.data.table(dat) )
  }
  
  hsHeader <- c("BAIT_SET","GENOME_SIZE","BAIT_TERRITORY","TARGET_TERRITORY","BAIT_DESIGN_EFFICIENCY","TOTAL_READS","PF_READS","PF_UNIQUE_READS","PCT_PF_READS","PCT_PF_UQ_READS","PF_UQ_READS_ALIGNED","PCT_PF_UQ_READS_ALIGNED","PF_UQ_BASES_ALIGNED","ON_BAIT_BASES","NEAR_BAIT_BASES","OFF_BAIT_BASES","ON_TARGET_BASES","PCT_SELECTED_BASES","PCT_OFF_BAIT","ON_BAIT_VS_SELECTED","MEAN_BAIT_COVERAGE","MEAN_TARGET_COVERAGE","PCT_USABLE_BASES_ON_BAIT","PCT_USABLE_BASES_ON_TARGET","FOLD_ENRICHMENT","ZERO_CVG_TARGETS_PCT","FOLD_80_BASE_PENALTY","PCT_TARGET_BASES_2X","PCT_TARGET_BASES_10X","PCT_TARGET_BASES_20X","PCT_TARGET_BASES_30X","PCT_TARGET_BASES_40X","PCT_TARGET_BASES_50X","PCT_TARGET_BASES_100X","HS_LIBRARY_SIZE","HS_PENALTY_10X","HS_PENALTY_20X","HS_PENALTY_30X","HS_PENALTY_40X","HS_PENALTY_50X","HS_PENALTY_100X","AT_DROPOUT","GC_DROPOUT","SAMPLE","LIBRARY","READ_GROUP", "DataReportID")
  dat <- makeEmptyDataTable(hsHeader)
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports[k, src],sep="/")
    
    if(file.exists(infile)){
      tb <- readM(infile)
    } else { 
      tb <- data.table(t(rep(NA, length(hsHeader))))
    }
    tb$DataReportID <- reports$DataReportID[k]
    dat <- rbindlist(list(dat, tb))  
  }
  dat
}


