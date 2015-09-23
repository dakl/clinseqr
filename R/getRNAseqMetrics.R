#' Get normalized gene body coverage for reports
#' 
#' @param reports data frame with reports
#' @param src string with name of column name to use (from reports)
#' @return A data frame with normalized gene body coverage for all samples
#' @examples
#' #dat <- getRNAseqMetrics(reports)
#' #ggplot(subset(dat, METRIC=="PCT_RIBOSOMAL_BASES"), 
#' #       aes(x=DataReportID, y=VALUE, fill=DataReportID)) + 
#' #geom_bar(stat="identity")
getRNAseqMetrics <- function(reports, src="RNASEQ_RNASEQ_METRICS"){
  readM <- function(f){
    cmd <- paste("grep -A 2 RIBOSOMAL_BASES", f)
    dat <- read.table(pipe(cmd), header=TRUE, sep="\t", row.names=NULL, stringsAsFactors=FALSE)
    return(as.data.table(dat) )
  }
  myHeader <- c("PF_BASES", "PF_ALIGNED_BASES", "RIBOSOMAL_BASES", "CODING_BASES", "UTR_BASES", "INTRONIC_BASES", "INTERGENIC_BASES", "IGNORED_READS", "CORRECT_STRAND_READS", "INCORRECT_STRAND_READS", "PCT_RIBOSOMAL_BASES", "PCT_CODING_BASES", "PCT_UTR_BASES", "PCT_INTRONIC_BASES", "PCT_INTERGENIC_BASES", "PCT_MRNA_BASES", "PCT_USABLE_BASES", "PCT_CORRECT_STRAND_READS", "MEDIAN_CV_COVERAGE", "MEDIAN_5PRIME_BIAS", "MEDIAN_3PRIME_BIAS", "MEDIAN_5PRIME_TO_3PRIME_BIAS", "SAMPLE", "LIBRARY", "READ_GROUP", "DataReportID")
  dat <- makeEmptyDataTable(myHeader)
  
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports[k,src],sep="/")
    
    if(file.exists(infile)){
      tb <- readM(infile)
    } else {
      tb <- data.table(t(rep(NA, length(myHeader)-1)))
      setnames(tb, names(tb), myHeader[-(length(myHeader)-1)])
    }
    tb$REPORTID <- reports$REPORTID[k]
    dat <- rbindlist(list(dat, tb))  
  }
  dat
}

