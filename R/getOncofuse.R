#' Get Oncofuse results
#' 
#' @param reports data frame with reports
#' @return A data table with Oncofuse results
#' @examples
#' #dat <- getOncofuse(reports, src="RNASEQ_ONCOFUSE")
getOncofuse <- function(reports, src="RNASEQ_ONCOFUSE"){
  mdHeader <- c("SAMPLE_ID","FUSION_ID","TISSUE","SPANNING_READS","ENCOMPASSING_READS","GENOMIC","5_FPG_GENE_NAME","5_IN_CDS?","5_SEGMENT_TYPE","5_SEGMENT_ID","5_COORD_IN_SEGMENT","5_FULL_AA","5_FRAME","3_FPG_GENE_NAME","3_IN_CDS?","3_SEGMENT_TYPE","3_SEGMENT_ID","3_COORD_IN_SEGMENT","3_FULL_AA","3_FRAME","FPG_FRAME_DIFFERENCE","P_VAL_CORR","DRIVER_PROB","EXPRESSION_GAIN","5_DOMAINS_RETAINED","3_DOMAINS_RETAINED","5_DOMAINS_BROKEN","3_DOMAINS_BROKEN","5_PII_RETAINED","3_PII_RETAINED","CTF","G","H","K","P","TF", "REPORTID")
  
  dat <- makeEmptyDataTable(mdHeader)
  for(k in 1:nrow(reports)){ #k <- 3
    infile   <- paste(reports$prefix[k] ,reports[k,src],sep="/")
    
    if(file.exists(infile)){
      tb <- fread(infile)
      tb$REPORTID <- reports$REPORTID[k]
    }
    dat <- rbindlist(list(dat, tb))  
  }
  dat
}
