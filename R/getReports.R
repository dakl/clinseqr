#' Get a data frame with all reports found in path
#' 
#' @param path Path to directory with reports. 
#' @param recursive Boolean whether or not to recurse when looking for report files (default TRUE)
#' @return A data frame with reports
#' @examples
#' # getReports("/home/Crisp/clinseq/BREASTv4")
getReports <- function(path, recursive=TRUE){
  ## get list of all report files
  reportFiles <- dir(path = path, pattern="report$", recursive=recursive)
  prefix <- paste(path, dirname(reportFiles), sep="/")
  ## set up empty data frames
  reports <- read.table(paste(path,reportFiles[1],sep="/"), sep="\t", header=TRUE, stringsAsFactors=FALSE)
  reports <- reports[-1,]
  
  ## read reports
  reportFile <- reportFiles[10]
  for(reportFile in reportFiles){ # 
    report <- read.table(paste(path,reportFile, sep="/"), header=TRUE, stringsAsFactors=FALSE)
    reports <- rbind(reports, report)
  }
  reports$prefix <- prefix
  reports
}

