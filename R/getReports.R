#' Get a data frame with all reports found in path
#' 
#' @param path Path to directory with reports. 
#' @param recursive Boolean whether or not to recurse when looking for report files (default TRUE)
#' @return A data frame with reports
#' @examples
#' getReports("/home/Crisp/clinseq/BREASTv4")
getReports <- function(path, recursive=TRUE){
  ## get list of all report files
  reportFiles <- dir(path = path, pattern="report$", recursive=recursive)
  
  ## set up empty data frames
  reports <- read.table(reportFiles[1], sep="\t", header=TRUE, stringsAsFactors=FALSE)
  reports <- reports[-1,]
  
  ## read reports
  reportFile <- reportFiles[10]
  for(reportFile in reportFiles){ # 
    report <- read.table(reportFile, header=TRUE, stringsAsFactors=FALSE)
    reports <- rbind(reports, report)
  }
  reports
}

