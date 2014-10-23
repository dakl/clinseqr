# get a data frame with all reports found in path
getReports <- function(path){
  ## get list of all report files
  reportFiles <- dir(path = path, pattern="report$", recursive=TRUE)
  
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

