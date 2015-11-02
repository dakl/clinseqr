#' Get a data frame with all reports found in path
#' 
#' @param path Path to directory with reports. 
#' @param recursive Boolean whether or not to recurse when looking for report files (default TRUE)
#' @param absolutepaths Boolean whether or not to make paths absolute paths instead of relative (default FALSE)
#' @return A data frame with reports
#' @examples
#' reports1 <- getReports(path="/proj/b2010040/private/clinseq/LAMLv6/", recursive=TRUE, absolutepaths=TRUE, max=10)
#' reports2 <- getReports(path="/proj/b2010040/private/clinseq/bla/", recursive=TRUE, absolutepaths=TRUE, max=10)
getReports <- function(path, recursive=TRUE, absolutepaths=FALSE, max=1e6){
  ## get list of all report files
  reportFiles <- dir(path = path, pattern="report$", recursive=recursive)
  if(length(reportFiles) == 0){ ## if there aren't any report files in path, return NULL
    return(NULL)
  } else{
    prefix <- paste(path, dirname(reportFiles), sep="/")
    ## set up empty data frames
    reports <- read.table(paste(path,reportFiles[1],sep="/"), sep="\t", header=TRUE, stringsAsFactors=FALSE)
    reports$prefix <- NA
    reports <- reports[-1,]
    
    ## read reports
    for(k in 1:length(reportFiles)){ # 
      if( k > max ){
        break
      }
      reportFile <- reportFiles[k]
      report <- getReport(paste(path,reportFile, sep="/"), absolutepaths)
        #read.table(, header=TRUE, stringsAsFactors=FALSE)
      
      report$prefix <- prefix[k]
      reports <- rbind(reports, report)
    }
    
    reports
  }
}

#' Get a data frame with a single report from a path
#' 
#' @param report_File path to .report file
#' @param absolutepaths Boolean whether or not to make paths absolute paths instead of relative (default FALSE)
#' @return A data frame with the report
#' @examples
#' report <- getReport(report_file="/proj/b2010040/private/clinseq/BREASTv6/K19378/datareports//DataReport.DataReport_K19378T.report", absolutepaths=TRUE)
getReport <- function(report_file, absolutepaths=FALSE){
  report <- fread(report_file)
  report$prefix <- dirname(report_file)
  if(absolutepaths){
    for( n in 11:ncol(report)){
      if(!is.na( report[[names(report)[n]]] )){        
        report[[names(report)[n]]] <- paste(dirname(report_file), report[[names(report)[n]]], sep="/")
      }
    }
  }
  return(report)
}
