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
      report <- read.table(paste(path,reportFile, sep="/"), header=TRUE, stringsAsFactors=FALSE)
      
      if(absolutepaths){
        for( n in 11:length(report)){
          if(!is.na(report[n])){
            report[n] <- paste(prefix[k], report[n], sep="/")
          }
        }        
      }
      
      report$prefix <- prefix[k]
      reports <- rbind(reports, report)
    }
    
    reports
  }
}

