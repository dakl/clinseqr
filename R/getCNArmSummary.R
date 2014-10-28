#' Make a table of CN summary stats per chromosome arm
#' 
#' @param segments Data table with segments from qdnaseq.
#' @param arms Data table with chromosome arms. 
#' @return A summary table per chromosome arm
#' @examples
#' #getCNArmSummary(segments, bandsFile = "~/genome/cytobands.txt")
getCNArmSummary <- function(segments, bandsFile){
  bands <- fread(bandsFile)
  setnames(bands, names(bands), c("chr", "start", "end", "band", "color"))
  
  bands$arm <- apply(bands, MAR=1, function(x){paste(x[1], substr(x[4], 1,1), sep="")})
  bands$arm[which(bands$color=="acen")] <- "cen"
  
  PROBCUTOFF <- 0.9 
  
  tab <- data.table(arm=NA, meanCN=NA, fracAMP=NA, fracDEL=NA)
  tab <- tab[-1]
  for( arm in unique( bands$arm ) ) { # arm <- "1p"
    if( arm == "cen" ) next
    armchr   <- bands$chr[ which(bands$arm == arm)[1] ]
    armstart <- min( bands$start[ which(bands$arm == arm) ] )
    armend   <- max( bands$end[ which(bands$arm == arm) ] )
    idx <- which ( segments$chr == armchr & segments$start > armstart & segments$end < armend )
    meancn <- fracamp <- fracdel <- 0
    if( length(idx) > 0 ){
      currentsegm  <- segments[idx]
      ## calc mean copy num
      meancn  <- mean( currentsegm$segmented, weight = currentsegm$end-segm$start )
      
      ## calc fraction amplified / gained bases
      idx.amp <- which( currentsegm$probamp + currentsegm$probgain > PROBCUTOFF )
      ampbases <- sum( currentsegm$end[idx.amp] - currentsegm$start[idx.amp] + 1 )
      fracamp <- ampbases / sum(currentsegm$end - currentsegm$start + 1)
      
      ## calc fraction deleted bases
      idx.del <- which( currentsegm$probdloss+currentsegm$probloss > PROBCUTOFF )
      delbases <- sum( currentsegm$end[idx.del] - currentsegm$start[idx.del] + 1 )
      fracdel <- delbases / sum(currentsegm$end - currentsegm$start + 1)
    }
    newline <- c(arm, meancn, fracamp, fracdel)
    tab <- rbindlist(list(tab, as.list(newline)))
    
  }
  
  tab  
}
