#' plot a copy number profile
#' 
#' @param segments Data frame with segments from qdnaseq. Must include column chr, start, end and segmented, and *prob.
#' @param chrsizes Data frame with chromosome sizes. 
#' @return A combined MAF data table
#' @examples
#' # genomeplot(segments, chrsizes)
genomeplot <- function(segments, chrsizes){

  chrsizes$cumend <- cumsum( as.numeric( chrsizes$size) )
  chrsizes$cumstart <- as.numeric(chrsizes$cumend) - as.numeric(chrsizes$size) 
  chrsizes$labelpos <- cumsum( as.numeric(chrsizes$size)) - chrsizes$size/2
  
  segments$cumstart <- NA # cumsum( segm[[SID]]$end - segm[[SID]]$start + 1 ) - ( segm[[SID]]$end - segm[[SID]]$start ) ## cumulative start pos
  segments$cumend   <- NA # cumsum( segm[[SID]]$end - segm[[SID]]$start + 1 ) ## cumulative end pos
  for(chr in chrsizes$chr){ ## chr = chrsizes$chr[1]
    idx <- which( segments$chr == chr )
    cumchrsize <- chrsizes$cumend[which(chrsizes$chr == chr)] - chrsizes$size[which(chrsizes$chr == chr)]
    segments$cumstart[idx] <- segments$start[idx] + cumchrsize
    segments$cumend[idx] <- segments$end[idx] + cumchrsize
  }
  
  segments$state <- NA
  probcutoff <- 0.8
  #segments$state[which(segments$probdloss > probcutoff)]  <- "HOMDEL"
  #segments$state[which(segments$probloss  > probcutoff)]  <- "HETDEL"
  segments$state[which(segments$probdloss+segments$probloss  > probcutoff)]  <- "DEL"
  segments$state[which(segments$probnorm  > probcutoff)]  <- "NORMAL"  
  segments$state[which(segments$probgain+segments$probamp  > probcutoff)]  <- "AMP"
  #segments$state[which(segments$probgain  > probcutoff)]  <- "GAIN"
  #segments$state[which(segments$probamp   > probcutoff)]  <- "AMP"

  ggplot(segments, aes(xmin=cumstart, xmax=cumend, ymin=0, 
                         ymax=log2(segmented), colour=state, fill=state)) + 
    geom_rect() + theme_bw() + scale_fill_manual(values=c('#D55E00','#0072B2', "000000")) +
    scale_color_manual(values=c('#D55E00','#0072B2', "000000")) + 
    coord_cartesian(ylim = c(-1.5, 4)) + ylab("Log(T/N)") + xlab("Chromosome") + 
    geom_vline(xintercept=chrsizes$cumend, linetype="dotted", colour="gray50") + 
    geom_hline(yintercept=log2(c(1/2, 3/2, 4/2, 5/2, 6/2)), linetype="dotted", 
               colour=c("#0072B2", "#D55E00", "#D55E00", "#D55E00", "#D55E00")) + 
    annotate("text", x=as.numeric(chrsizes$labelpos), y=-1.3, label=chrsizes$chr, size=4, colour="gray50") +
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(), panel.grid=element_blank()) + 
    ggtitle("Copy Number Profile") + 
    geom_segment(aes(x = 0, y = 0, xend = 2881033286, yend = 0), 
                 size=.2, colour="gray10")
  #annotate("text", x=max(segments$cumend)+1e7, y=log2(c(1/2, 3/2, 4/2, 5/2, 6/2)), 
  #           label=c("1", "3", "4", "5", "6"))
}

