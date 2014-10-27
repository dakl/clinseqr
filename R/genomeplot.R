#' plot a copy number profile
#' 
#' @param xsegments Data frame with xsegments from qdnaseq. Must include column chr, start, end and segmented, and *prob.
#' @param xchrsizes Data frame with chromosome sizes. 
#' @return A combined MAF data table
#' @examples
#' # genomeplot(xsegments, xchrsizes)
genomeplot <- function(xsegments, xchrsizes){
  
  xchrsizes$cumend <- cumsum( as.numeric( xchrsizes$size) )
  xchrsizes$cumstart <- as.numeric(xchrsizes$cumend) - as.numeric(xchrsizes$size) 
  xchrsizes$labelpos <- cumsum( as.numeric(xchrsizes$size)) - xchrsizes$size/2

  xsegments$cumstart <- NA 
  xsegments$cumend   <- NA 
  for(chr in xchrsizes$chr){ ## chr = xchrsizes$chr[1]
    idx <- which( xsegments$chr == chr )
    cumchrsize <- xchrsizes$cumend[which(xchrsizes$chr == chr)] - xchrsizes$size[which(xchrsizes$chr == chr)]
    xsegments$cumstart[idx] <- xsegments$start[idx] + cumchrsize
    xsegments$cumend[idx] <- xsegments$end[idx] + cumchrsize
  }
  
  xsegments$state <- NA
  probcutoff <- 0.9
  xsegments$state[which(xsegments$probdloss+xsegments$probloss  > probcutoff)]  <- "DEL"
  xsegments$state[which(xsegments$probnorm  > probcutoff)]  <- "NORMAL"  
  xsegments$state[which(xsegments$probgain+xsegments$probamp  > probcutoff)]  <- "AMP"
    
  ggplot(xsegments, aes(xmin=cumstart, xmax=cumend, ymin=0, 
                       ymax=log2(segmented), colour=state, fill=state)) + 
    geom_rect() + theme_bw() + scale_fill_manual(values=c('#D55E00','#0072B2', "000000")) +
    scale_color_manual(values=c('#D55E00','#0072B2', "000000")) + 
    coord_cartesian(ylim = c(-1.5, 4)) + ylab("Log(T/N)") + xlab("Chromosome") + 
    geom_vline(xintercept=c(0,xchrsizes$cumend), linetype="dotted", colour="gray50") + 
    geom_hline(yintercept=log2(c(1/2, 3/2, 4/2, 5/2, 6/2)), linetype="dotted", 
               colour=c("#0072B2", "#D55E00", "#D55E00", "#D55E00", "#D55E00")) + 
    annotate("text", x=as.numeric(xchrsizes$labelpos), y=-1.3, label=xchrsizes$chr, size=4, colour="gray50") +
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(), panel.grid=element_blank()) + 
    ggtitle("Copy Number Profile") + 
    geom_segment(aes(x = 0, y = 0, xend = 2881033286, yend = 0), 
                 size=.2, colour="gray10")
  
  #annotate("text", x=max(xsegments$cumend)+1e7, y=log2(c(1/2, 3/2, 4/2, 5/2, 6/2)), 
  #           label=c("1", "3", "4", "5", "6"))
}

