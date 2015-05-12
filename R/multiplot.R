#' Multiple plot function
#'
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' - cols:   Number of columns in layout
#' - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#'
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#'
#' @param ... plots to draw
#' @param plotlist list of plots to draw
#' @param cols number of columns
#' @param layout matrix with layout specifications
#' @param panelnames array of panel names (default LETTERS)
#' @param panelnames.size size of panel names (default 16)
#' @return NULL
#' @examples
#' # getCombinedMaf(reports)
#' library(ggplot2)
#' p1 <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()
#' p2 <- ggplot(mtcars, aes(x=mpg, fill=factor(cyl))) + geom_density(alpha=.7)
#' multiplot(p1, p2, cols = 2)
#' multiplot(p1, p2, cols = 2, panelnames = LETTERS)
#' multiplot(p1, p2, cols = 2, panelnames = LETTERS, panelnames.size = 24)
#' multiplot(p1, p2, p1, p2, layout=matrix(c(1,1,2,2,3,4), nc=3, byrow=FALSE), panelnames = LETTERS, panelnames.size = 24)
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL, 
                      panelnames=NULL, panelnames.size=16) {
  require(grid)
  require(gtable)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      p <- plots[[i]] 
      vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col)
      if(!is.null(panelnames)){
        ## Get the title style from the original plot
        p <- p + ggtitle(panelnames[i]) + 
          theme(plot.title=element_text(hjust=0, size=panelnames.size) )
      }
      print(p, vp = vp)
    }
  }
}
