#' Print a dot every k loops
#' 
#' @param k looping paramters
#' @param every print a symbol when k mod every == 0, default 100
#' @param symbol String to print, default \code{.}
#' @examples
#' for( k in 1:1000 ){ 
#'   dot(k)
#'   dot(k, every=150, symbol="_")
#' }
dot <- function(k, every=100, symbol="."){
  if(k %% every == 0){
    cat(symbol)
  }
}

