
## print a dot every k loops
dot <- function(k, every=100){
  if(k %% every == 0){
    cat(".")
  }
}

