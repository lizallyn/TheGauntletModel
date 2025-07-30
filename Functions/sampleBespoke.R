# because sample won't sample from list of length 1

sampleBespoke <- function(x, size, replace = FALSE){
  if(length(x) == 1){
    res <- rep(x, size)
  } else {
    res <- sample(x = x, size = size, replace = replace)
  }
  return(res)
}

# sampleBespoke(c(11, 1, 3), 2)

