# for the skewed dist that peaks at naive x value

receptivityX <- function(pars, baseline, x_t){
  m <- pars["height"]
  b <- pars["width"]
  c <- as.numeric(baseline)
  
  res <- (m*x_t^((c*(b-2)+1)/(1-c) - 1) * (1-x_t)^(b-1)) / 
    (c^((c*(b-2)+1)/(1-c)-1)*(1-c)^(b-1))
  
  return(as.numeric(res))
  
}

# seq <- seq(0,1,0.05)
# receptivityX(pars = tibble(height = sqrt(0.5), width = 4.5), 0, 0)
