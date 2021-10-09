myLeveneTest <- function(x,g){
  gp.median <- tapply(x, g, median)  # calculate group means
  xbar <- gp.median[g] # group mean associated with each obs
  z <- abs(x - xbar) # the transformed variable, Z
  a<- aov(z ~ g) 
  return(a)
}


