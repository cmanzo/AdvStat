myLeveneTest <- function(f,data){
x<-data[,all.vars(f)[1]]
  if(length(all.vars(f))==2){
    g<-data[,all.vars(f)[2]]
    gp.median <- tapply(x, g, median)  # calculate group means
    xbar <- gp.median[g] # group mean associated with each obs
    z <- abs(x - xbar) # the transformed variable, Z
    a<- aov(z ~ g)
  } else if (length(all.vars(f))==3) {
    g1<-data[,all.vars(f)[2]]
    g2<-data[,all.vars(f)[3]]
    g3<-interaction(g1, g2)
    gp.median <- tapply(x, g3, median)  # calculate group means
    xbar <- gp.median[g3]
    #xbar <- expand.grid(gp.median) # group mean associated with each obs
    z <- abs(x - xbar) # the transformed variable, Z
    a<- aov(z ~ g3)
  }
  print(summary(a))
  return(a)
}


#myLeveneTest <- function(x,g){
#  gp.median <- tapply(x, g, median)  # calculate group means
#  xbar <- gp.median[g] # group mean associated with each obs
#  z <- abs(x - xbar) # the transformed variable, Z
#  a<- aov(z ~ g)
#  return(a)
#}


