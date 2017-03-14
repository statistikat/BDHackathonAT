library(data.table)
library(parallel)
d <- data.table(x=rnorm(1e6),y=rnorm(1e6),group=1:10)
s <- split(d,as.factor(d$group))
?foreach
parf <- function(x){
  x[,.(mx=mean(x),my=mean(y))]
}
sout <- mclapply(s,parf,mc.cores = 3)