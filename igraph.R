library(igraph);library(data.table);library(GGally)
load("/data/prof.RData")
load("/data/skilllev1.RDatat")
makeRel <- function(x){
  x <- unique(x)
  if(length(x)<2){
    return(NULL)
  }
  as.data.table(t(combn(x,2)))
}
rel <- list()
for(sk in skilllev1){
  rel[[sk]] <- prof[!is.null(Skill_Esco_Level_2)&Skill_Esco_Level_1==sk,makeRel(Skill_Esco_Level_2),by=GeneralId]
  rel[[sk]] <- rel[[sk]][V1!="NULL"&V2!="NULL",]
  rel[[sk]][,GeneralId:=NULL]
}
net <- list()
for(sk in skilllev1){
  if(nrow(rel[[sk]])>5)
    #net[[sk]] <- as.network(rel[[sk]])
    net[[sk]] <- as.network(rel[[sk]][sample(1:nrow(rel[[sk]]),min(2000,nrow(rel[[sk]])))])
}
save(net,file="/data/skilllnet.RDatat")

plot(ggnet2(net[["Computing"]],label=TRUE))
