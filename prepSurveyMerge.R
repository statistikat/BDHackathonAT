library(VIM);library(data.table);
load("/data/eures.Rdata")
gerLFS <- fread("/data/eu_lfs_puf/DE_LFS_2013_y.csv")
gerLFS <- gerLFS[!ILOSTAT%in%c(3,9)]
gerLFS[,ageMATCH:=AGE]
germany[,ageMATCH:=7]
germany[AGE%between%c(15,24),ageMATCH:=20]
germany[AGE%between%c(25,39),ageMATCH:=32]
germany[AGE%between%c(40,54),ageMATCH:=47]
germany[AGE%between%c(55,74),ageMATCH:=65]
germany[AGE>74,ageMATCH:=75]

germany$GENDER
germany <- germany[GENDER!="U"&!is.na(OCCUP_L0_CODE)&!is.na(SKILLS),]
germany[,SEX:=1]
germany[GENDER=="F",SEX:=2]
germany[,.N,by=OCCUP_L0_CODE]
table(gerLFS[,.(ISCO1D,ISCOPR1D)],useNA = "always")
gerLFS[,iscomatch:=floor(ISCO1D/100)]
gerLFS[ISCO1D==999,iscomatch:=NA]
gerLFS[ISCO1D==999,iscomatch:=NA]
gerLFS[ILOSTAT==2,iscomatch:=ISCOPR1D]
gerLFS[is.na(iscomatch),]
sampleREP <- function(x,prob,size){
  if(is.na(size)){
    return(NULL)
  }
  if(length(x)==1){
    return(rep(x,size))
  }else{
    sample(x,size=size,prob=prob,replace=TRUE)
  }
}
germany[,Nl0:=.N,by=.(OCCUP_L0_CODE,ageMATCH,SEX)]
sampL3 <- germany[,.(rel=.N/head(Nl0,1)),by=.(OCCUP_L0_CODE,ageMATCH,OCCUP_L3_CODE,SEX)]
sampL3 <- merge(sampL3,gerLFS[,.N,by=.(iscomatch,ageMATCH,SEX)],by.x=c("OCCUP_L0_CODE","ageMATCH","SEX"),by.y=c("iscomatch","ageMATCH","SEX"),all.x=TRUE)
sampL3 <- sampL3[,.(OCCUP_L3_CODE=sampleREP(OCCUP_L3_CODE,prob = rel,size=head(N,1))),by=.(OCCUP_L0_CODE,ageMATCH,SEX)]
gerLFS <- gerLFS[!is.na(ageMATCH)&!is.na(iscomatch)]
gerLFS[,OCCUP_L3_CODE:=999999999]
for(i in 0:9){
  for(s in 1:2){
    for(a in gerLFS[,unique(ageMATCH)]){
      if(gerLFS[ageMATCH==a&SEX==s&iscomatch==i,.N]>0){
        if(sampL3[ageMATCH==a&SEX==s&OCCUP_L0_CODE==i,.N]>0){
          gerLFS[ageMATCH==a&SEX==s&iscomatch==i,OCCUP_L3_CODE:=sampL3[ageMATCH==a&SEX==s&OCCUP_L0_CODE==i,OCCUP_L3_CODE]]
        }else{
          gerLFS[ageMATCH==a&SEX==s&iscomatch==i,OCCUP_L3_CODE:=NA]
        }
      }
    }
  }
}

sampL3 <- germany[,.(rel=.N/head(Nl0,1)),by=.(OCCUP_L0_CODE,OCCUP_L3_CODE,SEX)]
sampL3 <- merge(sampL3,gerLFS[is.na(OCCUP_L3_CODE),.N,by=.(iscomatch,SEX)],by.x=c("OCCUP_L0_CODE","SEX"),by.y=c("iscomatch","SEX"),all.x=TRUE)
sampL3 <- sampL3[,.(OCCUP_L3_CODE=sampleREP(OCCUP_L3_CODE,prob = rel,size=head(N,1))),by=.(OCCUP_L0_CODE,SEX)]
for(i in 0:9){
  for(s in 1:2){
      if(gerLFS[SEX==s&iscomatch==i&is.na(OCCUP_L3_CODE),.N]>0){
        if(sampL3[SEX==s&OCCUP_L0_CODE==i,.N]>0){
          gerLFS[SEX==s&iscomatch==i&is.na(OCCUP_L3_CODE),OCCUP_L3_CODE:=sampL3[SEX==s&OCCUP_L0_CODE==i,OCCUP_L3_CODE]]
        }
      }
  }
}
gerLFS[,summary(OCCUP_L3_CODE)]
yy <- merge(sampL3[,.(nlfs=.N),by=OCCUP_L3_CODE],germany[,.N,by=OCCUP_L3_CODE],by="OCCUP_L3_CODE")
xx <- merge(gerLFS[,.(nlfs=.N),by=OCCUP_L3_CODE],germany[,.N,by=OCCUP_L3_CODE],by="OCCUP_L3_CODE")
setkey(xx,N)
xx

# ageMATCH 0-14, 15-24, 25-39, 40-54, 55-74 and 75+
# SEX
# OCCUP_L3_CODE
gerLFS[,ID:="NOMATCH"]
gerLFS[,LFSID:=.I]
germany[,LFSID:="NOMATCH"]
tmp <- germany[!is.na(SKILLS)&!duplicated(ID)&!is.na(OCCUP_L3_CODE)&!is.na(ageMATCH)&!is.na(SEX),.(ID,LFSID,ageMATCH,SEX,OCCUP_L3_CODE)]
setindex(tmp,ageMATCH,SEX)
setindex(tmp,ageMATCH,SEX,OCCUP_L3_CODE)
gerLFS[,ID:=NA]
for(cod in gerLFS[!is.na(OCCUP_L3_CODE),unique(OCCUP_L3_CODE)]){
  for(a in gerLFS[,unique(ageMATCH)]){
    for(s in 1:2){
      tmp2 <- tmp[OCCUP_L3_CODE==cod&ageMATCH==a&SEX==s]
      if(tmp2[,.N]>0){
        gerLFS[OCCUP_L3_CODE==cod&ageMATCH==a&SEX==s,ID:=tmp2[sample(1:nrow(tmp2),size=gerLFS[OCCUP_L3_CODE==cod&ageMATCH==a&SEX==s,.N],replace=TRUE),.(ID)]]
      }
    }
  }
}
for(cod in gerLFS[!is.na(OCCUP_L3_CODE),unique(OCCUP_L3_CODE)]){
  for(a in gerLFS[,unique(ageMATCH)]){
      tmp2 <- tmp[OCCUP_L3_CODE==cod&ageMATCH==a]
      if(tmp2[,.N]>0){
        gerLFS[is.na(ID)&OCCUP_L3_CODE==cod&ageMATCH==a,ID:=tmp2[sample(1:nrow(tmp2),size=gerLFS[is.na(ID)&OCCUP_L3_CODE==cod&ageMATCH==a,.N],replace=TRUE),.(ID)]]
      }
  }
}
for(cod in gerLFS[!is.na(OCCUP_L3_CODE),unique(OCCUP_L3_CODE)]){
  for(s in 1:2){
    tmp2 <- tmp[OCCUP_L3_CODE==cod&SEX==s]
    if(tmp2[,.N]>0){
      gerLFS[is.na(ID)&OCCUP_L3_CODE==cod&SEX==s,ID:=tmp2[sample(1:nrow(tmp2),size=gerLFS[is.na(ID)&OCCUP_L3_CODE==cod&SEX==s,.N],replace=TRUE),.(ID)]]
    }
  }
}
gerLFS2 <- gerLFS[ID!="NOMATCH"]
germany2 <- germany[!is.na(SKILLS)]
save(germany2,gerLFS2,file="/data/persons.RData",compress = TRUE)
