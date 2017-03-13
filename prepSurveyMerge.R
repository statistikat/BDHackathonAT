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
germany <- germany[GENDER!="U"&!is.na(OCCUP_L0_CODE),]
germany[,SEX:=1]
germany[GENDER=="F",SEX:=2]
germany[,.N,by=OCCUP_L0_CODE]
table(gerLFS[,.(ISCO1D,ISCOPR1D)],useNA = "always")
gerLFS[,iscomatch:=floor(ISCO1D/100)]
gerLFS[ISCO1D==999,iscomatch:=NA]
gerLFS[ISCO1D==999,iscomatch:=NA]
gerLFS[ILOSTAT==2,iscomatch:=ISCOPR1D]
gerLFS[is.na(iscomatch),]

germany[,Nl0:=.N,by=OCCUP_L0_CODE]
sampL3 <- germany[,.(rel=.N/head(Nl0,1)),by=.(OCCUP_L0_CODE,OCCUP_L3_CODE)]
sampL3 <- merge(sampL3,gerLFS[,.N,by=iscomatch],by.x="OCCUP_L0_CODE",by.y="iscomatch")
sampL3 <- sampL3[!is.na(OCCUP_L0_CODE)&!is.na(OCCUP_L3_CODE),.(OCCUP_L3_CODE=sample(OCCUP_L3_CODE,prob = rel,size=head(N,1),replace=TRUE)),by=OCCUP_L0_CODE]
gerLFS[,OCCUP_L3_CODE:=999999999]
for(i in 0:9){
  gerLFS[iscomatch==i,OCCUP_L3_CODE:=sampL3[OCCUP_L0_CODE==i,OCCUP_L3_CODE]]
}

yy <- merge(sampL3[,.(nlfs=.N),by=OCCUP_L3_CODE],germany[,.N,by=OCCUP_L3_CODE],by="OCCUP_L3_CODE")
xx <- merge(gerLFS[,.(nlfs=.N),by=OCCUP_L3_CODE],germany[,.N,by=OCCUP_L3_CODE],by="OCCUP_L3_CODE")
setkey(xx,N)
xx
gerLFS[OCCUP_L3_CODE==999999999,OCCUP_L3_CODE:=NA]
gerLFS

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
  tmp2 <- tmp[OCCUP_L3_CODE==cod]
  gerLFS[OCCUP_L3_CODE==cod,ID:=tmp2[sample(1:nrow(tmp2),size=gerLFS[OCCUP_L3_CODE==cod,.N],replace=TRUE),.(ID)]]
}

gerLFS2 <- gerLFS[ID!="NOMATCH"]

save(germany2,gerLFS2,file="/data/persons.RData",compress = TRUE)
