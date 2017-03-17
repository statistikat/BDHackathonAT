#########################################################################
# FUNCTION TO LINK pers_skill-DATA TO LABOURFORCE-DATA
#
#

merge_pers_skill_lfs <- function(pers_skill,lfs,match_var=c("AGE","ISCO1","GENDER")){
  
  #library(VIM);library(data.table);
  load("/data/eures.Rdata")
  lfs <- fread("/data/eu_lfs_puf/DE_LFS_2013_y.csv")
  lfs <- lfs[!ILOSTAT%in%c(3,9)]
  lfs[,ageMATCH:=AGE]
  pers_skill[,ageMATCH:=7]
  pers_skill[AGE%between%c(15,24),ageMATCH:=20]
  pers_skill[AGE%between%c(25,39),ageMATCH:=32]
  pers_skill[AGE%between%c(40,54),ageMATCH:=47]
  pers_skill[AGE%between%c(55,74),ageMATCH:=65]
  pers_skill[AGE>74,ageMATCH:=75]
  
  pers_skill$GENDER
  pers_skill <- pers_skill[GENDER!="U"&!is.na(OCCUP_L0_CODE)&!is.na(pers_skillS),]
  pers_skill[,SEX:=1]
  pers_skill[GENDER=="F",SEX:=2]
  pers_skill[,.N,by=OCCUP_L0_CODE]
  table(lfs[,.(ISCO1D,ISCOPR1D)],useNA = "always")
  lfs[,iscomatch:=floor(ISCO1D/100)]
  lfs[ISCO1D==999,iscomatch:=NA]
  lfs[ISCO1D==999,iscomatch:=NA]
  lfs[ILOSTAT==2,iscomatch:=ISCOPR1D]
  lfs[is.na(iscomatch),]
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
  pers_skill[,Nl0:=.N,by=.(OCCUP_L0_CODE,ageMATCH,SEX)]
  sampL3 <- pers_skill[,.(rel=.N/head(Nl0,1)),by=.(OCCUP_L0_CODE,ageMATCH,OCCUP_L3_CODE,SEX)]
  sampL3 <- merge(sampL3,lfs[,.N,by=.(iscomatch,ageMATCH,SEX)],by.x=c("OCCUP_L0_CODE","ageMATCH","SEX"),by.y=c("iscomatch","ageMATCH","SEX"),all.x=TRUE)
  sampL3 <- sampL3[,.(OCCUP_L3_CODE=sampleREP(OCCUP_L3_CODE,prob = rel,size=head(N,1))),by=.(OCCUP_L0_CODE,ageMATCH,SEX)]
  lfs <- lfs[!is.na(ageMATCH)&!is.na(iscomatch)]
  lfs[,OCCUP_L3_CODE:=999999999]
  for(i in 0:9){
    for(s in 1:2){
      for(a in lfs[,unique(ageMATCH)]){
        if(lfs[ageMATCH==a&SEX==s&iscomatch==i,.N]>0){
          if(sampL3[ageMATCH==a&SEX==s&OCCUP_L0_CODE==i,.N]>0){
            lfs[ageMATCH==a&SEX==s&iscomatch==i,OCCUP_L3_CODE:=sampL3[ageMATCH==a&SEX==s&OCCUP_L0_CODE==i,OCCUP_L3_CODE]]
          }else{
            lfs[ageMATCH==a&SEX==s&iscomatch==i,OCCUP_L3_CODE:=NA]
          }
        }
      }
    }
  }
  
  sampL3 <- pers_skill[,.(rel=.N/head(Nl0,1)),by=.(OCCUP_L0_CODE,OCCUP_L3_CODE,SEX)]
  sampL3 <- merge(sampL3,lfs[is.na(OCCUP_L3_CODE),.N,by=.(iscomatch,SEX)],by.x=c("OCCUP_L0_CODE","SEX"),by.y=c("iscomatch","SEX"),all.x=TRUE)
  sampL3 <- sampL3[,.(OCCUP_L3_CODE=sampleREP(OCCUP_L3_CODE,prob = rel,size=head(N,1))),by=.(OCCUP_L0_CODE,SEX)]
  for(i in 0:9){
    for(s in 1:2){
      if(lfs[SEX==s&iscomatch==i&is.na(OCCUP_L3_CODE),.N]>0){
        if(sampL3[SEX==s&OCCUP_L0_CODE==i,.N]>0){
          lfs[SEX==s&iscomatch==i&is.na(OCCUP_L3_CODE),OCCUP_L3_CODE:=sampL3[SEX==s&OCCUP_L0_CODE==i,OCCUP_L3_CODE]]
        }
      }
    }
  }
  lfs[,summary(OCCUP_L3_CODE)]
  yy <- merge(sampL3[,.(nlfs=.N),by=OCCUP_L3_CODE],pers_skill[,.N,by=OCCUP_L3_CODE],by="OCCUP_L3_CODE")
  xx <- merge(lfs[,.(nlfs=.N),by=OCCUP_L3_CODE],pers_skill[,.N,by=OCCUP_L3_CODE],by="OCCUP_L3_CODE")
  setkey(xx,N)
  xx
  
  # ageMATCH 0-14, 15-24, 25-39, 40-54, 55-74 and 75+
  # SEX
  # OCCUP_L3_CODE
  lfs[,ID:="NOMATCH"]
  lfs[,LFSID:=.I]
  pers_skill[,LFSID:="NOMATCH"]
  tmp <- pers_skill[!is.na(pers_skillS)&!duplicated(ID)&!is.na(OCCUP_L3_CODE)&!is.na(ageMATCH)&!is.na(SEX),.(ID,LFSID,ageMATCH,SEX,OCCUP_L3_CODE)]
  setindex(tmp,ageMATCH,SEX)
  setindex(tmp,ageMATCH,SEX,OCCUP_L3_CODE)
  lfs[,ID:=NA]
  for(cod in lfs[!is.na(OCCUP_L3_CODE),unique(OCCUP_L3_CODE)]){
    for(a in lfs[,unique(ageMATCH)]){
      for(s in 1:2){
        tmp2 <- tmp[OCCUP_L3_CODE==cod&ageMATCH==a&SEX==s]
        if(tmp2[,.N]>0){
          lfs[OCCUP_L3_CODE==cod&ageMATCH==a&SEX==s,ID:=tmp2[sample(1:nrow(tmp2),size=lfs[OCCUP_L3_CODE==cod&ageMATCH==a&SEX==s,.N],replace=TRUE),.(ID)]]
        }
      }
    }
  }
  for(cod in lfs[!is.na(OCCUP_L3_CODE),unique(OCCUP_L3_CODE)]){
    for(a in lfs[,unique(ageMATCH)]){
      tmp2 <- tmp[OCCUP_L3_CODE==cod&ageMATCH==a]
      if(tmp2[,.N]>0){
        lfs[is.na(ID)&OCCUP_L3_CODE==cod&ageMATCH==a,ID:=tmp2[sample(1:nrow(tmp2),size=lfs[is.na(ID)&OCCUP_L3_CODE==cod&ageMATCH==a,.N],replace=TRUE),.(ID)]]
      }
    }
  }
  for(cod in lfs[!is.na(OCCUP_L3_CODE),unique(OCCUP_L3_CODE)]){
    for(s in 1:2){
      tmp2 <- tmp[OCCUP_L3_CODE==cod&SEX==s]
      if(tmp2[,.N]>0){
        lfs[is.na(ID)&OCCUP_L3_CODE==cod&SEX==s,ID:=tmp2[sample(1:nrow(tmp2),size=lfs[is.na(ID)&OCCUP_L3_CODE==cod&SEX==s,.N],replace=TRUE),.(ID)]]
      }
    }
  }
  lfs2 <- lfs[ID!="NOMATCH"]
  pers_skill2 <- pers_skill[!is.na(pers_skillS)]
  save(pers_skill2,lfs2,file="/data/persons.RData",compress = TRUE)
  
  
  lfs2.na <- lfs2[,lapply(.SD,function(z){!all(is.na(z))})]
  lfs2 <- lfs2[,colnames(lfs2)[unlist(lfs2.na)],with=FALSE]
  
  lfs2 <- lfs2[ILOSTAT==2,.SD[rep(1:.N,unique(round(COEFF*1000)))],by="LFSID"]
  lfs2[,PersID:=1:nrow(lfs2)]
  
  lfs2 <- merge(lfs2,unique(pers_skill2[,.(ID,SKILLS)]),by="ID",allow.cartesian=TRUE)
  save(lfs2,file="/data/lfs2_skills.RData",compress=TRUE)
  
}