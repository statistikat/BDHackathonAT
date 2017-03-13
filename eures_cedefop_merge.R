library(data.table)
library(ggplot2)
library(VIM)


source("eures.R")
source("cedefop.R")



colnames(germany)

colnames(job)

colnames(doc)
colnames(prof)
colnames(prof_new)
colnames(expi)
colnames(requ)


# merge job with cedefop daten
colnames(job) # eures
colnames(prof) # cedefop

prof[,unique(Esco_Level_1)]
job[,unique(ISCO0_NAME)]

colnames(prof)
colnames(job)

job <- job[!is.na(ISCO0_CO)]
# required skills by cedefop (automatically identified)
tab_prof <- prof[Esco_Level_1=="Craft and related trades workers"&Skill_Esco_Level_4!='NULL',table(Skill_Esco_Level_2)]
tail(sort(tab_prof),10)

isco0names <- unique(job[,.(ISCO0_CO,ISCO0_NAME)])

prof <- merge(prof,unique(job[,.(ISCO0_CO,ISCO0_NAME)]),by.x="Esco_Level_1",by.y="ISCO0_NAME")
prof <- merge(prof,unique(job[,.(ISCO1_CO,ISCO1_NAME)]),by.x="Esco_Level_2",by.y="ISCO1_NAME")
prof <- merge(prof,unique(job[,.(ISCO2_CO,ISCO2_NAME)]),by.x="Esco_Level_3",by.y="ISCO2_NAME")
prof <- merge(prof,unique(job[,.(ISCO3_CO,ISCO3_NAME)]),by.x="Esco_Level_4",by.y="ISCO3_NAME")



get_skills <- function(prof,ISCO_LEVEL=2,sublevel=3,N=20){
  
  tab_prof <- prof[ISCO0_CO==ISCO_LEVEL&Skill_Esco_Level_4!='NULL',table(get(paste("Skill_Esco_Level_",sublevel,sep="")))]
  
  return(tail(sort(tab_prof),N))
}

get_skills(prof,ISCO_LEVEL=4)

# 
prof <- prof[PublicationCountry=="DEUTSCHLAND"]
prof <- unique(prof)
prof[Skill_Esco_Level_4=="NULL"&Skill_Esco_Level_0=="Job-specific skills/competences"] 

prof <- prof[Skill_Esco_Level_0=="Job-specific skills/competences"]
prof[Skill_Esco_Level_4=='NULL']

prof.test <- prof[Esco_Level_1=="Professionals"]

index <- unique(prof.test$GeneralId)
prof.test[,.N,by="GeneralId"][N>3]
setkey(prof.test,GeneralId)

for(i in 1:length(index)){
  
  # required skills for job index[i]
  skills.i <- prof.test[.(index[i]),Skill_Esco_Level_4]
  j <- 1
  if(class(skills.i)=="character"){
    
    while(any(skills.i=="NULL")){
      null_index <- skills.i=="NULL"
      skills.i[null_index] <- unlist(prof.test[.(index[i]),.(get(paste("Skill_Esco_Level_",4-j,sep="")))])[null_index]
      j <- j+1
    }
    
  }else{
    while(any(skills.i$V1=="NULL")){
      null_index <- skills.i$V1=="NULL"
      next_ESCO <- prof.test[.(index[i]),.(get(paste("Skill_Esco_Level_",4-j,sep="")))]
      skills.i[V1=="NULL",V1:=next_ESCO$V1[null_index]]
      j <- j+1
    }
  }
  
  
  gets_job <- germany[SKILLS%in%skills.i,.N,by=ID][N>=ceiling(length(skills.i)*.5)][,sample(ID,1,prob=N)]
  
  germany[ID==gets_job,HIRED:=TRUE]
  
}


match_skills <- function(prof,germany,ESCO_LEVEL=3){
  
  # match skills from prof to skills from germany
  # get Skills up from LEVEL = ESCO_LEVEL if NULL get to higher level for this case  
  
  out <- prof[,.(get(paste("Skill_Esco_Level_",ESCO_LEVEL,sep="")))]
  i <- 1
  while(any(out$V1=="NULL")|i<ESCO_LEVEL){
    
    null_index <- out$V1=="NULL"
    next_ESCO <- prof[,.(get(paste("Skill_Esco_Level_",ESCO_LEVEL-i,sep="")))]
    out[V1=="NULL",V1:=next_ESCO$V1[null_index]]
    i <- i+1
  }
  out <- unlist(unique(out))
  unlist(unique(out))[!unlist(unique(out))%in%unique(germany$SKILLS)]
  
}


unique(prof$Skill_Esco_Level_4[prof$Skill_Esco_Level_4%in%germany$SKILLS])

unique(germany$SKILLS[!germany$SKILLS%in%prof$Esco_Level_4])
