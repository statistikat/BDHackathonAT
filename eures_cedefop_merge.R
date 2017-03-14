library(data.table)
library(ggplot2)
library(VIM)


source("eures.R")
source("cedefop.R")

load("/data/persons.RData")
rm(germany2)
#gerLFS2[ID%in%unique(ID)[1],round(1/COEFF*10)]
gerLFS2.na <- gerLFS2[,lapply(.SD,function(z){!all(is.na(z))})]
gerLFS2 <- gerLFS2[,colnames(gerLFS2)[unlist(gerLFS2.na)],with=FALSE]

gerLFS2 <- gerLFS2[ILOSTAT==2,.SD[rep(1:.N,unique(round(COEFF*1000)))],by="LFSID"]
gerLFS2[,PersID:=1:nrow(gerLFS2)]

gerLFS2 <- merge(gerLFS2,germany[,.(ID,SKILLS)],by="ID",allow.cartesian=TRUE)
save(gerLFS2,file="/data/gerLFS2_skills.RData",compress=TRUE)


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

index <- unique(prof$GeneralId)
prof.test[,.N,by="GeneralId"][N>3]

colnames(germany)
germany <- copy(gerLFS2[,.(SKILLS,PersID)])
setkey(prof,GeneralId)
setkey(germany,SKILLS)

for(i in 1:length(index)){
  
  i <- 1:10000
  
  # required skills for job index[i]
  skills.i <- prof[.(index[i]),.(GeneralId,SKILL=Skill_Esco_Level_4)]
  j <- 1
  if(class(skills.i)[1]=="character"){
    
    while(any(skills.i=="NULL")){
      null_index <- skills.i=="NULL"
      skills.i[null_index] <- unlist(prof[.(index[i]),.(get(paste("Skill_Esco_Level_",4-j,sep="")))])[null_index]
      j <- j+1
    }
    
  }else{
    while(any(skills.i$SKILL=="NULL")){
      null_index <- skills.i$SKILL=="NULL"
      next_ESCO <- prof[.(index[i]),.(GeneralId,SKILL=get(paste("Skill_Esco_Level_",4-j,sep="")))]
      skills.i[SKILL=="NULL",SKILL:=next_ESCO$SKILL[null_index]]
      j <- j+1
    }
  }

  germany <- germany[SKILLS%in%skills.i$SKILL]
  #germany[skills.i,.N,by=EACHI]
  #skills.i[,germany[SKILL],by=GeneralId]
  #germany.skills.i <- merge(skills.i,germany[SKILLS%in%skills.i$SKILL,.(SKILLS,ID)],by.x="SKILL",by.y="SKILLS",all.x=TRUE)
  job_person_match <- skills.i[,sample_help(germany[SKILL],bound=ceiling(.N*.3)),by=GeneralId]
  
  job_person_out <- job_person_match[is.na(PersID),.(GeneralId,PersID)]
  
  job_person_match <- job_person_match[!is.na(PersID),.(GeneralId=GeneralId[sample_help2(which.max(prob))]),by=PersID]
  
  job_person_out <- rbind(job_person_out,job_person_match)
  
  skills.i <- skills.i[!GeneralId%in%job_person_out$GeneralId]
  
  donor_help <- germany[!PersID%in%job_person_out$PersID]
  setkey(donor_help,SKILLS)
  
  while(nrow(skills.i)>0){
    
    job_person_match <- skills.i[,sample_help(donor_help[SKILL],bound=ceiling(.N*.3)),by=GeneralId]
    
    job_person_out <- rbind(job_person_out,job_person_match[is.na(PersID),.(GeneralId,PersID)])
    
    job_person_match <- job_person_match[!is.na(PersID),.(GeneralId=GeneralId[sample_help2(which.max(prob))]),by=PersID]
    
    job_person_out <- rbind(job_person_match,job_person_out)

    skills.i <- skills.i[!GeneralId%in%job_person_out$GeneralId]
    
    donor_help <- donor_help[!PersID%in%job_person_out$PersID]
    setkey(donor_help,SKILLS)
    
  }
  
  # number of jobs with multiple selections
  setnames(job_person_out,c("GeneralId"),c("JobID"))

}

sample_help <- function(dat,bound){
  
  if(any(!is.na(dat$PersID))){
    potential_employee <- dat[,.N,by=PersID][N>=bound]
    #potential_employee <- dat[,.N,by=ID]
    if(nrow(potential_employee)>0){
      probs <- potential_employee$N/sum(potential_employee$N)
      sampindex <- sample(nrow(potential_employee),1,prob=probs)
      return(list(prob=probs[sampindex],PersID=potential_employee$PersID[sampindex]))
    }else{
      return(list(prob=NA_real_,PersID=NA_integer_))
    }
  }else{
    return(list(prob=NA_real_,PersID=NA_integer_))
  }
}

sample_help2 <- function(x){
  if(length(x)>1){
    return(sample(x,1))
  }else{
    return(x)
  }
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
