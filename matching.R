################################################################################
# DATA PREP
load("/data/gerLFS2_skills.RData")

prof <- fread("/data/cedefop/ft_skill_profession_en.csv",header=FALSE)
setnames(prof,colnames(prof),c("GeneralId","PublicationCountry","Esco_Level_4","Esco_Level_3",
                               "Esco_Level_2","Esco_Level_1","Skill_Esco_Level_4","Skill_Esco_Level_3",
                               "Skill_Esco_Level_2","Skill_Esco_Level_1","Skill_Esco_Level_0"))
prof <- prof[PublicationCountry=="DEUTSCHLAND"]
prof <- unique(prof)
prof <- prof[Skill_Esco_Level_0=="Job-specific skills/competences"]


################################################################################
# helpfunctions
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


##################################################################################
# match person and job
germany <- copy(gerLFS2[,.(SKILLS,PersID)])
setkey(prof,GeneralId)
setkey(germany,SKILLS)

for(i in 1:length(index)){
  
  i <- 1:length(index)
  
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



