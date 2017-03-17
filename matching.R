library(data.table)
library(parallel)
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

doc <- fread("/data/cedefop/ft_document_en.csv",header=FALSE)
setnames(doc,colnames(doc),c("GeneralId","PublicationCountry","GrabDate","YearGrabDate","MonthGrabDate",
                             "DayGrabDate","ExpireDate","YearExpireDate","MonthExpireDate","DayExpireDate",
                             "Esco_Level_4","Esco_Level_3","Esco_Level_2","Esco_Level_1","Nut_Level_3",
                             "Nut_Level_2","Nut_Level_1","Nut_Level_0","Contract","EducationalLevel","Industry_Level_2",
                             "Industry_Level_1","WorkingHours"))


doc[,QUARTER:=if(MonthGrabDate[1]<=3){'Q1'}else if(MonthGrabDate[1]<=6){'Q2'}else if(MonthGrabDate[1]<=9){'Q3'}else{'Q4'},by=MonthGrabDate]
job_quarter <- unique(doc[,.(GeneralId,QUARTER)])
save(job_quarter,file="/data/job_quarter.RData",compress=TRUE)

load("/data/jvs.RData")

uindex <- c(6,5,10,3,1,9,0,2,10,9,11,7,10,9,4,10,8,10,2,0,2)
ulevels <- doc[,unique(Industry_Level_1)]

doc[,NACE:=uindex[ulevels%in%Industry_Level_1],by=Industry_Level_1]

doc <- unique(doc,by="GeneralId")
doc <- doc[PublicationCountry=="DEUTSCHLAND"]
doc <- doc[GeneralId%in%prof$GeneralId]


quart <- c("Q1","Q2","Q3","Q4")

out <- NULL
for(i in 0:11){
  if(i==0){
    out_iq <- doc[NACE==0]
    out_iq[,WEIGHT:=1]
    out <- rbind(out,out_iq)
  }else{
    for(q in 1:4){
      out_iq <- doc[NACE==i&QUARTER==quart[q]]
      if(nrow(out_iq)>0){
        out_iq[,WEIGHT:=unlist(jvs[i][,c(2+q),with=FALSE])*1000/.N]
        if(out_iq$WEIGHT[1]<1){
          out_iq[,WEIGHT:=1]
        }
        out <- rbind(out,out_iq)
      }
    }
  }
}

job_weights <- out[,.(GeneralId,WEIGHT)]
save(job_weights,file="/data/job_weights.RData",compress=TRUE)

load("/data/job_weights.RData")
prof <- prof[GeneralId%in%job_weights$GeneralId]
prof <- merge(prof,job_weights,by="GeneralId",all.x=TRUE)

random_round <- function(x){
  
  if(sample(c(0,1),1)==1){
    xr <- ceiling(unique(x$WEIGHT))
  }else{
    xr <- floor(unique(x$WEIGHT))
  }
  n <- nrow(x)
  x <- x[rep(1:n,xr)]
  x[,ID_new:=sort(rep(1:xr,n))]
  
  return(x)
}

prof <- prof[,.(GeneralId,Skill_Esco_Level_1,Skill_Esco_Level_2,Skill_Esco_Level_3,Skill_Esco_Level_4,WEIGHT)]

prof <- prof[,random_round(.SD),by="GeneralId"]


save(prof,file="/data/prof_with_weights.RData",compress=TRUE)


################################################################################
# helpfunctions
sample_help <- function(dat,bound,jobid){
  
  if(any(!is.na(dat$PersID))){
    potential_employee <- dat[,.N,by=PersID][N>=bound]
    #potential_employee <- dat[,.N,by=ID]
    if(nrow(potential_employee)>0){
      probs <- potential_employee$N/sum(potential_employee$N)
      
      njob <- length(jobid)
      nemp <- nrow(potential_employee)
      sampn <- min(njob,nemp)
      
      sampindex <- sample(nemp,sampn,prob=probs)
      
      probs <- c(probs[sampindex],rep(NA,njob-sampn))
      PersID <- c(potential_employee$PersID[sampindex],rep(NA,njob-sampn))
      
      return(list(prob=probs,PersID=PersID,GeneralId=jobid))
    }else{
      return(list(prob=rep(NA_real_,length(jobid)),PersID=rep(NA_integer_,length(jobid)),GeneralId=jobid))
    }
  }else{
    return(list(prob=rep(NA_real_,length(jobid)),PersID=rep(NA_integer_,length(jobid)),GeneralId=jobid))
  }
}

sample_help2 <- function(GID,ISK,prob){
  if(length(prob)>1){
    take <- sample(length(prob),1,prob=prob)
    return(list(GeneralId=GID[take],ISK=ISK[take]))
  }else{
    return(list(GeneralId=GID,ISK=ISK))
  }
}

split_data <- function(dat,N=10){
  
  setkey(dat,ISK)
  uniqueid <- unique(dat$ISK)
  nparts <- ceiling(length(uniqueid)/N)
  
  finalparts <- c()
  for(i in 1:N){
    id_i <- uniqueid[((i-1)*nparts+1):min(nrow(dat),(i*nparts))]
    finalparts <- c(finalparts,rep(i,nrow(dat[ISK%in%id_i])))
  }
  
  dat[,parts:=finalparts]
  
  dat_out <- split(dat,as.factor(dat$parts))
  dat[,parts:=NULL]
  return(dat_out)
}

wrap_sample_help1 <- function(dat){
  return(dat[,sample_help(germany[unique(SKILL)],bound=ceiling(length(unique(SKILL))*.3),jobid=unique(GeneralId)),by=ISK])
}

wrap_sample_help2 <- function(dat){
  return(dat[,sample_help(donor_help[unique(SKILL)],bound=ceiling(length(unique(SKILL))*.3),jobid=unique(GeneralId)),by=ISK])
}

##################################################################################
# match person and job
germany <- gerLFS2[,.(SKILLS,PersID)]
save(germany,file="/data/germany_test.RData",compress=TRUE)
#load("/data/germany_test.RData")


save(prof,file="/data/prof_test.RData",compress=TRUE)

load("/data/germany_test.RData")
#load("/data/prof_test.RData")
load("/data/prof_with_weights.RData")
prof[,GeneralId:=paste(GeneralId,ID_new,sep="_")]
prof[,ID_new:=NULL]
setkey(prof,GeneralId)
setkey(germany,SKILLS)
index <- unique(prof$GeneralId)

i <- 1:length(index)

# required skills for job index[i]
skills.i <- prof[.(index[i]),.(GeneralId,SKILL=Skill_Esco_Level_4)]
#skills.i <- prof[,.(GeneralId,SKILL=Skill_Esco_Level_4)]

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

# new ID
#skills.i[,GeneralId2:=GeneralId]
# skills.i[,GeneralId_new:=sort(rep(1:(.N/length(unique(SKILL))),length(unique(SKILL)))),by=GeneralId]
# 
# help_index <- function(SK,N){
#   
#   freq <- table(SK)
#   
#   if(length(unique(freq))==1){
#     return(1:N)
#   }
#   if(){
#     
#   }else{
#     
#     n <- length(SK)
#     
#     m <- n%%2
#     return()
#   }
#   
#   
#   return(sort(rep(1:(.N/length(freq)),length(freq))))
# }
# 
# }
# 
# skills.i[,GeneralId:=paste(GeneralId,GeneralId_new,sep="_")]
# skills.i[,GeneralId_new:=NULL]
skills.i <- unique(skills.i)

skills.i[,help:=paste(sort(SKILL),collapse="."),by="GeneralId"]
skills.i[,ISK:=.GRP,by=help]
skills.i[,help:=NULL]

germany2 <- copy(germany)
germany <- germany[SKILLS%in%skills.i$SKILL]
rm(next_ESCO,null_index,germany2,prof,dat,donor_help,dat_out,i,index,skill.parts)
#germany[skills.i,.N,by=EACHI]
#skills.i[,germany[SKILL],by=GeneralId]
#germany.skills.i <- merge(skills.i,germany[SKILLS%in%skills.i$SKILL,.(SKILLS,ID)],by.x="SKILL",by.y="SKILLS",all.x=TRUE)
skill.parts <- split_data(skills.i,10)

job_person_match <- mclapply(skill.parts,wrap_sample_help1,mc.cores = 10)

#job_person_match <- skills.i[,sample_help(germany[unique(SKILL)],bound=ceiling(length(unique(SKILL))*.3),jobid=unique(GeneralId)),by=ISK]

job_person_match <- rbindlist(job_person_match)

#job_person_match <- skills.i[,sample_help(germany[SKILL],bound=ceiling(.N*.3)),by=GeneralId]

job_person_out <- job_person_match[is.na(PersID),.(GeneralId,PersID,ISK)]

job_person_match <- job_person_match[!is.na(PersID),sample_help2(GeneralId,ISK,prob),by=PersID]

job_person_out <- rbind(job_person_out,job_person_match)

skills.i <- skills.i[!GeneralId%in%job_person_out$GeneralId]

donor_help <- germany[!PersID%in%job_person_out$PersID]
setkey(donor_help,SKILLS)

while(nrow(skills.i)>0){
  
  if(length(unique(skills.i$SKI))>50){
    skill.parts <- split_data(skills.i,10)
    job_person_match <- mclapply(skill.parts,wrap_sample_help2,mc.cores = 8)
    job_person_match <- rbindlist(job_person_match)
  }else{
    job_person_match <- skills.i[,sample_help(donor_help[unique(SKILL)],bound=ceiling(length(unique(SKILL))*.3),jobid=unique(GeneralId)),by=ISK]
  }
  
  
  job_person_out <- rbind(job_person_out,job_person_match[is.na(PersID),.(GeneralId,PersID,ISK)])
  
  job_person_match <- job_person_match[!is.na(PersID),sample_help2(GeneralId,ISK,prob),by=PersID]
  
  job_person_out <- rbind(job_person_match,job_person_out)
  
  skills.i <- skills.i[!GeneralId%in%job_person_out$GeneralId]
  
  donor_help <- donor_help[!PersID%in%job_person_out$PersID]
  setkey(donor_help,SKILLS)
  
}

# number of jobs with multiple selections
setnames(job_person_out,c("GeneralId"),c("JobID"))
job_person_out[is.na(PersID)]

save(job_person_out,file="/data/job_person_match.RData",compress=TRUE)

job_person_out
job_person_out <- merge(unique(gerLFS2[,.(PersID,PersID)]),job_person_out,by="PersID",all.y=TRUE)
job_person_out <- job_person_out[,.(PersID,JobID)]
save(job_person_out,file="/data/job_person_match.RData",compress=TRUE)

final_sample <- function(donor_help,job_person_out,prof){
  
  prof_final <- prof[GeneralId%in%job_person_out[is.na(LSFID)]$JobID]
  
  
  
}
