##############################################################################
# MAIN FILE FOR MATCHING ALGORITHM
#
#

library(data.table)
library(VIM)
library(stringr)

source('random_round.R')
source('read_eures.R')
source('merge_skill_lfs.R')
source('merge_skill_jvs.R')
source('get_esco_skill.R')
source('match_pers_job.R')
source('match_data.R')

# read persons + skills from eures data for germany
pers_skill <- read_eures(COUNTRY="Germany")

lfs <- fread("/data/eu_lfs_puf/DE_LFS_2013_y.csv")
# remove NAs
lfs.na <- lfs[,lapply(.SD,function(z){!all(is.na(z))})]
lfs <- lfs[,colnames(lfs)[unlist(lfs.na)],with=FALSE]


##############################################################
# prep data manually
lfs <- lfs[!ILOSTAT%in%c(3,9)]
lfs[,ageMATCH:=AGE]
pers_skill[,ageMATCH:=7]
pers_skill[AGE%between%c(15,24),ageMATCH:=20]
pers_skill[AGE%between%c(25,39),ageMATCH:=32]
pers_skill[AGE%between%c(40,54),ageMATCH:=47]
pers_skill[AGE%between%c(55,74),ageMATCH:=65]
pers_skill[AGE>74,ageMATCH:=75]

#pers_skill$GENDER
pers_skill <- pers_skill[GENDER!="U"&!is.na(OCCUP_L0_CODE)&(!is.na(SKILLS)),]
pers_skill[,SEX:=1]
pers_skill[GENDER=="F",SEX:=2]
#pers_skill[,.N,by=OCCUP_L0_CODE]
#table(lfs[,.(ISCO1D,ISCOPR1D)],useNA = "always")
lfs[,OCCUP_L0_CODE:=floor(ISCO1D/100)]
lfs[ISCO1D==999,OCCUP_L0_CODE:=NA]
lfs[ISCO1D==999,OCCUP_L0_CODE:=NA]
lfs[ILOSTAT==2,OCCUP_L0_CODE:=ISCOPR1D]
#lfs[is.na(iscomatch),]
lfs[,LFSID:=.I]
#################################################################

# merge lfs with person_skill
# and use weights to expand data to the real number of unemployed
persons <- merge_pers_skill_lfs(pers_skill,lfs,match_var=c("OCCUP_L0_CODE","SEX","ageMATCH"))

# merge with desired occupation and desired residence

# des_occ <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_DESIRED_OCCUPATION.csv")
# des_occ_int <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_DESIRED_OCCUP_INTER.csv")
# des_res <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_DES_REGION.csv")
# des_res_int <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_DES_REGION_INTER.csv")
# 
# persons <- merge(persons,des_occ_int,by="ID",all.x=TRUE)
# persons <- merge(persons,des_occ,by="DES_OCCUPATION_ID",all.x=TRUE)
# persons <- merge(persons,des_res_int,by="ID",all.x=TRUE)
# persons <- merge(persons,des_res[,.(DESIRED_NUTS_ID,DES_REGION1_NAME)],by="DESIRED_NUTS_ID")
# 
# rm(des_occ,des_occ_int,des_res,des_res_int)

# use vacancies from cedefop
# and use also totals of vacancies from JVS to get reliable number of vacancies
source('read_cedefop.R')

jobs <- merge_skill_jvs(jobs=jobs,jobs_doc=jobs_doc,keep_var=c("EducationalLevel","Nut_Level_1"))
#rm(jobs_doc)
save(jobs,file="/data/jobs_expanded.RData",compress=TRUE)
load("/data/jobs_expanded.RData")
persons <- merge(persons[,.(ID,PersID)],pers_skill[,.(ID,SKILLS)],by="ID",allow.cartesian=TRUE)

rm(lfs,pers_skill,lfs.na)
gc()
jobs[,JobID:=paste(JobID,ID_new)]


load("~/BDHackathonAT/data/persons.RData")
load("~/BDHackathonAT/data/jobs.RData")

out1 <- matching(persons=persons,jobs=jobs,bound=0,match_vars=c("SKILLS"),mc.cores=10,mc.groups=50)
save(out1,file="/data/final_results.RData",compress=TRUE)



#out1[,JobID_o:=JobID]
out1[,JobID_o:=JobID]
out1[,JobID:=unlist(lapply(strsplit(JobID," "),`[[`,1))]
jobs[,JobID_o:=JobID]
jobs[,JobID:=unlist(lapply(strsplit(JobID_o," "),`[[`,1))]

# needs out1 / jobs (un-expandend) / persons
# prepare data for visualization
#
# esco2 <- jobs[,.(JobID,Esco_Level_2)]
# rm(prof);gc()
# load("/data/prof_with_weights.RData")
# load("/data/job_quarter.RData")
load("/data/jobgroupsMatch.RData")
# source("get_esco_skill.R")
# skill <- get_esco(prof)
# prof[,SKILL:=skill[,SKILL]]
jobs_doc <- fread("/data/cedefop/ft_document_en.csv",header=FALSE)
setnames(jobs_doc,colnames(jobs_doc),c("JobID","PublicationCountry","GrabDate","YearGrabDate","MonthGrabDate",
                                       "DayGrabDate","ExpireDate","YearExpireDate","MonthExpireDate","DayExpireDate",
                                       "Esco_Level_4","Esco_Level_3","Esco_Level_2","Esco_Level_1","Nut_Level_3",
                                       "Nut_Level_2","Nut_Level_1","Nut_Level_0","Contract","EducationalLevel","Industry_Level_2",
                                       "Industry_Level_1","WorkingHours"))
jobs_doc <- jobs_doc[PublicationCountry=="DEUTSCHLAND"]
jobs_doc[,QUARTER:=if(as.numeric(MonthGrabDate[1])<=3){'Q1'}else if(as.numeric(MonthGrabDate[1])<=6){'Q2'}else if(as.numeric(MonthGrabDate[1])<=9){'Q3'}else{'Q4'},by=MonthGrabDate]

jobs_doc <- jobs_doc[JobID%in%jobs$JobID]



jobs[,SKILL:=Skill_Esco_Level_4]
jobs <- jobs[SKILL!="NULL"]
jobs <- merge(jobs,unique(jobs_doc[,.(JobID,QUARTER,Esco_Level_2)]),by="JobID",all.x=TRUE,all.y=FALSE)
#jobs[,QUARTER:=sample(c("Q1","Q2","Q3","Q4"),nrow(jobs),rep=TRUE)]
#jobs <- merge(jobs,esco2[!duplicated(GeneralId)],by="GeneralId",all.x=TRUE,all.y=FALSE)
jobs <- merge(jobs,jobgroupsMatch,by="Esco_Level_2",all.x=TRUE,all.y=FALSE)


bubbleData <- jobs[!duplicated(JobID_o),.(value=.N),by=.(job_groups,QUARTER)]
jobs_open <- jobs[(!duplicated(JobID_o))&JobID_o%in%out1[is.na(PersID)]$JobID_o]#merge(prof,job_person_out[is.na(PersID)],by.x="GeneralId",by.y="JobID2")
#prof_open[,QUARTER:=sample(c("Q1","Q2","Q3","Q4"),nrow(prof_open),rep=TRUE)]
bubbleData <- merge(bubbleData,jobs_open[,.(nonFilled=.N),by=.(job_groups,QUARTER)],by=c("job_groups","QUARTER"),all.x=TRUE)
bubbleData[is.na(nonFilled),nonFilled:=0]
bubbleData[,pressure:=nonFilled/value]
save(bubbleData,file="/data/bubbleData.RData")
load("/data/bubbleData.RData")
npersav <- persons[!duplicated(PersID),.N]
matched <- merge(persons[!duplicated(PersID)],out1,by.y="PersID",by.x="PersID")#[LFSID%in%job_person_out[,PersID],,by=job_groups]
matched <- merge(matched,jobs[!duplicated(JobID_o),.(JobID_o,job_groups)],by="JobID_o")
matched <- matched[,.N,by=job_groups]
skillmiss <- jobs_open[!SKILL%in%c(
  "General nursing","General nursing","Staff development",
  "Industry and trade issues","Health care issues",
  "Electrical engineering","Driving","German language teaching","Computing"),.N,by=.(SKILL,job_groups)]
skillmiss <- skillmiss[order(N,decreasing = TRUE)]
skillmiss <- skillmiss[,head(.SD,10),by=.(job_groups)]

load("/data/jobs_expanded.RData")
jobs[,JobID:=paste(JobID,ID_new)]

p <- persons[PersID%in%sample(unique(PersID),100000)]
j <- jobs[JobID%in%sample(unique(JobID),10000)]

p <- persons[!PersID%in%out1$PersID]
j <- jobs[JobID%in%out1[is.na(PersID)]]

skillmiss.impact <- skillmiss[,skill.impact(p=p,j=j,bound=0,sk=.BY),by="SKILL"]

skillm2 <- persons[!PersID%in%out1$PersID,.(avail=.N),by=.(SKILLS)]
skillmiss <- merge(skillmiss,skillm2,by.x="SKILL",by.y="SKILLS",all.x=TRUE)
skillmiss[is.na(avail),avail:=0]
skillmiss[,availability:=avail/sum(avail),by="job_groups"]
#skillmiss[is.na(availability),availability:=0]
save(npersav,skillmiss,matched,file="/data/skillmiss.RData")
load("/data/skillmiss.RData")

skill.impact <- function(p,j,bound,sk){
  add.p <- unique(p[,.(ID,PersID)])
  add.p[,SKILLS:=sk]
  p <- unique(rbind(p,add.p))
  
  j[,index:=any(Esco_level_4_skill==sk),by=JobID_o]
  j <- j[index==TRUE]
  
  jp <- matching(persons=p,jobs=j,bound=.3,match_vars=c("SKILLS"),mc.cores=2)
  return(nrow(jp[!is.na(PersID)]))
}




