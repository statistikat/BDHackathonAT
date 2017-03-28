###########################################################################################
# READ DATA FROM CEDEFOP DATA
#
#

  
jobs <- fread("/data/cedefop/ft_skill_profession_en.csv",header=FALSE)
setnames(jobs,colnames(jobs),c("JobID","PublicationCountry","Esco_Level_4","Esco_Level_3",
                               "Esco_Level_2","Esco_Level_1","Skill_Esco_Level_4","Skill_Esco_Level_3",
                               "Skill_Esco_Level_2","Skill_Esco_Level_1","Skill_Esco_Level_0"))
jobs <- jobs[PublicationCountry=="DEUTSCHLAND"]
jobs <- unique(jobs)
jobs <- jobs[Skill_Esco_Level_0=="Job-specific skills/competences"]

# expi <- fread("/data/cedefop/ft_skill_jobsession_experience_en.csv",header=FALSE)
# jobs_new <- fread("/data/cedefop/ft_skill_jobsession_new_en.csv",header=FALSE)
# requ <- fread("/data/cedefop/ft_skill_jobsession_requirement_en.csv",header=FALSE)
# 
# colnames(expi) <- colnames(jobs_new) <- colnames(requ) <- c("GeneralId","PublicationCountry","Esco_Level_4","Esco_Level_3","Esco_Level_2","Esco_Level_1","IdEsco_Skill","NGram") 



jobs_doc <- fread("/data/cedefop/ft_document_en.csv",header=FALSE)
setnames(jobs_doc,colnames(jobs_doc),c("JobID","PublicationCountry","GrabDate","YearGrabDate","MonthGrabDate",
                                       "DayGrabDate","ExpireDate","YearExpireDate","MonthExpireDate","DayExpireDate",
                                       "Esco_Level_4","Esco_Level_3","Esco_Level_2","Esco_Level_1","Nut_Level_3",
                                       "Nut_Level_2","Nut_Level_1","Nut_Level_0","Contract","EducationalLevel","Industry_Level_2",
                                       "Industry_Level_1","WorkingHours"))
jobs_doc <- jobs_doc[PublicationCountry=="DEUTSCHLAND"]

jobs_doc <- jobs_doc[JobID%in%jobs$JobID]

jobs_doc[,QUARTER:=if(as.numeric(MonthGrabDate[1])<=3){'Q1'}else if(as.numeric(MonthGrabDate[1])<=6){'Q2'}else if(as.numeric(MonthGrabDate[1])<=9){'Q3'}else{'Q4'},by=MonthGrabDate]

  
