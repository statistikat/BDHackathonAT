library(data.table)

prof <- fread("/data/cedefop/ft_skill_profession_en.csv",header=FALSE)
setnames(prof,colnames(prof),c("GeneralId","PublicationCountry","Esco_Level_4","Esco_Level_3",
                             "Esco_Level_2","Esco_Level_1","Skill_Esco_Level_4","Skill_Esco_Level_3",
                             "Skill_Esco_Level_2","Skill_Esco_Level_1","Skill_Esco_Level_0"))

expi <- fread("/data/cedefop/ft_skill_profession_experience_en.csv",header=FALSE)
prof_new <- fread("/data/cedefop/ft_skill_profession_new_en.csv",header=FALSE)
requ <- fread("/data/cedefop/ft_skill_profession_requirement_en.csv",header=FALSE)

colnames(expi) <- colnames(prof_new) <- colnames(requ) <- c("GeneralId","PublicationCountry","Esco_Level_4","Esco_Level_3","Esco_Level_2","Esco_Level_1","IdEsco_Skill","NGram") 


doc <- fread("/data/cedefop/ft_document_en.csv",header=FALSE)
setnames(doc,colnames(doc),c("GeneralId","PublicationCountry","GrabDate","YearGrabDate","MonthGrabDate",
                             "DayGrabDate","ExpireDate","YearExpireDate","MonthExpireDate","DayExpireDate",
                             "Esco_Level_4","Esco_Level_3","Esco_Level_2","Esco_Level_1","Nut_Level_3",
                             "Nut_Level_2","Nut_Level_1","Nut_Level_0","Contract","EducationalLevel","Industry_Level_2",
                             "Industry_Level_1","WorkingHours"))


save(prof,prof_new,requ,expi,doc,file="/data/cedefop.Rdata",compress=TRUE)

