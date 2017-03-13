library(VIM);library(data.table);

person <- fread("eures/CV data DWH 2016-12-01 CSV/CV_MAIN.csv")
res_int <- fread("eures/CV data DWH 2016-12-01 CSV/CV_RESIDENCE_INTER.csv")
res <- fread("eures/CV data DWH 2016-12-01 CSV/CV_RES_COUNTRY.csv")
sk <- fread("eures/CV data DWH 2016-12-01 CSV/CV_SKILL.csv")
person <- merge(person,res_int,by="ID",all.x=TRUE,all.y=FALSE)
person <- merge(person, res, by="RES_COUNTRY_ID",all.x=T,all.y=FALSE)
g <- person[RES_COUNTRY=="Germany"]
ed <- fread("eures/CV data DWH 2016-12-01 CSV/CV_EDUCATION.csv")
g <- merge(g,ed,all.x=TRUE,by="ID")
g <- merge(g,sk,by="ID",all.x=TRUE,all.y=FALSE)

job <- fread("eures/JV data DWH 2016-11-30 CSV/JV_MAIN.csv")
edu <- fread("eures/JV data DWH 2016-11-30 CSV/JV_EDUC_SKILLS.csv")
isco <- fread("eures/JV data DWH 2016-11-30 CSV/JV_ISCO.csv")
isco[,ISCO_SYS_CODE:=as.character(ISCO_SYS_CODE)]
isco <- isco[!duplicated(ISCO_SYS_CODE)]
job <- merge(job,edu,by="EDU_SKILLS",all.x=TRUE,all.y=FALSE)
job <- merge(job,isco,by="ISCO_SYS_CODE",all.x=TRUE)
