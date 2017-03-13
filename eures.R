library(VIM);library(data.table);


person <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_MAIN.csv")
res_int <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_RESIDENCE_INTER.csv")
res <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_RES_COUNTRY.csv")
lo_int <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_LAST_OCCUPATION_INTER.csv")
lo <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_LAST_OCCUPATION.csv")
sk <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_SKILL.csv")
des_occ <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_DESIRED_OCCUPATION.csv")
des_occ_int <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_DESIRED_OCCUP_INTER.csv")
des_res <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_DES_REGION.csv")
des_res_int <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_DES_REGION_INTER.csv")
ed <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_EDUCATION.csv")


person <- merge(person,res_int,by="ID",all.x=TRUE,all.y=FALSE)
person <- merge(person, res, by="RES_COUNTRY_ID",all.x=T,all.y=FALSE)
germany <- person[RES_COUNTRY=="Germany"&CV_STATUS=='ACTIVE']
germany <- merge(germany,ed,all.x=TRUE,by="ID")
germany <- merge(germany,sk,by="ID",all.x=TRUE,all.y=FALSE)
germany <- merge(germany,lo_int,by="ID",all.x=T)
germany <- merge(germany,lo,by="OCCUPATION_ID",all.x=T)
germany <- merge(germany,des_occ_int,by="ID",all.x=T)
germany <- merge(germany,des_occ,by="DES_OCCUPATION_ID",all.x=T)
germany <- merge(germany,des_res_int,by="ID",all.x=T)
germany <- merge(germany,des_res,by="DESIRED_NUTS_ID",all.x=T)


germany[,ID_new:=substr(ID,6,nchar(ID))]

job <- fread("/data/eures/JV data DWH 2016-11-30 CSV/JV_MAIN.csv")
edu <- fread("/data/eures/JV data DWH 2016-11-30 CSV/JV_EDUC_SKILLS.csv")
isco <- fread("/data/eures/JV data DWH 2016-11-30 CSV/JV_ISCO.csv")
isco[,ISCO_SYS_CODE:=as.character(ISCO_SYS_CODE)]
isco <- isco[!duplicated(ISCO_SYS_CODE)]
job <- merge(job,edu,by="EDU_SKILLS",all.x=TRUE,all.y=FALSE)
job <- merge(job,isco,by="ISCO_SYS_CODE",all.x=TRUE)

save(job,germany,file="/data/eures.Rdata",compress=TRUE)
