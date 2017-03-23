###########################################################################################
# READ DATA FROM EURES DATA
#
#

read_eures <- function(COUNTRY="Germany"){
  
  person <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_MAIN.csv")
  res_int <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_RESIDENCE_INTER.csv")
  res <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_RES_COUNTRY.csv")
  lo_int <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_LAST_OCCUPATION_INTER.csv")
  lo <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_LAST_OCCUPATION.csv")
  sk <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_SKILL.csv")
  # des_occ <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_DESIRED_OCCUPATION.csv")
  # des_occ_int <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_DESIRED_OCCUP_INTER.csv")
  # des_res <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_DES_REGION.csv")
  # des_res_int <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_DES_REGION_INTER.csv")
  # ed <- fread("/data/eures/CV data DWH 2016-12-01 CSV/CV_EDUCATION.csv")
  
  person <- merge(person,res_int,by="ID",all.x=TRUE,all.y=FALSE)
  person <- merge(person, res, by="RES_COUNTRY_ID",all.x=T,all.y=FALSE)
  pers_skill <- person[RES_COUNTRY==COUNTRY]
  pers_skill <- merge(pers_skill,lo_int,by="ID",all.x=T)
  pers_skill <- merge(pers_skill,lo,by="OCCUPATION_ID",all.x=T)
  pers_skill <- merge(pers_skill,sk,by="ID",all.x=TRUE,all.y=FALSE)
  
  rm(person,res,res_int,lo,lo_int,sk,des_occ,des_occ_int,des_res,des_res_int,ed)
  
  return(pers_skill)
}
