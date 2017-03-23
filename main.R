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
rm(jobs_doc)

persons <- merge(persons[,.(ID,PersID)],pers_skill[,.(ID,SKILLS)],by="ID",allow.cartesian=TRUE)

rm(lfs,pers_skill,lfs.na)
gc()
jobs[,JobID:=paste(JobID,ID_new)]
out1 <- matching(persons=persons,jobs=jobs,bound=.3,match_vars=c("SKILLS"),mc.cores=10,mc.groups=50)
save(out1,file="/data/final_results.RData",compress=TRUE)


