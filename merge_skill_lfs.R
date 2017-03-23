#########################################################################
# FUNCTION TO LINK pers_skill-DATA TO LABOURFORCE-DATA
#
#

merge_pers_skill_lfs <- function(pers_skill,lfs,match_var=c("OCCUP_L0_CODE","SEX","ageMATCH"),keep_var=""){
  
  pers_tmp <- pers_skill[(!is.na(SKILLS))&!duplicated(ID)&!is.na(OCCUP_L3_CODE)&!is.na(ageMATCH)&!is.na(SEX),.(ID,ageMATCH,SEX,OCCUP_L0_CODE)]
  lfs_tmp <- lfs[,.(LFSID,ageMATCH,SEX,OCCUP_L0_CODE)]
  lfs_tmp[,ID:=NA_character_]

  pers_lfs <- rbind(pers_tmp,lfs_tmp,fill=TRUE)
  
  pers_lfs <- match_data(pers_lfs,imp_var="ID",match_var=match_var)$data
  lfs_skill <- pers_lfs[!is.na(LFSID)]
  
  lfs <- merge(lfs,lfs_skill,by=colnames(lfs_skill)[colnames(lfs_skill)!="ID"])
  
  lfs <- lfs[ILOSTAT==2,.SD[rep(1:.N,unique(round(COEFF*1000)))],by="LFSID"]
  lfs[,PersID:=1:nrow(lfs)]
  
  return(lfs)
}