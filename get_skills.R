prof <- merge(prof,unique(job[,.(ISCO0_CO,ISCO0_NAME)]),by.x="Esco_Level_1",by.y="ISCO0_NAME")
prof <- merge(prof,unique(job[,.(ISCO1_CO,ISCO1_NAME)]),by.x="Esco_Level_2",by.y="ISCO1_NAME")
prof <- merge(prof,unique(job[,.(ISCO2_CO,ISCO2_NAME)]),by.x="Esco_Level_3",by.y="ISCO2_NAME")
prof <- merge(prof,unique(job[,.(ISCO3_CO,ISCO3_NAME)]),by.x="Esco_Level_4",by.y="ISCO3_NAME")


get_skills <- function(prof,ISCO_LEVEL=2,sublevel=3,N=20){
  
  tab_prof <- prof[ISCO0_CO==ISCO_LEVEL&Skill_Esco_Level_4!='NULL',table(get(paste("Skill_Esco_Level_",sublevel,sep="")))]
  
  return(tail(sort(tab_prof),N))
}