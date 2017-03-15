library(data.table)
load("/data/job_person_out_final2.RData")
load("/data/gerLFS2_skills.RData")
load("/data/prof.RData")
esco2 <- prof[,.(GeneralId,Esco_Level_2)]
rm(prof);gc()
load("/data/prof_with_weights.RData")
load("/data/job_quarter.RData")
load("/data/jobgroupsMatch.RData")
source("get_esco_skill.R")
skill <- get_esco(prof)
prof[,SKILL:=skill[,SKILL]]
prof <- merge(prof,job_quarter,by="GeneralId",all.x=TRUE,all.y=FALSE)
prof[,QUARTER:=sample(c("Q1","Q2","Q3","Q4"),nrow(prof),rep=TRUE)]
prof <- merge(prof,esco2[!duplicated(GeneralId)],by="GeneralId",all.x=TRUE,all.y=FALSE)
prof <- merge(prof,jobgroupsMatch,by="Esco_Level_2",all.x=TRUE,all.y=FALSE)
prof
bubbleData <- prof[!duplicated(GeneralId),.(value=.N),by=.(job_groups,QUARTER)]
prof_open <- prof[!duplicated(GeneralId)&GeneralId%in%job_person_out[is.na(PersID),JobID]]#merge(prof,job_person_out[is.na(PersID)],by.x="GeneralId",by.y="JobID2")
#prof_open[,QUARTER:=sample(c("Q1","Q2","Q3","Q4"),nrow(prof_open),rep=TRUE)]
bubbleData <- merge(bubbleData,prof_open[,.(nonFilled=.N),by=.(job_groups,QUARTER)],by=c("job_groups","QUARTER"),all.x=TRUE)
bubbleData[is.na(nonFilled),nonFilled:=0]
bubbleData[,pressure:=nonFilled/value]
save(bubbleData,file="/data/bubbleData.RData")
npersav <- gerLFS2[!duplicated(PersID),.N]
matched <- merge(gerLFS2[!duplicated(PersID)],job_person_out,by.y="PersID",by.x="PersID")#[LFSID%in%job_person_out[,PersID],,by=job_groups]
matched <- merge(matched,prof[!duplicated(GeneralId),.(GeneralId,job_groups)],by.x="JobID",by.y="GeneralId")
matched <- matched[,.N,by=job_groups]
skillmiss <- prof_open[!SKILL%in%c(
  "General nursing","General nursing","Staff development",
  "Industry and trade issues",
  "Electrical engineering","Driving","German language teaching","Computing"),.N,by=.(SKILL,job_groups)]
skillmiss <- skillmiss[order(N,decreasing = TRUE)]
skillmiss <- skillmiss[,head(.SD,5),by=.(job_groups)]
skillm2 <- gerLFS2[,.(avail=.N),by=.(SKILLS)]
skillmiss <- merge(skillmiss,skillm2,by.x="SKILL",by.y="SKILLS",all.x=TRUE)
skillmiss[,availability:=avail/sum(avail),by="job_groups"]
skillmiss[is.na(availability),availability:=0]
save(npersav,skillmiss,matched,file="/data/skillmiss.RData")
