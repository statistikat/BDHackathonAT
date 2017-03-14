library(data.table)
load("/data/job_person_match.RData")
load("/data/gerLFS2_skills.RData")
load("/data/prof.RData")
source("get_esco_skill.R")
skill <- get_esco(prof)
prof[,SKILL:=skill[,SKILL]]
prof_open <- merge(prof,job_person_out[is.na(PersID)],by.x="GeneralId",by.y="JobID")
gerLFS3 <- gerLFS2[!LFSID%in%job_person_out[,PersID]]

skillmiss <- prof_open[,.N,by=.(SKILL,job_groups)]
skillmiss <- skillmiss[order(N,decreasing = TRUE)]
skillmiss <- skillmiss[,head(.SD,5),by=.(job_groups)]

skillmiss[,availability:=rlnorm(nrow(skillmiss))]
save(skillmiss,file="/data/skillmiss.RData")
