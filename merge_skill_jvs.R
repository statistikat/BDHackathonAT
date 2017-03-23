############################################################################
# FUNCTION TO MERGE JVS-DATA WITH DATA ON REQUIERED SKILLS FOR JOBS
#
#

merge_skill_jvs <- function(jobs,jobs_doc,keep_var=c("EducationalLevel","Nut_Level_1")){
  
  #job_quarter <- unique(jobs_doc[,.(JobID,QUARTER)])
  #save(job_quarter,file="/data/job_quarter.RData",compress=TRUE)
  
  # get better data from eurostat??
  load("/data/jvs.RData")
  
  jvsm[[9]][2] <- "PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES"
  jvsm[[3]][1] <- "MANUFACTURING"
  
  jobs_doc[,NACE1_h:=gsub(" ","_",toupper(str_trim(Industry_Level_1)))]
  jobs_doc[NACE1_h=="AGRICOLTURE,_FORESTRY_AND_FISHING",NACE1_h:="AGRICULTURE,_FORESTRY_AND_FISHING"]
  
  for(i in 1:length(jvsm)){
    
    nace_i <- gsub(" ","_",toupper(str_trim(jvsm[[i]])))
    
    jobs_doc[NACE1_h%in%nace_i,NACE1:=i]
  }
  
  
  jobs_doc[,NACE1_h:=NULL]
  jobs_doc[is.na(NACE1),NACE1:=0]
  
  quart <- c("Q1","Q2","Q3","Q4")
  jobs_doc[,.N,by="Industry_Level_1"]
  
  out <- NULL
  for(i in 0:11){
    if(i==0){
      out_iq <- jobs_doc[NACE1==0]
      out_iq[,WEIGHT:=1]
      out <- rbind(out,out_iq)
    }else{
      for(q in 1:4){
        out_iq <- jobs_doc[NACE1==i&QUARTER==quart[q]]
        if(nrow(out_iq)>0){
          out_iq[,WEIGHT:=unlist(jvs[i][,c(2+q),with=FALSE])*1000/.N]
          if(out_iq$WEIGHT[1]<1){
            out_iq[,WEIGHT:=1]
          }
          out <- rbind(out,out_iq)
        }
      }
    }
  }
  
  job_weights <- unique(out[,c("JobID","WEIGHT",keep_var),with=FALSE])
  #save(job_weights,file="/data/job_weights.RData",compress=TRUE)
  
  #load("/data/job_weights.RData")
  jobs <- merge(jobs,job_weights,by="JobID",all.x=TRUE)
  
  jobs <- jobs[,.(JobID,Skill_Esco_Level_1,Skill_Esco_Level_2,Skill_Esco_Level_3,Skill_Esco_Level_4,WEIGHT)]
  jobs <- jobs[,random_round(x=.SD,w=WEIGHT,group=.GRP),by=list(JobID,WEIGHT)]
  
  return(jobs)
}


