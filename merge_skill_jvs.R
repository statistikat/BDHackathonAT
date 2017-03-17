############################################################################
# FUNCTION TO MERGE JVS-DATA WITH DATA ON REQUIERED SKILLS FOR JOBS
#
#

merge_skill_jvs <- function(skill,jvs,match_var){
  
  prof <- fread("/data/cedefop/ft_skill_profession_en.csv",header=FALSE)
  setnames(prof,colnames(prof),c("JobID","PublicationCountry","Esco_Level_4","Esco_Level_3",
                                 "Esco_Level_2","Esco_Level_1","Skill_Esco_Level_4","Skill_Esco_Level_3",
                                 "Skill_Esco_Level_2","Skill_Esco_Level_1","Skill_Esco_Level_0"))
  prof <- prof[PublicationCountry=="DEUTSCHLAND"]
  prof <- unique(prof)
  prof <- prof[Skill_Esco_Level_0=="Job-specific skills/competences"]
  
  doc <- fread("/data/cedefop/ft_document_en.csv",header=FALSE)
  setnames(doc,colnames(doc),c("JobID","PublicationCountry","GrabDate","YearGrabDate","MonthGrabDate",
                               "DayGrabDate","ExpireDate","YearExpireDate","MonthExpireDate","DayExpireDate",
                               "Esco_Level_4","Esco_Level_3","Esco_Level_2","Esco_Level_1","Nut_Level_3",
                               "Nut_Level_2","Nut_Level_1","Nut_Level_0","Contract","EducationalLevel","Industry_Level_2",
                               "Industry_Level_1","WorkingHours"))
  
  
  doc[,QUARTER:=if(MonthGrabDate[1]<=3){'Q1'}else if(MonthGrabDate[1]<=6){'Q2'}else if(MonthGrabDate[1]<=9){'Q3'}else{'Q4'},by=MonthGrabDate]
  job_quarter <- unique(doc[,.(JobID,QUARTER)])
  save(job_quarter,file="/data/job_quarter.RData",compress=TRUE)
  
  load("/data/jvs.RData")
  
  uindex <- c(6,5,10,3,1,9,0,2,10,9,11,7,10,9,4,10,8,10,2,0,2)
  ulevels <- doc[,unique(Industry_Level_1)]
  
  doc[,NACE:=uindex[ulevels%in%Industry_Level_1],by=Industry_Level_1]
  
  doc <- unique(doc,by="JobID")
  doc <- doc[PublicationCountry=="DEUTSCHLAND"]
  doc <- doc[JobID%in%prof$JobID]
  
  
  quart <- c("Q1","Q2","Q3","Q4")
  
  out <- NULL
  for(i in 0:11){
    if(i==0){
      out_iq <- doc[NACE==0]
      out_iq[,WEIGHT:=1]
      out <- rbind(out,out_iq)
    }else{
      for(q in 1:4){
        out_iq <- doc[NACE==i&QUARTER==quart[q]]
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
  
  job_weights <- out[,.(JobID,WEIGHT)]
  save(job_weights,file="/data/job_weights.RData",compress=TRUE)
  
  load("/data/job_weights.RData")
  prof <- prof[JobID%in%job_weights$JobID]
  prof <- merge(prof,job_weights,by="JobID",all.x=TRUE)
  
  random_round <- function(x){
    
    if(sample(c(0,1),1)==1){
      xr <- ceiling(unique(x$WEIGHT))
    }else{
      xr <- floor(unique(x$WEIGHT))
    }
    n <- nrow(x)
    x <- x[rep(1:n,xr)]
    x[,ID_new:=sort(rep(1:xr,n))]
    
    return(x)
  }
  
  prof <- prof[,.(JobID,Skill_Esco_Level_1,Skill_Esco_Level_2,Skill_Esco_Level_3,Skill_Esco_Level_4,WEIGHT)]
  
  prof <- prof[,random_round(.SD),by="JobID"]
  
  
  
  
}


