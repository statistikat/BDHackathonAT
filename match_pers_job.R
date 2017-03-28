############################################################################
# FUNCTION TO MATCH PEOPLE TO JOBS
#
#

matching <- function(persons,jobs,bound=.3,match_vars=c("SKILLS","DES_OCCUPATION"),mc.cores=1,mc.groups=50,use.weight=TRUE){
  
  # get lowest level SKILLS codes from persons
  jobs_skills <- get_skills(jobs)
  
  # get groups of unique SKILLS
  # speeds up matching algorithm
  jobs_skills[,help:=paste(sort(SKILLS),collapse="."),by=JobID]
  jobs_skills[,JOB_GROUPS:=.GRP,by=help]
  jobs_skills[,help:=NULL]
  
  #
  setkeyv(persons,match_vars)
  donor_persons <- unique(persons[SKILLS%in%jobs_skills$SKILLS])
  # clean memory before parallize
  rm(jobs,persons)
  gc()
  #
  
  # find persons for every job
  if(mc.cores>1){
    
    jobs_skills.parts <- split_data(jobs_skills,mc.cores)
  
    job_person_match <- mclapply(jobs_skills.parts,wrap_sample_help,donor_persons,bound,match_vars,mc.cores = mc.cores)
    
    job_person_match <- rbindlist(job_person_match)
    
  }else{
    job_person_match <- jobs_skills[,sample_help(donor_persons[unique(.SD[,match_vars,with=FALSE])],bound=ceiling(length(unique(SKILLS))*bound),jobid=unique(JobID)),by=JOB_GROUPS]

    #job_person_match2 <- jobs_skills[,sample_help(donor_persons[unique(.SD[,match_vars,with=FALSE])],bound=ceiling(length(unique(SKILLS))*bound),jobid=unique(JobID)),by=JOB_GROUPS]
    #SD <- jobs_skills[JOB_GROUPS==2498]
    #dat <- donor_persons[unique(SD[,match_vars,with=FALSE])]

  }
  
  # donor_persons[!PersID%in%job_person_match$PersID][SKILLS%in%jobs_skills[JobID%in%job_person_match[is.na(PersID)]$JobID]$SKILLS]
  #
  # create output file
  # exist of jobs which have no potential person to occupie and jobs for which an potential employee was found
  job_person_out <- job_person_match[is.na(PersID),.(JobID,PersID,JOB_GROUPS)]
  
  # if person was listed for more than one job, select job for which poerson has moste matching SKILLSS
  job_person_match <- job_person_match[!is.na(PersID),sample_help2(JobID,JOB_GROUPS,Number),by=PersID]
  job_person_out <- rbind(job_person_out,job_person_match)
  
  # reduce set of available jobs by the set of jobs which have now been occupied (matched) by a person
  jobs_skills <- jobs_skills[!JobID%in%job_person_out$JobID]
  
  donor_persons <- donor_persons[!PersID%in%job_person_out$PersID]
  setkey(donor_persons,SKILLS)
  
  # do matching again until no more jobs are availiabe, meaning that they have been matched to a person or there exists not match with any person
  while(nrow(jobs_skills)>0){
    
    if(length(unique(jobs_skills$JOB_GROUPS))>mc.groups&mc.cores>1){
      jobs_skills.parts <- split_data(jobs_skills,mc.cores)
      job_person_match <- mclapply(jobs_skills.parts,wrap_sample_help,donor_persons,bound,match_vars,mc.cores = mc.cores)
      job_person_match <- rbindlist(job_person_match)
    }else{
      job_person_match <- jobs_skills[,sample_help(donor_persons[unique(.SD[,match_vars,with=FALSE])],bound=ceiling(length(unique(SKILLS))*bound),jobid=unique(JobID)),by=JOB_GROUPS]
    }
    
    
    job_person_out <- rbind(job_person_out,job_person_match[is.na(PersID),.(JobID,PersID,JOB_GROUPS)])
    
    job_person_match <- job_person_match[!is.na(PersID),sample_help2(JobID,JOB_GROUPS,Number),by=PersID]
    
    job_person_out <- rbind(job_person_match,job_person_out)
    
    jobs_skills <- jobs_skills[!JobID%in%job_person_out$JobID]
    
    donor_persons <- donor_persons[!PersID%in%job_person_out$PersID]
    setkey(donor_persons,SKILLS)
    
  }
  
  return(job_person_out)
  
}




################################################################################
################################################################################
# helpfunctions for matching function
#

sample_help <- function(dat,bound,jobid){
  
  if(any(!is.na(dat$PersID))){
    dat <- na.omit(dat)
    # condition on potential occupants
    potential_employee <- dat[,.N,by=PersID][N>=bound]
    #potential_employee <- dat[,.N,by=ID]
    if(nrow(potential_employee)>0){
      Number <- potential_employee$N
      
      njob <- length(jobid)
      nemp <- nrow(potential_employee)
      sampn <- min(njob,nemp)
      
      sampindex <- sample(nemp,sampn,prob=Number)
      
      Number <- c(Number[sampindex],rep(NA_integer_,njob-sampn))
      PersID <- c(potential_employee$PersID[sampindex],rep(NA_integer_,njob-sampn))
      
      return(list(Number=Number,PersID=PersID,JobID=jobid))
    }else{
      return(list(Number=rep(NA_integer_,length(jobid)),PersID=rep(NA_integer_,length(jobid)),JobID=jobid))
    }
  }else{
    return(list(Number=rep(NA_integer_,length(jobid)),PersID=rep(NA_integer_,length(jobid)),JobID=jobid))
  }
}

sample_help2 <- function(JobID,JOB_GROUPS,Number){
  if(length(Number)>1){
    take <- sample(length(Number),1,prob=Number)
    return(list(JobID=JobID[take],JOB_GROUPS=JOB_GROUPS[take]))
  }else{
    return(list(JobID=JobID,JOB_GROUPS=JOB_GROUPS))
  }
}

split_data <- function(dat,N=10){
  
  setkey(dat,JOB_GROUPS)
  uniqueid <- unique(dat$JOB_GROUPS)
  nparts <- ceiling(length(uniqueid)/N)
  
  finalparts <- c()
  for(i in 1:N){
    id_i <- uniqueid[((i-1)*nparts+1):min(nrow(dat),(i*nparts))]
    finalparts <- c(finalparts,rep(i,nrow(dat[JOB_GROUPS%in%id_i])))
  }
  
  dat[,parts:=finalparts]
  
  dat_out <- split(dat,as.factor(dat$parts))
  dat[,parts:=NULL]
  return(dat_out)
}

# wrap function usedd for parallelization only
wrap_sample_help <- function(dat,donor_persons,bound,match_vars){
  return(dat[,sample_help(donor_persons[unique(.SD[,match_vars,with=FALSE])],bound=ceiling(length(unique(SKILLS))*bound),jobid=unique(JobID)),by=JOB_GROUPS])
}

# wrap_sample_help2 <- function(dat){
#   return(dat[,sample_help(donor_help[unique(SKILLS)],bound=ceiling(length(unique(SKILLS))*.3),jobid=unique(GeneralId)),by=ISK])
# }
