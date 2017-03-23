####
#
# get ESCO LEVELS from 4-1


get_skills <- function(x,hierarchy=c("Skill_Esco_Level_4","Skill_Esco_Level_3","Skill_Esco_Level_2","Skill_Esco_Level_1")){
  
  skills <- x[,c("JobID",hierarchy[1]),with=FALSE]
  setnames(skills,hierarchy[1],"SKILLS")
  j <- 1
  if(class(skills)[1]=="character"){
    
    while(any(skills=="NULL")){
      null_index <- skills=="NULL"
      j <- j+1
      skills[null_index] <- unlist(x[,.(get(hierarchy[j]))])[null_index]
      
    }
    
  }else{
    while(any(skills$SKILLS=="NULL")){
      null_index <- skills$SKILLS=="NULL"
      j <- j+1
      next_ESCO <- x[,c("JobID",hierarchy[j]),with=FALSE]
      setnames(next_ESCO,hierarchy[j],"SKILLS")
      skills[SKILLS=="NULL",SKILLS:=next_ESCO$SKILLS[null_index]]
      
    }
  }
  return(skills)
}
