####
#
# get ESCO LEVELS from 4-1


get_skills <- function(x,hierarchy=c("Skill_Esco_Level_4","Skill_Esco_Level_3","Skill_Esco_Level_2","Skill_Esco_Level_1")){
  
  skills <- x[,.(JobID,SKILLS=get(hierarchy[1]))]
  j <- 1
  if(class(skills)[1]=="character"){
    
    while(any(skills=="NULL")){
      null_index <- skills=="NULL"
      j <- j+1
      skills[null_index] <- unlist(x[,.(get(hierarchy[j]))])[null_index]
      
    }
    
  }else{
    while(any(skills$SKILL=="NULL")){
      null_index <- skills$SKILL=="NULL"
      j <- j+1
      next_ESCO <- x[,.(JobID,SKILL=get(JobID,SKILLS=get(hierarchy[j])))]
      skills[SKILL=="NULL",SKILLW:=next_ESCO$SKILLS[null_index]]
      
    }
  }
  return(skills)
}
