####
#
# get ESCO LEVELS from 4-1


get_esco <- function(x){
  
  skills <- x[,.(GeneralId,SKILL=Skill_Esco_Level_4)]
  j <- 1
  if(class(skills)[1]=="character"){
    
    while(any(skills=="NULL")){
      null_index <- skills=="NULL"
      skills[null_index] <- unlist(x[,.(get(paste("Skill_Esco_Level_",4-j,sep="")))])[null_index]
      j <- j+1
    }
    
  }else{
    while(any(skills$SKILL=="NULL")){
      null_index <- skills$SKILL=="NULL"
      next_ESCO <- x[,.(GeneralId,SKILL=get(paste("Skill_Esco_Level_",4-j,sep="")))]
      skills[SKILL=="NULL",SKILL:=next_ESCO$SKILL[null_index]]
      j <- j+1
    }
  }
  return(skills)
}
