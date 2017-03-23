#####################################################################################
# HELPFUNCTION TO MATCH DATA 
# CONSISTS OF PRIMITIVE IMPUTATION 
# DESIGNED FOR MATCHING ON DISCRETE VARIABLES ONLY
#
#

# working function
primitive.impute <- function(x){
  x.na <- is.na(x)
  if(all(!x.na)|all(x.na)){
    return(x)
  }
  # if(all(x.na)){
  #   warning("no donors present in subsample")
  #   return(x)
  # }
  n.imp <- sum(x.na)
  if(length(x[!x.na])>1){
    x[x.na] <- sample(x[!x.na],n.imp,replace=TRUE)
  }else{
    x[x.na] <- x[!x.na]
  }
  return(x)
}

# main function
# imp_var can only be a single collumn (yet)
match_data <- function(dat,imp_var,match_var){
  
  na_present <- dat[,sum(is.na(get(imp_var)))]
  
  
  count_missings <- matrix(c(1,na_present),nrow=1,ncol=2)
  colnames(count_missings) <- c("STEP","NA_PRESENT")
  
  i <- length(match_var)
  j <- 1
  while(na_present>0){
    
    if(i>0){
      dat[,c(imp_var):=primitive.impute(get(imp_var)),by=c(match_var[1:i])]
    }else{
      dat[,c(imp_var):=primitive.impute(get(imp_var))]
    }
    na_present <- dat[,sum(is.na(get(imp_var)))]
    i <- i-1
    
    j <- j+1
    count_missings <- rbind(count_missings,c(j,na_present))
  }
  
  return(list(data=dat,count_missings=count_missings))
  
}






