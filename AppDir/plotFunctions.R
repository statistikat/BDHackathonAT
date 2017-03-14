plotMain <- function(bubbleData){
  ggplot(bubbleData,aes(x=quarter,y=Jobgroup,size=value,color=pressure))+
    geom_point()+theme_bw()+scale_colour_gradient2()+theme(legend.position = "none")
}


plotBar <- function(skillmiss, jobgruppe){ #bei jobgruppe namen eingeben
  dat <- skillmiss[which(skillmiss$job_groups==jobgruppe),]
  dat$SKILL <- factor(dat$SKILL, levels = dat$SKILL)
  
  ggplot(data=dat, aes(x=SKILL, y=N, fill=availability)) +
    geom_bar(colour="black", stat="identity") +
    guides(fill=FALSE) +
    scale_fill_gradient(low = "red", high = "green")+
    ggtitle("Top 5 Skills") +
    ylab("")
}