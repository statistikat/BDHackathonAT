plotMain <- function(bubbleData){
  ggplot(bubbleData,aes(x=quarter,y=Jobgroup,size=value,color=pressure))+
    geom_point()+theme_bw()+scale_colour_gradient2()+theme(legend.position = "none")
}

# änderungen zu plotMain: rot/grün; scale_size; achsenbeschriftung
plotMain2 <- function(bubbleData){
  ggplot(bubbleData,aes(x=quarter,y=Jobgroup,size=value,color=pressure))+
    geom_point()+theme_bw()+scale_colour_gradient(low = "red", high = "green")+theme(legend.position = "none")+ 
    scale_size_continuous(range=c(8,14)) + # größen einstellen wie wir es brauchen 
    theme(axis.text.x = element_text(size=10), 
          axis.text.y = element_text(size=10)) +
    ylab("") #+
  # coord_fixed(ratio = 1) #verändert abstand der quartale
}


plotBar <- function(skillmiss, jobgruppe){ #bei jobgruppe namen eingeben
  dat <- skillmiss[which(skillmiss$job_groups==jobgruppe),]
  dat$SKILL <- factor(dat$SKILL, levels = dat$SKILL)
  
  ggplot(data=dat, aes(x=SKILL, y=N, fill=availability)) +
    geom_bar(colour="black", stat="identity") +
    guides(fill=FALSE) +
    scale_fill_gradient(low = "red", high = "green")+
    ggtitle("Top 5 Skills") +
    theme(axis.text.x = element_text(size=11), 
          axis.text.y = element_text(size=11), 
          plot.title=element_text(size=14,face="bold")) +
    ylab("")
}


