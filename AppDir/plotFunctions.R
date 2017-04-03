library(stringr)
library(plotly)
#plotMain <- function(bubbleData){
#  ggplot(bubbleData,aes(x=quarter,y=Jobgroup,size=value,color=pressure))+
#    geom_point()+theme_bw()+scale_colour_gradient2()+theme(legend.position = "none")
#}

plotMain <- function(bubbleData){
  g <- ggplot(bubbleData,aes(x=QUARTER,y=job_groups,size=value,color=pressure))+
    geom_point()+theme_bw()+scale_colour_gradient(low = "lightsteelblue", high = "indianred3")+theme(legend.position = "none")+
    scale_size_continuous(range=c(1,7)) + # größen einstellen wie wir es brauchen 
    theme(axis.text.x = element_text(size=13),
          axis.ticks = element_blank(),
          axis.text.y = element_text(size=13), 
          axis.title.x= element_text(size=15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    coord_fixed(ratio = 1.3)+ #verändert abstand der quartale
    ylab("") #+
  ggplotly(g)
  #g
}


# plotBar: Farbabstufungen 
#plotBar <- function(skillmiss, jobgruppe){ #bei jobgruppe namen eingeben
#  dat <- skillmiss[which(skillmiss$job_groups==jobgruppe),]
#  dat$SKILL <- factor(dat$SKILL, levels = dat$SKILL)
#  
#  ggplot(data=dat, aes(x=SKILL, y=N, fill=availability)) +
#    geom_bar(colour="black", stat="identity") +
#    guides(fill=FALSE) +
 #   scale_fill_gradient(low = "coral1", high = "seagreen1")+
#    theme_bw()+
#    theme(axis.text.x = element_text(size=13), 
#          axis.text.y = element_text(size=13), 
#          axis.title.x= element_text(size=15)) +
#    ylab("") +
#    scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
#    
#}


# plotBar: nur 2 farben
plotBar <- function(skillmiss, jobgruppe){ #bei jobgruppe namen eingeben
  dat <- skillmiss[job_groups==jobgruppe,]
  #dat$SKILL <- factor(unique(dat$SKILL), levels = dat$SKILL)
  #dat$sign <- ifelse(dat$availability >= 0.06, "positive", "negative")
  
  dat[,SKILL:=factor(SKILL,levels=unique(dat$SKILL[sort(dat$value,decreasing=TRUE,index.return=TRUE)$ix]))]
  levels(dat$SKILL) <- gsub("\\/","",levels(dat$SKILL))
  levels(dat$SKILL) <- gsub("\\s+"," ",levels(dat$SKILL))
  levels(dat$SKILL) <- gsub(" ", "\n", levels(dat$SKILL))
  
  g <- ggplot(data=dat, aes(x=SKILL, y=value, fill=variable)) +
    geom_bar(colour="black", stat="identity") + 
    scale_fill_manual(values=c("lightsteelblue","indianred3"))+
    theme(legend.title=element_blank(),
          legend.justification=c(1,1), legend.position=c(1,1),
          legend.text = element_text(size = 13))+
    theme(axis.text.x = element_text(size=13), 
          axis.text.y = element_text(size=13), 
          axis.title.x= element_text(size=15),
          axis.line = element_line(colour = "grey"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    ylab("") + xlab("") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
  #ggplotly(g)
  g
}


