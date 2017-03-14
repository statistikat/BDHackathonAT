plotMain <- function(bubbleData){
  ggplot(bubbleData,aes(x=quarter,y=Jobgroup,size=value,color=pressure))+
    geom_point()+theme_bw()+scale_colour_gradient2()+theme(legend.position = "none")
}