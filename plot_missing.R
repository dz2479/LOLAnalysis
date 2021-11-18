plot_missing <- function(data,percent){
  if((dim(data)[2]>6)){
    colnames(data) = abbreviate(colnames(data), minlength = 4, strict = TRUE )
  }
  
  #draw graph 1:num/percent rows missing vs cols
  data1= colSums(is.na(data)) %>%
    sort(decreasing = TRUE)
  
  x=names(data1)
  y=data1
  data1 = as.data.frame(cbind(x,y))
  data1$y=as.integer(data1$y)
  if(percent==TRUE){
    data1$y=data1$y/dim(data)[1]*100
  }
  p1<-ggplot(data=data1, aes(x=factor(x, levels =data1$x), y=y)) +
    geom_bar(stat="identity",fill="lightblue") +
    theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  if(percent==TRUE){
    p1 <- p1+labs(title="Missing value patterns",
                  x=element_blank() ,y="% rows mssing:")+coord_cartesian(ylim = c(0, 100)) 
  }else{
    p1 <- p1+labs(title="Missing value patterns",
                  x=element_blank() ,y="num rows mssing:")
  }
  
  
  #draw graph 2: missing_patterns vs row count/percent
  missing_patterns <- data.frame(is.na(data)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  
  x = as.character(1:dim(missing_patterns)[1])
  y = sort(missing_patterns$count,decreasing = TRUE)
  data2=as.data.frame(cbind(x,y))
  data2$y=as.integer(data2$y)
  
  y=1:dim(missing_patterns)[1]
  missing_patterns1 = cbind(missing_patterns,y) %>% select(-count)
  missing_patterns2 = missing_patterns1 %>%
    pivot_longer(!y,names_to="var",values_to = "pattern")
  missing_patterns2$pattern = as.character(missing_patterns2$pattern)
  
  for (row in unique(missing_patterns2$y)){
    if (isTRUE(unique(missing_patterns2[missing_patterns2$y==row,]$pattern)=="FALSE")){
      len = length(missing_patterns2[missing_patterns2$y==row,]$pattern)
      missing_patterns2[missing_patterns2$y==row,]$pattern=rep("complete",len)
    }
  }
  
  index = unique(missing_patterns2[missing_patterns2$pattern=="complete",]$y)
  max = max(missing_patterns2$y)
  
  if(percent==TRUE){
    data2$y=data2$y/dim(data)[1]*100
  }
  p2<-ggplot(data=data2, aes(x=reorder(x,-(1:dim(data2)[1])), y=y,fill=factor(ifelse(x==index,"Highlighted","Normal")))) +
    geom_bar(stat="identity") +
    scale_fill_manual(values=c("lightblue3","lightblue2"))+
    theme(panel.grid.major.y = element_blank(),legend.position = "none")+coord_flip()
  if(percent==TRUE){
    p2 <- p2+labs(y="% row" ,x=element_blank())+scale_y_continuous(limits = c(0, 100))
  }else{
    p2 <- p2+labs(y="row count" ,x=element_blank())
  }
  
  #draw graph 3:
  text_num = length(index)
  
  p3 <- ggplot(missing_patterns2,aes(x=factor(var,levels=data1$x),reorder(y,-y)))+
    geom_tile(aes(fill=pattern),color="white",lwd=0.5,linetype=1)+ 
    scale_fill_manual(values=c("gray48", "gray","mediumslateblue")) +
    theme(legend.position = "none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x="variable",y="missing pattern")
  
  for(i in 1:text_num){
    p3 <- p3+ annotation_custom(grid::textGrob("complete cases"), 
                                xmin = -Inf, xmax = Inf, ymin = max-index[i]+1,ymax=max-index[i]+1)
  }
  
  #layout
  p1+plot_spacer()+p3+p2+plot_layout(widths = c(3, 1), heights = c(1, 3))
  
}


