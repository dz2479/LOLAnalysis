plot_missing <- function(data,percent,mode){
  if((dim(data)[2]>6) && unique(nchar(colnames(data))>=5)){
    colnames(data) = abbreviate(colnames(data), minlength=3)
  }
  
  #draw graph 1:num/percent rows missing vs cols
  
  data1= colSums(is.na(data)) %>%
  sort(decreasing = TRUE)
  x=names(data1)
  y=data1
  data1 = as.data.frame(cbind(x,y))
  data1$y=as.integer(data1$y)
  if(percent==TRUE){
    data1$y=data1$y/dim(data)[1]
  }
  if(mode == 1 || mode == 4){
    p1<-ggplot(data=data1, aes(x=reorder(x,-y), y=y)) +
    geom_bar(stat="identity",fill="lightblue") +
    theme(panel.grid.major.x = element_blank())
    if(percent==TRUE){
      p1 <- p1+labs(title="Missing value patterns",
      x=element_blank() ,y="% rows mssing:")
    }else{
      p1 <- p1+labs(title="Missing value patterns",
      x=element_blank() ,y="num rows mssing:")
    }
    if(mode == 1){
      plot(p1)
    }
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
  if(percent==TRUE){
    data2$y=data2$y/dim(data)[1]
  }
  if(mode == 2 || mode == 4){
    p2<-ggplot(data=data2, aes(x=reorder(x,-(1:dim(data2)[1])), y=y)) +
    geom_bar(stat="identity",fill="lightblue") +
    coord_flip() +
    theme(panel.grid.major.y = element_blank())
    if(percent==TRUE){
      p2 <- p2+labs(y="% row" ,x=element_blank())
    }else{
      p2 <- p2+labs(y="row count" ,x=element_blank())
    }
    if(mode == 2){
      plot(p2)
    }
  }
  
  #draw graph 3:
  
  
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
  text_num = length(index)
  
  if(mode == 3 || mode == 4){
    p3 <- ggplot(missing_patterns2,aes(x=fct_relevel(var,levels(reorder(data1$x,-data1$y))),reorder(y,-y)))+
    geom_tile(aes(fill=pattern),color="white",lwd=0.5,linetype=1)+
    scale_fill_manual(values=c("gray48", "gray","mediumslateblue")) +
    theme(legend.position = "none") +
    labs(x="variable",y="missing pattern")
    
    for(i in 1:text_num){
      p3 <- p3+ annotation_custom(grid::textGrob("complete cases"),
      xmin = -Inf, xmax = Inf, ymin = max-index[i]+1,ymax=max-index[i]+1)
    }
    if(mode == 3){
      plot(p3)
    }
  }
  
  #layout
  if(mode == 4){
    p1+plot_spacer()+p3+p2+plot_layout(widths = c(3, 1), heights = c(1, 3))
  }
}


