#移动平均
yan.ma<-function(cdata,mas=c(5,20,60),titie,sDate,eDate){ 
  ldata<-cdata
  for(m in mas){
    ldata<-merge(ldata,SMA(cdata,m))
  }
  ldata<-na.locf(ldata, fromLast=TRUE)
  names(ldata)<-c('Value',paste('ma',mas,sep=''))
  
  # 均线图
    g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
    g<-g+geom_line()
    g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
    g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),limits = c(sDate,eDate))
    g<-g+xlab("") + ylab("Price")+ggtitle(title)
    
    print(g)

    return(list(ldata,g))
}
