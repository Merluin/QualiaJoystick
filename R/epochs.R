epochs<-function(data,lag){
  
  require(plyr)
  if(data$procedure[1] == "emotion"){
  #HF
  temp <- data[which(data$category=="HF"),]
  idx <- c(0, cumsum(abs(diff(as.numeric(rownames(temp)))) > lag))+1
  HF <- split(temp,idx)
  dat<-ldply(HF, data.frame)

  #HD
  temp <- data[which(data$category=="HD"),]
  idx <- c(0, cumsum(abs(diff(as.numeric(rownames(temp)))) > lag))+1
  HD <- split(temp,idx)
  temp<-ldply(HD, data.frame)
  dat<- rbind(dat,temp)
  
  #NF
  temp <- data[which(data$category=="NF"),]
  idx <- c(0, cumsum(abs(diff(as.numeric(rownames(temp)))) > lag))+1
  NF <- split(temp,idx)
  temp<-ldply(NF, data.frame)
  dat<- rbind(dat,temp)
  
  #ND
  temp <- data[which(data$category=="ND"),]
  idx <- c(0, cumsum(abs(diff(as.numeric(rownames(temp)))) > lag))+1
  ND <- split(temp,idx)
  temp<-ldply(ND, data.frame)
  dat<- rbind(dat,temp)
  
  #HS
  temp <- data[which(data$category=="HS"),]
  idx <- c(0, cumsum(abs(diff(as.numeric(rownames(temp)))) > lag))+1
  HS <- split(temp,idx)
  temp<-ldply(HS, data.frame)
  dat<- rbind(dat,temp)
  
  #NS
  temp <- data[which(data$category=="NS"),]
  idx <- c(0, cumsum(abs(diff(as.numeric(rownames(temp)))) > lag))+1
  NS <- split(temp,idx)
  temp<-ldply(NS, data.frame)
  dat<- rbind(dat,temp)
  
  
  } else   if(data$procedure[1] == "gender"){
    #MF
    temp <- data[which(data$category=="MF"),]
    idx <- c(0, cumsum(abs(diff(as.numeric(rownames(temp)))) > lag))+1
    MF <- split(temp,idx)
    dat<-ldply(MF, data.frame)
    
    #MD
    temp <- data[which(data$category=="MD"),]
    idx <- c(0, cumsum(abs(diff(as.numeric(rownames(temp)))) > lag))+1
    MD <- split(temp,idx)
    temp<-ldply(MD, data.frame)
    dat<- rbind(dat,temp)
    
    #NF
    temp <- data[which(data$category=="FF"),]
    idx <- c(0, cumsum(abs(diff(as.numeric(rownames(temp)))) > lag))+1
    FF <- split(temp,idx)
    temp<-ldply(FF, data.frame)
    dat<- rbind(dat,temp)
    
    #ND
    temp <- data[which(data$category=="FD"),]
    idx <- c(0, cumsum(abs(diff(as.numeric(rownames(temp)))) > lag))+1
    FD <- split(temp,idx)
    temp<-ldply(FD, data.frame)
    dat<- rbind(dat,temp)
    
    #HS
    temp <- data[which(data$category=="FS"),]
    idx <- c(0, cumsum(abs(diff(as.numeric(rownames(temp)))) > lag))+1
    FS <- split(temp,idx)
    temp<-ldply(FS, data.frame)
    dat<- rbind(dat,temp)
    
    #NS
    temp <- data[which(data$category=="MS"),]
    idx <- c(0, cumsum(abs(diff(as.numeric(rownames(temp)))) > lag))+1
    MS <- split(temp,idx)
    temp<-ldply(MS, data.frame)
    dat<- rbind(dat,temp)
  }
    
  
  dat<-dat%>%
    'colnames<-'(c("epoch","subject",  "group", "block", "leftfile", "rightfile", "attribut", "menusone", "procedure", "row", "ms", "x", "y", "vector","category"))%>%
    select(subject,  group, block, leftfile, rightfile, attribut, menusone, procedure, row, ms, x, vector, category,epoch)
  
  return(dat)
}