study2 <- read_csv("~/GDrive/research/thesis_submission/code/data/study3.csv")
correct_practioner <- function(domain){
  if (domain == 'art'){
    return("The artist")
  } else if (domain == 'car'){
    return("The car mechanic")
  } else if (domain == 'health'){
    return("The consultant")
  } else if (domain == 'justice'){
    return("The consultant")
  } else{
    return("")
  }
}
study2$correct_practioner <-unlist(lapply(study2$domain,correct_practioner))
cl <- function(x){x[is.na(x)] <- ""; return(x)}
c0 <- function(x){x[is.na(x)] <- 0; return(as.numeric(as.matrix(x)))}
tcheck <- paste0(cl(study2$timcheck), cl(study2$Q157), cl(study2$Q160), cl(study2$Q163))
acheck <- paste0(cl(study2$alicecheck), cl(study2$Q158), cl(study2$Q161), cl(study2$Q164))
scheck <- paste0(cl(study2$Q39), cl(study2$Q159), cl(study2$Q162), cl(study2$Q165))

study2 <- study2[(tcheck == 'The technologist')  & (acheck == study2$correct_practioner)& is.na(study2$random_check),]
study2 <- study2[which(study2$consent == 'Yes'),]

for (i in 1:4){
  study2[,paste("raw_anthro_", i,sep="")] <- c0(study2[,paste("Q47_", i,sep="")]) + c0(study2[,paste("Q150_", i,sep="")]) + c0(study2[,paste("Q153_", i,sep="")]) + c0(study2[,paste("Q156_", i,sep="")])
}
p <- prcomp(data.matrix(study2[,c("raw_anthro_1","raw_anthro_2","raw_anthro_3","raw_anthro_4")]),center=F)
study2$anthro <- NA
study2$anthro[complete.cases(study2[,c("raw_anthro_1","raw_anthro_2","raw_anthro_3","raw_anthro_4")])]<- p$x[,1]*-1

for (i in 1:5){
  if (i < 5){
    study2[,paste("RESP_", i,sep="")] <- c0(study2[,paste("NMR_ART_AI_", i,sep="")]) + c0(study2[,paste("NMR_CAR_AI_", i,sep="")]) + c0(study2[,paste("NMR_HEALTH_AI_", i,sep="")]) + c0(study2[,paste("NMR_JUSTICE_AI_", i,sep="")]) + c0(study2[,paste("NMR_ART_NO_", i,sep="")]) + c0(study2[,paste("NMR_CAR_NO_", i,sep="")]) + c0(study2[,paste("NMR_HEALTH_NO_", i,sep="")]) + c0(study2[,paste("NMR_JUSTICE_NO_", i,sep="")])    
  } else{
    study2[,paste("RESP_", i,sep="")] <- c0(study2[,paste("NMR_ART_AI_", i,sep="")]) + c0(study2[,paste("NMR_CAR_AI_", i,sep="")]) + c0(study2[,paste("NMR_HEALTH_AI_", i,sep="")]) + c0(study2[,paste("NMR_JUSTICE_AI_", i,sep="")]) 
  }
}

x <- paste("NMR_",toupper(study2$domain), "_AI_5", sep = "")

cond <- c()
for (i in 1:nrow(study2)){
  out <- study2[i,x[i]]
  if (is.na(out)){
    cond <- c(cond, 0)
  } else{
    cond <- c(cond, 1)
  }
}
study2$condition <- cond



##########analysis
resp_artist_nmr <- study2$RESP_4[study2$condition==0]
resp_artist_ <- study2$RESP_4[study2$condition==1]
t.test(resp_artist_nmr, resp_artist_)



domain_resp <- function(raw){
  a1<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'art'),"RESP_1"])),na.rm = T)
  a3<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'art'),"RESP_2"])),na.rm = T)
  a5<-mean(as.numeric(as.matrix(raw[(raw$domain == 'art'),"RESP_3"])),na.rm = T)
  a4<-mean(as.numeric(as.matrix(raw[(raw$domain == 'art'),"RESP_4"])),na.rm = T)
  a2<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'art') & (raw$condition == 1),"RESP_5"])),na.rm = T)
  
  c1<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'car'),"RESP_1"])),na.rm = T)
  c3<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'car'),"RESP_2"])),na.rm = T)
  c5<-mean(as.numeric(as.matrix(raw[(raw$domain == 'car'),"RESP_3"])),na.rm = T)
  c4<-mean(as.numeric(as.matrix(raw[(raw$domain == 'car'),"RESP_4"])),na.rm = T)
  c2<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'car') & (raw$condition == 1),"RESP_5"])),na.rm = T)
  
  h1<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'justice'),"RESP_1"])),na.rm = T)
  h3<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'justice'),"RESP_2"])),na.rm = T)
  h5<-mean(as.numeric(as.matrix(raw[(raw$domain == 'justice'),"RESP_3"])),na.rm = T)
  h4<-mean(as.numeric(as.matrix(raw[(raw$domain == 'justice'),"RESP_4"])),na.rm = T)
  h2<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'justice') & (raw$condition == 1),"RESP_5"])),na.rm = T)
  
  j1<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'health'),"RESP_1"])),na.rm = T)
  j3<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'health'),"RESP_2"])),na.rm = T)
  j5<-mean(as.numeric(as.matrix(raw[(raw$domain == 'health'),"RESP_3"])),na.rm = T)
  j4<-mean(as.numeric(as.matrix(raw[(raw$domain == 'health'),"RESP_4"])),na.rm = T)
  j2<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'health') & (raw$condition == 1),"RESP_5"])),na.rm = T)
  
  se <- function(dom, row){
    if (row !="RESP_5"){
      data <- as.numeric(as.matrix(raw[(raw[,"domain"] == dom),row]))  
    } else{
      data <- as.numeric(as.matrix(raw[(raw[,"domain"] == dom)& (raw$condition == 1),row]))  
    }
    n <- length(data)
    s <- sd(data,na.rm = T)
    return(qt(0.975,df=n-1)*s/sqrt(n))
  }
  
  std_a1<-se("art", "RESP_1")
  std_a3<-se("art", "RESP_2")
  std_a5<-se("art", "RESP_3")
  std_a4<-se("art", "RESP_4")
  std_a2<-se("art", "RESP_5")
  
  std_c1<-se("car", "RESP_1")
  std_c3<-se("car", "RESP_2")
  std_c5<-se("car", "RESP_3")
  std_c4<-se("car", "RESP_4")
  std_c2<-se("car", "RESP_5")
  
  std_h1<-se("health", "RESP_1")
  std_h3<-se("health", "RESP_2")
  std_h5<-se("health", "RESP_3")
  std_h4<-se("health", "RESP_4")
  std_h2<-se("health", "RESP_5")
  
  std_j1<-se("justice", "RESP_1")
  std_j3<-se("justice", "RESP_2")
  std_j5<-se("justice", "RESP_3")
  std_j4<-se("justice", "RESP_4")
  std_j2<-se("justice", "RESP_5")
  
  ToPlot<-c(a1,c1,h1,j1,a2,c2,h2,j2,a3,c3,h3,j3,a4,c4,h4,j4,a5,c5,h5,j5)
  
  #colz = c("#FF4E50", "#FC913A", "#F9D423","#EDE574","#E1F5C4")
  #colz = c("#A8E6CE", "#DCEDC2", "#FFD3B5","#FFAAA6","#FF8C94")
  colz = c("#AAFF00", "grey", "#FF00AA", "#AA00FF","#FFAA00")
  
  #plot barplot
  nplot<-matrix(ToPlot,5,4,byrow=TRUE)   #with 2 being replaced by the number of genes!

  BarPlot <- barplot(nplot, beside=TRUE,ylab="Responsibility",main = "Responsibility by condition",ylim = c(0,7),
                     names.arg=c("AI Art","Self-Driving Car", "Medical Diagnosis", "Criminal Justice System"),col=colz)
  
  #add legend
  legend("topleft", cex = 0.5,
         legend = c("crowd", "ai", "technologist", "curator", "practitioner"), 
         fill = colz)
  
  #add error bars
  ee<-matrix(c(std_a1,std_c1,std_h1,std_j1,std_a2,std_c2,std_h2,std_j2,std_a3,std_c3,std_h3,std_j3,std_a4,std_c4,std_h4,std_j4,std_a5,std_c5,std_h5,std_j5),5,4,byrow=TRUE)
  
  error.bar(BarPlot,nplot,ee)
}



domain_rep_by_anthro <- function(raw){
  a1<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'art')& (raw$anthro < median(raw$anthro[raw$domain == 'art'])),"RESP_5"])),na.rm = T)
  a2<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'art')& (raw$anthro > median(raw$anthro[raw$domain == 'art'])),"RESP_5"])),na.rm = T)
  
  
  c1<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'car')& (raw$anthro < median(raw$anthro[raw$domain == 'car'])),"RESP_5"])),na.rm = T)
  c2<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'car')& (raw$anthro > median(raw$anthro[raw$domain == 'car'])),"RESP_5"])),na.rm = T)
  
  
  h1<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'justice')& (raw$anthro < median(raw$anthro[raw$domain == 'justice'])),"RESP_5"])),na.rm = T)
  h2<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'justice')& (raw$anthro > median(raw$anthro[raw$domain == 'justice'])),"RESP_5"])),na.rm = T)
  
  j1<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'health')& (raw$anthro < median(raw$anthro[raw$domain == 'health'])),"RESP_5"])),na.rm = T)
  j2<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'health')& (raw$anthro > median(raw$anthro[raw$domain == 'health'])),"RESP_5"])),na.rm = T)
  
  
  se <- function(dom, row){
    data <- as.numeric(as.matrix(raw[(raw[,"domain"] == dom),row]))
    n <- length(data)
    s <- sd(data,na.rm = T)
    return(qt(0.975,df=n-1)*s/sqrt(n))
  }
  
  std_a1<-se("art", "RESP_1")
  std_a2<-se("art", "RESP_2")
  
  std_c1<-se("car", "RESP_1")
  std_c2<-se("car", "RESP_2")
  
  std_h1<-se("health", "RESP_1")
  std_h2<-se("health", "RESP_2")
  
  std_j1<-se("justice", "RESP_1")
  std_j2<-se("justice", "RESP_2")
  
  ToPlot<-c(a1,c1,h1,j1,a2,c2,h2,j2)
  
  #colz = c("#FF4E50", "#FC913A", "#F9D423","#EDE574","#E1F5C4")
  #colz = c("#A8E6CE", "#DCEDC2", "#FFD3B5","#FFAAA6","#FF8C94")
  
  #plot barplot
  nplot<-matrix(ToPlot,2,4,byrow=TRUE)   #with 2 being replaced by the number of genes!
  BarPlot <- barplot(nplot, beside=TRUE,ylab="Responsibility",main = "Responsibility by condition",ylim = c(0,7),
                     names.arg=c("AI Art","Self-Driving Car", "Medical Diagnosis", "Criminal Justice"),col=c("#F38630","#69D2E7"))
  
  #add legend
  legend("topleft", cex = 0.5,
         legend = c("tool", "agent"), 
         fill = c("#F38630","#69D2E7"))
  
  #add error bars
  ee<-matrix(c(std_a1,std_c1,std_h1,std_j1,std_a2,std_c2,std_h2,std_j2),2,4,byrow=TRUE)
  error.bar(BarPlot,nplot,ee)
}

study2$health <- as.numeric(study2$domain == 'health')
study2$justice <- as.numeric(study2$domain == 'justice')
study2$car <- as.numeric(study2$domain == 'car')
study2$health <- study2$health - mean(study2$health)
study2$justice <- study2$justice - mean(study2$justice)
study2$car <- study2$car - mean(study2$car)

summary(lm(RESP_5~anthro + justice + car+ health + anthro*justice +  anthro*car+ anthro*health   , data = study2[study2$condition ==1,]))
