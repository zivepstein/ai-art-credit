study4 <- read_csv("~/GDrive/research/thesis_submission/code/data/study4.csv")
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
study4$correct_practioner <-unlist(lapply(study4$domain,correct_practioner))
cl <- function(x){x[is.na(x)] <- ""; return(x)}
c0 <- function(x){x[is.na(x)] <- 0; return(as.numeric(as.matrix(x)))}
tcheck <- paste0(cl(study4$timcheck), cl(study4$Q157), cl(study4$Q160), cl(study4$Q163))
acheck <- paste0(cl(study4$alicecheck), cl(study4$Q158), cl(study4$Q161), cl(study4$Q164))
scheck <- paste0(cl(study4$Q39), cl(study4$Q159), cl(study4$Q162), cl(study4$Q165))

study4 <- study4[(tcheck == 'The technologist')  & (acheck == study4$correct_practioner)& is.na(study4$random_check),]
study4 <- study4[which(study4$consent == 'Yes'),]

for (i in 1:4){
  study4[,paste("raw_anthro_", i,sep="")] <- c0(study4[,paste("Q47_", i,sep="")]) + c0(study4[,paste("Q150_", i,sep="")]) + c0(study4[,paste("Q153_", i,sep="")]) + c0(study4[,paste("Q156_", i,sep="")])
}
p <- prcomp(data.matrix(study4[,c("raw_anthro_1","raw_anthro_2","raw_anthro_3","raw_anthro_4")]),center=F)
study4$anthro_score <- NA
study4$anthro_score[complete.cases(study4[,c("raw_anthro_1","raw_anthro_2","raw_anthro_3","raw_anthro_4")])]<- p$x[,1]*-1

for (i in 1:4){
  study4[paste("MON_",i,sep="")] <-4000* l(study4[paste("neg_mon_",i,sep="")])  + 4000* l(study4[paste("Q131_",i,sep="")]) + 4000* l(study4[paste("Q134_",i,sep="")]) + 4000* l(study4[paste("Q137_",i,sep="")])  
}

for (i in 1:5){
  study4[paste("RESP_",i,sep="")] <- l(study4[paste("neg_moral_resp_",i,sep="")]) + l(study4[paste("Q132_",i,sep="")]) +  l(study4[paste("Q135_",i,sep="")]) + l(study4[paste("Q138_",i,sep="")])  
}

study4$is_agent = study4$anthro ==1


##########analysis
fig3 <- function(raw){
  mean_cr1<-mean(as.numeric(as.matrix(raw[(!raw$is_agent),"RESP_1"])),na.rm = T)
  mean_t1<-mean(as.numeric(as.matrix(raw[(!raw$is_agent),"RESP_2"])),na.rm = T)
  mean_a1<-mean(as.numeric(as.matrix(raw[(!raw$is_agent),"RESP_3"])),na.rm = T)
  mean_ca1<-mean(as.numeric(as.matrix(raw[(!raw$is_agent),"RESP_4"])),na.rm = T)
  mean_E1<-mean(as.numeric(as.matrix(raw[(!raw$is_agent),"RESP_5"])),na.rm = T)
  mean_cr3<-mean(as.numeric(as.matrix(raw[(raw$is_agent),"RESP_1"])),na.rm = T)
  mean_t3<-mean(as.numeric(as.matrix(raw[(raw$is_agent),"RESP_2"])),na.rm = T)
  mean_a3<-mean(as.numeric(as.matrix(raw[(raw$is_agent),"RESP_3"])),na.rm = T)
  mean_ca3<-mean(as.numeric(as.matrix(raw[raw$is_agent,"RESP_4"])),na.rm = T)
  mean_E3<-mean(as.numeric(as.matrix(raw[(raw$is_agent),"RESP_5"])),na.rm = T)
  
  se <- function(treatment, row){
    data <- as.numeric(as.matrix(raw[(raw[,"is_agent"] == treatment),row]))
    n <- length(data)
    s <- sd(data,na.rm = T)
    return(qt(0.975,df=n-1)*s/sqrt(n))
  }
  
  std_cr1<-se(FALSE, "RESP_1")
  std_t1<-se(FALSE, "RESP_2")
  std_a1<-se(FALSE, "RESP_3")
  std_ca1<-se(FALSE, "RESP_4")
  std_E1<-se(FALSE, "RESP_5")
  std_cr3<-se(TRUE, "RESP_1")
  std_t3<-se(TRUE, "RESP_2")
  std_a3<-se(TRUE, "RESP_3")
  std_ca3<-se(TRUE, "RESP_4")
  std_E3<-se(TRUE, "RESP_5")
  
  ToPlot<-c(mean_cr1,mean_cr3,mean_t1,mean_t3,mean_a1,mean_a3,mean_ca1,mean_ca3,mean_E1,mean_E3)
  
  #plot barplot
  plot<-matrix(ToPlot,5,2,byrow=TRUE)   #with 2 being replaced by the number of genes!
  tplot<-t(plot)
  BarPlot <- barplot(tplot, beside=TRUE,ylab="Responsibility", ylim = c(0,7),
                     names.arg=c("Crowd","Technologist", "Artist", "Curator", "ELIZA"),col=c("#F38630","#69D2E7"))
  
  #add legend
  #legend("topleft", cex = 0.75,
   #      legend = c("AI as tool", "AI as agent"), 
    #     fill = c("#F38630","#69D2E7"))
  
  #add error bars
  ee<-matrix(c(std_cr1,std_cr3,std_t1,std_t3,std_a1,std_a3,std_ca1,std_ca3,std_E1,std_E3),5,2,byrow=TRUE)
  tee<-t(ee)
  error.bar(BarPlot,tplot,tee)
}

fig4 <- function(raw){
  mean_cr1<-mean(as.numeric(as.matrix(raw[(!raw$is_agent),"MON_1"])),na.rm = T)
  mean_t1<-mean(as.numeric(as.matrix(raw[(!raw$is_agent),"MON_2"])),na.rm = T)
  mean_a1<-mean(as.numeric(as.matrix(raw[(!raw$is_agent),"MON_3"])),na.rm = T)
  mean_ca1<-mean(as.numeric(as.matrix(raw[(!raw$is_agent),"MON_4"])),na.rm = T)
  
  mean_cr3<-mean(as.numeric(as.matrix(raw[(raw$is_agent),"MON_1"])),na.rm = T)
  mean_t3<-mean(as.numeric(as.matrix(raw[(raw$is_agent),"MON_2"])),na.rm = T)
  mean_a3<-mean(as.numeric(as.matrix(raw[(raw$is_agent),"MON_3"])),na.rm = T)
  mean_ca3<-mean(as.numeric(as.matrix(raw[raw$is_agent,"MON_4"])),na.rm = T)
  
  
  se <- function(treatment, row){
    data <- as.numeric(as.matrix(raw[(raw[,"is_agent"] == treatment),row]))
    n <- length(data)
    s <- sd(data,na.rm = T)
    return(qt(0.975,df=n-1)*s/sqrt(n))
  }
  
  std_cr1<-se(FALSE, "MON_1")
  std_t1<-se(FALSE, "MON_2")
  std_a1<-se(FALSE, "MON_3")
  std_ca1<-se(FALSE, "MON_4")
  std_cr3<-se(TRUE, "MON_1")
  std_t3<-se(TRUE, "MON_2")
  std_a3<-se(TRUE, "MON_3")
  std_ca3<-se(TRUE, "MON_4")
  
  ToPlot<-c(mean_cr1,mean_cr3,mean_t1,mean_t3,mean_a1,mean_a3,mean_ca1,mean_ca3)
  
  #plot barplot
  plot<-matrix(ToPlot,4,2,byrow=TRUE)   #with 2 being replaced by the number of genes!
  tplot<-t(plot)
  BarPlot <- barplot(tplot, beside=TRUE,ylab="Money", ylim = c(0,400000),
                     names.arg=c("Crowd","Technologist", "Artist", "Curator"),col=c("#F38630","#69D2E7"))
  
  #add legend
  #legend("topleft", cex = 0.75,
  #      legend = c("AI as tool", "AI as agent"), 
  #     fill = c("#F38630","#69D2E7"))
  
  #add error bars
  ee<-matrix(c(std_cr1,std_cr3,std_t1,std_t3,std_a1,std_a3,std_ca1,std_ca3),4,2,byrow=TRUE)
  tee<-t(ee)
  error.bar(BarPlot,tplot,tee)
}
