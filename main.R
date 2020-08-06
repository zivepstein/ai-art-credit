#this is the code to fully replicate the results from The Curious Case of Edmund de Belamy: Human Perception of Machine Behavior
library(readr)
error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
l <- function(x){
  d <- as.numeric(unlist(x))
  d[is.na(d)] <- 0
  return(d)
}

study1 <- read_csv("~/GDrive/research/thesis_submission/code/data/study0.csv")

study1 <- study1[!is.na(study1$consent),]
study1 <- study1[!is.na(study1$Q47_1),]

study1 <- study1[is.na(study1$random_check),]
study1 <- study1[((study1$timmy_att == 'The technologist') & (!is.na(study1$timmy_att ))),]
study1 <- study1[study1$alice_att == 'The artist',]
study1 <- study1[study1$soft_att == 'ELIZA',]

for (i in 1:4){
  study1[paste("MON_",i,sep="")] <-4000* l(study1[paste("neg_mon_",i,sep="")])  +  4000*l(study1[paste("pos_money_",i,sep="")]) 
}

for (i in 1:5){
  study1[paste("RESP_",i,sep="")] <- l(study1[paste("neg_moral_resp_",i,sep="")])  +  l(study1[paste("pos_credit_",i,sep="")]) 
}

p <- prcomp(data.matrix(study1[,c("Q47_1","Q47_2","Q47_3","Q47_4")]),center=F)
study1$anthro <- NA
study1$anthro[complete.cases(study1[,c("Q47_1","Q47_2","Q47_3","Q47_4")])]<- p$x[,1]*-1

anthro_pos <- study1$anthro[!is.na(study1$pos_credit_1)]
anthro_neg <- study1$anthro[is.na(study1$pos_credit_1)]
t.test(anthro_pos, anthro_neg)
study1$valence <- is.na(study1$pos_credit_1)

resp_pos <- study1$RESP_5[!is.na(study1$pos_credit_1)]
resp_neg <- study1$RESP_5[is.na(study1$pos_credit_1)]
t.test(resp_pos, resp_neg)

fig1_left <- screeplot(p, type = "barplot", ylim = c(0,50))

colz <- c()
for (i in 1:length(study1$anthro)){
  if (study1$anthro[i] < median(study1$anthro)){
    colz <- c(colz,rgb(0.949,  0.525490,  0.1882353i, 0.3) )
  } else{
    colz <- c(colz,rgb(0.4117647,  0.82352, 0.90588, 0.5) )
  }
}

plot(study1$anthro, study1$RESP_5, col=colz, pch=16, ylab = "Responsibility", xlab = "Anthropomorphicity")
abline(lm(study1$RESP_5 ~ study1$anthro),lwd = 3, col= "grey")
anthro_density <- density(study1$anthro, bw=0.5)
plot(anthro_density, main="", xlab = "", xlim = c(2,12), type='n')
polygon(anthro_density, col=rgb(red = 0.5, green = 0.5, blue = 0.5, alpha = 0.5))

fig1_right <- function(){
  tool_data<-as.numeric(as.matrix(study1[ study1$anthro < median(study1$anthro) ,'RESP_5']))
  agent_data<-as.numeric(as.matrix(study1[ study1$anthro >= median(study1$anthro) ,'RESP_5']))
  
  y_tool <- mean(tool_data, na.rm=T)
  std_tool <- se(tool_data)
  y_agent <- mean(agent_data, na.rm=T)
  std_agent <- se(agent_data)
  
  se <- function(data){
    n <- length(data)
    s <- sd(data,na.rm = T)
    return(qt(0.975,df=n-1)*s/sqrt(n))
  }
  
  ToPlot<-c(y_tool,y_agent)
  
  #plot barplot
  plot<-matrix(ToPlot,2,1,byrow=TRUE)   #with 2 being replaced by the number of genes!
  tplot<-t(plot)
  BarPlot <- barplot(tplot, beside=TRUE,ylab="Responsibility",main = "", ylim = c(0,7), names.arg=c("Tool Condition" , "Agent Condition"), col=c("#F38630","#69D2E7"))
  
  #add error bars
  ee<-matrix(c(std_tool, std_agent),2,1,byrow=TRUE)
  tee<-t(ee)
  error.bar(BarPlot,tplot,tee)
}

resp_low_anthro<-as.numeric(as.matrix(study1[ study1$anthro < median(study1$anthro) ,'RESP_5']))
resp_high_anthro<-as.numeric(as.matrix(study1[ study1$anthro >= median(study1$anthro) ,'RESP_5']))
t.test(resp_low_anthro, resp_high_anthro, alternative = "less")


study1 <- read_csv("~/GDrive/research/thesis_submission/code/data/study1.csv")

study1 <- study1[!is.na(study1$consent),]
study1 <- study1[is.na(study1$random_check),]
study1 <- study1[study1$timcheck == 'The technologist',]
study1 <- study1[study1$alicecheck == 'The artist',]
study1 <- study1[study1$Q39 == study1$software,]

study1$is_agent = study1$software == 'SARA'

negative_valence = !is.na(study1$neg_mon_1)
positive_valence = !is.na(study1$pos_money_1)
study1$valence = as.numeric(positive_valence)
neither = (!positive_valence) & (!negative_valence) 
study1 <- study1[!neither,]


for (i in 1:4){
  study1[paste("MON_",i,sep="")] <-4000* l(study1[paste("neg_mon_",i,sep="")]) * l(1-study1['valence']) +  4000*l(study1[paste("pos_money_",i,sep="")]) * l(study1['valence'])
}

for (i in 1:5){
  study1[paste("RESP_",i,sep="")] <- l(study1[paste("neg_moral_resp_",i,sep="")]) * l(1-study1['valence']) +  l(study1[paste("pos_credit_",i,sep="")]) * l(study1['valence'])
}

p <- prcomp(data.matrix(study1[,c("Q47_1","Q47_2","Q47_3","Q47_4")]),center=F)
study1$anthro <- NA
study1$anthro[complete.cases(study1[,c("Q47_1","Q47_2","Q47_3","Q47_4")])]<- p$x[,1]*-1

tool_anthro_scores<-as.numeric(as.matrix(study1[ (!study1$is_agent),'anthro']))
agent_anthro_scores<-as.numeric(as.matrix(study1[ (study1$is_agent),'anthro']))
t.test(tool_anthro_scores, agent_anthro_scores, "less")

figt2_left <- screeplot(p, type = "barplot")

fig2_right <- function(){
  tool_data<-as.numeric(as.matrix(study1[ (!study1$is_agent),'anthro']))
  agent_data<-as.numeric(as.matrix(study1[ (study1$is_agent),'anthro']))
  
  y_tool <- mean(tool_data, na.rm=T)
  std_tool <- se(tool_data)
  y_agent <- mean(agent_data, na.rm=T)
  std_agent <- se(agent_data)
  
  se <- function(data){
    n <- length(data)
    s <- sd(data,na.rm = T)
    return(qt(0.975,df=n-1)*s/sqrt(n))
  }
  
  ToPlot<-c(y_tool,y_agent)
  
  #plot barplot
  plot<-matrix(ToPlot,2,1,byrow=TRUE)   #with 2 being replaced by the number of genes!
  tplot<-t(plot)
  BarPlot <- barplot(tplot, beside=TRUE,ylab="Anthropomorphicity",main = "", ylim = c(0,max(study1$anthro)), names.arg=c("Tool Condition" , "Agent Condition"), col=c("#F38630","#69D2E7"))
  
  #add error bars
  ee<-matrix(c(std_tool, std_agent),2,1,byrow=TRUE)
  tee<-t(ee)
  error.bar(BarPlot,tplot,tee)
}
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
  legend("topleft", cex = 0.75,
         legend = c("AI as tool", "AI as agent"), 
         fill = c("#F38630","#69D2E7"))
  
  #add error bars
  ee<-matrix(c(std_cr1,std_cr3,std_t1,std_t3,std_a1,std_a3,std_ca1,std_ca3,std_E1,std_E3),5,2,byrow=TRUE)
  tee<-t(ee)
  error.bar(BarPlot,tplot,tee)
}

resp_ai_tool <- as.numeric(as.matrix(raw[(raw[,"is_agent"]),"RESP_5"]))
resp_ai_agent <-as.numeric(as.matrix(raw[(!raw[,"is_agent"]),"RESP_5"]))
t.test(resp_ai_tool, resp_ai_agent, alternative = "greater")

resp_artist_tool <- as.numeric(as.matrix(raw[(raw[,"is_agent"]),"RESP_3"]))
resp_artis_agent <-as.numeric(as.matrix(raw[(!raw[,"is_agent"]),"RESP_3"]))
t.test(resp_artist_tool, resp_artis_agent, alternative = "less")

resp_technologist_tool <- as.numeric(as.matrix(raw[(raw[,"is_agent"]),"RESP_2"]))
resp_technologist_agent <-as.numeric(as.matrix(raw[(!raw[,"is_agent"]),"RESP_2"]))
t.test(resp_technologist_tool, resp_technologist_agent, alternative = "greater")

fig4 <- function(raw){
  mean_cr1<-mean(as.numeric(as.matrix(raw[(!raw[,"is_agent"]),"MON_1"])),na.rm = T)
  mean_t1<-mean(as.numeric(as.matrix(raw[(!raw[,"is_agent"]),"MON_2"])),na.rm = T)
  mean_a1<-mean(as.numeric(as.matrix(raw[(!raw[,"is_agent"]),"MON_3"])),na.rm = T)
  mean_ca1<-mean(as.numeric(as.matrix(raw[(!raw[,"is_agent"]),"MON_4"])),na.rm = T)
  mean_cr3<-mean(as.numeric(as.matrix(raw[(raw[,"is_agent"]),"MON_1"])),na.rm = T)
  mean_t3<-mean(as.numeric(as.matrix(raw[(raw[,"is_agent"]),"MON_2"])),na.rm = T)
  mean_a3<-mean(as.numeric(as.matrix(raw[(raw[,"is_agent"]),"MON_3"])),na.rm = T)
  mean_ca3<-mean(as.numeric(as.matrix(raw[(raw[,"is_agent"]),"MON_4"])),na.rm = T)
  
  se <- function(treatment, row){
    data <- as.numeric(as.matrix(raw[(raw[,"is_agent"] == treatment),row]))
    n <- len(data)
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
  BarPlot <- barplot(tplot, beside=TRUE,ylab="Percent", ylim = c(0,250000),
                     names.arg=c("Crowd","Technologist", "Artist", "Curator"),col=c("#F38630","#69D2E7"))
  
  #add legend
  legend("topleft", cex = 0.75,
         legend = c("AI as tool", "AI as agent"), 
         fill = c("#F38630","#69D2E7"))
  
  #add error bars
  ee<-matrix(c(std_cr1,std_cr3,std_t1,std_t3,std_a1,std_a3,std_ca1,std_ca3),4,2,byrow=TRUE)
  tee<-t(ee)
  error.bar(BarPlot,tplot,tee)
}

mon_artist_tool <- as.numeric(as.matrix(raw[(raw[,"is_agent"]),"MON_3"]))
mon_artis_agent <-as.numeric(as.matrix(raw[(!raw[,"is_agent"]),"MON_3"]))
t.test(mon_artist_tool, mon_artis_agent, alternative = "less")

mon_resp_technologist_agent_tool <- as.numeric(as.matrix(raw[(raw[,"is_agent"]),"MON_2"]))
mon_resp_technologist_agent_agent <-as.numeric(as.matrix(raw[(!raw[,"is_agent"]),"MON_2"]))
t.test(mon_resp_technologist_agent_tool, mon_resp_technologist_agent_agent, alternative = "greater")
##################################
study2 <- read_csv("~/GDrive/research/thesis_submission/code/data/study2.csv")
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



domain_resp <- function(raw){
  a1<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'art'),"RESP_1"])),na.rm = T)
  a2<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'art'),"RESP_2"])),na.rm = T)
  a3<-mean(as.numeric(as.matrix(raw[(raw$domain == 'art'),"RESP_3"])),na.rm = T)
  a4<-mean(as.numeric(as.matrix(raw[(raw$domain == 'art'),"RESP_4"])),na.rm = T)
  a5<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'art') & (raw$condition == 1),"RESP_5"])),na.rm = T)
  
  c1<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'car'),"RESP_1"])),na.rm = T)
  c2<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'car'),"RESP_2"])),na.rm = T)
  c3<-mean(as.numeric(as.matrix(raw[(raw$domain == 'car'),"RESP_3"])),na.rm = T)
  c4<-mean(as.numeric(as.matrix(raw[(raw$domain == 'car'),"RESP_4"])),na.rm = T)
  c5<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'car') & (raw$condition == 1),"RESP_5"])),na.rm = T)
  
  h1<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'justice'),"RESP_1"])),na.rm = T)
  h2<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'justice'),"RESP_2"])),na.rm = T)
  h3<-mean(as.numeric(as.matrix(raw[(raw$domain == 'justice'),"RESP_3"])),na.rm = T)
  h4<-mean(as.numeric(as.matrix(raw[(raw$domain == 'justice'),"RESP_4"])),na.rm = T)
  h5<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'justice') & (raw$condition == 1),"RESP_5"])),na.rm = T)
  
  j1<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'health'),"RESP_1"])),na.rm = T)
  j2<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'health'),"RESP_2"])),na.rm = T)
  j3<-mean(as.numeric(as.matrix(raw[(raw$domain == 'health'),"RESP_3"])),na.rm = T)
  j4<-mean(as.numeric(as.matrix(raw[(raw$domain == 'health'),"RESP_4"])),na.rm = T)
  j5<-mean(as.numeric(as.matrix(raw[ (raw$domain == 'health') & (raw$condition == 1),"RESP_5"])),na.rm = T)
  
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
  std_a2<-se("art", "RESP_2")
  std_a3<-se("art", "RESP_3")
  std_a4<-se("art", "RESP_4")
  std_a5<-se("art", "RESP_5")
  std_c1<-se("car", "RESP_1")
  std_c2<-se("car", "RESP_2")
  std_c3<-se("car", "RESP_3")
  std_c4<-se("car", "RESP_4")
  std_c5<-se("car", "RESP_5")
  std_h1<-se("health", "RESP_1")
  std_h2<-se("health", "RESP_2")
  std_h3<-se("health", "RESP_3")
  std_h4<-se("health", "RESP_4")
  std_h5<-se("health", "RESP_5")
  std_j1<-se("justice", "RESP_1")
  std_j2<-se("justice", "RESP_2")
  std_j3<-se("justice", "RESP_3")
  std_j4<-se("justice", "RESP_4")
  std_j5<-se("justice", "RESP_5")
  
  ToPlot<-c(a1,c1,h1,j1,a2,c2,h2,j2,a3,c3,h3,j3,a4,c4,h4,j4,a5,c5,h5,j5)
  
  #colz = c("#FF4E50", "#FC913A", "#F9D423","#EDE574","#E1F5C4")
  #colz = c("#A8E6CE", "#DCEDC2", "#FFD3B5","#FFAAA6","#FF8C94")
  colz = c("#AAFF00", "#FF00AA","#FFAA00", "#AA00FF","grey")
  
  #plot barplot
  nplot<-matrix(ToPlot,5,4,byrow=TRUE)   #with 2 being replaced by the number of genes!
  
  colorFlip <- function(orderby,i){
    print(orderby)
    return(colz[order(orderby)])
  }
  

  
  plot <- apply(nplot,2,sort,decreasing=F)
  cplot <- apply(nplot,2,colorFlip, i =1:5)
  BarPlot <- barplot(plot, beside=TRUE,ylab="Responsibility",main = "Responsibility by condition",ylim = c(0,7),
                     names.arg=c("art","car", "health", "justice"),col=cplot)
  
  #add legend
  legend("topleft", cex = 0.5,
         legend = c("crowd", "technologist", "practioner", "curator", "ai"), 
         fill = colz)
  
  #add error bars
  ee<-matrix(c(std_a1,std_c1,std_h1,std_j1,std_a2,std_c2,std_h2,std_j2,std_a3,std_c3,std_h3,std_j3,std_a4,std_c4,std_h4,std_j4,std_a5,std_c5,std_h5,std_j5),5,4,byrow=TRUE)
  
  stdFlip <- function(orderby){
    i <- get_col_index(nplot, orderby)
    stdz <- ee[,i]
    return(stdz[order(orderby)])
  }
 
  get_col_index <- function(m,c){
    for (i in 1:dim(m)[2]){
      if (sum(c == nplot[,i]) == dim(m)[1]){
        return(i)
      }
    }
  }
  tee <- apply(nplot,2,stdFlip)
  error.bar(BarPlot,plot,tee)
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

summary(lm(RESP_5~anthro + as.factor(domain) + RESP_1+RESP_2+ RESP_3+ RESP_4   , data = study2[study2$condition ==1,]))
