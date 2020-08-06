study2 <- read_csv("~/GDrive/research/thesis_submission/code/data/study2.csv")

study2 <- study2[!is.na(study2$consent),]
study2 <- study2[is.na(study2$random_check),]

study2 <- study2[study2$timcheck == 'The technologist',]
study2 <- study2[study2$alicecheck == 'The artist',]
study2 <- study2[study2$Q39 == study2$software,]

study2$is_agent = study2$software == 'SARA'

negative_valence = !is.na(study2$neg_mon_1)
positive_valence = !is.na(study2$pos_money_1)
study2$valence = as.numeric(positive_valence)
study2 <- study2[!is.na(study2$Q47_1),]

for (i in 1:4){
  study2[paste("MON_",i,sep="")] <-4000* l(study2[paste("neg_mon_",i,sep="")]) * l(1-study2['valence']) +  4000*l(study2[paste("pos_money_",i,sep="")]) * l(study2['valence'])
}

for (i in 1:5){
  study2[paste("RESP_",i,sep="")] <- l(study2[paste("neg_moral_resp_",i,sep="")]) * l(1-study2['valence']) +  l(study2[paste("pos_credit_",i,sep="")]) * l(study2['valence'])
}

p <- prcomp(data.matrix(study2[,c("Q47_1","Q47_2","Q47_3","Q47_4")]),center=F)
study2$anthro <- NA
study2$anthro[complete.cases(study2[,c("Q47_1","Q47_2","Q47_3","Q47_4")])]<- p$x[,1]*-1


###figs and results
figt3_left <- screeplot(p, type = "barplot")

fig3_right <- function(){
  tool_data<-as.numeric(as.matrix(study2[ (!study2$is_agent),'anthro']))
  agent_data<-as.numeric(as.matrix(study2[ (study2$is_agent),'anthro']))
  
  se <- function(data){
    n <- length(data)
    s <- sd(data,na.rm = T)
    return(qt(0.975,df=n-1)*s/sqrt(n))
  }
  
  
  y_tool <- mean(tool_data, na.rm=T)
  std_tool <- se(tool_data)
  y_agent <- mean(agent_data, na.rm=T)
  std_agent <- se(agent_data)
  
  
  ToPlot<-c(y_tool,y_agent)
  
  #plot barplot
  plot<-matrix(ToPlot,2,1,byrow=TRUE)   #with 2 being replaced by the number of genes!
  tplot<-t(plot)
  BarPlot <- barplot(tplot, beside=TRUE,ylab="Anthropomorphicity",main = "", ylim = c(0,max(study2$anthro)), names.arg=c("Tool Condition" , "Agent Condition"), col=c("#F38630","#69D2E7"))
  
  #add error bars
  ee<-matrix(c(std_tool, std_agent),2,1,byrow=TRUE)
  tee<-t(ee)
  error.bar(BarPlot,tplot,tee)
}

fig3_right()

tool_anthro_scores<-as.numeric(as.matrix(study2[ (!study2$is_agent),'anthro']))
agent_anthro_scores<-as.numeric(as.matrix(study2[ (study2$is_agent),'anthro']))
t.test(tool_anthro_scores, agent_anthro_scores, "less")

fig4 <- function(raw){
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

fig4(study2)

resp_ai_tool <- as.numeric(as.matrix(study2[(study2$is_agent),"RESP_5"]))
resp_ai_agent <-as.numeric(as.matrix(study2[(!study2$is_agent),"RESP_5"]))
t.test(resp_ai_tool, resp_ai_agent, alternative = "greater")

resp_artist_tool <- as.numeric(as.matrix(study2[(study2$is_agent),"RESP_3"]))
resp_artis_agent <-as.numeric(as.matrix(study2[(!study2$is_agent),"RESP_3"]))
t.test(resp_artist_tool, resp_artis_agent, alternative = "less")

resp_technologist_tool <- as.numeric(as.matrix(study2[(study2$is_agent),"RESP_2"]))
resp_technologist_agent <-as.numeric(as.matrix(study2[(!study2$is_agent),"RESP_2"]))
t.test(resp_technologist_tool, resp_technologist_agent, alternative = "greater")

summary(lm(RESP_5 ~ is_agent + valence + is_agent*valence, data=study2))
summary(lm(RESP_2 ~ is_agent + valence + is_agent*valence, data=study2))
summary(lm(RESP_3 ~ is_agent + valence + is_agent*valence, data=study2))

summary(lm(RESP_1 ~ is_agent + valence + is_agent*valence, data=study2))
summary(lm(RESP_4 ~ is_agent + valence + is_agent*valence, data=study2))

summary(lm(MON_2 ~ is_agent + valence + is_agent*valence, data=study2))
summary(lm(MON_3 ~ is_agent + valence + is_agent*valence, data=study2))

summary(lm(MON_1 ~ is_agent + valence + is_agent*valence, data=study2))
summary(lm(MON_4 ~ is_agent + valence + is_agent*valence, data=study2))

resp_technologist_tool <- as.numeric(as.matrix(study2[(study2$is_agent),"MON_4"]))
resp_technologist_agent <-as.numeric(as.matrix(study2[(!study2$is_agent),"MON_4"]))
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

mon_artist_tool <- as.numeric(as.matrix(study2[(study2[,"is_agent"]),"MON_3"]))
mon_artis_agent <-as.numeric(as.matrix(study2[(!study2[,"is_agent"]),"MON_3"]))
t.test(mon_artist_tool, mon_artis_agent, alternative = "less")

mon_resp_technologist_agent_tool <- as.numeric(as.matrix(raw[(raw[,"is_agent"]),"MON_2"]))
mon_resp_technologist_agent_agent <-as.numeric(as.matrix(raw[(!raw[,"is_agent"]),"MON_2"]))
t.test(mon_resp_technologist_agent_tool, mon_resp_technologist_agent_agent, alternative = "greater")
