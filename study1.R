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

study1 <- read_csv("~/GDrive/research/thesis_submission/code/data/study1.csv")

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
study1$valence <- !is.na(study1$pos_credit_1)






#Figure 2 left
anthro_density1 <- density(study1$anthro[study1$valence], bw=0.5)
anthro_density2 <- density(study1$anthro[!study1$valence], bw=0.5)
plot(anthro_density2, main="", xlab = "Anthropomorphicity", xlim = c(2,12), type='n')
legend("topright", cex = 0.75,
       legend = c("Positive valence", "Negative valence"), 
       fill = c(rgb(red = 0, green = 1, blue = 0, alpha = 0.5),rgb(red =1, green =0, blue = 0, alpha = 0.5))
       )
polygon(anthro_density1, col=rgb(red = 0, green = 1, blue = 0, alpha = 0.5))
polygon(anthro_density2, col=rgb(red = 1, green =0, blue = 0, alpha = 0.5))


#fiture 2 right
fig1_right <- function(){
  tool_data1<-as.numeric(as.matrix(study1[( study1$anthro < median(study1$anthro)) & !study1$valence,'RESP_5']))
  agent_data1<-as.numeric(as.matrix(study1[ (study1$anthro > median(study1$anthro)) &! study1$valence ,'RESP_5']))
  tool_data2<-as.numeric(as.matrix(study1[( study1$anthro < median(study1$anthro)) & study1$valence,'RESP_5']))
  agent_data2<-as.numeric(as.matrix(study1[ (study1$anthro > median(study1$anthro)) & study1$valence ,'RESP_5']))
  
  se <- function(data){
    n <- length(data)
    s <- sd(data,na.rm = T)
    return(qt(0.975,df=n-1)*s/sqrt(n))
  }
  
  
  y_tool1 <- mean(tool_data1, na.rm=T)
  std_tool1 <- se(tool_data1)
  y_agent1 <- mean(agent_data1, na.rm=T)
  std_agent1 <- se(agent_data1)
  y_tool2 <- mean(tool_data2, na.rm=T)
  std_tool2 <- se(tool_data2)
  y_agent2 <- mean(agent_data2, na.rm=T)
  std_agent2 <- se(agent_data2)
  
  
  ToPlot<-c(y_tool1,y_agent1,y_tool2,y_agent2)
  
  #plot barplot
  plot<-matrix(ToPlot,2,2,byrow=TRUE)   #with 2 being replaced by the number of genes!
  tplot<-t(plot)
  BarPlot <- barplot(tplot, beside=TRUE,ylab="Responsibility",main = "", ylim = c(0,7), names.arg=c("Negative valence" , "Positive valence"), col=c("#F38630","#69D2E7"))
  legend("topleft", cex = 0.75,
         legend = c("Ai < median(A)", "Ai > median(A)"), 
         fill = c("#F38630","#69D2E7"))
  #add error bars
  ee<-matrix(c(std_tool1, std_agent1,std_tool2, std_agent2),2,2,byrow=TRUE)
  tee<-t(ee)
  error.bar(BarPlot,tplot,tee)
}

fig1_right()

study1$is_agent = study1$anthro > median(study1$anthro)

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


#summary statistics of anthro measure by valence
mean(study1$anthro[study1$valence])
sd(study1$anthro[study1$valence])

mean(study1$anthro[!study1$valence])
sd(study1$anthro[!study1$valence])

#anthro and responsibiltiy
resp_low_anthro<-as.numeric(as.matrix(study1[ study1$anthro < median(study1$anthro) ,'RESP_5']))
resp_high_anthro<-as.numeric(as.matrix(study1[ study1$anthro >= median(study1$anthro) ,'RESP_5']))
t.test(resp_low_anthro, resp_high_anthro, alternative = "less")

################analyses
summary(lm(RESP_5 ~ anthro + valence + anthro*valence, data=study1))
summary(lm(RESP_2 ~ anthro + valence + anthro*valence, data=study1))
summary(lm(RESP_1 ~ anthro + valence + anthro*valence, data=study1))

summary(lm(RESP_5 ~ anthro, data=study1[study1$valence,]))
summary(lm(RESP_5 ~ anthro, data=study1[!study1$valence,]))#add scale 

tool_data1<-as.numeric(as.matrix(study1[( study1$anthro < median(study1$anthro)) & !study1$valence,'RESP_5']))
agent_data1<-as.numeric(as.matrix(study1[ (study1$anthro > median(study1$anthro)) &! study1$valence ,'RESP_5']))
t.test(tool_data1, agent_data1, alternative = "less")

tool_data1<-as.numeric(as.matrix(study1[( study1$anthro < median(study1$anthro)) & study1$valence,'RESP_5']))
agent_data1<-as.numeric(as.matrix(study1[ (study1$anthro > median(study1$anthro)) &study1$valence ,'RESP_5']))
t.test(tool_data1, agent_data1, alternative = "less")

tool_data1<-as.numeric(as.matrix(study1[( study1$anthro < median(study1$anthro)),'RESP_1']))
agent_data1<-as.numeric(as.matrix(study1[ (study1$anthro > median(study1$anthro)) ,'RESP_1']))
t.test(tool_data1, agent_data1, alternative = "less")





summary(lm(RESP_1 ~ is_agent + valence + is_agent*valence, data=study1)) # 0.20584

summary(lm(RESP_2 ~ is_agent + valence + is_agent*valence, data=study1)) #  0.2293    

summary(lm(RESP_3 ~ is_agent + valence + is_agent*valence, data=study1)) # 0.21724    

summary(lm(RESP_4 ~ is_agent + valence + is_agent*valence, data=study1))  #  0.231    

summary(lm(RESP_5 ~ anthro + valence + anthro*valence, data=study1))  #  0.231    

summary(lm(RESP_5 ~ anthro, data=study1[study1$valence,]))  #  0.231    
summary(lm(RESP_5 ~ anthro , data=study1))  #  0.231    
