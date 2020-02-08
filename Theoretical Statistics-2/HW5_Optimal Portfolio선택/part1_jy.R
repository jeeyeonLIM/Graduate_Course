options("scipen" = 100)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(MASS)
library(jmuOutlier)
library(ggpmisc)
library(car)

QQ_Plot_fun <- function(data, var){
  n <- length(var)
  data = data %>% mutate(u = rank(var)/(1+n),
                         qnorm = qnorm(u), qlogistic = log(u/(1-u)),qlaplace = qlaplace(u), 
                         qt1 = qt(u,1),qt2 = qt(u,2),qt3 = qt(u,3),qt4 = qt(u,4),qt5 = qt(u,5),
                         qt6 = qt(u,6),qt7 = qt(u,7),qt8 = qt(u,8),qt9 = qt(u,9),qt10 = qt(u,10))
  g1 <- ggplot(data, aes(x = var, y = qnorm)) + geom_point() +geom_smooth(method="lm",se=F)+
    ggtitle(paste("Normal", "R^2 = ",
                  round(summary(lm(qnorm ~ var, data))$adj.r.squared,4)*100 ,"%", sep=" "))
  g2 <- ggplot(data, aes(x = var, y = qlogistic)) + geom_point() +geom_smooth(method="lm",se=F)+
    ggtitle(paste("Logistic", "R^2 = ",
                  round(summary(lm(qlogistic ~ var, data))$adj.r.squared,4)*100 ,"%", sep=" "))
  g3 <- ggplot(data, aes(x = var, y = qlaplace)) + geom_point() +geom_smooth(method="lm",se=F)+
    ggtitle(paste("Laplace", "R^2 = ",
                  round(summary(lm(qlaplace ~ var, data))$adj.r.squared,4)*100 , sep=" "))
  t1 <- ggplot(data, aes(x = var, y = qt1)) + geom_point() +geom_smooth(method="lm",se=F)+
    ggtitle(paste("t(1)", "R^2 = ",
                  round(summary(lm(qt1 ~ var, data))$adj.r.squared,4)*100 , sep=" "))
  t2 <- ggplot(data, aes(x = var, y = qt2)) + geom_point() +geom_smooth(method="lm",se=F)+
    ggtitle(paste("t(2)", "R^2 = ",
                  round(summary(lm(qt2 ~ var, data))$adj.r.squared,4)*100, sep=" "))
  t3 <- ggplot(data, aes(x = var, y = qt3)) + geom_point() +geom_smooth(method="lm",se=F)+
    ggtitle(paste("t(3)", "R^2 = ",
                  round(summary(lm(qt3 ~ var, data))$adj.r.squared,4)*100 , sep=" "))
  t4 <- ggplot(data, aes(x = var, y = qt4)) + geom_point() +geom_smooth(method="lm",se=F)+
    ggtitle(paste("t(4)", "R^2 = ",
                  round(summary(lm(qt4 ~ var, data))$adj.r.squared,4)*100 , sep=" "))
  t5 <- ggplot(data, aes(x = var, y = qt5)) + geom_point() +geom_smooth(method="lm",se=F)+
    ggtitle(paste("t(5)", "R^2 = ",
                  round(summary(lm(qt5 ~ var, data))$adj.r.squared,4)*100 , sep=" "))
  t6 <- ggplot(data, aes(x = var, y = qt6)) + geom_point() +geom_smooth(method="lm",se=F)+
    ggtitle(paste("t(6)", "R^2 = ",
                  round(summary(lm(qt6 ~ var, data))$adj.r.squared,4)*100 , sep=" "))
  t7 <- ggplot(data, aes(x = var, y = qt7)) + geom_point() +geom_smooth(method="lm",se=F)+
    ggtitle(paste("t(7)", "R^2 = ",
                  round(summary(lm(qt7 ~ var, data))$adj.r.squared,4)*100 , sep=" "))
  t8 <- ggplot(data, aes(x = var, y = qt8)) + geom_point() +geom_smooth(method="lm",se=F)+
    ggtitle(paste("t(8)", "R^2 = ",
                  round(summary(lm(qt8 ~ var, data))$adj.r.squared,4)*100 , sep=" "))
  t9 <- ggplot(data, aes(x = var, y = qt9)) + geom_point() +geom_smooth(method="lm",se=F)+
    ggtitle(paste("t(9)", "R^2 = ",
                  round(summary(lm(qt9 ~ var, data))$adj.r.squared,4)*100 , sep=" "))
  t10 <- ggplot(data, aes(x = var, y = qt10)) + geom_point() +geom_smooth(method="lm",se=F)+
    ggtitle(paste("t(10)", "R^2 = ",
                  round(summary(lm(qt10 ~ var, data))$adj.r.squared,4)*100 , sep=" "))
  grid.arrange(g1,g2,g3,ncol = 3)
  grid.arrange(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10, nrow = 2, ncol=5)
}

interest = read.csv("C:/Users/jeeyeon/Desktop/data/4.1.2 시장금리(월_분기_년).csv", stringsAsFactors = F)
interest = interest[-1:-3,]; names(interest) = c("date", "interest") 
interest = interest %>% transmute(date=as.numeric(gsub("/", "", date)), interest=as.numeric(interest))
interest = interest[c(order(interest$date)),]

hd = read.csv("C:/Users/jeeyeon/Desktop/data/hyundai.csv", header = T, stringsAsFactor = F)
hd = hd[,c(1,2,3)];  colnames(hd) = c("date", "hd_lower","hd_upper") 
hd = hd %>% transmute(date =as.numeric(gsub("/", "", date)), P1 = (as.numeric(gsub(",", "", hd_lower))+ as.numeric(gsub(",", "", hd_upper)))/2)
hd = hd[c(order(hd$date)),]

kt = read.csv("C:/Users/jeeyeon/Desktop/data/kt.csv", header = T, stringsAsFactor = F)
kt = kt[,c(1,2,3)]; colnames(kt) = c("date", "kt_lower", "kt_upper")  
kt = kt %>% transmute(date = as.numeric(gsub("/", "", date)), P2 = (as.numeric(gsub(",", "", kt_lower))+ as.numeric(gsub(",", "", kt_upper)))/2)
kt = kt[c(order(kt$date)),]

gas = read.csv("C:/Users/jeeyeon/Desktop/data/gas.csv", header = T, stringsAsFactor = F)
gas = gas[,c(1,2,3)]; colnames(gas) = c("date", "gas_lower", "gas_upper")  
gas = gas %>% transmute(date = as.numeric(gsub("/", "", date)), P2 = (as.numeric(gsub(",", "", gas_lower))+ as.numeric(gsub(",", "", gas_upper)))/2)
gas = gas[c(order(gas$date)),]

total = hd %>% left_join(kt, by = "date") %>% left_join(interest, by="date") %>%
               transmute(date = as.numeric(gsub("/", "", date)), P1 = P1, P2 = P2,interest = interest )  %>% 
               mutate(r1 = (P1-lag(P1))/lag(P1), r2 = (P2-lag(P2))/lag(P2), year = substr(date, 1, 4))
total = total[complete.cases(total), ];  n <- length(total$date)
total = total[c(order(total$date)),]
total = total %>% mutate(t = c(0:(n-1)))


### 1 ----------------------------------------------------------------------------------------------------------

t1 <- ggplot(total) + geom_line(aes(x = t, y = P1), colour = "darkblue", size = 1 )+ggtitle("Hyundai Time Series")
t2 <- ggplot(total) + geom_line(aes(x = t, y = P2), colour = "pink", size = 1 )+ggtitle("KT&G Time Series")
grid.arrange(t1,t2, ncol= 2)

t1 <- ggplot(total) + geom_line(aes(x = t, y = log(P1)), colour = "darkblue", size = 1 )+ggtitle("log: Hyundai Time Series")
t2 <- ggplot(total) + geom_line(aes(x = t, y = log(P2)), colour = "pink", size = 1 )+ggtitle("log: KT&G Time Series")
grid.arrange(t1,t2, ncol= 2)

t1 <- ggplot(total) + geom_line(aes(x = t, y = P1), colour = "darkblue", size = 1 ) + 
      geom_line(aes(x = t, y = P2), colour = "pink", size = 1) +ggtitle("Pt graph")
t2 <- ggplot(total) + geom_line(aes(x = t, y = log(P1)), colour = "darkblue", size = 1 ) + 
      geom_line(aes(x = t, y = log(P2)), colour = "pink", size = 1)+ggtitle("log(Pt) graph")
grid.arrange(t1,t2, ncol=2)


### 2 ----------------------------------------------------------------------------------------------------------

#시계열도표 
ggplot(total) + geom_line(aes(x = t, y = r1), colour = "darkblue") + ggtitle("Hyundai r : Time Series") + theme_bw()
ggplot(total) + geom_line(aes(x = t, y = r2), colour = "darkblue") + ggtitle("KT&G r : Time Series")    + theme_bw()

#히스토그램
ggplot(total) +  geom_histogram(aes(r1)) + ggtitle("Hyundai r : Histogram") + theme_bw()
ggplot(total) +  geom_histogram(aes(r2)) + ggtitle("KT&G r : Histogram")    + theme_bw()


QQ_Plot_fun(total, total$r1)
QQ_Plot_fun(total, total$r2)

acf(total$r1)# 독립이아니다
pacf(total$r1)
fit <- lm(r1 ~ t, total); durbinWatsonTest(fit, max.lag=20) 

acf(total$r2)# 독립이아니다
pacf(total$r2)
fit <- lm(r2 ~ t, total); durbinWatsonTest(fit, max.lag=20) 


### 3 ----------------------------------------------------------------------------------------------------------

learning_data <- filter(total, total$year <2013 )
test_data <- filter(total, total$year >= 2013 )

stat_table <- data.frame( "learn_r1" = c("mean" = mean(learning_data$r1), "var" = var(learning_data$r1)), 
                         "learn_r2" = c("mean" = mean(learning_data$r2), "var" = var(learning_data$r2)),
                          "test_r1" = c("mean" = mean(test_data$r1), "var" = var(test_data$r1)), 
                          "test_r2" = c("mean" =mean(test_data$r2), "var" = var(test_data$r2)))
stat_table
cov(learning_data[,c(5,6)]) # learning data 에서 현대차와 kt&g 의 공분산
cov(test_data[,c(5,6)])     # test data 에서 현대차와 kt&g 의 공분산


cov(learning_data[,5], test_data[-1,5]) #현대차
cov(learning_data[,6], test_data[-1,6]) #kt&g



### 4 -----------------------------------------------------------------------------------------------------------


Vt_fun = function(data, th0,th1,th2){
  V0 = 1
  n = length(data$date)
  table = data %>% mutate(rp = th0 * ((1+interest/100)^(1/12)-1) + th1*r1 +th2*r2 )
  #table$Vt[1] = V0 * (1+table$rp[1])
  table$Vt[1] = V0
  for(i in 2:n){table$Vt[i] = table$Vt[i-1] * (1+table$rp[i])}
  return(data.frame("t" = table$t,"Vt" = table$Vt))
}


P1_0 = test_data$P1[1]; P2_0 = test_data$P2[1]

# Vt_star_fun = function(data, th0,th1,th2){
#   n = length(data$date)
#   table = data %>% mutate(r1_star = (P1-P1_0)/P1_0, r2_star = (P2-P2_0)/P2_0)
#   i_12 <- ((1+table$interest/100)^(1/12)) - 1
#   # for(i in 1:n){ table$r0_star[i] = ( ( 1+table$interest[i]) ^ (1/12) ) - 1 }
#   for(i in 1:n){table$r0_star[i] <- prod((1+table$i_12[1:i]))-1}
#   table = table %>% mutate(Vt_star =V0 *(1 + th0*r0_star + th1*r1_star + th2*r2_star))
#   return(table)
#   #return(data.frame("t" = table$t,"Vt_star" = table$Vt_star))
# }



Vt_star_fun <- function(data, th0, th1, th2){
  n <- length(data$date)
  table <- data 
  for(i in 1:n){table$r1_star[i] <- (table$P1[i]-P1_0)/P1_0}
  for(i in 1:n){table$r2_star[i] <- (table$P2[i]-P2_0)/P2_0}
  i_12 <- ((1+table$interest/100)^(1/12)) - 1
  table$r0_star[1] <- (1+i_12[1])-1
  for(i in 2:n){table$r0_star[i] <- prod((1+i_12[1:i]))-1}
  table <- table %>% mutate(Vt_star =V0 *(1 + th0*r0_star + th1*r1_star + th2*r2_star))
  return(data.frame("t" = table$t,"Vt_star" = table$Vt_star))
}



Port0 = Vt_fun(test_data,1,0,0)
Port1 =Vt_fun(test_data,0,1,0)
Port2 =Vt_fun(test_data,0,0,1)
Port3 =Vt_fun(test_data,1/3,1/3,1/3)
Port4 =Vt_fun(test_data,0,1/2,1/2)

Port_star2 = Vt_star_fun(test_data,0,0,1)
Port_star3 = Vt_star_fun(test_data,1/3,1/3,1/3)
Port_star4 =Vt_star_fun(test_data,0,1/2,1/2)

Vtgraph = data.frame("t" =test_data$t, "Vt0" = Port0[2], "Vt1" = Port1[2], 
                     "Vt2" = Port2[2], "Vt3" = Port3[2], "Vt4" = Port4[2],
                     "Vt_star2" = Port_star2[2], "Vt_star3" = Port_star3[2], "Vt_star4" = Port_star4[2])
colnames(Vtgraph) = c("t","Vt0","Vt1","Vt2","Vt3","Vt4","Vt_star2" ,"Vt_star3" , "Vt_star4" )

### 4 --------------------------------------------------------------------------------------------------------

ggplot(Vtgraph) + geom_line(aes(x = t, y = Vt0)) + geom_line(aes(x = t, y = Vt1),colour = "pink", size =1, show.legend = T)+
                  geom_line(aes(x = t, y = Vt2),colour = "red", size =1) +geom_line(aes(x = t, y = Vt3),colour = "blue", size =1)+
                  theme_test()

### 5 --------------------------------------------------------------------------------------------------------

# P2
ggplot(Vtgraph) + geom_line(aes(x = t, y = Vt2))+ geom_line(aes(x = t, y = Vt_star2), colour="red")+theme_test()
# P3
p3 = ggplot(Vtgraph) + geom_line(aes(x = t, y = Vt3))+ geom_line(aes(x = t, y = Vt_star3), colour="red")+theme_test()+
  ggtitle("P3")
# P4
p4 = ggplot(Vtgraph) + geom_line(aes(x = t, y = Vt4))+ geom_line(aes(x = t, y = Vt_star4), colour="red")+theme_test()+
  ggtitle("P4")

grid.arrange(p3,p4, ncol = 2)



