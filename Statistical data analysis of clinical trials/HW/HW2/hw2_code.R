library(nlme)
library(faraway)
library(ggplot2)
library(gridExtra)
library(gmodels)
library(car)
library(reshape)
library(dplyr)
options("scipen" = 100)

write.csv(BodyWeight, file = "C:/Users/jeeyeon/Desktop/data/BodyWeight.csv") 
write.csv(vision, file = "C:/Users/jeeyeon/Desktop/data/vision.csv") 


### 1 ----------------------------------------------------

#1 적절한 그림으로 자료를살펴보시오 

# weight : numeric vector   giving the body weight of the rat
# time : numeric vector   giving time at which the measurement is made
# Rat : ordered factor   
# Diet : a factor level 1 to 3   

# Trt - Diet , Rat - Patient , Time - time, Weight 
# 1일 ~ 7 일까지 64 일 동안 측정 된 쥐의 체중 (44 일에는 추가 측정)
# 세 그룹의 쥐를 각각 다른식이 요법

str(BodyWeight)

theme_set(theme_bw())
ggplot(BodyWeight) + geom_line(aes(x = Time, y = weight, colour = Diet, group = Rat))


#2 적절한 방법으로 분석하여 결과를 해석하시오

# lm1 <- lm(weight ~  factor(Diet) + factor(Rat) + factor(Time) + factor(Diet) * factor(Time),data = BodyWeight )
# anova(lm1)

# Anova
lm1 <- lm(weight ~ factor(Diet) + factor(Rat) + factor(Time) +factor(Diet) * factor(Time), data = BodyWeight )
temp <- anova(lm1); temp
F.group <- temp[1,3] / temp[2,3] # F.value
P.group <- 1-pf(F.group,temp[1,1], temp[2,1])
c(F.group, P.group)

lme1 <- lme(weight ~ factor(Diet) + factor(Time) +factor(Diet) * factor(Time), random = ~1|factor(Rat),data = BodyWeight )
anova(lme1)


# Manova
head(BodyWeight)
BodyWeight <- BodyWeight %>% mutate(Week = paste("W",BodyWeight$Time, sep=""))
BodyWeight_M <- cast(BodyWeight , Diet + Rat ~ Week, value="weight")

lm2 <- lm( cbind(W1,W8,W15,W22,W29,W36,W43,W44,W50,W57,W64) ~ Diet ,data = BodyWeight_M )
measure_time <- factor( c("W1","W8","W15","W22","W29","W36","W43","W44","W50","W57","W64") )
measure_time_data <- data.frame(measure_time = measure_time)
mv1 <- Anova(lm2, idata = measure_time_data,idesign = ~ measure_time)
summary(mv1)






### 2 ----------------------------------------------------

#1 적절한 그림으로 자료를살펴보시오 

# acuity : a numeric vector (예민함 속도)
# power : a factor with levels 6/6 6/18 6/36 6/60   -> power of lens의미함 :  / 물체의거리
# eye : a factor with levels : left right
# subject : a factor with levels 1 2 3 4 5 6 7
# 7명의 피실험자가 시력검사로 예민함 측정power: 

vision
str(vision)


# left , right eye    -------------------------------------------
theme_set(theme_bw())
g1 <- ggplot(vision[vision$eye=='left',], aes(x=power,y=acuity))+
  geom_line(aes(color=subject,group=subject))+ggtitle('Left eye')
g2 <- ggplot(vision[vision$eye=='right',], aes(x=power,y=acuity))+
  geom_line(aes(color=subject,group=subject))+ggtitle('Right eye')
grid.arrange(g1,g2,ncol=2)


# power별그래프 따로그리는데 x=eye y=acuity! --------------------
g1<-ggplot(vision[vision$power=='6/6',],aes(eye,acuity))+
  geom_line(aes(color=subject,group=subject))+
  ggtitle('Power 6/6')+ ylim(90,130)
g2<-ggplot(vision[vision$power=='6/18',],aes(eye,acuity))+
  geom_line(aes(color=subject,group=subject))+
  ggtitle('Power 6/18')+ ylim(90,130)
g3<-ggplot(vision[vision$power=='6/36',],aes(eye,acuity))+
  geom_line(aes(color=subject,group=subject))+
  ggtitle('Power 6/36')+ ylim(90,130)
g4<-ggplot(vision[vision$power=='6/60',],aes(eye,acuity))+
  geom_line(aes(color=subject,group=subject))+
  ggtitle('Power 6/60')+ ylim(90,130)
grid.arrange(g1,g2,ncol=2)
grid.arrange(g3,g4,ncol=2)



#2 적절한 방법으로 분석하여 결과를 해석하시오

# Anova
lm1 <- lm(acuity ~ factor(power) + factor(subject) + factor(eye) +factor(power) * factor(eye), data = vision )
temp <- anova(lm1); temp
F.group <- temp[1,3] / temp[2,3] # F.value
P.group <- 1-pf(F.group,temp[1,1], temp[2,1])
c(F.group, P.group)

lme2 <- lme(acuity ~ factor(power) + factor(eye) + factor(power) * factor(eye), random = ~1|factor(subject),data = vision )
#   lme(acuity ~ factor(power), random = ~1|subject/eye , data = vision )  이렇게하는거 아닌가 ㅠㅠ
anova(lme2)



# Manova
vision_M <- cast(vision , power + subject ~ eye, value="acuity")

lm2 <- lm( cbind(left, right) ~ power ,data = vision_M )
measure_time <- factor( c("left","right") )
measure_time_data <- data.frame(measure_time = measure_time)
mv2 <- Anova(lm2, idata = measure_time_data,idesign = ~ measure_time)
summary(mv2)



