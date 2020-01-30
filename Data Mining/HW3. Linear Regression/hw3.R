
library(MASS)
library(ISLR)
fix(Boston)
names(Boston)
lm.fit = lm(medv ~ lstat, data = Boston)
lm.fit; summary(lm.fit)
names(lm.fit)
coef(lm.fit)    # 신뢰계수
confint(lm.fit) # 신뢰구간

predict(lm.fit, data = data.frame(lstat = c(5,10,15)), interval = "confidence")
predict(lm.fit, data = data.frame(lstat = c(5,10,15)), interval = "prediction")

plot(lstat, medv)
abline(lm.fit)
abline(lm.fit, lwd = 3, col = "red")

plot(lstat, lwd = 3, col = "red", pch = "+")
plot(lstat, lwd = 3, col = "red", pch = 20)
plot(lstat, lwd = 3, col = "red", pch = 1:20)

par(mfrow = c(2,1))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# 6.3
lm.fit = lm(medv ~ lstat + age , data= Boston )
summary(lm.fit)
lm.fit = lm(medv ~., data = Boston)
summary(lm.fit)

library(car)
vif(lm.fit)
lm.fit1 = lm(medv ~.-age, data = Boston)
summary(lm.fit1)
lm.fit1 = update(lm.fit ,  ~.-age)

summary(lm(medv ~ lstat*age, data= Boston))
lm.fit2 = lm(medv ~ lstat+I(lstat^2))

lm.fit = lm(medv ~ lstat)
anova(lm.fit, lm.fit2)
par(mfrow = c(2,2))
plot(lm.fit2)

lm.fit5 = lm(medv ~ poly(lstat,5))
summary(lm.fit5)

summary(lm(medv ~ log(rm), data = Boston))

fix(Carseats)
names(Carseats)
lm.fit = lm(Sales ~. +Income:Advertising + Price : Age, data = Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc)


LoadLibraries = function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}

LoadLibraries
function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded")
}

LoadLibraries()




## 8 
#a
options(scipen = 100)
library(ISLR)
library(tidyverse)
data(Auto)
lm.fit= lm(mpg ~ horsepower, data =Auto )
summary(lm.fit)

write.csv( round( summary(lm.fit)$coefficients,3 ) , 
          "C:/Users/jeeyeon/Desktop/데마/HW4/Ex8a.csv")


predict(lm.fit, Auto %>% filter(horsepower == 98))
predict(lm.fit, Auto %>% filter(horsepower == 98), interval = "confidence")
predict(lm.fit, Auto %>% filter(horsepower == 98), interval = "prediction")

#b
par(mfrow = c(1,1))
attach(Auto)
plot(horsepower, mpg)
abline(lm.fit, col = "red")

#c
par(mfrow=c(2,2))
plot(lm.fit)


## 9 
#a
library(psych)
pairs.panels(Auto, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

#b
write.csv( round( cor( select_if(Auto, is.numeric) ),2) , 
           "C:/Users/jeeyeon/Desktop/데마/HW4/Ex9a.csv")

#c

lm.fit = lm(mpg ~. -name, Auto)
summary(lm.fit)

write.csv( round( summary(lm.fit)$coefficients,3 ) , 
           "C:/Users/jeeyeon/Desktop/데마/HW4/Ex9c.csv")

#d
par(mfrow = c(2,2))
plot(lm.fit)


#e
step( lm(mpg ~ displacement*weight*year*origin) ,direction = "both")
?step

lm.fit1 = lm( mpg~displacement+weight+year:origin, 
             data = Auto)

lm.fit2 = lm( mpg~displacement+weight+year*origin, 
              data = Auto)
lm.fit3 = lm( mpg~displacement+origin+year*weight, 
              data = Auto)
lm.fit4 = lm( mpg~origin+weight+year* displacement, 
              data = Auto)
lm.fit5 = lm( mpg~year+weight+origin* displacement, 
              data = Auto)

summary(lm.fit1)
summary(lm.fit2)
summary(lm.fit3)
summary(lm.fit4)
summary(lm.fit5)

write.csv( round( summary(lm.fit3)$coefficients,3 ) , 
           "C:/Users/jeeyeon/Desktop/데마/HW4/Ex9e.csv")

#f
lm.fit1 <- lm(mpg~poly(displacement,3)+weight+year+origin, data=Auto)
lm.fit2 <- lm(mpg~displacement+weight^2+year+origin, data=Auto)
lm.fit3 <- lm(mpg~displacement+log(year)+origin, data=Auto)
summary(lm.fit1)
summary(lm.fit2)
summary(lm.fit3)

write.csv( round( summary(lm.fit1)$coefficients,3 ) , 
           "C:/Users/jeeyeon/Desktop/데마/HW4/Ex9f.csv")


##13
#a
set.seed(1)
x = rnorm(100)

#b
eps = rnorm(100, sd = 0.25^0.5)


#c
y = -1 + 0.5*x + eps
length(y)

#d
par(mfrow= c(1,1))
plot(x,y)

#e
lm.fit = lm(y ~ x)
write.csv( round( summary(lm(y ~ x) )$coefficients,3 ) , 
           "C:/Users/jeeyeon/Desktop/데마/HW4/Ex13e.csv")

#f
plot(x,y)
abline(-1, 0.5, col="blue") 
abline(lm.fit, col="red")   
legend(x = c(0,2.5),y = c(-2.5,-2),
       legend = c("Population", "FittedModel"),
       col = c("blue","red"), lwd=2 )

#g
lm.fit1 <- lm(y~x+x^2)
summary(lm.fit1)
write.csv( round( summary(lm.fit1)$coefficients,3 ) , 
           "C:/Users/jeeyeon/Desktop/데마/HW4/Ex13g1.csv")

anova(lm.fit, lm.fit1)
write.csv( round( anova(lm.fit, lm.fit1),3 ) , 
           "C:/Users/jeeyeon/Desktop/데마/HW4/Ex13g2.csv")

#h
eps2 <- rnorm(100, sd=0.2)  # 0.5 -> 0.2로 감소함 
y2 = -1 + 0.5*x + eps2
lm.fit2 <- lm(y2 ~ x)
summary(lm.fit2)
write.csv( round( summary(lm.fit2)$coefficients,3 ) , 
           "C:/Users/jeeyeon/Desktop/데마/HW4/Ex13h.csv")

plot(x, y2)
abline(-1, 0.5, col="blue") 
abline(lm.fit2, col="red") 
legend(x = c(0,2.5),y = c(-2.3,-1.8),
       legend = c("Population", "FittedModel"),
       col = c("blue","red"), lwd=2 )


#i
eps3 <- rnorm(100, sd=1)  # 0.5 -> 0.2로 감소함 
y3 = -1 + 0.5*x + eps3
lm.fit3 <- lm(y3 ~ x)
summary(lm.fit3)
write.csv( round( summary(lm.fit3)$coefficients,3 ) , 
           "C:/Users/jeeyeon/Desktop/데마/HW4/Ex13i.csv")

plot(x, y3)
abline(-1, 0.5, col="blue") 
abline(lm.fit3, col="red") 
legend(x = c(0,2.5),y = c(-3,-2),
       legend = c("Population", "FittedModel"),
       col = c("blue","red"), lwd=2 )

#j

confint(lm.fit)
confint(lm.fit2)
confint(lm.fit3)

write.csv( round( confint(lm.fit),3 ) , 
           "C:/Users/jeeyeon/Desktop/데마/HW4/Ex13j1.csv")
write.csv( round( confint(lm.fit2),3 ) , 
           "C:/Users/jeeyeon/Desktop/데마/HW4/Ex13j2.csv")
write.csv( round( confint(lm.fit3),3 ) , 
           "C:/Users/jeeyeon/Desktop/데마/HW4/Ex13j3.csv")

## 14
#a
set.seed(1)
x1 <- runif(100)
x2 <- 0.5*x1 + rnorm(100)/10
y <- 2 + 2*x1 + 0.3*x2 + rnorm(100)

#b
cor(x1,x2)
plot(x1,x2)

#c
fit.lm <- lm(y~x1+x2)
summary(fit.lm)

write.csv( round( summary(fit.lm)$coefficients,3 ) , 
           "C:/Users/jeeyeon/Desktop/데마/HW4/Ex14c.csv")

#d
fit.lm1 <- lm(y~x1)
summary(fit.lm1)

write.csv( round( summary(fit.lm1)$coefficients,3 ) , 
           "C:/Users/jeeyeon/Desktop/데마/HW4/Ex14d.csv")


#e
fit.lm2 <- lm(y~x2)
summary(fit.lm2)

write.csv( round( summary(fit.lm2)$coefficients,3 ) , 
           "C:/Users/jeeyeon/Desktop/데마/HW4/Ex14e.csv")


#g
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)
par(mfrow=c(2,2))

# regression with both x1 and x2
fit.lm <- lm(y~x1+x2)
summary(fit.lm)
plot(fit.lm)

# regression with x1 only
fit.lm1 <- lm(y~x2)
summary(fit.lm1)
plot(fit.lm1)

# regression with x2 only
fit.lm2 <- lm(y~x1)
summary(fit.lm2)
plot(fit.lm2)














