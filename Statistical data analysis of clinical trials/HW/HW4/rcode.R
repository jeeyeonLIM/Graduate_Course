library(tidyr);library(dplyr);library(nlme);library(car);library(MASS); library(ggplot2)

dog = read.csv("C:/Users/jeeyeon/Desktop/data/sleeping-dog.csv")
colnames(dog) = c("Dog","HO","LO","HI","LI")
dog = dog %>% mutate(Dog = as.factor(Dog))

dog_gather = dog %>% gather(trt, heartbeat ,HO,LO,HI,LI)
dog_gather = dog_gather %>% mutate(CO2 = ifelse(substr(trt,1,1)=="H","High","Low" ),
                                   H = ifelse(substr(trt,2,2)=="O","Out","In")) 
#dog_gather = dog_gather %>% select(trt)  #나는 왜 select를 쓸수없는가?

# Graph
ggplot(dog_gather, aes(y=heartbeat, x= trt)) + geom_boxplot(stat = "boxplot")
ggplot(dog_gather,aes(x=H, y=heartbeat, colour = Dog, group = Dog)) + geom_line(size = 1) +geom_point() + facet_wrap(~CO2) 
ggplot(dog_gather,aes(x=CO2, y=heartbeat, colour = Dog, group = Dog)) + geom_line(size = 1) +geom_point() + facet_wrap(~H) 
ggplot(dog_gather,aes(x=H, y=heartbeat, colour = CO2, group = CO2)) + geom_line() +geom_point()+ facet_wrap(~Dog) 
ggplot(dog_gather,aes(x=CO2, y=heartbeat, colour = H, group = H)) + geom_line() +geom_point()+facet_wrap(~Dog) 

# ANOVA
model1 = lme(heartbeat ~ CO2* H ,random= ~1|Dog, data= dog_gather)
anova(model1)
summary(model1)

# MANOVA
model2 = lm( cbind(HO,LO,HI,LI) ~ 1 ,data = dog )
H = factor(c("Out","In"))
CO2 = factor(c("High","Low"))
measure_time = data.frame(H = factor(rep(H,each = 2)), CO2 = factor(rep(CO2, 2)))
mv1 = Anova(model2, idata = measure_time, idesign = ~H*CO2)
summary(mv1)






### 과제 solution !

library(ICSNP)
library(car)

dog = read.csv("C:/Users/jeeyeon/Desktop/data/sleeping-dog.csv")  # "HO","LO","HI","LI"

C1 = dog$TRT3 + dog$TRT4 - dog$TRT1 - dog$TRT2  # H effect    (IN, out)
C2 = dog$TRT1 + dog$TRT3 - dog$TRT2 - dog$TRT4  # CO2 effect  (HIGH, LOW)
C3 = dog$TRT1 + dog$TRT4 - dog$TRT2 - dog$TRT3  # CO2 * H interaction

C.total = data.frame(C1 = C1, C2 = C2, C3 = C3)  


## t.test 

    HotellingsT2(C.total)  # 모두 다 0인지를 봄
    
    t.test(C1)  # H effect
    t.test(C2)  # CO2 effect
    t.test(C3)  # H*CO2 effect


## MANOVA 

    dog.lm = lm( cbind(TRT1,TRT2,TRT3,TRT4) ~ . ,data = dog )
    trt.data = data.frame( H = c("Out", "Out", "In", "In"),
                           CO2 = c("H", "L", "H", "L"))
    summary(Anova(dog.lm, idata = trt.data, idesign = ~ H + CO2 + H*CO2 ))
    




