Untitled
================

\`\`\`{r setup, include=FALSE}

\`\`\`

Including Code
--------------

``` {r}

# https://github.com/yahwes/ISLR/blob/master/ch02soln.Rmd


### 2.3 Lab : Introduction to R


# Basic Commands
    x = c(1,3,2,5);x
    x = c(1,6,2);x
    y = c(1,4,3)
    length(x)
    length(y)
    x+y
    
    ls() # look at a list of all of the objects
    rm(x,y) # delete 
    ls() 
    rm(list = ls()) # 모든 object 삭제하라
    
    ?matrix
    
    x = matrix( data = c(1,2,3,4), nrow = 2, ncol = 2);x
    x = matrix( c(1,2,3,4), 2,2, byrow = TRUE);x
    
    sqrt(x)
    x^2
    
    x = rnorm(50)
    y = x + rnorm(50, mean = 50, sd = 1)
    cor(x,y)
    
    set.seed(1303)
    rnorm(50)
    
    set.seed(3)
    y = rnorm(100)
    mean(y)
    var(y)
    sqrt(var(y))
    sd(y)

# Graphics
    
    x = rnorm(100)
    y = rnorm(100)
    plot(x,y)
    plot(x,y, xlab = "this is x-axis", ylab = "this is y-axis", main = "Plot of X vs Y")
    
    pdf("Figure.pdf") # Create the pdf file
    plot(x,y, col = "green")
    dev.off() # we are done 
    
    x = seq(1,10); x
    x = 1:10; x
    
    x = seq(-pi, pi, length = 50); x
    
    ## use contour
    y = x
    f = outer(x,y, function(x,y) cos(y)/(1 + x^2) )
    ?contour
    contour(x,y,f) # create contour plot , add contour line 
    contour(x,y,f, nlevels = 45, add = T)
    
    fa = (f - t(f))/2    # t(f) : transpose of f
    contour(x,y,fa, nlevels = 15)
    
    ## use image
    image(x,y,fa) # heatmap
    persp(x,y,fa) # 3-dim plot
    persp(x,y,fa, theta = 30)  
    persp(x,y,fa, theta = 30, phi=20)
    persp(x,y,fa, theta = 30, phi=70)
    persp(x,y,fa, theta = 30, phi=40)
    
# Indexing data 
    A = matrix(1:16,4,4); A
    A[2,3]    
    A[c(1,3),c(2,4)]  #(1,2) (1,4) (3,2) (3,4) 이렇게 원소가 뽑힘 
    A[1:3,2:4]  
    A[1:2,]  
    A[,1:2]    
    A[1,]
    A[-c(1,3),]        
    dim(A)    
    
# Loading Data 
    
    #install.packages("ISLR")
    library(ISLR)
    
    data = na.omit(Auto) 
    fix(Auto)
    names(data)
    dim(data)
    
    # boxplot
    attach(data)
    plot(cylinders, mpg)
    cylinders = as.factor(cylinders)
    plot(cylinders, mpg)
    plot(cylinders, mpg, col = "Red")
    plot(cylinders, mpg, col = "Red",varwidth = T)
    plot(cylinders, mpg, col = "Red",varwidth = T,horizontal=T)
    plot(cylinders, mpg, col = "Red",varwidth = T,horizontal=T, xlab= "cylinders", ylab = "MPG")
    detach(data) 
  
    library(ggplot2)
    ggplot(data, aes(factor(cylinders), mpg)) + geom_boxplot(fill ="Red",varwidth = TRUE,horizontal=T) + 
      theme_bw()+ coord_flip() +xlab("cylinders") + ylab("MPG")
    
    # histogram 
    hist(mpg)
    hist(mpg, col =2)
    hist(mpg, col =2, breaks=15)
    ggplot(data, aes(mpg)) + geom_histogram(color = "black",fill = "Red",bins = 15) + theme_bw()
    
    # pairs 
    pairs(data)
    pairs(~mpg + displacement + horsepower + weight + acceleration, data)
    
    plot(horsepower, mpg)
    ggplot(data, aes(horsepower, mpg)) + geom_point() + theme_bw()
    #identify(horsepower, mpg, name)
    #?identify
    
    summary(data)
    summary(mpg)
    ```
    ```{r}
### 9번문제
    
    #a
    str(Auto)
    write.csv(str(Auto), "C:/Users/jeeyeon/Desktop/데마/HW3/Ex9a.csv")
    ?write.csv
    
    #b #c
    library(dplyr)
    mynum = select_if(Auto, is.numeric)
    summary(mynum)
    
    b = data.frame( apply(mynum, 2, range))
    b[3,] = b[2,] - b[1,]
    #write.csv( b, "C:/Users/jeeyeon/Desktop/데마/HW3/Ex9b.csv")

    c1 = apply(mynum, 2, mean)
    c2 = apply(mynum, 2, sd)
    
    write.csv(round( rbind(b,c1,c2),2), 
              "C:/Users/jeeyeon/Desktop/데마/HW3/Ex9bc.csv")
    
    #d
    Auto 
    fix(Auto)
    quantile(Auto$mpg)
    ?quantile
    
    # create temp matrix for numeric columns
    tmp = mynum[-(10:85),]  # drop rows
    
    d = data.frame( apply(tmp, 2, range))
    d[3,] = d[2,] - d[1,]
    
    t1 = apply(tmp, 2, mean)
    t2 = apply(tmp, 2, sd)
    
    write.csv(round( rbind(d,t1,t2),2), 
              "C:/Users/jeeyeon/Desktop/데마/HW3/Ex9d.csv")
    
    #e
    pairs(mynum)
    
    library(psych)
    pairs.panels(mynum, 
                 method = "pearson", # correlation method
                 hist.col = "#00AFBB",
                 density = TRUE,  # show density plots
                 ellipses = TRUE # show correlation ellipses
    )
    
### 10번문제 
    library(tidyverse)
    library(MASS)
    Boston    
    ?Boston    
    summary(Boston)
    
    #a
    dim(Boston)
    #b
    pairs(Boston)
  
    pairs.panels(Boston, 
                 method = "pearson", # correlation method
                 hist.col = "#00AFBB",
                 density = TRUE,  # show density plots
                 ellipses = TRUE # show correlation ellipses
    )
      
    names(Boston)
    
    #c
    cor(Boston)[1,]
    ggplot(Boston, aes(x = crim, y = zn)) + geom_point() +theme_bw()
    ggplot(Boston, aes(x = crim, y = rad)) + geom_point() +theme_bw()
    ggplot(Boston, aes(x = crim, y = tax)) + geom_point() +theme_bw()
    ggplot(Boston, aes(x = crim, y = black)) + geom_point() +theme_bw()
    
    #d
    ggplot(Boston, aes(x=1:nrow(Boston), y=crim)) + geom_point()+theme_bw()
    ggplot(Boston, aes(x=1:nrow(Boston), y=tax)) + geom_point()+theme_bw()
    ggplot(Boston, aes(x=1:nrow(Boston), y=ptratio))+ geom_point()+theme_bw()
    
    #e
    sum(Boston$chas)
    
    #f
    median(Boston$ptratio)
    
    #g
    apply(Boston, 2, range)
    
    b = data.frame( apply(Boston, 2, range))
    b[3,] = b[2,] - b[1,]
    
    c1 = apply(Boston, 2, mean)
    c2 = apply(Boston, 2, sd)
    
    write.csv(round( rbind(b,c1,c2),0), 
              "C:/Users/jeeyeon/Desktop/데마/HW3/Ex10.csv")
    
    
    Boston %>% filter(medv == min(medv))
    write.csv(round( Boston %>% filter(medv == min(medv)),0), 
              "C:/Users/jeeyeon/Desktop/데마/HW3/Ex10g.csv")
    apply(Boston %>% filter(medv == min(medv)), 2, range)
    
    #h
    
    Boston %>% filter(rm > 7)
    nrow(Boston %>% filter(rm > 7) )
    Boston %>% filter(rm > 8)
    nrow(Boston %>% filter(rm > 8) )
    
    write.csv(round( Boston %>% filter(rm > 8) ,0), 
              "C:/Users/jeeyeon/Desktop/데마/HW3/Ex10h.csv")
    
```
