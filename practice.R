library(readxl)
library(tidyverse)
rbc<-read_xls("rbc.xls",skip=1)
time<-c(1:249)
rbc<-cbind(rbc,time)
#rbc01<-rbc[,-rbc$time.1]
ggplot(rbc,aes(x=time,y=c)) + geom_line(color="red")  +
  geom_line(aes(y=y),color="blue")  + geom_line(aes(y=i))

logy<-log(rbc$y)
logc<-log(rbc$c)
logi<-log(rbc$i)
logh<-log(rbc$h)

rbc<-cbind(rbc,logy,logc,logi,logh)
# rbc<-rbc[,-c(13)]
ggplot(rbc,aes(x=time,y=logy)) + geom_line(color="red") + 
  geom_line(aes(y=logc),color="blue")  + geom_line(aes(y=logi),color="green") +
  geom_line(aes(y=logh))


A= matrix(c(1,2,3,4),2,2)
EV=eigen(A)$vector
Evl=eigen(A)$value
A%*%EV[,1]/Evl[1]

#--------------------------------------
require(ggplot2)
require(GGally)
require(CCA)
mm <- read.csv("https://stats.idre.ucla.edu/stat/data/mmreg.csv")
colnames(mm) <- c("Control", "Concept", "Motivation", "Read", "Write", "Math", 
                  "Science", "Sex")
summary(mm)
xtabs(~Sex, data = mm)
psych <- mm[, 1:3]
acad <- mm[, 4:8]
ggpairs(psych)
ggpairs(acad)

ts.plot(acad[,1])
# same with plot(acad[,1],type='l')
hist(density(as.numeric(acad[,1])))
class(acad[,1])
hist(acad[,1],breaks = seq(range(acad[,1])[1],range(acad[,1])[1],by=.01))

# correlations
matcor(psych, acad)

cc1 <- cc(psych, acad)

# display the canonical correlations
cc1$co

# raw canonical coefficients
cc1[3:4]

# compute canonical loadings
cc2 <- comput(psych, acad, cc1)

# display canonical loadings
cc2[3:6]