## 1. DATA
dat<-read.csv("bwght2.csv", header=T)###Reading data
head(dat)

data<-na.omit(dat)###omitting missing observations



### 2.creating treatment varibale
trt<-c()
for (i in 1:length(data[,"cigs"])){
if (data[,"cigs"][i]>0)
{trt[i]=1}
else {trt[i]=0}
}

###adding the new variable in data
data$trt<-NA
data[,"trt"]<-trt

### 3. finding out the difference
smok<-c()
n<-length(data[,"cigs"])
for (i in 1:n){
if (data[,"trt"][i]==1){smok[i]=data[,"lbwght"][i]}
else {smok[i]=0}
}

nsmok<-c()
for (i in 1:n){
if (data[,"trt"][i]==0){nsmok[i]=data[,"lbwght"][i]}
else {nsmok[i]=0}
}

diff<-mean(smok)-mean(nsmok)

### 4. running regression
attach(data)
mod<-lm(lbwght~trt+magesq+meduc+monpre+npvis+fage+feduc+fblck+magesq+npvissq+mblck)
summary(mod)

### 5. installing MatchIt package 
install.packages("MatchIt")
library("MatchIt")

###Matching with propensity score
mod1 <- matchit(trt~lbwght+magesq+meduc+monpre+npvis+fage+feduc+fblck+magesq+npvissq+mblck, 
              data = data, method = "nearest", distance = "logit")
summary(mod1)
m.dat<-match.data(mod1,distance="pscore")##new data set based on matching
m.dat.c<-match.data(mod1,"control",distance="pscore")##matched data set excluding treatment group
m.dat.t<-match.data(mod1,"treat",distance="pscore")##matched data set excluding control group

## 6. treatment effect on the treated
tt<-mean(m.dat.t$lbwght-m.dat.c$lbwght)

## 7(a+b). Logit model
mod2<-glm(trt~lbwght+magesq+meduc+monpre+npvis+fage+feduc+fblck+magesq+npvissq+mblck, 
              data = data,family=binomial)
prop.sc<-predict(mod2)##returns propensity score based on logit model

## 7(c). conditional distribution of propensity score

prop.sc.trt<-c()
for (i in 1:length(prop.sc)){
if(trt[i]==1)prop.sc.trt[i]<-prop.sc[i]
}
prop.sc.trt<-na.omit(prop.sc.trt)

prop.sc.utrt<-c()
for (i in 1:length(prop.sc)){
if(trt[i]==1)prop.sc.utrt[i]<-prop.sc[i]
}
prop.sc.utrt<-na.omit(prop.sc.utrt)

par(mfrow=c(1,2))
hist(prop.sc.trt, main="Propensity dist of Treated")
hist(prop.sc.utrt,main="Propensity dist of Untreated")

##########

##matching with propensity score (My codes)
y0<-c()
for (i in 1:length(prop.sc.trt)){
s=0
k=0
for (j in 1:length(prop.sc.utrt)){
if (abs(prop.sc.trt[i]-prop.sc.utrt[j])<=0.1){s=s+data$lbwght[j]
k=k+1}
}
y0[i]<-s/k
}

y1<-c()
for (i in 1:dim(data)[1]){
if (data$trt[i]==1)y1[i]<-data$lbwght[i]
}
y1<-na.omit(y1)

tt.est<-0
for (i in 1:length(y1)){
z<-(y1[i]/-y0[i])/length(y1)
tt.est<-tt.est+z
}