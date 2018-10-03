##Problem-1
library(plm)
data(Gasoline)
dat<-Gasoline
head(dat)
data<-dat[,c("country","year","lgaspcar","lrpmg")]
mod<-plm(data[,"lgaspcar"]~data[,"lrpmg"],data=data,method="within")
coef(summary(mod,robust=T))
coef(summary(mod,vcov=vcovHC))

##c
m<-matrix(0,nrow=length(data[,"country"]),ncol=nlevels(data[,"country"]))
for (j in 1:nlevels(data[,"country"])){
for (i in 1:length(data[,"country"])){
if (data[i,"country"]==levels(data[,"country"])[j])m[i,j]=1
}
}
y<-data[,"lgaspcar"]
x<-as.matrix(cbind(data[,"lrpmg"],m))
beta1<-solve(t(x)%*%x)%*%(t(x)%*%y)
res<-y-x%*%beta1
sig<-matrix(0,nrow=length(data[,"country"]),ncol=length(data[,"country"]))
diag(sig)<-res*res
sig.c<-matrix(0,nrow=length(data[,"country"]),ncol=length(data[,"country"]))
n<-c()
r<-0
for (k in 1:nlevels(data[,"country"])){
n[k]<-length(which(data[,"country"]==levels(data[,"country"])[k]))
for (i in 1:n[k]){
for (j in 1:n[k]){
sig.c[i+r,j+r]<-res[i+r]*res[j+r]
}
}
r=r+n[k]
}

cov<-solve(t(x)%*%x)%*%t(x)%*%sig%*%x%*%solve(t(x)%*%x)
cov.c<-solve(t(x)%*%x)%*%t(x)%*%sig.c%*%x%*%solve(t(x)%*%x)
stder<-sqrt(diag(cov))
stder.c<-sqrt(diag(cov.c))
t.stat<-beta1/stder
t.stat.c<-beta1/stder.c
p.val<-2*pt(-abs(t.stat),length(data[,"country"])-length(beta1))
p.val.c<-2*pt(-abs(t.stat.c),length(data[,"country"])-length(beta1))
sum1<-data.frame(cbind(beta1,stder,t.stat,p.val,stder.c,t.stat.c,p.val.c))
colnames(sum1)<-c("Par.Est","Rob_StD.Err","t-value","p-value","Clstrd.StD.Err","Clstrd.t-value","Clstrd.p-value")
rownames(sum1)<-c("lgaspcar",levels(data[,"country"]))
sum1

##b

ybar<-(t(y)%*%m)/apply(m,2,sum)
xbar<-(t(data[,"lrpmg"])%*%m)/apply(m,2,sum)


y.tld<-c()
for (i in 1:length(y)){
for (j in 1:length(ybar)){
#if (data[i,"year"]==1959+j) y.tld[i]<-ybar[j]
if (data[i,"country"]==levels(data[,"country"])[j]) y.tld[i]<-ybar[j]
}
}

x.tld<-c()
for (i in 1:length(y)){
for (j in 1:length(xbar)){
if(data[i,"country"]==levels(data[,"country"])[j])x.tld[i]<-xbar[j]
}
}

X<-data[,"lrpmg"]-x.tld
Y<-y-y.tld

beta2<-solve(t(X)%*%X)%*%t(X)%*%Y
res<-Y-X%*%beta2
sig<-matrix(0,nrow=length(data[,"country"]),ncol=length(data[,"country"]))
diag(sig)<-res*res

sig.c<-matrix(0,nrow=length(data[,"country"]),ncol=length(data[,"country"]))
n<-c()
r<-0
for (k in 1:nlevels(data[,"country"])){
n[k]<-length(which(data[,"country"]==levels(data[,"country"])[k]))
for (i in 1:n[k]){
for (j in 1:n[k]){
sig.c[i+r,j+r]<-res[i+r]*res[j+r]
}
}
r=r+n[k]
}

cov<-solve(t(X)%*%X)%*%t(X)%*%sig%*%X%*%solve(t(X)%*%X)
cov.c<-solve(t(X)%*%X)%*%t(X)%*%sig.c%*%X%*%solve(t(X)%*%X)
stder<-sqrt(diag(cov))
stder.c<-sqrt(diag(cov.c))
t.stat<-beta2/stder
t.stat.c<-beta2/stder.c
p.val<-2*pt(-abs(t.stat),length(data[,"country"])-length(beta1))
p.val.c<-2*pt(-abs(t.stat.c),length(data[,"country"])-length(beta1))
sum2<-data.frame(cbind(beta2,stder,t.stat,p.val,stder.c,t.stat.c,p.val.c))
colnames(sum2)<-c("Par.Est","Rob_StD.Err","t-value","p-value","Clstrd.StD.Err","Clstrd.t-value","Clstrd.p-value")
rownames(sum2)<-"lgaspcar"
sum2
################
rm(list=ls())

##Problem-2
dat1<-read.csv("jtrain1.csv",header=T)
names(dat1)[1]<-"year"
dat<-dat1[,c("year","hrsemp","grant")]
dat<-cbind(dat,as.factor(dat1[,"fcode"]))
names(dat)<-c("year","hrsemp","grant","fcode")
dat<-na.omit(dat)
head(dat)
table(dat[,"year"])
for (i in 1:dim(dat)[1]){
if (dat[i,"year"]==1987) dat[i,]<-NA
}
data<-na.omit(dat)
head(data)
n<-dim(data)[1]

y11<-0
n11<-0
for(i in 1:n){
if (data[i,"year"]==1989 & data[i,"grant"]==1){
y11<-y11+data[i,"hrsemp"]
n11<-n11+1
}
}
y11.bar<-y11/n11

y10<-0
n10<-0
for(i in 1:n){
if (data[i,"year"]==1988 & data[i,"grant"]==1){
y10<-y10+data[i,"hrsemp"]
n10<-n10+1
}
}
y10.bar<-y10/n10

y00<-0
n00<-0
for(i in 1:n){
if (data[i,"year"]==1988 & data[i,"grant"]==0){
y00<-y00+data[i,"hrsemp"]
n00<-n00+1
}
}
y00.bar<-y00/n00

y01<-0
n01<-0
for(i in 1:n){
if (data[i,"year"]==1989 & data[i,"grant"]==0){
y01<-y01+data[i,"hrsemp"]
n01<-n01+1
}
}
y01.bar<-y01/n01

alpha1<-(y11.bar-y10.bar)-(y01.bar-y00.bar)

##(ii)
Ei<-c()
for (i in 1:dim(data)[1]){
if (data[i,"year"]==1989 & data[i,"grant"]==1) {Ei[i]=1}
else {Ei[i]=0}
}
data$Ei<-Ei

yr_89<-c()
for (i in 1:dim(data)[1]){
if (data[i,"year"]==1989) {yr_89[i]=1}
else {yr_89[i]=0}
}
data$yr_89<-yr_89

x<-as.matrix(cbind(rep(1,dim(data)[1]),data[,"grant"],data[,"yr_89"],data[,"Ei"]))
y<-data[,"hrsemp"]
alpha2<-solve(t(x)%*%x)%*%(t(x)%*%y)


##(iii)
f<-levels(data[,"fcode"])
for (i in 1:length(f)){
if (table(data[,"fcode"])[i]==0)f<-f[-i]
}

dum<-matrix(0,nrow=dim(data)[1],ncol=length(f))
for (j in 1:length(f)){
for (i in 1:dim(data)[1]){
if(data[i,"fcode"]==f[j])dum[i,j]<-1
}
}

x<-as.matrix(cbind(data[,"grant"],data[,"yr_89"],dum))
y<-data[,"hrsemp"]
alpha3<-solve(t(x)%*%x)%*%(t(x)%*%y)

