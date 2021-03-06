##16.3
dat<-read.csv("home_heating.csv",header=T)
names(dat)[1]<-"id"
head(dat)
dat.a<-dat[c(3,14,21),c("choice","id","ic1","ic2","ic3","ic4","ic5","oc1","oc2","oc3","oc4","oc5")]

a<-matrix(rep(0,15),nrow=3,ncol=5)
for (i in 1:dim(a)[1]){
for (j in 1:dim(a)[2]){
if (dat.a[i,"choice"]==j){a[i,j]=1/5}
else {a[i,j]=1/5}
}
}
dat.a[,13:17]<-a
names(dat.a)<-c("choice","id","ic1","ic2","ic3","ic4","ic5","oc1","oc2","oc3","oc4","oc5","alt1","alt2","alt3","alt4","alt5")

dat.a$red5<--0.15*dat.a[,"ic5"]
dat.a$wtp5<-(dat.a$red5/-0.6231)


##16.4
library("mlogit")
#a
dat.m<-mlogit.data(dat,choice="choice",shape="wide",varying=c(3:7,8:12),sep="")
head(dat.m,10)
mod<-mlogit(choice~ic+oc|0,dat.m)
summary(mod)
wtp<-coef(mod)["oc"]/coef(mod)["ic"]
#b
mod1<-mlogit(choice~ic+oc|-1+income+age+rooms+r1+r2+r3,dat.m)
summary(mod1)
wtp1<-coef(mod1)["oc"]/coef(mod1)["ic"]
#c
m<-mlogit(choice~log(income-ic)+oc|0,dat.m)
m1<-mlogit(choice~log(income-ic)+oc|-1+age+rooms+r1+r2+r3,dat.m)
summary(m)
summary(m1)
#d
n.dat<-dat
n.dat[,"ic5"]<-0.85*dat[,"ic5"]
n.dat.m<-mlogit.data(n.dat,choice="choice",shape="wide",varying=c(3:7,8:12),sep="")
apply(fitted(mod,outcome=F),2,mean)
apply(predict(mod,newdata=n.dat.m),2,mean)
apply(fitted(mod1,outcome=F),2,mean)
apply(predict(mod1,newdata=n.dat.m),2,mean)
#e
mod2<-mlogit(choice~ic+oc|0,n.dat.m)
xtable(coef(summary(mod3)))
wtp2<-coef(mod2)["oc"]/coef(mod2)["ic"]
mod3<-mlogit(choice~ic+oc|-1+income+age+rooms+r1+r2+r3,n.dat.m)
wtp3<-coef(mod3)["oc"]/coef(mod3)["ic"]

#########################################################
rm(list=ls())

##17.3(a)
dat<-read.csv("NC_beach.csv",header=T)
names(dat)[1]<-"id"
head(dat)

log.lik<-function(beta,data){
l<-0
for (i in 1:dim(data)[1]){
deltai<-exp(beta[1]+beta[2]*data[i,"pr10"]+beta[3]*data[i,"inc"])
if (dat[i,"tr10"]!=0){dij=1}
else {dij=0}
a<--dij*(deltai+data[i,"tr10"]*log(deltai))
l<-l+a
}
return(-l)
}

par.est<-optim(c(1,0.001,0.0001),fn=log.lik,data=dat)
par.est

log.lik1<-function(beta,data){
l<-0
for (i in 1:dim(data)[1]){
deltai<-exp(beta[1]+beta[2]*data[i,"pr10"])
if (dat[i,"tr10"]!=0){dij=1}
else {dij=0}
a<--deltai+data[i,"tr10"]*log(deltai)
l<-l+a
}
return(-l)
}

par.est1<-optim(c(1,0.0001),fn=log.lik1,data=dat)
par.est1
##Average welfare loss

cv1<-c()
for (i in 1:dim(dat)[1]){
cv1[i]<-exp(par.est1$par[1]+par.est1$par[2]*dat[i,"pr9"])/abs(par.est1$par[2])
}
mean(cv1)

##Average welfare loss for access fee

cv2<-c()
for (i in 1:dim(dat)[1]){
a<-exp(par.est1$par[1]+par.est1$par[2]*dat[i,"pr9"])/par.est1$par[2]
b<-exp(par.est1$par[1]+par.est1$par[2]*(5+dat[i,"pr9"]))/par.est1$par[2]
cv2[i]<-b-a
}
mean(cv2)
########################################################
rm(list=ls())
###17.4
dat<-read.csv("WI_100.csv",header=T)
names(dat)[1]<-"id"
head(dat)
library(mlogit)
#a
dat.m<-mlogit.data(dat,choice="choice",shape="wide",varying=c(3:102),sep="")
d<-read.csv("att.csv",header=T)
names(d)[1]<-"ramp"
ramp<-rep(d$ramp,2404)
restroom<-rep(d$restroom,2404)
walleye<-rep(d$walleye,2404)
salmon<-rep(d$salmon,2404)
panfish<-rep(d$panfish,2404)
dat.m$ramp<-ramp
dat.m$restroom<-restroom
dat.m$walleye<-walleye
dat.m$salmon<-salmon
dat.m$panfish<-panfish
mod<-mlogit(choice~tc+walleye+salmon+panfish|0,data=dat.m)
summary(mod)
wtp<-coef(mod)["walleye"]/coef(mod)["tc"]
wtp
#b
#mod1<-mlogit(choice~tc+walleye+salmon+panfish+ramp+restroom+ramp*boat+restroom*kids|0,data=dat.m)
#c
a<-table(dat[,"choice"])
sort(a)[91:100]
y<-names(sort(a)[91:100])
b<-as.numeric(names(sort(a)[91:100]))
n.dat.m<-dat.m
for (i in 1:length(b)){
d$walleye[b[i]]*1.3->nw
}
nwalley<-rep(nw,2404)
n.dat.m$walley<-nwalley
v<-sort(apply(predict(mod,newdata=n.dat.m),2,mean))[100]
x<-as.numeric(names(v))
