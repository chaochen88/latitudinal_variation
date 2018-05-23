require (Rmisc)
require(ggplot2)
require(readxl)
require(plyr)

#life history trait data set
data1=read_excel("data.xlsx",sheet = "lifehistory_trait")
#mean of each traits
meandat<-ddply(data1,.(latitude,longitude,block),summarize,growthrate=mean(growthrate),pupaltime=mean(pupaltime),larvaltime=mean(larvaltime),totaltime=mean(totaltime),pupalweight=mean(pupalweight),adultweight=mean(adultweight))



#chill comma data
data2=read_excel("data.xlsx",sheet = "chill_comma")
#mean of each traits
chill_mean<-ddply(data2,.(latitude,longitude,block),summarize,ccrt=mean(ccrt))

#heat knock down data
data3=read_excel("data.xlsx",sheet = "heat_knock")
#mean of each traits
heat_mean<-ddply(data3,.(latitude,longitude,block),summarize,hkt=mean(knockdown_time))



#starvation data
data4=read_excel("data.xlsx",sheet = "starvation")
#mean of each traits
starvation_mean<-ddply(data4,.(latitude,longitude,block),summarize,starvation=mean(time))



#desiccation data
data5=read_excel("data.xlsx",sheet = "desiccation")
#mean of each traits
desi_mean<-ddply(data5,.(latitude,longitude,block),summarize,desi=mean(time))
mod_desi=lm(time~latitude*sex*longitude,data=data5)
summary(mod_desi)

full_mean=cbind(meandat,chill_mean,heat_mean,starvation_mean,desi_mean)
write.csv(full_mean,"full_mean.csv")

data=read.csv("full_mean.csv")

data_pca=data[,c("starvation","desi","hkt","ccrt","growthrate")]
pca=princomp(scale(data_pca))
summary(pca)
pca$loadings[,1:3]

scores<-pca$scores[,1:2]

full.dat<-data.frame(full_mean,scores)
names(full.dat)


ggplot(full.dat,aes(x=latitude,y=Comp.1))+geom_point()+stat_smooth()

summary(lm(Comp.1~latitude+I(latitude^2)+I(latitude^3),data=full.dat))

ggplot(data,aes(x=starvation,y=growthrate))+geom_point()+geom_smooth(method="lm",se=F)
mod1=lm(growthrate~desi*block*sex,data=data)
summary(step(mod1,direction=c("both")))


mod2=lm(growthrate~latitude*hkt*block*sex,data=data)
summary(step(mod2,direction=c("both")))





#biolcim
library(maps)
library(raster)
library(mapdata)
w<-getData('worldclim', var='bio', res=2.5)
#plot(w,1)

plot(w,4,xlim=c(80,140),ylim=c(15,50), legend=F, col="white",main="",box=FALSE)
map("world",c("China"),add=TRUE)
points(full.dat$longitude,full.dat$latitude,pch=20,col="black",cex=3)

map("china")
points(full.dat$longitude,full.dat$latitude,pch=20,col="red",cex=3)
maps::map.scale(x=80, y=20, ratio=FALSE, relwidth=0.1) 
#library(GISTools)  
north.arrow(xb=85, yb=25, len=.5, lab="N")  



k<-extract(w,full.dat[,c("longitude","latitude")])
full.dat2<-data.frame(full.dat,k)


str(full.dat2)


bio<-princomp(scale(k))
summary(bio)
bio$loadings[,1:3]

bioclim.pc<-bio$scores[,1:3]
full.dat3<-data.frame(full.dat2,bioclim.pc)
str(full.dat3)

summary(lm(latitude~Comp.1.1,data=full.dat3))
summary(lm(latitude~Comp.1.1+Comp.2.1,data=full.dat3))

ggplot(full.dat3,aes(x=latitude,y=Comp.1.1))+stat_smooth()+geom_point()
ggplot(full.dat3,aes(x=Comp.1.1,y=Comp.1))+stat_smooth()+geom_point()

dat3.sub<-full.dat3%>%
  filter(Comp.1.1>-5)
ggplot(dat3.sub,aes(x=Comp.1.1,y=Comp.1))+stat_smooth(method="lm")+geom_point()


### latitudinal variation in climate pcs

names(full.dat3)

ggplot(full.dat3,aes(y=latitude,x=Comp.1.1))+geom_point()+stat_smooth(method="lm")
ggplot(full.dat3,aes(y=latitude,x=Comp.2.1))+geom_point()+stat_smooth(method="lm")



#### Trait PC1 analysis

#summary(lm(Comp.1~Comp.1.1+I(Comp.1.1^2)+Comp.2.1+I(Comp.2.1^2),data=full.dat3))
fullcompmod<-lm(Comp.1~Comp.1.1+I(Comp.1.1^2)+Comp.2.1+I(Comp.2.1^2),data=full.dat3)
summary(stepAIC(fullcompmod,direction="both"))

#plot based on model
full.dat3$resid0<-resid(lm(Comp.1~Comp.2.1+I(Comp.2.1^2),data=full.dat3))
a<-ggplot(full.dat3,aes(x=Comp.1.1,y=resid0))+stat_smooth(method="lm",formula= y~x +I(x^2),size=1)+geom_point()+ylab("Growth rate-hardiness trade-off (PC1)")+xlab("Seasonal to Aseasonal Cliamte (PC1)")
full.dat3$resid1<-resid(lm(Comp.1~Comp.1.1+I(Comp.1.1^2),data=full.dat3))
b<-ggplot(full.dat3,aes(x=Comp.2.1,y=resid1))+stat_smooth(method="lm")+geom_point()+ylab("")+xlab("Overall Precipitation (PC2)")#+ylab("Growth rate-hardiness trade-off (PC1)")

library(gridExtra)

grid.arrange(a,b,ncol=2)
#### Trait PC2 analysis
library(MASS)

fullcompmod2<-lm(Comp.2~Comp.1.1+I(Comp.1.1^2)+Comp.2.1+I(Comp.2.1^2),data=full.dat3)
summary(stepAIC(fullcompmod2,direction="both"))
#fig
ggplot(full.dat3,aes(x=Comp.1.1,y=Comp.2))+stat_smooth(method="lm",size=1)+geom_point()+ylab("Thermal Tolerance trade-off (PC2)")+xlab("Seasonal to Aseasonal Climate (PC1)")




#### Trait PC3 analysis


fullcompmod3<-lm(pca$scores[,3]~Comp.1.1+I(Comp.1.1^2)+Comp.2.1+I(Comp.2.1^2),data=full.dat3)
summary(stepAIC(fullcompmod3,direction="both"))
#fig
c<-ggplot(full.dat3,aes(x=Comp.1.1,y=pca$scores[,3]))+stat_smooth(method="lm",formula= y~x +I(x^2),size=1)+geom_point()+ylab("Desiccation/Starvation - Thermal Tolerance Trade-off (PC3)")+xlab("Seasonal to Aseasonal Climate (PC1)")
d<-ggplot(full.dat3,aes(x=Comp.2.1,y=pca$scores[,3]))+stat_smooth(method="lm",formula= y~x +I(x^2),size=1)+geom_point()+ylab("Desiccation/Starvation - Thermal Tolerance Trade-off (PC3)")+xlab("Overall Precipitation (PC2)")

grid.arrange(c,d,ncol=2)
