require (Rmisc)
require(ggplot2)
require(readxl)
require(dplyr)


data6=read_excel("data.xlsx",sheet = "correlation")
ggplot(data_correlation,aes(x=hkt,y=growthrate))+geom_point()+geom_smooth(method="lm",se=F)
ggplot(data_correlation,aes(x=desi,y=growthrate))+geom_point()+geom_smooth(method="lm",se=F)
ggplot(data_correlation,aes(x=starvation,y=growthrate))+geom_point()+geom_smooth(method="lm",se=F)
ggplot(data_correlation,aes(x=ccrt,y=growthrate))+geom_point()+geom_smooth(method="lm",se=F)

ggplot(data_correlation,aes(x=desi,y=starvation))+geom_point()+geom_smooth(method="lm",se=F)





#correlation plot
par(mfrow=c(2,2))
par(mar=c(4, 4, 1, 1))
plot(x=data6$hkt, y=data6$growthrate, col=data6$color,xlim=c(350,585),ylim=c(2.53,2.75),xlab="Heat knock down time (sec)",ylab="Growth rate (ln(mg/day))",mgp=c(2, 0.5, 0))
abline(lm(data6$growthrate~data6$hkt),lwd=1.5)     
arrows(data6$hkt,data6$growthrate-data6$se1,data6$hkt,data6$growthrate+data6$se1,angle=90,lwd=1.5,length=0.02,code=3,col=data6$color)
arrows(data6$hkt-data6$se3,data6$growthrate,data6$hkt+data6$se3,data6$growthrate,angle=90,lwd=1.5,length=0.02,code=3,col=data6$color)
legend(520,2.75,c("female","male"),pch=c(1,1),col=data6$color,box.col = "white")

par(mar=c(4, 1, 1, 4))
plot(x=data6$ccrt, y=data6$growthrate, xlim=c(900,1900), ylim=c(2.53,2.75),col=data6$color,xlab="Chill comma recovery time (sec)",ylab="",mgp=c(2, 0.5, 0))
abline(lm(data6$growthrate~data6$ccrt),lwd=1.5)
arrows(data6$ccrt,data6$growthrate-data6$se1,data6$ccrt, data6$growthrate+data6$se1,code=3, lwd=1.5,length=0.02, angle = 90,col=data6$color)
arrows(data6$ccrt-data6$se5,data6$growthrate,data6$ccrt+data6$se5,data6$growthrate, code=3, lwd=1.5,length=0.02, angle = 90,col=data6$color)

par(mar=c(4, 4, 1, 1))
plot(x=data6$starvation, y=data6$growthrate,  xlim=c(200,380),ylim=c(2.53,2.75),col=data6$color,xlab="Starvation tolerance (hours)",ylab="Growth rate (ln(mg/day))",mgp=c(2, 0.5, 0))
abline(lm(data6$growthrate~data6$starvation),lwd=1.5)
arrows(data6$starvation-data6$se4,data6$growthrate,data6$starvation+data6$se4,data6$growthrate, code=3, lwd=1.5,length=0.02, angle = 90,col=data6$color)
arrows(data6$starvation,data6$growthrate-data6$se1,data6$starvation,data6$growthrate+data6$se1, code=3, lwd=1.5,length=0.02, angle = 90,col=data6$color)

par(mar=c(4, 1, 1, 4))
plot(x=data6$desiccation, y=data6$growthrate,  xlim=c(90,170),ylim=c(2.53,2.75),col=data6$color,xlab="Desiccation tolerance (hours)",ylab="",mgp=c(2, 0.5, 0))
abline(lm(data6$growthrate~data6$desiccation),lwd=1.5)
arrows(data6$desiccation-data6$se2,data6$growthrate,data6$desiccation+data6$se2, data6$growthrate,code=3, lwd=1.5,length=0.02, angle = 90,col=data6$color)
arrows(data6$desiccation,data6$growthrate-data6$se1,data6$desiccation,data6$growthrate+data6$se1, code=3, lwd=1.5,length=0.02, angle = 90,col=data6$color)


dev.off()






par(mar=c(4, 4, 1, 1))
plot(x=data6$desiccation, y=data6$starvation, col=data6$color,xlim=c(90,170),ylim=c(200,380),xlab="Desiccation tolerance (hours)",ylab="Starvation tolerance (hours)",mgp=c(2, 0.5, 0))
abline(lm(data6$starvation~data6$desiccation),lwd=1.5)     
arrows(data6$desiccation,data6$starvation-data6$se4,data6$desiccation,data6$starvation+data6$se4,angle=90,lwd=1.5,length=0.02,code=3,col=data6$color)
arrows(data6$desiccation-data6$se2,data6$starvation,data6$desiccation+data6$se2,data6$starvation,angle=90,lwd=1.5,length=0.02,code=3,col=data6$color)
legend(90,380,c("female","male"),pch=c(1,1),col=c("red","blue"),box.col = "white")









