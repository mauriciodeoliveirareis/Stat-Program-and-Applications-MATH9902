#gets the path to the folder where this R script is in
current_path = rstudioapi::getActiveDocumentContext()$path
#set current working directory to the folder this script is in
setwd(dirname(current_path))

ldlData <- read.csv(file = 'ldldata.csv')
colnames(ldlData)<-c('weight','ldl')

#pdf('ldlplot.pdf',width = 11.75, height = 7.25, paper='a4r')
plot(ldlData$weight,ldlData$ldl)
fitLdl=lm(ldl~weight,data=ldlData)
abline(fitLdl,col='red',lwd=2)
legend('topleft', legend='Least Squares Line',col='red',lwd=1)
#dev.off()


dr <- read.table(file = 'doseresponse.txt',header=TRUE,sep=' ')
with(dr, plot(dose,activity,pch=19,cex=1.5,pty='s',cex.lab=1.5,col='blue'))
fit_dr<- lm(activity~dose+I(dose^2),data=dr)
fit_dr
curve(430+69.5*x-0.817*x^2,from=10,to=80,col='red',add=T,lwd=2)


bw <- read.table(file = 'breadwrapper.txt',header=TRUE,sep='')
fit_bw=lm(Seal_Strength~sealtemp+polyethylene,data=bw)
summary(fit_bw)

y=bw[,1]
x1=bw[,2]
x12=x1^2
x2=bw[,4]
x22=x2^2
fit2=lm(y~x1+x12+x2+x22)

library(rgl)
plot3d(x1,x2,y,col='red',size=1,type='s',xlab='Temperature',ylab='Polyethylene',zlab='Seal Strength',par3d(mouseMode=c("trackball")),zlim=c(.9,10.2))
x=seq(min(x1),max(x1),len=20)
y=seq(min(x2),max(x2),len=20)
z=matrix(0,ncol=length(y),nrow=length(x))
f=function(x,y) {fit2$coeff[1]+fit2$coeff[2]*x+fit2$coeff[3]*x^2+fit2$coeff[4]*y+fit2$coeff[5]*y^2}
for(i in 1:length(x)){
  for(j in 1:length(y)){
    z[i,j]=f(x[i],y[j])
  }
}
surface3d(x,y,z,col="blue",add=T,front="lines",back="lines",lit=F)