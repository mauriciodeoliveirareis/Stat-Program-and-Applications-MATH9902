x=c(1,5,10)
y=x^2
plot(x,y,main = expression(y==x^2), pch=20, cex=2)

x=1:10
y=x^2
plot(x,y,main = expression(y==x^2), pch=20, cex=2)


curve(x^2, from=1, to=10,  type = 'p', main = expression(y==x^2), lwd = 4, cex=0.1)


curve(x^2, from=1, to=10,  type = 'l', main = expression(y==x^2), lwd = 4, cex=0.1)

#Straight lines polinomials of degree 1
### line 1
curve(0+1*x,from=0,to=10,col='blue',lwd=4,ylab='y',main='(Polynomial) Straight Lines')
text(1.5,1,labels=expression(y==0+1*x),col='blue',cex=1,pos=4)
### line 2
curve(9-0.5*x,from=0,to=10,col='red',lwd=4,add=T)
text(2,9,labels=expression(y==9-0.5*x),col='red',cex=1,pos=4)
### line 3
curve(4+0.15*x,from=0,to=10,col='darkgreen',lwd=4,add=T)
text(0,5,labels=expression(y==4+0.15*x),col='darkgreen',cex=1,pos=4)


#Quadratic curved lines (Polynomials of degree 2)
### quadratic 1
curve(0+4*x-0.4*x^2,from=0,to=10,type='l',col='blue',lwd=4,ylab='y',main='Quadratic Lines',xlim=c(0,15),ylim=c(0,11))
text(1,1.5,labels=expression(y==0+4*x-0.4*x^2),col='blue',cex=1,pos=4)
### quadratic 2
curve(0+2*x-0.13*x^2,from=0,to=15,,col='red',lwd=4,lty=1,add=T)
text(9.5,8,labels=expression(y==0+2*x-0.13*x^2),col='red',cex=1,pos=4)
### quadratic 3
curve(9-1.4*x+0.08*x^2,from=0,to=15,col='darkgreen',lty=1,lwd=6,add=T)
text(0,10,labels=expression(y==9-1.4*x+0.08*x^2),col='darkgreen',cex=1,pos=4)

#polynomials with degree 3+
### polynomial 1 (degree 3)
curve(0+3*x-0.6*x^2+0.038*x^3,from=0,to=10,type='l',col='blue',lwd=4,ylab='y',main='Polynomials with Higher Degrees',ylim=c(0,8),cex=1.5)
text(2.5,4,labels=expression(y==0+3*x-0.6*x^2+0.038*x^3),col='blue',cex=1,pos=4)
### polynomial 2 (degree 4)
curve(4-1.56*x+1.2*x^2-0.21*x^3+0.011*x^4,from=0,to=10,cex=1.5,col='red',lwd=4,lty=1,add=T)
text(2,7.25,labels=expression(y==4-1.56*x+1.2*x^2-0.21*x^3+0.011*x^4),col='red',cex=1,pos=4)
### polynomial 3 (degree 3)
curve(4-1.9*x+0.38*x^2-0.02*x^3,from=0,to=10,col='darkgreen',lty=1,lwd=6,add=T)
text(5,1,labels=expression(y==4-1.9*x+0.38*x^2-0.02*x^3),col='darkgreen',cex=1,pos=4)


#3d plots
x1=seq(0,10,len=11)
x2=x1
f=function(x1,x2){15+5*x1-2*x2}
f=Vectorize(f)
y=outer(x1,x2,f)
library(rgl)
persp3d(x1,x2,y,front="points",back="points",size=10,cex.main=5)
p1=recordPlot() #this is needed for R markdown compiation
#plottimg a wire frame
persp3d(x1,x2,y,front="lines",back="lines",col='blue')
#adding second plane
p1
g=function(x1,x2){0-2*x1+4*x2}
g=Vectorize(g)
z=outer(x1,x2,g)
persp3d(x1,x2,z,front="lines",back="lines",col='red',add=T)

#curved surfaces in 3D

