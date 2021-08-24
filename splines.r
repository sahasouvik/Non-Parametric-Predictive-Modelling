library(ISLR)
attach(Auto)
str(Auto)
#Splines
#Natural Splines
require(splines,quiet=T)
not=c(50,100,200)
spl=ns(horsepower,knots=not,Boundary.knots=c(250,350))
plot(spl[,1]~horsepower, ylim=c(0,max(spl)), type='l',
     lwd=2, col=1,xlab="Natural Cubic spline basis", ylab="")
for (j in 2:ncol(spl)) lines(spl[,j]~horsepower, lwd=2, col=j)
#Basis Splines
not=c(50,100,200)
spl=ns(horsepower,knots=not)
plot(spl[,1]~horsepower, ylim=c(0,max(spl)), type='l',
     lwd=2, col=1,xlab="B spline basis", ylab="")
for (j in 2:ncol(spl)) lines(spl[,j]~horsepower, lwd=2, col=j)
#Smoothing Splines
model=smooth.spline(horsepower,mpg)
model
plot(x,y)
lines(model,col="red",lwd=4)
