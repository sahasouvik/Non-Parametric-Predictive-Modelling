library(ISLR)
attach(Auto)
str(Auto)
#Polynomial Fitting
par(mfrow=c(2,2))
xvals <- seq(min(horsepower),max(horsepower),.1)
l2 <- lm(mpg~poly(horsepower,deg=2,raw=T))
l3 <- lm(mpg~poly(horsepower,deg=3,raw=T))
l4 <- lm(mpg~poly(horsepower,deg=4,raw=T))
l8 <- lm(mpg~poly(horsepower,deg=8,raw=T))
plot(mpg~horsepower,xlab="Range(m)",ylab="MPG",col="grey",main="degree 2")
lines(xvals,l2$coeff[1]+l2$coeff[2]*xvals+l2$coeff[3]*xvals*xvals)
plot(mpg~horsepower,xlab="Range(m)",ylab="MPG",col="grey",main="degree 3")
lines(xvals,l3$coeff[1]+l3$coeff[2]*xvals+l3$coeff[3]*xvals*xvals+
        l3$coeff[4]*xvals*xvals*xvals,lty=1)
plot(mpg~horsepower,xlab="Range(m)",ylab="MPG",col="grey",main="degree 4")
lines(xvals,l4$coeff[1]+l4$coeff[2]*xvals+l4$coeff[3]*xvals*xvals+
        l4$coeff[4]*xvals*xvals*xvals+l4$coeff[5]*xvals*xvals*xvals*xvals,lty=1)
plot(mpg~horsepower,xlab="Range (m)",ylab="MPG",col="grey",main="degree 8")
lines(xvals,l8$coeff[1]+l8$coeff[2]*xvals+l8$coeff[3]*xvals^2+
        l8$coeff[4]*xvals^3+l8$coeff[5]*xvals^4+l8$coeff[6]*xvals^5+
        l8$coeff[7]*xvals^6+l8$coeff[8]*xvals^7+l8$coeff[9]*xvals^8,lty=1)
