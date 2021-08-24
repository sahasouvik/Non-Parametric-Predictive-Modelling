library(ISLR)
attach(Auto)
str(Auto)
#Piecewise Constant Models
y <-mpg
x <-horsepower 
xi1 <- min(x) + (range(x)[2]-range(x)[1])/3
xi2 <- min(x) + 2*(range(x)[2]-range(x)[1])/3
xtruncflat1 <- ifelse(x>xi1,1,0)
xtruncflat2 <- ifelse(x>xi2,1,0)
pieceflat <- lm(y~1+xtruncflat1+xtruncflat2)
plot(x,y,ylim=c(min(y),max(y)),ylab="y",type="n")
points(x,y,col="grey")
segments(x0=min(x),x1=xi1,y0=pieceflat$coeff[1],y1=pieceflat$coeff[1])
segments(x0=xi1,x1=xi2,y0=pieceflat$coeff[1]+pieceflat$coeff[2],
         y1=pieceflat$coeff[1]+pieceflat$coeff[2])
segments(x0=xi2,x1=max(x),y0=pieceflat$coeff[1]+pieceflat$coeff[2]+
           pieceflat$coeff[3],y1=pieceflat$coeff[1]+pieceflat$coeff[2]+pieceflat$coeff[3])
abline(v=xi1,lty=2)
abline(v=xi2,lty=2)