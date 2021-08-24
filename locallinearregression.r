library(ISLR)
attach(Auto)
str(Auto)
#Local Linear Regression
hlims =range(horsepower)
l=seq(from=hlims[1],to=hlims[2],length=392)
a=data.frame(l)
plot(horsepower,mpg,xlim=hlims,cex =.5,col =" darkgrey ")
fit1=loess(horsepower~mpg,span=0.2,data=Auto)
fit2=loess(horsepower~mpg,span=0.5,data=Auto)
lines(l,predict(fit1,a),col ="red ",lwd =2)
lines(l,predict(fit2,a),col =" blue",lwd =2)
legend("topright",legend =c("Span =0.2" ," Span =0.5") ,
        col=c("red "," blue "),lty =1, lwd =2, cex =.8)
