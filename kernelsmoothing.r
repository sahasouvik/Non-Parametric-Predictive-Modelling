library(ISLR)
attach(Auto)
str(Auto)
require(FNN,quiet=T)
#Kernel Smoothing
ks1=ksmooth(horsepower,mpg,"box",bandwidth=0.05)
names(ks1)
ks2=ksmooth(horsepower,mpg,"box",bandwidth=0.5)
ks3=ksmooth(horsepower,mpg,"box",bandwidth=10)
ks4=ksmooth(horsepower,mpg,"box",bandwidth=50)
par(mfrow=c(2,2))
plot(horsepower,mpg,main="Bandwidth is 0.05")
lines(horsepower,ks1$y,col="blue")
plot(horsepower,mpg,main="Bandwidth is 0.5")
lines(horsepower,ks2$y,col="pink")
plot(horsepower,mpg,main="Bandwidth is 10")
lines(horsepower,ks3$y,col="violet")
plot(horsepower,mpg,main="Bandwidth is 50")
lines(horsepower,ks4$y,col="orange")
#Finding the bandwidth h by cross validation
cvkernel<-function(h,n){
error2=NULL
for(i in 1:length(h))
{
folds=split(sample(1:n),ceiling(seq_along(sample(1:n))/(n/10)))
test.error2=NULL
for(j in 1:10)
{
model=ksmooth(horsepower[-folds[[j]]],mpg[-folds[[j]]],"box",bandwidth=h[i],x.points=horsepower[folds[[j]]])
ter=mean((mpg[folds[[j]]]-model$y)^2)
test.error2=c(test.error2,ter)
}
error2=c(error2,mean(test.error2))
}
return(h[which.min(error2)])
}
h=seq(10,300,by=10)
hopt=cvkernel(h,nrow(Auto))
hopt
#Fitting with the optimum bandwidth
ksmod=ksmooth(horsepower,mpg,"box",bandwidth=hopt)
plot(horsepower,mpg)
lines(horsepower,ks$y,col="yellow")
#For normal
ks2=ksmooth(horsepower,mpg,"normal",bandwidth=50)
plot(horsepower,mpg)
lines(horsepower,ks2$y,col="pink")
#Finding the bandwidth h by cross validation
cvkernel2<-function(h,n){
error3=NULL
for(i in 1:length(h))
{
folds=split(sample(1:n),ceiling(seq_along(sample(1:n))/(n/10)))
test.error3=NULL
for(j in 1:10)
{
model=ksmooth(horsepower[-folds[[j]]],mpg[-folds[[j]]],"normal",bandwidth=h[i],x.points=horsepower[folds[[j]]])
ter3=mean((mpg[folds[[j]]]-model$y)^2)
test.error3=c(test.error3,ter3)
}
error3=c(error3,mean(test.error3))
}
b=h[which(error3==min(error3))]
return(b)
}
h=seq(10,300,by=10)
hopt2=cvkernel2(h,nrow(Auto))
hopt2
#Kernel Smoothing
ksmod=ksmooth(horsepower,mpg,"box",bandwidth=hopt2)
plot(horsepower,mpg)
lines(horsepower,ks$y,col="yellow")
