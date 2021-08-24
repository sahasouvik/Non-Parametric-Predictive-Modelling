library(ISLR)
attach(Auto)
str(Auto)
#KNN
require(FNN,quiet=T)
l=seq(min(horsepower),max(horsepower),5)
a=data.frame(l)
knn1=knn.reg(train=horsepower,test=a,y=mpg,k=5)
knn2=knn.reg(train=horsepower,test=a,y=mpg,k=12)
knn3=knn.reg(train=horsepower,test=a,y=mpg,k=25)
knn4=knn.reg(train=horsepower,test=a,y=mpg,k=40)
par(mfrow=c(2,2))
plot(horsepower,mpg,main="KNN Regression for k=5")
lines(l,knn1$pred,col="red")
plot(horsepower,mpg,main="KNN Regression for k=12")
lines(l,knn2$pred,col="blue")
plot(horsepower,mpg,main="KNN Regression for k=25")
lines(l,knn3$pred,col="green")
plot(horsepower,mpg,main="KNN Regression for k=40")
lines(l,knn4$pred,col="yellow")
#Finding k by cross validation
cvknn<-function(k,n){
error=NULL
for(i in 1:length(k))
{
test.error=NULL
folds=split(sample(1:n),ceiling(seq_along(sample(1:n))/(n/10)))
for(j in 1:10)
{
model=knn.reg(train=horsepower[-folds[[j]]],test=data.frame(horsepower[folds[[j]]]),y=mpg[-folds[[j]]],k=k[i])
ter=sum((mpg[folds[[j]]]-model$pred)^2)
test.error=c(test.error,ter)
}
error=c(error,mean(test.error))
}
a=k[which(error==min(error))]
return(a)
}
k=seq(2,200,by=5)
kopt=cvknn(k,nrow(Auto))
kopt
#Fitting with the optimum k
knnopt=knn.reg(train=horsepower,test=a,y=mpg,k=kopt)
plot(horsepower,mpg)
lines(l,knnopt$pred,col=578,main="KNN with optimum k by Cross-Validation")

