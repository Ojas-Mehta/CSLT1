my.data <- readRDS("Q1_data_02.Rda")
##Question-1
cor_predict = cor(subset(my.data, select = c(-y)))
cor_all = cor(my.data)
show(cor_predict)
show(cor_all)
pairs(my.data[,1:3])
pairs(my.data[,1:4])
my.model<-lm(y~x1+x2+x3, my.data)
mod_sum = summary(my.model)
show(mod_sum)
##Plots Linear Model
plot(my.model)

##Plot of Linear Model
my.dataframe = data.frame(x1 = my.data$x1,x2=my.data$x2,x3=my.data$x3)
y_predicted = predict(my.model, my.dataframe)
plot(my.data$y,y_predicted,xlab = expression('y'['actual']),ylab = expression('y'['predicted']),main="Predicted Output vs Actual Output")

##Question-2
Q2_fun_01 <- function(x) exp(-5*(x-0.3)^2)+0.5*exp(-100*(x-0.5)^2)+0.5*exp(-100*(x-0.75)^2)
Q2_fun_02 <- function(x) 2-3*x+10*x^4-5*x^9+6*x^14
##Plot functions 1 and 2
curve(Q2_fun_01, from = -2, to = 2)
curve(Q2_fun_02, from = -1, to = 1)
x_01=runif(300,-2,2)
x_02=runif(300,-1,1)
x_01 = sort(x_01)
x_02 = sort(x_02)
y_01 = Q2_fun_01(x_01)
y_02 = Q2_fun_02(x_02)
noise_01 = rnorm(n=300, m=0, sd=1)
noise_02 = rnorm(n=300, m=0, sd=1)
yhat_01 = y_01+noise_01
yhat_02 = y_02+noise_02

##My Degrees: 8 15 25
mod01_8<- lm(yhat_01 ~ poly(x_01,8))
mod01_15<- lm(yhat_01 ~ poly(x_01,15))
mod01_25<- lm(yhat_01 ~ poly(x_01,25))
mod02_8<- lm(yhat_02 ~ poly(x_02,8))
mod02_15<- lm(yhat_02 ~ poly(x_02,15))
mod02_25<- lm(yhat_02 ~ poly(x_02,25))
ypred01_8 = predict(mod01_8, data.frame(x_01))
ypred01_15 = predict(mod01_15, data.frame(x_01))
ypred01_25 = predict(mod01_25, data.frame(x_01))
ypred02_8 = predict(mod02_8, data.frame(x_02))
ypred02_15 = predict(mod02_15, data.frame(x_02))
ypred02_25 = predict(mod02_25, data.frame(x_02))
show(summary(mod01_8))
show(summary(mod01_15))
show(summary(mod01_25))
show(summary(mod02_8))
show(summary(mod02_15))
show(summary(mod02_25))

##Plot polynomial curve fitting for Q_2_fun_01
plot(x_01,yhat_01,col='deepskyblue4',xlab='x', ylab='y',main='Polynomial Curve Fitting\nQ2_fun_01')
lines(x_01,y_01,lwd=3)
lines(x_01,ypred01_8,col='red',lwd=3)
legend("topright",c("Training data","Actual","Predicted"), col=c("deepskyblue4","black","red"), lwd=3)
plot(x_01,yhat_01,col='deepskyblue4',xlab='x', ylab='y',main='Polynomial Curve Fitting\nQ2_fun_01')
lines(x_01,y_01,lwd=3)
lines(x_01,ypred01_15,col='red',lwd=3)
legend("topright",c("Training data","Actual","Predicted"), col=c("deepskyblue4","black","red"), lwd=3)
plot(x_01,yhat_01,col='deepskyblue4',xlab='x', ylab='y',main='Polynomial Curve Fitting\nQ2_fun_01')
lines(x_01,y_01,lwd=3)
lines(x_01,ypred01_25,col='red',lwd=3)
legend("topright",c("Training data","Actual","Predicted"), col=c("deepskyblue4","black","red"), lwd=3)

##Plot polynomial curve fitting for Q_2_fun_02
plot(x_02,yhat_02,col='deepskyblue4',xlab='x', ylab='y',main='Polynomial Curve Fitting\nQ2_fun_02')
lines(x_02,y_02,lwd=3)
lines(x_02,ypred02_8,col='red',lwd=3)
legend("topright",c("Training data","Actual","Predicted"), col=c("deepskyblue4","black","red"), lwd=3)
plot(x_02,yhat_02,col='deepskyblue4',xlab='x', ylab='y',main='Polynomial Curve Fitting\nQ2_fun_02')
lines(x_02,y_02,lwd=3)
lines(x_02,ypred02_15,col='red',lwd=3)
legend("topright",c("Training data","Actual","Predicted"), col=c("deepskyblue4","black","red"), lwd=3)
plot(x_02,yhat_02,col='deepskyblue4',xlab='x', ylab='y',main='Polynomial Curve Fitting\nQ2_fun_02')
lines(x_02,y_02,lwd=3)
lines(x_02,ypred02_25,col='red',lwd=3)
legend("topright",c("Training data","Actual","Predicted"), col=c("deepskyblue4","black","red"), lwd=3)


#Plot Bias-Variance Curve
#Q2_fun_01
degrees <- c(8,15,25)
biases_01 <- vector(mode="double", length=3)
vars_01 <- vector(mode="double", length=3)
temp_x_01=runif(300,-2,2)
act_y_01 = Q2_fun_01(temp_x_01)
for(i in 1:3){
  exp_predy<-vector(mode="double", length=300)
  diff_squares<-vector(mode="double", length=300)
  for(j in 1:10){
    temp_deg <- degrees[i]
    temp_noise <- rnorm(n=300, m=0, sd=1)
    temp_yhat <- act_y_01+temp_noise
    temp_mod<- lm(temp_yhat ~ poly(temp_x_01,degrees[i]))
    temp_preds<-predict(temp_mod, data.frame(temp_x_01))
    diff_squares<-diff_squares+(temp_preds*temp_preds)
    exp_predy <- exp_predy + temp_preds
  }
    exp_predy <- exp_predy/10
    diff_squares<-diff_squares/10
    biases_01[i] <- mean((exp_predy-act_y_01)*(exp_predy-act_y_01))
    vars_01[i]<-mean(diff_squares-exp_predy*exp_predy)
}
par(mar=c(5, 4, 4, 6) + 1)
plot(degrees, biases_01, pch=16, axes=FALSE, xlab="", ylab="",
     type="b",col="black", main=('Bias-Variance Plot\nQ2_fun_01'))
axis(2, ylim=range(biases_01),col="black")

mtext(expression('Bias'^2),side=2,line=2.5)
box()
par(new=TRUE)
plot(degrees, vars_01, pch=15,  xlab="", ylab="", axes=FALSE, type="b", col="red")
mtext("Variance",side=4,col="red",line=4)
axis(4, ylim=range(vars_01), col="red",col.axis="red",las=1)

axis(1,pretty(range(degrees),20))
mtext("Degrees",side=1,col="black",line=2.5)
legend("right",legend=c(expression('Bias'^2),"Variance"),text.col=c("black","red"),pch=c(16,15),col=c("black","red"))

#Plot Bias-Variance Curve
#Q2_fun_02

biases_02 <- vector(mode="double", length=3)
vars_02 <- vector(mode="double", length=3)
temp_x_02=runif(300,-1,1)
act_y_02 = Q2_fun_02(temp_x_02)
for(i in 1:3){
  exp_predy<-vector(mode="double", length=300)
  diff_squares<-vector(mode="double", length=300)
  for(j in 1:10){
    temp_deg <- degrees[i]
    temp_noise <- rnorm(n=300, m=0, sd=0.1)
    temp_yhat <- act_y_02+temp_noise
    temp_mod<- lm(temp_yhat ~ poly(temp_x_02,degrees[i]))
    temp_preds<-predict(temp_mod, data.frame(temp_x_02))
    diff_squares<-diff_squares+(temp_preds*temp_preds)
    exp_predy <- exp_predy + temp_preds
  }
  exp_predy <- exp_predy/10
  diff_squares<-diff_squares/10
  biases_02[i] <- mean((exp_predy-act_y_02)*(exp_predy-act_y_02))
  vars_02[i]<-mean(diff_squares-exp_predy*exp_predy)
}
par(mar=c(5, 4, 4, 6) + 1)
plot(degrees, biases_02, pch=16, axes=FALSE, xlab="", ylab="",
     type="b",col="black", main=('Bias-Variance Plot\nQ2_fun_02'))
axis(2, ylim=range(biases_01),col="black")

mtext(expression('Bias'^2),side=2,line=2.5)
box()
par(new=TRUE)
plot(degrees, vars_02, pch=15,  xlab="", ylab="", axes=FALSE, type="b", col="red")
mtext("Variance",side=4,col="red",line=4)
axis(4, ylim=range(vars_01), col="red",col.axis="red",las=1)
#1.377006e-03 6.301738e-07 1.198664e-06
#2.705527e-06 4.225399e-06 8.754480e-06
axis(1,pretty(range(degrees),20))
mtext("Degrees",side=1,col="black",line=2.5)
legend("right",legend=c(expression('Bias'^2),"Variance"),text.col=c("black","red"),pch=c(16,15),col=c("black","red"))