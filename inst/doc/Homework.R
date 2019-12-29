## ----results='asis'-----------------------------------------------------------
library(SC19056)
data("exams")
x<-exams
x$stu_id<-1:dim(x)[[1]]
p<-xtable::xtable((x))
print(p,type="html")

## -----------------------------------------------------------------------------
exam_m<-(subset(x,x$gender=="M")[-3])
exam_f<-(subset(x,x$gender=="F")[-3])

## ----results='asis'-----------------------------------------------------------
p<-xtable::xtable((exam_m)) 
print(p,type="html")

## ----results='asis'-----------------------------------------------------------
p<-xtable::xtable((exam_f)) 
print(p,type="html") 

## -----------------------------------------------------------------------------
a_exam1_m<-mean(exam_m[,1])
a_exam1_f<-mean(exam_f[,1])
plot(exam_m[,3],exam_m[,1],main="Scatter plot of all students' exam_1 scrore?",xlim=c(1,12),ylim=c(0,6),xla="stu_id",yla="exam1_sco",col="red")
points(exam_f[,3],exam_f[,1],col="blue")
abline(h=a_exam1_m, col="red")
abline(h=a_exam1_f, col="blue")


## -----------------------------------------------------------------------------
a_exam2_m<-mean(exam_m[,2])
a_exam2_f<-mean(exam_f[,2])
plot(exam_m[,3],exam_m[,2],main="Scatter plot of all students' exam_2 scrore?",xlim=c(1,12),ylim=c(0,6),xla="stu_id",yla="exam2_sco",col="red")
points(exam_f[,3],exam_f[,2],col="blue")
abline(h=a_exam2_m, col="red") 
abline(h=a_exam2_f, col="blue")

## -----------------------------------------------------------------------------
plot(x$exam1,x$exam2,xla="exam1",yla="exam2")

## -----------------------------------------------------------------------------
weight<-c(x$exam1,x$exam2)
group<-gl(2,dim(x)[[1]],2*dim(x)[[1]],labels=c("x","y"))
lm_ex<-lm(weight~group)
#par(mfrow=c(2,2))
plot(lm_ex)
summary(lm_ex)$coef
y<-summary(lm_ex)$coef

## -----------------------------------------------------------------------------
# Define a function to drow histogram for any sigma.
plot_sigma1<-function(sigma){
  f_x<-function(x){     
    return((x/sigma^2)*exp(-(x^2)/(2*sigma^2)))  
    }
set.seed(12345)
n<-10000
u<-runif(n)
y<-sigma*sqrt(-2*log(1-u))
hist(y,prob=TRUE,main =paste("sigma=",sigma))
x<-seq(0,60,.01)
lines(x,f_x(x))
}
#par(mfrow=c(2,2))
plot_sigma1(2);plot_sigma1(5);plot_sigma1(8);plot_sigma1(10)

## -----------------------------------------------------------------------------
# Define a function to drow histogram for any sigma.
plot_sigma2<-function(sigma){
 f_x<-function(x){     
    return((x/sigma^2)*exp(-(x^2)/(2*sigma^2)))  
    }
  n<-1e6
  c<-4*sigma^2/(exp(-1/(2*sigma^2)))
  y<-rgamma(n,2,1/(2*sigma^2))
  u<-runif(n)
  p<-y[which(f_x(y)/(c*dgamma(y,2,1/(2*sigma^2)))>=u)]
  hist(p,prob=TRUE,main =paste("sigma=",sigma))
  x<-seq(0,60,.01)
  lines(x,f_x(x))
 # lines(x,c*dgamma(x,2,1/(2*sigma^2)))
}
#par(mfrow=c(2,2)) 
plot_sigma2(2);plot_sigma2(5);plot_sigma2(8);plot_sigma2(10)

## -----------------------------------------------------------------------------
# Define a function to drow histogram for any sigma.
plot_sigma3<-function(sigma){
  f_x<-function(x){      
    return((x/sigma^2)*exp(-x^2/(2*sigma^2)))  
    }
  n<-1e4
  y<-sqrt(rexp(n,1/(2*sigma^2)))
  hist(y,prob=TRUE,main =paste("sigma=",sigma)) 
  x<-seq(0,60,.01) 
  lines(x,f_x(x))
}
#par(mfrow=c(2,2)) 
plot_sigma3(2);plot_sigma3(5);plot_sigma3(8);plot_sigma3(10)

## -----------------------------------------------------------------------------
n<-1e6
# Define a function to drow histogram for any p1
get_norm<-function(p){
x<-sample(0:1,n,replace = TRUE,c(1-p,p))
y<-vector(length=length(x))
  for(i in 1:length(x)){
    if (x[i]==1)
    y[i]<-rnorm(1)
    else
      y[i]<-rnorm(1)+3
  }
  hist(y,main=paste("p=",p))
}
#par(mfrow=c(2,3))
get_norm(.3);get_norm(.4);get_norm(.5);get_norm(.6);get_norm(.75);get_norm(.9)

## -----------------------------------------------------------------------------
library(MASS)
n<-20
p<-6
# Get Covariance matrix sigma:
w<-matrix(sample(-10:10,p*p,replace = TRUE),p,p)
sigma<-cov(w)
# Produce random variable based on  Multivariate normal distribution.
x<-mvrnorm(n, sample(-3:3,p,replace = TRUE), sigma)
s<-matrix(0,p,p)
for (i in 1:n){
  s<-s+(as.matrix(x[i,])%*%t(as.matrix(x[i,])))
}
s

## ----results='asis'-----------------------------------------------------------
set.seed(123)
n<-1e7
x<-(pi/3)*runif(n)
y<-sin(x)*(pi/3)
I<-mean(y)
m<-matrix(c(mean(y),var(y)),1,2)
rownames(m)<-"Simple MC  "
colnames(m)<-c("mean","variance")
p<-knitr::kable(m)
print(p,type="latex")

## ----results='asis'-----------------------------------------------------------
n<-1e7
x1<-(pi/3)*runif(n)
x2<-pi/3-x1
y1<-sin(x1)*(pi/3)
y2<-sin(x2)*(pi/3)
I<-mean(y1+y2)/2
m<-matrix(c(I,var((y1+y2)/2)),1,2)
rownames(m)<-"Control variate  "
colnames(m)<-c("mean","variance")
p<-knitr::kable(m)
print(p,type="latex")

## ----results='asis'-----------------------------------------------------------
n<-1e7
x<-(pi/3)*runif(n)
y<-sin(x)*(pi/3)
a<--cov(x,y)/var(x)
y<-sin(x)*(pi/3)+a*(x-pi/6)
m<-matrix(c(mean(y),var(y)),1,2)
rownames(m)<-"Control variate  "
colnames(m)<-c("mean","variance")
p<-knitr::kable(m)
print(p,type="latex")

## ----results='asis'-----------------------------------------------------------
n<-1e7
x1<-runif(n)
y1<-exp(-x1)/(1+x1^2)
I<-mean(y1)
m<-matrix(c(I,var(y1)),1,2)
rownames(m)<-"Simple MC  "
colnames(m)<-c("mean","variance")
p<-knitr::kable(m)
print(p,type="latex")

## ----results='asis'-----------------------------------------------------------
library(scales)
n<-1e7
x1<-runif(n/2)
x2<-1-x1
y1<-exp(-x1)/(1+x1^2)
y2<-exp(-x2)/(1+x2^2)
I<-mean(y1+y2)/2
m<-matrix(c(I,var((y1+y2)/2)),1,2)
rownames(m)<- "Antithetic variables"
colnames(m)<-c("mean","variance")
p<-knitr::kable(m)
print(p,type="latex")

## -----------------------------------------------------------------------------
n<-1e5
K<-5
N<-1000
Y<-numeric(K)
est <-matrix(0, N, 2)
F<-function(k,K) 
  {   
    return(c((1-exp(-(k-1)/5))/(1-exp(-1)),(1-exp(-k/K))/(1-exp(-1))))
}
for(j in 1:N){
  x<--log(1-(1-exp(-1))*runif(n))
  y<-(1-exp(-1))/(1+x^2)
for (i in 1:K){
  u<-runif(n*(F(i,K)[2]-F(i,K)[1]))
  u1<-u*(F(i,K)[2]-F(i,K)[1])+F(i,K)[1]
  x1<--log(1-(1-exp(-1))*u1)
  y1<-(F(i,K)[2]-F(i,K)[1])*(1-exp(-1))/(1+x1^2)
  Y[i]<-mean(y1)
}
  est[j,1]<-mean(y)
  est[j,2]<-sum(Y)
}


## ----results='asis'-----------------------------------------------------------
m<-matrix(nrow=2,ncol=2)
m[1,]<-apply(est,2,mean)
m[2,]<-apply(est,2,sd)
colnames(m)<-c("Importance sampling","Stratified importance sampling")
rownames(m)<-c("mean","standard error")
p<-knitr::kable(m)
print(p,type="latex")

## -----------------------------------------------------------------------------
set.seed(1234)
n<-1e6
u<-runif(n)
x1<-sqrt(-2*log((1-u)/exp(1/2)))
x2<-(-3*log((1-u)/exp(1/3)))^(1/3)
y1<-x1/(2*pi*exp(1))^(1/2)
y2<-1/((2*pi)^(1/2)*exp(1/3))*exp(x2^3/3-x2^2/2)
I1<-mean(y1)
I2<-mean(y2)
I1
I2
var(y1)
var(y2)

## -----------------------------------------------------------------------------
l<-seq(1,5,0.01)
plot(l,1-exp(1/2-l^2/2),xlab="x",ylab="F(x)",main = paste("CDF of importance functions f1(x)"))

## -----------------------------------------------------------------------------
n<-1e6
K<-3
N<-50
Y<-numeric(K)
est <-matrix(0, N, 2)
F<-function(k) 
  {   
    if(k<=2)  
    return(c(1-exp(1/2-k^2/2),1-exp(1/2-(k+1)^2/2)))   
     else   
    return(c(1-exp(1/2-k^2/2),0.9999))
}
for(j in 1:N){
  x<-sqrt(-2*log((1-runif(n))/exp(1/2)))
  y<-x/(2*pi*exp(1))^(1/2)
for (i in 1:K){
  u<-runif(n*(F(i)[2]-F(i)[1]))
  u1<-u*(F(i)[2]-F(i)[1])+F(i)[1]
  x1<-sqrt(-2*log((1-u1)/exp(1/2)))
  y1<-(F(i)[2]-F(i)[1])*x1/(2*pi*exp(1))^(1/2)
  Y[i]<-mean(y1)
}
  est[j,1]<-mean(y)
  est[j,2]<-sum(Y)
}
apply(est,2,mean)
apply(est,2,sd)

## -----------------------------------------------------------------------------
library(ggplot2)
set.seed(123)
M<-100
n <- 20
alpha <- .05
U<-U1<-matrix(ncol=2)
for (i in 1:M){
x <- rnorm(n, mean=0, sd=2)
U<-rbind(U,c((n-1) * var(x) / qchisq(alpha/2, df=n-1),(n-1) * var(x) / qchisq(1-alpha/2, df=n-1)))
}
U<-U[-1,]
c<-sum(((U[,1]-4)*(U[,2]-4))<0)/100
O<-as.data.frame(cbind(1:M,U))
O<-as.data.frame(cbind(4,O))
colnames(O)<-c("Variance","Experiment","upper_bound","down_bound")
par(mfrow=c(2,1))
ggplot(O, aes(x = Experiment,y=Variance )) +
geom_ribbon(aes(ymin =O[,4], ymax=O[,3]),alpha=0.5)+
geom_line()

## -----------------------------------------------------------------------------

for (i in 1:M){ 
  x <- rnorm(n, mean=0, sd=2)
  U1<-rbind(U1,c((n-1) * var(x) / qchisq(alpha, df=n-1),(n-1) * var(x) / qchisq(1-alpha, df=n-1))) 
  } 
U1<-U1[-1,] 
c<-sum((U1[,1]-4)>0)/100
O1<-as.data.frame(cbind(1:M,U1)) 
O1<-as.data.frame(cbind(4,O1)) 
colnames(O1)<-c("Variance","Experiment","Confidence upper bound","Confidence down bound")
ggplot(O1, aes(x = Experiment,y=Variance )) + 
geom_ribbon(aes(ymin =0, ymax=U1[,1]),alpha=0.5)+ 
geom_line()

## -----------------------------------------------------------------------------
library(ggplot2) 
M<-100 
n <- 10
alpha <- .05 
U<-U1<-matrix(ncol=2) 
for (i in 1:M){ 
  x<-rchisq(n,df=2) 
  U<-rbind(U,c(mean(x)+qt(1-alpha/2,df=n-1)*sqrt(var(x)/n),mean(x)-qt(1-alpha/2,df=n-1)*sqrt(var(x)/n)))
  } 
U<-U[-1,] 
c<-sum(((U[,1]-2)*(U[,2]-2))<0)/100  
O<-as.data.frame(cbind(1:M,U)) 
O<-as.data.frame(cbind(2,O)) 
colnames(O)<-c("Variance","Experiment","upper_bound","down_bound") 
ggplot(O, aes(x = Experiment,y=Variance )) + 
geom_ribbon(aes(ymin =O[,4], ymax=O[,3]),alpha=0.5)+ 
geom_line()

## -----------------------------------------------------------------------------
estimate_q<-function(n){
M<-1000
P<-numeric(M)
for (i in 1:M){
x<-rnorm(n)
b1<-mean(x^3)
P[i]<-b1
}
q<-c(0.025,0.05,0.95,0.975)
sd<-numeric(4)
f_q<-function(q) {   
  x<-qnorm(q,0,sd=sqrt(6*(n-2)/((n+1)*(n+3)))) 
return(dnorm(x,0,sqrt(6*(n-2)/((n+1)*(n+3)))))
}
sd<-sqrt(q*(1-q)/(n*f_q(q)^2))
P<-quantile(P,q)
P1<-qnorm(q,0,sqrt(6/n))
names(P)<-NULL
e<-matrix(0,4,3)
e[,1]<-P
e[,2]<-P1
e[,3]<-sd
return(e)
}

## ----results='asis'-----------------------------------------------------------
N<-c(10,20,100,1000,10000)
for (i in N){
m<-estimate_q(i)
rownames(m)<-c("2.5%","5%","95%","97.5%")
colnames(m)<-c("Mc estimation","normal estimation","Standard error")
p<-knitr::kable(m)
print(p,type="latex")
}



## -----------------------------------------------------------------------------
set.seed(1)
f<-function(pdf,epsilon,I){
n<-100
cv <- qnorm(.975, 0, sqrt(6/n))
M<-1e3
p<-numeric(M)
power<-numeric(length(epsilon))
for (j in 1:length(epsilon)){
for (i in 1:M){
J<-sample(1:2,n,replace = TRUE,prob=c(1-epsilon[j],epsilon[j]))
if (pdf==1){
x<-rbeta(n,1,1)*(J==1)+rbeta(n,10,10)*(J==2)*I(I==0)+rbeta(n,1,100)*(J==2)*I(I==1)
}
if (pdf ==2){
x<-rt(n,5)*(J==1)+rbeta(n,5,5)*(J==2)
}
xbar <- mean(x) 
m3 <- mean((x - xbar)^3)
m2 <- mean((x - xbar)^2)
t<-abs(m3/(m2^1.5))
p[i]<-(t>cv)
}
  power[j]<-sum(p)/M
}
return(power)
}
epsilon<-seq(0,1,length.out = 30)

## -----------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(epsilon,f(1,epsilon,0),type="l",ylab="power",main="contaminated by beta(10,10) ")
plot(epsilon,f(1,epsilon,1),type="l",ylab="power",main="contaminated by beta(1,100) ")

## -----------------------------------------------------------------------------
plot(epsilon,f(2,epsilon),type="l",ylab="power")

## ----results='asis'-----------------------------------------------------------
M<-1e4
n<-20
mu<-1
#p<-numeric(3)
result<-matrix(0,M,4)
cv<-qt(.975,n-1)
for (i in 1:M){
x1<-rchisq(n,1)
x2<-runif(n,0,2)
x3<-rexp(n,1)
x4<-rnorm(n,1,1)
result[i,1]<-abs(mean(x1-mu)/sqrt(var(x1)/n))>cv
result[i,2]<-abs(mean(x2-mu)/sqrt(var(x2)/n))>cv
result[i,3]<-abs(mean(x3-mu)/sqrt(var(x3)/n))>cv
result[i,4]<-abs(mean(x4-mu)/sqrt(var(x4)/n))>cv
}
p<-t(as.matrix(apply(result,2,mean)))
rownames(p)<-"empirical Type I error"
colnames(p)<-c("chi","uniform","Exponential","normal")
m<-knitr::kable(p)
print(m,type="latex")

## -----------------------------------------------------------------------------
library(bootstrap)
data(scor)
#par(mfrow=c(2,2))
plot(scor$mec,scor$vec);plot(scor$alg,scor$ana);plot(scor$alg,scor$sta);plot(scor$ana,scor$sta)
cor(scor)

## ----results='asis'-----------------------------------------------------------
p<-cor(scor)
p_hat<-c(p[1,2],p[3,4],p[3,5],p[4,5])
B<-2000
n<-dim(scor)[1]
p_boot<-matrix(0,B,4)
for ( i in 1:B){
  n<-sample(1:88,88,replace = TRUE)
  p<-cor(scor[n,])
  p_boot[i,]<-c(p[1,2],p[3,4],p[3,5],p[4,5])
}
sd_hat<-function(p_boot){
  apply(p_boot,2,function(x) sqrt(var(x)))
}
p<-t(as.matrix(sd_hat(p_boot)))
rownames(p)<-"standard error"
colnames(p)<-c("12","34","35","45")
print(knitr::kable(p),type="latex")

## ----results='asis'-----------------------------------------------------------
set.seed(01)
n<-20
B<-1e3
M<-1e3
alpha<-.05
ske_hat<-function(x) mean((x-mean(x))^3)/((n-1)*var(x)/n)^(3/2)
#x<-rnorm(n)
#y<-rchisq(n,5)

CI_est<-function(x,y){
ske_boot<-matrix(0,B,2)
##  Bootstrap estimation of skewness
for (i in 1:B){
  n1<-sample(1:n,n,replace = TRUE)
  x1<-x[n1]
  y1<-y[n1]
  ske_boot[i,]<-c(ske_hat(x1),ske_hat(y1))
}
##Normal Interval
sd<-sd_hat(ske_boot)
CI<-list()
CI$NI_x<-c(ske_hat(x)-qnorm(1-alpha/2)*sd[1],ske_hat(x)+qnorm(1-alpha/2)*sd[1])
CI$NI_y<-c(ske_hat(y)-qnorm(1-alpha/2)*sd[2],ske_hat(y)+qnorm(1-alpha/2)*sd[2])
##Percentile Interval
CI$PI_x<-quantile(ske_boot[,1],c(alpha/2,1-alpha/2))
CI$PI_y<-quantile(ske_boot[,2],c(alpha/2,1-alpha/2))
## Basic Interval
CI$BI_x<-c(2*ske_hat(x)-CI$PI_x[2],2*ske_hat(x)-CI$PI_x[1])
CI$BI_y<-c(2*ske_hat(y)-CI$PI_y[2],2*ske_hat(y)-CI$PI_y[1])
return (CI)
}
chi_ske<-4/sqrt(10)
rc<-matrix(0,M,6)
re<-matrix(0,M,6)
for (i in 1:M){
  x<-rnorm(n)
  y<-rchisq(n,5)
  CI<-CI_est(x,y)
  rc[i,1]<-0>=CI$NI_x[1]&&0<=CI$NI_x[2]
  rc[i,2]<-0>=CI$PI_x[1]&&0<=CI$PI_x[2]
  rc[i,3]<-0>=CI$BI_x[1]&&0<=CI$BI_x[2]
  rc[i,4]<-chi_ske>=CI$NI_y[1]&&chi_ske<=CI$NI_y[2]        
  rc[i,5]<-chi_ske>=CI$PI_y[1]&&chi_ske<=CI$PI_y[2]   
  rc[i,6]<-chi_ske>=CI$BI_y[1]&&chi_ske<=CI$BI_y[2]
}
p<-apply(rc,2,mean)
m<-matrix(p,2,3,byrow=TRUE)
rownames(m)<-c("normal","chi(5)")
colnames(m)<-c("Normal Interval","Percentile Interval","Basic Interval")
print(knitr::kable(m),type="latex")

## -----------------------------------------------------------------------------
set.seed(1)
library(bootstrap)
theta_hat<-function(x){
  n<-dim(x)[1]
  y<-(n/(n-1))*cov(x)
  lambda<-eigen(y)$values
  theta=lambda[1]/sum(lambda)
  return(theta)
}

## -----------------------------------------------------------------------------
n_row<-nrow(scor)
## Get estimation vector via jackknife. 
theta_j<-vector(length=n_row)
for(i in 1:n_row){
  x<-scor[-i,]
  theta_j[i]<-theta_hat(x)
}
## Estimate bias of theta_hat
bias_hat_jack<-(n_row-1)*(mean(theta_j)-theta_hat(scor))
bias_hat_jack
## Estimate standard deviation of theta_hat
se_hat_jack<-sqrt((n_row-1)/n_row*sum((theta_j-mean(theta_j))^2))
se_hat_jack

## -----------------------------------------------------------------------------
set.seed(0)
## Get estimation vector via bootstrap
B<-2000
theta_b<-vector(length=B)
for (b in 1:B){
  i <- sample(1:n_row, size = n_row, replace = TRUE)
  x<-scor[i,]
  theta_b[b]<-theta_hat(x)
}
## Estimate deviation of theta_hat
bias_hat_boot<-(mean(theta_b)-theta_hat(scor))
bias_hat_boot
## Estimate standard deviation of theta_hat
se_hat_boot<-sqrt(1/B*sum((theta_b-mean(theta_b))^2))
se_hat_boot

## -----------------------------------------------------------------------------
library(DAAG)
magnetic<-ironslag$magnetic
chemical<-ironslag$chemical
n <- length(magnetic) #in DAAG ironslag
e1 <- e2 <- e3 <- e4 <- numeric(n)

# for n-fold cross validation
# fit models on leave-one-out samples
for (k in 1:n) {
y <- magnetic[-k]
x <- chemical[-k]

J1 <- lm(y ~ x)
yhat1 <- predict.lm(J1,newdata = data.frame(x=chemical[k]))
e1[k] <- magnetic[k] - yhat1


J2 <- lm(y ~ x + I(x^2))
yhat2<-predict.lm(J2,newdata = data.frame(x=chemical[k]))
e2[k] <- magnetic[k] - yhat2

J3 <- lm(log(y) ~ x)
logyhat3 <-predict.lm(J3,newdata = data.frame(x=chemical[k]))
yhat3 <- exp(logyhat3)
e3[k] <- magnetic[k] - yhat3


J4 <- lm(y ~ x+I(x^2)+I(x^3))
yhat4<-predict.lm(J4,newdata = data.frame(x=chemical[k]))
e4[k] <- magnetic[k] - yhat4

}
c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))
J <- lm(magnetic ~ chemical+I(chemical^2)+I(chemical^3))
J

## -----------------------------------------------------------------------------
library(caret)
r<-numeric(4)
f<-function(x,k) 1-((n-1)*(1-x))/(n-k)
y <- magnetic
x <- chemical
J1 <- lm(y ~ x)
yhat1 <- predict.lm(J1,newdata = data.frame(x=x))
r[1]<-f(R2(y,yhat1),2)

J2 <- lm(y ~ x + I(x^2))
yhat2<-predict.lm(J2,newdata = data.frame(x=x))
r[2]<-f(R2(y,yhat2),3)

J3 <- lm(log(y) ~ x)
logyhat3 <-predict.lm(J3,newdata = data.frame(x=x))
yhat3 <- exp(logyhat3)
r[3]<-f(R2(y,yhat3),2)

J4 <- lm(y ~ x+I(x^2)+I(x^3))
yhat4<-predict.lm(J4,newdata = data.frame(x=x))
r[4]<-f(R2(y,yhat4),4)
r

## -----------------------------------------------------------------------------
library(boot)
set.seed(11111)
count5test <- function(z,ix,sizes) {
n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
if(is.vector(z)) z <- data.frame(z,0);z <- z[ix, ];
X<-z[1:n1,1]
Y<-z[(n1+1):n,1]
X <- X - mean(X)
Y <- Y - mean(Y)
outx <- sum(X > max(Y)) + sum(X < min(Y))
outy <- sum(Y > max(X)) + sum(Y < min(X))
return(max(c(outx, outy)))
}
mu1<-1
mu2<-0
sigma1<-2
sigma2<-2
n1<-10
n2<-20
x<-rnorm(n1,mu1,sigma1)
y<-rnorm(n2,mu2,sigma2)
z<-c(x,y)
N<-c(n1,n2)
boot.obj<-boot(z,statistic =count5test,R=999,sim="permutation",sizes=N)
ts <- c(boot.obj$t0,  boot.obj$t)
mean(boot.obj$t>=ts[1])

## -----------------------------------------------------------------------------
library(mvtnorm)
library(Ball)
library(boot)
dCov <- function(x, y) {
x <- as.matrix(x); y <- as.matrix(y)
n <- nrow(x); m <- nrow(y)
if (n != m || n < 2) stop("Sample sizes must agree")
if (! (all(is.finite(c(x, y)))))
stop("Data contains missing or infinite values")
Akl <- function(x) {
d <- as.matrix(dist(x))
m <- rowMeans(d); M <- mean(d)
a <- sweep(d, 1, m); b <- sweep(a, 2, m)
b + M
}
A<- Akl(x); B <- Akl(y)
sqrt(mean(A * B))
}


ndCov2 <- function(z, ix, dims) {
#dims contains dimensions of x and y
p <- dims[1]
q <- dims[2]
d <- p + q
x <- z[ , 1:p] #leave x as is
y <- z[ix, -(1:p)] #permute rows of y
return(nrow(z) * dCov(x, y)^2)
}

# caculate the power with sample number n
power<-function(n,alpha){
  power_hat<-numeric(4)
  p_hat<-matrix(0,100,4)
  for (i in 1:100){
  set.seed(i*10)
  x1<-rmvnorm(n,c(0,0),diag(c(1,1)))
  e1<-rmvnorm(n,c(0,0),diag(c(1,1)))
  Y1<-x1/4+e1
  Y2<-x1*e1
  z1<-cbind(x1,Y1)
  z2<-cbind(x1,Y2)
  boot.obj <- boot(data = z1, statistic = ndCov2, R = 999,
  sim = "permutation", dims = c(2, 2))
  tb <- c(boot.obj$t0, boot.obj$t)
  p.cor <- mean(tb>=tb[1])
  p.ball <- bcov.test(z1[,1:2],z1[,3:4],R=999)$p.value

  boot.obj <- boot(data = z2, statistic = ndCov2, R = 999,
  sim = "permutation", dims = c(2, 2))
  tb <- c(boot.obj$t0, boot.obj$t)
  p.cor1 <- mean(tb>=tb[1])
  p.ball1 <- bcov.test(z2[,1:2],z2[,3:4],R=999)$p.value
  p_hat[i,]<-c(p.cor,p.ball,p.cor1,p.ball1)

}
  power_hat<-colMeans(p_hat<alpha)
  return(power_hat)
}

alpha<-0.05
power1<-matrix(0,10,4)
for(i in 1:10){
  power1[i,]<-power(i*10,alpha)


}
#par(mfrow=c(2,2))
plot(10*(1:10),power1[,1],type = "l",xlab = "n",ylab = "Power",main = "Model 1 with DC test")
plot(10*(1:10),power1[,2],type = "l",xlab = "n",ylab = "Power",main = "Model 1 with Ball test")
plot(10*(1:10),power1[,3],type = "l",xlab = "n",ylab = "Power",main = "Model 2 with DC test")
plot(10*(1:10),power1[,4],type = "l",xlab = "n",ylab = "Power",main = "Model 2 with Ball test")

## -----------------------------------------------------------------------------
f<-function(x) 1/2*exp(-abs(x))
#g<-function(x,sigma) dnorm(x,sd=sigma)
Rwms<-function(sigma,x0,N){
  x<-numeric(N)
  x[1]<-x0
  u<-runif(N)
   k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= ( f(y)/f(x[i-1]) ))
     x[i] <- y else {
     x[i] <- x[i-1]
     k <- k + 1
   }
  }
   plot(1:N,x,type="l",xlab = paste("sigma=",sigma),main=paste("acceptance rates=",(N-k)/N))
   abline(h=c(log(.05/2),-log(.05/2)))
   c<-list(x=x, k=k)
}
# par(mfrow=c(2,2))
#  sigma<-c(0.05,0.5,2,16)
#  for (i in 1:4){
#   Rwms(sigma[i],25,2000) 
 # }
 

## -----------------------------------------------------------------------------
x<-c(2*(1:10),1000)
x1<-exp(log(x))
x2<-log(exp(x))
x1==x2
mapply(function(x,y) isTRUE(all.equal(x,y)),x=x1,y=x2)   



## -----------------------------------------------------------------------------
library(rootSolve)
k<-c(4:25,100,500,1000)
f<-function(x,k){
 pt(sqrt(x^2*k/(k+1-x^2)),df=k)-pt(sqrt(x^2*(k-1)/(k-x^2)),df=k-1)}
s<-sapply(k, function(k) {
  g<-function(x){
 return(pt(sqrt(x^2*k/(k+1-x^2)),df=k)-pt(sqrt(x^2*(k-1)/(k-x^2)),df=k-1))}
  uniroot(g,interval = c(1e-6,sqrt(k)-1e-6))$root
  })
s
# mapply(f,x=s,k=k)



## -----------------------------------------------------------------------------
inte<-function(x,k){
  c<-2*gamma(k/2)/(sqrt(pi*(k-1))*gamma((k-1)/2))
  f<-function(u) (1+u^2/(k-1))^(-k/2)
  d<-c*integrate(f,0,sqrt(x^2*(k-1)/(k-x^2)),rel.tol=.Machine$double.eps^0.25)$value
  return(d)
}
k1<-4:25
s1<-sapply(k1, function(k) {
  g<-function(x) inte(x,k+1)-inte(x,k)
  uniroot(g,interval = c(1e-5,sqrt(k)-0.15))$root
  })
s1
k2<-100
s2<-sapply(k2, function(k) {
  g<-function(x) inte(x,k+1)-inte(x,k)
  uniroot(g,interval = c(1,sqrt(k)-3))$root
  })
s2

## ----echo=FALSE---------------------------------------------------------------
    dat <- rbind(Genotype=c('AA','BB','OO','AO','BO','AB','Sum'),
                 Frequency=c('p^2','q^2','r^2','2pr','2qr','2pq',1),
                 Count=c('nAA','nBB','nOO','nAO','nBO','nAB','n'))
    knitr::kable(dat,format='latex')

## -----------------------------------------------------------------------------
# p<-c(1/2,1/4,1/4)
# f<-function(x){
#   (c(x[1]^2,x[2]^2,x[3]^2,2*x[1]*x[3],2*x[2]*x[3],2*x[1]*x[3]))}
# d<-sample(1:6,150,replace = TRUE,prob=f(p))
# e<-sapply(1:6,function(x,d) {length(which(d==x))},d=d)
#nA<-e[1]+e[4];nB<-e[2]+e[5];nOO<-e[3];nAB<-e[6]
nA<-28;nB<-24;nOO<-41;nAB<-70
theta<-c(1/3,1/3,1/3);v<-1;L<-numeric(1)
n<-nA+nB+nOO+nAB
while (v<50){
  a1<-(theta[1]^2/((theta[1]^2)+2*theta[1]*theta[3]))
  a<-nA*(a1+1)+nAB
  b1<-(theta[2]^2/((theta[2]^2)+2*theta[2]*theta[3]))
  b<-nB*(b1+1)+nAB
  c<-2*nOO+nA+nB-a1*nA-b1*nB
  theta1<-c(a,b,c)/sum(c(a,b,c))
  l<-a*log(theta1[1])+b*log(theta1[2])+c*log(theta1[3])+log(2)*nA*(1-a1)+log(2)*nB*(1-b1)+log(factorial(n))-log(factorial(floor(nA*a1))*factorial(floor(nA-nA*a1))*factorial(floor(nB*b1))*factorial(floor(nB-nB*b1))*factorial(nAB)*factorial(nOO))
  L<-c(L,l)
  theta<-theta1
  v<-v+1
}
theta
plot(L[2:20],xlab="n",ylab="Likelihood value",type="b")

## -----------------------------------------------------------------------------
data("mtcars")
formulas <- list(    
  mpg ~ disp,     
  mpg ~ I(1 / disp),    
  mpg ~ disp + wt,    
  mpg ~ I(1 / disp) + wt    
)   
##use loops
result3.1<-list()
v<-1
for (i in formulas){
  fit<-lm(formula = i,data=mtcars)
  result3.1[[v]]<-fit
  v<-v+1
}
#use lapply
result3.2<-lapply(formulas, function(formulas){
  lm(formula = formulas,data = mtcars)
})


## -----------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {    
  rows <- sample(1:nrow(mtcars), rep = TRUE)     
  mtcars[rows, ]     
})  
## use for loop
result4.1<-list()
for (i in 1:10){
result4.1[[i]]<-lm(formula=mpg~disp,data=bootstraps[[i]])
  
}
## use lapply
result4.2<-lapply(bootstraps,function(x) lm(formula=mpg~disp,data=x))

## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared 
R_3.1<-lapply(result3.1, rsq)
R_3.2<-lapply(result3.2, rsq)
R_4.1<-lapply(result4.1, rsq)
R_4.2<-lapply(result4.2, rsq)

## -----------------------------------------------------------------------------
trials <- replicate(100, t.test(rpois(10, 10), rpois(7, 10)), simplify = FALSE) 
p1<-sapply(trials, function(x) x$p.value)
p2<-numeric(100)
for (i in 1:100){
p2[i]<-trials[[i]]$p.value
}

## -----------------------------------------------------------------------------
library(parallel)
f<-function(x) c(mean(x),sd(x),mad(x))
g<-function(x) x^2
x<-list(1:10,20:30,c(TRUE,FALSE,TRUE,TRUE))
# mcsapply()
mcsapply<-function(k,f){
  cl <- makeCluster(4)
  result<-parLapply(cl,k,f)
  stopCluster(cl) 
  return(unlist(result))
}
system.time(cc<-mcsapply(1:100,g))  
vapply(x,f,c(1,2,3))

## -----------------------------------------------------------------------------
#  R function
library(Rcpp)
library(microbenchmark)
Rwms<-function(sigma,x0,N){
x<-matrix(NA,N,2);x[1,]<-c(x0,0);u<-runif(N)
for (i in 2:N) {
y <- rnorm(1, x[i-1,1], sigma)
c1=1/2*exp(-abs(y))
c2=1/2*exp(-abs(x[i-1,1]))
if (u[i] <= ( c1/c2 )){
x[i,1] <- y ;x[i,2]<-1}
else {
x[i,1] <- x[i-1,1];x[i,2]<-0
}
}
return(x)
}
# Cpp function
cppFunction('NumericMatrix RwmsC(double sigma, double x_0,int N) {
  NumericMatrix x(N,2);
  NumericVector U=runif(N,0,1);
  x(0,0)=x_0;x(0,1)=0; 
  double y=0;double u=0;double c1;double c2;
  for (int i=1;i < N;i++){
    y = rnorm(1,x(i-1,0),sigma)[0];
    u = U[i];
    c1=0.5*exp(-abs(y));
    c2=0.5*exp(-abs(x(i-1,0)));
    if (u <= c1/c2){
      x(i,0) = y;
      x(i,1)=1;
    }
    else{
      x(i,0)=x(i-1,0);
      x(i,1)=0;
    }
    }
  return x;
  }')
# Question one:
ts <- microbenchmark(R=Rwms(1,25,10000), Cpp=RwmsC(1,25,10000))
summary(ts)[,c(1,3,5,6)]
# Question two:
set.seed(13)
c<-Rwms(1,5,10000)
R<-c[which(c[,2]==1),1]
c1<-RwmsC(1,5,10000)
Cpp<-c1[which(c[,2]==1),1]
qqplot(R,Cpp)
points(x=seq(-5,10,length.out = 100),y=seq(-5,10,length.out = 100),type = "l",col=2)
# Result
#par(mfrow=c(1,2))
hist(R)
hist(Cpp)

