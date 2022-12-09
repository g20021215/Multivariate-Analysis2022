##Q5
X<-read.table("T8-4.DAT",header = FALSE)
##Hotelling T^2
S<-cov(X)
n<-nrow(X)
p<-ncol(X)
mx<-apply(X, 2, mean)
mu0<-c(0,0,0,0,0)
T<-n*t(mx-mu0)%*%solve(S)%*%(mx-mu0)
T
F<-(n-p)*T/((n-1)*p)
F
pvalue<-pf(F,p,n-p,lower.tail=FALSE)
pvalue

##95%- confidence interval for each ¦Ì_i
n<-nrow(X)
p<-ncol(X)
m<-apply(X,2,mean)
S<-cov(X)
c2<-(n-1)*p/(n*(n-p))*qf(.05,p,n-p,lower.tail = FALSE)

##CI for ¦Ì_1
a1<-c(1,0,0,0,0)
LCL_1<-t(a1)%*%m-sqrt(c2*t(a1)%*%S%*%a1)
UCL_1<-t(a1)%*%m+sqrt(c2*t(a1)%*%S%*%a1)
round(LCL_1,4)
round(UCL_1,4)

#CI for ¦Ì_2
a2<-c(0,1,0,0,0)
LCL_2<-t(a2)%*%m-sqrt(c2*t(a2)%*%S%*%a2)
UCL_2<-t(a2)%*%m+sqrt(c2*t(a2)%*%S%*%a2)
round(LCL_2,4)
round(UCL_2,4)

#for ¦Ì_3
a3<-c(0,0,1,0,0)
LCL_3<-t(a3)%*%m-sqrt(c2*t(a3)%*%S%*%a3)
UCL_3<-t(a3)%*%m+sqrt(c2*t(a3)%*%S%*%a3)
round(LCL_3,4)
round(UCL_3,4)

#for ¦Ì_4
a4<-c(0,0,0,1,0)
LCL_4<-t(a4)%*%m-sqrt(c2*t(a4)%*%S%*%a4)
UCL_4<-t(a4)%*%m+sqrt(c2*t(a4)%*%S%*%a4)
round(LCL_4,4)
round(UCL_4,4)

#for ¦Ì_5
a5<-c(0,0,0,0,1)
LCL_5<-t(a5)%*%m-sqrt(c2*t(a5)%*%S%*%a5)
UCL_5<-t(a5)%*%m+sqrt(c2*t(a5)%*%S%*%a5)
round(LCL_5,4)
round(UCL_5,4)


##95% confidence region
X<-read.table("T8-4.DAT",header = FALSE)
n<-nrow(X)
p<-ncol(X)
m<-apply(X,2,mean)
S<-cov(X)
S1<-solve(S)
c2<-(n-1)*p/(n*(n-p))*qf(.05,p,n-p,lower.tail = FALSE)
round(m,4)
round(S1,4)
round(c2,4)
##95% confidence ellipse
library(car)
windows()
dataEllipse(X$V1,X$V2,levels = 0.95,xlab = "mu1",ylab = "mu2",xlim=c(-0.06,0.06)
            ,col = "blue")


