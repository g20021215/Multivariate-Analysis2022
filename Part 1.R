##Q1
X<-read.table("T8-4.DAT",header = FALSE)
S<-cov(X)
e<-eigen(S)
#covariance matrix of the PCs
E<-e$vectors
#(a)
PC<-t(E)%*%S%*%E ## sample covariance matrix of PCS
PC<-round(PC,4)
View(PC)
#variance contribution from each principal component PC
var_cont<-e$values/sum((e$values))

c1<-c("Variable","JPM","Citi","Wells","Royal","Exxon",
      "Var. Contr.")
c2<-c("PC e1",round(E[,1],4),round(var_cont[1],4))
c3<-c("PC e2",round(E[,2],4),round(var_cont[2],4))
c4<-c("PC e3",round(E[,3],4),round(var_cont[3],4))
c5<-c("PC e4",round(E[,4],4),round(var_cont[4],4))
c6<-c("PC e5",round(E[,5],4),round(var_cont[5],4))
T1<-cbind(c1,c2,c3,c4,c5,c6)
T<-as.data.frame(T1)
View(T)

##b
#cumulative eigenvalue contribution
a<-e$values
s1<-sum(a)
s<-length(a)
b<-rep(0,s)
for (i in 1:s) {
  b[i]=sum(a[1:i])/s1
}
b  #the cumulative eigenvalue contribution

z1<-c("Cumulative % of totalvariance")
z2<-c(round(b[1],4))
z3<-c(round(b[2],4))
z4<-c(round(b[3],4))
z5<-c(round(b[4],4))
z6<-c(round(b[5],4))
T2<-cbind(z1,z2,z3,z4,z5,z6)
Z<-as.data.frame(T2)
View(Z)


