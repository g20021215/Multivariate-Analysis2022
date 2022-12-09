getwd()
X<-read.csv("T8-4.DAT",sep="\t",dec=".")
X <- as.matrix(X)
#question_a(1).sample correlation matrix R
R<-solve(sqrt(diag(diag(S))))%*%S%*%solve(sqrt(diag(diag(S))))
R

e<-eigen(R)
#verification of total variance
sum(diag(R))
sum((e$values))
#variance contribution from each principal component PC
e$values/sum((e$values))
#covariance matrix of the PCs
E<-e$vectors
t(E)%*%S%*%E #this is the covariance matrix of the PCs
#verification of the covariance between Y=PCs and X=(X1,...,Xp)'
CV<-diag(e$values)%*%t(E)
#verification of the correlation between Y and X=(X1,...,Xp)'
Res <- solve(sqrt(diag(e$values)))%*%CV%*%solve(sqrt(diag(diag(R))))
Res <- round(Res,digits = 4)
# construct the 
c0 <- c("Y|X","Y1","Y2","Y3","Y4","Y5")
c1 <- c("X1",Res[,1])
c2 <- c("X2",Res[,2])
c3 <- c("X3",Res[,3])
c4 <- c("X4",Res[,4])
c5 <- c("X5",Res[,5])
Result <- data.frame(cbind(c0,c1,c2,c3,c4,c5))
View(Result)

