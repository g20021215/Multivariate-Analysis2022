#PCA based on correlation matrix
X<-read.table("T8-4.DAT",header = FALSE)
S<-cov(X)
#question_a(1).sample correlation matrix R
R<-solve(sqrt(diag(diag(S))))%*%S%*%solve(sqrt(diag(diag(S))))
R
eig<-eigen(R)
#question_a(2).sample PCs
std_X<-scale(X,center = TRUE,scale = TRUE)
PC<-as.matrix(std_X)%*%eig$vectors
PC
#question_b.the proportion explained by first3 PC
p<-(eig$values[1]+eig$values[2]+eig$values[3])/sum((eig$values))
p
#fill in the table
#PCA directions
eig$vectors
#variance contribution from each principal component PC
eig$values/sum((eig$values))


