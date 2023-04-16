par(mfrow=c(1,3)) # partage l'affichage en 2
Q<-qchisq(p=seq(0.05,0.95,by=0.1),df=2)
sigma<-matrix(c(2,1,1,0.75),2,2)
Y<-matrix(rnorm(2000),1000,2)%*%chol(sigma)
plot(Y,xlab="x",ylab="y",pch='.')
  
x<-seq(-4,4,length=100)
y<-seq(-4,4,length=100)
sigmainv<-solve(sigma)
a<-sigmainv[1,1]
b<-sigmainv[2,2]
c<-sigmainv[1,2]
z<-outer(x,y,function(x,y) (a*x^2+b*y^2+2*c*x*y))
image(x,y,z)
  
contour(x,y,z,col="blue4",levels=Q,labels=seq(from=0.05,to=0.95,by=0.1),add= TRUE)
persp(x,y,1/(2*pi)*det(sigmainv)^(-1/2)*exp(-0.5*z),col="cornflowerblue")

plot(Q , )



Bell <- function(N=0) {
  B <- rep(0, times = N+1)
  B[1] <- 1 
  B[2] <- 1 
  
  for( n in 2:N){
    for(k in 1:n){
      B[n+1]<-B[n+1]+choose(n-1,k-1)*B[k]
    }
  }
  return ( B )
}

Bell(9)


library(MASS)
pairs(crabs)

pairs(crabs[4:8], main = "Morphological Measurements on Leptograpsus Crabs' Data", pch = c(21,23)[unclass(crabs$sex)], bg = c("orange", "blue")[unclass(crabs$sp)])

pairs(crabs[,4:8]/crabs[,4], main = "Morphological Measurements on Leptograpsus Crabs' Data", pch = c(21,23)[unclass(crabs$sex)], bg = c("orange", "blue")[unclass(crabs$sp)])






