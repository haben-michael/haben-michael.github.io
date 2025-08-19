


## 1. IPW

n <- 1e4
X <- rnorm(n)
pi <- plogis(X)
A <- rbinom(n,1,pi)
Y <- X + rnorm(n)

mean(Y)
## mean(Y[A==1]/pi[A==1])
mean(Y/pi*(A==1))
mean(Y[A==1])

MSEs.by.n <- sapply(seq(3,log(1e5),length.out=10),function(logn) {
    n <- round(exp(logn))
    MSEs <- replicate(1e2,{
        X <- rnorm(n)
        pi <- plogis(X)
        A <- rbinom(n,1,pi)
        Y <- X + rnorm(n)
        (c(mean(Y/pi*(A==1)),mean(Y[A==1])) - mean(Y))^2
    })
    ## rowMeans(MSEs)
    ## MSEs <- MSEs^2
    rowMeans(MSEs)
})
plot(MSEs.by.n[1,],type='l',ylim=range(MSEs.by.n),xlab='log sample size',ylab='MSE')
lines(MSEs.by.n[2,],type='l',col='red')
legend('topright',col=c('black','red'),lty=1,legend=c('IPW','unweighted'))



## 2. individualized treatment regime--MCAR

n <- 1e3
X <- rnorm(n)
pi <- 1/2
A <- rbinom(n,1,pi)
D <- as.numeric(plogis(X)>.2)
Y <- D + rnorm(n) # target Y(D(X))

mean(Y)
mean(Y[A==D])
mean((Y/(pi^A*(1-pi)^(1-A)))*(A==D))

MSEs.by.n <- sapply(seq(5,log(1e5),length.out=10),function(logn) {
    n <- round(exp(logn))
    MSEs <- replicate(1e2,{
        X <- rnorm(n)
        pi <- 1/7
        A <- rbinom(n,1,pi)
        D <- as.numeric(plogis(X)>.2)
        Y <- D + rnorm(n) # target Y(D(X))
        (c(mean(Y/(pi^A*(1-pi)^(1-A))*(A==D)),mean(Y[A==D])) - mean(Y))^2
    })
    ## rowMeans(MSEs)
    ## MSEs <- MSEs^2
    rowMeans(MSEs)
})
plot(MSEs.by.n[1,],type='l',ylim=range(MSEs.by.n),xlab='log sample size',ylab='MSE')
lines(MSEs.by.n[2,],type='l',col='red')
legend('topright',col=c('black','red'),lty=1,legend=c('IPW','unweighted'))



## 2. individualized treatment regime--MAR

n <- 1e3
X <- rnorm(n)
pi <- plogis(X) ## only difference from MCAR code
A <- rbinom(n,1,pi)
D <- as.numeric(3*X-2 > 0)
Y <- D + rnorm(n) # target Y(D(X))

mean(Y)
mean(Y[A==D])
mean((Y/(pi^A*(1-pi)^(1-A)))*(A==D))

MSEs.by.n <- sapply(seq(5,log(1e5),length.out=10),function(logn) {
    n <- round(exp(logn))
    MSEs <- replicate(1e2,{
        X <- rnorm(n)
        pi <- plogis(X)
        A <- rbinom(n,1,pi)
        D <- as.numeric(3*X-2 > 0)
        Y <- D + rnorm(n) # target Y(D(X))
        (c(mean(Y/(pi^A*(1-pi)^(1-A))*(A==D)),mean(Y[A==D])) - mean(Y))^2
    })
    ## rowMeans(MSEs)
    ## MSEs <- MSEs^2
    rowMeans(MSEs)
})
plot(MSEs.by.n[1,],type='l',ylim=range(MSEs.by.n),xlab='log sample size',ylab='MSE')
lines(MSEs.by.n[2,],type='l',col='red')
legend('topright',col=c('black','red'),lty=1,legend=c('IPW','unweighted'))



## 3. SVM approach

require(quadprog)
n <- 3e2
p <- 1
beta <- matrix(runif(p)+1,ncol=1)
kappa <- 1e-2
ans <- replicate(1e2,{
    X <- matrix(rnorm(n*p),ncol=p)
    pi <- 1/5
    A <- rbinom(n,1,pi)
    A <- 2*A-1
    D <- as.numeric(X%*%beta > 0)
    D <- 2*D-1
    Y <- 1 + D*0 + rexp(n)
    
    Dmat <- (A*X)%*%t(A*X)
    dvec <- matrix(1,nrow=n,ncol=1)
    Amat1 <- matrix(A,nrow=1)
    Amat2 <- diag(1,n)
    Amat3 <- diag(-1,n)
    Amat <- t(rbind(Amat1,Amat2,Amat3))
    bvec1 <- 0
    bvec2 <- matrix(0,nrow=n,ncol=1)
    bvec3 <- -matrix(kappa*Y/(A*pi + (1-A)/2),ncol=1)
    bvec <- rbind(bvec1,bvec2,bvec3)
    alpha.hat <- solve.QP(Dmat=Dmat+1e-4*diag(n),dvec=dvec,Amat=Amat,bvec=bvec,meq=1)$solution

    beta.hat <- sum((alpha.hat*A*X)[alpha.hat>0,])
    D.hat <- as.numeric(X%*%beta.hat > 0)
    D.hat <- 2*D.hat-1
    fpr <- mean(D.hat[D==-1]==1)
    fnr <- mean(D.hat[D==1]==-1)
    c(beta.hat=beta.hat,fpr=fpr,fnr=fnr)
})

op <- par(mfrow=c(1,3))
hist(ans['beta.hat',]); abline(v=beta,col='red')
hist(ans['fpr',])
hist(ans['fnr',])
par(op)
