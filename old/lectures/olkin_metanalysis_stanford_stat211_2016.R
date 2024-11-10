## 2.6.15 Stats 211
## Meta-analysis worked examples

## 1. Inverse variance weighting
## 2. Generalized least squares -- multiple effects
## 3. Generalized least squares -- single effect

## 1. Handout: "Continuous Data -- Single Study"
## single effect size per study, inverting variance

mu.C <- c(510,383,475)
n.C <- c(34,17,52)
sd.C <- c(84,83,89)

mu.E <- c(620,446,470)
n.E <- c(21,16,52)
sd.E <- c(103,105,96)

s.sqr <- ((n.C-1)*sd.C^2 + (n.E-1)*sd.E^2)/(n.C+n.E-1)
s <- sqrt(s.sqr)

d <- (mu.C - mu.E)/s # erratum in handout

var.d <- 1/n.E + 1/n.C + d^2/(2*(n.E+n.C))

## combine standardized mean differences using inverse variance weights
inv.var <- 1/var.d
d.plus <- sum(inv.var*d)/sum(inv.var)

var.d.plus <- 1/sum(inv.var)
sd.d.plus <- sqrt(var.d.plus)
z.975 <- qnorm(.975)

d.plus + c(-1,1)*z.975*sd.d.plus # 95% CI
##  -0.67811931 -0.09278742

## Here is a wrapper for the above computations:

meta.table.fixed <- function(effect.sizes,n.C,n.E) {
    var.d <- 1/n.C + 1/n.E + d^2/(2*(n.C+n.E))
    var.d.inv <- 1/var.d
    weights <- var.d.inv/sum(var.d.inv)
    d.weighted <- effect.sizes*weights

    meta.table <- data.frame(
        n.C = n.C,
        n.E = n.E,
        d = effect.sizes,
        'var(d)' = var.d,
        '1/var(d)' = var.d.inv,
        weights = weights,
        'weighted effects' = d.weighted,
        check.names=F
    )

    row.names(meta.table) <- row.names(effect.sizes)

    sums <- apply(meta.table,2,sum)
    sums <- round(sums,3)
    meta.table <- rbind(meta.table,sums)
    rownames(meta.table)[nrow(meta.table)] <- '(sum)'
    meta.table['(sum)',c('n.C','n.E','d','var(d)')] <- NA
    meta.table <- round(meta.table,3)

    cat('\n\n')
    print(meta.table)
    cat('\n\n')
}


meta.table.fixed(d,n.C,n.E)

##       n.C n.E      d var(d) 1/var(d) weights weighted effects
## 1      34  21 -1.212  0.090   11.065   0.247           -0.299
## 2      17  16 -0.679  0.128    7.794   0.174           -0.118
## 3      52  52  0.054  0.038   25.990   0.580            0.031
## (sum)  NA  NA     NA     NA   44.849   1.000           -0.385




## an R package

#install.packages()
library(metafor)
?rma

ma0 <- rma(yi=d,n1i=n.E,n2i=n.C,vi=var.d,method="FE",measure="MD")
## This call produces the invariance weighted summary statistic, like we
## did. The routine "rma" has the advantage of also operating with
## random effects models or different primary study statistics with an
## easy change of the above parameters.


## Finally, let's also see what kind of graphical output R can produce
## for meta-analyses.
plot(ma0)
forest(ma0,slab=c('study 1','study 2','study 3'),
       main='Effect of Coaching on SAT Scores')






## GLS framework

## 2. Handout: Multiple Treatment Studies
## fixed effects model, multiple effects per study, using regression

# two auxiliary functions for the computations:

# join matrices into a block diagonal matrix
block.diagonal <- function(...) {
    Reduce(function(M,N) {
        rbind(cbind(M,matrix(0,nrow=nrow(M),ncol=ncol(N))),
              cbind(matrix(0,nrow=nrow(N),ncol=ncol(M)),N))
    },list(...))
}

A <- matrix(1:9,nrow=3)
B <- matrix(1:4,nrow=2)
block.diagonal(A,B,B)

# compute cov(d) (formula (22-5) in Gleser-Olkin handout)
meta.covariance <- function(d,N) {
  d <- na.omit(d)
  N <- na.omit(N)
  psi <- (1+(1/2)*d%*%t(d))/N[1] #note different denominator in Hedges-Olkin ch.5
  diag(psi) <- diag(psi) + 1/N[-1]
  return(psi)
}


# input effect sizes
d1 <- c(0.808,1.308,1.379,NA,NA)
d2 <- c(NA,1.266,1.828,1.962,NA)
d3 <- c(NA,1.835,NA,2.568,NA)
d4 <- c(NA,1.272,NA,NA,2.038)
d5 <- c(1.171,2.024,2.423,3.159,NA)
d6 <- c(0.681,NA,NA,NA,NA)

d <- c(d1,d2,d3,d4,d5,d6)
d <- na.omit(d)

# input sample sizes
N1 <- c(25,22,25,23,NA,NA)
N2 <- c(40,NA,38,37,40,NA)
N3 <- c(30,NA,30,NA,28,NA)
N4 <- c(50,NA,50,NA,NA,50)
N5 <- c(30,30,30,28,26,NA)
N6 <- c(100,100,NA,NA,NA,NA)

# compute covariance matrices for each study
psi1 <- meta.covariance(d1,N1)
psi2 <- meta.covariance(d2,N2)
psi3 <- meta.covariance(d3,N3)
psi4 <- meta.covariance(d4,N4)
psi5 <- meta.covariance(d5,N5)
psi6 <- meta.covariance(d6,N6)

psi <- block.diagonal(psi1,psi2,psi3,psi4,psi5,psi6)

# generate design matrix
X <- rbind(diag(d1),diag(d2),diag(d3),diag(d4),diag(d5),diag(d6))
X <- na.omit(X)
X <- as.matrix(X!=0)
X <- apply(X,2,as.numeric)

Beta.hat <- solve(t(X)%*%solve(psi)%*%X)%*%t(X)%*%solve(psi)%*%d

##           [,1]
## [1,] 0.7561631
## [2,] 1.3982469
## [3,] 1.7453657
## [4,] 2.1459786
## [5,] 2.1411986

Sigma.hat <- solve(t(X)%*%solve(psi)%*%X)

##             [,1]        [,2]        [,3]       [,4]        [,5]
## [1,] 0.013110681 0.003839496 0.005131653 0.00481786 0.003138538
## [2,] 0.003839496 0.016009493 0.011255540 0.01337402 0.013086717
## [3,] 0.005131653 0.011255540 0.027100867 0.01724424 0.009200671
## [4,] 0.004817860 0.013374016 0.017244243 0.03323099 0.010932387
## [5,] 0.003138538 0.013086717 0.009200671 0.01093239 0.054692623


## wrapper for the above computations under the fixed effects model

meta.gls <- function(effect.sizes,sample.sizes) {

  effect.sizes <- split(effect.sizes,row(effect.sizes))
  sample.sizes <- split(sample.sizes,row(sample.sizes))
  d <- as.numeric(unlist(effect.sizes))
  d <- na.omit(d)

  Psi <- mapply(meta.covariance,effect.sizes,sample.sizes,SIMPLIFY=FALSE)

  Psi <- do.call(block.diagonal,Psi)

  X <- lapply(effect.sizes,function(x)diag(x,nrow=length(x)))
  X <- do.call(rbind,X)
  X <- na.omit(X)
  X <- as.matrix(X!=0)
  X <- apply(X,2,as.numeric)

  Beta.hat <- solve(t(X)%*%solve(Psi)%*%X)%*%t(X)%*%solve(Psi)%*%d

  Sigma.hat <- solve(t(X)%*%solve(Psi)%*%X)

  return(list(Beta.hat=Beta.hat,Sigma.hat=Sigma.hat))
}


effect.sizes <- rbind(d1,d2,d3,d4,d5,d6)
sample.sizes <- rbind(N1,N2,N3,N4,N5,N6)

meta.gls(effect.sizes,sample.sizes)

## $Beta.hat
##           [,1]
## [1,] 0.7561631
## [2,] 1.3982469
## [3,] 1.7453657
## [4,] 2.1459786
## [5,] 2.1411986

## $Sigma.hat
##             [,1]        [,2]        [,3]       [,4]        [,5]
## [1,] 0.013110681 0.003839496 0.005131653 0.00481786 0.003138538
## [2,] 0.003839496 0.016009493 0.011255540 0.01337402 0.013086717
## [3,] 0.005131653 0.011255540 0.027100867 0.01724424 0.009200671
## [4,] 0.004817860 0.013374016 0.017244243 0.03323099 0.010932387
## [5,] 0.003138538 0.013086717 0.009200671 0.01093239 0.054692623


## 3. Re-doing coaching meta-analysis in GLS framework

mu.C <- c(510,383,475)
n.C <- c(34,17,52)
sd.C <- c(84,83,89)

mu.E <- c(620,446,470)
n.E <- c(21,16,52)
sd.E <- c(103,105,96)

s.sqr <- ((n.C-1)*sd.C^2 + (n.E-1)*sd.E^2)/(n.C+n.E-1)
s <- sqrt(s.sqr)

d <- (mu.C - mu.E)/s

## our routine "meta.gls" expect to be passed matrices
effect.sizes <- matrix(d,ncol=1)
sample.sizes <- matrix(c(n.C,n.E),byrow=F,ncol=2)
meta.gls(effect.sizes,sample.sizes)

## $Beta.hat
##            [,1]
## [1,] -0.3654367

## $Sigma.hat
##            [,1]
## [1,] 0.02297019

## similar to part 1
