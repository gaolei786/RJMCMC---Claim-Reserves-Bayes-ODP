
library(LearnBayes)
library(ggplot2)

logpos <- function(alpha_beta, X, I, k, u.new, a1, b1, lambda, tau, phi) {
  alpha <- alpha_beta[1]
  beta <- alpha_beta[2]
  fun1 <- function(j) {
    -exp(alpha - beta * j) * sum(u.new[1:(I-j+1)]) / phi +
    (alpha - beta * j) * sum(X[1:(I-j+1), j+1]) / phi
  }
  fun2 <- function(alpha, a1, lambda, beta, b1, tau) {
    - (alpha - a1)^2 / (2 * lambda^2) - (beta - b1)^2 / (2 * tau^2)
  } 
    
  sum(sapply(k:I, fun1)) + fun2(alpha, a1, lambda, beta, b1, tau)
}

move.alpha.beta <- function(m, start, X, I, k, u.new, a1, b1, lambda, tau, phi) {
  proposal <- list(scale = 0.13, var = array(c(1, 0, 0, 1), c(2, 2)))
  #m <- 10000
  r <- rwmetrop(logpos, proposal, start, m, X, I, k, u.new, a1, b1, lambda, tau, phi)
  return(r$par[1:m, ])
  #plot(density(r$par))
  #curve(dnorm(x, -1, 10), from = -2.5, to = 0.5, add = T)
  #mean(r$par[1000:10000, 1])
  #mean(r$par[1000:10000, 2])
}

move.u.r <- function(alpha, beta, u, k, r, I, X, phi) {
  fun1 <- function(i) {
    s.post <- s + 1 / phi * sum(X[i+1, 1:(I-i+1)]) 
    s.m <- s / m[i + 1] + 1 / phi * sum(r[1:(I-i+1)])
    rgamma(1, shape = s.post, rate = s.m)
  } 
  fun2 <- function(j) {
    v.post <- v + 1 / phi * sum(X[1:(I-j+1), j+1])
    v.c <- v / c[j+1] + 1 / phi * sum(u.new[1:(I-j+1)])
    rgamma(1, shape = v.post, rate = v.c)
  }
  u.new <- sapply(0:I, fun1)
  r.new1 <- sapply(0:(k - 1), fun2)
  
  alpha.beta <- move.alpha.beta(1, c(alpha, beta), X, I, k, u.new, a1, b1, lambda, tau, phi)
  r.new2 <- exp(alpha.beta[1] - alpha.beta[2]*(k:I))
  r.new <- c(r.new1, r.new2)
  return(list(u = u.new, r = r.new, alpha.beta = alpha.beta))
}
birth <- function(k) {
  if (k >=1 & k <= (I-1)) {
    return(1/3)
  } else { 
    return(0)
  }
}
death <- function(k) {
  if (k >1 & k <= I) {
    return(1/3) 
  } else {
    return(0)
  }
}
move <- function(n, k.start, r.start, u.start, alpha.start, beta.start) {
  R <- array(0, c(n, 22))
  U <- array(0, c(n, 22))
  Alpha.Beta <- array(0, c(n, 2))
  K <- array(0, n)
  R[1, ]<- r.start
  U[1, ] <-  u.start
  K[1] <- k.start
  Alpha.Beta[1, ] <- c(alpha.start, beta.start)
  for (d in 2:n) {
    type <- runif(1)
    if (type < 1-birth(K[d-1])-death(K[d-1])) {
      RU <- move.u.r(Alpha.Beta[d-1, 1], Alpha.Beta[d-1, 2], U[d-1, ], 
        K[d-1], R[d-1,], I, X, phi)
      R[d, ] <- RU$r
      U[d, ] <- RU$u
      K[d] <- K[d-1]
      Alpha.Beta[d, ]  <- RU$alpha.beta
    } else if (type > 1-birth(K[d-1])-death(K[d-1]) & type < 1-death(K[d-1]) ) {
      v.star <- 100
      u.old <- U[d-1, ]
      r.old <- R[d-1, ]
      k.old <- K[d-1]
      alpha.beta.old <- Alpha.Beta[d-1, ]
      
      u <- rgamma(1, shape = v.star, rate = v.star / r.old[k.old+1])
      r.star <- u
      
      s1 <- sum(-u.old[1:(I-k.old+1)]*r.star / phi + X[1:(I-k.old+1), k.old+1] / phi * log(r.star) +
          u.old[1:(I-k.old+1)]*r.old[k.old+1] / phi - X[1:(I-k.old+1), k.old+1]  / phi * log(r.old[k.old+1]) )
      
      s2 <- v * log(v/c[k.old+1]) + (v - 1) * log(r.star) - v * r.star / c[k.old+1] - log(gamma(v))
      s3 <- v.star * log(v.star/r.old[k.old+1]) + (v.star - 1)*log(r.star) - v.star * r.star / r.old[k.old+1] - log(gamma(v.star))
      print(s1)
      print(s2)
      print(s3)
      alpha.birth1 <- exp(s1+s2-s3)
      up <- runif(1)

      if(up < alpha.birth1) {
        r.new <- r.old
        r.new[k.old + 1] <- u
        k.new <- k.old + 1
        u.new <- u.old
        R[d, ] <- r.new
        U[d, ] <- u.new
        K[d] <- k.new
        Alpha.Beta[d, ] <- alpha.beta.old
      } else {
        R[d, ] <- r.old
        U[d, ] <- u.old
        K[d] <- k.old
        Alpha.Beta[d, ] <- alpha.beta.old
      }
    }  else {
      v.star <- 100
      u.old <- U[d-1, ]
      r.old <- R[d-1, ]
      k.old <- K[d-1]
      k.star <- k.old - 1
      alpha.beta.old <- Alpha.Beta[d-1, ]
      alpha.old <- alpha.beta.old[1]
      beta.old <- alpha.beta.old[2]
      rk.star <- exp(alpha.old - k.star * beta.old) 
      r.star <- r.old[k.star+1]
      s1 <-  sum(-u.old[1:(I-k.star+1)]*rk.star/phi +X[1:(I-k.star+1), k.star+1]/phi * log(rk.star) +
        u.old[1:(I-k.star+1)]*r.star/phi - X[1:(I-k.star+1), k.star+1]/phi *log(r.star)) 
      s2 <-  v.star*log(v.star/rk.star)+(v.star-1)*log(r.star)-v.star*r.star/rk.star-log(gamma(v.star))
      s3 <- v * log(v/c[k.star+1]) + (v-1)*log(r.star) - v*r.star/c[k.star+1]-log(gamma(v))

      up <- runif(1)
 
      if(up < exp(s1+s2-s3)) {
        u.new <- u.old
        r.new <- r.old
        r.new[k.star+1] <- rk.star
        k.new <- k.star
        R[d, ] <- r.new
        U[d, ] <- u.new
        K[d] <- k.new
        Alpha.Beta[d, ] <- alpha.beta.old 
      } else {
        R[d, ] <- r.old
        U[d, ] <- u.old
        K[d] <- k.old
        Alpha.Beta[d, ] <- alpha.beta.old
      }
    }
  }
  return(list(r = R, u = U, k = K, ab = Alpha.Beta))
}

mle <- function() {
  X <- as.matrix(read.table("data.csv", sep = ",", header = F))
  X <- X[-1, ]
  X <- X[, -1]
  y <- NULL
  X0 <- array(0, dim = c(253, 44))
  for (i in 1:22) {
    for (j in 1:(23 - i)) {
      y <- c(y, X[i, j])
      X0[length(y), c(i, 22 + j)] <- c(1, 1)
    }
  }
  coef <- exp(coefficients(glm(y ~ X0 - 1, family = poisson(link = "log"))))
  u <- coef[1:22] * sum(X[1, ]) / coef[1]
  names(u) <- NULL
  r <- c((coef[23:44] * coef[1] / sum(X[1, ]))[1:21], 1 - sum(coef[23:43] * coef[1] / sum(X[1, ])))
  names(r) <- NULL
  fit <- fitted(glm(y ~ X0 - 1, family = poisson(link = "log")))
  phi <- sum(((y - fit) / fit^0.5)^2)/(253-43)
  return(list(u = u, r = r, phi = phi))
}


X <- read.table("data.csv", sep = ",", header = T)
X <- X[, -1]
s <- 100
v <- 1
a1 <- -1
I <- 21
b1 <- 0.5
tau <- 10
v.star <- 100
u <- mle()$u
r <- mle()$r
phi <- mle()$phi  




set.seed(123) 
iteration <- 20000    
listre <- move(n = iteration, k.start=7, r.start=r, u.start=u, alpha.start=alpha, beta.start=beta) 

plot(listre$k, type = "l", xlab = "", ylab = "", col = rgb(0, 0, 0, 0.3))
table(listre$k)/length(listre$k)

plot(listre$r[, 1], type = "l", xlab = "", ylab = "")
plot(listre$u[, 20], type = "l", xlab = "", ylab = "")

re <- listre$r[listre$k==7, ]
plot(log(apply(re,2, mean)), type = "l", ylim = c(-6, -1))
lines(log(apply(re,2, mean) + 2 * apply(listre$r, 2, sd)), lty = 3)
lines(log(apply(re,2, mean) - 2 * apply(listre$r, 2, sd)), lty = 3)
points(1:22, log(apply(re,2, mean) + 2 * apply(listre$r, 2, sd)),
  pch = 4, cex = 0.5)
points(1:22, log(apply(re,2, mean) - 2 * apply(listre$r, 2, sd)),
  pch = 4, cex = 0.5)
abline(h = seq(-6, -1, length = 6), col = rgb(0, 0, 0, 0.1))


R1 <- NULL


