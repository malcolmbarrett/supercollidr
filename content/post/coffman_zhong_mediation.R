library(mediation)
library(twang)
data("jobs")
# mean center job_seek => job_seek.c
jobs$job_seek.c <- jobs$job_seek - mean(jobs$job_seek)
attach(jobs)
# Analyze jobs data
###########################################################################
# No interactions
# no adjustment
mod.m1 <- lm(job_seek.c ~ treat)
mod.y1 <- lm(depress2 ~ treat + job_seek.c)
# reg. adjustment
mod.y2 <- lm(depress2 ~ treat + job_seek.c + depress1 + econ_hard + sex + age
  + occp + marital + nonwhite + educ + income)
# propensity models for continuous mediator
num.mod <- lm(job_seek.c ~ treat, data = jobs)
den.mod <- lm(job_seek.c ~ treat + depress1 + econ_hard + sex + age + occp +
  marital
  + nonwhite + educ + income, data = jobs)
sigma.n <- summary(num.mod)$sigma
sigma.d <- summary(den.mod)$sigma
num.p <- dnorm(job_seek.c, mean = num.mod$fitted, sd = sigma.n)
den.p <- dnorm(job_seek.c, mean = den.mod$fitted, sd = sigma.d)
jobs$w.m <- num.p / den.p
# MSM - with robust SE
design.ps <- svydesign(ids = ~1, weights = ~jobs$w.m, data = jobs)
mod.y3 <- svyglm(depress2 ~ treat + job_seek.c, design = design.ps)
# Bootstrapping to estimate the covariance matrix
mod1.Boot.ab <- function(dat, n) { # n -- the number of bootstrap replications
  a <- c()
  b <- c()
  ab <- c()
  samsize <- dim(dat)[1]
  for (i in 1:n) {
    resam.num <- sample(samsize, replace = T)
    dat.new <- dat[resam.num, ]
    num.mod <- lm(job_seek.c ~ treat, data = dat.new)
    den.mod <- lm(job_seek.c ~ treat + depress1 + econ_hard + sex + age + occp +
      marital
      + nonwhite + educ + income, data = dat.new)
    sigma.n <- summary(num.mod)$sigma
    sigma.d <- summary(den.mod)$sigma
    num.p <- dnorm(job_seek.c, mean = num.mod$fitted, sd = sigma.n)
    den.p <- dnorm(job_seek.c, mean = den.mod$fitted, sd = sigma.d)
    jobs$w.m <- num.p / den.p
    design.ps <- svydesign(ids = ~1, weights = ~jobs$w.m, data = dat.new)
    mod.m3 <- lm(job_seek.c ~ treat, data = dat.new)
    mod.y3 <- svyglm(depress2 ~ treat + job_seek.c, design = design.ps)
    a[i] <- summary(mod.m3)$coef[2, 1]
    b[i] <- summary(mod.y3)$coef[3, 1]
    ab[i] <- a[i] * b[i]
  }
  var.a <- var(a)
  var.b <- var(b)
  cov.ab <- cov(a, b)
  cov.mtx <- matrix(c(var.a, cov.ab, cov.ab, var.b), 2, 2)
  list(a.est = a, b.est = b, ab.est = ab, cov = cov.mtx)
}
boot.mod1 <- mod1.Boot.ab(jobs, 1000)
# Wald Test to test the significance of mediation effects
ahat <- summary(mod.m1)$coef[2, 1]
bhat <- summary(mod.y1)$coef[3, 1]
var.new <- c(bhat, ahat) %*% boot.mod1$cov %*% c(bhat, ahat)
W <- bhat * ahat / sqrt(var.new)
pval <- 2 * (1 - pnorm(abs(W)))
###########################################################################
# ZT Models
# no adjustment
mod.m1 <- lm(job_seek.c ~ treat + sex + sex * treat)
mod.y1 <- lm(depress2 ~ treat + job_seek.c + sex + sex * treat)
# reg. adjustment
mod.y2 <- lm(depress2 ~ treat + job_seek.c + depress1 + econ_hard + sex +
  age + occp
  + marital + nonwhite + educ + income + sex * treat)
# propensity models for continuous mediator
num.mod <- lm(job_seek.c ~ sex + treat, data = jobs)
den.mod <- lm(job_seek.c ~ treat + depress1 + econ_hard + sex + age + occp +
  marital
  + nonwhite + educ + income, data = jobs)
sigma.n <- summary(num.mod)$sigma
sigma.d <- summary(den.mod)$sigma
num.p <- dnorm(job_seek.c, mean = num.mod$fitted, sd = sigma.n)
den.p <- dnorm(job_seek.c, mean = den.mod$fitted, sd = sigma.d)
jobs$w.m <- num.p / den.p
# MSM - with robust SE
design.ps <- svydesign(ids = ~1, weights = ~jobs$w.m, data = jobs)
mod.y3 <- svyglm(depress2 ~ treat + job_seek.c + sex + sex * treat,
  design = design.ps
)
# Bootstrapping to estimate the covariance matrix
ztmod1.Boot.ab <- function(dat, n) { # n -- the number of bootstrap
  replications
  a1 <- c()
  b <- c()
  a2 <- c()
  ab <- c()
  samsize <- dim(dat)[1]
  for (i in 1:n) {
    resam.num <- sample(samsize, replace = T)
    dat.new <- dat[resam.num, ]
    num.mod <- lm(job_seek.c ~ treat + sex, data = dat.new)
    den.mod <- lm(job_seek.c ~ treat + depress1 + econ_hard + sex + age + occp +
      marital
      + nonwhite + educ + income, data = dat.new)
    sigma.n <- summary(num.mod)$sigma
    sigma.d <- summary(den.mod)$sigma
    num.p <- dnorm(job_seek.c, mean = num.mod$fitted, sd = sigma.n)
    den.p <- dnorm(job_seek.c, mean = den.mod$fitted, sd = sigma.d)
    dat.new$w.m <- num.p / den.p
    design.ps <- svydesign(ids = ~1, weights = ~w.m, data = dat.new)
    mod.m3 <- lm(job_seek.c ~ treat + sex + sex * treat, data = dat.new)
    mod.y3 <- svyglm(depress2 ~ treat + job_seek.c + sex + sex * treat,
      design = design.ps
    )
    a1[i] <- summary(mod.m3)$coef[2, 1]
    b[i] <- summary(mod.y3)$coef[3, 1]
    a2[i] <- summary(mod.m3)$coef[4, 1] ### for ZT model : beta_6 ###
    ab[i] <- (a1[i] + a2[i]) * b[i]
  }
  a <- a1 + a2
  var.a <- var(a)

  var.b <- var(b)
  cov.ab <- cov(a, b)
  cov.mtx <- matrix(c(var.a, cov.ab, cov.ab, var.b), 2, 2)
  list(a1.est = a1, b.est = b, a2.est = a2, ab.est = ab, cov = cov.mtx)
}
boot.ztmod1 <- ztmod1.Boot.ab(jobs, 1000)
# Wald Test to test the significance of mediation effects
ahat <- summary(mod.m3)$coef[2, 1] + summary(mod.m3)$coef[4, 1]
bhat <- summary(mod.y3)$coef[3, 1]
var.new <- c(bhat, ahat) %*% boot.ztmod1$cov %*% c(bhat, ahat)
W <- bhat * ahat / sqrt(var.new)
pval <- 2 * (1 - pnorm(abs(W)))
###########################################################################
# ZM Models
# no adjustment
mod.m1 <- lm(job_seek.c ~ treat)
mod.y1 <- lm(depress2 ~ treat + job_seek.c + sex + sex * job_seek.c)
# reg. adjustment
mod.y2 <- lm(depress2 ~ treat + job_seek.c + depress1 + econ_hard + sex +
  age + occp
  + marital + nonwhite + educ + income + sex * job_seek.c)
# propensity models for continuous mediator
num.mod <- lm(job_seek.c ~ treat + sex, data = jobs)
den.mod <- lm(job_seek.c ~ treat + depress1 + econ_hard + sex + age + occp +
  marital
  + nonwhite + educ + income, data = jobs)
sigma.n <- summary(num.mod)$sigma
sigma.d <- summary(den.mod)$sigma
num.p <- dnorm(job_seek.c, mean = num.mod$fitted, sd = sigma.n)
den.p <- dnorm(job_seek.c, mean = den.mod$fitted, sd = sigma.d)
jobs$w.m <- num.p / den.p
# MSM - with robust SE
design.ps <- svydesign(ids = ~1, weights = ~jobs$w.m, data = jobs)
mod.y3 <- svyglm(depress2 ~ treat + job_seek.c + sex + sex * job_seek.c,
  design = design.ps
)
# Bootstrapping to estimate the covariance matrix
zmmod1.Boot.ab <- function(dat, n) { # n -- the number of bootstrap
  replications
  a <- c()
  b1 <- c()
  b2 <- c()
  ab <- c()
  samsize <- dim(dat)[1]
  for (i in 1:n) {
    resam.num <- sample(samsize, replace = T)
    dat.new <- dat[resam.num, ]
    num.mod <- lm(job_seek.c ~ treat + sex, data = dat.new)
    den.mod <- lm(job_seek.c ~ treat + depress1 + econ_hard + sex + age + occp +
      marital
      + nonwhite + educ + income, data = dat.new)
    sigma.n <- summary(num.mod)$sigma
    sigma.d <- summary(den.mod)$sigma
    num.p <- dnorm(job_seek.c, mean = num.mod$fitted, sd = sigma.n)
    den.p <- dnorm(job_seek.c, mean = den.mod$fitted, sd = sigma.d)
    dat.new$w.m <- num.p / den.p
    design.ps <- svydesign(ids = ~1, weights = ~w.m, data = dat.new)
    mod.m3 <- lm(job_seek.c ~ treat, data = dat.new)
    mod.y3 <- svyglm(depress2 ~ treat + job_seek.c + sex + sex * job_seek.c,
      design = design.ps
    )
    a[i] <- summary(mod.m3)$coef[2, 1]
    b1[i] <- summary(mod.y3)$coef[3, 1]
    b2[i] <- summary(mod.y3)$coef[5, 1]
    ab[i] <- a[i] * (b1[i] + b2[i])
  }
  b <- b1 + b2
  var.a <- var(a)
  var.b <- var(b)
  cov.ab <- cov(a, b)
  cov.mtx <- matrix(c(var.a, cov.ab, cov.ab, var.b), 2, 2)
  list(a.est = a, b1.est = b1, b2.est = b2, ab.est = ab, cov = cov.mtx)
}
boot.zmmod1 <- zmmod1.Boot.ab(jobs, 1000)
# Wald Test to test the significance of mediation effects
ahat <- summary(mod.m3)$coef[2, 1]
bhat <- summary(mod.y3)$coef[3, 1] + summary(mod.y3)$coef[5, 1]
var.new <- c(bhat, ahat) %*% boot.zmmod1$cov %*% c(bhat, ahat)
W <- bhat * ahat / sqrt(var.new)
pval <- 2 * (1 - pnorm(abs(W)))
###########################################################################
# TM Models
# no adjustment
mod.m1 <- lm(job_seek.c ~ treat)
mod.y1 <- lm(depress2 ~ treat + job_seek.c + treat * job_seek.c)
# reg. adjustment
mod.y2 <- lm(depress2 ~ treat + job_seek.c + depress1 + treat * job_seek.c +
  econ_hard + sex + age
  + occp + marital + nonwhite + educ + income)
# propensity models for continuous mediator
num.mod <- lm(job_seek.c ~ treat, data = jobs)
den.mod <- lm(job_seek.c ~ treat + depress1 + econ_hard + sex + age + occp +
  marital + nonwhite
  + educ + income, data = jobs)
sigma.n <- summary(num.mod)$sigma
sigma.d <- summary(den.mod)$sigma
num.p <- dnorm(job_seek.c, mean = num.mod$fitted, sd = sigma.n)
den.p <- dnorm(job_seek.c, mean = den.mod$fitted, sd = sigma.d)
jobs$w.m <- num.p / den.p
# MSM - with robust SE
design.ps <- svydesign(ids = ~1, weights = ~jobs$w.m, data = jobs)
mod.y3 <- svyglm(depress2 ~ treat + job_seek.c + treat * job_seek.c,
  design = design.ps
)
# Bootstrapping to estimate the covariance matrix
tmmod1.Boot.ab <- function(dat, n) { # n -- the number of bootstrap
  replications
  a <- c()
  b1 <- c()
  b2 <- c()
  ab <- c()
  samsize <- dim(dat)[1]
  for (i in 1:n) {
    resam.num <- sample(samsize, replace = T)
    dat.new <- dat[resam.num, ]
    num.mod <- lm(job_seek.c ~ treat, data = dat.new)
    den.mod <- lm(job_seek.c ~ treat + depress1 + econ_hard + sex + age + occp +
      marital + nonwhite
      + educ + income, data = dat.new)
    sigma.n <- summary(num.mod)$sigma
    sigma.d <- summary(den.mod)$sigma
    num.p <- dnorm(job_seek.c, mean = num.mod$fitted, sd = sigma.n)
    den.p <- dnorm(job_seek.c, mean = den.mod$fitted, sd = sigma.d)
    dat$w.m <- num.p / den.p
    design.ps <- svydesign(ids = ~1, weights = ~w.m, data = dat.new)
    mod.m3 <- lm(job_seek.c ~ treat, dat = dat.new)
    mod.y3 <- svyglm(depress2 ~ treat + job_seek.c + treat * job_seek.c,
      design = design.ps
    )
    a[i] <- summary(mod.m3)$coef[2, 1]
    b1[i] <- summary(mod.y3)$coef[3, 1]
    b2[i] <- summary(mod.y3)$coef[4, 1]
    ab[i] <- a[i] * (b1[i] + b2[i])
  }
  b <- b1 + b2
  var.a <- var(a)
  var.b <- var(b)
  cov.ab <- cov(a, b)
  cov.mtx <- matrix(c(var.a, cov.ab, cov.ab, var.b), 2, 2)
  list(a.est = a, b1.est = b1, b2.est = b2, ab.est = ab, cov = cov.mtx)
}
boot.tmmod1 <- tmmod1.Boot.ab(jobs, 1000)
# Wald Test to test the significance of mediation effects
ahat <- summary(mod.m3)$coef[2, 1]
bhat <- summary(mod.y3)$coef[3, 1] + summary(mod.y3)$coef[4, 1]
var.new <- c(bhat, ahat) %*% boot.tmmod1$cov %*% c(bhat, ahat)
W <- bhat * ahat / sqrt(var.new)
pval <- 2 * (1 - pnorm(abs(W)))