
n = 500

## function for sampling from causal model
sim = function(n) {
  A = rnorm(n)
  B = rnorm(n)
  C = rnorm(n)
  X = A + B + rnorm(n)
  D = X + rnorm(n)
  E = B + C + 0.5*X + rnorm(n)
  Y = 3*E + 3*rnorm(n)
  
  df = data.frame(A,B,C,D,E,X,Y)
  return(df)
}

## sample from causal model
set.seed(1)
data = sim(n)

## regress Y on X and ...
# ...valid adjustment set
(lm(Y ~ X + B, data)$coef)[2]               # minimal VAS
(lm(Y ~ X + B + C, data)$coef)[2]
(lm(Y ~ X + B + C + D + A, data)$coef)[2]   # maximal VAS

# ...subset of valid adjustment set
(lm(Y ~ X + A, data)$coef)[2]               # B omitted
(lm(Y ~ X + A + C + D, data)$coef)[2]       # B omitted

# ...superset of valid adjustment set
(lm(Y ~ X + B + E, data)$coef)[2]           # E extra
(lm(Y ~ ., data)$coef)[2]                   # control for everything

## compare efficiency when using different VAS
beta1 = beta2 = beta3 = c()
for (i in 1:1000) {
  set.seed(i)
  data = sim(n)
  beta1[i] = (lm(Y ~ X + B + C, data)$coef)[2]
  beta2[i] = (lm(Y ~ X + B, data)$coef)[2]
  beta3[i] = (lm(Y ~ X + B + C + D + A, data)$coef)[2]
  
  if ((i %% 100) == 0) print(i)
}
par(mfrow=c(3,1))
hist(beta1, xlim=c(1,2), breaks=seq(0,3,by=0.01), xlab="", ylab="", main="Z = {B,C}")
abline(v=c(mean(beta1)-sd(beta1), mean(beta1)+sd(beta1)), col="red", lwd=2)
hist(beta2, xlim=c(1,2), breaks=seq(0,3,by=0.01), xlab="", ylab="", main="Z = {B}")
abline(v=c(mean(beta2)-sd(beta2), mean(beta2)+sd(beta2)), col="red", lwd=2)
hist(beta3, xlim=c(1,2), breaks=seq(0,3,by=0.01), xlab="beta", ylab="", main="Z = {A,B,C,D}")
abline(v=c(mean(beta3)-sd(beta3), mean(beta3)+sd(beta3)), col="red", lwd=2)
