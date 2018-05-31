soil.data = read.csv("soilrespiration1.csv", sep=";")

plot(log(resp)~temp, data=soil.data)

y = log(soil.data$resp)
x = cbind(1, soil.data$temp)
beta.hat = solve(t(x) %*% x) %*% t(x) %*% y
abline(beta.hat)
#https://de.wikipedia.org/wiki/Konfidenzintervall
sigma2.tilde = 1/(76-2)*sum(( y- x %*% beta.hat)^2)
beta.hat[1] - qt(0.975, df = 76-2) * sqrt(sigma2.tilde * solve(t(x) %*% x)[1,1])
beta.hat[1] + qt(0.975, df = 76-2) * sqrt(sigma2.tilde * solve(t(x) %*% x)[1,1])
beta.hat[1]
soil.lm = lm(log(resp)~1+temp, data=soil.data)

confint(soil.lm)
confint(soil.lm, level = 0.99)

# Konfidenzintervall

## Simulation

# EW Y_i = beta_o + beta_1*x_j = mue_i  y_i = N(mue_i, simga^2)
# beta_0 = 4.3
# beta_1 = 0.07
# sigma = 0.07
beta0 = 4.3
beta1 = 0.07
sigma2 = 0.07
x = soil.data$temp

set.seed(1)
simanz = 100
beta0.confint = matrix(0, simanz, 2)
# sim. pseudobeobachtungen
for ( i in 1:simanz){
  y.sim = rnorm(76,mean = beta0+beta1*x, sd=sqrt(sigma2))
  points(x,y.sim, col=2)
  ### konfidenzintervall fuer beta0 
  sim.lm = lm(y.sim~1+x)
  beta0.confint[i,] = confint(sim.lm)[1,]
}

beta0.confint

plot(1:simanz, 1:simanz, ylim=c(2,5), type="n")
for(i in 1:simanz){
  points(c(i,i),beta0.confint[i,],type="l")
}
abline(h=beta0,col="cyan")

# (1-alpha) - Konfidenzintervall

predict(soil.lm)
predict(soil.lm, interval = "confidence")
y.confint = predict(soil.lm, interval = "confidence")
plot(log(resp)~temp, data=soil.data)
abline(soil.lm)
points(soil.data$temp, y.confint[,2],col=2, pch="+")
points(soil.data$temp, y.confint[,3],col=2, pch="-")

# Prognoseintervall fuer eine neue Beobachtung fuer eine enue Beobachtung
# EW Y_(neu)^^ = beta_0^^+beta_1^^*x_(neu)    y_(neu) ~ N(beta_0^^+beta_1^^*x_(neu), sigma2)
# In welchem Bereich ist die Beobachtung Y_(neu) zu erwarten?
# Y_(neu) - E Y_(neu)^^
# Welche Verteilung besitzt diese Zufallsgroesse?
# (Y_(neu) - E Y_(neu)^^) ~ N(?,?)
# Var(Y_(neu) - E Y_(neu)^^) = Var(Y_(neu))+Var(EW Y_(neu)^^)+ 2*Cov(Y_(neu),EW Y_(neu)^^)
#                                                                     '-> Unabhaengig <-' = 0
# = sigma^2 + Var(x_(neu)^T*beta^^)
# = sigma^2 + simga^2(x_(neu)^T*(X^T*X)^-1*x_(neu))
# ...
# -> Konfidenzstreifen

y.prognoseint = predict(soil.lm, interval = "prediction")
points(soil.data$temp, y.prognoseint[,2],col=3, pch="+")
points(soil.data$temp, y.prognoseint[,3],col=3, pch="-")

#Varianzhomogenes Modell
# -> gibt auch Varianzheterogene Modelle
