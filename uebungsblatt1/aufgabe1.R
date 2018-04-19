soilrespiration = read.csv("2018_SS_7_2_3_soilrespiration1.csv", sep = ";")
soilrespiration.frame = data.frame(soilrespiration)

beta0.seq = seq(4,6,0.001)
beta1.seq = seq(0,0.1,0.001)

X = soilrespiration.frame$temp
y = log(soilrespiration.frame$resp)

result = matrix(0,length(beta0.seq),length(beta1.seq))
for(i in 1:length(beta0.seq)){
  for(j in 1:length(beta1.seq)){
    beta0 = beta0.seq[i]
    beta1 = beta1.seq[j]
    result[i,j] = sum(abs(y-(beta0+beta1*X)))
  }
}
which(result == min(result), arr.ind=T)

plot(X,y)

beta0.seq[228]
beta1.seq[78]
abline(beta0.seq[228], beta1.seq[78],col=2,lwd=0.2)

matrix(c(rep(1,76),X), nrow=76, ncol=2)
x = cbind(1,X)
beta.hat = c(beta0.seq[228],beta1.seq[78])

x%*%beta.hat

points(X, x %*% beta.hat, pch=16, col = 2)
t(x)%*%x
solve(t(x) %*% x)

result2 = matrix(0,length(beta0.seq),length(beta1.seq))
result4 = matrix(0,length(beta0.seq),length(beta1.seq))
result0 = matrix(0,length(beta0.seq),length(beta1.seq))

for(i in 1:length(beta0.seq)){
  for(j in 1:length(beta1.seq)){
    beta0 = beta0.seq[i]
    beta1 = beta1.seq[j]
    result2[i,j] = sum(abs(y-(beta0+beta1*X))^2)
    result4[i,j] = sum(abs(y-(beta0+beta1*X))^4)
    result0[i,j] = sum(abs(cos(atan(beta1))*(y-(beta0+beta1*X))))
  }
}

which(result2 == min(result2), arr.ind=T)
which(result4 == min(result4), arr.ind=T)
which(result0 == min(result0), arr.ind=T)

abline(beta0.seq[321],beta1.seq[75], col = 4, lwd=0.2)
abline(beta0.seq[393], beta1.seq[72],col=5,lwd=0.2)
abline(beta0.seq[228], beta1.seq[78],col=6,lwd=0.2)

# Illustration der statistischen Eigenschaften der Kriterien durch Simulation mehrfacher Anwendung im Rahmen eines Modells
# Modell:
# Ew[Y_i] + beta_0 + beta_1*x_i=sigma_i Y_i ist Normalverteilung mit Ew_i und Sigma^2
# Für die Simulation müssen Parameter beta_0, beta_1 und sigma^2 bekannt sein
# beta_0 = 4.227, beta_1 = 0.077 und simga^2 = ???
# Varianz = Zentralles Zweites Moment -> EW[Y-EW(Y)]^2
# Var(Y) = 1/n(sum i = 1-> n (Y_1-Ydach_i)^2)

sigma2.hat = min(result2)/76

# Idee Erzeugen von gleichverteilten Zufallsgrößen auf [0,1]
# linearer Kongruenzgenerator
# z_0 - Startwert
# z_i+1 = (a*z_i+b) mod c
# Erzeugen von Pseudo-Zufallszahlen

set.seed(1)
runif(10)
result = runif(10000)
hist(result)
hist(result,breaks = seq(0,1,0.1),freq = FALSE)

# Beweis das Gleichverteilung
# 1. Dichtefunktion
# f(x)  { 1 x in [0,1]
#       { 0 sonst

x = seq(0,1,0.01)
points(x,dunif(x),type="l", col = 2, lwd = 3)

# 2. Verteilungsfunktion
# F(x)  { 0 x<=0
#       { x 0<x<=1
#       { 1 x>0
# = P(X<=x)

plot(x, punif(x),type = "l", col = 3, lwd = 1)

# empirische  Verteilungsfunktion

set.seed(1)
y = runif(10000)
plot(ecdf(y))
points(x, punif(x),type = "l", col = 3, lwd = 1)

y = rnorm(10000)
hist(y, freq=F)

# Dichtefunktion
x = seq(-4,4,0.01)
points(x, dnorm(x),type = "l", col=3)

# empirische Verteilungsfunktion
plot(ecdf(y))
points(x, pnorm(x),type = "l", col=4)

#Standardabweichung !NICHT! die Varianz
y = rnorm(10000, mean = 5, sd = sqrt(0.066))
hist(y)

beta0 = 4.2227
beta1 = 0.07
sigma2 = 0.066

# Pseudozufallszahlen mit unterschiedlichen Erwartungswerten
# Zurueck Korrigieren
X = soilrespiration.frame$temp
muis = beta0+beta1*X

# Standardabweichung ist Wurzel Varianz
y.sim = rnorm(76, mean = muis, sd=sqrt(sigma2))
y = log(soilrespiration.frame$resp)

plot(X,y)

points(X, y.sim, pch=16, col = 3)

