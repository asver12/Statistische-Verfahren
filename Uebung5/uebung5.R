miete.data <- read.csv("mietspiegel99.csv")
plot(mieteqm~flaeche, data=miete.data)

X <- cbind(1,1/miete.data$flaeche)
y <- miete.data$mieteqm

beta.hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta.hat

abline(beta.hat)

x <- seq(20, 160,0.1)
points(x, beta.hat[1]+beta.hat[2]*1/x, type="l", col=2, lwd=3)

mieteqm.m1 <- lm(mieteqm~1 + I(1/flaeche), data=miete.data)
points(miete.data$flaeche, predict(mieteqm.m1), col=3, pch=16)
coef(mieteqm.m1)

mieteqm.m2 <- lm(mieteqm~1 + I(1/flaeche)+ as.factor(lage)
                 + as.factor(lage):I(1/flaeche), data=miete.data)
mieteqm.m3 <- lm(mieteqm~as.factor(lage)*I(1/flaeche), data=miete.data)

points(miete.data$flaeche, predict(mieteqm.m3), col=miete.data$lage, pch=16)


mieteqm.m4 <- lm(mieteqm~1+bjahr+I(bjahr^2), data=miete.data)
plot(mieteqm~bjahr, data=miete.data)
points(miete.data$bjahr, predict(mieteqm.m4), col=2, pch=16)

mieteqm.m5 <- lm(mieteqm~as.factor(lage)*I(1/flaeche)+bjahr+I(bjahr^2), data=miete.data)

mieteqm.m6 <- lm(mieteqm~as.factor(lage)*(1+I(1/flaeche)+bjahr+I(bjahr^2)), data=miete.data)

coef(mieteqm.m6)
mieteqm.m7 <- lm(mieteqm~as.factor(lage)*(1+I(1/flaeche)+bjahr+I(bjahr^2)+zh+kueche+bad), data=miete.data)

plot(mieteqm.m7, which=1)

100*predict(mieteqm.m1, newdata=data.frame(flaeche=100, bjahr=1950, lage=3,zh=1,bad=1,kueche=0))
100*predict(mieteqm.m2, newdata=data.frame(flaeche=100, bjahr=1950, lage=3,zh=1,bad=1,kueche=0))
100*predict(mieteqm.m3, newdata=data.frame(flaeche=100, bjahr=1950, lage=3,zh=1,bad=1,kueche=0))
100*predict(mieteqm.m4, newdata=data.frame(flaeche=100, bjahr=1950, lage=3,zh=1,bad=1,kueche=0))
100*predict(mieteqm.m5, newdata=data.frame(flaeche=100, bjahr=1950, lage=3,zh=1,bad=1,kueche=0))
100*predict(mieteqm.m6, newdata=data.frame(flaeche=100, bjahr=1950, lage=3,zh=1,bad=1,kueche=0))
100*predict(mieteqm.m7, newdata=data.frame(flaeche=100, bjahr=1950, lage=3,zh=1,bad=1,kueche=0))


bp.data <- read.csv("bloodpressure.csv", sep=";")
head(bp.data)

bp.m1 <- lm(bp~age, data=bp.data)
bp.m1
plot(bp.m1, which=1)


### gewichtete Methode der kleinsten quadrate
age.group <- cut(bp.data$age, breaks=seq(19,59,5))

variances <- tapply(bp.data$bp,age.group,var)

V <- diag(1/variances[match(age.group,names(variances))])

V[1:10,1:10]

X <- cbind(1, bp.data$age)
y <- bp.data$bp
beta.hat <- solve(t(X) %*% V %*% X) %*% t(X) %*% V %*% y

plot(bp~age, data=bp.data)
abline(bp.m1, col=2, lwd=3)
abline(beta.hat, col=3, lwd=3)+
  
# Uebung6
plot(bp.m1, which=3)
x = predict(bp.m1)
mean(residuals(bp.m1))
res.standard = residuals(bp.m1)/sd(residuals(bp.m1))
y = sqrt(abs(res.standard))
points(x,y, col=2, pch=16)
# Brauchen unterschiedliche Residuen -> man benoetigt unterschiedliche Varianzen
# Varinaz der Residuen
# e_i^^ = y_i - (beta_0^^+beta_1^^*x_i)
#  '-> Vektor der Residuen
# e_i^^ = y - Xeta^
# Cov(e^)=Cov(y-X*beta^^) = Cov(Y-X(X^T*X)^-1*X^T*y)
#                         = Cov((Id_n-X(X^T*X)^-1*X^T)y) = A*Cov(y)A^T
#                                                              '->sigma^2*Id_n
# sigma^2(Id_n-X(X^T*X)^-1*X^T)

# Sigma^2 schaetzen
# ~sigma^2 = 1/(n-(k+1))sum((y_i-x_i^T*beta^^)^2) = 1/(n-(k+1))*RSS
#                                                           '->1/(n-2)
sigma2.hat = 1/(52-2)* sum(residuals(bp.m1)^2)
cov.residuals = sigma2.hat * (diag(54)-X %*% solve(t(X) %*% X) %*% t(X))

var.residuals = diag(cov.residuals)
var.residuals

res.standard2 = residuals(bp.m1)/sqrt(var.residuals)
y2 = sqrt(abs(res.standard2))

points(x,y2, col=2, pch=16)
