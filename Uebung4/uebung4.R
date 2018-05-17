library(readr)
mietspiegel = read.csv("mietspiegel99.csv", sep=",")
mietspiegel[mietspiegel$flaeche==100, "miete"]

#Statitisches Modell 
# EW[miete]; = beta_o + beta_1*flaeche;
# EW[y]; = beta_o + beta_1*x;

#Stochastischer Teil
# miete ~N(mue_i,sigma^2) unabhaengig
# beta_dach = (beta_dach_0,beta_dach_1)^T beta_dacht = (X^T X)^-1*X^T*y
# Designmatrix X = (1 flaeche_1, 1 flaeche_2, ..., 1 flaeche_3032)
X = cbind(1, mietspiegel$flaeche)
X[1:5,]

y = mietspiegel$miete

beta.hat = solve(t(X) %*% X) %*% t(X) %*% y

plot(mietspiegel$flaeche, mietspiegel$miete)
abline(beta.hat, lwd=3, col=2)

#R Funktion fuer das selbe
# lm(formula, data)
# formular = y ~ 1 + x
# y = Zielgroesse
# 1 = konstanter Term
# data = datensatz

miete.lm1 = lm(miete~1+flaeche, mietspiegel)

# Solve ist schneller
system.time(for (i in 1:1000) solve(t(X) %*% X) %*% t(X) %*% y)
system.time(for (i in 1:1000) lm(miete~1+flaeche, mietspiegel))

# Residuen

e = y - x %*% beta.hat
e1 = residuals(miete.lm1)

cbind(e,e1)

# Darstellung der Residuen
# y-Achse Residuen
# x-Achse:  1, beobachtete Miete
plot(mietspiegel$miete, e)

#           2, flaeche
plot(mietspiegel$flaeche, e)

#           3, vorhergesaget Miete
plot(predict(miete.lm1), e)


