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

plot(miete.lm1,which = 1)

# Einbeziehung der Lage
miete.lm2 = lm(miete~1+flaeche+lage, mietspiegel)
plot(miete~flaeche, mietspiegel)
points(mietspiegel$flaeche, predict(miete.lm2), col=mietspiegel$lage, pch=16)

#Bloedsinn -> EW[Miete] = beta_o + beta_1*flaeche + beta_2*lage;
# Verschiebt einfach die Punkte um die Lage nach oben 
# -> Lage ist qualitativ(faktor) aber im Datensatz numerisch

#Faktor
miete.lm3 = lm("miete~1+flaeche+as.factor(lage)", mietspiegel)
plot(miete~flaeche, mietspiegel)
points(mietspiegel$flaeche, predict(miete.lm3), col=mietspiegel$lage, pch=16)

x3 = model.matrix(miete.lm3)
x3[1:20,]

# Einbeziehung der Wechselwirkung
miete.lm4 = lm("miete~1+flaeche+as.factor(lage)+flaeche:as.factor(lage)", mietspiegel)
plot(miete~flaeche, mietspiegel)
points(mietspiegel$flaeche, predict(miete.lm4), col=mietspiegel$lage, pch=16)


x4 = model.matrix(miete.lm4)
x4[1:20,]

beta.hat4 = coef(miete.lm4)
abline(beta.hat4[1], beta.hat4[2], lwd=3)
abline(beta.hat4[1]+beta.hat4[3], beta.hat4[2]+beta.hat4[5], lwd=3, col=2)

plot(miete.lm4, which = 1)

plot(miete~bjahr, mietspiegel)

# EW[Miete] = EW[Miete] = beta_o + beta_1*bjahr;
miete.lm5 = lm("miete~1+bjahr", mietspiegel)
plot(miete.lm5, which = 1)

# EW[Miete] = EW[Miete] = beta_o + beta_1*bjahr+ beta_2*bjahr^2;
# linear in den Parametern -> lineares Modell
# aber! nichtlinearer Zusammenhang
miete.lm6 = lm("miete~1+bjahr+bjahr^2", mietspiegel)
plot(miete.lm6, which = 1)

# 1+bjahr+bjahr:bjahr -> sinnlos
coef(miete.lm5)
coef(miete.lm6)

#
miete.lm7 = lm("miete~1+bjahr+I(bjahr^2)", mietspiegel)
plot(miete.lm7, which = 1)


