gala.data = read.csv("galapagos.csv", sep= ",")

## Artenzahlen auf Palapagos-Inseln
names(gala.data)
head(gala.data)
plot(gala.data$logarea, gala.data$species)

# Y_i - Artenzahl auf Insel i
# Y_i ~ Poisson(mue_i)
# Wahrscheinlichkeitsverteilung
# f(y_i) = (mue_i^(y_i))/(Y_i!)*e^(mue_i) = 1/(y_i!)*exp(y_i*log(mue_i)-mue_i) | mue_i = A(Teta_i)
# 1/(y_i!) = c(y, phi_i)
# Teta_i = log(mue_i)
# EW(y_i) = mue_i = exp(Teta_i) = exp(beta_0 + beta_1 * logarea_i)
# log(EW(y_i)) = log(mue_i) = Teta_i = beta_0 + beta_1 * logarea_i -> log ist kanonische Link-Funktion

### Loglikelihood-Schaetzung
## Fktion
# L(betaV) = prod(1/(y_i!)exp(y_i*log(mue_i)- mue_i))
# LogL(betaV) = sum(log(1/(y_i!)) + y_i*log(mue_i)- mue_i) = sum(log(1/(y_i!)) +y_i*(beta_0 +beta_1*x_i) - exp(beta_0 +beta_1*x_i))
# ScoreFktionen
# S_0(betaV)=nabla(beta_0)(LogL(betaV)) = sum(y_i - exp(beta_0 +beta_1*x_i)) =! 0
# S_1(betaV)=nabla(beta_1)(LogL(betaV)) = sum(y_i - exp(beta_0 +beta_1*x_i)*x_i) = sum((y_i - mue_i)*x_i =! 0

# Newton-Verfahren zur Bestimmung der Nullstellen
# 0 = S(betaV) rund S(beta^0) +S'(beta_0)*(beta_beta_0)
# S(beta_0) = S'(beta_0)(beta-beta^0)
# beta_0 - 1/(S'(beta_0)) * S(beta_0) = beta_1
# Newton-Verfahren fuer Gleichungssysteme
# 0 = S_0(betaV) rund S_0(beta_0) + nabla(beta_0) S_0*(beta_0)*(beta_0 - beta_0^0) + nabla(beta_1) S_0*(beta_1)*(beta_1 - beta_1^0) <- TaylorPolynome
# 0 = S_1(betaV) rund S_1(beta_0) + nabla(beta_0) S_1*(beta_0)*(beta_0 - beta_0^0) + nabla(beta_1) S_1*(beta_1)*(beta_1 - beta_1^0) <- TaylorPolynome
# ...
# umstellen
# S_o(betaV^0) - nabla(beta_0) S_0*(beta_0^0) - nabla(beta_1) S_0*(beta_1^0) = lineares Gleichungssystem in beta_0
# Zweite Ableitung ausrechnen nach beta_0 und beta_1
# betaV^1 = I^-1(betaV^0)*S(beta^0) + betaV^0
# Oteratopmsverfahren zur Loesung des Score-Gleichungssystems
# Startvektor = betaV^0
# n = 0
# REPEAT
#   betaV^(n+1) = I^-1(betaV^n)*S(beta^n) + betaV^n
# UNITL ||betaV^(n+1) - betaV^n || < epsilon bzw relative Aenderung ||betaV^(n+1) - betaV^n || / ||beta^n|| < epsilon

# Scorefktion
# S_0(betaV) = sum(y_i - exp(beta_0 +beta_1*x_i))
# S_1(betaV) = sum(y_i - exp(beta_0 +beta_1*x_i))*x_i
# Fisher-Informationsmatrix = 
#   sum(exp(beta_0 +beta_1*x_i))            |   sum(y_i - exp(beta_0 +beta_1*x_i))*x_i
#   sum(y_i - exp(beta_0 +beta_1*x_i))*x_i  |   sum(y_i - exp(beta_0 +beta_1*x_i))*x_i^2

# Hier ist ein Fehler
score = function(beta){
  c(sum(y-exp(beta[1]+beta[2]*x)), 
    sum(y-exp(beta[1]+beta[2]*x)*x))
}

fisher.mat = function(beta){
  I00 = sum(y-exp(beta[1]+beta[2]*x))
  I01 = sum(y-exp(beta[1]+beta[2]*x)*x)
  I10 = sum(y-exp(beta[1]+beta[2]*x)*x)
  I11 = sum(y-exp(beta[1]+beta[2]*x)*x^2)
  matrix(c(I00, I01, I10, I11), 2, 2)
}

y = gala.data$species
x = gala.data$logarea
eps = 0.00001

betaalt = c(0,1)
repeat{
  betaneu = solve(fisher.mat(betaalt)) %*% score(betaalt) + betaalt
  print(solve(fisher.mat(betaalt)) %*% score(betaalt) + betaalt)
  if (sum((betaneu - betaalt)^2) / sum(betaalt^2) < eps) break
  betaalt = betaneu
}

?family

# Ab hier ist alles wieder richtig
plot(gala.data$logarea, gala.data$species)
gala.glm1 = glm(species~1+logarea, data=gala.data, family = poisson(link="log"))
points(gala.data$logarea, predict(gala.glm1), pch=16, col=2)
points(gala.data$logarea, predict(gala.glm1, type="response"), pch=16, col=3)
# type="response" -> deutet an, dass wir die skala auf der groesse der Zielgrosse haben wollen

gala.glm2 = glm(species~1+logarea, data=gala.data)
points(gala.data$logarea, predict(gala.glm2, type="response"), pch=16, col=4)

gala.glm3 = glm(species~1+logarea, data=gala.data, family = poisson)
points(gala.data$logarea, predict(gala.glm2, type="response"), pch=16, col=5)



gala.glm4 = glm(species~1+logarea, data=gala.data, family = poisson(link="sqrt"))
# sobald eine nicht kanonische ... ist nicht mehr garantiert, dass das Verfahren konvergiert
start=c(0,1)
points(gala.data$logarea, predict(gala.glm4, type="response"), pch=16, col=6)
