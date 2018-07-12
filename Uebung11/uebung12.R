schul.data = read.csv("schule.csv")
head(schul.data)

# Zielgroesse: daysabs
#   daysabs ~ Poisson(mue_i)
# Modell 0: daysabs ~ 1
# Modell 1: daysabs ~ 1 + male
# Modell 2: daysabs ~ 1 + male + math
# Modell 3: daysabs ~ 1 + male + math + male:math

m0 = glm(daysabs~1,data=schul.data, family = poisson)
m1 = glm(daysabs~1 + male,data=schul.data, family = poisson)
m2 = glm(daysabs~1 + male + math,data=schul.data, family = poisson)
m3 = glm(daysabs~1 + male + math + male:math,data=schul.data, family = poisson)

anova(m0,m1,m2,m3,test="LRT")

summary(m3)

### Simulation von Poissonverteilungen
y.pseudo = rpois(316, lambda = predict(m3, type = "response"))

plot(daysabs~math, data = schul.data, pch=16, col=male+1)
points(schul.data$math,y.pseudo, pch=16, col=3)

# Ansatz, dass es eine Poissonverteilung ist ist falsch -> bzw beschreibt den Datensatz nicht korrekt
# Diskrete Verteilung -> Negative Binomilverteilung
# Interssant ist die Tatsache, dass  Negative Binomilverteilung in der Lage ist Daten zu beschreiben 
# wo die Varianz groesser ist als der Erwartungswert

# Negative Binomilverteilung
require(MASS)

# daysabs~neg.Binomial
m0 = glm.nb(daysabs~1,data=schul.data)
m1 = glm.nb(daysabs~1 + male,data=schul.data)
m2 = glm.nb(daysabs~1 + male + math,data=schul.data)
m3 = glm.nb(daysabs~1 + male + math + male:math,data=schul.data)

# Ueberschreibungswahrscheinlichkeit groesser als 0.05 zwischen 3 und 4 also keinen Gewinn durch m3
anova(m0,m1,m2,m3,test="LRT")

summary(m2)

### Simulation von Negative Binomilverteilung
y.nb = rpois(316, lambda = predict(m2, type = "response"))
# Null deviance: 371.56  on 315  degrees of freedom
# Residual deviance: 357.06  on 313  degrees of freedom
# Sehr aehnlich

plot(daysabs~math, data = schul.data, pch=16, col=male+1)
points(schul.data$math,y.nb, pch=16, col=3)
