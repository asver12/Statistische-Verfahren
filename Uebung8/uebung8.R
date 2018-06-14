cities.data = read.csv("cities.csv")

# Schaetzung des erwarteten Prognosefehlers
# SPSE = EW( Sum(y_(i+n)-y_(i^M))^2)
# Zerlegung in Trainingsdaten + Testdaten

set,seed(1)
index = sample(1:41,10)
cities.train = cities.data[-index,]
cities.test = cities.data[index,]


### 2. Schaetzung der Paramter basierend auf Trainingsdaten

cities.lm1 = lm(y~1+x3, data=cities.train)
cities.lm2 = lm(y~1+x3+x2, data=cities.train)
cities.lm3 = lm(y~1+x3+x2+x6, data=cities.train)
cities.lm4 = lm(y~1+x3+x2+x6+x5, data=cities.train)
cities.lm5 = lm(y~1+x3+x2+x6+x5+x2:x5, data=cities.train)

# Ab ins Rennen! Wie gut sind die Prognosen?
predict(cities.lm1, newdata = cities.test)

# Fehler berechnen
cbind(cities.test$y, predict(cities.lm1, newdata = cities.test))

spse1.hat = sum(cities.test$y-predict(cities.lm1, newdata = cities.test))^2
spse2.hat = sum(cities.test$y-predict(cities.lm2, newdata = cities.test))^2
spse3.hat = sum(cities.test$y-predict(cities.lm3, newdata = cities.test))^2
spse4.hat = sum(cities.test$y-predict(cities.lm4, newdata = cities.test))^2
spse5.hat = sum(cities.test$y-predict(cities.lm5, newdata = cities.test))^2

c(spse1.hat, spse2.hat, spse3.hat, spse4.hat, spse5.hat)

# Sehr schwankend in der Erkennung da zu wenig Daten!
# Idee Kreuzvalidierung
# u -> Vereinigung
# {1,... ,41} = I_1 u I_2 u ... u I_k
# Haeufig ist k = 10

### 3. 6-fache Kreuzvalidierung
set.seed(1)

index = rep(1:6, length.out=41)
#gefaerlich wenn datensatz sortiert ist, dann nicht zufaellig -> durchruehren
index = sample(index)
spse1.hat = spse2.hat = spse3.hat = spse4.hat = spse5.hat = 0

for (i in 1:6){
  # Zerlegung in Trainings- und Test-Daten 
  cities.test = cities.data[index==i,]
  cities.train = cities.data[-index==i,]
  
  # Schaetzung der Parameter im Trainingsdatensatz
  cities.lm1 = lm(y~1+x3, data=cities.train)
  cities.lm2 = lm(y~1+x3+x2, data=cities.train)
  cities.lm3 = lm(y~1+x3+x2+x6, data=cities.train)
  cities.lm4 = lm(y~1+x3+x2+x6+x5, data=cities.train)
  cities.lm5 = lm(y~1+x3+x2+x6+x5+x2:x5, data=cities.train)
  
  # Schaetzung des erwarteten Prognosefehlers
  spse1.hat = spse1.hat + sum(cities.test$y-predict(cities.lm1, newdata = cities.test))^2
  spse2.hat = spse2.hat + sum(cities.test$y-predict(cities.lm2, newdata = cities.test))^2
  spse3.hat = spse3.hat + sum(cities.test$y-predict(cities.lm3, newdata = cities.test))^2
  spse4.hat = spse4.hat + sum(cities.test$y-predict(cities.lm4, newdata = cities.test))^2
  spse5.hat = spse5.hat + sum(cities.test$y-predict(cities.lm5, newdata = cities.test))^2
}

# Auch nur Statistisches Verfahren -> wie gut ist berechneter Prognosefehler im Vergleich zum Schaetzwert von Kreuzvalidierung
c(spse1.hat, spse2.hat, spse3.hat, spse4.hat, spse5.hat)

# Schaetzung von SPSE basierend auf RSS (Residuen Quadratsumme)
# EW[RSS^m] = EW[Sum(y_i-y^_i^m)^2]
# SPSE^^M = RSS^M+ 2sigma~_max^2(|M|+1)
# sigma~_max^2 = 1/(n-(|M|+1))sum(y_i-Y^_i^M)^2

### 4. Schaetzung von SPSE basierend auf RSS

cities.lm1 = lm(y~1+x3, data=cities.data)
cities.lm2 = lm(y~1+x3+x2, data=cities.data)
cities.lm3 = lm(y~1+x3+x2+x6, data=cities.data)
cities.lm4 = lm(y~1+x3+x2+x6+x5, data=cities.data)
cities.lm5 = lm(y~1+x3+x2+x6+x5+x2:x5, data=cities.data)

RSS1 = sum(residuals(cities.lm1)^2)
RSS2 = sum(residuals(cities.lm2)^2)
RSS3 = sum(residuals(cities.lm3)^2)
RSS4 = sum(residuals(cities.lm4)^2)
RSS5 = sum(residuals(cities.lm5)^2)

# sigma~_max^2 = 1/(n-(|M|+1))sum(y_i-Y^_i^M)^2
sigma2.max.tilde = RSS5/(41-(5+1))

spse1.hat = RSS1 + 2 * sigma2.max.tilde * (1+1)
spse2.hat = RSS2 + 2 * sigma2.max.tilde * (2+1)
spse3.hat = RSS3 + 2 * sigma2.max.tilde * (3+1)
spse4.hat = RSS4 + 2 * sigma2.max.tilde * (4+1)
spse5.hat = RSS5 + 2 * sigma2.max.tilde * (5+1)

c(RSS1, RSS2, RSS3, RSS4, RSS5)
c(spse1.hat, spse2.hat, spse3.hat, spse4.hat, spse5.hat)

### 5. Mallow's C_p
# aequivalent zu Methode 4
# Packet leaps benoetigt
# macht best subset regression
# alle Teilmodelle eines maximalen Modells werden beruecksichtigt

require(leaps)
cities.bss = regsubsets(y~1+x1+x2+x3+x4+x5+x6, data = cities.data)
summary(cities.bss)
#          x1  x2  x3  x4  x5  x6 
#1  ( 1 ) " " "*" " " " " " " " " -> Modell x2 ist am besten fuer eine Einflussgroesse
#2  ( 1 ) " " "*" "*" " " " " " "
#3  ( 1 ) " " "*" "*" " " " " "*"
#4  ( 1 ) "*" "*" "*" " " "*" " "
#5  ( 1 ) "*" "*" "*" "*" "*" " "
#6  ( 1 ) "*" "*" "*" "*" "*" "*" 
#'-> Anzahl der Einflussgroessen
# ABER! Unklar welches Modell gewaehlt werden sollte
summary(cities.bss)$cp
# Fuer Modelle wird der Entsprechende Wert fuer das Mellow's C_p Kriterium angegeben
# [1] 23.108932  7.558597  6.361005  6.072891  5.103214  7.000000 -> wie viel Paramter kleiner = besser
cbind(summary(cities.bss)$cp, summary(cities.bss)$which)
# Es fehlen Wechselwirkungen und Nichtlinearitaeten -> transfomation einer Eigenschaft

# nvmax = maximale Anzahl von Variablen
cities.bss2 = regsubsets(y~(1+x1+x2+x3+x4+x5+x6)^2, data = cities.data, nvmax = 21)
cbind(summary(cities.bss2)$cp, summary(cities.bss2)$which)
# Was ist zweitbestes Modell ? -> kann man nicht sehen 
# (7.574500) Muss nicht zweitbestes Modell sein -> ist nur bestes Modell mit 11 Parametern, 
# aber nicht unbedingt zweitbestes insgesamt
# nbest = 3

cities.bss2 = regsubsets(y~(1+x1+x2+x3+x4+x5+x6)^2, data = cities.data, nvmax = 21, nbest = 3)
cbind(summary(cities.bss2)$cp, summary(cities.bss2)$which)
