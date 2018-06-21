# Exponential-Dispersions-Familien
# Dichtefunktion/ Wahrschienlichkeitsfunktion
# Der Form -> 
# f(y) = c(y, phi)*exp((y*Teta-A(Teta))/phi)
# Bsp.  Binomialverteilung n fest, p
#       Normalverteilung mue, sigma^2
#       Exponentialverteilung gamma
#       Poinsonverteilung
#       X^2-Verteilung
# Schlecht -> Gleichverteilung
# X^2-Verteilung als Spezialfall einer Gammerverteilung
# f(y) = 1/(S^a*T(a))*y^(a-1)*exp(-y/S)     | y > 0
# exp(-y/s+(a+1)*log(y)-a*log(s)-log(T(a)))
# Gesucht Teta und Phi
# exp((y*Teta)/phi - A(Teta)/phi) * c(y, phi)

# phi = 1/a
# Teta = 1/(a*S)

# exp((y*(-1/(a*S)))/(1/a) + a*log(S))*y^(a-1)/T(a)
# ...
# (a^a*y^(a-1))/T(a)*exp((y*(-1/(a*S))^(1/a)-log(a*S))/(1/a))

# A(Teta) = log(a*S) = log(-(1/Teta)) = -log(-Teta)
# c(y, phi) = a^a/T(a)*y^(a-1)

# Momente der Gammerverteilung 
# mue = EW(y) = A'(Teta) = -(1/-Teta)*(-1) = -1/Teta = a * S
# Var(Y) = phi*A''(Teta) = 1/a+ 1/Teta^2 = 1/a * a^2*s^2 = a* s^2
# Varianzfunktion v(mue) = mue^2
# Var(Y) = phi*mue^2

### Gammerverteilung
# Vorher beschriebenes in R
?rgamma

x = seq(0,10,0.01)
plot(x, dgamma(x, shape=1, scale=2), type="l")
points(x, dgamma(x, shape=3, scale=2), type="l", col = 2)
points(x, dgamma(x, shape=5, scale=2), type="l", col = 3)
# Jetzt kommt der Polinomiale Anteil zum Tragen 
# -> verschiebt die Kurve aendert das Maximum -> deswegen shape
# Verteilung ist Schief ergibt sich natuerlich aus der unteren Grenze null

# geometrische Verteilung
#   unabhaengige identische Versuche mit Erfolgswahrscheinlichkeit p
#   Wartezeit bis zum 1. Erfolg
# Wertebereich: {1,2,3, ...}
# P(Y=k) = (1-p)^(k-1)*p
# P(Y=1) = p
# P(Y=2) = (1-p)*p
# Spezialfall einer negativen Binomialverteilung
#   - Variante 1: Anzahl der Versuche bis zum r-ten Erfolg
#   - Variante 2: Anzahl der Misserfolgebis zum r-ten Erfolg
# P(Y=k) = (k+r-1) ueber k (1-p)^k*p = (k+r-1)!/(k!(r-1)!)*(1-p)^k*p
# Verallgemeinerung: r nicht notwendig element Natuerlicher Zahlen sondern r elem Reelen Zahlen
# -> P(Y=k) = (T(k+r)/(T(r)*k!))*(1-p)^k*p T ^= Gamma >.>
# r fest, bekannt
# = T(y+r)/(T(r)*y!)*(1-p)^y*p = T(y+r)/(T(r)*y!) exp(y*log(1-p) + r*log(p))
# Teta = log(1-p)
# exp(Teta) = 1-p
# p = 1 - exp(Teta)
# A(Teta) = -r*log(p) = -r*log(1-exp(Teta))
# Erwartungwert: 
#   mue = EW(Y) = A'(Teta) = -r * (1/(1-exp(Teta)) * (-exp(Teta)) = r * (exp(Teta)/(1 - exp(Teta))))
# umkehrung = log(mue/(mue+r)) = Teta
# mue = r*(1-p)/p

#Zweite Ableitung
# Var = A''(Teta) = r * (exp(Teta)*(1-exp(Teta))-exp(Teta)*(-exp(Teta)))/(1-exp(Teta))^2 = r*exp(Teta)/(1-exp(Teta))^2
# Var(Y) = mue + mue^2/r > mue diskrete Verteilung mit Varianz groesser als bei Poissonverteilung
# neg Binomialverteilung fuer Situationen mit groesserer Variablitaet

# Beispiel Bodenatmung
# y_i = log( Bodenatmung)
# x_i = Temperatur
# bisher:
# EW(y_i) = beta_0 +betat_1*x_i =mue_i      < Y_i = N(mue, sigma^2)
# jetzt:
# Y_i ~ Gammerverteilung mit geeingetem Erwartungswert und geeigneter Varianz
# T(S_i, a)

soil.data <- read.csv("soilrespiration1.csv", sep=";")

x <- soil.data$temp
y <-log(soil.data$resp)

b0 = 4.3
b1 = 0.075
sigma = sqrt(0.07)
means = b0+b1*x
y.sim = rnorm(76, mean=means, sd=sigma)

y.simgamma = rgamma(76, shape= 1, scale= means)

plot(x,y, ylim=c(0,20))
points(x,y.simgamma, pch = 16, col = 2)
# wir gucken nur auf den Erwartungswert und ignorieren a
# EW(y_i) = beta_0 +betat_1*x_i = a * s_i
# Var(Y_i) = a*s_i^2
# Varianz der Gammerverteilung stimmt fuer mittleres x_i mit der vorherigen Varianz der Normalverteilung ueberein
# x_i = 17.5
b0+b1*17.5
# mue_i = beta_0 +betat_1*x_i = 5.6125 = a * s_i
# Var(Y_i) = 0.07 = a * S_i^2
# s_i = (a * s_i^2) / (a * s_i) = 0.07/5.6125
# a = 5.6125 / s_i
s_i = 0.07 / 5.6125
a = 5.6125 / s_i

# scale = mue_i/a
y.simgamma = rgamma(76, shape= a, scale= means / a)
points(x,y.simgamma, pch = 16, col = 5)
