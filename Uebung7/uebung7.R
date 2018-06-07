stem.data = read.csv("stembiomass.csv", sep=";")
names(stem.data)
#d -> Brusthoehendruchmesser, h -> hoehe, stem -> Biomasse der Staemme
head(stem.data)
plot(stem~d, data=stem.data)
# nicht linear sondern quadratisch und variablitaet bei kleinen baumen geringer als bei grossen
# -> so koennen wir nichts damit anfangen
# -> Allometrische Funktionen - Potenzfunktionen
# ~~ ^= unefaehr
# stem_i ~~ beta_0*d_i^(beta_1)
# log(stem_i) ~~ log(beta_0)*log(d_i)*(beta_1)
# Statistisches Modell
# EW[log(stem_i)] = beta_0^*+beta_1*log(d_i) = i*mue_i*log(stem_i)~N(mue_i,sigma^2)
plot(log(stem)~log(d), data=stem.data)

x = log(stem.data$d)
y = log(stem.data$stem)

stem.lm1 = lm(y~1+x)
abline(stem.lm1)

#Residuenplot + smothing-fit
plot(stem.lm1, which=1)
#smothing-fit nicht gerade -> wir koennten eventuell besser sein als dieses Modell
# Versuch Polynomilae Regression
# situation natuerliche Modellhierarchie
# Modell 1, Modell 2, Modell 3, ...
# Innerhalb dieser Ueberpruefen ob es sinnvoll ist noch einen zusaetzlichen Predictor mit aufzunehmen
# Natuerliche Modellhierarchie
# Modell 1: EW[y_i] = beta_0 + beta_1*x_i                     >-| H_0: beta_2 = 0
# Modell 2: EW[y_i] = beta_0 + beta_1*x_i + beta_2*x_i^2      <-|
# Frage: Ist Modell 2 besser geeignet?
# Zwei antworten:
#   1 garantiert besser, da wir mehr parameter haben
#   2 im Rahmen Parametrischer Tests ist die Antwort nicht so klar
#   '-> Frage: ist das Modell in dem Sinne besser, dass es mehr bietet als die verbesserung durch den zweiten Parameter?
# H_o:beta_2=0 gegen H_1:beta_2 /= 0

stem.lm2 = lm(y~1+x+x^2)

plot(log(stem)~log(d), data=stem.data)
abline(stem.lm1)
x_neu = seq(1.5,4,0.01)
points(x_neu, predict(stem.lm2,newdata=data.frame(x=x_neu)), type="l", col = 2, lwd = 2)

# !!! WIEDER FEHLER !!! wie beim Linearen Modell
# Richtig : 

stem.lm2 = lm(y~1+x+I(x^2))

plot(log(stem)~log(d), data=stem.data)
abline(stem.lm1)
x_neu = seq(1.5,4,0.01)
points(x_neu, predict(stem.lm2,newdata=data.frame(x=x_neu)), type="l", col = 2, lwd = 2)

# Wie kann man ueberpruefen ob stem.lm2 besser ist als stem.lm1?
# P-Test
# Teststatistisk(t.stat) : T(y_1,...,y_n) = beta^_2 / (sqrt(sigma~^2(X^T*X)^-1_(2 2)))

# Anzahl der Beobachtungen
n = 71
# 3 -> Anzahl der Koeffizienten count(beta_0,beta_1,beta_2,...)
sigma2.tilde = sum(residuals(stem.lm2)^2) / (n-3) 

X = cbind(1,x,x^2)

# letzes Element der Matrix
# Da wir mit null anfangen zu zahlen 3,3
std.error = sqrt(sigma2.tilde*solve(t(X) %*% X)[3,3])
t.stat = coef(stem.lm2)[3]/std.error
# t-Verteilung mit 71(Beobachtungen) - 3(Koeffizienten) Freiheitsgraden
# Quantil der T-Verteilung -> kritischer Wert
# Signifikatnzniveau = alpha = 0.05
# t_(n-(k+1), 1-alpha/2)=t_(68,0.975) = 1.95469
# auf Kurve kritischer Wert
# ^ y
# |               _
# |      |     ___  ___     |
# |      | ____         ____|
# | _____|                  |______
# --------------------------------------> x
#      '-> qt           <-'  in Normalverteilung
qt(0.975,df=68)
#
# H_0 ablehnen -> Entscheiden uns fuer H_1(Modell 2)
# Deswegen lehnen wir Nullhypothese ab

# Ueberschreitungswahrwschienlichkeit
2*pt(t.stat,df=68)

summary(stem.lm2)
# Warum jetzt nicht Modell 3-Grades 
# Modell 3: EW[y_i] = beta_0 + beta_1*x_i + beta_2*x_i^2 + beta_3*x_i^3                 >-|
#oder 4-Grades?                                                                           | H_0 = beta_4 = 0
# Modell 4: EW[y_i] = beta_0 + beta_1*x_i + beta_2*x_i^2 + beta_3*x_i^3 + beta_4*x_i^4  <-|

stem.lm3 = lm(y~1+x+I(x^2)+I(x^3))
summary(stem.lm3) 
# Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -3.57384    2.01183  -1.776   0.0802 .
# x            2.84215    2.31345   1.229   0.2235  
# I(x^2)       0.05710    0.86249   0.066   0.9474  
# I(x^3)      -0.02542    0.10448  -0.243   0.8085  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.1761 on 67 degrees of freedom
# Multiple R-squared:  0.9802,	Adjusted R-squared:  0.9793 
# F-statistic:  1103 on 3 and 67 DF,  p-value: < 2.2e-16
# -> p-value sehr nah an 0 ( 2.2e-16 ) 
# Ablehnen
# !!!Trotzdem immernoch Residuen angucken!!!
# Nicht ablehenen, da nicht besser als Modell 2 -> brauchen Modell 4 nicht zu betrachten