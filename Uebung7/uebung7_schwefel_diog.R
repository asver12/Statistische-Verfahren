cities.data = read.csv("cities.csv")
head(cities.data)

# y - SO_2 -Belastung der Luft (Mittlere)
# x_1 - Mittlere Jahrestemperatur
# x_2 - Anzahl der Betriebe mit >= 20 Beschaeftigten
# x_3 - Einwohnerzahl
# x_4 - Mittlerer Jahresniederschlag
# x_5 - Mittlere Windgeschwindigkeit
# x_6 - Mittlere Anzahl an Regentagen
# Problem Hierarchie nicht offensichtlich -> Muesste man sich erst ueberlegen
# z.B.
# Modell 0: EW(y_i)=beta_0 -> keine der Potenziellen Einflussgroessen hat einen Einfluss auf die SO_2-Belastung der Luft
# Modell 1: EW(y_i)=beta_0 + beta_1*x_3 -> Einwohnerzahl hat Einfluss
# Modell 2: EW(y_i)=beta_0 + beta_1*x_3 + beta_2*x_2 -> Einwohnerzahl hat Einfluss und Anzahl der Betriebe
# Modell 3: EW(y_i)=beta_0 + beta_1*x_3 + beta_2*x_2 + beta_3*x_6

# zwischensprung:
# Je mehr einwohner desto mehr betriebe -> korrelation
# Wechselwirkung -> 3 Groessen -> zwei Einflussgroessen und eine Zielgroesse
# Mittlere Windgeschwindigkeit 
#     -> wenn gering -> Anzahl der Betriebe sehr starker Einfluss auf SO_2 Belastung
#     -> wenn gross -> weniger praegnant da direkt in Nachbarstaedte geblassen
# Fuer Wechselwirkungen wird Main-Effekt benoetigt
# Deswegen Modell 4 einschieben
# Modell 4: EW(y_i)=beta_0 + beta_1*x_3 + beta_2*x_2 + beta_3*x_6 + beta_4*x_5
# Modell 4: EW(y_i)=beta_0 + beta_1*x_3 + beta_2*x_2 + beta_3*x_6 + beta_5*x_2*x_5
# Simulation Wechselwirkung ^= beta_5*x_2*x_5

### Umsetzung
cities.lm1 = lm(y~1+x3, data=cities.data)
cities.lm2 = lm(y~1+x3+x2, data=cities.data)
cities.lm3 = lm(y~1+x3+x2+x6, data=cities.data)
cities.lm4 = lm(y~1+x3+x2+x6+x5, data=cities.data)
cities.lm5 = lm(y~1+x3+x2+x6+x5+x2:x5, data=cities.data)

cities.lm4
summary(cities.lm1)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-32.545 -14.456  -4.019  11.019  72.549 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 17.868316   4.713844   3.791 0.000509
# x3           0.020014   0.005644   3.546 0.001035 -> Numerisch nicht weit von der Null weg 
# aber Std. error zeigt sehr genau geschaetzter Parameter -> t value extrem gross -> deswegen legen wir 0-hypothese an

summary(cities.lm2)
summary(cities.lm3)

# Andere Vorhergehensweisen:
# z.B alles Ausprobieren
# Schrittweise Variablen(Vorwaerz)-Selektion forwardselection
# Modell 0 ~ 1
# Modell 1 ~1+x_1         ~1+x_2            ~1+x_3            ~1+x_4          ~1+x_5            ~1+x_6
# + Hyptothese
#       H_0: beta_1 = 0 H_0: beta_2 = 0 H_0: beta_3 = 0 H_0: beta_4 = 0 H_0: beta_5 = 0 H_0: beta_6 = 0
cities.lm1 = lm(y~1+x1, data=cities.data)
cities.lm2 = lm(y~1+x2, data=cities.data)
cities.lm3 = lm(y~1+x3, data=cities.data)
cities.lm4 = lm(y~1+x4, data=cities.data)
cities.lm5 = lm(y~1+x5, data=cities.data)
cities.lm6 = lm(y~1+x6, data=cities.data)

summary(cities.lm1)
# P-Wert 0.0046
summary(cities.lm2)
# p-Wert 0.0005
summary(cities.lm3)
# p-Wert 0.001
summary(cities.lm4)
# p-Wert 0.556
summary(cities.lm5)
# p-Wert 0.736
summary(cities.lm6)
# p-Wert 0.0174
# Vorschlag 1: Alle Einflussgroessen fuer die H_0 abgelehnt
#       Problem: Mehrere Einflussgroessen konnen stark miteinder korreliert sein -> z.B x2 und x3
#                 Ignorieren von Korrelationen
# Vorschlag 2: Nur Einflussgroesse mit kleinster Ueberschreitung waehlen (0.0005)
#       ein kleines bisschen weniger schlecht
#       Problem: Wann soll Vorwaerzselektion beendet werden?
#       Beispielhaftes weitergehen:  ~1+x1+x2  ~1+x2+x3  ~1+x2+x4  ~1+x2+x5 ~1+x2+x6
#   T_1(y_1,...,y_n) = beta^_1 / (sqrt(sigma~^2(X^T*X)^-1_(1 1)))
#   T_2(y_1,...,y_n) = beta^_2 / (sqrt(sigma~^2(X^T*X)^-1_(2 2)))
#   ...
#   Entspricht Auswahl der groessten dieser Statistiken T(y_) max_i( T_i(y_) )
#   -KEINE t-Verteilung mehr
#   -- Erhoeht wahrscheinlichkeit unrelevante Einflussgroessen zu waehlen
#   -> Auch nicht machen -> Zu grosse Modelle
#   richtig machen -> r-Packet Multcomp

# Statistische Test koennen nur richtig verwendet werden, wenn Modellhierarchie apriorie gegeben ist, 
# falls keine gegeben wird, dann sollte man sehr vorsichtig sein. Insbesondere keine Naive Variante der Vorwaerzselection
# Beide schlecht