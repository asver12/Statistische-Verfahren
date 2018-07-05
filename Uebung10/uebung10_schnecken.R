oedi.data = read.csv("offenboden.csv")

### Blauflueglige Oedlandschnecke

head(oedi.data)
# mue_i = EW( vorkommen) = h(beta_0 + beta_1 offen_i)
# vorkommen ~ Bin(1, mue_i)
# Kanonische Linkfktion
# log(mue_i/(1-mue_i)) = beta_0 + beta_1 offen_i
# mue_i = 1 / (1 + exp(-h(beta_0 + beta_1 offen_i)))
# logit
oedi.glm1 = glm(vorkommen~1+offen, data = oedi.data, family=binomial(link="logit"))

plot(oedi.data$offen, predict(oedi.glm1, type="response"), pch=16, col=2, ylim=c(0,1))
points(oedi.data$offen, oedi.data$vorkommen)

offen.int = cut(oedi.data$offen, breaks= seq(0,40,7))
rel.hfk = tapply(oedi.data$vorkommen, offen.int, mean)
points(seq(3.5, 31.5, 7), rel.hfk, pch=16, col = 3)

oedi.glm2 = glm(vorkommen~1+offen+I(offen^2), data = oedi.data, family=binomial(link="logit"))
points(oedi.data$offen, predict(oedi.glm2, type="response"), pch=16, col=4)
## Modellhierarchie:
#   Modell 1: vorkommen~1 + offen
#   Modell 1: vorkommen~1 + offen + I(offen^2)
## Hypothesenpaar
#   H_0: beta_2 = 0 gegen H_1: beta_2 != 0
# Teststatistik: T = beta^_2 / sqrt(I(beta^_)_(2,2)) -> testwert durch varianz

summary(oedi.glm2)

#Call:
  #glm(formula = vorkommen ~ 1 + offen + I(offen^2), family = binomial(link = "logit"), 
      #data = oedi.data)

#Deviance Residuals: 
  #Min       1Q   Median       3Q      Max  
#-1.8256  -0.7681  -0.3471   0.7923   2.0122  

# Verteilung jetzt Standardnormalverteilung
#Coefficients:
  #Estimate Std. Error z value Pr(>|z|)
#(Intercept) -3.758755   2.639577  -1.424    0.154
#offen        0.255298   0.308169   0.828    0.407
#I(offen^2)  -0.002601   0.008220  -0.316    0.752 -> wenn 0 Hypothese richtig, dann gibt es eine 75 % wahrscheinlihckeit, dass der wert extremer ist
# 0.752 -> wenn 0 Hypothese richtig, dann gibt es eine 75 % wahrscheinlihckeit, dass der wert extremer ist
# -> es gibt keinen grund die Nullhypothese abzulehnen

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 34.296  on 24  degrees of freedom
# Residual deviance: 25.323  on 22  degrees of freedom
# AIC: 31.323

# Number of Fisher Scoring iterations: 4

