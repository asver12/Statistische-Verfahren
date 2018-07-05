seeds.data = read.csv("seeds.csv")
head(seeds.data)

levels(seeds.data$treatment)
levels(seeds.data$stress)

# Zielgroesse: Anzahl der Sammen
# Einflussgroesse:  Treatment: controll, caterpillar, scissors
#                   Stress: dry, moist

# Modell:
#   seeds ~Poisson(mue_i)
#         mue_i ) EW[seeds_i] = linearer Praedikator
# Modell0: seeds ~ 1
# Modell1: seeds ~ 1+ treatment
# Modell2: seeds ~ 1+ treatment + stress
# Modell3: seeds ~ 1+ treatment + stress + treatment:stress

seeds.glm0 = glm(seeds~1, data=seeds.data, family = poisson(link="identity"))
seeds.glm1 = glm(seeds~1+treatment, data=seeds.data, family = poisson(link="identity"))
seeds.glm2 = glm(seeds~1+treatment+stress, data=seeds.data, family = poisson(link="identity"))
seeds.glm3 = glm(seeds~1+treatment+stress+ treatment:stress, data=seeds.data, family = poisson(link="identity"))

X = model.matrix(seeds.glm1)
head(X)
# h_0: beta != 0 fuer ein j elem(1,2)

logLik(seeds.glm0)
logLik(seeds.glm1)

test.stat = 2*(logLik(seeds.glm1)-logLik(seeds.glm0))

qchisq(0.95, df=2)
## Ueberschreitungswkt
1-pchisq(test.stat,df=2)

anova(seeds.glm0, seeds.glm1,seeds.glm2, seeds.glm3, test="LRT")

interaction.plot(seeds.data$treatment, seeds.data$stress, seeds.data$seeds)
