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
