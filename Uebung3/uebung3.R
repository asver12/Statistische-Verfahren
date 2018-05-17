y = c(41, 39, 111, 30, 36, 2, 12, 34, 22, 75, 70, 33, 11, 28, 44, 45, 61, 61, 185, 51)
hist(y)
# Maximum Likelihood Schätzung für unbekannten Parameter Theta-Groß
# Likelihoodfunktion L(Theta) = L(Theta|y_1,...,y_20) = Produkt^20_(i=1) f_Theta(y_i) = Produkt^20_(i=1) = Theta*e^(-Theta*y_i)

likelihood = function(theta){
  prod(
    theta*exp(-theta*y)
  )
}

likelihood(0.1)

theta.vec = seq(0,2,0.001)

# Funktion vekorisieren
likelihood.vec = Vectorize(FUN = likelihood, vectorize.args = "theta")
likelihood.vec(theta.vec)
plot(theta.vec, likelihood.vec(theta.vec), type = "l")

# Loglikelihood-Funktion
# l(Theta) = l(Theta|y_1,...,y_20) = Sum^20_(i=1) log(Theta)-Theta*y_i = 20log(Theta)-Theta*Sum^20_(i=1) y_i
loglikelihood = function(theta){
  length(y)*log(theta) - theta*sum(y)
}
loglikelihood.vec = Vectorize(FUN = loglikelihood, vectorize.args = "theta")
plot(theta.vec, loglikelihood.vec(theta.vec), type = "l")

# Score-Funktion
# S(Theta) = S(Theta,y_1,...,y_20)=20/Theta-Sum^20_(i=1) y_i
score = function(theta){
  length(y)/theta-sum(y)
}
score.vec = Vectorize(FUN = score, vectorize.args = "theta")
plot(theta.vec, score.vec(theta.vec), type = "l", xlim = c(0.015,0.035), ylim = c(-1,100))
abline(h=0, lty=2)

# Theta-Sum^20_(i=1) y_i = 0 -> Theta^^ = 20/Sum^20_(i=1) y_i
theta.hat = length(y)/sum(y)
abline(v=theta.hat, col = 2)

# Fischer-Information
# I(Theta) = -\nabla^2 l(Theta|y_1,...,y_20) = -\nabla^2 S(Theta,y_1,...,y_20) = --20/Theta^2 = n/Theta^2
# I(Theta^2) = n/Theta^^^2 = n/(n/(Sum^n_(i=1) y_i))^2 = 1/n*(Sum^n_(i=1) y_i))^2
# Theta^^ -> Theta dach
fisher.obs = sum(y)^2/length(y)

# Beobachtet Fischer-Info beschreibt Form der Loglikelihood-Funktion in der Umgebung der Maximalstelle
# Veranschaulichen durch Tylor-Approximation für l(Theta) in Umgebugn von Theta = Theta^^
# l(Theta) rund l(Theta) +(Theta-Theta^^) \nabla l(Theta) + 1/2*()(Theta-Theta^^)^2*\nabla^2 l(Theta)
# \nabla l(Theta) = S(Theta^^) = 0
# \nabla^2 l(Theta^^) = -I(Theta^^)
# l(Theta) rund l(Theta) - 1/2*()(Theta-Theta^^)^2*-I(Theta^^)
plot(theta.vec, loglikelihood.vec(theta.vec), type = "l")
points(theta.vec, loglikelihood(theta.hat)-(theta.vec-theta.hat)^2*fisher.obs/2, col=2, type="l")
abline(v=theta.hat, col=3)

# Varianz des MaximumLikelihood(ML)-Schätzers:
# Theta^^ = Theta^^(y_1,...,y_20) = n/(Sum^n_(i=1) y_i)
# Simulation
simanz = 1000
# Y_i rund Exp(Theta)
# Theta = 0.02
theta = 0.2
theta.hat.vec = rep(0, simanz)
for(i in 1:simanz){
  # simulation von Pseudobeobachtungen
  # es wird kleiner wenn n angehoben wird z.B. 20
  y.sim = rexp(20,theta)
  # bestimme ML-Schätzer
  theta.hat.vec[i] = length(y.sim)/sum(y.sim)
}

var(theta.hat.vec)

# untere Grenze für Varianz: 1/(F(Theta)) = 1/erwartete Fischerinformation
# I(Theta)= \nabla^2 l(Theta) = n/Theta^2
# F(Theta)=Ew_Theta(n/Theta^2)

# untere schranke

0.02^2/length(y.sim)
#ML-Schätzer konvergiert gegen untere schranke