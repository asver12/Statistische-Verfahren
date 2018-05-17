
# Vektor
temp = c(7.5,8.4,9.1,9.0,10.0,10.7,11.8,11.9,12.8)
respiration = c(122,176,102,162,288,182,196,218,148)
x = temp
y = log(respiration)

plot(x,y,ylim=c(0,10),xlim=c(0,15))


beta0 = 4.2
beta1 = 0.09

abline(beta0,beta1, col=3,lwd=0.5)
sum(abs(y - (beta0-beta1*x)))

beta0.seq = seq(4,6,0.001)
beta1.seq = seq(0,0.1,0.001)

abline(4,0,col=2,lwd=0.2)
abline(6,0.1,col=4,lwd=0.2)

result = matrix(0,length(beta0.seq),length(beta1.seq))
for(i in 1:length(beta0.seq)){
  for(j in 1:length(beta1.seq)){
    beta0 = beta0.seq[i]
    beta1 = beta1.seq[j]
    result[i,j] = sum(abs(y-(beta0+beta1*x)))
  }
}
which(result == min(result), arr.ind=T)

beta0.seq[477]
beta1.seq[69]
abline(beta0.seq[477], beta1.seq[69],col=2,lwd=0.2)
