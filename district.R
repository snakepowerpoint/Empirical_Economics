library(xtable)

rate <- read.table("2008election.csv", header=T, sep=",")
rate = rate[-c(19,39), -c(2, 8)]

mylogit <- glm(win ~ rKMTcore.rate, data=rate, family=binomial)
summary(mylogit) ### high core rate results in high probability of winning
mylogit1 <- glm(win ~ rswing.rate, data=rate, family=binomial)
summary(mylogit1) ## swing rate has nothing to do with winning probability
summary(glm(win ~ rDPPcore.rate, data=rate, family=binomial))
summary(glm(win ~ rKMTcore.rate + rDPPcore.rate + rswing.rate, 
            data=rate, family=binomial))
## linearly dependent

xtable(summary(mylogit))
xtable(summary(glm(win ~ rDPPcore.rate, data=rate, family=binomial)))
xtable(summary(mylogit1))

var(rate$rKMTcore.rate)
var(rate$rDPPcore.rate) ## similar
mean(rate$rKMTcore.rate)
mean(rate$rDPPcore.rate) ## different

diff <- rate[, 13] - rate[, 14]
max(diff)
min(diff)
sum(diff[diff>0])
sum(diff[diff<0])

##### normality test
library(MASS)
install.packages("sma")
library(sma)

qqnorm(rate$rKMTcore.rate, ylab="KMT Rate Quantiles")
qqline(rate$rKMTcore.rate)

qqnorm(rate$rDPPcore.rate, ylab="DPP Rate Quantiles")
qqline(rate$rDPPcore.rate)

chiqqplot(rate[, 13:14])

##### mean test

Sp <- var(rate[, 13])*(37-1)/(37+37-2) + var(rate[, 14])*(37-1)/(37+37-2)
T <- t(matrix(rate[, 13]-rate[, 14]))%*%matrix(rate[, 13]-rate[, 14])/(Sp*(1/37+1/37))
T <- T*(37+37-1-1)/(37+37-2)
1 - pf(T, df1=1, df2=37+37-1-1) ## reject Ho: mu1 = mu2

##### using different definition of core supporter and swing voter
##### and run again
summary(glm(win ~ KMTcore.rate, data=rate, family=binomial))
summary(glm(win ~ DPPcore.rate, data=rate, family=binomial))
summary(glm(win ~ swing.rate, data=rate, family=binomial))

xtable(summary(glm(win ~ KMTcore.rate, data=rate, family=binomial)))
xtable(summary(glm(win ~ DPPcore.rate, data=rate, family=binomial)))
xtable(summary(glm(win ~ swing.rate, data=rate, family=binomial)))

qqnorm(rate$KMTcore.rate, ylab="KMT Rate Quantiles")
qqline(rate$KMTcore.rate)

qqnorm(rate$DPPcore.rate, ylab="DPP Rate Quantiles")
qqline(rate$DPPcore.rate)

chiqqplot(rate[, 8:9])

############################################################################
#############     appendix       ###########################################
############################################################################
chiqqplot <-function(x){
    if(!is.matrix(x))
        x<-as.matrix(x)
    n<-nrow(x)
    p<-ncol(x)
    D2<-mahalanobis(x,colMeans(x),cov(x))
    qqplot(qchisq(ppoints(n), df = p), D2,xlab="Theoretical Q of Chisquare",
           ylab="Mah_distance",
           main = expression("Q-Q plot for" ~~ {chi^2}[p]), pch=19)
    abline(c(0,1))
}

