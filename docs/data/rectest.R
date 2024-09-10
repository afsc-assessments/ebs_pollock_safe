
## Quick simulation script to explore bias correction in recruitment.
## Note this requires a fresh install of RTMB to run
## devtools::install_github('kaskr/RTMB/RTMB')
library(RTMB)

## global parameters of interest
nyears <- 150
m <- 5 # mean recruits in natural space
sigR <- .8 # sigmaR

## Simulate recruitment and observed data
devs0 <- rnorm(nyears, 0, sigR)
rec <- m*exp(devs0-sigR^2/2)
#rec <- m*exp(devs0) #-sigR^2/2)
mean(rec)
## sample from them to mimic info in a stock assessment
CV <- sort(runif(nyears, .01, .5), decreasing=TRUE)  # observation error
obs <- rnorm(nyears, mean=rec, sd=CV*rec)
##obs[1:10] <- NA

## Fit TMB for a simple recruitment random effects model. dat$b is the recruitment adjustment from M-T
dat <- list(obs=obs, sigR=sigR, CV=CV, b=rep(1,nyears))
pars <- list(logm=log(5), devs=rnorm(nyears)*0)
## library(TMB)
## compile('bc.cpp')
## dyn.load('bc.dll')
## obj <- MakeADFun(dat, pars, silent=TRUE, random='devs')
f <- function(pars) {
    getAll(dat,pars)
    m <- exp(logm)
    rec2 <- m*exp(devs-b*sigR^2/2)
    nll <-
        -sum(dnorm(devs, 0, sigR, log=TRUE))+
        -sum(dnorm(obs, rec2+sigR^2/2, sd=CV*rec2, log=TRUE))
    REPORT(rec2)
    ADREPORT(rec2)
    REPORT(m)
    return(nll)
}
obj <- MakeADFun(f, pars, silent=TRUE, random='devs')
## obj$fn()-f(pars)
opt <- nlminb(obj$par, obj$fn, obj$gr)
mhat <- exp(opt$par[1])
mhat

## Extract outputs
sdrep0 <- sdreport(obj, bias.correct=FALSE)
sdrep1 <- sdreport(obj, bias.correct=TRUE)
ses0 <- as.list(sdrep0, 'Std. Error')$devs
ses1 <- as.list(sdrep1, 'Std. Error')$devs
##ses0-ses1
devhat <- as.list(sdrep0, 'Est')$devs
bhat <- 1- ses0^2/sigR^2 ## Methot-Taylor form
rec2 <- obj$report()$rec2
## bias-corrected recruitment
rec2bc <- sdrep1$unbiased$value[1:nyears] ## Thorson approach

## Can now put the bias factor (bhat) back into the model and confirm it improves (SS3)
dat$b <- bhat
obj2 <- MakeADFun(f, pars, silent=TRUE, random='devs')
## opt <- TMBhelper::fit_tmb(obj, control=list(trace=0))
opt2 <- nlminb(obj2$par, obj2$fn, obj2$gr)
rec2bcMT <- obj2$report()$rec2
mean(rec2bcMT)
mean(rec2bc)
mean(rec2)

par(mfrow=c(3,1), mar=c(2,4,1,1))
ylim <- c(min(devhat-ses0*2), max(devhat+ses0*2))
plot(devhat, ylim=ylim, ylab='Recdev estimate')
segments(1:nyears, y0=devhat-1.96*ses0, y1=devhat+1.96*ses0)
plot(exp(devs0-sigR^2/2), ylim=c(0,1.1*max(exp(devhat))), type='o', lty=3, col=gray(.2),
     ylab='Recruitment multiplier')
lines(exp(devhat), col=2) # none
lines(exp(devhat-bhat*sigR^2/2), col=1) # M-T
lines(sdrep1$unbiased$value[-(1:nyears)], col=3) # Thorson
legend('top', legend=c('None', 'M-T', 'Epsilon'), col=c(2,1,3), lty=1, ncol=3, bty='n')
plot(rec-rec, type='l', ylim=range(rec2-rec), ylab='Recruitment')
lines(rec2-rec, col=2)
lines(rec2bc-rec, col=2)
lines(rec2bcMT-rec, col=4)
legend <- c(paste0('Truth (',round(mean(rec),2), ')'),
            paste0('Naive (',round(mean(rec2),2), ')'),
            paste0('Bias-corrected (',round(mean(rec2bc),2), ")"),
            paste0('M-T (',round(mean(rec2bcMT),2), ")")
            )
legend('bottomright', legend=legend, lty=1, col=1:4, bty='n')






