## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(QUALYPSO)

## ----Tab1, echo=FALSE, out.width="90%"----------------------------------------
knitr::kable(head(scen_DJFTas), format = "markdown",caption = "Two factors characterise the climate projections for the ensemble of average winter temperatures: the GCM and the RCM.")

## ----Fig1, fig.cap="Ensemble of average winter temperature (DJF) climate projections for the SREX Central-Eastern Europe CEU region, obtained with 20 combinations of four GCMs and five RCMs. The different colors correspond to the different GCMs.", echo=TRUE, out.width="90%"----
# plot of the 20 projections
vecGCM = unique(scen_DJFTas$GCM)
X_time_vec=1971:2099
plot(-1,-1,xlim=range(X_time_vec),ylim=range(Y_DJFTas),xlab="Years",ylab="DJF Temperature (°C)")
for(i in 1:nrow(Y_DJFTas)){
  lines(X_time_vec,Y_DJFTas[i,],col=match(scen_DJFTas$GCM[i], vecGCM))
}
legend("bottomright",legend = vecGCM, col=1:length(vecGCM),lty=1)

## ----Fig2, fig.cap="Climate responses obtained with different trend models (smoothing splines, 'SS'; polynomial models, 'poly', and lowess) and different smoothing parameters.", echo = TRUE, out.width="90%"----
# illustration for the first simulation
iSimu=1
X_time_vec=1971:2099
dfCR = data.frame(x=X_time_vec,y=Y_DJFTas[iSimu,])
plot(-1,-1,xlim=range(dfCR$x),ylim=range(dfCR$y),xlab="Years",ylab="DJF Temperature (°C)")
lines(dfCR$x,dfCR$y,col="black")
lableg = c()
colleg = c()
ltyleg = c()

# fit smoothing splines, df can take values between 1 and the length of the simulations (here, 129)
vec_df_SS = c(3,6,15)
for(idf in 1:length(vec_df_SS)){
  ySS = stats::smooth.spline(x = dfCR$x,y = dfCR$y, df=vec_df_SS[idf])$y
  lines(X_time_vec,ySS,col="red",lty=idf,lwd=2)
}
lableg = c(lableg,paste0("SS: df=",vec_df_SS))
colleg = c(colleg,rep("red",length(vec_df_SS)))
ltyleg = c(ltyleg,1:length(vec_df_SS))

# fit polynomial curves of order 1,2, and 3
vec_poly = c(1,2,4)
for(iorder in 1:length(vec_poly)){
  yPOLY = stats::lm(y ~ stats::poly(x, vec_poly[iorder]), data = dfCR)
  lines(X_time_vec,yPOLY$fitted.values,col="blue",lty=iorder,lwd=2)
}
lableg = c(lableg,paste0("poly: degree=",vec_poly))
colleg = c(colleg,rep("blue",length(vec_poly)))
ltyleg = c(ltyleg,1:length(vec_poly))

# fit LOWESS with different degrees of smoothness
vec_lowess = c(0.2,0.4,0.6)
for(ilowess in 1:length(vec_lowess)){
  yLOWESS <- stats::lowess(x=dfCR$x,y=dfCR$y, f=vec_lowess[ilowess], iter=0, delta=0)
  lines(yLOWESS$x,yLOWESS$y,col="green",lty=ilowess,lwd=2)
}
lableg = c(lableg,paste0("lowess: f=",vec_lowess))
colleg = c(colleg,rep("green",length(vec_lowess)))
ltyleg = c(ltyleg,1:length(vec_lowess))

legend("bottomright",legend = lableg,lty=ltyleg,col=colleg)

## ----echo=TRUE----------------------------------------------------------------
# create nS=3 fictive climate scenarios with 2 GCMs and 2 RCMs, for a period of nY=20 years
n=20
t=0:n/n

# GCM effects (sums to 0 for each t)
effGCM1 = t*2
effGCM2 = t*1
effGCM3 = t*-3

# RCM effects (sums to 0 for each t)
effRCM1 = t*1
effRCM2 = t*-1

# These climate scenarios are a sum of effects and a random gaussian noise
scenGCM1RCM1 = effGCM1 + effRCM1 + rnorm(n=n+1,sd=0.5)
scenGCM1RCM2 = effGCM1 + effRCM2 + rnorm(n=n+1,sd=0.5)
scenGCM2RCM1 = effGCM2 + effRCM1 + rnorm(n=n+1,sd=0.5)
scenGCM2RCM2 = effGCM2 + effRCM2 + rnorm(n=n+1,sd=0.5)
scenGCM3RCM1 = effGCM3 + effRCM1 + rnorm(n=n+1,sd=0.5)
Y_synth = rbind(scenGCM1RCM1,scenGCM1RCM2,scenGCM2RCM1,scenGCM2RCM2,scenGCM3RCM1)

# Here, scen_Synth indicates that the first scenario is obtained with the combination of the
# GCM "GCM1" and RCM "RCM1", the second scenario is obtained with the combination of
# the GCM "GCM1" and RCM "RCM2", etc.
scen_Synth = data.frame(GCM=c('GCM1','GCM1','GCM2','GCM2','GCM3'),
RCM=c('RCM1','RCM2','RCM1','RCM2','RCM1'))

## ----echo=TRUE----------------------------------------------------------------
# run QUALYPSO
X = seq(from=2000,to=2100,by=5)
Q_synth = QUALYPSO(Y=Y_synth, scen=scen_Synth, X=X,listOption = list(typeClimateResponse="poly"))

## ----Fig3, fig.cap="Raw projection (plain curve), fitted climate response (thick line) and true climate response (dotted line)", echo=TRUE, out.width="90%"----
plotQUALYPSO_CR(Q_synth,i=1)
lines(X, effGCM1 + effRCM1,lty=2)
legend("topleft",inset=c(0,0.1), legend = c("True response"), lty = 2, bty = "n")

## ----Fig4, fig.cap="Median estimates of the main effects (plain lines) and corresponding 90% credible intervals, and true main effects (dotted lines)", echo=TRUE, out.width="90%"----
plotQUALYPSO_effect(Q_synth,nameEff="GCM",xlab="Years")
lines(X, effGCM1,lty=2,lwd=2,col="black")
lines(X, effGCM2,lty=2,lwd=2,col="red")
lines(X, effGCM3,lty=2,lwd=2,col="green")
legend("topleft",inset=c(0,0.1), legend = c("Truth"), lty = 2, bty = "n")

## ----Fig5, fig.cap = "Fitted climate response for two projections", echo=TRUE, out.width="45%"----

# call QUALYPSO
Q_time = QUALYPSO(Y=Y_DJFTas,scen=scen_DJFTas,X=1971:2099)

# diagnostic of the climate responses
for(iS in 1:2) plotQUALYPSO_CR(QUALYPSOOUT = Q_time, iS = iS)

## ----Fig6, fig.cap = "Diagnostic plot for internal variability. Points represent all $D_i(t)$ estimates as a function of $X$. The red curves correspond to [-1.645*$\\sqrt{IV(t)}$ , +1.645*$\\sqrt{IV(t)}$] and should contain about 90% of the points.", echo=TRUE, out.width="90%"----
# diagnostic of the internal variability
plotQUALYPSO_IV(QUALYPSOOUT = Q_time)

## ----Fig7, fig.cap="Global warming levels from the four GCMs.", echo=TRUE, out.width="90%"----
vecGCM = unique(scen_DJFTas$GCM)
X_time_vec = 1971:2099
plot(-1,-1,xlim=range(X_time_vec),ylim=range(X_DJFTas_WL),xlab="Years",ylab="Global warming levels (°C)")
for(i in 1:nrow(X_DJFTas_WL)){
  lines(X_time_vec,X_DJFTas_WL[i,],col=match(scen_DJFTas$GCM[i], vecGCM))
}
legend("bottomright",legend = vecGCM, col=1:length(vecGCM),lty=1)

## ----Fig8, fig.cap="Fraction of total variance for absolute changes in mean winter temperature as a function of global warming levels.", echo=TRUE, out.width="90%"----
# call QUALYPSO
Q_wl = QUALYPSO(Y=Y_DJFTas,scen=scen_DJFTas,X=X_DJFTas_WL,Xfut=seq(from=1,to=3,by=0.1))

# variance decomposition
plotQUALYPSO_effect(QUALYPSOOUT = Q_wl,nameEff="GCM",xlab="lobal warming levels (°C)")

## ----echo=TRUE, out.width="90%"-----------------------------------------------
plot(-1,-1,xlim=range(X_SWE_WL),ylim=range(Y_SWE),
xlab="Global warming levels (°C)",ylab = "SWE maxima (mm w.e.)")
for(i in 1:nrow(Y_SWE)){
  lines(X_SWE_WL[i,],Y_SWE[i,],col=i)
}

## ----echo = TRUE, out.width="90%"---------------------------------------------
# illustration for the first simulation
iSimu=1

# df can take values between 1 and the length of the simulations (here, 129)
vec_df = c(3,6,15)

# take one projection
dfpred = data.frame(x=X_SWE_WL[4,],y=Y_SWE[4,])

plot(-1,-1,xlim=range(dfpred$x),ylim=range(dfpred$y),
xlab="Global warming levels (°C)",ylab = "SWE maxima (mm w.e.)")

# observations
points(dfpred$x,dfpred$y,pch=20,cex=1.3)

# smoothing splines
smooth.spline.out<- stats::smooth.spline(x=dfpred$x,y=dfpred$y,df=4)
lines(dfpred$x,smooth.spline.out$y,col="red",lwd=2)

# polynomial function
lm_out = stats::lm(y ~ stats::poly(x, 1), data = dfpred)
lines(dfpred$x,lm_out$fitted.values,col="green",lwd=2)

# lowess
lowess_out <- lowess(x=dfpred$x,y=dfpred$y, f=0.6, iter=0, delta=0)
lines(dfpred$x,lowess_out$y,col="blue",lwd=2)

# tweedie
glm_out <- stats::glm(y ~ x, 
        family = statmod::tweedie(var.power = 1.5, link.power = 0), 
        data = dfpred)
lines(dfpred$x,glm_out$fitted.values,col="orange",lwd=2)
        
legend("topright",legend = c("smooth.spline","poly","lowess","tweedie"),
lwd=2,col=c("red","green","blue","orange"),title="climate response model")

## ----echo = TRUE, out.width="90%"---------------------------------------------
# list of options
listOption = list(typeChangeVariable='abs',typeClimateResponse="tweedie")

# call QUALYPSO
Q_SWE = QUALYPSO(Y=Y_SWE,scen=scen_SWE,X=X_SWE_WL,Xfut=seq(from=0.5,to=3,by=0.1),
listOption=listOption)

# plot climate responses
plotQUALYPSO_CCR(Q_SWE)


## ----echo = TRUE, out.width="45%"---------------------------------------------
# remove scenario for one GCM
Y_sel = Y_DJFTas[-c(12,13,14,15,17,18,19,20),]
scen_sel = scen_DJFTas[-c(12,13,14,15,17,18,19,20),]

# lm
listOption = list(typeChangeVariable='abs',typeClimateReponse = "smooth.spline", parClimateResponse = 3)
system.time({Q_lm = QUALYPSO(Y=Y_sel,scen=scen_sel,X=1971:2099,Xfut=1990:2099,listOption=listOption)})

# Bayesian
listOption = list(typeChangeVariable='abs',parClimateResponse=3, ANOVAmethod="Bayesian",nBurn=2000,nKeep=5000)
system.time({Q_Bayes = QUALYPSO(Y=Y_sel,scen=scen_sel,X=1971:2099,Xfut=1990:2099,listOption=listOption)})

# main GCM effects
lQ = list(lm=Q_lm,Bayesian=Q_Bayes)
for(method in names(lQ)) plotQUALYPSO_effect(lQ[[method]],nameEff="GCM",xlab="Years",main=method)

## ----echo = TRUE, out.width="90%"---------------------------------------------
library(MASS)
library(gamlss)

# define dimensions
Xs = 1971:2099
Xfut = 1990:2099
Xref = Xfut[1]
nY = length(Xs)
nFut = length(Xfut)
nS = nrow(Y_DJFTas)

# initialise matrices
CCR_tfut = CR_tfut = matrix(nrow=nS,ncol=nFut)
D_t = CHANGEVAR_t = matrix(nrow=nS,ncol=nY)

for(iS in 1:nS){
  Ys = Y_DJFTas[iS,]

  # data.frame with Xs and Ys
  dfpred = data.frame(x=Xs,y=Ys)

  # apply robust regression
  rlm_out = rlm(y~x,data = dfpred)

  # fitted responses at the points of the fit (for Dt), at Xfut and at Xref
  CR_t_s = predict(rlm_out, newdata = data.frame(x = Xs))
  CR_tfut_s = predict(rlm_out, newdata = data.frame(x = Xfut))
  CR_tref = predict(rlm_out, newdata = data.frame(x = Xref))     

  # store climate response for this simulation chain
  CR_tfut[iS,] = CR_tfut_s

  # fill matrices
  CCR_tfut[iS,] = CR_tfut_s-CR_tref
  D_t[iS,] = Ys-CR_t_s
  CHANGEVAR_t[iS,] = Ys-CR_tref
}

# Heteroscedasticity: assume gaussian deviations from the climate responses
# with a mean of zero
Xmat = matrix(rep(Xs,nS),byrow=T,nrow=nS,ncol=nY)
df_fit = data.frame(x = as.vector(Xmat), y = as.vector(D_t))
gamlss_out = gamlss(y ~ x, sigma.formula = ~x, family = gamlss.dist::NO(), data = df_fit,mu.fix = TRUE, 
mu.start = 0,control = gamlss.control(trace = FALSE))

# predict IV(t) for Xfut
sigma_pred = predict(object = gamlss_out, what = "sigma", type = "response", newdata = data.frame(x=Xfut),
data = df_fit)
IV_tfut = sigma_pred^2

# return objects
ClimateResponse = list(CR_tfut=CR_tfut,CCR_tfut=CCR_tfut,D_t=D_t,CHANGEVAR_t=CHANGEVAR_t,IV_tfut=IV_tfut)

# call QUALYPSO with this list of custom climate responses
Q_customCR = QUALYPSO(Y=Y_DJFTas,scen=scen_DJFTas,X=Xs,Xfut=Xfut,
listOption=list(ClimateResponse=ClimateResponse))

## ----echo = TRUE, out.width="90%"---------------------------------------------
plotQUALYPSO_IV(Q_SWE)

