library(BayesCP)



#simulate data

dat<-bayescp_simulate(
  
  n=100,
  tau=50,
  mu1=0,
  mu2=1,
  sigma=1
  
)


#create prior

prior<-bayescp_prior(
  
  
  m01=0,
  m02=1,
  kappa01=5,
  kappa02=5
  
)


#fit model

fit<-bayescp_fit(
  
  dat$y,
  prior=prior
  
)


#summaries

summary(fit)


#plots

plot(fit)