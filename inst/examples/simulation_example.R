library(ExactBayesCP)


#--------------------------------
# Example 1
# Simulate Gaussian data
#--------------------------------

dat<-bayescp_simulate(
  
  n=100,
  tau=50,
  mu1=0,
  mu2=1,
  sigma=1
  
)



#--------------------------------
# Example 2
# Specify priors
#--------------------------------

prior<-bayescp_prior(
  
  m01=0,
  m02=1,
  kappa01=5,
  kappa02=5
  
)



#--------------------------------
# Example 3
# Fit the model
#--------------------------------

fit<-bayescp_fit(
  
  dat$y,
  prior=prior
  
)



#--------------------------------
# Example 4
# Summaries
#--------------------------------

summary(fit)



#--------------------------------
# Example 5
# Posterior plot
#--------------------------------

plot(fit)



#--------------------------------
# Example 6
# Simulation study
#--------------------------------


scenarios<-data.frame(
  
  scenario_id="P001",
  n=100,
  delta=1,
  rho=0.5
  
)



priors<-function(sc){
  
  bayescp_prior_grid(
    
    m01=0,
    m02=sc$delta
    
  )
  
}


result<-bayescp_run_study(
  
  scenarios=scenarios,
  priors=priors,
  replications=5,
  draws=100
  
)



#--------------------------------
# Finished
#--------------------------------
