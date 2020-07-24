#####################################################--------------------------#######
#   WTA MODEL MODEL              ----------------#######
#------------------------------------------------------------------------------#######
#    
# By: Carla Archibald & Jonathan Rhodes
# Created: 16 June 2020
# Objective: The thing we need to estimate from the statistical model 
#            is the probability distribution of f(X) based on willingness to 
#            accept survey data, actual adoption events, etc. 
#
###### ----------------------------------------------------------------------- #######
###### ----------------------------------------------------------------------- #######

# 1. Model ####

model {                                  
    
  for (i in 1:N) {                     
    ACCEPT[i] ~ dbern(p[i])
    logit(p[i]) <- alpha_1 + beta_1*HA_N[i]
  }
  
  for (i in 1:n) {                     
    CENS[i] ~ dinterval(WTA[i], LIM[i,]) 
    WTA[i] ~ dnorm(mu[i], tau)           
    mu[i] <- alpha_2 + beta_2*HA_M[i]   
  }                                       
 
  # Defining Priors  
  
  alpha_1 ~ dnorm(0,0.01)                 
  beta_1 ~ dnorm(0,0.01)  
  
  alpha_2 ~ dnorm(0,0.01)                 
  beta_2 ~ dnorm(0,0.01)                  
  
  tau ~ dunif(0,10)                      
  
}                                         



