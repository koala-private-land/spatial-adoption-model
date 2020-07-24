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
    logit(p[i]) <- alpha_1 + N_1*HA_N[i]
  }
  
  for (i in 1:n) {                     
    CENS[i] ~ dinterval(WTA[i], LIM[i,]) 
    WTA[i] ~ dnorm(mu[i], tau)           
    mu[i] <- alpha_2 + n_1*HA_n[i]
  }                                       
 
  # Defining Priors  
  
  alpha_1 ~ dnorm(0,0.1)                 
  alpha_2 ~ dnorm(0,0.1)                 
  
  N_1 ~ dnorm(0,0.1)  
  n_1 ~ dnorm(0,0.1)  
  
  tau ~ dunif(0,10)                      
  
  # Predict participation in CC's for a property in northern-NSW
  for (i in 1:M) {
  logit(p_2[i]) <-  alpha_1 + N_1*HA_M[i]
  ACCEPT_p[i] ~ dbern(p_2[i])

  # Predict the WTA for a property in northern-NSW
  mu_p[i] <-  alpha_2 + n_1*HA_M[i]
  WTA_P[i] ~ dnorm(mu_p[i],tau)

}                                         

}

