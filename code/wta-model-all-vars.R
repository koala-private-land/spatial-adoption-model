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
    
  for (i in 1:A) {                     
    ACCEPT[i] ~ dbern(p[i])
    logit(p[i]) <- A_int + A_1*HA_A[i] #+ N_2*AGE_N[i] + N_3*GENDER_N[i] + N_4*UNI_N[i] + N_5*RETIRE_N[i] + N_6*BLUECOLL_N[i] + N_7*FARMING_N[i] + N_8*RESIDENCE_N[i] + N_9*INCOME_N[i] + N_10*MORTGAGE_N[i] + N_11*KOALA_N[i] 
  }
  
  for (i in 1:W) {                     
    CENS[i] ~ dinterval(WTA[i], LIM[i,]) 
    WTA[i] ~ dnorm(mu[i], tau)           
    mu[i] <- W_int + W_1*HA_W[i] #+ n_2*AGE_n[i] + n_3*GENDER_n[i] + n_4*UNI_n[i] + n_5*RETIRE_n[i] + n_6*BLUECOLL_n[i] + n_7*FARMING_n[i] + n_8*RESIDENCE_n[i] + n_9*INCOME_n[i] + n_10*MORTGAGE_n[i] + n_11*KOALA_n[i]   
  }                                       
 
  # Defining Priors  
  
  A_int ~ dnorm(0,0.01)                 
  W_int ~ dnorm(0,0.01)                 
  
  A_1 ~ dnorm(0,0.01)  
 # N_2 ~ dnorm(0,0.01) 
 # N_3 ~ dnorm(0,0.01)  
 # N_4 ~ dnorm(0,0.01)
 # N_5 ~ dnorm(0,0.01)  
 # N_6 ~ dnorm(0,0.01)
 # N_7 ~ dnorm(0,0.01)  
 # N_8 ~ dnorm(0,0.01)
 # N_9 ~ dnorm(0,0.01)
 # N_10 ~ dnorm(0,0.01)  
 # N_11 ~ dnorm(0,0.01)
  
  W_1 ~ dnorm(0,0.01)  
 # n_2 ~ dnorm(0,0.01) 
 # n_3 ~ dnorm(0,0.01)  
 # n_4 ~ dnorm(0,0.01)
 # n_5 ~ dnorm(0,0.01)  
 # n_6 ~ dnorm(0,0.01)
 # n_7 ~ dnorm(0,0.01)  
 # n_8 ~ dnorm(0,0.01)
 # n_9 ~ dnorm(0,0.01)
 # n_10 ~ dnorm(0,0.01)  
 # n_11 ~ dnorm(0,0.01)
  
  tau ~ dunif(0,10)                      
  
}                                         



