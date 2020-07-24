#####################################################--------------------------#######
#   WTA MODEL MODEL                                  ----------------#######
#------------------------------------------------------------------------------#######
#    
# By: Carla Archibald & Johnathan Rhodes
# Created: 16 June 2020
# Objective: Th iInitialisationscript for the "wta-model" model scrip that estimates
#            the probability distribution of f(X) based on willingness to 
#            accept survey data, actual adoption events, etc. 
#
###### ----------------------------------------------------------------------- #######
###### ----------------------------------------------------------------------- #######
#---# Initialisation #---# 
# 1. Install Packages and install data ####

memory.limit(1000000)

library(coda)
library(rjags)
library(runjags)
library(snowfall)
library(parallel)
library(igraph)
library(DiagrammeR)
library(bayesplot)


load("R:/KPRIVATE19-A2212/code/social-survey/spatial-model-of-adoption/data-input/wta-input-data.RData")

## The data list
forjags <-list("ACCEPT"=ACCEPT, 
               "HA_N"=HA_N, 
               "CENS"=CENS,
               "WTA"=WTA,
               "HA_M"=HA_M,
               "LIM"=LIM,
               "N"=N,
               "n"=n)

## parameters-to-keep list (short for simplicity)
parms <-c("alpha_1","alpha_2","beta_1","beta_2") # save out a, b, c, d, mu, p

##########################################
### Initiating values 
##########################################

alpha_1.init <-rnorm(3,0,0.01)
alpha_2.init <-rnorm(3,0,0.01)

beta_1.init <-rnorm(3,0,0.01)
beta_2.init <-rnorm(3,0,0.01)

#WTA.init <-

####### Inits List ####

j.inits <-list(list(alpha_1 = alpha_1.init[1], alpha_2 = alpha_2.init[1], beta_1 = beta_1.init[1], beta_2 = beta_2.init[1]),# WTA = WTA.init[1]),
               list(alpha_1 = alpha_1.init[2], alpha_2 = alpha_2.init[2], beta_1 = beta_1.init[2], beta_2 = beta_2.init[2]),# WTA = WTA.init[2]),
               list(alpha_1 = alpha_1.init[3], alpha_2 = alpha_2.init[3], beta_1 = beta_1.init[3], beta_2 = beta_2.init[3]))#, WTA = WTA.init[3]))

####### Model ####

cl <- makeCluster(3)

jagsout_V2 <- run.jags(model="R:/KPRIVATE19-A2212/code/social-survey/spatial-model-of-adoption/implementation-success-model/wta-model.R",
                       monitor=parms,
                       inits=j.inits,
                       jags.refresh=30,
                       data=forjags, n.chains=3, 
                       burnin=1500,thin=100,sample=5000,
                       method="rjparallel")

#failed.jags(c('model')) #,'data','inits'

stopCluster(cl)

summary_V2 <-summary(jagsout_V2)
mcmc_trace(jagsout_V2, facet_args = list(ncol = 3))
write.csv(summary_V1, file="R:/KPRIVATE19-A2212/code/social-survey/spatial-model-of-adoption/data-output/cjagsout_V1_summry.csv")
save.image("R:/KPRIVATE19-A2212/code/social-survey/spatial-model-of-adoption/data-output/cjagsout_V1_summry.RData")
