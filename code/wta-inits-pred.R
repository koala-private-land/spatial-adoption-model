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
library(runjags)
library(snowfall)
library(parallel)
library(igraph)
library(DiagrammeR)
library(tidybayes)
library(bayesplot)


load("R:/KPRIVATE19-A2212/code/social-survey/spatial-model-of-adoption/data-input/wta-input-data-pred.RData")

## The data list
forjags <-list("ACCEPT"=ACCEPT, 
               "CENS"=CENS,
               "WTA"=WTA,
               "LIM"=LIM,
               "N"=N,
               "n"=n,
               "M"=M,
               "HA_N"=HA_N,   
               "HA_n"=HA_n,
               "HA_M"=HA_M)

## parameters-to-keep list (short for simplicity)
parms <- c("WTA_P") # save out a, b, c, d, mu, p

##########################################
### Initiating values 
##########################################

alpha_1.init <-rnorm(3,0,0.1)
alpha_2.init <-rnorm(3,0,0.1)

N_1.init <-rnorm(3,0,0.1)

n_1.init <-rnorm(3,0,0.1)

####### Inits List ####

j.inits <-list(list(alpha_1 = alpha_1.init[1], alpha_2 = alpha_2.init[1], N_1 = N_1.init[1], n_1 = n_1.init[1]),
               list(alpha_1 = alpha_1.init[2], alpha_2 = alpha_2.init[2], N_1 = N_1.init[2], n_1 = n_1.init[2]),
               list(alpha_1 = alpha_1.init[3], alpha_2 = alpha_2.init[3], N_1 = N_1.init[3], n_1 = n_1.init[3]))

####### Model ####

cl <- makeCluster(3)

jagsout_predV2 <- run.jags(model="R:/KPRIVATE19-A2212/code/social-survey/spatial-model-of-adoption/implementation-success-model/wta-model-pred.R",
                       monitor=parms,
                       inits=j.inits,
                       jags.refresh=30,
                       data=forjags, n.chains=3, 
                       burnin=1500,thin=100,sample=5000,
                       method="rjparallel")

failed.jags(c('model','inits')) #,'data','inits'

stopCluster(cl)

summary_pred_V2 <-summary(jagsout_predV2)
mcmc_trace(jagsout_predV2, facet_args = list(ncol = 3))
write.csv(summary_pred_V2, file="R:/KPRIVATE19-A2212/code/social-survey/spatial-model-of-adoption/data-output/predictive-model/summary_V2.csv")
save.image("R:/KPRIVATE19-A2212/code/social-survey/spatial-model-of-adoption/data-output/predictive-model/summary_V2.RData")

lm1_mcmc <- as.mcmc(jagsout_predV2)
plot(lm1_mcmc)
