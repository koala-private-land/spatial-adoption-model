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


load("R:/KPRIVATE19-A2212/code/social-survey/spatial-model-of-adoption/data-input/wta-input-data.RData")

## The data list
forjags <-list("ACCEPT"=ACCEPT, 
               "CENS"=CENS,
               "WTA"=WTA,
               "LIM"=LIM,
               "A"=A,
               "W"=W,
               "HA_A"=HA_A,#,"AGE_N"=AGE_N, "GENDER_N"=GENDER_N, "UNI_N"=UNI_N, "RETIRE_N"=RETIRE_N, "BLUECOLL_N"=BLUECOLL_N, "FARMING_N"=FARMING_N, "RESIDENCE_N"=RESIDENCE_N, "INCOME_N"=INCOME_N, "MORTGAGE_N"=MORTGAGE_N, "KOALA_N"=KOALA_N,   
               "HA_W"=HA_W)#, "AGE_n"=AGE_n, "GENDER_n"=GENDER_n, "UNI_n"=UNI_n, "RETIRE_n"=RETIRE_n, "BLUECOLL_n"=BLUECOLL_n, "FARMING_n"=FARMING_n, "RESIDENCE_n"=RESIDENCE_n, "INCOME_n"=INCOME_n, "MORTGAGE_n"=MORTGAGE_n, "KOALA_n"=KOALA_n)

## parameters-to-keep list (short for simplicity)
parms <-c("A_int","W_int",
          "A_1", #"N_2","N_3","N_4","N_5","N_6","N_7","N_8","N_9","N_10","N_11",
          "W_1") #,"n_2","n_3","n_4","n_5","n_6","n_7","n_8","n_9","n_10","n_11") # save out a, b, c, d, mu, p

##########################################
### Initiating values 
##########################################

A_int.init <-rnorm(3,0,0.01)
W_int.init <-rnorm(3,0,0.01)

A_1.init <-rnorm(3,0,0.01)
#N_2.init <-rnorm(3,0,0.01)
#N_3.init <-rnorm(3,0,0.01)
#N_4.init <-rnorm(3,0,0.01)
#N_5.init <-rnorm(3,0,0.01)
#N_6.init <-rnorm(3,0,0.01)
#N_7.init <-rnorm(3,0,0.01)
#N_8.init <-rnorm(3,0,0.01)
#N_9.init <-rnorm(3,0,0.01)
#N_10.init <-rnorm(3,0,0.01)
#N_11.init <-rnorm(3,0,0.01)

W_1.init <-rnorm(3,0,0.01)
#n_2.init <-rnorm(3,0,0.01)
#n_3.init <-rnorm(3,0,0.01)
#n_4.init <-rnorm(3,0,0.01)
#n_5.init <-rnorm(3,0,0.01)
#n_6.init <-rnorm(3,0,0.01)
#n_7.init <-rnorm(3,0,0.01)
#n_8.init <-rnorm(3,0,0.01)
#n_9.init <-rnorm(3,0,0.01)
#n_10.init <-rnorm(3,0,0.01)
#n_11.init <-rnorm(3,0,0.01)

####### Inits List ####

j.inits <-list(list(A_int = A_int.init[1], W_int = W_int.init[1], A_1 = A_1.init[1], W_1 = W_1.init[1]),  #N_2 = N_2.init[1], N_3 = N_3.init[1], N_4 = N_4.init[1], N_5 = N_5.init[1], N_6 = N_6.init[1], N_7 = N_7.init[1], N_8 = N_8.init[1], N_9 = N_9.init[1], N_10 = N_10.init[1], N_11 = N_11.init[1], n_1 = n_1.init[1], n_2 = n_2.init[1], n_3 = n_3.init[1], n_4 = n_4.init[1], n_5 = n_5.init[1], n_6 = n_6.init[1], n_7 = n_7.init[1], n_8 = n_8.init[1], n_9 = n_9.init[1], n_10 = n_10.init[1], n_11 = n_11.init[1]),
               list(A_int = A_int.init[2], W_int = W_int.init[2], A_1 = A_1.init[2], W_1 = W_1.init[2]),  #N_2 = N_2.init[2], N_3 = N_3.init[2], N_4 = N_4.init[2], N_5 = N_5.init[2], N_6 = N_6.init[2], N_7 = N_7.init[2], N_8 = N_8.init[2], N_9 = N_9.init[2], N_10 = N_10.init[2], N_11 = N_11.init[2], n_1 = n_1.init[2], n_2 = n_2.init[2], n_3 = n_3.init[2], n_4 = n_4.init[2], n_5 = n_5.init[2], n_6 = n_6.init[2], n_7 = n_7.init[2], n_8 = n_8.init[2], n_9 = n_9.init[2], n_10 = n_10.init[2], n_11 = n_11.init[2]),
               list(A_int = A_int.init[3], W_int = W_int.init[3], A_1 = A_1.init[3], W_1 = W_1.init[3])) #N_2 = N_2.init[3], N_3 = N_3.init[3], N_4 = N_4.init[3], N_5 = N_5.init[3], N_6 = N_6.init[3], N_7 = N_7.init[3], N_8 = N_8.init[3], N_9 = N_9.init[3], N_10 = N_10.init[3], N_11 = N_11.init[3], n_1 = n_1.init[3], n_2 = n_2.init[3], n_3 = n_3.init[3], n_4 = n_4.init[3], n_5 = n_5.init[3], n_6 = n_6.init[3], n_7 = n_7.init[3], n_8 = n_8.init[3], n_9 = n_9.init[3], n_10 = n_10.init[3], n_11 = n_11.init[3]))

####### Model ####

cl <- makeCluster(3)

jagsout_V7 <- run.jags(model="R:/KPRIVATE19-A2212/code/social-survey/spatial-model-of-adoption/implementation-success-model/wta-model-all-vars.R",
                       monitor=parms,
                       inits=j.inits,
                       jags.refresh=30,
                       data=forjags, n.chains=3, 
                       burnin=1500,thin=100,sample=5000,
                       method="rjparallel")

failed.jags(c('model')) #,'data','inits'

stopCluster(cl)

summary_V7 <-summary(jagsout_V7)
summary_V7
write.csv(summary_V7, file="R:/KPRIVATE19-A2212/code/social-survey/spatial-model-of-adoption/data-output/summary_V7.csv")
save.image("R:/KPRIVATE19-A2212/code/social-survey/spatial-model-of-adoption/data-output/summary_V4.RData")
