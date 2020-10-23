# load packages
library(vctrs)
library(tidyverse)
library(runjags)
library(rjags)
library(coda)
library(snowfall)
library(parallel)
library(modeest)

# load data
rd_WTA_data <-read.csv("./data/ppData2.csv")

# load functions
source("./code/functions.r")

# data preparation

# fill missing values - THIS IS A VERY NAIVE DATA IMPUTATION METHOD - NEED TO LOOK AT THIS TO IMPROVE (24/7/20)
rd_WTA_data <- rd_WTA_data %>%
               mutate(ha = ifelse(is.na(ha), mean(ha, na.rm = T), ha),
                      Q8.4_Inf_WTA = trimws(as.character(Q8.4_Inf_WTA)),
                      Q8.6_10yr_WTA = trimws(as.character(Q8.6_10yr_WTA)),
                      Age = ifelse(is.na(Age), mean(Age, na.rm = T), Age),
                      Residence = ifelse(is.na(Residence), mean(Residence, na.rm = T), Residence),
                      Retired = ifelse(is.na(Retired), "0", Retired),
                      BlueCollar = ifelse(is.na(BlueCollar), "0", BlueCollar),
                      HighMortgage = ifelse(is.na(HighMortgage), "0", HighMortgage),
                      HighIncome = ifelse(is.na(HighIncome), "0",  HighIncome),
                      SeenKoala = ifelse(is.na(SeenKoala), "0", SeenKoala),
                      RelianceFarmingGT25perc = ifelse(is.na(RelianceFarmingGT25perc), "0", RelianceFarmingGT25perc)) %>% as_tibble()

# whether the landholder would even consider adopting an agreement or not for a perpetual covenant
ACCEPT.Inf.df <- rd_WTA_data %>%
                  mutate(ACCEPT = recode(Q8.4_Inf_WTA,
                                   "$50"="1",
                                   "$100"="1",
                                   "$1,500"="1",
                                   "$2,500"="1",
                                   ">$2,500"="1",
                                   "$2,000"="1",
                                   "I would not participate"="0",
                                   "I would pay"="1",
                                   "$500"="1",
                                   "$750"="1",
                                   "$25"="1",
                                   "$1,000"="1",
                                   "$250"="1",
                                   "$0"="1",
                                   " "=NA_character_, .default =NA_character_)) %>%
                  select(ACCEPT)

ACCEPT.Inf <- as.numeric(unlist(ACCEPT.Inf.df))

# whether the landholder would even consider adopting an agreement or not for a 10 year covenant
ACCEPT.10yr.df <- rd_WTA_data %>%
                   mutate(ACCEPT = recode(Q8.6_10yr_WTA,
                                   "$50"="1",
                                   "$100"="1",
                                   "$1,500"="1",
                                   "$2,500"="1",
                                   ">$2,500"="1",
                                   "$2,000"="1",
                                   "I would not participate"="0",
                                   "I would pay"="1",
                                   "$500"="1",
                                   "$750"="1",
                                   "$25"="1",
                                   "$1,000"="1",
                                   "$250"="1",
                                   "$0"="1",
                                   " "=NA_character_, .default=NA_character_)) %>%
                   select(ACCEPT)

ACCEPT.10yr <- as.numeric(unlist(ACCEPT.10yr.df))

# wta for perpetual covenant
WTA.Inf <- rd_WTA_data %>%
            mutate(WTA = recode(Q8.4_Inf_WTA,
                          "$50"="50",
                          "$100"="100",
                          "$1,500"="1500",
                          "$2,500"="2500",
                          ">$2,500"=NA_character_,
                          "$2,000"="2000",
                          "I would not participate"=NA_character_,
                          "I would pay"=NA_character_,
                          "$500"="500",
                          "$750"="750",
                          "$25"="25",
                          "$1,000"="1000",
                          "$250"="250",
                          "$0"="0",
                          " "=NA_character_, .default=NA_character_)) %>%
            select(WTA)
WTA.Inf <- as.numeric(unlist(WTA.Inf)) / 1000 # to convert to 1000s of dollars


# wta for 10 year covenant
WTA.10yr <- rd_WTA_data %>%
            mutate(WTA = recode(Q8.6_10yr_WTA,
                          "$50"="50",
                          "$100"="100",
                          "$1,500"="1500",
                          "$2,500"="2500",
                          ">$2,500"=NA_character_,
                          "$2,000"="2000",
                          "I would not participate"=NA_character_,
                          "I would pay"=NA_character_,
                          "$500"="500",
                          "$750"="750",
                          "$25"="25",
                          "$1,000"="1000",
                          "$250"="250",
                          "$0"="0",
                          " "=NA_character_, .default=NA_character_)) %>%
            select(WTA)
WTA.10yr <- as.numeric(unlist(WTA.10yr)) / 1000 # to convert to 1000s of dollars

# censoring for perpetual covenant
CENS.Inf <- rd_WTA_data %>%
              mutate(CENS = recode(Q8.4_Inf_WTA,
                         "$50"="1",
                         "$100"="1",
                         "$1,500"="1",
                         "$2,500"="1",
                         ">$2,500"="2",
                         "$2,000"="1",
                         "I would not participate"=NA_character_,
                         "I would pay"="0",
                         "$500"="1",
                         "$750"="1",
                         "$25"="1",
                         "$1,000"="1",
                         "$250"="1",
                         "$0"="1",
                         " "=NA_character_, .default=NA_character_)) %>%
              select(CENS)
CENS.Inf <- as.numeric(unlist(CENS.Inf))

# censoring for 10 year covenant
CENS.10yr <- rd_WTA_data %>%
              mutate(CENS = recode(Q8.6_10yr_WTA,
                         "$50"="1",
                         "$100"="1",
                         "$1,500"="1",
                         "$2,500"="1",
                         ">$2,500"="2",
                         "$2,000"="1",
                         "I would not participate"=NA_character_,
                         "I would pay"="0",
                         "$500"="1",
                         "$750"="1",
                         "$25"="1",
                         "$1,000"="1",
                         "$250"="1",
                         "$0"="1",
                         " "=NA_character_, .default=NA_character_)) %>%
              select(CENS)
CENS.10yr <- as.numeric(unlist(CENS.10yr))

#property size
AREA  <- rd_WTA_data %>%
    select(ha)
AREA <- as.numeric(unlist(AREA))
LOG_AREA <- log(AREA)

# select only required records for the JAGS model
# and scale covariates where necessary
CENS.Inf.New <- CENS.Inf[which(ACCEPT.Inf == 1)]
CENS.10yr.New <- CENS.10yr[which(ACCEPT.10yr == 1)]
WTA.Inf.New <- WTA.Inf[which(ACCEPT.Inf == 1)]
WTA.10yr.New <- WTA.10yr[which(ACCEPT.10yr == 1)]
ACCEPT.Inf.New <- ACCEPT.Inf[which(!is.na(ACCEPT.Inf))]
ACCEPT.10yr.New <- ACCEPT.10yr[which(!is.na(ACCEPT.10yr))]
AREA.Inf.X <- scale(AREA[which(!is.na(ACCEPT.Inf))])
AREA.10yr.X <- scale(AREA[which(!is.na(ACCEPT.10yr))])
AREA.Inf.Y <- scale(AREA[which(ACCEPT.Inf == 1)])
AREA.10yr.Y <- scale(AREA[which(ACCEPT.10yr == 1)])

# compile covariates for perpetual covenant model
# intercept - x
X.Inf <- as.matrix(rep(1,length(ACCEPT.Inf.New)))
# property size - x
X.Inf <- cbind(X.Inf,as.matrix(AREA.Inf.X))
# intercept - y
Y.Inf <- as.matrix(rep(1,length(WTA.Inf.New)))
# property size - y
Y.Inf <- cbind(Y.Inf,as.matrix(AREA.Inf.Y))

# compile covariates for 10 year covenant model
# intercept - x
X.10yr <- as.matrix(rep(1,length(ACCEPT.10yr.New)))
# property size - x
X.10yr <- cbind(X.10yr,as.matrix(AREA.10yr.X))
# intercept - y
Y.10yr <- as.matrix(rep(1,length(WTA.10yr.New)))
# property size - y
Y.10yr <- cbind(Y.10yr,as.matrix(AREA.10yr.Y))

# set up JAGS data for perpetual covenant
X <- X.Inf[,1:2] # change for covariates included in the model
Y <- Y.Inf[,1:2] # change for covariates included in the model
data1.Inf <- list(N = length(ACCEPT.Inf.New), M = length(WTA.Inf.New), ACCEPT = ACCEPT.Inf.New, WTA = WTA.Inf.New, CENS = CENS.Inf.New,
              LIM = c(0, 2.5), X = X, Nx = ncol(X), Y = Y, Ny = ncol(Y))

# set up JAGS data for perpetual covenant
X <- X.10yr[,1:2] # change for covariates included in the model
Y <- Y.10yr[,1:2] # change for covariates included in the model
data1.10yr <- list(N = length(ACCEPT.10yr.New), M = length(WTA.10yr.New), ACCEPT = ACCEPT.10yr.New, WTA = WTA.10yr.New, CENS = CENS.10yr.New,
              LIM = c(0, 2.5), X = X, Nx = ncol(X), Y = Y, Ny = ncol(Y))

# run JAGS models

# combine data
data <- list(data1.Inf, data1.10yr)

# load functions
source("./code/functions.r")

# parallel processing - comment out if not using parallel processing
# initialise
#sfInit( parallel = TRUE, cpus = 2)
# export data, functions and libraries to workers
#sfExportAll()
#sfClusterEval(library(runjags))
#sfClusterEval(library(coda))
#sfClusterEval(library(rjags))
#sfClusterEval(library(parallel))
#sfClusterEval(library(rjags))
#sfClusterEval(library(modeest))
# run JAGS model
#Jags.Fits <- sfLapply(data,get.jags)
# stop
#sfStop()

# non-parallel processing - comment out if using parallel processing
Jags.Fits <- get.jags(data[[1]])

# export models fits to the outpyt folder
saveRDS(Jags.Fits, file = "./output/Jags_Fits.rds")
