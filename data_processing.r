# load libraries
library(tidyverse)
library(sf)
library(raster)
library(rgdal)
library(runjags)
library(rjags)
library(coda)
library(snowfall)
library(parallel)
library(modeest)
library(sjlabelled)

# load functions
source("functions.r")

# survey response data
Pure <- read_csv("input/survey_responses/pure_resp.csv")
BCT <- read_csv("input/survey_responses/bct_resp.csv")
Mail <- read_csv("input/survey_responses/mail_resp.csv")

# load relevant postcodes
Poas <-read_csv("input/postcodes.csv") # load postcodes used to define the study areas for the survey data

# load geocoded lats and longs
Coords1 <- read_csv("input/geocodes/other/Geocoded_NSW_lh_survey.csv")
Coords2 <- do.call(rbind, lapply(list.files(path = "input/geocodes/mailout/", full.names = TRUE), read_csv))
Coords2 <- Coords2 %>% mutate(Address = paste(Ad1, ", ", Suburb, ", ", State, ", ", Postcode, sep = ""))

# compile willingness to accept data

# compile pure
Compiled_Pure <- Pure %>%
                filter(DistributionChannel == "anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
                         Finished == "TRUE",
                         Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
                         Q3.6 !="", Q3.6 !="No",              # [COLUMN: Q3.6] remove if not property manager etc
                         rid != 3657683231|7851483144|7393760845|6887891501|5410774572|1370281511) %>% # [COLUMN: rid] remove PureProfile tests: 3657683231, 7851483144, 7393760845, 6887891501, 5410774572, 1370281511
                         dplyr::select(ResponseId, RecordedDate, Q3.4, Q3.5, Q8.4, Q8.5, Q8.6, Q8.7) %>%
                         arrange(RecordedDate) %>% left_join(Coords1, by = c("ResponseId" = "ID")) %>% filter(!is.na(Lat)) %>%
                         mutate(AreaHa = Q3.4, AreaAc = Q3.5, WTAInf = trimws(as.character(Q8.4)), WTATen = trimws(as.character(Q8.6)),
                                          PercInf = ifelse(Q8.5 >= 0 & Q8.5 <= 100, Q8.5, NA), PercTen = ifelse(Q8.7 >= 0 & Q8.7 <= 100, Q8.7, NA)) %>%
                         dplyr::select(RecordedDate, Address, AreaHa, AreaAc, Lat, Long, WTAInf, WTATen, PercInf, PercTen)
write.csv(Compiled_Pure, file="output/compiled_survey_data/pure_compiled.csv", row.names = FALSE)

# compile bct
Compiled_BCT <- BCT %>%
                filter(DistributionChannel == "anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
                         Finished == "TRUE",
                         Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
                         Q3.6 !="", Q3.6 !="No") %>%              # [COLUMN: Q3.6] remove if not property manager etc
                         dplyr::select(ResponseId, RecordedDate, Q3.4, Q3.5, Q8.4, Q8.5, Q8.6, Q8.7) %>%
                         arrange(RecordedDate) %>% left_join(Coords1, by = c("ResponseId" = "ID")) %>% filter(!is.na(Lat)) %>%
                         mutate(AreaHa = Q3.4, AreaAc = Q3.5, WTAInf = trimws(as.character(Q8.4)), WTATen = trimws(as.character(Q8.6)),
                                          PercInf = ifelse(Q8.5 >= 0 & Q8.5 <= 100, Q8.5, NA), PercTen = ifelse(Q8.7 >= 0 & Q8.7 <= 100, Q8.7, NA)) %>%
                         dplyr::select(RecordedDate, Address, AreaHa, AreaAc, Lat, Long, WTAInf, WTATen, PercInf, PercTen)
write.csv(Compiled_BCT, file="output/compiled_survey_data/bct_compiled.csv", row.names = FALSE)

# compile mail
Compiled_Mail <- Mail %>%
                filter(DistributionChannel == "anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
                         Finished == "TRUE",
                         Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
                         Q3.6 !="", Q3.6 !="No") %>%              # [COLUMN: Q3.6] remove if not property manager etc
                         dplyr::select(Q83, RecordedDate, Q3.4, Q3.5, Q8.4, Q8.5, Q8.6, Q8.7) %>%
                         arrange(RecordedDate) %>% mutate(Q83 = toupper(Q83)) %>% left_join(Coords2, by = c("Q83" = "ExternalDataReference")) %>% filter(!is.na(Lat)) %>%
                         mutate(AreaHa = Q3.4, AreaAc = Q3.5, WTAInf = trimws(as.character(Q8.4)), WTATen = trimws(as.character(Q8.6)),
                                          PercInf = ifelse(Q8.5 >= 0 & Q8.5 <= 100, Q8.5, NA), PercTen = ifelse(Q8.7 >= 0 & Q8.7 <= 100, Q8.7, NA)) %>%
                         dplyr::select(RecordedDate, Address, AreaHa, AreaAc, Lat, Long, WTAInf, WTATen, PercInf, PercTen)
write.csv(Compiled_Mail, file="output/compiled_survey_data/mail_compiled.csv", row.names = FALSE)

# compile all
Compiled_All <- rbind(Compiled_Pure, Compiled_BCT, Compiled_Mail) %>% arrange(RecordedDate)
# remove duplicates based on address
Compiled_All <- Compiled_All[!duplicated(Compiled_All$Address), ]
write.csv(Compiled_All, file="output/compiled_survey_data/all_compiled.csv", row.names = FALSE)

# create shape file from lats and longs
Data_Spatial <- st_as_sf(Compiled_All, coords = c("Long", "Lat"))
Data_Spatial <- st_set_crs(Data_Spatial, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# import spatial predictor data
Pred_Spatial <- st_read("input/spatial/properties_mosaic_final_wgs84.shp")
Pred_Spatial_Table <- as_tibble(Pred_Spatial)

# import data on the target properties for making predictions (those that are private land and have koala habitat on them)
Target_Props_Spatial <- st_read("input/spatial/properties_mosaic_final_private_not_intense_hab_only_WGS84.shp")
Target_Props_Spatial_Table <- as_tibble(Target_Props_Spatial)

# intersect survey data and spatial predictors
Data_Int_Spatial <- st_intersection(Data_Spatial, Pred_Spatial)

# remove duplicates based on plan number and lot number
Data_Final <- Pred_Spatial_Table[!duplicated(Pred_Spatial_Table[,c("PLANLABEL", "LOTNUMBER")]), ]
# set land values that are 0 to NA
Data_Final[which(Data_Final[,"LValHa"] == 0), "LValHa"] <- NA
write.csv(Data_Final, file="output/compiled_survey_data/final_compiled_data.csv", row.names = FALSE)

# write spatial representation of data to shapefiles
st_write(Data_Spatial, "output/spatial/data_spatial_wgs84.shp", delete_layer = TRUE)
st_write(Data_Int_Spatial, "output/spatial/data_int_spatial_wgs84.shp", delete_layer = TRUE)

# extract data we needed to for the models
Data_Model_Fit_Inf <- Data_Final %>% dplyr::select(WTAInf, ha, LValHa, MosType, LU_Sec, Hab)
Data_Model_Fit_Ten <- Data_Final %>% dplyr::select(WTATen, ha, LValHa, MosType, LU_Sec, Hab)

# check for NA values in the continuous predictors and replace with averages
Data_Model_Fit_Inf[which(is.na(Data_Model_Fit_Inf$ha)), "ha"] <- mean(Data_Model_Fit_Inf$ha, na.rm = TRUE)
Data_Model_Fit_Ten[which(is.na(Data_Model_Fit_Ten$ha)), "ha"] <- mean(Data_Model_Fit_Ten$ha, na.rm = TRUE)
Data_Model_Fit_Inf[which(is.na(Data_Model_Fit_Inf$LValHa)), "LValHa"] <- mean(Data_Model_Fit_Inf$LValHa, na.rm = TRUE)
Data_Model_Fit_Ten[which(is.na(Data_Model_Fit_Ten$LValHa)), "LValHa"] <- mean(Data_Model_Fit_Ten$LValHa, na.rm = TRUE)
Data_Model_Fit_Inf[which(is.na(Data_Model_Fit_Inf$Hab)), "Hab"] <- mean(Data_Model_Fit_Inf$Hab, na.rm = TRUE)
Data_Model_Fit_Ten[which(is.na(Data_Model_Fit_Ten$Hab)), "Hab"] <- mean(Data_Model_Fit_Ten$Hab, na.rm = TRUE)

# whether the landholder would even consider adopting an agreement or not for a perpetual covenant
ACCEPT.Inf.df <- Data_Model_Fit_Inf %>%
                  mutate(ACCEPT = recode(WTAInf,
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
                  dplyr::select(ACCEPT)

ACCEPT.Inf <- as.numeric(unlist(ACCEPT.Inf.df))

# whether the landholder would even consider adopting an agreement or not for a 10 year covenant
ACCEPT.10yr.df <- Data_Model_Fit_Ten %>%
                   mutate(ACCEPT = recode(WTATen,
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
                   dplyr::select(ACCEPT)

ACCEPT.10yr <- as.numeric(unlist(ACCEPT.10yr.df))

# wta for perpetual covenant
WTA.Inf <- Data_Model_Fit_Inf %>%
            mutate(WTA = recode(WTAInf,
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
            dplyr::select(WTA)
WTA.Inf <- as.numeric(unlist(WTA.Inf)) / 1000 # to convert to 1000s of dollars

# wta for 10 year covenant
WTA.10yr <- Data_Model_Fit_Ten %>%
            mutate(WTA = recode(WTATen,
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
            dplyr::select(WTA)
WTA.10yr <- as.numeric(unlist(WTA.10yr)) / 1000 # to convert to 1000s of dollars

# censoring for perpetual covenant
CENS.Inf <- Data_Model_Fit_Inf %>%
              mutate(CENS = recode(WTAInf,
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
              dplyr::select(CENS)
CENS.Inf <- as.numeric(unlist(CENS.Inf))

# censoring for 10 year covenant
CENS.10yr <- Data_Model_Fit_Ten %>%
              mutate(CENS = recode(WTATen,
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
              dplyr::select(CENS)
CENS.10yr <- as.numeric(unlist(CENS.10yr))

# property size
AREA <- Data_Model_Fit_Inf %>%
                  dplyr::select(ha)
AREA <- as.numeric(unlist(AREA))

# land value
LVAL <- Data_Model_Fit_Inf %>%
                  dplyr::select(LValHa)
LVAL <- as.numeric(unlist(LVAL))

# get Mosaic type
MOS <- Data_Model_Fit_Inf %>%
                  dplyr::select(MosType) %>% droplevels()

# visualise the frequency of Mosaic types in the responses and target properties and save
fct_count(MOS$MosType)
write.csv(fct_count(MOS$MosType), file="output/compiled_survey_data/mosaic_freq_responses.csv", row.names = FALSE)
# compare to the Mosaic type frequencies in private properties with koala habitat in the study area (the target population for land holders)
fct_count(Target_Props_Spatial_Table$MosType)
write.csv(fct_count(Target_Props_Spatial_Table$MosType), file="output/compiled_survey_data/mosaic_freq_properties.csv", row.names = FALSE)

# group factors
MOS$MosType <- fct_other(MOS$MosType, keep = c("E16", "N48", "N49", "N50"), other_level = "OTHER")

# land use
LU <- Data_Model_Fit_Inf %>%
                  dplyr::select(LU_Sec) %>% droplevels()
# visualise the frequency of land-uses in the responses and target properties and save
fct_count(LU$LU_Sec)
write.csv(fct_count(LU$LU_Sec), file="output/compiled_survey_data/landuse_freq_responses.csv", row.names = FALSE)
# compare to the land-use frequencies in private properties with koala habitat in the study area (the target population for landholders)
fct_count(Target_Props_Spatial_Table$LU_Sec)
write.csv(fct_count(Target_Props_Spatial_Table$LU_Sec), file="output/compiled_survey_data/landuse_freq_properties.csv", row.names = FALSE)

# group factors
LU$LU_Sec <- fct_other(LU$LU_Sec, keep = c("1.3.0 Other minimal use", "2.1.0 Grazing native vegetation", "3.2.0 Grazing modified pastures", "5.4.0 Residential and farm infrastructure"),
                                            other_level = "OTHER")

# koala habitat
HAB <- Data_Model_Fit_Inf %>%
                  dplyr::select(Hab)
HAB <- as.numeric(unlist(HAB))

# select only required records for the JAGS model
# and scale predictors where necessary
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
LVAL.Inf.X <- scale(LVAL[which(!is.na(ACCEPT.Inf))])
LVAL.10yr.X <- scale(LVAL[which(!is.na(ACCEPT.10yr))])
LVAL.Inf.Y <- scale(LVAL[which(ACCEPT.Inf == 1)])
LVAL.10yr.Y <- scale(LVAL[which(ACCEPT.10yr == 1)])
MOS.Inf.X <- MOS[which(!is.na(ACCEPT.Inf)), 1]$MosType
MOS.10yr.X <- MOS[which(!is.na(ACCEPT.10yr)), 1]$MosType
MOS.Inf.Y <- MOS[which(ACCEPT.Inf == 1), 1]$MosType
MOS.10yr.Y <- MOS[which(ACCEPT.10yr == 1), 1]$MosType
LU.Inf.X <- LU[which(!is.na(ACCEPT.Inf)), 1]$LU_Sec
LU.10yr.X <- LU[which(!is.na(ACCEPT.10yr)), 1]$LU_Sec
LU.Inf.Y <- LU[which(ACCEPT.Inf == 1), 1]$LU_Sec
LU.10yr.Y <- LU[which(ACCEPT.10yr == 1), 1]$LU_Sec
HAB.Inf.X <- scale(HAB[which(!is.na(ACCEPT.Inf))])
HAB.10yr.X <- scale(HAB[which(!is.na(ACCEPT.10yr))])
HAB.Inf.Y <- scale(HAB[which(ACCEPT.Inf == 1)])
HAB.10yr.Y <- scale(HAB[which(ACCEPT.10yr == 1)])

# drop levels for factors and make numeric
MOS.Inf.X <- droplevels(MOS.Inf.X) %>% relevel(ref = "OTHER")
MOS.10yr.X <-droplevels(MOS.10yr.X) %>% relevel(ref = "OTHER")
MOS.Inf.Y <- droplevels(MOS.Inf.Y) %>% relevel(ref = "OTHER")
MOS.10yr.Y <-droplevels(MOS.10yr.Y) %>% relevel(ref = "OTHER")
LU.Inf.X <- droplevels(LU.Inf.X) %>% relevel(ref = "OTHER")
LU.10yr.X <-droplevels(LU.10yr.X) %>% relevel(ref = "OTHER")
LU.Inf.Y <- droplevels(LU.Inf.Y) %>% relevel(ref = "OTHER")
LU.10yr.Y <- droplevels(LU.10yr.Y) %>% relevel(ref = "OTHER")

# compile predictors for perpetual covenant model
# intercept - x
X.Inf <- as.matrix(rep(1, length(ACCEPT.Inf.New)))
# predictors - x
X1.Inf <- cbind(X.Inf, as.matrix(AREA.Inf.X), as.matrix(LVAL.Inf.X))
X2.Inf <- cbind(X.Inf, as.matrix(AREA.Inf.X), as.matrix(LVAL.Inf.X), model.matrix(~MOS.Inf.X)[,2:length(levels(MOS.Inf.X))])
X3.Inf <- cbind(X.Inf, as.matrix(AREA.Inf.X), as.matrix(LVAL.Inf.X), model.matrix(~LU.Inf.X)[,2:length(levels(LU.Inf.X))])
# intercept - y
Y.Inf <- as.matrix(rep(1, length(WTA.Inf.New)))
# predictors - y
Y1.Inf <- cbind(Y.Inf, as.matrix(AREA.Inf.Y), as.matrix(LVAL.Inf.Y))
Y2.Inf <- cbind(Y.Inf, as.matrix(AREA.Inf.Y), as.matrix(LVAL.Inf.Y), model.matrix(~MOS.Inf.Y)[,2:length(levels(MOS.Inf.Y))])
Y3.Inf <- cbind(Y.Inf, as.matrix(AREA.Inf.Y), as.matrix(LVAL.Inf.Y), model.matrix(~LU.Inf.Y)[,2:length(levels(LU.Inf.Y))])

# compile predictors for 10 year covenant model
# intercept - x
X.10yr <- as.matrix(rep(1, length(ACCEPT.10yr.New)))
# predictors - x
X1.10yr <- cbind(X.10yr, as.matrix(AREA.10yr.X), as.matrix(LVAL.10yr.X))
X2.10yr <- cbind(X.10yr, as.matrix(AREA.10yr.X), as.matrix(LVAL.10yr.X), model.matrix(~MOS.10yr.X)[,2:length(levels(MOS.10yr.X))])
X3.10yr <- cbind(X.10yr, as.matrix(AREA.10yr.X), as.matrix(LVAL.10yr.X), model.matrix(~LU.10yr.X)[,2:length(levels(LU.10yr.X))])

# intercept - y
Y.10yr <- as.matrix(rep(1, length(WTA.10yr.New)))
# predictors - y
Y1.10yr <- cbind(Y.10yr, as.matrix(AREA.10yr.Y), as.matrix(LVAL.10yr.Y))
Y2.10yr <- cbind(Y.10yr, as.matrix(AREA.10yr.Y), as.matrix(LVAL.10yr.Y), model.matrix(~MOS.10yr.Y)[,2:length(levels(MOS.10yr.Y))])
Y3.10yr <- cbind(Y.10yr, as.matrix(AREA.10yr.Y), as.matrix(LVAL.10yr.Y), model.matrix(~LU.10yr.Y)[,2:length(levels(LU.10yr.Y))])

# set up JAGS data for perpetual covenant
X1 <- X1.Inf[,1:ncol(X1.Inf)]
Y1 <- Y1.Inf[,1:ncol(Y1.Inf)]
X2 <- X2.Inf[,1:ncol(X2.Inf)]
Y2 <- Y2.Inf[,1:ncol(Y2.Inf)]
X3 <- X3.Inf[,1:ncol(X3.Inf)]
Y3 <- Y3.Inf[,1:ncol(Y3.Inf)]
data1.Inf <- list(N = length(ACCEPT.Inf.New), M = length(WTA.Inf.New), ACCEPT = ACCEPT.Inf.New, WTA = WTA.Inf.New, CENS = CENS.Inf.New,
              LIM = c(0, 2.5), X = X1, Nx = ncol(X1), Y = Y1, Ny = ncol(Y1))
data2.Inf <- list(N = length(ACCEPT.Inf.New), M = length(WTA.Inf.New), ACCEPT = ACCEPT.Inf.New, WTA = WTA.Inf.New, CENS = CENS.Inf.New,
              LIM = c(0, 2.5), X = X2, Nx = ncol(X2), Y = Y2, Ny = ncol(Y2))
data3.Inf <- list(N = length(ACCEPT.Inf.New), M = length(WTA.Inf.New), ACCEPT = ACCEPT.Inf.New, WTA = WTA.Inf.New, CENS = CENS.Inf.New,
              LIM = c(0, 2.5), X = X3, Nx = ncol(X3), Y = Y3, Ny = ncol(Y3))

# set up JAGS data for 10 year covenant
X1 <- X1.10yr[,1:ncol(X1.10yr)]
Y1 <- Y1.10yr[,1:ncol(Y1.10yr)]
X2 <- X2.10yr[,1:ncol(X2.10yr)]
Y2 <- Y2.10yr[,1:ncol(Y2.10yr)]
X3 <- X3.10yr[,1:ncol(X3.10yr)]
Y3 <- Y3.10yr[,1:ncol(Y3.10yr)]
data1.10yr <- list(N = length(ACCEPT.10yr.New), M = length(WTA.10yr.New), ACCEPT = ACCEPT.10yr.New, WTA = WTA.10yr.New, CENS = CENS.10yr.New,
              LIM = c(0, 2.5), X = X1, Nx = ncol(X1), Y = Y1, Ny = ncol(Y1))
data2.10yr <- list(N = length(ACCEPT.10yr.New), M = length(WTA.10yr.New), ACCEPT = ACCEPT.10yr.New, WTA = WTA.10yr.New, CENS = CENS.10yr.New,
              LIM = c(0, 2.5), X = X2, Nx = ncol(X2), Y = Y2, Ny = ncol(Y2))
data3.10yr <- list(N = length(ACCEPT.10yr.New), M = length(WTA.10yr.New), ACCEPT = ACCEPT.10yr.New, WTA = WTA.10yr.New, CENS = CENS.10yr.New,
              LIM = c(0, 2.5), X = X3, Nx = ncol(X3), Y = Y3, Ny = ncol(Y3))

# run JAGS models

# combine data
data <- list(data1.Inf, data2.Inf, data3.Inf, data1.10yr, data3.10yr, data3.10yr)

# load functions
source("functions.r")

# parallel processing - comment out if not using parallel processing
# initialise
sfInit( parallel = TRUE, cpus = 6)
# export data, functions and libraries to workers
sfExportAll()
sfClusterEval(library(runjags))
sfClusterEval(library(coda))
sfClusterEval(library(rjags))
sfClusterEval(library(parallel))
sfClusterEval(library(rjags))
sfClusterEval(library(modeest))
# run JAGS model
Jags.Fits <- sfLapply(data,get.jags)
# stop
sfStop()

# non-parallel processing - comment out if using parallel processing
#Jags.Fits <- get.jags(data[[1]])

# export models fits to the output folder
saveRDS(Jags.Fits, file = "output/jags/Jags_Fits.rds")

# get spatial predictions from selected models

# probability of adoption

# get coefficients
Coefs <- summary(Jags.Fits[[2]])[1:7, "Mean"]

# get property data for predictions
Predictions_Data <- Target_Props_Spatial_Table %>% dplyr::select(CADID, ha, LValHa, MosType)
Predictions_Data$MosType <- fct_other(Predictions_Data$MosType, keep = c("E16", "N48", "N49", "N50"), other_level = "OTHER") %>% relevel(ref = "OTHER")
Predictions_Data <- Predictions_Data %>% mutate(ha = as.vector(scale(ha, center = attr(AREA.Inf.X,"scaled:center"), scale = attr(AREA.Inf.X,"scaled:scale"))),
                                            LValHa = as.vector(scale(ha, center = attr(LVAL.Inf.X,"scaled:center"), scale = attr(LVAL.Inf.X,"scaled:scale"))))

# generate predictions
Model_Matrix <- model.matrix(~ ha + LValHa + MosType, data = Predictions_Data)
Predictions <- Model_Matrix %*% Coefs
Predictions <- exp(Predictions) / (1 + exp(Predictions))

# compile predictions and write to csv
Compiled_Predictions <- as_tibble(cbind(Predictions_Data, Property_Predictions))
write.csv(Compiled_Predictions, file="output/predictions/adoption_predictions.csv", row.names = FALSE)
