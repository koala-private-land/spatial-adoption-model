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
library(factoextra)
library(HDInterval)
library(PerformanceAnalytics)
library(tidybayes)

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
                                          PropInf = ifelse(Q8.5 >= 0 & Q8.5 <= 100, Q8.5 / 100, NA), PropTen = ifelse(Q8.7 >= 0 & Q8.7 <= 100, Q8.7 / 100, NA)) %>%
                         dplyr::select(RecordedDate, Address, AreaHa, AreaAc, Lat, Long, WTAInf, WTATen, PropInf, PropTen)
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
                                          PropInf = ifelse(Q8.5 >= 0 & Q8.5 <= 100, Q8.5 / 100, NA), PropTen = ifelse(Q8.7 >= 0 & Q8.7 <= 100, Q8.7 / 100, NA)) %>%
                         dplyr::select(RecordedDate, Address, AreaHa, AreaAc, Lat, Long, WTAInf, WTATen, PropInf, PropTen)
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
                                          PropInf = ifelse(Q8.5 >= 0 & Q8.5 <= 100, Q8.5 / 100, NA), PropTen = ifelse(Q8.7 >= 0 & Q8.7 <= 100, Q8.7 / 100, NA)) %>%
                         dplyr::select(RecordedDate, Address, AreaHa, AreaAc, Lat, Long, WTAInf, WTATen, PropInf, PropTen)
write.csv(Compiled_Mail, file="output/compiled_survey_data/mail_compiled.csv", row.names = FALSE)

# compile all
Compiled_All <- rbind(Compiled_Pure, Compiled_BCT, Compiled_Mail) %>% arrange(RecordedDate)
# remove duplicates based on address
Compiled_All <- Compiled_All[!duplicated(Compiled_All$Address), ]
write.csv(Compiled_All, file="output/compiled_survey_data/all_compiled.csv", row.names = FALSE)

# create shape file from lats and longs of survey locations
Data_Spatial <- st_as_sf(Compiled_All, coords = c("Long", "Lat"))
Data_Spatial <- st_set_crs(Data_Spatial, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# import property level spatial predictor data
Pred_Spatial <- st_read("input/spatial/properties_mosaic_final_hab_wgs84.shp")

# import data on the target properties for making predictions (those that are private land and not intensive land uses)
Target_Props_Spatial <- st_read("input/spatial/properties_mosaic_final_private_not_intense_only_hab_WGS84.shp")
Target_Props_Spatial_Table <- as_tibble(Target_Props_Spatial)

# intersect survey data and spatial predictors
Data_Int_Spatial <- st_intersection(Data_Spatial, Pred_Spatial)
Data_Int_Spatial_Table <- as_tibble(Data_Int_Spatial)

# write spatial representation of survey data to shapefiles
st_write(Data_Spatial, "output/spatial/data_spatial_wgs84.shp", delete_layer = TRUE)
st_write(Data_Int_Spatial, "output/spatial/data_int_spatial_wgs84.shp", delete_layer = TRUE)

# remove duplicates based on plan number and lot number
Data_Final <- Data_Int_Spatial_Table[!duplicated(Data_Int_Spatial_Table[,c("PLANLABEL", "LOTNUMBER")]), ]
# set land values that are 0 to NA
Data_Final[which(Data_Final[,"LValHa"] == 0), "LValHa"] <- NA
write.csv(Data_Final, file="output/compiled_survey_data/final_compiled_data.csv", row.names = FALSE)

# import and compile census data
# import census data at SA1 level
G01 <- read_csv("input/census/2016Census_G01_NSW_SA1.csv") %>% dplyr::select(SA1_7DIGITCODE_2016, Birthplace_Australia_P, Lang_spoken_home_Eng_only_P, High_yr_schl_comp_Yr_12_eq_P, Tot_P_P) %>%
          mutate(BirthAus = Birthplace_Australia_P / Tot_P_P, EngLang = Lang_spoken_home_Eng_only_P / Tot_P_P, Year12Ed = High_yr_schl_comp_Yr_12_eq_P / Tot_P_P)
G02 <- read_csv("input/census/2016Census_G02_NSW_SA1.csv") %>% dplyr::select(SA1_7DIGITCODE_2016, Median_age_persons, Average_household_size, Median_tot_hhd_inc_weekly, Median_mortgage_repay_monthly) %>%
          mutate(Age = Median_age_persons, HSize = Average_household_size, HInc = Median_tot_hhd_inc_weekly, MortPay = Median_mortgage_repay_monthly) %>% dplyr::select(-SA1_7DIGITCODE_2016)
G08 <- read_csv("input/census/2016Census_G08_NSW_SA1.csv") %>% dplyr::select(SA1_7DIGITCODE_2016, Tot_P_BP_B_OS, Tot_P_Tot_Resp, Tot_P_BP_NS) %>%
          mutate(PBornOS = Tot_P_BP_B_OS / (Tot_P_Tot_Resp - Tot_P_BP_NS)) %>% dplyr::select(-SA1_7DIGITCODE_2016)
G25 <- read_csv("input/census/2016Census_G25_NSW_SA1.csv") %>% dplyr::select(SA1_7DIGITCODE_2016, CF_ChU15_a_Total_P, CF_no_ChU15_a_Total_P, OPF_ChU15_a_Total_P, OPF_no_ChU15_a_Total_P, Total_P) %>%
          mutate(HComp1 = CF_ChU15_a_Total_P / Total_P, HComp2 = CF_no_ChU15_a_Total_P / Total_P, HComp3 = OPF_ChU15_a_Total_P / Total_P, HComp4 = OPF_no_ChU15_a_Total_P / Total_P) %>%
          dplyr::select(-SA1_7DIGITCODE_2016)
G46 <- bind_cols(read_csv("input/census/2016Census_G46A_NSW_SA1.csv"), read_csv("input/census/2016Census_G46B_NSW_SA1.csv") %>% dplyr::select(-SA1_7DIGITCODE_2016)) %>%
          dplyr::select(SA1_7DIGITCODE_2016, P_BachDeg_Total, P_Tot_Total, P_Lev_Edu_NS_Total, P_Lev_Edu_IDes_Total) %>%
          mutate(BachEd = P_BachDeg_Total / (P_Tot_Total - P_Lev_Edu_NS_Total - P_Lev_Edu_IDes_Total)) %>% dplyr::select(-SA1_7DIGITCODE_2016)
G53 <- bind_cols(read_csv("input/census/2016Census_G53A_NSW_SA1.csv"), read_csv("input/census/2016Census_G53B_NSW_SA1.csv") %>% dplyr::select(-SA1_7DIGITCODE_2016)) %>%
          dplyr::select(SA1_7DIGITCODE_2016, Agri_for_fish_Tot, Tot_Tot, ID_NS_Tot) %>%
          mutate(AgEmploy = Agri_for_fish_Tot / (Tot_Tot - ID_NS_Tot)) %>% dplyr::select(-SA1_7DIGITCODE_2016)
# combine census data
Census <- bind_cols(G01, G02, G08, G25, G46, G53) %>% dplyr::select(SA1_7DIGITCODE_2016, Age, Year12Ed, BachEd, BirthAus, EngLang, PBornOS, HSize, HComp1, HComp2, HComp3, HComp4, HInc, MortPay, AgEmploy)

# summarise by the SA1 7 digit code, generate sequential IDs for SA1s, and join SA1 level census predictors
SA1_IDLookUp <- Data_Final %>% group_by(SA1_7DIG16) %>% summarise(ObsCount = n()) %>% mutate(SA1_7DIG16 = as.numeric(SA1_7DIG16)) %>% arrange(SA1_7DIG16) %>% mutate(SA1ID = 1:n())
SA1_Var <- SA1_IDLookUp %>% left_join(Census, by = c("SA1_7DIG16" = "SA1_7DIGITCODE_2016"))

# undertake dimension reduction for the census data predictors using PCA
SA1_PCA <- prcomp(~Age + Year12Ed + BachEd + BirthAus + EngLang + PBornOS + HSize + HComp1 + HComp2 + HComp3 + HComp4 + HInc + MortPay + AgEmploy, data = SA1_Var, scale = TRUE)

# inspect scree plots
fviz_eig(SA1_PCA)
fviz_eig(SA1_PCA, choice = "eigenvalue")

# inspect bi-plot of variables
fviz_pca_var(SA1_PCA, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_var(SA1_PCA, axes = c(3, 4), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_var(SA1_PCA, axes = c(5, 6), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

# create component values for each individual sample and choose number of components to include
# MODIFY HERE BASED ON INSPECTION OF SCREE PLOT / VARIANCE EXPLAINED TO CHOOSE THE NUMBER OF COMPONENTS TO INCLUDE
SA1_PCA_Ind <- get_pca_ind(SA1_PCA)$coord[, 1:5] %>% as_tibble()

# join to SA1 variables
SA1_Var <- bind_cols(SA1_Var, SA1_PCA_Ind)

# join IDs for SA1s to survey data
Data_Final <- Data_Final %>% mutate(SA1_7DIG16 = as.numeric(SA1_7DIG16)) %>% left_join(SA1_IDLookUp, by = c("SA1_7DIG16")) %>% dplyr::select(-SA1_MAIN16, -ObsCount)

# extract data we need for the models at the individual property level
Data_Model_Fit <- Data_Final %>% dplyr::select(WTAInf, PropInf, WTATen, PropTen, ha, LValHa, MosType, LU_Sec, Hab, Dist_MUrb, Dist_OUrb, SA1ID)

# compile data at the individual property level

# whether the landholder would even consider adopting an agreement or not for a perpetual covenant
ACCEPT.Inf.df <- Data_Model_Fit %>%
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
ACCEPT.10yr.df <- Data_Model_Fit %>%
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
WTA.Inf <- Data_Model_Fit %>%
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
WTA.10yr <- Data_Model_Fit %>%
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
CENS.Inf <- Data_Model_Fit %>%
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
CENS.10yr <- Data_Model_Fit %>%
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

# proportion of property for perpetual covenant
PROP.Inf <- Data_Model_Fit %>% dplyr::select(PropInf)
PROP.Inf <- as.numeric(unlist(PROP.Inf))

# proportion of property for 10 year covenant
PROP.10yr <- Data_Model_Fit %>% dplyr::select(PropTen)
PROP.10yr <- as.numeric(unlist(PROP.10yr))

# property size
AREA <- Data_Model_Fit %>%
                  dplyr::select(ha)
AREA <- as.numeric(unlist(AREA))

# land value
LVAL <- Data_Model_Fit %>%
                  dplyr::select(LValHa)
LVAL <- as.numeric(unlist(LVAL))

# get Mosaic type
MOS <- Data_Model_Fit %>%
                  dplyr::select(MosType) %>% droplevels()

# visualise the frequency of Mosaic types in the responses and target properties and save
fct_count(MOS$MosType)
write.csv(fct_count(MOS$MosType), file="output/compiled_survey_data/mosaic_freq_responses.csv", row.names = FALSE)
# compare to the Mosaic type frequencies in private properties in the study area (the target population for land holders)
fct_count(Target_Props_Spatial_Table$MosType)
write.csv(fct_count(Target_Props_Spatial_Table$MosType), file="output/compiled_survey_data/mosaic_freq_properties.csv", row.names = FALSE)

# group factors
# CHANGE HERE TO GROUP DIFFERENTLY
MOS$MosType <- fct_other(MOS$MosType, keep = c("E16", "N48", "N49", "N50"), other_level = "OTHER")

# land use
LU <- Data_Model_Fit %>%
                  dplyr::select(LU_Sec) %>% droplevels()
# visualise the frequency of land-uses in the responses and target properties and save
fct_count(LU$LU_Sec)
write.csv(fct_count(LU$LU_Sec), file="output/compiled_survey_data/landuse_freq_responses.csv", row.names = FALSE)
# compare to the land-use frequencies in private properties in the study area (the target population for landholders)
fct_count(Target_Props_Spatial_Table$LU_Sec)
write.csv(fct_count(Target_Props_Spatial_Table$LU_Sec), file="output/compiled_survey_data/landuse_freq_properties.csv", row.names = FALSE)

# group factors
# CHANGE HERE TO GROUP DIFFERENTLY
LU$LU_Sec <- fct_other(LU$LU_Sec, keep = c("1.3.0 Other minimal use", "2.1.0 Grazing native vegetation", "3.2.0 Grazing modified pastures", "5.4.0 Residential and farm infrastructure"),
                                            other_level = "OTHER")

# koala habitat
HAB <- Data_Model_Fit %>%
                  dplyr::select(Hab)
HAB <- as.numeric(unlist(HAB))

# distance to major urban centres
DMURB <- Data_Model_Fit %>%
                  dplyr::select(Dist_MUrb)
DMURB <- as.numeric(unlist(DMURB))

# distance to other urban centres
DOURB <- Data_Model_Fit %>%
                  dplyr::select(Dist_OUrb)
DOURB <- as.numeric(unlist(DOURB))

# SA1 ID
SA1ID <- Data_Model_Fit %>%
                  dplyr::select(SA1ID)
SA1ID <- as.numeric(unlist(SA1ID))

# compile data at the SA1 level

# extract data we need for the models at the SA1 level
Data_Model_Fit_SA1 <- SA1_Var %>% dplyr::select(Dim.1, Dim.2, Dim.3, Dim.4, Dim.5)

# DIM 1
DIM1 <- Data_Model_Fit_SA1 %>%
                  dplyr::select(Dim.1)
DIM1 <- as.numeric(unlist(DIM1))

# DIM 2
DIM2 <- Data_Model_Fit_SA1 %>%
                  dplyr::select(Dim.2)
DIM2 <- as.numeric(unlist(DIM2))

# DIM 3
DIM3 <- Data_Model_Fit_SA1 %>%
                  dplyr::select(Dim.3)
DIM3 <- as.numeric(unlist(DIM3))

# DIM 4
DIM4 <- Data_Model_Fit_SA1 %>%
                  dplyr::select(Dim.4)
DIM4 <- as.numeric(unlist(DIM4))

# DIM 5
DIM5 <- Data_Model_Fit_SA1 %>%
                  dplyr::select(Dim.5)
DIM5 <- as.numeric(unlist(DIM5))

# select only required records for the JAGS model
# and scale predictors where necessary

# landholder would consider stewardship agreement model (X)
ACCEPT.Inf.X <- ACCEPT.Inf[which(!is.na(ACCEPT.Inf))]
ACCEPT.10yr.X <- ACCEPT.10yr[which(!is.na(ACCEPT.10yr))]
AREA.Inf.X <- scale(log(AREA[which(!is.na(ACCEPT.Inf))])) # log transformed to make distribution more symmetrical
AREA.10yr.X <- scale(log(AREA[which(!is.na(ACCEPT.10yr))])) # log transformed to make distribution more symmetrical
LVAL.Inf.X <- scale(log(LVAL[which(!is.na(ACCEPT.Inf))])) # log transformed to make distribution more symmetrical
LVAL.10yr.X <- scale(log(LVAL[which(!is.na(ACCEPT.10yr))])) # log transformed to make distribution more symmetrical
MOS.Inf.X <- MOS[which(!is.na(ACCEPT.Inf)), 1]$MosType
MOS.10yr.X <- MOS[which(!is.na(ACCEPT.10yr)), 1]$MosType
LU.Inf.X <- LU[which(!is.na(ACCEPT.Inf)), 1]$LU_Sec
LU.10yr.X <- LU[which(!is.na(ACCEPT.10yr)), 1]$LU_Sec
HAB.Inf.X <- scale(HAB[which(!is.na(ACCEPT.Inf))])
HAB.10yr.X <- scale(HAB[which(!is.na(ACCEPT.10yr))])
DMU.Inf.X <- scale(DMURB[which(!is.na(ACCEPT.Inf))])
DMU.10yr.X <- scale(DMURB[which(!is.na(ACCEPT.10yr))])
DOU.Inf.X <- scale(DOURB[which(!is.na(ACCEPT.Inf))])
DOU.10yr.X <- scale(DOURB[which(!is.na(ACCEPT.10yr))])
SA1ID.Inf.X <- (as_tibble(SA1ID[which(!is.na(ACCEPT.Inf))]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(SA1ID[which(!is.na(ACCEPT.Inf))]), NEWID = 1:length(unique(SA1ID[which(!is.na(ACCEPT.Inf))])))),
                    by = c("value" = "OLDID")))$NEWID
SA1ID.10yr.X <- (as_tibble(SA1ID[which(!is.na(ACCEPT.10yr))]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(SA1ID[which(!is.na(ACCEPT.10yr))]), NEWID = 1:length(unique(SA1ID[which(!is.na(ACCEPT.10yr))])))),
                    by = c("value" = "OLDID")))$NEWID
DIM1.Inf.X <- DIM1[unique(SA1ID[which(!is.na(ACCEPT.Inf))])]
DIM1.10yr.X <- DIM1[unique(SA1ID[which(!is.na(ACCEPT.10yr))])]
DIM2.Inf.X <- DIM2[unique(SA1ID[which(!is.na(ACCEPT.Inf))])]
DIM2.10yr.X <- DIM2[unique(SA1ID[which(!is.na(ACCEPT.10yr))])]
DIM3.Inf.X <- DIM3[unique(SA1ID[which(!is.na(ACCEPT.Inf))])]
DIM3.10yr.X <- DIM3[unique(SA1ID[which(!is.na(ACCEPT.10yr))])]
DIM4.Inf.X <- DIM4[unique(SA1ID[which(!is.na(ACCEPT.Inf))])]
DIM4.10yr.X <- DIM4[unique(SA1ID[which(!is.na(ACCEPT.10yr))])]
DIM5.Inf.X <- DIM5[unique(SA1ID[which(!is.na(ACCEPT.Inf))])]
DIM5.10yr.X <- DIM5[unique(SA1ID[which(!is.na(ACCEPT.10yr))])]

# willingness to accept model (Y)
CENS.Inf.Y <- CENS.Inf[which(ACCEPT.Inf == 1)]
CENS.10yr.Y <- CENS.10yr[which(ACCEPT.10yr == 1)]
WTA.Inf.Y <- WTA.Inf[which(ACCEPT.Inf == 1)]
WTA.10yr.Y <- WTA.10yr[which(ACCEPT.10yr == 1)]
AREA.Inf.Y <- scale(log(AREA[which(ACCEPT.Inf == 1)])) # log transformed to make distribution more symmetrical
AREA.10yr.Y <- scale(log(AREA[which(ACCEPT.10yr == 1)])) # log transformed to make distribution more symmetrical
LVAL.Inf.Y <- scale(log(LVAL[which(ACCEPT.Inf == 1)])) # log transformed to make distribution more symmetrical
LVAL.10yr.Y <- scale(log(LVAL[which(ACCEPT.10yr == 1)])) # log transformed to make distribution more symmetrical
MOS.Inf.Y <- MOS[which(ACCEPT.Inf == 1), 1]$MosType
MOS.10yr.Y <- MOS[which(ACCEPT.10yr == 1), 1]$MosType
LU.Inf.Y <- LU[which(ACCEPT.Inf == 1), 1]$LU_Sec
LU.10yr.Y <- LU[which(ACCEPT.10yr == 1), 1]$LU_Sec
HAB.Inf.Y <- scale(HAB[which(ACCEPT.Inf == 1)])
HAB.10yr.Y <- scale(HAB[which(ACCEPT.10yr == 1)])
DMU.Inf.Y <- scale(DMURB[which(ACCEPT.Inf == 1)])
DMU.10yr.Y <- scale(DMURB[which(ACCEPT.10yr == 1)])
DOU.Inf.Y <- scale(DOURB[which(ACCEPT.Inf == 1)])
DOU.10yr.Y <- scale(DOURB[which(ACCEPT.10yr == 1)])
SA1ID.Inf.Y <- (as_tibble(SA1ID[which(ACCEPT.Inf == 1)]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(SA1ID[which(ACCEPT.Inf == 1)]), NEWID = 1:length(unique(SA1ID[which(ACCEPT.Inf == 1)])))),
                    by = c("value" = "OLDID")))$NEWID
SA1ID.10yr.Y <- (as_tibble(SA1ID[which(ACCEPT.10yr == 1)]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(SA1ID[which(ACCEPT.10yr == 1)]), NEWID = 1:length(unique(SA1ID[which(ACCEPT.10yr == 1)])))),
                    by = c("value" = "OLDID")))$NEWID
DIM1.Inf.Y <- DIM1[unique(SA1ID[which(ACCEPT.Inf == 1)])]
DIM1.10yr.Y <- DIM1[unique(SA1ID[which(ACCEPT.10yr == 1)])]
DIM2.Inf.Y <- DIM2[unique(SA1ID[which(ACCEPT.Inf == 1)])]
DIM2.10yr.Y <- DIM2[unique(SA1ID[which(ACCEPT.10yr == 1)])]
DIM3.Inf.Y <- DIM3[unique(SA1ID[which(ACCEPT.Inf == 1)])]
DIM3.10yr.Y <- DIM3[unique(SA1ID[which(ACCEPT.10yr == 1)])]
DIM4.Inf.Y <- DIM4[unique(SA1ID[which(ACCEPT.Inf == 1)])]
DIM4.10yr.Y <- DIM4[unique(SA1ID[which(ACCEPT.10yr == 1)])]
DIM5.Inf.Y <- DIM5[unique(SA1ID[which(ACCEPT.Inf == 1)])]
DIM5.10yr.Y <- DIM5[unique(SA1ID[which(ACCEPT.10yr == 1)])]

# proportion of property under stewardship model (Z)

# transform propportions using (y * (n−1) + 0.5) / n where n is the sample size.
# see Smithson M, Verkuilen J (2006). "A Better Lemon Squeezer? Maximum-Likelihood Regression with Beta-Distributed Dependent Variables." Psychological Methods, 11 (1), 54–71.
PROP.Inf.Z <- (PROP.Inf[which(ACCEPT.Inf == 1)] * (length(PROP.Inf[which(ACCEPT.Inf == 1)]) - 1) + 0.5) / length(PROP.Inf[which(ACCEPT.Inf == 1)])
PROP.10yr.Z <- (PROP.10yr[which(ACCEPT.10yr == 1)] * (length(PROP.10yr[which(ACCEPT.10yr == 1)]) - 1) + 0.5) / length(PROP.10yr[which(ACCEPT.10yr == 1)])
AREA.Inf.Z <- scale(log(AREA[which(ACCEPT.Inf == 1)])) # log transformed to make distribution more symmetrical
AREA.10yr.Z <- scale(log(AREA[which(ACCEPT.10yr == 1)])) # log transformed to make distribution more symmetrical
LVAL.Inf.Z <- scale(log(LVAL[which(ACCEPT.Inf == 1)])) # log transformed to make distribution more symmetrical
LVAL.10yr.Z <- scale(log(LVAL[which(ACCEPT.10yr == 1)])) # log transformed to make distribution more symmetrical
MOS.Inf.Z <- MOS[which(ACCEPT.Inf == 1), 1]$MosType
MOS.10yr.Z <- MOS[which(ACCEPT.10yr == 1), 1]$MosType
LU.Inf.Z <- LU[which(ACCEPT.Inf == 1), 1]$LU_Sec
LU.10yr.Z <- LU[which(ACCEPT.10yr == 1), 1]$LU_Sec
HAB.Inf.Z <- scale(HAB[which(ACCEPT.Inf == 1)])
HAB.10yr.Z <- scale(HAB[which(ACCEPT.10yr == 1)])
DMU.Inf.Z <- scale(DMURB[which(ACCEPT.Inf == 1)])
DMU.10yr.Z <- scale(DMURB[which(ACCEPT.10yr == 1)])
DOU.Inf.Z <- scale(DOURB[which(ACCEPT.Inf == 1)])
DOU.10yr.Z <- scale(DOURB[which(ACCEPT.10yr == 1)])
SA1ID.Inf.Z <- (as_tibble(SA1ID[which(ACCEPT.Inf == 1)]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(SA1ID[which(ACCEPT.Inf == 1)]), NEWID = 1:length(unique(SA1ID[which(ACCEPT.Inf == 1)])))),
                    by = c("value" = "OLDID")))$NEWID
SA1ID.10yr.Z <- (as_tibble(SA1ID[which(ACCEPT.10yr == 1)]) %>%
                    left_join(as_tibble(cbind(OLDID = unique(SA1ID[which(ACCEPT.10yr == 1)]), NEWID = 1:length(unique(SA1ID[which(ACCEPT.10yr == 1)])))),
                    by = c("value" = "OLDID")))$NEWID
DIM1.Inf.Z <- DIM1[unique(SA1ID[which(ACCEPT.Inf == 1)])]
DIM1.10yr.Z <- DIM1[unique(SA1ID[which(ACCEPT.10yr == 1)])]
DIM2.Inf.Z <- DIM2[unique(SA1ID[which(ACCEPT.Inf == 1)])]
DIM2.10yr.Z <- DIM2[unique(SA1ID[which(ACCEPT.10yr == 1)])]
DIM3.Inf.Z <- DIM3[unique(SA1ID[which(ACCEPT.Inf == 1)])]
DIM3.10yr.Z <- DIM3[unique(SA1ID[which(ACCEPT.10yr == 1)])]
DIM4.Inf.Z <- DIM4[unique(SA1ID[which(ACCEPT.Inf == 1)])]
DIM4.10yr.Z <- DIM4[unique(SA1ID[which(ACCEPT.10yr == 1)])]
DIM5.Inf.Z <- DIM5[unique(SA1ID[which(ACCEPT.Inf == 1)])]
DIM5.10yr.Z <- DIM5[unique(SA1ID[which(ACCEPT.10yr == 1)])]

# drop levels for factors
MOS.Inf.X <- droplevels(MOS.Inf.X) %>% relevel(ref = "OTHER")
MOS.10yr.X <-droplevels(MOS.10yr.X) %>% relevel(ref = "OTHER")
MOS.Inf.Y <- droplevels(MOS.Inf.Y) %>% relevel(ref = "OTHER")
MOS.10yr.Y <-droplevels(MOS.10yr.Y) %>% relevel(ref = "OTHER")
MOS.Inf.Z <- droplevels(MOS.Inf.Z) %>% relevel(ref = "OTHER")
MOS.10yr.Z <-droplevels(MOS.10yr.Z) %>% relevel(ref = "OTHER")
LU.Inf.X <- droplevels(LU.Inf.X) %>% relevel(ref = "OTHER")
LU.10yr.X <-droplevels(LU.10yr.X) %>% relevel(ref = "OTHER")
LU.Inf.Y <- droplevels(LU.Inf.Y) %>% relevel(ref = "OTHER")
LU.10yr.Y <- droplevels(LU.10yr.Y) %>% relevel(ref = "OTHER")
LU.Inf.Z <- droplevels(LU.Inf.Z) %>% relevel(ref = "OTHER")
LU.10yr.Z <- droplevels(LU.10yr.Z) %>% relevel(ref = "OTHER")

# check for collinearity - have included all variables in subsequent models, so this is to understand potential collinearity issues
# continuous variables
Collinear_Cont <- cor(cbind(as.matrix(LVAL.Inf.X), as.matrix(AREA.Inf.X), as.matrix(HAB.Inf.X), as.matrix(DMU.Inf.X), as.matrix(DOU.Inf.X)), use = "complete.obs")
chart.Correlation(cbind(as.matrix(LVAL.Inf.X), as.matrix(AREA.Inf.X), as.matrix(HAB.Inf.X), as.matrix(DMU.Inf.X), as.matrix(DOU.Inf.X)), histogram = TRUE, pch = 19)
# categorical variables
Cases <- cbind(as.matrix(MOS.Inf.X), as.matrix(LU.Inf.X))
dimnames(Cases) = list(NULL, c("MOS", "LU"))
Cases <- as_tibble(Cases)
CTable <- table(Cases$MOS, Cases$LU)
ChiTest <- chisq.test(CTable)

# compile predictors for perpetual covenant model

# predictors - x
XSA1.Inf <- cbind(as.matrix(rep(1, length(DIM1.Inf.X))), as.matrix(DIM1.Inf.X), as.matrix(DIM2.Inf.X), as.matrix(DIM3.Inf.X), as.matrix(DIM4.Inf.X), as.matrix(DIM5.Inf.X))
# put categorical variables for the individual level at the start
XInd.Inf <- cbind(model.matrix(~MOS.Inf.X)[,2:length(levels(MOS.Inf.X))], model.matrix(~LU.Inf.X)[,2:length(levels(LU.Inf.X))], as.matrix(LVAL.Inf.X), as.matrix(AREA.Inf.X),
            as.matrix(HAB.Inf.X), as.matrix(DMU.Inf.X), as.matrix(DOU.Inf.X))

# predictors - y
YSA1.Inf <- cbind(as.matrix(rep(1, length(DIM1.Inf.Y))), as.matrix(DIM1.Inf.Y), as.matrix(DIM2.Inf.Y), as.matrix(DIM3.Inf.Y), as.matrix(DIM4.Inf.Y), as.matrix(DIM5.Inf.Y))
# put categorical variables for the individual level at the start
YInd.Inf <- cbind(model.matrix(~MOS.Inf.Y)[,2:length(levels(MOS.Inf.Y))], model.matrix(~LU.Inf.Y)[,2:length(levels(LU.Inf.Y))], as.matrix(LVAL.Inf.Y), as.matrix(AREA.Inf.Y),
            as.matrix(HAB.Inf.Y), as.matrix(DMU.Inf.Y), as.matrix(DOU.Inf.Y))

# predictors - z
ZSA1.Inf <- cbind(as.matrix(rep(1, length(DIM1.Inf.Z))), as.matrix(DIM1.Inf.Z), as.matrix(DIM2.Inf.Z), as.matrix(DIM3.Inf.Z), as.matrix(DIM4.Inf.Z), as.matrix(DIM5.Inf.Z))
# put categorical variables for the individual level at the start
ZInd.Inf <- cbind(model.matrix(~MOS.Inf.Z)[,2:length(levels(MOS.Inf.Z))], model.matrix(~LU.Inf.Z)[,2:length(levels(LU.Inf.Z))], as.matrix(LVAL.Inf.Z), as.matrix(AREA.Inf.Z),
            as.matrix(HAB.Inf.Z), as.matrix(DMU.Inf.Z), as.matrix(DOU.Inf.Z))

# set up JAGS data for perpetual covenant
data.Inf <- list(NR = length(DIM1.Inf.X), MR = length(DIM1.Inf.Y), OR = length(DIM1.Inf.Z), N = length(ACCEPT.Inf.X), M = length(WTA.Inf.Y), O = length(PROP.Inf.Z),
                ACCEPT = ACCEPT.Inf.X, WTA = WTA.Inf.Y, CENS = CENS.Inf.Y, PROP = PROP.Inf.Z, LIM = c(0, 2.5), XR = XSA1.Inf, X = XInd.Inf, NxR = ncol(XSA1.Inf), Nx = ncol(XInd.Inf),
                 YR = YSA1.Inf, Y = YInd.Inf, NyR = ncol(YSA1.Inf), Ny = ncol(YInd.Inf), ZR = ZSA1.Inf, Z = ZInd.Inf, NzR = ncol(ZSA1.Inf), Nz = ncol(ZInd.Inf),
                 RIDx = SA1ID.Inf.X, RIDy = SA1ID.Inf.Y, RIDz = SA1ID.Inf.Z)

 # compile predictors for 10 year covenant model

 # predictors - x
 XSA1.10yr <- cbind(as.matrix(rep(1, length(DIM1.10yr.X))), as.matrix(DIM1.10yr.X), as.matrix(DIM2.10yr.X), as.matrix(DIM3.10yr.X), as.matrix(DIM4.10yr.X), as.matrix(DIM5.10yr.X))
# put categorical variables for the individual level at the start
 XInd.10yr <- cbind(model.matrix(~MOS.10yr.X)[,2:length(levels(MOS.10yr.X))], model.matrix(~LU.10yr.X)[,2:length(levels(LU.10yr.X))], as.matrix(LVAL.10yr.X), as.matrix(AREA.10yr.X),
             as.matrix(HAB.10yr.X), as.matrix(DMU.10yr.X), as.matrix(DOU.10yr.X))

 # predictors - y
 YSA1.10yr <- cbind(as.matrix(rep(1, length(DIM1.10yr.Y))), as.matrix(DIM1.10yr.Y), as.matrix(DIM2.10yr.Y), as.matrix(DIM3.10yr.Y), as.matrix(DIM4.10yr.Y), as.matrix(DIM5.10yr.Y))
 # put categorical variables for the individual level at the start
 YInd.10yr <- cbind(model.matrix(~MOS.10yr.Y)[,2:length(levels(MOS.10yr.Y))], model.matrix(~LU.10yr.Y)[,2:length(levels(LU.10yr.Y))], as.matrix(LVAL.10yr.Y), as.matrix(AREA.10yr.Y),
             as.matrix(HAB.10yr.Y), as.matrix(DMU.10yr.Y), as.matrix(DOU.10yr.Y))

 # predictors - z
 ZSA1.10yr <- cbind(as.matrix(rep(1, length(DIM1.10yr.Z))), as.matrix(DIM1.10yr.Z), as.matrix(DIM2.10yr.Z), as.matrix(DIM3.10yr.Z), as.matrix(DIM4.10yr.Z), as.matrix(DIM5.10yr.Z))
 # put categorical variables for the individual level at the start
 ZInd.10yr <- cbind(model.matrix(~MOS.10yr.Z)[,2:length(levels(MOS.10yr.Z))], model.matrix(~LU.10yr.Z)[,2:length(levels(LU.10yr.Z))],as.matrix(LVAL.10yr.Z), as.matrix(AREA.10yr.Z),
             as.matrix(HAB.10yr.Z), as.matrix(DMU.10yr.Z), as.matrix(DOU.10yr.Z))

# set up JAGS data for perpetual covenant
data.10yr <- list(NR = length(DIM1.10yr.X), MR = length(DIM1.10yr.Y), OR = length(DIM1.10yr.Z), N = length(ACCEPT.10yr.X), M = length(WTA.10yr.Y), O = length(PROP.10yr.Z),
                 ACCEPT = ACCEPT.10yr.X, WTA = WTA.10yr.Y, CENS = CENS.10yr.Y, PROP = PROP.10yr.Z, LIM = c(0, 2.5), XR = XSA1.10yr, X = XInd.10yr, NxR = ncol(XSA1.10yr), Nx = ncol(XInd.10yr),
                  YR = YSA1.10yr, Y = YInd.10yr, NyR = ncol(YSA1.10yr), Ny = ncol(YInd.10yr), ZR = ZSA1.10yr, Z = ZInd.10yr, NzR = ncol(ZSA1.10yr), Nz = ncol(ZInd.10yr),
                  RIDx = SA1ID.10yr.X, RIDy = SA1ID.10yr.Y, RIDz = SA1ID.10yr.Z)

# FITTING MODELS WITHOUT VARIABLE SELECTION (SATURATED MODELS)

# run JAGS models

# combine data
data <- list(data.Inf, data.10yr)

# load functions
source("functions.r")

# parallel processing - comment out if not using parallel processing
# initialise
sfInit( parallel = TRUE, cpus = 2)
# export data, functions and libraries to workers
sfExportAll()
sfClusterEval(library(runjags))
sfClusterEval(library(coda))
sfClusterEval(library(rjags))
sfClusterEval(library(parallel))
sfClusterEval(library(rjags))
sfClusterEval(library(modeest))
# run JAGS model
Jags.Fits <- sfLapply(data, get.jags)
# stop
sfStop()

# non-parallel processing - comment out if using parallel processing
#Jags.Fits <- get.jags(data[[1]])

# export models fits to the output folder
saveRDS(Jags.Fits, file = "output/jags/Jags_Fits.rds")

# FITTING MODELS WITH VARIABLE SELECTION

# set up JAGS data for perpetual covenant
# define coefficient index where the categorical variables start (NEED TO EDIT HERE IF IDENPENDENT VARIABLES CHANGED)
# NOTE THAT THIS SECTION OF CODE ONLY WORKS IF THE CATEGORICAL VARIALBES ARE BEFORE THE CONTINUOUS VARIABLES
NxF <- length(levels(MOS.Inf.X)) + length(levels(LU.Inf.X)) - 2
NyF <- length(levels(MOS.Inf.Y)) + length(levels(LU.Inf.Y)) - 2
NzF <- length(levels(MOS.Inf.Z)) + length(levels(LU.Inf.Z)) - 2
data.Inf.Sel <- list(NR = length(DIM1.Inf.X), MR = length(DIM1.Inf.Y), OR = length(DIM1.Inf.Z), N = length(ACCEPT.Inf.X), M = length(WTA.Inf.Y), O = length(PROP.Inf.Z),
                ACCEPT = ACCEPT.Inf.X, WTA = WTA.Inf.Y, CENS = CENS.Inf.Y, PROP = PROP.Inf.Z, LIM = c(0, 2.5), XR = XSA1.Inf, X = XInd.Inf, NxR = ncol(XSA1.Inf), Nx = ncol(XInd.Inf),
                 YR = YSA1.Inf, Y = YInd.Inf, NyR = ncol(YSA1.Inf), Ny = ncol(YInd.Inf), ZR = ZSA1.Inf, Z = ZInd.Inf, NzR = ncol(ZSA1.Inf), Nz = ncol(ZInd.Inf),
                 RIDx = SA1ID.Inf.X, RIDy = SA1ID.Inf.Y, RIDz = SA1ID.Inf.Z, NxF = NxF, NyF = NyF, NzF = NzF,
                 FVARX = c(rep(1, length(levels(MOS.Inf.X)) - 1), rep(2, length(levels(LU.Inf.X)) - 1)),
                 FVARY = c(rep(1, length(levels(MOS.Inf.Y)) - 1), rep(2, length(levels(LU.Inf.Y)) - 1)),
                 FVARZ = c(rep(1, length(levels(MOS.Inf.Z)) - 1), rep(2, length(levels(LU.Inf.Z)) - 1)))

# set up JAGS data for 10 year covenant
# define coefficient index where the categorical variables start (NEED TO EDIT HERE IF IDENPENDENT VARIABLES CHANGED)
# NOTE THAT THIS SECTION OF CODE ONLY WORKS IF THE CATEGORICAL VARIALBES ARE BEFORE THE CONTINUOUS VARIABLES
NxF <- length(levels(MOS.10yr.X)) + length(levels(LU.10yr.X)) - 2
NyF <- length(levels(MOS.10yr.Y)) + length(levels(LU.10yr.Y)) - 2
NzF <- length(levels(MOS.10yr.Z)) + length(levels(LU.10yr.Z)) - 2
data.10yr.Sel <- list(NR = length(DIM1.10yr.X), MR = length(DIM1.10yr.Y), OR = length(DIM1.10yr.Z), N = length(ACCEPT.10yr.X), M = length(WTA.10yr.Y), O = length(PROP.10yr.Z),
               ACCEPT = ACCEPT.10yr.X, WTA = WTA.10yr.Y, CENS = CENS.10yr.Y, PROP = PROP.10yr.Z, LIM = c(0, 2.5), XR = XSA1.10yr, X = XInd.10yr, NxR = ncol(XSA1.10yr), Nx = ncol(XInd.10yr),
                YR = YSA1.10yr, Y = YInd.10yr, NyR = ncol(YSA1.10yr), Ny = ncol(YInd.10yr), ZR = ZSA1.10yr, Z = ZInd.10yr, NzR = ncol(ZSA1.10yr), Nz = ncol(ZInd.10yr),
                RIDx = SA1ID.10yr.X, RIDy = SA1ID.10yr.Y, RIDz = SA1ID.10yr.Z, NxF = NxF, NyF = NyF, NzF = NzF,
                FVARX = c(rep(1, length(levels(MOS.10yr.X)) - 1), rep(2, length(levels(LU.10yr.X)) - 1)),
                FVARY = c(rep(1, length(levels(MOS.10yr.Y)) - 1), rep(2, length(levels(LU.10yr.Y)) - 1)),
                FVARZ = c(rep(1, length(levels(MOS.10yr.Z)) - 1), rep(2, length(levels(LU.10yr.Z)) - 1)))

# run JAGS models

# combine data
data.Sel <- list(data.Inf.Sel, data.10yr.Sel)

# load functions
source("functions.r")

# parallel processing - comment out if not using parallel processing
# initialise
sfInit( parallel = TRUE, cpus = 2)
# export data, functions and libraries to workers
sfExportAll()
sfClusterEval(library(runjags))
sfClusterEval(library(coda))
sfClusterEval(library(rjags))
sfClusterEval(library(parallel))
sfClusterEval(library(rjags))
sfClusterEval(library(modeest))
# run JAGS model
Jags.Fits.Sel <- sfLapply(data.Sel, get.jags.sel)
# stop
sfStop()

# non-parallel processing - comment out if using parallel processing
#Jags.Fits.Sel <- get.jags.sel(data.Sel[[1]])

# export models fits to the output folder
saveRDS(Jags.Fits.Sel, file = "output/jags/Jags_Fits_Sel.rds")

# COEFFICIENT PLOTS

# load models
Jags.Fits <- readRDS("output/jags/Jags_Fits.rds")
Jags.Fits.Sel <- readRDS("output/jags/Jags_Fits_Sel.rds")

# MODELS WITHOUT VARIABLE SELECTION

# PERPETUAL COVENANT

# property level predictors

# adoption
# spread draws
DrawsInfAdopt <- spread_draws(Jags.Fits[[1]], beta_x[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfAdopt$Param <- DrawsInfAdopt$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfAdopt %>% ggplot(aes(y = Param, x = beta_x)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Probability Would Consider Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_adopt_ind_inf.jpg")

# wta
# spread draws
DrawsInfWTA <- spread_draws(Jags.Fits[[1]], beta_y[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfWTA$Param <- DrawsInfWTA$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfWTA %>% ggplot(aes(y = Param, x = beta_y)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Financial Payment Required") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_wta_ind_inf.jpg")

# proportion
# spread draws
DrawsInfProp <- spread_draws(Jags.Fits[[1]], beta_z[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfProp$Param <- DrawsInfProp$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfProp %>% ggplot(aes(y = Param, x = beta_z)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Proportion of Property Would Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_prop_ind_inf.jpg")

# SA1 level predictors

# adoption
# spread draws
DrawsInfAdopt <- spread_draws(Jags.Fits[[1]], betar_x[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfAdopt$Param <- DrawsInfAdopt$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfAdopt %>% ggplot(aes(y = Param, x = betar_x)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Probability Would Consider Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_adopt_sa1_inf.jpg")

# wta
# spread draws
DrawsInfWTA <- spread_draws(Jags.Fits[[1]], betar_y[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfWTA$Param <- DrawsInfWTA$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfWTA %>% ggplot(aes(y = Param, x = betar_y)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Financial Payment Required") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_wta_sa1_inf.jpg")

# proportion
# spread draws
DrawsInfProp <- spread_draws(Jags.Fits[[1]], betar_z[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfProp$Param <- DrawsInfProp$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfProp %>% ggplot(aes(y = Param, x = betar_z)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Proportion of Property Would Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_prop_sa1_inf.jpg")

# 10 YEAR COVENANT

# property level predictors

# adoption
# spread draws
DrawsInfAdopt <- spread_draws(Jags.Fits[[2]], beta_x[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfAdopt$Param <- DrawsInfAdopt$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfAdopt %>% ggplot(aes(y = Param, x = beta_x)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Probability Would Consider Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_adopt_ind_10yr.jpg")

# wta
# spread draws
DrawsInfWTA <- spread_draws(Jags.Fits[[2]], beta_y[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfWTA$Param <- DrawsInfWTA$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfWTA %>% ggplot(aes(y = Param, x = beta_y)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Financial Payment Required") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_wta_ind_10yr.jpg")

# proportion
# spread draws
DrawsInfProp <- spread_draws(Jags.Fits[[2]], beta_z[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfProp$Param <- DrawsInfProp$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfProp %>% ggplot(aes(y = Param, x = beta_z)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Proportion of Property Would Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_prop_ind_10yr.jpg")

# SA1 level predictors

# adoption
# spread draws
DrawsInfAdopt <- spread_draws(Jags.Fits[[2]], betar_x[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfAdopt$Param <- DrawsInfAdopt$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfAdopt %>% ggplot(aes(y = Param, x = betar_x)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Probability Would Consider Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_adopt_sa1_10yr.jpg")

# wta
# spread draws
DrawsInfWTA <- spread_draws(Jags.Fits[[2]], betar_y[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfWTA$Param <- DrawsInfWTA$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfWTA %>% ggplot(aes(y = Param, x = betar_y)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Financial Payment Required") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_wta_sa1_10yr.jpg")

# proportion
# spread draws
DrawsInfProp <- spread_draws(Jags.Fits[[2]], betar_z[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfProp$Param <- DrawsInfProp$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfProp %>% ggplot(aes(y = Param, x = betar_z)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Proportion of Property Would Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_prop_sa1_10yr.jpg")

# MODELS WITH VARIABLE SELECTION

# PERPETUAL COVENANT

# property level predictors

# adoption
# spread draws
DrawsInfAdopt <- spread_draws(Jags.Fits.Sel[[1]], beta_x[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfAdopt$Param <- DrawsInfAdopt$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfAdopt %>% ggplot(aes(y = Param, x = beta_x)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Probability Would Consider Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_adopt_ind_inf_sel.jpg")

# wta
# spread draws
DrawsInfWTA <- spread_draws(Jags.Fits.Sel[[1]], beta_y[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfWTA$Param <- DrawsInfWTA$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfWTA %>% ggplot(aes(y = Param, x = beta_y)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Financial Payment Required") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_wta_ind_inf_sel.jpg")

# proportion
# spread draws
DrawsInfProp <- spread_draws(Jags.Fits.Sel[[1]], beta_z[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfProp$Param <- DrawsInfProp$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfProp %>% ggplot(aes(y = Param, x = beta_z)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Proportion of Property Would Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_prop_ind_inf_sel.jpg")

# SA1 level predictors

# adoption
# spread draws
DrawsInfAdopt <- spread_draws(Jags.Fits.Sel[[1]], betar_x[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfAdopt$Param <- DrawsInfAdopt$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfAdopt %>% ggplot(aes(y = Param, x = betar_x)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Probability Would Consider Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_adopt_sa1_inf_sel.jpg")

# wta
# spread draws
DrawsInfWTA <- spread_draws(Jags.Fits.Sel[[1]], betar_y[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfWTA$Param <- DrawsInfWTA$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfWTA %>% ggplot(aes(y = Param, x = betar_y)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Financial Payment Required") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_wta_sa1_inf_sel.jpg")

# proportion
# spread draws
DrawsInfProp <- spread_draws(Jags.Fits.Sel[[1]], betar_z[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfProp$Param <- DrawsInfProp$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfProp %>% ggplot(aes(y = Param, x = betar_z)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Proportion of Property Would Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_prop_sa1_inf_sel.jpg")

# 10 YEAR COVENANT

# property level predictors

# adoption
# spread draws
DrawsInfAdopt <- spread_draws(Jags.Fits.Sel[[2]], beta_x[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfAdopt$Param <- DrawsInfAdopt$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfAdopt %>% ggplot(aes(y = Param, x = beta_x)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Probability Would Consider Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_adopt_ind_10yr_sel.jpg")

# wta
# spread draws
DrawsInfWTA <- spread_draws(Jags.Fits.Sel[[2]], beta_y[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfWTA$Param <- DrawsInfWTA$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfWTA %>% ggplot(aes(y = Param, x = beta_y)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Financial Payment Required") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_wta_ind_10yr_sel.jpg")

# proportion
# spread draws
DrawsInfProp <- spread_draws(Jags.Fits.Sel[[2]], beta_z[Param]) %>% mutate(Param = as.factor(Param))
# recode factor
DrawsInfProp$Param <- DrawsInfProp$Param %>% recode_factor("1" = "MOSAIC Hardware & Acreage", "2" = "MOSAIC Farming Reliance", "3" = "MOSAIC Outback Comfort", "4" = "MOSAIC Soil & Toil",
                          "5" = "LU Minimal Use", "6" = "LU Grazing Native", "7" = "LU Grazing Modified", "8" = "LU Residential", "9" = "Land Value",
                          "10" = "Property Size", "12" = "Distance Major Urban", "13" = "Distance Other Urban",  "11" = "Proportion Koala Habitat")
DrawsInfProp %>% ggplot(aes(y = Param, x = beta_z)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (Property Level)", x = "Coefficient Estimate",
                        title = "Proportion of Property Would Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_prop_ind_10yr_sel.jpg")

# SA1 level predictors

# adoption
# spread draws
DrawsInfAdopt <- spread_draws(Jags.Fits.Sel[[2]], betar_x[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfAdopt$Param <- DrawsInfAdopt$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfAdopt %>% ggplot(aes(y = Param, x = betar_x)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Probability Would Consider Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_adopt_sa1_10yr_sel.jpg")

# wta
# spread draws
DrawsInfWTA <- spread_draws(Jags.Fits.Sel[[2]], betar_y[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfWTA$Param <- DrawsInfWTA$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfWTA %>% ggplot(aes(y = Param, x = betar_y)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Financial Payment Required") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_wta_sa1_10yr_sel.jpg")

# proportion
# spread draws
DrawsInfProp <- spread_draws(Jags.Fits.Sel[[2]], betar_z[Param]) %>% mutate(Param = as.factor(Param)) %>% filter(Param != "1")
# recode factor
DrawsInfProp$Param <- DrawsInfProp$Param %>% recode_factor("2" = "PC1 (Income, Household Size)", "3" = "PC2 (English Language, Birth Australia)", "4" = "PC3 (Education)", "5" = "PC4 (Household Composition, English Language)", "6" = "PC5 (Age, Household Composition)")
DrawsInfProp %>% ggplot(aes(y = Param, x = betar_z)) + stat_halfeye(.width = c(0.95)) + geom_vline(xintercept = 0, linetype = "dashed") + labs(y = "Variable (SA1 Level)", x = "Coefficient Estimate",
                        title = "Proportion of Property Would Covenant") + theme(plot.title = element_text(hjust = 0.5))
ggsave("output/figures/coefs_prop_sa1_10yr_sel.jpg")


# SPATIAL PREDICTIONS - THIS NEEDS RE-WRITING TO GET EXPECTED VALUES AND UNCERTAINTIES

# load models
Jags.Fits <- readRDS("output/jags/Jags_Fits.rds")

# get property data for predictions
Census_Char <- Census %>% mutate(SA1_7DIGITCODE_2016 = as.character(SA1_7DIGITCODE_2016))
Target_Props_Spatial_Table_Join <- Target_Props_Spatial_Table %>% left_join(Census_Char, by = c("SA1_7DIG16" = "SA1_7DIGITCODE_2016"))
PredPCAVars <- Target_Props_Spatial_Table_Join %>% dplyr::select(Age, Year12Ed, BachEd, BirthAus, EngLang, PBornOS, HSize, HComp1, HComp2, HComp3, HComp4, HInc, MortPay, AgEmploy)
PredPCA <- as_tibble(predict(SA1_PCA, newdata = PredPCAVars))
Target_Props_Spatial_Table_Pred <- bind_cols(Target_Props_Spatial_Table, PredPCA)

Predictions_Data <- Target_Props_Spatial_Table_Pred %>% dplyr::select(CADID, LValHa, ha, MosType, LU_Sec, Hab, Dist_MUrb, Dist_OUrb, PC1, PC2, PC3, PC4, PC5)
Predictions_Data$MosType <- fct_other(Predictions_Data$MosType, keep = c("E16", "N48", "N49", "N50"), other_level = "OTHER") %>% relevel(ref = "OTHER")
Predictions_Data$LU_Sec <- fct_other(Predictions_Data$LU_Sec, keep = c("1.3.0 Other minimal use", "2.1.0 Grazing native vegetation", "3.2.0 Grazing modified pastures", "5.4.0 Residential and farm infrastructure"),
                                other_level = "OTHER") %>% relevel(ref = "OTHER")

# IN PERPETUITY MODEL

# get coefficients
CoefsX <- summary(Jags.Fits[[1]])[c(40, 1:13, 41:45), "Mean"]
CoefsY <- summary(Jags.Fits[[1]])[c(46, 14:26, 47:51), "Mean"]
CoefsZ <- summary(Jags.Fits[[1]])[c(52, 27:39, 53:57), "Mean"]

Predictions_DataX <- Predictions_Data %>% mutate(LValHa = as.vector(scale(log(LValHa), center = attr(LVAL.Inf.X,"scaled:center"), scale = attr(LVAL.Inf.X,"scaled:scale"))),
                                            ha = as.vector(scale(log(ha), center = attr(AREA.Inf.X,"scaled:center"), scale = attr(AREA.Inf.X,"scaled:scale"))),
                                            Hab = as.vector(scale(Hab, center = attr(HAB.Inf.X,"scaled:center"), scale = attr(HAB.Inf.X,"scaled:scale"))),
                                            Dist_MUrb = as.vector(scale(Dist_MUrb, center = attr(DMU.Inf.X,"scaled:center"), scale = attr(DMU.Inf.X,"scaled:scale"))),
                                            Dist_OUrb = as.vector(scale(Dist_OUrb, center = attr(DOU.Inf.X,"scaled:center"), scale = attr(DOU.Inf.X,"scaled:scale"))))

Predictions_DataY <- Predictions_Data %>% mutate(LValHa = as.vector(scale(log(LValHa), center = attr(LVAL.Inf.Y,"scaled:center"), scale = attr(LVAL.Inf.Y,"scaled:scale"))),
                                            ha = as.vector(scale(log(ha), center = attr(AREA.Inf.Y,"scaled:center"), scale = attr(AREA.Inf.Y,"scaled:scale"))),
                                            Hab = as.vector(scale(Hab, center = attr(HAB.Inf.Y,"scaled:center"), scale = attr(HAB.Inf.Y,"scaled:scale"))),
                                            Dist_MUrb = as.vector(scale(Dist_MUrb, center = attr(DMU.Inf.Y,"scaled:center"), scale = attr(DMU.Inf.Y,"scaled:scale"))),
                                            Dist_OUrb = as.vector(scale(Dist_OUrb, center = attr(DOU.Inf.Y,"scaled:center"), scale = attr(DOU.Inf.Y,"scaled:scale"))))

Predictions_DataZ <- Predictions_Data %>% mutate(LValHa = as.vector(scale(log(LValHa), center = attr(LVAL.Inf.Z,"scaled:center"), scale = attr(LVAL.Inf.Z,"scaled:scale"))),
                                            ha = as.vector(scale(log(ha), center = attr(AREA.Inf.Z,"scaled:center"), scale = attr(AREA.Inf.Z,"scaled:scale"))),
                                            Hab = as.vector(scale(Hab, center = attr(HAB.Inf.Z,"scaled:center"), scale = attr(HAB.Inf.Z,"scaled:scale"))),
                                            Dist_MUrb = as.vector(scale(Dist_MUrb, center = attr(DMU.Inf.Z,"scaled:center"), scale = attr(DMU.Inf.Z,"scaled:scale"))),
                                            Dist_OUrb = as.vector(scale(Dist_OUrb, center = attr(DOU.Inf.Z,"scaled:center"), scale = attr(DOU.Inf.Z,"scaled:scale"))))

# generate predictions
# adoption
options(na.action='na.pass')
Model_MatrixX <- model.matrix(~ LValHa + ha + MosType + LU_Sec + Hab + Dist_MUrb + Dist_OUrb + PC1 + PC2 + PC3 + PC4 + PC5, data = Predictions_DataX)
PredictionsX <- Model_MatrixX %*% CoefsX
PredictionsX <- exp(PredictionsX) / (1 + exp(PredictionsX))
# wta
options(na.action='na.pass')
Model_MatrixY <- model.matrix(~ LValHa + ha + MosType + LU_Sec + Hab + Dist_MUrb + Dist_OUrb + PC1 + PC2 + PC3 + PC4 + PC5, data = Predictions_DataY)
PredictionsY <- Model_MatrixX %*% CoefsY
# proportion
options(na.action='na.pass')
Model_MatrixZ <- model.matrix(~ LValHa + ha + MosType + LU_Sec + Hab + Dist_MUrb + Dist_OUrb + PC1 + PC2 + PC3 + PC4 + PC5, data = Predictions_DataZ)
PredictionsZ <- Model_MatrixZ %*% CoefsZ
PredictionsZ <- exp(PredictionsZ) / (1 + exp(PredictionsZ))

# compile predictions and write to csv
Compiled_Predictions_Inf <- as_tibble(cbind(Predictions_DataX, PredAdopt = PredictionsX, PredWTA = PredictionsY, PredProp = PredictionsZ)) %>% dplyr::select(CADID, PredAdopt, PredWTA, PredProp)
write.csv(Compiled_Predictions_Inf, file="output/predictions/spatial_predictions_inf.csv", row.names = FALSE)

# 10 YEAR MODEL

# get coefficients
CoefsX <- summary(Jags.Fits[[2]])[c(40, 1:13, 41:45), "Mean"]
CoefsY <- summary(Jags.Fits[[2]])[c(46, 14:26, 47:51), "Mean"]
CoefsZ <- summary(Jags.Fits[[2]])[c(52, 27:39, 53:57), "Mean"]

Predictions_DataX <- Predictions_Data %>% mutate(LValHa = as.vector(scale(log(LValHa), center = attr(LVAL.10yr.X,"scaled:center"), scale = attr(LVAL.10yr.X,"scaled:scale"))),
                                            ha = as.vector(scale(log(ha), center = attr(AREA.10yr.X,"scaled:center"), scale = attr(AREA.10yr.X,"scaled:scale"))),
                                            Hab = as.vector(scale(Hab, center = attr(HAB.10yr.X,"scaled:center"), scale = attr(HAB.10yr.X,"scaled:scale"))),
                                            Dist_MUrb = as.vector(scale(Dist_MUrb, center = attr(DMU.10yr.X,"scaled:center"), scale = attr(DMU.10yr.X,"scaled:scale"))),
                                            Dist_OUrb = as.vector(scale(Dist_OUrb, center = attr(DOU.10yr.X,"scaled:center"), scale = attr(DOU.10yr.X,"scaled:scale"))))

Predictions_DataY <- Predictions_Data %>% mutate(LValHa = as.vector(scale(log(LValHa), center = attr(LVAL.10yr.Y,"scaled:center"), scale = attr(LVAL.10yr.Y,"scaled:scale"))),
                                            ha = as.vector(scale(log(ha), center = attr(AREA.10yr.Y,"scaled:center"), scale = attr(AREA.10yr.Y,"scaled:scale"))),
                                            Hab = as.vector(scale(Hab, center = attr(HAB.10yr.Y,"scaled:center"), scale = attr(HAB.10yr.Y,"scaled:scale"))),
                                            Dist_MUrb = as.vector(scale(Dist_MUrb, center = attr(DMU.10yr.Y,"scaled:center"), scale = attr(DMU.10yr.Y,"scaled:scale"))),
                                            Dist_OUrb = as.vector(scale(Dist_OUrb, center = attr(DOU.10yr.Y,"scaled:center"), scale = attr(DOU.10yr.Y,"scaled:scale"))))

Predictions_DataZ <- Predictions_Data %>% mutate(LValHa = as.vector(scale(log(LValHa), center = attr(LVAL.10yr.Z,"scaled:center"), scale = attr(LVAL.10yr.Z,"scaled:scale"))),
                                            ha = as.vector(scale(log(ha), center = attr(AREA.10yr.Z,"scaled:center"), scale = attr(AREA.10yr.Z,"scaled:scale"))),
                                            Hab = as.vector(scale(Hab, center = attr(HAB.10yr.Z,"scaled:center"), scale = attr(HAB.10yr.Z,"scaled:scale"))),
                                            Dist_MUrb = as.vector(scale(Dist_MUrb, center = attr(DMU.10yr.Z,"scaled:center"), scale = attr(DMU.10yr.Z,"scaled:scale"))),
                                            Dist_OUrb = as.vector(scale(Dist_OUrb, center = attr(DOU.10yr.Z,"scaled:center"), scale = attr(DOU.10yr.Z,"scaled:scale"))))

# generate predictions
# adoption
options(na.action='na.pass')
Model_MatrixX <- model.matrix(~ LValHa + ha + MosType + LU_Sec + Hab + Dist_MUrb + Dist_OUrb + PC1 + PC2 + PC3 + PC4 + PC5, data = Predictions_DataX)
PredictionsX <- Model_MatrixX %*% CoefsX
PredictionsX <- exp(PredictionsX) / (1 + exp(PredictionsX))
# wta
options(na.action='na.pass')
Model_MatrixY <- model.matrix(~ LValHa + ha + MosType + LU_Sec + Hab + Dist_MUrb + Dist_OUrb + PC1 + PC2 + PC3 + PC4 + PC5, data = Predictions_DataY)
PredictionsY <- Model_MatrixX %*% CoefsY
# proportion
options(na.action='na.pass')
Model_MatrixZ <- model.matrix(~ LValHa + ha + MosType + LU_Sec + Hab + Dist_MUrb + Dist_OUrb + PC1 + PC2 + PC3 + PC4 + PC5, data = Predictions_DataZ)
PredictionsZ <- Model_MatrixZ %*% CoefsZ
PredictionsZ <- exp(PredictionsZ) / (1 + exp(PredictionsZ))

# compile predictions and write to csv
Compiled_Predictions_10yr <- as_tibble(cbind(Predictions_DataX, PredAdopt = PredictionsX, PredWTA = PredictionsY, PredProp = PredictionsZ)) %>% dplyr::select(CADID, PredAdopt, PredWTA, PredProp)
write.csv(Compiled_Predictions_10yr, file="output/predictions/spatial_predictions_10yr.csv", row.names = FALSE)
