# load libraries
library(tidyverse)

# load functions
source("functions.r")

# survey response data
Pure <- read_csv("input/survey_responses/pure_resp.csv")
BCT <- read_csv("input/survey_responses/bct_resp.csv")
Mail <- read_csv("input/survey_responses/mail_resp.csv")

# load relevant postcodes
Poas <-read_csv("input/survey_responses/uq_poa_expanded_studyextent.csv") #call uq_poa_expanded_studyextent.csv

# load lats and longs
Coords1 <- read_csv("input/geocode/other/Geocoded_246_NSW_lh.csv")
Coords2 <- do.call(rbind, lapply(list.files(path = "input/geocode/mailout/", full.names = TRUE), read_csv))

Compiled_Pure <- Pure %>%
                filter(DistributionChannel == "anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
                         Finished == "TRUE",
                         Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
                         Q3.6 !="", Q3.6 !="No",              # [COLUMN: Q3.6] remove if not property manager etc
                         #Q3.7_7 %in% Poas,       # [COLUMN: Q3.7_7] remove records that don't match the POA list
                         rid != 3657683231|7851483144|7393760845|6887891501|5410774572|1370281511) %>%                     # [COLUMN: rid] remove PureProfile tests: 3657683231, 7851483144, 7393760845, 6887891501, 5410774572, 1370281511
                         dplyr::select(ResponseId, rid, EndDate, DistributionChannel, # rids
                         Q3.7_4, Q3.7_5, Q3.7_6, Q3.7_7, Q3.7.1_4) %>% arrange(desc(EndDate)) %>% left_join(Coords1, by = c("ResponseId" = "ID"))

# remove those with no spatial location if necessary
Compiled_Pure <- Compiled_Pure %>% filter(!is.na(x))

write.csv(Compiled_Pure, file="output/compiled/pure_compiled.csv")

Compiled_BCT <- BCT %>%
                filter(DistributionChannel == "anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
                         Finished == "TRUE",
                         Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
                         Q3.6 !="", Q3.6 !="No",              # [COLUMN: Q3.6] remove if not property manager etc
                         #Q3.7_7 %in% Poas,       # [COLUMN: Q3.7_7] remove records that don't match the POA list
                         rid != 3657683231|7851483144|7393760845|6887891501|5410774572|1370281511) %>%                     # [COLUMN: rid] remove PureProfile tests: 3657683231, 7851483144, 7393760845, 6887891501, 5410774572, 1370281511
                         dplyr::select(ResponseId, rid, EndDate, DistributionChannel, # rids
                         Q3.7_4, Q3.7_5, Q3.7_6, Q3.7_7, Q3.7.1_4) %>% arrange(desc(EndDate)) %>% left_join(Coords1, by = c("ResponseId" = "ID"))

# remove those with no spatial location if necessary
Compiled_BCT <- Compiled_BCT %>% filter(!is.na(x))

write.csv(Compiled_BCT, file="output/compiled/bct_compiled.csv")

Compiled_Mail <- Mail %>%
                filter(DistributionChannel == "anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
                         Finished == "TRUE",
                         Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
                         Q3.6 !="", Q3.6 !="No",              # [COLUMN: Q3.6] remove if not property manager etc
                         #Q3.7_7 %in% Poas,       # [COLUMN: Q3.7_7] remove records that don't match the POA list
                         rid != 3657683231|7851483144|7393760845|6887891501|5410774572|1370281511) %>%                     # [COLUMN: rid] remove PureProfile tests: 3657683231, 7851483144, 7393760845, 6887891501, 5410774572, 1370281511
                         dplyr::select(ResponseId, rid, EndDate, DistributionChannel, Q83, # rids
                         Q3.7_4, Q3.7_5, Q3.7_6, Q3.7_7, Q3.7.1_4) %>% arrange(desc(EndDate)) %>% mutate(Q83 = toupper(Q83)) %>% left_join(Coords2, by = c("Q83" = "ExternalDataReference"))

# remove those with no spatial location if necessary
Compiled_Mail <- Compiled_Mail %>% filter(!is.na(Lat))

write.csv(Compiled_Mail, file="output/compiled/mail_compiled.csv")








                 dplyr::select(ResponseId, rid, SurveyID, EndDate, DistributionChannel, # rids
                        Q2.1, Q3.1,Q3.3,Q3.4,Q3.5,Q3.6,Q3.7_4,Q3.7_5,Q3.7_6,Q3.7_7,Q3.7.1_4, # selection criteria
                        Q5.1,Q5.1_19_TEXT, # land use
                        Q6.2_1,Q6.3,Q6.4,Q6.5,Q6.6,Q6.6_8_TEXT, # participation
                        Q6.7_1,Q6.7_2,Q6.7_3,Q6.7_4,Q6.7_5,Q6.7_6,Q6.7_7,Q6.7_8,Q6.7_9,Q6.7_10,Q6.7_11,Q6.7_12,Q6.7_13,Q6.7_14,Q6.7_15,Q6.8,Q6.8_11_TEXT,Q6.9,Q6.9_11_TEXT, # SLF motivation questions
                        Q9.1_1,Q9.1_2,Q9.1_3,Q9.1_4,Q9.2_1,Q9.2_2,Q9.2_3,Q9.2_4,Q9.3_1,Q9.3_2,Q9.3_3,Q9.3_4,Q9.4_1,Q9.4_2,Q9.4_3,Q9.4_4,Q9.4_5,Q9.4_6, # PMT questions
                        Q10.1_1,Q10.1_2,Q10.2_1,Q10.2_2,Q10.3_1,Q10.3_2,Q10.3_3,Q10.4_1,Q10.4_2,Q10.4_3,Q10.4_4,Q10.4_5,Q10.4_6,Q10.4_7,Q10.4_8,Q10.4_9,Q10.4_10,Q10.4_11,Q10.4_12,Q10.4_13, # Norms and Values questions
                        Q11.1,Q11.2,Q11.3,Q11.3_8_TEXT,Q11.4,Q11.4_9_TEXT,Q11.5,Q11.6,Q11.7,Q11.8,Q11.8_24_TEXT,Q11.9,Q11.10,Q11.11,Q11.12,Q11.12_10_TEXT,Q11.13,Q11.13_7_TEXT,Q11.14,Q11.14_3_TEXT,Q11.15)%>%  # Demographic information questions | Select only relevant columns to come up with a complete rid list
                 arrange(desc(EndDate))














#####################################################--------------------------#######
#   RAW SOCIAL SURVEY DATA PROCESSING AND ANALYSIS             ----------------#######
#------------------------------------------------------------------------------#######
#
# By: Carla Archibald
# Created: 17 April 2020
# Objective: Script for taking the raw survey data from PureProfile and converting it
#            into a usable format for subsequent analysis
#
# Data & object codes: rd_ = raw data; pd_ = processed data; ud_ = using data
#
###### ----------------------------------------------------------------------- #######

# PACKAGES ####
install.packages("tidyverse")
library(tidyverse) # for readr, tidyr, ggplot2 and dplyr
library(tidyr) # for readr, tidyr, ggplot2 and dplyr
library(dplyr) # for readr, tidyr, ggplot2 and dplyr
library(fs) # file system operations
library(purrr)
library(readr)

# wd ####
wd <- setwd("R:/KPRIVATE19-A2212/code/data/social-survey/")
data_dir <-"R:/KPRIVATE19-A2212/code/data/social-survey/"

csv_files_list <- fs::dir_ls(data_dir, regexp = "\\.csv$")

#readr::read_csv(csv_files[5])

csv_files_object <-  csv_files_list %>% # All files being imported need to be closed in other programs (e.g., excel) otherwise this will crash R
                     map_dfr(read_csv)

temp <- list.files(pattern="*.csv")

rawData <- lapply(temp, function(x) {
  tmp <- try(read.csv(x))
  if (!inherits(tmp, 'try-error')) tmp
})

rd_survey_data <-rawData[[1]]
rd_survey_data$Q3.4<-as.numeric(rd_survey_data$Q3.4)
rd_survey_data$Q3.5<-as.numeric(rd_survey_data$Q3.5)

rd_poa_study_area <-rawData[[5]][,c(1)] #call uq_poa_expanded_studyextent.csv

# Data processing ####
#---# Generate questions ID and description table  #####
pd_question_number_index <- rd_survey_data %>%
                top_n(1) %>%  # Select question ID and description rows
                gather(QuestionID, Description, everything()) # Transponse rows to columns

write.csv(pd_question_number_index, file="./processed_raw_data/pd_question_number_index.csv")

#---# To find entries that were outside of the study area  #####
pd_notinpostcode_data_list <- rd_survey_data %>%
                              dplyr::select(ResponseId, rid, Finished, EndDate, DistributionChannel, Q2.1, Q3.3, Q3.4, Q3.5, Q3.6, Q3.7_4, Q3.7_5, Q3.7_6, Q3.7_7,Q3.7.1_4)%>%  # Select only relevant columns to come up with a complete rid list
                              dplyr::filter(DistributionChannel == "anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
                                            Finished == "TRUE",                 # [COLUMN: Q2.1] remove 'I do not consent'
                                       Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
                                       Q3.6 !="", Q3.6 !="No",              # [COLUMN: Q3.6] remove if not property manager etc
                                       !(Q3.7_7 %in% rd_poa_study_area),       # [COLUMN: Q3.7_7] remove records that dont match the POA list
                                       rid != 3657683231|7851483144|7393760845|6887891501|5410774572|1370281511) %>%                       # [COLUMN: rid] remove PureProfile tests: 3657683231, 7851483144, 7393760845, 6887891501, 5410774572, 1370281511
                              arrange(desc(EndDate))

#---# To find entries that were not in the POA and NOT TOTALLY complete, did not include FULL ADDRESS  #####
pd_completes_data_no_poa_list_PP <- rd_survey_data %>%
  dplyr::filter(DistributionChannel=="anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
         Finished == "TRUE",
         Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
         Q3.6 !="", Q3.6 !="No",              # [COLUMN: Q3.6] remove if not property manager etc
         # Q3.7_7 %in% rd_poa_study_area,       # [COLUMN: Q3.7_7] remove records that dont match the POA list
         rid != 3657683231|7851483144|7393760845|6887891501|5410774572|1370281511) %>%                       # [COLUMN: rid] remove PureProfile tests: 3657683231, 7851483144, 7393760845, 6887891501, 5410774572, 1370281511
  dplyr::select(ResponseId, rid, EndDate, DistributionChannel, Q2.1, Q3.3, Q3.4, Q3.5, Q3.6, Q3.7_4, Q3.7_5, Q3.7_6, Q3.7_7,Q3.7.1_4 )%>%  # Select only relevant columns to come up with a complete rid list
  arrange(desc(EndDate))

write.csv(pd_completes_data_no_poa_list_PP, file="./processed_raw_data/pd_all_data_NswQld_PP.csv")

#---# To find entries that were NOT TOTALLY complete did not include FULL ADDRESS  #####
pd_completes_data_list_PP <- rd_survey_data %>%
                             filter(DistributionChannel=="anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
                                 Finished == "TRUE",
                                 Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
                             Q3.6 !="", Q3.6 !="No",              # [COLUMN: Q3.6] remove if not property manager etc
                             Q3.7_7 %in% rd_poa_study_area,       # [COLUMN: Q3.7_7] remove records that dont match the POA list
                             rid != 3657683231|7851483144|7393760845|6887891501|5410774572|1370281511) %>%                       # [COLUMN: rid] remove PureProfile tests: 3657683231, 7851483144, 7393760845, 6887891501, 5410774572, 1370281511
                          # dplyr::select(ResponseId, rid, EndDate, DistributionChannel, Q2.1, Q3.3, Q3.4, Q3.5, Q3.6, Q3.7_4, Q3.7_5, Q3.7_6, Q3.7_7,Q3.7.1_4 )%>%  # Select only relevant columns to come up with a complete rid list
                           arrange(desc(EndDate))

write.csv(pd_completes_data_list_PP, file="./processed_raw_data/pd_completes_data_list_PP.csv")

#---# To find entries that were TOTALLY complete including FULL ADDRESS  #####
pd_complete_spatial_list <- rd_survey_data %>%
                            filter(DistributionChannel == "anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
                                     Finished == "True",
                                     Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
                                     Q3.6 !="", Q3.6 !="No",              # [COLUMN: Q3.6] remove if not property manager etc
                                     Q3.7_7 %in% rd_poa_study_area,       # [COLUMN: Q3.7_7] remove records that dont match the POA list
                                     Q3.3 == "Hectares (ha)" & as.numeric(Q3.4) >=1.99 |  Q3.3 == "Acres (ac)" & as.numeric(Q3.5) >=4.94,  # [COLUMN: Q3.3] if Q3.3 = HECTARES (ha) remove records less than 3; [COLUMN: Q3.3] if Q3.3 = [COLUMN: Q3.3] if Q3.3 = ACRES (ac) remove records less than 7.5 remove records less than 3
                                     rid != 3657683231|7851483144|7393760845|6887891501|5410774572|1370281511) %>%                       # [COLUMN: rid] remove PureProfile tests: 3657683231, 7851483144, 7393760845, 6887891501, 5410774572, 1370281511
                            dplyr::select(ResponseId, rid, StartDate, DistributionChannel, Q2.1, Q3.3, Q3.4, Q3.5, Q3.6, Q3.7_4, Q3.7_5, Q3.7_6, Q3.7_7,Q3.7.1_4 )  # Select only relevant columns to come up with a complete rid list

write.csv(pd_complete_spatial_list, file="./processed_raw_data/pd_all_data_spatial.csv")

#---# To compile data for the SLF analysis  #####
rd_SLF_data <- rd_survey_data %>%
                      filter(DistributionChannel == "anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
                               Finished == "TRUE",
                               Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
                               Q3.6 !="", Q3.6 !="No",              # [COLUMN: Q3.6] remove if not property manager etc
                               #Q3.7_7 %in% rd_poa_study_area,       # [COLUMN: Q3.7_7] remove records that dont match the POA list
                               rid != 3657683231|7851483144|7393760845|6887891501|5410774572|1370281511) %>%                       # [COLUMN: rid] remove PureProfile tests: 3657683231, 7851483144, 7393760845, 6887891501, 5410774572, 1370281511
                       dplyr::select(ResponseId, rid, SurveyID, EndDate, DistributionChannel, # rids
                              Q2.1, Q3.1,Q3.3,Q3.4,Q3.5,Q3.6,Q3.7_4,Q3.7_5,Q3.7_6,Q3.7_7,Q3.7.1_4, # selection criteria
                              Q5.1,Q5.1_19_TEXT, # land use
                              Q6.2_1,Q6.3,Q6.4,Q6.5,Q6.6,Q6.6_8_TEXT, # participation
                              Q6.7_1,Q6.7_2,Q6.7_3,Q6.7_4,Q6.7_5,Q6.7_6,Q6.7_7,Q6.7_8,Q6.7_9,Q6.7_10,Q6.7_11,Q6.7_12,Q6.7_13,Q6.7_14,Q6.7_15,Q6.8,Q6.8_11_TEXT,Q6.9,Q6.9_11_TEXT, # SLF motivation questions
                              Q9.1_1,Q9.1_2,Q9.1_3,Q9.1_4,Q9.2_1,Q9.2_2,Q9.2_3,Q9.2_4,Q9.3_1,Q9.3_2,Q9.3_3,Q9.3_4,Q9.4_1,Q9.4_2,Q9.4_3,Q9.4_4,Q9.4_5,Q9.4_6, # PMT questions
                              Q10.1_1,Q10.1_2,Q10.2_1,Q10.2_2,Q10.3_1,Q10.3_2,Q10.3_3,Q10.4_1,Q10.4_2,Q10.4_3,Q10.4_4,Q10.4_5,Q10.4_6,Q10.4_7,Q10.4_8,Q10.4_9,Q10.4_10,Q10.4_11,Q10.4_12,Q10.4_13, # Norms and Values questions
                              Q11.1,Q11.2,Q11.3,Q11.3_8_TEXT,Q11.4,Q11.4_9_TEXT,Q11.5,Q11.6,Q11.7,Q11.8,Q11.8_24_TEXT,Q11.9,Q11.10,Q11.11,Q11.12,Q11.12_10_TEXT,Q11.13,Q11.13_7_TEXT,Q11.14,Q11.14_3_TEXT,Q11.15)%>%  # Demographic information questions | Select only relevant columns to come up with a complete rid list
                       arrange(desc(EndDate))
write.csv(rd_SLF_data, file="./processed_raw_data/rd_SLF_data.csv")

#---# To compile data for the BWS analysis  #####
rd_BWS_data <- rd_survey_data %>%
               dplyr::filter(DistributionChannel == "anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
                      Finished == "TRUE",
                      Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
                     Q3.6 !="", Q3.6 !="No",              # [COLUMN: Q3.6] remove if not property manager etc
                     #Q3.7_7 %in% rd_poa_study_area,       # [COLUMN: Q3.7_7] remove records that dont match the POA list
                     rid != 3657683231|7851483144|7393760845|6887891501|5410774572|1370281511) %>%                       # [COLUMN: rid] remove PureProfile tests: 3657683231, 7851483144, 7393760845, 6887891501, 5410774572, 1370281511
  dplyr::select(ResponseId, rid, EndDate, DistributionChannel, # rids
                     Q2.1, Q3.1,Q3.3,Q3.4,Q3.5,Q3.6,Q3.7_4,Q3.7_5,Q3.7_6,Q3.7_7,Q3.7.1_4, # selection criteria
                     Q5.1,Q5.1_19_TEXT, # land use
                     Q6.2_1,Q6.3,Q6.4,Q6.5,Q6.6,Q6.6_8_TEXT,Q6.8,Q6.8_11_TEXT,Q6.9,Q6.9_11_TEXT, # participation
                     Q7.2_1,Q7.2_2,Q7.2_3,Q7.3_1,Q7.3_2,Q7.3_3,Q7.4_1,Q7.4_2,Q7.4_3,Q7.5_1,Q7.5_2,Q7.5_3,Q7.6_1,Q7.6_2,Q7.6_3,Q7.7_1,Q7.7_2, Q7.7_3,Q7.8_1,Q7.8_2,Q7.8_3, # BWS motivation questions
                     Q8.8_1,
                     Q10.1_1,Q10.1_2,Q10.2_1,Q10.2_2,Q10.3_1,Q10.3_2,Q10.3_3,Q10.4_1,Q10.4_2,Q10.4_3,Q10.4_4,Q10.4_5,Q10.4_6,Q10.4_7,Q10.4_8,Q10.4_9,Q10.4_10,Q10.4_11,Q10.4_12,Q10.4_13, # Norms and Values questions
                     Q11.1,Q11.2,Q11.3,Q11.3_8_TEXT,Q11.4,Q11.4_9_TEXT,Q11.5,Q11.6,Q11.7,Q11.8,Q11.8_24_TEXT,Q11.9,Q11.10,Q11.11,Q11.12,Q11.12_10_TEXT,Q11.13,Q11.13_7_TEXT,Q11.14,Q11.14_3_TEXT,Q11.15)%>%  # Demographic information questions | Select only relevant columns to come up with a complete rid list
               arrange(desc(EndDate))
write.csv(rd_BWS_data, file="./processed_raw_data/rd_BWS_data.csv")

#---# To compile data for the WTP analysis  #####
rd_WTP_data <- rd_survey_data %>%
  filter(DistributionChannel == "anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
         Finished == "TRUE",
         Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
         Q3.6 !="", Q3.6 !="No",              # [COLUMN: Q3.6] remove if not property manager etc
        # Q3.7_7 %in% rd_poa_study_area,       # [COLUMN: Q3.7_7] remove records that dont match the POA list
         rid != 3657683231|7851483144|7393760845|6887891501|5410774572|1370281511) %>%                       # [COLUMN: rid] remove PureProfile tests: 3657683231, 7851483144, 7393760845, 6887891501, 5410774572, 1370281511
  select(ResponseId, rid, EndDate, DistributionChannel, # rids
         Q2.1, Q3.1,Q3.3,Q3.4,Q3.5,Q3.6,Q3.7_4,Q3.7_5,Q3.7_6,Q3.7_7,Q3.7.1_4, # selection criteria
         Q5.1,Q5.1_19_TEXT, # land use
         Q6.2_1,Q6.3,Q6.4,Q6.5,Q6.6,Q6.6_8_TEXT, # participation
         Q8.4,Q8.5,Q8.6,Q8.7,Q8.8_1, # WTP questions
         Q10.1_1,Q10.1_2,Q10.2_1,Q10.2_2,Q10.3_1,Q10.3_2,Q10.3_3,Q10.4_1,Q10.4_2,Q10.4_3,Q10.4_4,Q10.4_5,Q10.4_6,Q10.4_7,Q10.4_8,Q10.4_9,Q10.4_10,Q10.4_11,Q10.4_12,Q10.4_13, # Norms and Values questions
         Q11.1,Q11.2,Q11.3,Q11.3_8_TEXT,Q11.4,Q11.4_9_TEXT,Q11.5,Q11.6,Q11.7,Q11.8,Q11.8_24_TEXT,Q11.9,Q11.10,Q11.11,Q11.12,Q11.12_10_TEXT,Q11.13,Q11.13_7_TEXT,Q11.14,Q11.14_3_TEXT,Q11.15)%>%  # Demographic information questions | Select only relevant columns to come up with a complete rid list
  arrange(desc(EndDate))
write.csv(rd_WTP_data, file="./processed_raw_data/rd_WTP_data.csv")

#---# To compile data for the Mosaic characterisation analysis  #####
rd_Mosaic_Coding_data <- rd_survey_data %>%
  filter(DistributionChannel == "anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
         Finished == "TRUE",
         Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
         Q3.6 !="", Q3.6 !="No",              # [COLUMN: Q3.6] remove if not property manager etc
         Q3.7_7 %in% rd_poa_study_area,       # [COLUMN: Q3.7_7] remove records that dont match the POA list
         rid != 3657683231|7851483144|7393760845|6887891501|5410774572|1370281511) %>%                       # [COLUMN: rid] remove PureProfile tests: 3657683231, 7851483144, 7393760845, 6887891501, 5410774572, 1370281511
  select(ResponseId, rid, EndDate, DistributionChannel, # rids
         Q3.3,Q3.4,Q3.5,Q3.7_7, # selection criteria
         Q11.1,Q11.2,Q11.3,Q11.3_8_TEXT,Q11.4,Q11.4_9_TEXT,Q11.5,Q11.6,Q11.7,Q11.8,Q11.8_24_TEXT,Q11.9,Q11.10,Q11.11,Q11.12,Q11.12_10_TEXT,Q11.13,Q11.13_7_TEXT,Q11.14,Q11.14_3_TEXT,Q11.15)%>%  # Demographic information questions | Select only relevant columns to come up with a complete rid list
  arrange(desc(EndDate))
write.csv(rd_Mosaic_Coding_data, file="./processed_raw_data/rd_Mosaic_Coding_data.csv")

#---# To compile data for the PMT analysis  #####
rd_PMT_data <- rd_survey_data %>%
  filter(DistributionChannel == "anonymous",  # [COLUMN: DistributionChannel] remove preview dates]
         Finished == "TRUE",
         Q2.1 == "I consent",                 # [COLUMN: Q2.1] remove 'I do not consent'
         Q3.6 !="", Q3.6 !="No",              # [COLUMN: Q3.6] remove if not property manager etc
         #Q3.7_7 %in% rd_poa_study_area,       # [COLUMN: Q3.7_7] remove records that dont match the POA list
         rid != 3657683231|7851483144|7393760845|6887891501|5410774572|1370281511) %>%                       # [COLUMN: rid] remove PureProfile tests: 3657683231, 7851483144, 7393760845, 6887891501, 5410774572, 1370281511
  select(ResponseId, rid, EndDate, DistributionChannel, # rids
         Q2.1, Q3.1,Q3.3,Q3.4,Q3.5,Q3.6,Q3.7_4,Q3.7_5,Q3.7_6,Q3.7_7,Q3.7.1_4, # selection criteria
         Q5.1,Q5.1_19_TEXT, # land use
         Q6.2_1,Q6.3,Q6.4,Q6.5,Q6.6,Q6.6_8_TEXT, # participation
         Q6.7_1,Q6.7_2,Q6.7_3,Q6.7_4,Q6.7_5,Q6.7_6,Q6.7_7,Q6.7_8,Q6.7_9,Q6.7_10,Q6.7_11,Q6.7_12,Q6.7_13,Q6.7_14,Q6.7_15,Q6.8,Q6.8_11_TEXT,Q6.9,Q6.9_11_TEXT, # SLF motivation questions
         Q9.1_1,Q9.1_2,Q9.1_3,Q9.1_4,Q9.2_1,Q9.2_2,Q9.2_3,Q9.2_4,Q9.3_1,Q9.3_2,Q9.3_3,Q9.3_4,Q9.4_1,Q9.4_2,Q9.4_3,Q9.4_4,Q9.4_5,Q9.4_6, # PMT questions
         Q10.1_1,Q10.1_2,Q10.2_1,Q10.2_2,Q10.3_1,Q10.3_2,Q10.3_3,Q10.4_1,Q10.4_2,Q10.4_3,Q10.4_4,Q10.4_5,Q10.4_6,Q10.4_7,Q10.4_8,Q10.4_9,Q10.4_10,Q10.4_11,Q10.4_12,Q10.4_13, # Norms and Values questions
         Q11.1,Q11.2,Q11.3,Q11.3_8_TEXT,Q11.4,Q11.4_9_TEXT,Q11.5,Q11.6,Q11.7,Q11.8,Q11.8_24_TEXT,Q11.9,Q11.10,Q11.11,Q11.12,Q11.12_10_TEXT,Q11.13,Q11.13_7_TEXT,Q11.14,Q11.14_3_TEXT,Q11.15)%>%  # Demographic information questions | Select only relevant columns to come up with a complete rid list
  arrange(desc(EndDate))
write.csv(rd_PMT_data, file="./processed_raw_data/rd_PMT_data.csv")
