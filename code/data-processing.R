# load packages
library(vctrs)
library(tidyverse)

# load data
rd_WTA_data <-read.csv("./data/ppData2.csv")

# data preparation

#LIM <- cbind((rep(0,224)),(rep(2500,224)))
#A <- 262
#W <- 224   # number of respondents who are Willing to Participate

# fill missing values - THIS IS A VERY NAIVE DATA IMPUTATION METHOD - ALSO NOTE WE DON'T IMPUTE THE REPSONSE VARIABLE - THESE STAY AS NA - NEED TO LOOK AT THIS TO IMPROVE (24/7/20)
rd_WTA_data <- rd_WTA_data %>%
               mutate(ha = ifelse(is.na(ha), mean(ha, na.rm = T), ha),
                      Q8.4_Inf_WTA = as.character(Q8.4_Inf_WTA),
                      Q8.6_10yr_WTA = as.character(Q8.6_10yr_WTA),
                      Age = ifelse(is.na(Age), mean(Age, na.rm = T), Age),
                      Residence = ifelse(is.na(Residence), mean(Residence, na.rm = T), Residence),
                      Retired = ifelse(is.na(Retired), "0", Retired),
                      BlueCollar = ifelse(is.na(BlueCollar), "0", BlueCollar),
                      HighMortgage = ifelse(is.na(HighMortgage), "0", HighMortgage),
                      HighIncome = ifelse(is.na(HighIncome), "0",  HighIncome),
                      SeenKoala = ifelse(is.na(SeenKoala), "0", SeenKoala),
                      RelianceFarmingGT25perc = ifelse(is.na(RelianceFarmingGT25perc), "0", RelianceFarmingGT25perc)) %>% as_tibble()

# create binary vector of whether the landholder would even consider adopting an agreement
ACCEPT.Inf.df <- rd_WTA_data %>%
             arrange(desc(ResponseId)) %>% mutate (Q8.4_Inf_WTA = trimws(Q8.4_Inf_WTA)) %>%
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
                                   " "="", .default = "")) %>%
                   select(ACCEPT)

ACCEPT.Inf <- as.numeric(unlist(ACCEPT.Inf.df))

# create binary vector of whether the landholder would even consider adopting an agreement
ACCEPT.10yr.df <- rd_WTA_data %>%
             arrange(desc(ResponseId)) %>% mutate (Q8.6_10yr_WTA = trimws(Q8.6_10yr_WTA)) %>%
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
                                   " "="", .default = "")) %>%
                   select(ACCEPT)

ACCEPT.10yr <- as.numeric(unlist(ACCEPT.10yr.df))







HA_A.df  <- rd_WTA_data %>% # HA vector, length = N (262)
    arrange(desc(ResponseId)) %>%
    select(ha)
HA_A.df <- as.numeric(unlist(HA_A.df))
HA_A <-as.data.frame(scale(HA_A.df))
HA_A <- unlist(HA_A)
names(HA_A) <- NULL
HA_A

AGE_A.df  <- rd_WTA_data %>% # HA vector, length = N (262)
    arrange(desc(ResponseId)) %>%
    select(Age)
AGE_A <- scale(as.numeric(unlist(AGE_A.df)))

GENDER_A.df  <- rd_WTA_data %>% # HA vector, length = N (262)
  arrange(desc(ResponseId)) %>%
  select(Gender)
GENDER_A <- scale(as.numeric(unlist(GENDER_A.df)))

COUPLE_A.df  <- rd_WTA_data %>%
  arrange(desc(ResponseId)) %>%
  select(Couple)
COUPLE_A <- scale(as.numeric(unlist(COUPLE_A.df)))

FAMILY_A.df  <- rd_WTA_data %>%
  arrange(desc(ResponseId)) %>%
  select(FamilyAtHome)
FAMILY_A <- scale(as.numeric(unlist(FAMILY_A.df)))

AUS_A.df  <- rd_WTA_data %>%
  arrange(desc(ResponseId)) %>%
  select(BirthPlace_Aus)
AUS_A <- scale(as.numeric(unlist(AUS_A.df)))

UNI_A.df  <- rd_WTA_data %>%
  arrange(desc(ResponseId)) %>%
  select(UniEd)
UNI_A <- scale(as.numeric(unlist(UNI_A.df)))

RETIRE_A.df  <- rd_WTA_data %>%
  arrange(desc(ResponseId)) %>%
  select(Retired)
RETIRE_A <- scale(as.numeric(unlist(RETIRE_A.df)))

BLUECOLL_A.df  <- rd_WTA_data %>%
  arrange(desc(ResponseId)) %>%
  select(BlueCollar)
BLUECOLL_A <- scale(as.numeric(unlist(BLUECOLL_A.df)))

FARMING_A.df  <- rd_WTA_data %>%
  arrange(desc(ResponseId)) %>%
  select(RelianceFarmingGT25perc)
FARMING_A <- scale(as.numeric(unlist(FARMING_A.df)))

RESIDENCE_A.df  <- rd_WTA_data %>%
  arrange(desc(ResponseId)) %>%
  select(Residence)
RESIDENCE_A <- scale(as.numeric(unlist(RESIDENCE_A.df)))

INCOME_A.df  <- rd_WTA_data %>%
  arrange(desc(ResponseId)) %>%
  select(HighIncome)
INCOME_A <- scale(as.numeric(unlist(INCOME_A.df)))

MORTGAGE_A.df  <- rd_WTA_data %>%
  arrange(desc(ResponseId)) %>%
  select(HighMortgage)
MORTGAGE_A <- scale(as.numeric(unlist(MORTGAGE_A.df)))

KOALA_A.df  <- rd_WTA_data %>%
  arrange(desc(ResponseId)) %>%
  select(SeenKoala)
KOALA_A <- scale(as.numeric(unlist(KOALA_A.df)))

KMA_A.df  <- rd_WTA_data %>%
  arrange(desc(ResponseId)) %>%
  select(kma_code)
KMA_A <- scale(as.numeric(unlist(KMA_A.df)))


M.df <- rd_WTA_data %>%  # binary vector of whether you will accept a payment, length = N (262)
  arrange(desc(ResponseId)) %>%
  mutate(ACCEPT = recode(Q8.4_Inf_WTA,
                         "$50 "="1",
                         "$100 "="1",
                         "$1,500 "="1",
                         "$2,500 "="1",
                         ">$2,500"="1",
                         "$2,000 "="1",
                         "I would not participate"="0",
                         "I would pay"="1",
                         "$500 "="1",
                         "$750 "="1",
                         "$25 "="1",
                         "$1,000 "="1",
                         "$250 "="1",
                         "$0 "="1", .default = "0")) %>%
    filter(ACCEPT== "1") %>%
    select(ResponseId,ACCEPT,Q8.4_Inf_WTA,ha,CC_Adopt_Num, CC_Adopt_Bin,POA,Age,Gender,Couple,FamilyAtHome,UniEd,BirthPlace_Aus,
           BlueCollar,Residence,RelianceFarmingGT25perc,HighIncome,HighMortgage, SeenKoala,
           kma_code,Retired)

CENS <- M.df %>%  # vector indicating whether the value is left or right censored, length = M
  arrange(desc(ResponseId)) %>%
  mutate(CENS = recode(Q8.4_Inf_WTA,
                         "$50 "="1",
                         "$100 "="1",
                         "$1,500 "="1",
                         "$2,500 "="1",
                         ">$2,500"="2",
                         "$2,000 "="1",
                         "I would pay"="0",
                         "$500 "="1",
                         "$750 "="1",
                         "$25 "="1",
                         "$1,000 "="1",
                         "$250 "="1",
                         "$0 "="1", .default = NA_character_)) %>%
  select(CENS)
CENS <- as.numeric(unlist(CENS))

# and the NA values in the WTA, perhaps a value below 0 for the left censored one and above zero for the right censored one, Change values that have a value to NA
WTA <- M.df %>%  # vector indicaing whether the value is left or right censored, length = M # continuous numeric vaule indicating the WTA value, length = M... There will be NAs where values were censored out.
        arrange(desc(ResponseId)) %>%
        mutate(WTA = recode(Q8.4_Inf_WTA,
                       ">$2,500"= "NA",
                       "I would pay"= "NA",
                       "$50 "="50",
                       "$100 "="100",
                       "$1,500 "="1500",
                       "$2,500 "="2500",
                       "$2,000 "="2000",
                       "$500 "="500",
                       "$750 "="750",
                       "$25 "="25",
                       "$1,000 "="1000",
                       "$250 "="250",
                       "$0 "="0")) %>%
        select(WTA)

WTA_Aum <- as.numeric(WTA$WTA)
WTA <- unlist(WTA_Aum)
WTA.test <- lapply(WTA, function(x) x / 1000)
WTA <- unlist(WTA.test)


HA_W.df  <- M.df %>% # HA vector, length = M
            arrange(desc(ResponseId)) %>%
            select(ha)
HA_W.df <- as.numeric(unlist(HA_W.df))
HA_W <-as.data.frame(scale(HA_W.df))
HA_W <- unlist(HA_W)
names(HA_W) <- NULL
HA_W

AGE_W.df  <- M.df %>% # HA vector, length = N (262)
  arrange(desc(ResponseId)) %>%
  select(Age)

AGE_W <- scale(as.numeric(unlist(AGE_W.df)))

GENDER_W.df  <- M.df %>% # HA vector, length = N (262)
  arrange(desc(ResponseId)) %>%
  select(Gender)
GENDER_W <- scale(as.numeric(unlist(GENDER_W.df)))

COUPLE_W.df  <- M.df %>%
  arrange(desc(ResponseId)) %>%
  select(Couple)
COUPLE_W <- scale(as.numeric(unlist(COUPLE_W.df)))

FAMILY_W.df  <- M.df %>%
  arrange(desc(ResponseId)) %>%
  select(FamilyAtHome)
FAMILY_W <- scale(as.numeric(unlist(FAMILY_W.df)))

AUS_W.df  <- M.df %>%
  arrange(desc(ResponseId)) %>%
  select(BirthPlace_Aus)
AUS_W <- scale(as.numeric(unlist(AUS_W.df)))

UNI_W.df  <- M.df %>%
  arrange(desc(ResponseId)) %>%
  select(UniEd)
UNI_W <- scale(as.numeric(unlist(UNI_W.df)))

RETIRE_W.df  <- M.df %>%
  arrange(desc(ResponseId)) %>%
  select(Retired)
RETIRE_W <- as.numeric(unlist(RETIRE_W.df))

BLUECOLL_W.df  <- M.df %>%
  arrange(desc(ResponseId)) %>%
  select(BlueCollar)
BLUECOLL_W <- as.numeric(unlist(BLUECOLL_W.df))

FARMING_W.df  <- M.df %>%
  arrange(desc(ResponseId)) %>%
  select(RelianceFarmingGT25perc)
FARMING_W <- scale(as.numeric(unlist(FARMING_W.df)))

RESIDENCE_W.df  <- M.df %>%
  arrange(desc(ResponseId)) %>%
  select(Residence)
RESIDENCE_W <- scale(as.numeric(unlist(RESIDENCE_W.df)))

INCOME_W.df  <- M.df %>%
  arrange(desc(ResponseId)) %>%
  select(HighIncome)
INCOME_W <- scale(as.numeric(unlist(INCOME_W.df)))

MORTGAGE_W.df  <- M.df %>%
  arrange(desc(ResponseId)) %>%
  select(HighMortgage)
MORTGAGE_W <- scale(as.numeric(unlist(MORTGAGE_W.df)))

KOALA_W.df  <- M.df %>%
  arrange(desc(ResponseId)) %>%
  select(SeenKoala)
KOALA_W <- scale(as.numeric(unlist(KOALA_W.df)))

KMA_W.df  <- M.df %>%
  arrange(desc(ResponseId)) %>%
  select(kma_code)
KMA_W <- scale(as.numeric(unlist(KMA_W.df)))


save(A, W, LIM, ACCEPT, CENS, WTA,
     HA_A, AGE_A, GENDER_A, COUPLE_A, FAMILY_A, AUS_A, UNI_A, RETIRE_A, BLUECOLL_A, FARMING_A, RESIDENCE_A, INCOME_A, MORTGAGE_A, KOALA_A,
     HA_W, AGE_W, GENDER_W, COUPLE_W, FAMILY_W, AUS_W, UNI_W, RETIRE_W, BLUECOLL_W, FARMING_W, RESIDENCE_W, INCOME_W, MORTGAGE_W, KOALA_W,
     file = "wta-input-data-all.RData")

save(A, W, LIM, ACCEPT, CENS, WTA,
     HA_A,
     HA_W,
     file = "wta-input-data.RData")


M <- 500

HA_M.df <- rd_spatial_data %>%
  arrange(desc(cadid)) %>%
  mutate(ha = (shape_area/10000))%>%
  select(ha)%>%
  head(500)

HA_M <- scale(as.numeric(unlist(HA_M.df)))

save(N, n, M,LIM, ACCEPT, CENS, WTA,
     HA_A,
     HA_A,
     HA_M,
     file = "wta-input-data-pred.RData")

View(summary_pred_V1)
summary_expl_V1 <- read_csv("R:/KPRIVATE19-A2212/code/social-survey/spatial-model-of-adoption/data-output/summary_V5.csv")
View(summary_expl_V1)

ggplot(data = summary_expl_V1)+
  geom_point(aes(x=Name,y=Median,color=Model),size=3)+
  geom_linerange(aes(x = Name, ymin = Lower95, ymax = Median, color = Model))+
  geom_linerange(aes(x = Name, ymax = Upper95, ymin = Median, color = Model))+
  facet_grid(~Model)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete("")

ggplot(data = summary_expl_V1)+
  geom_point(aes(x=X1,y=Median),size=3)+
  geom_linerange(aes(x = X1, ymin = Lower95, ymax = Median))+
  geom_linerange(aes(x = X1, ymax = Upper95, ymin = Median))+
  #facet_grid(~Model)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete("")
