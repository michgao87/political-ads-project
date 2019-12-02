# Clean downloaded data sets to be used in Shiny app

library(fs)
library(infer)
library(purrr)
library(lubridate)
library(ISOweek)
library(broom)
library(ggplot2)
library(tidyverse)

# GOOGLE ADS DATA

# Read in the political ad data sets and filter relevant information
# Only rows pertaining to US presidential election 
# and only columns with data I will use for the regression models
# See README.txt in google-ads-data folder for details on each csv file

ads <- read_csv("google-ads-data/google-political-ads-creative-stats.csv") %>% 
  filter(Regions == "US") %>% 
  select(Ad_Type, Advertiser_ID, Advertiser_Name, Ad_Campaigns_List, 
         Date_Range_Start, Date_Range_End, Num_of_Days, Impressions, 
         Spend_USD, Spend_Range_Min_USD, Spend_Range_Max_USD) %>% 
  mutate(end_week = ceiling_date(Date_Range_Start, "week", 
                                 week_start = 6, change_on_boundary = F))

advertisers <- read_csv("google-ads-data/google-political-ads-advertiser-stats.csv") %>% 
  filter(Regions == "US") %>% 
  select(Advertiser_ID, Advertiser_Name, Public_IDs_List, Total_Creatives, Spend_USD)

advertisers_weekly_spend <-
  read_csv("google-ads-data/google-political-ads-advertiser-weekly-spend.csv") %>% 
  select(Advertiser_ID, Advertiser_Name, Week_Start_Date, Spend_USD) %>% 
  mutate(Week_End_Date = Week_Start_Date + 6)

ad_campaigns <- read_csv("google-ads-data/google-political-ads-campaign-targeting.csv") %>% 
  mutate(end_week = ceiling_date(Start_Date, "week", week_start = 6, change_on_boundary = F))

geo_spend <- read_csv("google-ads-data/google-political-ads-geo-spend.csv")
keywords <- read_csv("google-ads-data/google-political-ads-top-keywords-history.csv")

# Create a vector holding the PACs for the Democratic presidential candidates
# to determine which ads belong to which candidates 

candidates_pacs = "BIDEN|WARREN|BERNIE|PETE FOR AMERICA|KAMALA HARRIS FOR THE PEOPLE|CORY 2020|YANG|TULSI NOW|WILLIAMSON|AMY FOR AMERICA|BULLOCK FOR PRESIDENT|JULIAN FOR THE FUTURE|JOHN DELANEY|WAYNE MESSAM|TOM STEYER|GILLIBRAND 2020|BETO FOR AMERICA|DE BLASIO|MIKE GRAVEL|HICKENLOOPER|INSLEE|SETH MOULTON|SWALWELL|TIM RYAN"

# Create a function that creates a new column with 
# candidates' names based on the advertisers' names

ad_to_candidate <- function(Advertiser_Name) {
  case_when(
    str_detect(Advertiser_Name, "BIDEN") ~ "Biden",
    str_detect(Advertiser_Name, "WARREN") ~ "Warren",
    str_detect(Advertiser_Name, "BERNIE") ~ "Sanders",
    str_detect(Advertiser_Name, "PETE") ~ "Buttigieg",
    str_detect(Advertiser_Name, "KAMALA") ~ "Harris",
    str_detect(Advertiser_Name, "CORY") ~ "Booker",
    str_detect(Advertiser_Name, "YANG") ~ "Yang",
    str_detect(Advertiser_Name, "TULSI") ~ "Gabbard",
    str_detect(Advertiser_Name, "AMY") ~ "Klobuchar",
    str_detect(Advertiser_Name, "WILLIAMSON") ~ "Williamson",
    str_detect(Advertiser_Name, "BULLOCK") ~ "Bullock",
    str_detect(Advertiser_Name, "JULIAN") ~ "Castro",
    str_detect(Advertiser_Name, "DELANEY") ~ "Delaney",
    str_detect(Advertiser_Name, "MESSAM") ~ "Messam",
    str_detect(Advertiser_Name, "STEYER") ~ "Steyer",
    str_detect(Advertiser_Name, "GILLIBRAND") ~ "Gillibrand",
    str_detect(Advertiser_Name, "BETO") ~ "O'Rourke",
    str_detect(Advertiser_Name, "DE BLASIO") ~ "De Blasio",
    str_detect(Advertiser_Name, "GRAVEL") ~ "Gravel",
    str_detect(Advertiser_Name, "HICKENLOOPER") ~ "Hickenlooper",
    str_detect(Advertiser_Name, "INSLEE") ~ "Inslee",
    str_detect(Advertiser_Name, "MOULTON") ~ "Moulton",
    str_detect(Advertiser_Name, "SWALWELL") ~ "Swalwell",
    str_detect(Advertiser_Name, "RYAN") ~ "Ryan")
}

# Create a function that will add a new candidate column
# to existing tibbles by mapping the ad_to_candidate function

add_candidate_column <- function(data) {
  data %>% 
    filter(str_detect(Advertiser_Name, candidates_pacs)) %>% 
    mutate(candidate = map_chr(Advertiser_Name, ~ad_to_candidate(.x)))
}

# Call the functions on each dataset so that each ad row is 
# assigned to the right candidate 

advertisers <- add_candidate_column(advertisers)
ads <- add_candidate_column(ads)
ad_campaigns <- add_candidate_column(ad_campaigns)
advertisers_weekly_spend <- add_candidate_column(advertisers_weekly_spend)

# CREATE NEW PREDICTOR VARIABLES FOR REGRESSION MODELING

# Create predictor variable: number of ads per week

number_of_ads_weekly <- ads %>% 
  filter(end_week > "2019-01-01") %>% 
  group_by(end_week, candidate) %>% 
  tally() %>% 
  rename(ad_number = n) %>% 
  arrange(end_week, desc(ad_number))

# From ads dataset
# Create variable for ad type: 
# Count ad types and calculate percentage of each
# Vast majority of ads are text, so compare text effectivity to videos and images
# by creating text_ads, a variable denoting percentage of ads per week that are text

# Also create binary variable: below and above 10K impressions, 
# since impressions are only denoted in log 10 buckets
# and the majority are below 10K impressions

ads_data_weekly <- ads %>% 
  filter(end_week > "2019-01-01") %>% 
  group_by(end_week, candidate) %>%
  summarize(text_ads = sum(Ad_Type == "Text") / n(),
            above_10k_impressions = 1 - sum(Impressions == "≤ 10k") / n()) %>% 
  arrange(end_week)

# Create variable for percentage of ads that do age and gender targeting

targeting_weekly <- ad_campaigns %>% 
  filter(end_week > "2019-01-01") %>% 
  group_by(end_week, candidate) %>%
  summarize(age_targeting = 1 - sum(Age_Targeting == "Not targeted") / n(),
            gender_targeting = 1 - sum(Gender_Targeting == "Not targeted") / n()) %>% 
  arrange(end_week)

# FIVETHIRTYEIGHT POLLING DATA

# Read in primary election polling data

primaries <- read_csv("538-polls-data/primaries.csv")

# Create a vector for all the candidates, to be used in filtering
# out candidates who appear in the Google ads dataset

candidates = c("Biden", "Warren", "Sanders", "Buttigieg", "Harris", "Booker",
               "Yang", "Gabbard", "Klobuchar", "Williamson", "Bullock", 
               "Castro", "Delaney", "Messam", "Steyer", "Gillibrand", 
               "O'Rourke", "De Blasio", "Gravel", "Hickenlooper", "Inslee", 
               "Moulton", "Swalwell", "Ryan")

primaries <- primaries %>% 
  select(poll_id, state, pollster, pollster_id, start_date, end_date, answer, candidate_name, party, pct) %>% 
  filter(party == "DEM" & answer %in% candidates)

# Calculate candidates' avg weekly results based on end date of primary polls

primary_weekly_avgs <- primaries %>% 
  mutate(start_week = floor_date(mdy(end_date), "week", week_start = 7)) %>%
  
  # Group polls by the Saturday of each week
  
  mutate(end_week = ceiling_date(mdy(end_date), "week", 
                                 week_start = 6, change_on_boundary = F)) %>%
  arrange(end_week) %>% 
  filter(start_week > "2019-01-01") %>%  
  group_by(start_week, end_week, answer) %>% 
  summarize(avg_pct = mean(pct))

# MERGE DATASETS 

# Use left_join so that data isn't overridden
# First, merge the polling data with ad spend data

poll_ads_weekly <- primary_weekly_avgs %>% 
  left_join(advertisers_weekly_spend, 
            by = c("end_week" = "Week_End_Date", 
                   "start_week" = "Week_Start_Date", 
                   "answer" = "candidate")) %>% 
  arrange(end_week, desc(avg_pct)) %>% 
  select(-c(Advertiser_ID, Advertiser_Name)) %>% 
  filter(end_week < "2019-11-25") %>% 
  
  # Left_join the new predictor variables created above

  left_join(number_of_ads_weekly, 
            by = c("end_week" = "end_week", "answer" = "candidate")) %>% 
  left_join(ads_data_weekly, 
            by = c("end_week" = "end_week", "answer" = "candidate")) %>% 
  left_join(targeting_weekly, 
            by = c("end_week" = "end_week", "answer" = "candidate")) %>% 
  
  # For columns with NA, turn to 0
  
  mutate(Spend_USD = replace_na(Spend_USD, 0),
         ad_number = replace_na(ad_number, 0),
         text_ads = replace_na(text_ads, 0),
         above_10k_impressions = replace_na(above_10k_impressions, 0),
         age_targeting = replace_na(age_targeting, 0),
         gender_targeting = replace_na(gender_targeting, 0)) 

# WRITE CLEAN DATA INTO RDS FILE

write_rds(poll_ads_weekly, "political-ads-app/poll_ads_weekly.rds")
