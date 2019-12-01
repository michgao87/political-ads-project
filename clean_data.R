library(fs)
library(infer)
library(purrr)
library(lubridate)
library(ISOweek)
library(broom)
library(ggplot2)
library(tidyverse)

# GOOGLE ADS DATA

# Read in the political ad data sets and filter for US information

ads <- read_csv("google-ads-data/google-political-ads-creative-stats.csv") %>% 
  filter(Regions == "US") %>% 
  select(Ad_Type, Advertiser_ID, Advertiser_Name, Ad_Campaigns_List, Date_Range_Start, Date_Range_End, Num_of_Days, Impressions, Spend_USD, Spend_Range_Min_USD, Spend_Range_Max_USD) %>% 
  mutate(end_week = ceiling_date(Date_Range_Start, "week", week_start = 6, change_on_boundary = F))

advertisers <- read_csv("google-ads-data/google-political-ads-advertiser-stats.csv") %>% 
  filter(Regions == "US") %>% 
  select(Advertiser_ID, Advertiser_Name, Public_IDs_List, Total_Creatives, Spend_USD)

advertisers_weekly_spend <-
  read_csv("google-ads-data/google-political-ads-advertiser-weekly-spend.csv") %>% 
  select(Advertiser_ID, Advertiser_Name, Week_Start_Date, Spend_USD) %>% 
  mutate(Week_End_Date = Week_Start_Date + 6)

# Probably won't use
ad_campaigns <- read_csv("google-ads-data/google-political-ads-campaign-targeting.csv") %>% 
  mutate(end_week = ceiling_date(Start_Date, "week", week_start = 6, change_on_boundary = F))
geo_spend <- read_csv("google-ads-data/google-political-ads-geo-spend.csv")
keywords <- read_csv("google-ads-data/google-political-ads-top-keywords-history.csv")

# Create a vector holding the PACs for the Democratic presidential candidates

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

advertisers <- add_candidate_column(advertisers)
ads <- add_candidate_column(ads)
ad_campaigns <- add_candidate_column(ad_campaigns)
advertisers_weekly_spend <- add_candidate_column(advertisers_weekly_spend)


# FIVETHIRTYEIGHT POLLING DATA

# Read in polling data

primaries <- read_csv("538-polls-data/primaries.csv")

candidates = c("Biden", "Warren", "Sanders", "Buttigieg", "Harris", "Booker",
               "Yang", "Gabbard", "Klobuchar", "Williamson", "Bullock", 
               "Castro", "Delaney", "Messam", "Steyer", "Gillibrand", 
               "O'Rourke", "De Blasio", "Gravel", "Hickenlooper", "Inslee", 
               "Moulton", "Swalwell", "Ryan")

primaries <- primaries %>% 
  select(poll_id, state, pollster, pollster_id, start_date, end_date, answer, candidate_name, party, pct) %>% 
  filter(party == "DEM" & answer %in% candidates)

# Calculate avg results for each candidate based on end date of primary polls
# by day, week, and month

primary_daily_avgs <- primaries %>% 
  mutate(end_date = mdy(end_date)) %>% 
  group_by(end_date, answer) %>%
  summarize(avg_pct = mean(pct)) 

primary_weekly_avgs <- primaries %>% 
  mutate(start_week = floor_date(mdy(end_date), "week", week_start = 7)) %>%
  mutate(end_week = ceiling_date(mdy(end_date), "week", week_start = 6, change_on_boundary = F)) %>%
  # Get the Saturday, last day, of that week
  arrange(end_week) %>% 
  # Keep only 2019
  filter(start_week > "2019-01-01") %>%  
  group_by(start_week, end_week, answer) %>% 
  summarize(avg_pct = mean(pct))

primary_monthly_avgs <- primaries %>% 
  mutate(month = month(mdy(end_date))) %>% 
  group_by(month, end_date, answer) %>% 
  summarize(avg_pct = mean(pct))

# MERGE DATASETS

# Merge datasets by week 

poll_ads_weekly <- primary_weekly_avgs %>% 
  left_join(advertisers_weekly_spend, by = c("end_week" = "Week_End_Date", "start_week" = "Week_Start_Date", "answer" = "candidate")) %>% 
  arrange(end_week, desc(avg_pct)) %>% 
  select(-c(Advertiser_ID, Advertiser_Name)) %>% 
  # If there is no advertiser for that week, make Spend_USD = 0
  mutate(Spend_USD = replace_na(Spend_USD, 0)) 

# Add predictor variable: number of ads per week

number_of_ads_weekly <- ads %>% 
  filter(end_week > "2019-01-01") %>% 
  group_by(end_week, candidate) %>% 
  tally() %>% 
  rename(ad_number = n) %>% 
  arrange(end_week, desc(ad_number))

# Add number of ads to the merged data 

poll_ads_weekly <- poll_ads_weekly %>% 
  left_join(number_of_ads_weekly, by = c("end_week" = "end_week", "answer" = "candidate")) %>% 
  # If there are no ads for that week, make ad_number = 0
  mutate(ad_number = replace_na(ad_number, 0)) 

# Count ad types and calculate percentage
# Vast majority of ads are text; so compare to videos and texts

text_ads_weekly <- ads %>% 
  filter(end_week > "2019-01-01") %>% 
  group_by(end_week, candidate) %>%
  summarize(text_ads = sum(Ad_Type == "Text") / n()) %>% 
  arrange(end_week, desc(text_ads))

poll_ads_weekly <- poll_ads_weekly %>% 
  left_join(text_ads_weekly, by = c("end_week" = "end_week", "answer" = "candidate")) %>% 
  mutate(text_ads = replace_na(text_ads, 0)) 

# Create binary variable: below and above 10K impressions, since that is the finest level of data
impressions_ads_weekly <- ads %>% 
  filter(end_week > "2019-01-01") %>% 
  group_by(end_week, candidate) %>%
  summarize(above_10k_impressions = 1 - sum(Impressions == "≤ 10k") / n()) %>% 
  arrange(end_week, desc(above_10k_impressions))

poll_ads_weekly <- poll_ads_weekly %>% 
  left_join(impressions_ads_weekly, by = c("end_week" = "end_week", "answer" = "candidate")) %>% 
  mutate(above_10k_impressions = replace_na(above_10k_impressions, 0)) 

# Create variable for percentage of ads that do age targeting
age_targeting_weekly <- ad_campaigns %>% 
  filter(end_week > "2019-01-01") %>% 
  group_by(end_week, candidate) %>%
  summarize(age_targeting = 1 - sum(Age_Targeting == "Not targeted") / n()) %>% 
  arrange(end_week, desc(age_targeting))

poll_ads_weekly <- poll_ads_weekly %>% 
  left_join(age_targeting_weekly, by = c("end_week" = "end_week", "answer" = "candidate")) %>% 
  mutate(age_targeting = replace_na(age_targeting, 0)) 

# Create variable for percentage of ads that do gender targeting
gender_targeting_weekly <- ad_campaigns %>% 
  filter(end_week > "2019-01-01") %>% 
  group_by(end_week, candidate) %>%
  summarize(gender_targeting = 1 - sum(Gender_Targeting == "Not targeted") / n()) %>% 
  arrange(end_week, desc(gender_targeting))

poll_ads_weekly <- poll_ads_weekly %>% 
  left_join(gender_targeting_weekly, by = c("end_week" = "end_week", "answer" = "candidate")) %>% 
  mutate(gender_targeting = replace_na(gender_targeting, 0))

# WRITE CLEAN DATA INTO RDS

write_rds(ads, "political-ads-app/ads.rds")
write_rds(advertisers, "political-ads-app/advertisers.rds")
write_rds(advertisers_weekly_spend, "political-ads-app/advertisers_weekly_spend.rds")
write_rds(ad_campaigns, "political-ads-app/ad_campaigns.rds")
write_rds(primaries, "political-ads-app/primaries.rds")
write_rds(poll_ads_weekly, "political-ads-app/poll_ads_weekly.rds")