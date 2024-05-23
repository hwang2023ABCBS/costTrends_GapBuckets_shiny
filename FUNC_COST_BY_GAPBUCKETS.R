
# Libraries ---------------------------------------------------------------

library(dplyr)
library(DBI)
library(RODBC)
library(odbc)
library(curl)
library(httr)
library(RCurl)
library(tidyverse)
library(dbplyr)
library(here)
library(data.table)
library(lubridate)





# load data ---------------------------------------------------------------
load(file = 'EnvironObjects.RData')


# set con -----------------------------------------------------------------

con <- dbConnect(odbc::odbc(),
                 "snowflakeodbc",
                 UID = "SFK_SVC_PRD_POSIT")
DBI::dbGetQuery(con, 'USE DATABASE PRD_DATASCIENCE_DB;')
DBI::dbGetQuery(con, 'USE SCHEMA HEALTHECON;')


# access tables -----------------------------------------------------------

# member monthly table that Joe created
Mbr_Monthly_Claims <- tbl(con, in_catalog('PRD_DATASCIENCE_DB','HEALTHECON','MEMBER_MONTHLY_COMPLETE'))



# this is a self-defined function used to subset a specific gap bucket cohort then tie it to costs.
# to check the cost trend respect to gap length. 

func_CostByGapbuckets_subsetting <- function(GapLength_filteredGapBucket_MbrLevel){
  # Get Asthma population, all Asthma gap members
  # library(pins)
  # board <- board_connect()
  # GapLength_MbrGapBucketLevel_AllGaps <- pin_read(board, "hwang/GapLength_MbrGapBucketLevel_AllGaps")
  # 
  # GapLength_filteredGapBucket_MbrLevel <- 
  #   GapLength_MbrGapBucketLevel_AllGaps %>% 
  #   # filter(grepl("Chronic Kidney Disease/DoctorVisit",GAP_CAT_BUCKETED)) 
  #   filter(grepl(GapBucket,GAP_CAT_BUCKETED)) 
  
  
  
  
  Filtered_Gap_Mbrs <- GapLength_filteredGapBucket_MbrLevel %>% pull(MBR_KEY)
  # this is Mbr level aggregated cost (sum of 2023)
  filteredGapBucket_Costs2023 <- Mbr_Monthly_Claims %>% 
    filter(PARTY_KEY %in% Filtered_Gap_Mbrs) %>% 
    filter(MONTH>='2023-01-01' & MONTH<'2024-01-01') %>% 
    group_by(PARTY_KEY,MEMBERID) %>% 
    summarise(across(c(ALLOWED,ALLOWED_PHARM,PAID,PAID_PHARM), sum)) %>% 
    ungroup() %>% 
    collect()
  
  
  
  filteredGapBucket_ExliScores_2021_2022_MbrMonthlyDF <- Mbr_Monthly_Claims %>% 
    filter(PARTY_KEY %in% Filtered_Gap_Mbrs) %>% 
    filter(MONTH>='2021-01-01' & MONTH<'2023-01-01') %>% 
    collect()
  
  
  
  # get Elix scores for the cohort
  source('Elixhauser_Scores.R')
  ElixScore_Readmit <- 
    Score_Elixhauser_Readmit(
      filteredGapBucket_ExliScores_2021_2022_MbrMonthlyDF, MONTH
    ) 
  ElixScore_Mortality <- 
    Score_Elixhauser_Mortality(
      filteredGapBucket_ExliScores_2021_2022_MbrMonthlyDF, MONTH
    )
  
  ElixScores <- merge(ElixScore_Readmit,ElixScore_Mortality, by=c('MEMBERID','MONTH') )
  
  ElixScores_MbrLevel <- ElixScores %>%
    mutate(Elixhuaser_Readmit =ifelse(Elixhuaser_Readmit <0,0,Elixhuaser_Readmit ),
           Elixhuaser_Mortality=ifelse(Elixhuaser_Mortality<0,0,Elixhuaser_Mortality)) %>% 
    group_by(MEMBERID) %>% 
    summarise(Elixhuaser_Readmit=mean(Elixhuaser_Readmit),
              Elixhuaser_Mortality=mean(Elixhuaser_Mortality))
  # this is the member level Exli scores
  
  
  
  # merge all with static table 
  
  
  Merged_final_tbl_filteredGapBucketCohort <- GapLength_filteredGapBucket_MbrLevel %>% 
    left_join(filteredGapBucket_Costs2023, by=c('MBR_KEY'='PARTY_KEY')) %>% 
    left_join(ElixScores_MbrLevel, by=c('MEMBERID')) %>% 
    left_join(Master_Static_distinct %>% select(Mbr_ID,Age,Gender), by=c('MEMBERID'='Mbr_ID'))
  # this is the final table that can be used for visual from modeling..  
  return(Merged_final_tbl_filteredGapBucketCohort)
  
  
  # # tables to be merged- all in member level
  # Asthma_Costs2023 # party key & memberid
  # ElixScores_MbrLevel # memberid only
  # GapLength_AsthmaDocVisits_MbrLevel    # party key
  # Master_Static_distinct   # MbrID & party key
  
  
  
}


# call function to try it on ----------------------------------------------
# GapBucket <- "Chronic Kidney Disease/DoctorVisit"
# CKD_cohort <- func_CostByGapbuckets_subsetting(GapBucket)
# 
# library(marginaleffects)
# library(ggplot2)
# mod_CKD <- lm(ALLOWED~ 
#                    # episode_length
#                    # +Elixhuaser_Mortality+Age
#                    # +Gender
#                    Elixhuaser_Mortality*Age*episode_length
#                  + I(Age^2)
#                  , 
#                  data = CKD_cohort )
# plot_predictions(mod_CKD, condition = 'episode_length')
# THIS FUNCTION WORKS! 




# tables ------------------------------------------------------------------

# static table
# library(pins)
# board <- board_connect()
# Master_Static <- pin_read(board, "dmpettis/Master_Static")
# Master_Static_distinct <- 
#   Master_Static %>% 
#   select(Mbr_ID,Age,Gender)
#   group_by(Mbr_ID) %>% dplyr::slice(1)
  
  

# save & load -------------------------------------------------------------

# save(Master_Static_distinct,
#      file = 'EvrionmentObjs.RData')
