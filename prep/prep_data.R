
#### Aggregate data to allow presentation in open app

source("sis_app/prep/readSIS.R")
source("sis_app/prep/read_attribution.R")

library(dplyr)
library(tidyr)
library(car)

# Define dataset without PHI to use with Shiny Apps 

  # Make an ID key
    mcaid_id <- unique(sub_sis$mcaid_id)
    sis_key <- data.frame(mcaid_id)
    sis_key$fake_id <- sample(x = 100000001:999999999, 
                              size = length(sis_key$mcaid_id), 
                              replace = FALSE)
    sis_key$mcaid_id <- as.character(sis_key$mcaid_id)
    sis_key$fake_id <- as.character(sis_key$fake_id)
    rm(mcaid_id)

  # Make PHI-free dataset
    scrub_sis <-
      sub_sis %>%
      mutate(mcaid_id = as.character(mcaid_id)) %>%
      left_join(sis_key, by = "mcaid_id") %>%
      select(-sis_id, -mcaid_id, -age, -gender)
    
    write.csv(scrub_sis,"sis_app/data/scrub_sis.csv", row.names = F)

#     scrub_sis_all <-
#       sis %>%
#       mutate(sis_id = as.character(sis_id)) %>%
#       left_join(sis_key, by = "sis_id") 
    

#####
# Summary of Section 1 & 2 Type of Service
#####
source("sis_app/prep_tos.R")

##### 
# Summary of Section 3 
#####
  
  section3 <-
    scrub_sis %>%
    select(fake_id, agency, LivingSituation, starts_with("s3")) %>%
    select(fake_id, agency, LivingSituation, ends_with("support")) %>%
    gather(item,score,s3a_1_support:s3b_13_support) %>%
    mutate(subtype = car::recode(item,
                                 "c('s3a_1_support','s3a_2_support','s3a_3_support',
                                 's3a_4_support') = 'Respiratory Care';
                                 c('s3a_5_support','s3a_6_support',
                                 's3a_7_support') = 'Feeding Assistance';
                                 c('s3a_8_support','s3a_9_support') = 'Skin Care';
                                 c('s3a_10_support','s3a_11_support','s3a_12_support',
                                 's3a_13_support','s3a_14_support','s3a_15_support',
                                 's3a_16_support') = 'Other Medical';
                                 c('s3b_1_support','s3b_2_support','s3b_3_support') 
                                 = 'External Destructiveness';
                                 c('s3b_4_support','s3b_5_support','s3b_6_support') 
                                 = 'Self Destructiveness';
                                 c('s3b_7_support','s3b_8_support') = 'Sexual';
                                 c('s3b_9_support','s3b_10_support',
                                 's3b_11_support','s3b_12_support',
                                 's3b_13_support') = 'Other Behavioral'"),
           type = car::recode(subtype,
                              "c('Respiratory Care','Feeding Assistance',
                              'Skin Care','Other Medical') = 'Medical';
                              c('External Destructiveness','Self Destructiveness',
                              'Sexual','Other Behavioral') = 'Behavioral'"),
           name = car::recode(item,
                              "'s3a_1_support' = 'Oxygen therapy';
                              's3a_2_support' = 'Postural drainage';
                              's3a_3_support' = 'Chest PT';
                              's3a_4_support' = 'Suctioning';
                              's3a_5_support' = 'Oral stimulation';
                              's3a_6_support' = 'Tube feeding';
                              's3a_7_support' = 'Parental feeding';
                              's3a_8_support' = 'Positioning';
                              's3a_9_support' = 'Dressing wounds';
                              's3a_10_support' = 'Prevent infection';
                              's3a_11_support' = 'Seizure mgmt';
                              's3a_12_support' = 'Dialysis';
                              's3a_13_support' = 'Ostomy care';
                              's3a_14_support' = 'Transfers';
                              's3a_15_support' = 'Therapy svs';
                              's3a_16_support' = 'Other medical';
                              's3b_1_support' = 'Assault';
                              's3b_2_support' = 'Property destruction';
                              's3b_3_support' = 'Stealing';
                              's3b_4_support' = 'Self injury';
                              's3b_5_support' = 'Pica';
                              's3b_6_support' = 'Suicide attempts';
                              's3b_7_support'  = 'Sexual aggression';
                              's3b_8_support' = 'Inappropriate';
                              's3b_9_support' = 'Outbursts';
                              's3b_10_support' = 'Wandering';
                              's3b_11_support' = 'Substance abuse';
                              's3b_12_support' = 'Mental health tx';
                              's3b_13_support' = 'Other behavioral'"),
           level = car::recode(score,
                               "0 = 'No Support Needed';
                               1 = 'Some Support Needed';
                               2 = 'Extensive Support Needed'")) 
  
  # https://www.ascendami.com/ami/Portals/1/VA%20SIS%20Resources/Supports_Intensity_Scale.pdf
  
  write.csv(section3,"sis_app/data/section3.csv", row.names = F)