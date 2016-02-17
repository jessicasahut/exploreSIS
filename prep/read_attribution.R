## read_attribution.R ##

library(dplyr)

cmh_map <- read.delim("C:\\Users\\joshh\\Documents\\files\\Projects\\LRP_DataExtract\\data\\attribution\\ZTSAttributionFile_Region3_20151130_11_10_OUTPUT.TXT",
                      sep = "|", header = F)

# Transform

cmh_map %<>%
  rename(mcaid_id = V1, cmhsp_id = V2, as_of_dt = V3) %>%
  mutate(mcaid_id = sprintf("%010d", as.integer(mcaid_id)), 
         # pad leading zeroes on mcaid id
         mcaid_id = ifelse(mcaid_id == "        NA", NA, mcaid_id), 
         # make NAs
         cmhsp_id = as.character(cmhsp_id),
         cmhsp_nm = car::recode(cmhsp_id,
                                "'1182573' = 'Allegan CMH';
                                '1181773' = 'HealthWest';
                                '1182448' = 'Network 180';
                                '1182009' = 'Ottawa CMH';
                                '1181647' = 'West Michigan CMH'")
         ) 

sub_sis <- 
sub_sis %>% 
  left_join(cmh_map, by = "mcaid_id") %>%
  select(sis_id:mcaid_id,cmhsp_id:cmhsp_nm,agency,
         interviewer_orig,interviewer,current_int,
         sis_wk:sis_yrwk,sis_date:LivingType) %>%
  mutate(agency = as.character(agency),
         agency = ifelse(is.na(cmhsp_nm) == TRUE,
                         yes = agency, no = cmhsp_nm))

# Empty Mcaid IDs in SIS
# sum(is.na(sub_sis$mcaid_id)) 
# nlevels(as.factor(sub_sis$sis_id))
# sum(is.na(sub_sis$mcaid_id))/nlevels(as.factor(sub_sis$sis_id))*100