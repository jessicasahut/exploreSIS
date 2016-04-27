## read_attribution.R ##

library(dplyr)

cmh_map <- read.delim(attribution, sep = "|", header = F)

# Transform

cmh_map %<>%
  rename(mcaid_id = V1, cmhsp_id = V2, as_of_dt = V3) %>%
  mutate(mcaid_id = gsub("[^0-9]", "", mcaid_id), # remove non-num
         mcaid_id = sprintf("%010d", as.integer(mcaid_id)), # pad leading zeroes
         mcaid_id = ifelse(grepl("NA",mcaid_id), NA, mcaid_id), #rm coerced NAs
         cmhsp_id = as.character(cmhsp_id),
         cmhsp_nm = car::recode(cmhsp_id,
                                "'1182573' = 'Allegan CMH';
                                '1181773' = 'HealthWest';
                                '1182448' = 'Network 180';
                                '1182009' = 'Ottawa CMH';
                                '1181647' = 'West Michigan CMH';
                                '1181576' = 'Bay Arenac';
                                '1181601' = 'Gratiot';
                                '1181683' = 'Tuscola';
                                '1181709' = 'Central Michigan';
                                '1181718' = 'LifeWays';
                                '1181782' = 'Saginaw';
                                '1181834' = 'The Right Door';
                                '1181862' = 'Shiawassee';
                                '1181923' = 'Huron';
                                '1182045' = 'Montcalm';
                                '1181807' = 'Newaygo';
                                '1182018' = 'CEI';
                                '1182967' = 'CEI';
                                '1182063' = 'Au Sable';
                                '1182134' = 'Barry';
                                '1181807' = 'Newaygo';
                                '1182153' = 'Berrien';
                                '1181816' = 'North Country';
                                '1181853' = 'Northeast';
                                '1182107' = 'Northern Lakes';
                                '1181594' = 'Copper';
                                '1181905' = 'Northpointe';
                                '2813568' = 'Detroit Wayne';
                                '1705289' = 'Oakland';
                                '1181610' = 'Genesee';
                                '1181727' = 'Gogebic';
                                '1182457' = 'Pathways';
                                '1181825' = 'Pines';
                                '1181997' = 'Hiawatha';
                                '1181585' = 'Sanilac';
                                '1181763' = 'Kalamazoo';
                                '1182143' = 'St. Clair';
                                '1181656' = 'Lapeer';
                                '1181979' = 'St. Joseph';
                                '1181736' = 'Lenawee';
                                '1181665' = 'Summit Pointe';
                                '1181871' = 'Livingston';
                                '1181899' = 'Van Buren';
                                '3396315' = 'Macomb';
                                '1181674' = 'Washtenaw';
                                '1182116' = 'Centra Wellness';
                                '1181198' = 'Monroe';
                                '1182125' = 'Woodlands'")
         ) 

sub_sis <- 
sub_sis %>% 
  left_join(cmh_map, by = "mcaid_id") %>%
  select(sis_id:mcaid_id,cmhsp_id:cmhsp_nm,agency,
         interviewer_orig,interviewer,current_int,
         sis_cl_st,
         sis_wk:sis_yrwk,sis_date:LivingType) %>%
  mutate(agency = as.character(agency),
         agency = ifelse(is.na(cmhsp_nm) == TRUE,
                         yes = agency, no = cmhsp_nm))

# Empty Mcaid IDs in SIS
# sum(is.na(sub_sis$mcaid_id)) 
# nlevels(as.factor(sub_sis$sis_id))
# sum(is.na(sub_sis$mcaid_id))/nlevels(as.factor(sub_sis$sis_id))*100