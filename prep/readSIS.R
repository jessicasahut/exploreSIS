## readSIS.R ##

  library(dplyr)
  library(car)
  
  sis_full <- read.csv(sis_src, colClasses = c("sis_cl_medicaidNum" = "character"))

# What does the data look like?
  ncol(sis_full)
  nrow(sis_full)
  nlevels(as.factor(sis_full$sis_id))
  nlevels(sis_full$user_id)
  
# Remove columns unnecessary for analysis
  # This removes multiple columns with similar endings using regular expressions
  redact <- sis_full[, -(grep(paste0( "notes" , "$" ) , colnames(sis_full),perl = TRUE) ) ]
  redact <- redact[, -(grep(paste0( "PageNotes" , "$" ) , colnames(redact),perl = TRUE) ) ]
  #redact <- redact[, -( grep(paste0( "nm" , "$" ) , colnames(redact),perl = TRUE) ) ]
  #redact <- redact[, -( grep(paste0( "num" , "$" ) , colnames(redact),perl = TRUE) ) ]
  redact <- redact[, -( grep(paste0( "name" , "$" ) , colnames(redact),perl = TRUE) ) ]
  redact <- redact[, -( grep(paste0( "ext" , "$" ) , colnames(redact),perl = TRUE) ) ]
  redact <- redact[, -( grep(paste0( "addr_line1" , "$" ) , colnames(redact),perl = TRUE) ) ]
  #redact <- redact[, -( grep(paste0( "email" , "$" ) , colnames(redact),perl = TRUE) ) ]
  redact <- redact[, -( grep(paste0( "lang_spoken_cd" , "$" ) , colnames(redact),perl = TRUE) ) ]
  redact <- redact[, -( grep(paste0( "reln_typ_cd" , "$" ) , colnames(redact),perl = TRUE) ) ]
  redact <- redact[, -( grep(paste0( "code" , "$" ) , colnames(redact),perl = TRUE) ) ]
  redact <- redact[, -( grep("_st$" , colnames(redact),perl = TRUE) ) ]
  
  # Remove other unneeded fields
  redact <-
  redact %>% 
    select(-user_id,-sis_modified_dt,
           -sis_cl_ssn,-sis_cl_age,
           -sis_int_position_cd,-sis_int_city,
           -sis_int_zip,-sis_db_create_dt,-sis_other_info,
           -SisStatusChgDte,-LstModUser,              
           -LstModDate,-ReminderDate,
           -SixtyReminderDate,-ThirtyReminderDate,
           -FinalBudget,-LocationId,
           -SitChanged,-FormConfigId,
           -sis_res1_agency,-sis_res2_agency,-sis_res3_agency,-sis_res4_agency,-sis_res5_agency,
           -sis_res6_agency,-sis_res7_agency,-sis_res8_agency,-sis_res9_agency,-sis_res10_agency,
           -sis_archived,-Deleted,-Upload_Info,-sis_cl_county,-Training,-Attachment,
           -Recipient_ContactID,-ReviewStatus)

# Filter Status == Completed ?
  sis <- redact %>% tbl_df %>% filter(Status %in% c("Completed","Completed-Locked"))
  
#  Currently no duplicates (unique(sis$Related_SIS_ID) = NA)
#  Will need to de-dup
  
# Convert all date/time to POSIXct format
  library(lubridate)
  # Remove hms from sis_completed_dt and convert it
    sis$sis_completed_dt <- gsub( " .*$", "", sis$sis_completed_dt)
  # Paste sis_completed_dt and InterviewStartTime, InterviewEndTime
    sis$InterviewStartTime <- as.character(sis$InterviewStartTime)
    sis$InterviewEndTime <- as.character(sis$InterviewEndTime)
  # Convert InterviewStartTime, InterviewEndTime to mdy_hms
    sis$InterviewStartTime <- paste(sis$sis_completed_dt,
                                    sis$InterviewStartTime, sep = " ")
    sis$InterviewEndTime <- paste(sis$sis_completed_dt,
                                    sis$InterviewEndTime, sep = " ")
    
    
    sis$sis_completed_dt <- mdy(sis$sis_completed_dt)

  # Convert DOB
    sis$sis_cl_dob_dt <- gsub( " .*$", "", sis$sis_cl_dob_dt)
    sis$sis_cl_dob_dt <- mdy(sis$sis_cl_dob_dt)
  
# Create calculated time variables
  # Calculate length of interview, "InterviewStartTime" to "InterviewEndTime"
  sis <-
  sis %>%
    mutate(start = lubridate::mdy_hms(InterviewStartTime),
           end = lubridate::mdy_hms(InterviewEndTime),
           duration = as.numeric(difftime(end, start, units = "mins")),
           DaysSince = as.POSIXct(today()) - sis_completed_dt,
           ClientAge = round((sis_completed_dt - sis_cl_dob_dt)/365.242, digits = 1))  # Calculate age at assessment
 
# Calculate max estimated hours per area
  # recode Frequency fields as numeric # of instances / mo
    # 0 = None or Less Than Monthly (Up to 11 Times a Year)
    # 1 = At Least Once a Month, But Not Once a Week
    # 2 = At Least Once a Week, But Not Once a Day (Up to 6 Days a Week)
    # 3 = At Least Once a Day, But Not Once an Hour (At Least 7 Days a Week)
    # 4 = Hourly or More Frequently (24 Hours a Day)
  # recode Daily Support Time fields as numeric max # minutes
    # 0 = None
    # 1 = Less Than 30 Minutes
    # 2 = 30 Minutes to Less Than 2 Hours
    # 3 = 2 Hours to Less Than 4 Hours
    # 4 = 4 Hours or More 

# Create subset of summary variables
  sub_sis <- 
    sis %>%
    select(sis_id, 
           mcaid_id = sis_track_num, # map correct field
           interviewer_orig = sis_int_email,
           interviewer = InterviewerUserId,
           agency = sis_int_agency_nm,
           sis_date = sis_completed_dt,
           start, end, duration, 
           DaysSince, 
           age = ClientAge, gender = sis_cl_sex_cd, 
           LivingSituation, 
           homeliving_std = s1a_Score_Standard,
           homeliving_pct = s1a_Score_Percent,
           commliving_std = s1b_Score_Standard,
           commliving_pct = s1b_Score_Percent,
           hlthsafety_std = s1e_Score_Standard,
           hlthsafety_pct = s1e_Score_Percent,
           lifelearng_std = s1c_Score_Standard,
           lifelearng_pct = s1c_Score_Percent,
           employment_std = s1d_Score_Standard,
           employment_pct = s1d_Score_Percent,
           social_std = s1f_Score_Standard,
           social_pct = s1f_Score_Percent,
           SupportNeedsIndex,
           TotalStandard,
           TotalPercentile,
           # And Section 1
           s1a_1_fqy:s1f_8_for,
           # And Section 2
           s2_1_fqy:s2_Score_Eight_Raw,
           # And Section 3
           s3a_1_support:s3b_Score_Total) %>%
    mutate(mcaid_id = stringr::str_trim(mcaid_id), # trim lead / trail whitespace
           mcaid_id = stringr::str_replace_all(mcaid_id, "[[:alpha:]]", ""), # remove alpha chars
           mcaid_id = ifelse(mcaid_id == "", yes = NA, no = mcaid_id), # blanks to NA
           mcaid_id = ifelse(nchar(as.character(mcaid_id)) > 10,
                             yes = substr(mcaid_id, 
                                          start = nchar(as.character(mcaid_id)) - 9, 
                                          stop = nchar(as.character(mcaid_id))
                             ),
                             no = mcaid_id),
           mcaid_id = ifelse(nchar(as.character(mcaid_id)) < 10,
                             yes = sprintf("%010d", as.integer(mcaid_id)),
                             no = mcaid_id),
           mcaid_id = ifelse(mcaid_id %in% c("        NA","0000000000"), # Make 'NA' & 0000000000 to NA
                             yes = NA,
                             no = mcaid_id),
           mcaid_id = as.factor(mcaid_id)) %>%
    mutate(ABE_std = homeliving_std + commliving_std + hlthsafety_std,
           self_advoc = s2_1_fqy + s2_1_dst + s2_1_tos,
           money_mgmt = s2_2_fqy + s2_2_dst + s2_2_tos,
           no_exploit = s2_3_fqy + s2_3_dst + s2_3_tos,
           legal_resp = s2_4_fqy + s2_4_dst + s2_4_tos,
           participate = s2_5_fqy + s2_5_dst + s2_5_tos,
           legal_srvs = s2_6_fqy + s2_6_dst + s2_6_tos,
           decisions = s2_7_fqy + s2_7_dst + s2_7_tos,
           other_advoc = s2_8_fqy + s2_8_dst + s2_8_tos) %>%
    select(sis_id:TotalPercentile,ABE_std,
           s1a_1_fqy:s1f_8_for,
           s2_1_fqy:s2_Score_Eight_Raw,
           self_advoc:other_advoc,
           s3a_1_support:s3b_Score_Total)

# Make Living Situation Groupings

  # First, we've got to remove the "â€“" characters
    sub_sis$LivingType <- gsub("[^a-zA-Z0-9]","", sub_sis$LivingSituation)

  # Then recode
    sub_sis$LivingType <- recode(sub_sis$LivingType,
                                 "'AdultFosterCarehomecertified' = 'Facility';
                                 'Agencyprovidedresidentialhomewith4to6people' = 'Facility';
                                 'Agencyprovidedresidentialhomewith10ormorepeople' = 'Facility';
                                 'Fosterfamilyhome' = 'Family';
                                 'GeneralresidentialAFCNOTcertified' = 'Facility';
                                 'Homeless' = 'Independent';
                                 'Institutionalsetting' = 'Facility';
                                 'Livingindependentlywithsupports' = 'Independent';
                                 'Livingwithfamily' = 'Family';
                                 'NursingCareFacility' = 'Facility';
                                 'Prisonjailjuveniledetentioncenter' = 'Facility';
                                 'Privateresidencealoneorwithspouseornonrelatives' = 'Independent';
                                 'PrivateresidenceownedbythePIHPCMHSPorprovider' = 'Independent';
                                 'PrivateresidenceownedbyPIHPorProvider' = 'Independent';
                                 'Privateresidencewithfamily' = 'Family';
                                 'Privateresidencewithfamilymembers' = 'Family';
                                 'SpecializedresidentialAFC' = 'Facility';
                                 '' = NA")
    
    sub_sis$LivingType <- as.factor(sub_sis$LivingType)

# Assumes all interviewers current unless defined in local file
  sub_sis <- sub_sis %>% mutate(current_int = TRUE) # stop-gap until file submitted
    
# Get rid on the non-essentials
    
    rm(redact); rm(current)
    