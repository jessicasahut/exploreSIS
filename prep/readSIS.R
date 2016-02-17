# readSIS

# Read in the Supports Intensity Scale (SIS) data extract
  sis_src <- "C:\\Users\\joshh\\Documents\\files\\Projects\\LRP_DataExtract\\data\\sis\\SisExport_131000320475899426%2002152016.csv"
  
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

  
# Recode interviewers

sub_sis$interviewer_orig <- car::recode(sub_sis$interviewer_orig,
                                   "'abrink@miottawa.org' = 'Brink, Adam';
                                   'Alicia.Anderson@Healthwest.net' = 'Anderson, Alicia';
                                   'alicia.anderson@muskegoncmh.org' = 'Anderson, Alicia';
                                   'Alicia.Anderson@muskegoncmh.org' = 'Anderson, Alicia';
                                   'amie.berryhill@healthwest.net' = 'Berryhill, Amie';
                                   'amie.berryhill@muskegoncmh.org' = 'Berryhill, Amie';
                                   'aquigg@miottawa.org' = 'Quigg, A';
                                   'bill.huben@network180.org' = 'Huben, Bill';
                                   'corie.demski' = 'Demski, Corie';
                                   'corie.demski@network180.or' = 'Demski, Corie';
                                   'corie.demski@network180.or' = 'Demski, Corie';
                                   'corie.demski@network180.org' = 'Demski, Corie';
                                   'Corie.Demski@network180.org' = 'Demski, Corie';
                                   'corie.demski@nework180.org' = 'Demski, Corie';
                                   'dmaat@accmhs.org' = 'Maat, Dave';
                                   'faithn@wmcmhs.org' = 'Nekola, Faith';
                                   'faithn@WMCMHS.org' = 'Nekola, Faith';
                                   'jdils@miottawa.org' = 'Dils, Jennifer';
                                   'Jdils@miottawa.org' = 'Dils, Jennifer';
                                   'Jmyers@miottawa.org' = 'Myers, Jennifer';
                                   'lbrink@miottawa.org' = 'Brink, Leah';
                                   'lisa.burlingame@network180.org' = 'Burlingame, Lisa';
                                   'nholtz@aaidd.org' = 'Holtz, Nancy';
                                   'Nora.Barkey@network180.org' = 'Barkey, Nora';
                                   'ptenbrink@miottawa.org' = 'TenBrink, Pam';
                                   'renees@wmcmhs.org' = 'Sporer, Renee';
                                   'sarah.gordon@miottawa.org' = 'Gordon, Sarah';
                                   'scastle@miottawa.org' = 'Castle, Sandy';
                                   'shanna.steinberg@network180.org' = 'Steinberg, Shanna';
                                   'shanna.steinberg@network180.org ' = 'Steinberg, Shanna';
                                   'todd.kamphuis@network180.org' = 'Kamphuis, Todd';
                                   'wscastle@att.net' = 'Castle, Sandy' 
                                   ")


# Recode last modified by

sub_sis$interviewer <- car::recode(sub_sis$interviewer,
                                  "'abrink@miottawa.org' = 'Brink, Adam';
                                  'AdamBrink' = 'Brink, Adam';
                                  'alicia.anderson' = 'Anderson, Alicia';
                                  'Alicia.Anderson@HealthWest.net' = 'Anderson, Alicia';
                                  'Amie.Berryhill@HealthWest.net' = 'Berryhill, Amie';
                                  'amie.berryhill' = 'Berryhill, Amie';
                                  'bill.huben@network180.org' = 'Huben, Bill';
                                  'Corie.Demski@network180.org' = 'Demski, Corie';
                                  'dmaat@accmhs.org' = 'Maat, Dave';
                                  'faithn@wmcmhs.org' = 'Nekola, Faith';
                                  'jdils@miottawa.org' = 'Dils, Jennifer';
                                  'JMyers' = 'Myers, Jennifer';
                                  'lbrink@miottawa.org' = 'Brink Leah';
                                  'lisa.burlingame@network180.org' = 'Burlingame, Lisa';
                                  'Nora.Barkey@network180.org' = 'Barkey, Nora';
                                  'ptenbrink@miottawa.org' = 'TenBrink, Pam';
                                  'renees@wmcmhs.org' = 'Sporer, Renee';
                                  'SandyCastle' = 'Castle, Sandy';
                                  'scastle' = 'Castle, Sandy';
                                  'SGordon' = 'Gordon, Sarah';
                                  'shanna.steinberg@network180.org' = 'Steinberg, Shanna';
                                  'todd.kamphuis@network180.org' = 'Kamphuis, Todd';
                                  'MeganT@wmcmhs.org' = 'Teall, Megan'
                                  ")



# Recode agencies

sub_sis$agency <- car::recode(sub_sis$agency,
                                  "'Allegan CMH / Lakeshore PIHP' = 'Allegan CMH';
                                  'Allegan Community Mental Health' = 'Allegan CMH';
                                  'Allegan Count Community Mental Health' = 'Allegan CMH';
                                  'Allegan County Communty Mental Health' = 'Allegan CMH';
                                  'Health West of Muskeogn County' = 'HealthWest';
                                  'HealthWest of Muskegon County' = 'HealthWest';
                                  'CMH' = 'HealthWest';
                                  'Healthwest' = 'HealthWest';
                                  '12263 James St.' = 'Ottawa CMH';
                                  '12263 JAMES' = 'Ottawa CMH';
                                  'CMHOC' = 'Ottawa CMH';
                                  'Ottawa County CMH' = 'Ottawa CMH';
                                  'OCCMHK' = 'Ottawa CMH';
                                  'Network 180 ' = 'Network 180';
                                  'network180' = 'Network 180';
                                  'Network180' = 'Network 180';
                                  'Nework 180' = 'Network 180';
                                  'West Mi CMH' = 'West Michigan CMH';
                                  'West MI Community Mental Health' = 'West Michigan CMH';
                                  'West Michigan Community Mental Health' = 'West Michigan CMH';
                                  'West Michigan CMHSP' = 'West Michigan CMH'
                                  ")

# Format dates

  sub_sis$sis_wk <- lubridate::week(sub_sis$sis_date)
  sub_sis$sis_yr <- lubridate::year(sub_sis$sis_date)
  sub_sis$sis_yrwk <- lubridate::floor_date(sub_sis$sis_date, unit = "week")
  #sub_sis$sis_yrwk <- sprintf("%04d-%02d", sub_sis$sis_yr, sub_sis$sis_wk)

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

# Mark current SIS Interviewers to allow for filtering
# Requires access to updated list of current interviewers
# Logic easy to break until Interviewer ID/Name are standardized

library(readxl)

current <-
read_excel("C:\\Users\\joshh\\Documents\\files\\Projects\\LRP_DataExtract\\data\\sis\\SIS Assessor List 12-18-15 Lakeshore.xlsx",
           sheet = 2)

names(current)[1] <- "firstname" 
names(current)[2] <- "lastname" 
names(current)[3] <- "pihp" 
names(current)[4] <- "cmh" 
names(current)[6] <- "status" 
names(current)[13] <- "remove_date" 
names(current)[14] <- "remove_rsn" 

current <- current[c(1:15)] 

current <-
  current %>% 
  select(firstname, lastname, pihp, cmh, status, remove_date, remove_rsn) %>%
  mutate(firstname = gsub("^\\s+|\\s+$", "", firstname), #Rm lead/trail whitespace
         lastname = gsub("^\\s+|\\s+$", "", lastname),
         name = paste0(lastname, ", ", firstname)) %>%
  filter(!is.na(firstname) 
         & is.na(remove_date) 
         & is.na(remove_rsn))

current <- as.list(current$name)

sub_sis <- sub_sis %>% mutate(current_int = interviewer %in% current) 

# Get rid on the non-essentials

rm(redact)
rm(current)

# Put it out as .csv
# write.csv(sub_sis,"sub_sis.csv")

# Comments from Daniel re: SIS algorithm
  # VR-12 used with algorithms to predict spending, get Dr. Kazis (Boston University)
  # Using same elements: standard scores for section 1 ABE
  # Raw score from section 3A and 3B
  # Medicaid number not consistent, perhaps due to AIDD changes
  # Include Michigan supplemental questions 1-4, called "ORE"
  # For planning: trying to group score combinations 
  # look at all score combinations to classify needs category:
    # types of combinations (e.g. low ABE, high medical OR hi Behavioral, hi ABE)
  # Developing 2 algorithms: licensed setting v unlicensed setting
    # based on levels of factor for Living Arrangement: res v all else
    # use different algorithm, since we would assume that Medicaid 
    # services would be covering most of the needs included 
  # Display values for ABE, Med and Beh
  # only include values in Longitudinal Health Record 
    # when score > x %ile
  # Assign gradient values based on HSRI-defined ranges for behavioral 
    # and medical sections
  
# Comments from Lisa B.
  # "What type of support would it take to be successful in completing 
  #  this item at the level of someone living in the community?"
  # For "Type of Activity", code "dominant type of support" needed.  
  # Default to higher score in case of lack of clear option between 
  # respondents, though this is rare in practice.
  # "Daily Support Time" score for each day that they do receive supports 
  # (noted in Frequency), includes natural supports, services, other community 
  # resources. 
  # Does "Section 3: Exceptional Medical and Behavioral Support Needs" 
  # influence interpretation of functional areas (e.g. Home Activities, etc.)?  
  # Yes.  Helps set the stage to document medical/behavioral necessity for 
  # functional supports.
  # Section 1 does tend to represent CLS (meaning the definition in the 
  # Medicaid manual). In addition, Section 1 represents Personal Care 
  # (from the definition in Medicaid Manual – such as Home Help or personal 
  # care in licensedsetting, Model Payment).
  # SIS Support Index doesn't include section 2 or section 3 (med/behavioral) 
  # or section 4 (determines highest needs).
  # Manager, look at: 4, 3ab, 2, 1 (index)
  # Order of assessment in practice: 3a, 3b, 4, and then as decided by assessor.  
  # Section 2 generally at end of assessment.
  # All areas important, depending on person.  E.g. if person doesn't want to work.  
  # Can't consistently use absence of "important to" to determine what's 
  # not important to a person.
  # Part of assessor's job is resolving apparent logical contradictions between 
  # various areas of the assessment, e.g. medical needs and functional areas.  
  # AIDD training notes these areas.  Lisa can provide a few examples.
    # Hi – Part A 8 – links to using technology in learning C 4.    
    # Part c. 7 links to E 6 & 7.    All questions that involve transportation link.  
    # Part E 1 links to Behavior 3 B (and maybe Medical).  
    # Part E 8 links to possibly 3A.  
    # Social questions link to each other to some degree 
    # (in household vs out of household). 
    # The making and keeping friends links to some degree about engaging in 
    # a loving and romantic relationship.  C 8 links to Protection and Advocacy 1.   
    # Protection & advocacy 1 & 8 link. 
    # Those are most – but, it requires other judgement sometimes when someone is 
    # responding so totally off that an assessor needs to become even more aware off 
    # links/making sense/painting a picture.
  # For planning: First focus on goals that people want to work on (Important to/for).
  # Medical/behavioral scores are most critical.  These do the most to differentiate people.

# Comments from Katie C.
  # In Ottawa, a team-based approach for CM is guided by this section.  
    # Katie looks at the short report, which is the first 3 pages of the 
    # family friendly report.
  # Long/Family Report are used by SC teams.  Use SIS data for CLS: 
    # keep Excel sheet and scores.
  # Use: Support Needs Index, ABE, Medical/Behavioral
  # Med/Beh usually drives cost, except when natural supports are present.  
    # Helps focus on most severe need.
  # Could look at services not required to provide, and work backwards 
    # to identify people who need those services and not others.
  # Mapping to Services:
    # Medical = Enhanced Health, nursing OT PT speech...is the personal receiving services from MHPs for these needs, where possible?
    # Behavioral = CLS per diem, Personal Care, Psychiatry, high level of in-home CLS.
    # Section 1: Pull out A, B, E score from Section 1 related to CLS
  # Skill-Building mapping is counter-intuitive, since people with lower 
    # need are better candidates for skill-building services.  
    # Consider how to display "Lifelong Learning" and "Employment" sections.
  # Not comfortable using it to drive residential per diems, since SIS doesn't 
    # measure level of intensity of risk.  Also, legal issues can drive placement 
    # (e.g. court-ordered)  
  # Section 2 not used for service referral.  Reviewed at PCP meeting.


