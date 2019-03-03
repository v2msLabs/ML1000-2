# main script
# Clean all variables that might be left by other script to avoid collusion
rm(list=ls(all=T))
source('utils.R') # supplementary code
library(dplyr) # 
library(tm) # text mining

# Initial observation of the data shows that there is a number of features which do not present
# any analytical value namely: incident_id, incident_url, source_url, 
# state_house_district, state_senate_district, sources, incident_url_fields_missing and
# congressional_district. We are going to remove them
#
# We also drop participant_age in favour of participant_age_group. The age group 
# is more suatable for categarization and has less missing data. 
# Common sense  dictates that the missing data may be defaulted to 18+ without stretch
# 
# paricipant_name and participant_relationship miss a lot of data , 50% and 93% respectively. 
# The names of the individuals would not give us much and impossible to impute. 
# The participant relationship is practically absent. Thus we drop both
#
# The data set has two features that describe crime scene: notes and incident-characteristics.
#  These are free format features that a are hard to parse...?? Notes feature misses about 33% of data
# The incident characteristics one is almost fully popluated. Let's keep them for now
#
# In addition to the said above we can drop address column in favour of the geo coordinates
# (latitude and longitude). But before we get rid of the address feature let's try to use address data
# to impute missing coordinates. This process is knows as forward geocoding. 
# There are 3075 missing coordinates, which is about 5% of the whole population
#
# So let's start with geocoding procedure first. Then we will drop redundant features
# We will employ openCage API which offers a trial account. the trial account allows to issue
# 2500 requests per day ith the speed 1 request per second or slower
#
# NOTE: the code below submitted for illustration purpose only. due to the free account restriction
# it took 2 days to impute missing coordinate data.
# the resulting structure was saved into the file for further processing


# !!!! Imputation of coordinates has been complete; no need to run the code again !!!!
# ==========================================================================================
 # imputeCoordinates();
# ==============================================================================================
#
# read data with imputed longitude and latitude
data = read.csv("../data/gun-violence-with-all-coord.csv", header = T, 
                na.strings = c("NA","","#NA"),sep=",")
data$date = as.Date(as.character(data$date),"%Y-%m-%d")

# now let's remove redundant features discussed earlier
data = subset(data, select = c(-incident_id, -incident_url, -source_url, 
  -state_house_district, -state_senate_district,-sources, -incident_url_fields_missing,
  -congressional_district, -address, -participant_age, -participant_name,
  -participant_relationship))

summary(data)
# Remaining features could be split into a few groups
# 
# Participant related
#
# particpant_gender, participant_type, participant_age_group and participant_status
# give us full picture about the offenders, victims and the assult outcome
#
# Weapons used
#
# gun_type, gun_stolen and n_guns_involved
#
# Geographical Location
#
# state, city_or_county, latitude, longitude and location_description.
#
# Crime outcome
#
# n_killed, n_injured
#
# Descriptive features
#
# notes, incident_characteristics
# =====================================================================================
#
# Let review feature groups one by one
#
# =====================================================================================
#
#   Participant Group
#
# The participant-related columns have the following pattern: [idx1::value1||idx2::value2]
# where idx is a number from 0 to N and value is an alphanumeric piece of information
# All participant_* columns describe respective attribute of each individual found
# on the crime scene
# It is feasible to parse each column data resulting in a structured presentation of the content
# in the form of a named vector, for example a participent type could be structured as follows:
#
#        0                 1                 2                 3                 4 
# "Victim"          "Victim" "Subject-Suspect" "Subject-Suspect"          "Victim" 
#
# Having the structured presentation of the data we shall ask ourselves how we record it for 
# further analisys.
#
# One approach is to add more features that would describe each participant. We are talking
# about 4 features per participant. This approach would make our data set is very wide and very sparse.
#
#  The second approach is to come up with the features that would provide more insight into each
# case. Let's consider this approach.
#
# participant_gender investigation
participantGender = data %>%  mutate(text = trimws(gsub('\\|\\||::|Unknown'," ",participant_gender, fixed = F))) %>% 
  filter(text != "0" ) %>% select(text)
participantGender = VCorpus(VectorSource(participantGender))
# clean text
participantGender  = tm_map(participantGender , removePunctuation)
participantGender  = tm_map(participantGender , removeNumbers)
# create documnet term matrix
genderTermMatrix = tm::DocumentTermMatrix(participantGender)
# count frequent words
tm::findFreqTerms(genderTermMatrix, 10)
# The following new categorical feature will describe gender of various groups of participants
# victim_gender - gender of the victims, factor
# suspect_gender - gender of suspects, factor
# =================================================================
#
#     Gender Codes
#
# 0 - no info
# 1 - male  
# 2 - female
# 3 - male dominated group
# 4 - femail dominated goup
#
# =================================================================

# participant type. we can get number of victims and suspects from this column to cacluclate all victim - suspect stats
participantType = data %>%  mutate(text = trimws(gsub('\\|\\||::|Unknown'," ",participant_type, fixed = F))) %>% 
  filter(text != "0" ) %>% select(text)
participantType = VCorpus(VectorSource(participantType))
# create document term matrix
typeTermMatrix = tm::DocumentTermMatrix(participantType)
# count frequent words
tm::findFreqTerms(typeTermMatrix, 10)
# New features
# n_victims - number
# n_suspects - number
#
# participan age group
participantAgeGroup = data %>%  mutate(text = trimws(gsub('\\|\\||::|Unknown'," ",participant_age_group, fixed = F))) %>% 
  filter(text != "0" ) %>% select(text)
participantAgeGroup = distinct(participantAgeGroup)

# Data analysis (age_groups) revealed three groups: Adult 18+, Teen 12-17, Child 0-11
# Let's add two, age-related features:
# victim_age_group - age group of victims, factor  
# suspect_age_group - age group of suspects, factor
# ==================================================================
# 
#     Age Group Codes
#
# 0 - no info
# 1 - all adults
# 2 - children/ teens
# 3 - adults and children/ teens . Adults make majority
# 4 - adults and children/ teens. Chlidren/ teens make majority
#
# ==================================================================
#
#  Crime Casualties
#
# we take both columns as is (n_killed, n_injured) If there is an NA  we can analize synthetic features
# (see participant related topic) to fill the missing values 
# paricipant status
praticipantStatus = data %>% distinct(participant_status)
# Gives us 2 features:
# n_victim_killed - number of victims killed, numerical
# n_victim_injured - number of victims injured, numerical

#
#  Weapon used
# gun type
gunTypes = data %>% distinct(gun_type)
# Gun type has variety of guns used, where unknown, handgun, shotgun, auto and AK-74. we can use word frequency
# calculators to enumerate the entries and pick the most frequently used for categarization.
gunTypes = data %>%  mutate(text = trimws(gsub('\\|\\||::|Unknown'," ",gun_type, fixed = F))) %>% filter(text != "0" ) %>% select(text)
gunTypes = VCorpus(VectorSource(gunTypes))
# clean text
gunTypes  = tm_map(gunTypes , removePunctuation)
gunTypes  = tm_map(gunTypes , removeNumbers)
# create documnet term matrix
gunTypeTermMatrix = tm::DocumentTermMatrix(gunTypes)
# count frequent words
tm::findFreqTerms(gunTypeTermMatrix, 10)
# Our investigation resulted in the following new feature:
# gun_type_involved - factor
# =================================================================
#
#     Gun type Codes
#
# 0 - unknown 
# 1 - handgun
# 2 - shotgun/ rifle
# 3 - automatic
# 4 - mix/other 
#
#  gun origin
gunOrigin = data %>% distinct(gun_stolen)
gunOrigin = data %>%  mutate(text = trimws(gsub('\\|\\||::|Unknown'," ",gun_stolen, fixed = F))) %>% filter(text != "0" ) %>% select(text)
gunOrigin = VCorpus(VectorSource(gunOrigin))
# clean text
gunOrigin  = tm_map(gunOrigin , removePunctuation)
gunOrigin  = tm_map(gunOrigin , removeNumbers)
# create documnet term matrix
gunOriginTermMatrix = tm::DocumentTermMatrix(gunOrigin)
# count frequent words
tm::findFreqTerms(gunOriginTermMatrix, 10)
# Based on our data research we will create the following new feature:
# gun_origin - gun origin, factor
# =================================================================
#
#     Gun Origin Codes
#
# 0 - unknown
# 1 - all stolen
# 2 - all acquired legally
# 3 - mix of stolen and legal guns
#
# ================================================================

# Geographical location
#
# We take latitude, longitude, state and city/county as is
# location_description is a useful feature that categrize locations of being
# public/ private
# school, hospital, mall, street, etc. Let's examine it further..
locations = VCorpus(VectorSource(data$location_description))
# clean text
locations = tm_map(locations, removeWords, stopwords())
locations = tm_map(locations, removePunctuation)
locations = tm_map(locations, removeNumbers)
# create document term matrix
locTermMatrix = tm::TermDocumentMatrix(locations, control = list(tokenize = BigramTokenizer))
locWordMatrix =  tm::DocumentTermMatrix(locations)
# count frequent bi-grams
tm::findFreqTerms(locTermMatrix, 30)
# count frequent words
tm::findFreqTerms(locWordMatrix, 30)
# Based on our finding we will add another new feature:
#
# place_type -  factor
#
# =================================================================
#
#     Place Type Codes
#
# 0 - unknown
# 1 - school/ university/ college
# 2 - community center/ shopping center/ hospital/ church
# 3 - home invasion
# 4 - street/drive by
# 5 - other public places
#
#
# Descriptive features
#
# notes is a free format description of a crime scene. 33% of data is missing. The rest is unique data. 
# only data mining technique could help us to get the sentiment from te notes. but again it is unclear how to use
# the extracted data for clustering. since we cannot use the feature we get rid of it
#
# The incident_characteristics on the other hand is pratcically fully populated and better structured. 
# Let' investigate the data this feature has...
characteristics = data %>% distinct(incident_characteristics)
#
# about 20% of data contain "Shot - Wounded/Injured"
# 10% of "Dead (murder, accidental, suicide)
# 8% - "Shots Fired - No Injuries"
# 2.6% - "Shot - Wounded/Injured||Drive-by (car to street, car to car) "
# 1.5% -  "Non-Shooting Incident||Drug involvement||ATF/LE Confiscation/Raid/Arrest||Possession (gun(s) 
#  found during commission of other crimes)"
#
# At first glance 40% of data contained in top 5 categories do not add to the knowledge we can collect from other
# features. Let's take a second look
#
# remove || and /
characteristics = data %>%  mutate(text = gsub('\\|\\||/'," ",incident_characteristics, fixed = F)) %>% select(text)
characteristics = VCorpus(VectorSource(characteristics))
# remove stop words, puntctuation and number
#characteristics = tm_map(characteristics, removeWords, stopwords() ) 
characteristics = tm_map(characteristics, removePunctuation)
characteristics = tm_map(characteristics, removeNumbers)
# create document term matrix
charTermMatrix = tm::TermDocumentMatrix(characteristics, control = list(tokenize = TrigramTokenizer))
# count frequent bi-grams, which apear at least 2000
tm::findFreqTerms(charTermMatrix, 1000)
# In fact thie feature could be used to create an ew feature that provide additional insights and support location
# identification. 
# Firstly laet's add a new featues:
# incident_type - factor.
# =================================================================
#
#     Incident Type Codes
#
# 0 - unknown
# 1 - accidental
# 2 - defensive use
# 3 - armded robbery
# 4 - suicide
# 5 - raid/ arrest/ warrant
# 6 - domestic violence
# 7 - gun brandishing, flourishing, open demonstration 
#
# ================================================================
# is_drug_alcohol - factor (0/1) if drugs alcohol were involved
# as mentioned above along with the location_description this feature is useful to identify the place type.
#
# Date
#
# It seems reasonable to add a day of the week and month to try to detect any seasonal patterns 
# month - factor
# day_of_week, factor



# @TODO: plot charTermMatrix$dimnames$Terms

#
# Summary of new features
# =======================================
# victim_gender -  factor
# suspect_gender -  factor
# victim_age_group - factor  
# suspect_age_group - factor
# n_victim_killed - number 
# n_victim_injured - number 
# n_victims - number
# n_suspects - number
# n_arrested - number
# gun_type_involved - factor
# gun_origin -  factor
# place_type -  factor
# incident_type - factor
# is_drug_alcohol - factor 
# month - factor
# day_of_week, factor
# =======================================

# as discussed let's remove notes
data = subset(data, select = c(-notes))

for (idx in 1:nrow(data)) {
 date = data[idx,"date"]
 data[idx,"month"] = as.numeric( format(date, "%m")) 
 data[idx,"day_of_week"] = as.numeric( format(date, "%u")) 
 # parse participant gender  
 participantGender = parseFeature(data[idx,"participant_gender"])  
 # parse participant type
 participantType = parseFeature(data[idx,"participant_type"])  
 # parse participant age group
 participantAgeGroup = parseFeature(data[idx,"participant_age_group"])  
 # parse participant status
 participantStatus = parseFeature(data[idx,"participant_status"])  
 # parse gun origin
 gunOrigin = data[idx,"gun_stolen"]  
 # parse gun type
 gunType = data[idx,"gun_type"]  
 # assign other features of interest to variables to optimize performance and simplify the reference
 numKilled = data[idx,"n_killed"]
 numInjured = data[idx,"n_injured"]
 numGuns = data[idx,"n_guns_involved"]
 characteristics = data[idx,"incident_characteristics"]
 locationDecription  = data[idx,"location_description"]

 # default new, participant-related  features
 data[idx,"victim_gender"]  = 0
 data[idx,"suspect_gender"]  = 0 
 data[idx,"victim_age_group"]  = 0 
 data[idx,"suspect_age_group"]  = 0 
 data[idx,"n_victim_killed"]  = 0 
 data[idx,"n_victim_injured"]  = 0 
 data[idx,"n_victims"]  = 0 
 data[idx,"n_suspects"]  = 0
 data[idx,"n_arrested"] = 0
 # process participant-related data
 if (!all(is.na(participantType))) {
   victims = participantType[which(participantType %in% c("Victim"))]
   suspects = participantType[which(participantType %in% c("Subject-Suspect"))]
   
   n_victims = length(victims)
   n_suspects = length(suspects)
   data[idx,"n_victims"] = n_victims
   data[idx,"n_suspects"] = n_suspects

   if (n_victims > 0) {
     
     injured = 0
     killed = 0
     female = 0
     male = 0
     child = 0
     teen = 0
     adult = 0
     
     for (i in names(victims)){
       ## outcome
       status = participantStatus[i]
       if ( !isNaOrNull(status) ) {
         if (str_count(status,fixed("injured", ignore_case = T)) > 0) {
           injured = injured + 1
         } else if (str_count(status,fixed("killed", ignore_case = T)) > 0) {
           killed = killed + 1
         } 
       }   
       # gender
       gen = participantGender[i]
       if (!isNaOrNull(gen)) {
         if (str_count(gen,regex("^male", ignore_case = T)) > 0) {
           male = male + 1
         } else if (str_count(gen,regex("^female", ignore_case = T)) > 0) {
           female = female + 1
         }
       }
       # age group
       age = participantAgeGroup[i]
       if (!isNaOrNull(age)) {
         if (str_count(age,fixed("teen", ignore_case = T)) > 0) {
           teen = teen + 1
         } else if (str_count(age,fixed("adult", ignore_case = T)) > 0) {
           adult = adult + 1
         } else if (str_count(age,fixed("child", ignore_case = T)) > 0) {
           child = child + 1
         } 
       }
     }
     
     # summarize
     data[idx,"n_victim_killed"] = killed
     data[idx,"n_victim_injured"] = injured
     if ((child > 0|| teen > 0) && adult  == 0) {
       data[idx,"victim_age_group"] = 2
     } else if ( (child > 0|| teen > 0) && adult >0){
       data[idx,"victim_age_group"] = ifelse(child + teen >= adult, 4, 3)
     } else if (adult > 0) {
       data[idx,"victim_age_group"] = 1   
     }
     
     
     if (female > 0 && male > 0) {
       data[idx,"victim_gender"] = ifelse(female > male, 4, 3)
     } else if (male > 0) {
       data[idx,"victim_gender"] = 1
     } else if (female > 0) {
       data[idx,"victim_gender"] = 2
     }
     
   }
 
   if (n_suspects >0) {
     arrested = 0
     female = 0
     male = 0
     child = 0
     teen = 0
     adult = 0  
     for (i in names(suspects)){
       ## outcome
       status = participantStatus[i]
       if ( !isNaOrNull(status) && str_count(status,fixed("arrested", ignore_case = T)) > 0) {
         arrested = arrested + 1
       }  
       # gender
       gen = participantGender[i]
       if (!isNaOrNull(gen)) {
         if (str_count(gen,regex("^male", ignore_case = T)) > 0) {
           male = male + 1
         } else if (str_count(gen,regex("^female", ignore_case = T)) > 0) {
           female = female + 1
         }
       }
       # age group
       age = participantAgeGroup[i]
       if (!isNaOrNull(age)) {
         if (str_count(age,fixed("teen", ignore_case = T)) > 0) {
           teen = teen + 1
         } else if (str_count(age,fixed("adult", ignore_case = T)) > 0) {
           adult = adult + 1
         } else if (str_count(age,fixed("child", ignore_case = T)) > 0) {
           child = child + 1
         } 
       }
     }
     
     # summarize
     data[idx,"n_arrested"] = arrested
     
     if ((child > 0|| teen > 0) && adult  == 0) {
       data[idx,"suspect_age_group"] = 2
     } else if ( (child > 0|| teen > 0) && adult >0){
       data[idx,"suspect_age_group"] = ifelse(child + teen >= adult, 4, 3)
     } else if (adult > 0) {
       data[idx,"suspect_age_group"] = 1   
     }
     
     if (female > 0 && male >0) {
       data[idx,"suspect_gender"] = ifelse(female > male, 4, 3)
     } else if (male > 0) {
       data[idx,"suspect_gender"] = 1
     } else if (female > 0) {
       data[idx,"suspect_gender"] = 2
     }   
   }
 }
 
 if ( isNaOrNull(numKilled) ) {
   data[idx,"n_killed"] = ifelse(data[idx,"n_victim_killed"] > 0,data[idx,"n_victim_killed"],0 )
 }
 
 if ( isNaOrNull(numInjured) ) {
   data[idx,"n_injured"] = ifelse(data[idx,"n_victim_injured"] > 0,data[idx,"n_victim_injured"],0 )
 }
 
 
 # process gun-related data
 data[idx,"gun_type_involved"]  = 0 
 data[idx,"gun_origin"]  = 0

  # gun origin
 if ( !isNaOrNull(gunOrigin) ) {
   stolen = str_count(gunOrigin,fixed("stolen", ignore_case = T))
   notStolen = str_count(gunOrigin,fixed("notstolen", ignore_case = T))
   if (stolen > 0 && notStolen > 0) {
     data[idx,"gun_origin"] = 3
   } else if (stolen > 0){
     data[idx,"gun_origin"] = 1
   } else if (notStolen > 0){
     data[idx,"gun_origin"] = 2
   }
 }

 shotgun = 0
 handgun = 0
 auto = 0
 other = 0
 if ( !isNaOrNull(gunType) ) { 
   shotgun = str_count(gunType,regex("shotgun|rifle", ignore_case = T))
   auto = str_count(gunType,regex("auto|ak-", ignore_case = T))
   handgun = str_count(gunType,regex("handgun|spl", ignore_case = T))
   other = str_count(gunType,regex("gauge|mag|other|rem|spr|win", ignore_case = T)) 
   
   if (other == 0 && shotgun == 0 && auto == 0 && handgun > 0 ) {
     data[idx,"gun_type_involved"]  = 1
   } else if (shotgun > 0 && auto == 0 && handgun == 0 && other == 0) {
     data[idx,"gun_type_involved"]  = 2
   } else if (shotgun == 0 && auto > 0 && handgun == 0 && other == 0) {
     data[idx,"gun_type_involved"]  = 3
   } else if (shotgun > 0 || auto > 0 || handgun> 0 || other > 0) {
     data[idx,"gun_type_involved"]  = 4
   } 
   
 }
 
 if ( isNaOrNull(numGuns) ) {
   data[idx,"n_guns_involved"] = shotgun + handgun + auto + other
 }
 
 # process location and characteristics 
 data[idx,"place_type"]  = 0 
 data[idx,"incident_type"]  = 0
 data[idx,"is_drug_alcohol"] = 0

 if ( !isNaOrNull(locationDecription) ) { 
   if (str_count( locationDecription,regex("school|university|college", ignore_case = T)) > 0){
     data[idx,"place_type"]  = 1
   } else if (str_count( locationDecription,regex("music hall|community|dollar|convenience store|circle k|church|food mart|medical center|shopping center|mall|hospital", ignore_case = T)) > 0) {
     data[idx,"place_type"]  = 2
   } else  if (str_count( locationDecription,regex("apartment|back yards|apartments|mobile home", ignore_case = T)) > 0) {
     data[idx,"place_type"]  = 3
   } else  if (str_count( locationDecription,regex("car", ignore_case = T)) > 0) {
     data[idx,"place_type"]  = 4
   }else  if (str_count( locationDecription,regex("village|neighborhood|bar|park|gas station|grill|east garfield|west garfield|waffle house|jail|taco bell|police department|bank|club|restaurant|pizza|inn|hotel|office", ignore_case = T)) > 0) {
     data[idx,"place_type"]  = 5
   }
 }

 if ( !isNaOrNull(characteristics) ) {
   if (str_count( characteristics,regex("accidental shooting|accidental suicide|murder accidental|injury accidental|accidental negligent|negligent discharge", ignore_case = T)) > 0){
     data[idx,"incident_type"]  = 1
   } else if (str_count( characteristics,regex("defensive use|use defensive|dgu", ignore_case = T)) > 0) {
     data[idx,"incident_type"]  = 2
   } else if (str_count( characteristics,regex("armed robbery|robbery with", ignore_case = T)) > 0) {
     data[idx,"incident_type"]  = 3
   } else if (str_count( characteristics,regex("suicide shot|suicide nonshooting|suicide officer", ignore_case = T)) > 0) {
     data[idx,"incident_type"]  = 4
   } else if (str_count( characteristics,regex("arrest|raid|warrant", ignore_case = T)) > 0) {
     data[idx,"incident_type"]  = 5
   } else if (str_count( characteristics,regex("domestic violence", ignore_case = T)) > 0) {
     data[idx,"incident_type"]  = 6
   } else if (str_count( characteristics,regex("brandishing|flourishing|open carry", ignore_case = T)) > 0) {
     data[idx,"incident_type"]  = 7
   }
   # impute place_type if required and possible
   if (data[idx,"place_type"] == 0) {
     if (str_count( characteristics,regex("home invasion", ignore_case = T)) > 0) {
       data[idx,"place_type"]  = 3
     } else if (str_count( characteristics,regex("car to car|car to street|drive-by|car shot", ignore_case = T)) > 0) {
       data[idx,"place_type"]  = 4
     } else if (str_count( characteristics,regex("business|institution group|club|bar", ignore_case = T)) > 0) {
       data[idx,"place_type"]  = 5
     } 
   }
   # if drugs/alcohol
   data[idx,"is_drug_alcohol"] = ifelse(str_count( characteristics,regex("drug|atf|alcohol|under influence",
                                  ignore_case = T)) > 0, 1, 0)   
     
 }   

}  

# test = subset(data, select = c(gun_type, gun_type_involved,gun_stolen, gun_origin))
# test2 = subset(data, select = c(location_description, incident_characteristics,place_type,
#                                incident_type, is_drug_alcohol))
# test3 = subset(data, select = c(participant_type, n_victims, n_suspects,n_arrested, participant_age_group, 
#                                victim_age_group, suspect_age_group,  participant_gender, victim_gender, suspect_gender,
#                                participant_status,n_victim_killed, n_victim_injured, n_killed,n_injured ))
# drop redundant features
data = subset(data, select = c(-participant_age_group, -participant_type,
      -participant_gender, -participant_status, -location_description, 
      -incident_characteristics, -gun_stolen,-gun_type))
# save engineered data set 
# write.csv(data,'../data/gun-violence-engineered.csv', row.names = F)


summary(data)
# Clean all variables that might be left by other script to avoid collusion
rm(list=ls(all=T))
# implement models