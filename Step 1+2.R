#First the needed packages are installed

install.packages("data.table")
library(data.table)

#STEP 1:here we open the data and make sure missing data is imported accordingly In the case of observation_Periods
dt<-fread("OBSERVATION_PERIODS.csv")
dt<-dt[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))]

#This is where step 1 begins its needed to remove all entries with less than 1 year follow up before 
#the start of the studydate, hence op_start_date should be before 2016/01/01 or 20160101.
OP_followup<-dt[op_start_date<20160101]
minIday_OP_followup<-OP_followup[op_end_date>20170101]
minIday_OP_followup

# It is probably useful to set the end dates on 20200101, but I'm not sure yet.

#STEP 2 here we open the data of person sand make sure missing data is imported accordingly.
data_persons<-fread("PERSONS.CSV")
data_persons<-data_persons[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))]
data_persons$birth_date <- gsub("\\s", "", paste(data_persons$year_of_birth,data_persons$month_of_birth, data_persons$day_of_birth))


data_persons$age <- 2017- as.numeric(data_persons$year_of_birth)

data_persons[age < 160, ageband := "80+"]
data_persons[age < 80, ageband := "60-79"]
data_persons[age < 60, ageband := "41-60"]
data_persons[age < 40, ageband := "21-40"]
data_persons[age < 20, ageband := "0-19"]

#STEP 3 Merging the data tables Observation Periods with persons on Persons_id
merged_set <- merge(data_persons, minIday_OP_followup, all.x=T) 

#STEP 4 Creating a data table with the Diagnosis event codes and vocabulary

Vocabulary = data.table(
  Diagnosis = c("GBS", "GBS", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "narrow", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible", "possible"),
  event_code = c("357.0", "G61.0", "999.4", "T80.5", "T88.6", "T78.00", "995.60", "T78.05", "995.64", "T78.0", "995.5", "995.6", "T78.01", "995.61", "995.62", "T78.04", "995.63", "995.65", "T78.06", "995.66", "995.67", "T78.08", "995.68", "T78.02", "T78.03", "T78.07", "T78.09", "T78.2XXA", "T78.2XXD", "T78.2XXS", "995.0", "995.69", "T78.3", "995.1", "R23.0", "782.5", "R60.9", "782.3", "T78.4", "T78.40", "I95", "I95.9", "458", "458.9", "R57.9", "785.50", "L50.0", "708.0", "995.3", "995.4", "995.27", "995.2"),
  vocabulary = c("ICD9", "ICD10/CM", "ICD9", "ICD10/CM", "ICD10/CM", "ICD10/CM", "ICD9", "ICD10/CM", "ICD9", "ICD10/CM", "ICD9", "ICD9", "ICD10/CM", "ICD9", "ICD9", "ICD10/CM", "ICD9", "ICD9", "ICD10/CM", "ICD9", "ICD9", "ICD10/CM", "ICD9", "ICD10/CM", "ICD10/CM", "ICD10/CM", "ICD10/CM", "ICD10/CM", "ICD10/CM", "ICD10/CM", "ICD9", "ICD9", "ICD10/CM", "ICD9", "ICD10/CM", "ICD9", "ICD10/CM", "ICD9", "ICD10/CM", "ICD10/CM", "ICD10/CM", "ICD10/CM", "ICD9", "ICD9", "ICD10/CM", "ICD9", "ICD10/CM", "ICD9", "ICD9", "ICD9", "ICD9", "ICD9")
)




#STEP 5 Merge tables vocabulary and Events
EVENTS<-fread("EVENTS.csv")
EVENTS
merge_events <- merge(EVENTS, Vocabulary, all.x=T)
merge_events[, .N, by=.(Diagnosis)] #Count by different diagnoses, add entries in the data table, to get a better count.

#merge_events<-merge_events[Diagnosis=="GBS"]
merge_events<-merge_events[Diagnosis=="possible"]
merge_events<-merge_events[start_date_record>20169999]
merge_events<-unique(merge_events[start_date_record<20210000])
merge_events<-unique(merge_events$person_id)
merge_events[, .N, by=.(meaning_of_event)]
merge_events

#Step6
