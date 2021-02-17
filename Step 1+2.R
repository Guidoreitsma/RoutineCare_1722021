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
  Diagnosis = c("GBS", "GBS"),
  event_code = c("357.0", "G61.0"),
  vocabulary = c("ICD9", "ICD10")
)




#STEP 5 Merge tables vocabulary and Events
EVENTS<-fread("EVENTS.csv")
EVENTS
merge_events <- merge(EVENTS, Vocabulary, all.x=T)
merge_events[, .N, by=.(Diagnosis)] #Count by different diagnoses, add entries in the datatable, to get a better count.

merge_events<-merge_events[Diagnosis=="GBS"]
merge_events<-merge_events[start_date_record>20169999]
merge_events

#Step6
