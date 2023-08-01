source("libraries.R") # list of packages


### PARAMETERS ###
SURVEY_NAME = "S13" # Survey 13, for example


con <- dbConnect(RSQLite::SQLite(), here("ygdpDB_20210407.db"))
# connect to the local database that contains the new survey data in long format
# dbListTables(con)

### Filter just the responses from the new survey
dataRoughCollect <- tbl(con, "ratings")
dataRoughCollect <- filter(dataRoughCollect, surveyID == SURVEY_NAME) 
dataRoughCollect <- dataRoughCollect %>% collect()
dataRoughCollect <- subset(dataRoughCollect, select = -c(questionID, updateID) )

### Long to wide conversion using pivot_wider
dataRoughCollect <- dataRoughCollect %>% pivot_wider(names_from = sentenceID, values_from = rating)

### Merge ratings with other necessary tables in the database
dataRoughCollect = merge(dataRoughCollect, collect(tbl(con, "demo_geo")), by="responseID")
cities <- subset(collect(tbl(con, "cities")), select = -c(updateID) )

dataRoughCollect = merge(dataRoughCollect, cities, by.x="currentCityID", by.y="cityID", suffixes="current")
dataRoughCollect = rename(dataRoughCollect, "currentCityName" = "cityName", "currentStateID" = "stateID", "currentStateName" = "stateName", "currentCountyName" = "countyName", "currentCountryID" = "countryID", "currentLat" = "lat", "currentLong" = "long", "currentPostalCode" = "postalCode")
dataRoughCollect = merge(dataRoughCollect, cities, by.x="raisedCityID", by.y="cityID", suffixes="raised")
dataRoughCollect = rename(dataRoughCollect, "raisedCityName" = "cityName", "raisedStateID" = "stateID", "raisedStateName" = "stateName", "raisedCountyName" = "countyName", "raisedCountryID" = "countryID", "raisedLat" = "lat", "raisedLong" = "long", "raisedPostalCode" = "postalCode")
dataRoughCollect = merge(dataRoughCollect, cities, by.x="motherCityID", by.y="cityID", suffixes="mother")
dataRoughCollect = rename(dataRoughCollect, "motherCityName" = "cityName", "motherStateID" = "stateID", "motherStateName" = "stateName", "motherCountyName" = "countyName", "motherCountryID" = "countryID", "motherLat" = "lat", "motherLong" = "long", "motherPostalCode" = "postalCode")
dataRoughCollect = merge(dataRoughCollect, cities, by.x="fatherCityID", by.y="cityID", suffixes="father")
dataRoughCollect = rename(dataRoughCollect, "fatherCityName" = "cityName", "fatherStateID" = "stateID", "fatherStateName" = "stateName", "fatherCountyName" = "countyName", "fatherCountryID" = "countryID", "fatherLat" = "lat", "fatherLong" = "long", "fatherPostalCode" = "postalCode")

dataRoughCollect = merge(dataRoughCollect, collect(tbl(con, "tech")), by="responseID")
dataRoughCollect <- dataRoughCollect %>% add_column(langs = "english")

spokenLangs <- collect(tbl(con, "spoken_langs"))
for (i in 1800:2197) { # generalize indices
  for (j in 1:1086) {
    if (spokenLangs$responseID[i] == dataRoughCollect$responseID[j]) {
      dataRoughCollect$langs[j] <- paste(dataRoughCollect$langs[j], ", ", spokenLangs$language[i], sep = "")
    }
  }
}

# Don't worry about the warnings here
dataRoughCollect = merge(dataRoughCollect, collect(tbl(con, "dialect_regions")), by.x="raisedCityID", by.y="cityID", all.x=T)
dataRoughCollect = merge(dataRoughCollect, collect(tbl(con, "CENSUS_URBAN_AREAS")), by.x="raisedCityID", by.y="cityID", all.x=T)
dataRoughCollect = merge(dataRoughCollect, collect(tbl(con, "CENSUS_COUNTY_DEMO")), by.x="raisedCityID", by.y="cityID", all.x=T)


# Set up final table (called cleanTable)
cleanTable <- read.csv("headers.csv")

for (i in 1:length(dataRoughCollect[[1]])) {
  cleanTable[nrow(cleanTable) + 1, ] <- NA
}

# Begin data fill

ENTRY_LENGTH = 1086
OBJECTID_STARTINDEX = 5002
SURVEY = 13
NUM_QUESTIONS = 45
OBJECTID_2_STARTINDEX = 5222

cleanTable$OBJECTID_12 = OBJECTID_STARTINDEX : (OBJECTID_STARTINDEX+length(dataRoughCollect[[1]])-1)
cleanTable$Join_Count = rep(1)
cleanTable$TARGET_FID = OBJECTID_STARTINDEX : (OBJECTID_STARTINDEX+length(dataRoughCollect[[1]])-1)
# JOIN COUNT 1
cleanTable$TARGET_FID_1 = OBJECTID_STARTINDEX : (OBJECTID_STARTINDEX+length(dataRoughCollect[[1]])-1)
cleanTable$Survey = rep(SURVEY)

cleanTable$Age = dataRoughCollect$age
cleanTable$Gender = dataRoughCollect$gender
cleanTable$Education = dataRoughCollect$education
cleanTable$Income = dataRoughCollect$income
cleanTable$Race = dataRoughCollect$race
cleanTable$RaisedCity = dataRoughCollect$raisedCityName 
cleanTable$RaisedStat = dataRoughCollect$raisedStateName 
cleanTable$Latitude = dataRoughCollect$raisedLat 
cleanTable$Longitude = dataRoughCollect$raisedLong 
cleanTable$RaisedYear = dataRoughCollect$raisedYears 



for (i in 18:240) {
  cleanTable[i] = rep(0)
}

for (j in 7:(7+NUM_QUESTIONS-1)) {
  present <- FALSE
  for (i in 18:240) {
    if (colnames(cleanTable)[i] == paste("F", colnames(dataRoughCollect)[j], sep="")) {
      cleanTable[i] = dataRoughCollect[j]
      present <- TRUE
    }
  }
  if (!present) {
    cleanTable[colnames(dataRoughCollect)[j]] <- dataRoughCollect[j]
    cleanTable <- cleanTable %>% relocate(colnames(dataRoughCollect)[j], .before=personid)
  }
}

cleanTable$personid = dataRoughCollect$responseID
for (i in 1:ENTRY_LENGTH) {
  cleanTable$personid[i] = paste(cleanTable$personid[i], "Survey", SURVEY)
}
cleanTable$responseID = dataRoughCollect$responseID
cleanTable$nLangs = dataRoughCollect$nLangs
cleanTable$Langs = dataRoughCollect$langs

cleanTable$MoCity = dataRoughCollect$motherCityName
cleanTable$MoState = dataRoughCollect$motherStateName
cleanTable$MoCountry = dataRoughCollect$motherCountyName
cleanTable$MoLon = dataRoughCollect$motherLong
cleanTable$MoLat = dataRoughCollect$motherLat

cleanTable$FaCity = dataRoughCollect$fatherCityName
cleanTable$FaState = dataRoughCollect$fatherStateName
cleanTable$FaCountry = dataRoughCollect$fatherCountyName
cleanTable$FaLon = dataRoughCollect$fatherLong
cleanTable$FaLat = dataRoughCollect$fatherLat

cleanTable$CuCity = dataRoughCollect$currentCityName
cleanTable$CuState = dataRoughCollect$currentStateName
cleanTable$TimeCu = dataRoughCollect$currentYears
cleanTable$CuLon = dataRoughCollect$currentLong
cleanTable$CuLat = dataRoughCollect$currentLat

cleanTable$Id = rep(0)

cleanTable$NAME = dataRoughCollect$raisedCityName
cleanTable$STATE_NAME = dataRoughCollect$raisedStateName
cleanTable$STATE_FIPS = dataRoughCollect$STATE_FIPS
cleanTable$CNTY_FIPS = dataRoughCollect$CNTY_FIPS
cleanTable$FIPS = dataRoughCollect$FIPS

for (i in 0:51) {
  cleanTable[[342 + i ]] = dataRoughCollect[[i+134]]
}

cleanTable$UACE10 = dataRoughCollect$UACE10
cleanTable$GEOID10 = dataRoughCollect$GEOID10
cleanTable$NAME10 = dataRoughCollect$NAME10
cleanTable$NAMELSAD10 = dataRoughCollect$NAMELSAD10
cleanTable$LSAD10 = dataRoughCollect$LSAD10
cleanTable$MTFCC10 = dataRoughCollect$MTFCC10
cleanTable$UATYP10 = dataRoughCollect$UATYP10
cleanTable$FUNCSTAT10 = dataRoughCollect$FUNCSTAT10
cleanTable$ALAND10 = dataRoughCollect$ALAND10
cleanTable$AWATER10 = dataRoughCollect$AWATER10
cleanTable$INTPTLAT10 = dataRoughCollect$INTPTLAT10
cleanTable$INTPTLON10 = dataRoughCollect$INTPTLON10


for (i in 1:ENTRY_LENGTH) {
  cleanTable$Age_Bin[i] = AGEBIN(cleanTable$Age[i])
}

cleanTable$StatusResp = rep("IP Address")
cleanTable$IPaddress = dataRoughCollect$ipAddress
cleanTable$DurationSe = dataRoughCollect$durationSeconds
cleanTable$Finished = dataRoughCollect$finished

for (i in 1:ENTRY_LENGTH) {
  if (dataRoughCollect$finished[i] == "True") {
    cleanTable$Finished[i] = 1
  } else {
    cleanTable$Finished[i] = 0
  }
}

cleanTable$UserLang = dataRoughCollect$userLanguage
cleanTable$Consent = dataRoughCollect$consent

for (i in 1:ENTRY_LENGTH) {
  if (cleanTable$Gender[i] != "male" && cleanTable$Gender[i] != "female") {
    cleanTable$Gender[i] = "Other"
    cleanTable$GenderOthe[i] = dataRoughCollect$gender[i]
  }
}

cleanTable$Join_Count_1 <- rep(1)
for (i in 1:ENTRY_LENGTH) {
  if (is.na(cleanTable$NAME10[i])) {
    cleanTable$Join_Count_1[i] <- 0
  }
}

for (i in 1:ENTRY_LENGTH) {
  counter <- 0
  for (j in 18:282) {
    if (cleanTable[[j]][i] != "0") {
      counter <- counter + 1
    }
  }
  cleanTable$NumberJudg[i] <- counter
}

NUM_SENTENCES = 252
sentences <- collect(tbl(con, "sentences"))
colnames = colnames(cleanTable)

# Heads up- this loop takes a couple of minutes time
for (i in 1:ENTRY_LENGTH) {
  CG_MIN = "NA"
  CU_MAX = "NA"
  for (j in 18:282) {
    for (k in 1:NUM_SENTENCES) {
      if (sentences$sentenceID[k] == colnames[j]) {
        if (sentences$sentenceType[k] == "CG") {
          if (CG_MIN == "NA" || cleanTable[[j]][i] < CG_MIN) {
            CG_MIN = cleanTable[[j]][i]
          }
        }
        if (sentences$sentenceType[k] == "CU") {
          if (CU_MAX == "NA" || cleanTable[[j]][i] > CU_MAX) {
            CU_MAX = cleanTable[[j]][i]
          }
        }
      }
      #continue
    }
    cleanTable$CG_Min[i] = CG_MIN
    cleanTable$CU_Max[i] = CU_MAX
  }
}

cleanTable$StartDate = dataRoughCollect$dateTimeStart
cleanTable$EndDate = dataRoughCollect$dateTimeEnd
cleanTable$RecordedDa = dataRoughCollect$recordedDate

cleanTable$GenderOthe = rep("NA")
cleanTable$RaceOther = rep("NA")

for (i in 1:ENTRY_LENGTH) {
  if (dataRoughCollect$gender[i] != "male" && dataRoughCollect$gender[i] != "female") {
    cleanTable$GenderOthe[i] = dataRoughCollect$gender[i]
    cleanTable$Gender[i] = "Other"
  } else {
    cleanTable$Gender[i] <- str_to_title(dataRoughCollect$gender[i])
  }
}

cleanTable$Shape__Are = dataRoughCollect$Shape_Area
cleanTable$Shape__Len = dataRoughCollect$Shape_Length

cleanTable$OBJECTID_1 = rep(0)



cleanTable$StartDate = dataRoughCollect$dateTimeStart
cleanTable$EndDate = dataRoughCollect$dateTimeEnd

for (misc_col in c("Min_Max_Di", "CG_Score", "CU_Score", "C_Sum_Disc", "Controls_D", "Completion", "Geography_", "Discard_",  'Input_FID', 'Row_Labels','Region_Nam' ,'SHAPE_Leng', 'Dialect_Re', 'Shape_Le_1', 'Region', 'Sub_Region', 'SubSub_Reg', 'OBJECTID_2', 'OBJECTID')) {
  cleanTable[misc_col] = rep("NA")
}
cleanTable$RecordedDa<-dataRoughCollect$recordedDate

write.csv(cleanTable, "survey13data.csv", row.names=TRUE)



# ----------- view some db tables -------------
# constructions <- collect(tbl(con, "constructions"))
# sentences <- collect(tbl(con, "sentences"))
# comments <- collect(tbl(con, "comments"))
# questions <- collect(tbl(con, "questions"))
# responses <- collect(tbl(con, "responses"))
# ratings <- collect(tbl(con, "ratings"))
# surveys <- collect(tbl(con, "surveys"))
# survey_comments <- collect(tbl(con, "survey_comments"))
# dialect_regions <- collect(tbl(con, "dialect_regions"))
# update_metadata <- collect(tbl(con, "update_metadata"))
# variables <- collect(tbl(con, "variables"))
# version_history <- collect(tbl(con, "version_history"))