#############################
#                           #
##  PreProcessing PMS data ##
#############################
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

########################## Declare workspace and load data ###########################

library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))# Set working directory to current directory

dataDir = "Z:/shares/ghepmk_data/2020_Kappen_PMS//"
# dateDir = "24082021//"
dateDir = "06102021//"

DataFrame <- as.data.frame(read.csv(file = paste0(dataDir, dateDir,"results-survey987313.csv"), head = TRUE, sep=",",  stringsAsFactors=FALSE))
# DataFrameCOMPLETE <- as.data.frame(read.csv(file = paste0(dataDir, dateDir,"allPMSdata.csv"), head = TRUE, sep=",",  stringsAsFactors=FALSE))

DataFrame <- select(DataFrame, -c(lastpage, startlanguage, startdate, datestamp, IFC1, IFC2, IFC3, IFC4, IFC5, IFC6, auto1.SQ001., AgeValidation.SQ001., AgeValidation.SQ002., interviewtime, groupTime17, IFC1Time, IFC2Time, IFC3Time, IFC4Time, IFC5Time, IFC6Time, auto1Time, groupTime13, GenderTime, AgeTime, MenstruationTime, FirstMenstrualTime, RegularMentrualTime, MenopauseTime, PregnantTime, PostPregnantTime, ContraceptiveTime, DutchTime, HormonesTime, MentalTime, LaptopTime, MenstrualToelichtTime, CurrentMensesTime, MenstrualStartTime, MenstrualEndTime, MenstrualEndExpectedTime, MenstrualDurationTime, AgeValidationTime, EMailTime, groupTime16, SymptomsTime, DisturbanceTime, SymptomsPRETime, groupTime14, RRSTime, groupTime15, DASS21Time )) #Remove columns with irrelevant data

isala = DataFrame$Isala[DataFrame$Isala != ""]
### Clean data --> remove all rows without submitdate, since that means they didnt complete the screening
DataFrame <- DataFrame[!(is.na(DataFrame$submitdate) | DataFrame$submitdate==""), ]

substrRight <- function(x, n){ # A function that takes the last n characters of a string
  substr(x, nchar(x)-n+1, nchar(x))}

DataFrame <- DataFrame[-c(which(DataFrame$ï..id == 2684)), ] # remove this one participant - not included because slipped through

DataFrameClean <- data.frame()
DataFrameClean <- select(DataFrame, c(ï..id, Age, FirstMenstrual, MenstrualStart, MenstrualEnd, MenstrualEndExpected, MenstrualDuration, Isala))

### Get total score per questionnaire
#Symptoms.PST 1-14
#Disturbance.PST A-E? 1-5
#RRS.R 1-22
#DASS21.DAS 1-21
################ Linking Excel file to assess A-B/B-A distribution #################
# ExcelPMS <- as.data.frame(read.csv(file = paste0(dataDir, dateDir,"Participant-Excel.csv"), head = TRUE, sep=";",  stringsAsFactors=FALSE))
ExcelPMS <- as.data.frame(read.csv(file = paste0(dataDir, dateDir,"Participant-Excel.csv"), head = TRUE, sep=",",  stringsAsFactors=FALSE))

library(tidyr)
ExcelPMS <- ExcelPMS %>% mutate_all(~ replace_na(.x, "")) # Replace all NA's with empty lines. This makes the code make better sense

library(dplyr)
# Add a column with the actual testing moments so we can verify whether they did it on time
ExcelPMS$TrueFollicular[ExcelPMS$Test.gemist == "TRUE"] = ExcelPMS$Nieuwe.folliculaire.fase[ExcelPMS$Test.gemist == "TRUE"] 
ExcelPMS$TrueFollicular[ExcelPMS$Test.gemist == ""] = ExcelPMS$folliculaire.fase[ExcelPMS$Test.gemist == ""]

ExcelPMS$TrueLuteal[ExcelPMS$Test.gemist == "TRUE"] = ExcelPMS$Nieuwe.luteale.fase[ExcelPMS$Test.gemist == "TRUE"]
ExcelPMS$TrueLuteal[ExcelPMS$Test.gemist == ""] = ExcelPMS$luteale.fase[ExcelPMS$Test.gemist == ""] 

Randomisatie <- select(ExcelPMS, Entry.nummer, email, ï..Participantnummer, Randomisatie, Exclusie, TrueFollicular, TrueLuteal, duur.cyclus)

# Trim the email addresses because some have whitespace at the end
DataFrame$EMail <- trimws(DataFrame$EMail)
Randomisatie$email <- trimws(Randomisatie$email)

DataFrame$testVolgorde = ''
DataFrame$participantID = ''
for (i in 1:nrow(DataFrame)){ # Loop over all participant rows that filled out screening completely
  # loc = which(Randomisatie$Entry.nummer == DataFrame$ï..id[i]) # Check for location of their entry number in the participant Excel file
  loc = which(Randomisatie$email == DataFrame$EMail[i]) # Check for location of their entry number in the participant Excel file
  
  if (length(loc) == 0) {
    print(paste0("Something going on with participant ",toString(DataFrame$ï..id[i])," AKA entrynumber " ))
  } else {
    DataFrame$testVolgorde[i] = Randomisatie$Randomisatie[loc] # Use this location to grab their randomisation and participantNumber allocated
    DataFrame$participantID[i] = Randomisatie$ï..Participantnummer[loc]
    DataFrame$Exclusie[i] = Randomisatie$Exclusie[loc]
    DataFrame$TrueFollicular[i] = Randomisatie$TrueFollicular[loc]
    DataFrame$TrueLuteal[i] = Randomisatie$TrueLuteal[loc]
    
    if (DataFrameClean$MenstrualDuration[i] != Randomisatie$duur.cyclus[loc]){ # If the durations of cycle don't match, this often means the participant didnt understand the question properly. So overwrite this with manual data entered after correspondance with participant
      DataFrameClean$MenstrualDuration[i] = Randomisatie$duur.cyclus[loc]
    }
  }
}

# Add to Clean Dataframe
Order <- DataFrame$testVolgorde
participantNo <- DataFrame$participantID
Exclusie <- DataFrame$Exclusie
TrueFollicular <- DataFrame$TrueFollicular
TrueLuteal <- DataFrame$TrueLuteal
DataFrameClean <- cbind(DataFrameClean, participantNo, Order, Exclusie, TrueFollicular, TrueLuteal)

########################## Symptoms ###########################
SymptomsData <- DataFrame[ , grepl( "Symptoms.PST" , names( DataFrame ) ) ] # Make dataset with only Symtoms variables
SymptomsData <- cbind(SymptomsData, Symptoms.PST04=c(DataFrame$Symptoms.SPST04))


SymptomsData <- SymptomsData[,c(1,2,3,14,4,5,6,7,8,9,10,11,12,13)]

allSymptoms = 0

for(i in 1:nrow(DataFrame)) { # loop through participants
  SymptomsScore <- 0
  for(t in 1:ncol(SymptomsData)){ # loop through questions
    temp = as.numeric(substrRight(unlist(SymptomsData[t])[i],1)) # Take value i (participant) from SymptomsDATA, unlist, then take last character and turn it into a number (double)
    SymptomsScore <- SymptomsScore + temp
  }
  allSymptoms[i] <- SymptomsScore
}
# print(allSymptoms) # allSymptoms consists of all total Symptoms scores per participant

DataFrameClean <- cbind(DataFrameClean, allSymptoms)


########################## Disturbance ###########################
DisturbanceData <- DataFrame[ , grepl( "Disturbance.PST" , names( DataFrame ) ) ] # Make dataset with only Disturbance variables
allDisturbance = 0

for(i in 1:nrow(DataFrame)) { # loop through participants
  DisturbanceScore <- 0
  for(t in 1:ncol(DisturbanceData)){ # loop through questions
    temp = as.numeric(substrRight(unlist(DisturbanceData[t])[i],1)) # Take value i (participant) from DisturbanceDATA, unlist, then take last character and turn it into a number (double)
    DisturbanceScore <- DisturbanceScore + temp
  }
  allDisturbance[i] <- DisturbanceScore
}
# print(allDisturbance) # allDisturbance consists of all total Disturbance scores per participant

DataFrameClean <- cbind(DataFrameClean, allDisturbance)

########################## RRS ###########################
RRSData <- DataFrame[ , grepl( "RRS.R" , names( DataFrame ) ) ] # Make dataset with only RRS variables
allRRS = 0

for(i in 1:nrow(DataFrame)) { # loop through participants
  RRSScore <- 0
  for(t in 1:ncol(RRSData)){ # loop through questions
    temp = as.numeric(substrRight(unlist(RRSData[t])[i],1)) # Take value i (participant) from RRSDATA, unlist, then take last character and turn it into a number (double)
    RRSScore <- RRSScore + temp
  }
  allRRS[i] <- RRSScore
}
# print(allRRS) # allRRS consists of all total RSS scores per participant

DataFrameClean <- cbind(DataFrameClean, allRRS)

########################## DASS ###########################
DASSData <- data.frame(DASS.Total = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1), DASS.Stress = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1), DASS.Anxiety = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1), DASS.Depresh = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1))
DASSDataframe <- DataFrame[ , grepl( "DASS21.DAS" , names( DataFrame ) ) ] # Make dataset with only DASS variables

for(i in 1:nrow(DataFrame)) { # loop through participants
  DASSScore <- 0
  DASSStress <- 0
  DASSAnxiety <- 0
  DASSDepresh <- 0
  for(t in 1:ncol(DASSDataframe)){ # loop through questions
    temp = as.numeric(substrRight(unlist(DASSDataframe[t])[i],1)) # Take value i (participant) from DisturbanceDATA, unlist, then take last character and turn it into a number (double)
    DASSScore <- DASSScore + temp
    if (t == 1 | t == 6 | t == 8 | t == 11 | t == 12 | t == 14 | t == 18){
      DASSStress <- DASSStress + temp
    } else if (t == 2 | t == 4 | t == 7 | t == 9 | t == 15 | t == 19 | t == 20) {
      DASSAnxiety <- DASSAnxiety + temp
    } else if (t == 3 | t == 5 | t == 10 | t == 13 | t == 16 | t == 17 | t == 21){
      DASSDepresh <- DASSDepresh + temp
    }
  }
  DASSData$DASS.Total[i] <- DASSScore
  DASSData$DASS.Stress[i] <- DASSStress
  DASSData$DASS.Anxiety[i] <- DASSAnxiety
  DASSData$DASS.Depresh[i] <- DASSDepresh
}

DataFrameClean <- cbind(DataFrameClean, DASSData)

####### Distinguish patients from non-patients based on questionnaire scores #######

SymptomsDataNums <- data.frame(matrix(ncol = ncol(SymptomsData), nrow = nrow(SymptomsData))) #Make empty dataframe to put in 'raw' symptomscores (e.g. 3 instead of L403)
colnames(SymptomsDataNums) <- colnames(SymptomsData)  #Give this new dataframe the same colnames as the original Symptomsdata dataframe

# Make better DataFrame
for (b in 1:ncol(SymptomsData)){   #loop over all columns (b refers to 'width' = 'breedte' = amount of columns)
  for (h in 1:nrow(SymptomsData)){   #within each column, loop over all rows = participants (h refers to height = amount of rows)
    SymptomsDataNums[h,b] <- as.numeric(substrRight(unlist(SymptomsData[h,b]),1)) # in each cell, fill in the last digit (e.g. 3) of the corresponding cell in SymptomsData (e.g. L403)
  }
}

#same logic as with SymptomsDataNums
DisturbanceDataNums <- data.frame(matrix(ncol = ncol(DisturbanceData), nrow = nrow(DisturbanceData)))
colnames(DisturbanceDataNums) <- colnames(DisturbanceData)

for (b in 1:ncol(DisturbanceData)){
  for (h in 1:nrow(DisturbanceData)){
    DisturbanceDataNums[h,b] <- as.numeric(substrRight(unlist(DisturbanceData[h,b]),1))
  }
}


PMSScore <- 0 #initialize variable

for (i in 1:nrow(DataFrame)){ #loop over all participants
  req1 = 0 #initialize variables
  req2 = 0
  req3 = 0
  # Requirement 1
  if (sum(SymptomsDataNums[i,1:4] == 4)>0){ #if there's one or more values within the first four columns(=questions) of SymptomsDataNums that are equal to 4
    req1 = 2  #then requirement 1 gets value 2
  } else if (sum(SymptomsDataNums[i,1:4] == 3)>0){ #if there's one or more values within the first four columns of SymptomsDataNums that are equal to 3
    req1 = 1 #then requirement 1 gets value 1
  } else { #if there's no values within the first four columns that are equal to 3 or more
    req1 = 0 #then requirement 1 gets value zero
  }
  
  # Requirement 2
  if (sum(SymptomsDataNums[i,] >= 3) >= 5){ #if there are 5 or more values in SymptomsDataNums that are equal to three or more
    req2 = 2 #then requirement 2 gets value 2
  } else { #if not,
    req2 = 0 #requirement 2 gets value 0
  }
  
  # Requirement 3
  if (sum(DisturbanceDataNums[i,] == 4)>0){ #if there's one or more values within the five columns(=questions) of DisturbanceDataNums that are equal to 4
    req3 = 2 #then requirement 3 gets value 2
  } else if (sum(DisturbanceDataNums[i,] >= 3)>0){ #if there's one or more values within the first four columns of DisturbanceDataNums that are equal to 3
    req3 = 1 #then requirement 3 gets value 1 
  } else { #if there's not one value within these five columns that is equal to 3 or more
    req3 = 0 #then requirement 3 gets value 0
  }
  
  #Give each participant a PMSScore
  if (req1 == 2 && req2 == 2 && req3 == 2){ #if the value of req 1 =2, req 2 = 2 and req 3 = 2
    PMSScore[i] <- 2 #then that participant gets PMSScore 2 --> PMDD
  } else if (req1 == 0 | req2 == 0 | req3 == 0){ #if the value of one of the requirements is equal to 0
    PMSScore[i] <- 0 #then that participant gets PMSScore 0 --> no PMS
  } else { #in all other cases...
    PMSScore[i] <- 1 #...the participant gets PMSScore 1 --> PMS
  }
}


PMSData <- cbind(PMSScore, DataFrame) #Add columns with PMSScore to DataFrame

DataFrameClean <- cbind(DataFrameClean, PMSScore)

########################### VISUALIZATION ###########################


### visualization ingredients

library(ggplot2)


## AGE

barplot(table(PMSData$Age), col = 'orange', main = 'Age', ylab = 'frequency', )

# ggplot(data=PMSData, aes(x=Age)) +
#   geom_bar(stat="count", color="black", fill = "steelblue",)

## CONTRACEPTION
#make dataset with info about contraception
ContraData <- data.frame(PMSData$Contraceptive.SQ001., PMSData$Contraceptive.SQ002., PMSData$Contraceptive.SQ003., PMSData$Contraceptive.SQ004., PMSData$Contraceptive.other.) #make dataframe with 'contraceptive' columns only
names(ContraData) <- c("pill", "hor. coil", "cop. coil", "natural", "other") #give new names to these columns

ContraData$Overview[ContraData$`pill` == 'Y'] = 'Pill'
ContraData$Overview[ContraData$`hor. coil` == 'Y'] = 'Hor. Coil'
ContraData$Overview[ContraData$`cop. coil` == 'Y'] = 'Cop. Coil'
ContraData$Overview[ContraData$`natural` == 'Y'] = 'Natural'
ContraData$Overview[ContraData$other != ''] = 'other'
ContraData$Overview <- as.factor(ContraData$Overview)

Contraception <- ContraData$Overview
DataFrameClean <- cbind(DataFrameClean, Contraception)

# barplot(table(ContraData$Overview), col = 'red', main = 'Contraception Type', ylab = 'frequency') #as aanpassen

plot <- ggplot(data=ContraData, aes(x=Overview)) +
  geom_bar(stat="count", color = "black", fill = "orangered3") +
  xlab("Contraception Type")
plot
## PMS

plot <- ggplot(data=PMSData, aes(x=PMSScore)) +
  geom_bar(stat="count", color = "black", fill = "green4") +
  xlab("PMSScore") 
plot
## QUESTIONNAIRES

#Symptoms.PST + Disturbance.PST + RRS.R + DASS21.DAS

SymptomsDF <- as.data.frame(allSymptoms)
DisturbanceDF <- as.data.frame(allDisturbance)
RRSDF <- as.data.frame(allRRS)
DASSDF <- as.data.frame(DASSData$DASS.Total)

library("yarrr")

plotVariable <- (numeric(nrow(SymptomsDF))+1)

SymptomsPlot <- pirateplot(formula = allSymptoms ~ plotVariable,
                           data = SymptomsDF,
                           theme = 3,
                           main = "Symptoms")

DisturbancePlot <- pirateplot(formula = allDisturbance ~ plotVariable,
                           data = DisturbanceDF,
                           theme = 3,
                           main = "Disturbance")

RRSPlot <- pirateplot(formula = allRRS ~ plotVariable,
                           data = RRSDF,
                           theme = 3,
                           main = "RRS")

DASSPlot <- pirateplot(formula = DASSData$DASS.Total ~ plotVariable,
                           data = DASSDF,
                           theme = 3,
                           main = "DASS")

#dataframe maken
#eerst dataframe aanmaken met juiste dimensies en namen geven aan rijen en kolommen
#Dan 1 voor 1 de juiste som aan de juiste cel koppelen

### check where sums go wrong
#DataFrame <- cbind(DataFrame, PMSData$PMSScore)
#library(dplyr)
#SubsetAB1 <- DataFrame[which(DataFrame$`PMSData$PMSScore` == 1 & DataFrame$testVolgorde == "A-B"), ]
#SubsetBA2 <- DataFrame[which(DataFrame$`PMSData$PMSScore` == 2 & DataFrame$testVolgorde == "B-A"), ]

# Some people responded something else than 'other' when using Nuvaring, so should be set to other (however it seems like they are already set to 'other' so probably changed in Excel file)

DataFrameClean$Contraception[DataFrame$EMail == 'axxxboels@gmail.com'] = 'other' # axxxboels@gmail.com
DataFrameClean$Contraception[DataFrame$EMail == 'lorerobeyns@hotmail.com'] = 'other' # lorerobeyns@hotmail.com


ABData <- data.frame(matrix(ncol = 6, nrow = 3))
x <- c("Pill", "Hor. coil", "Cop.coil", "Natural", "Other", "RowTotal")
y <- c("No PMS", "PMS", "ColTotal")
colnames(ABData) <- x
rownames(ABData) <- y

ABData[1,1] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore == 0 & DataFrameClean$Contraception == 'Pill'] == "A-B")
ABData[1,2] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore == 0 & DataFrameClean$Contraception == 'Hor. Coil'] == "A-B")
ABData[1,3] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore == 0 & DataFrameClean$Contraception == 'Cop. Coil'] == "A-B")
ABData[1,4] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore == 0 & DataFrameClean$Contraception == 'Natural'] == "A-B")
ABData[1,5] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore == 0 & DataFrameClean$Contraception == 'other'] == "A-B")
ABData[1,6] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore == 0] == "A-B")


ABData[2,1] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore != 0 & DataFrameClean$Contraception == 'Pill'] == "A-B")
ABData[2,2] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore != 0 & DataFrameClean$Contraception == 'Hor. Coil'] == "A-B")
ABData[2,3] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore != 0 & DataFrameClean$Contraception == 'Cop. Coil'] == "A-B")
ABData[2,4] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore != 0 & DataFrameClean$Contraception == 'Natural'] == "A-B")
ABData[2,5] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore != 0 & DataFrameClean$Contraception == 'other'] == "A-B")
ABData[2,6] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore != 0] == "A-B")



ABData[3,1] <- sum(DataFrameClean$Order[DataFrameClean$Contraception == 'Pill'] == "A-B")
ABData[3,2] <- sum(DataFrameClean$Order[DataFrameClean$Contraception == 'Hor. Coil'] == "A-B")
ABData[3,3] <- sum(DataFrameClean$Order[DataFrameClean$Contraception == 'Cop. Coil'] == "A-B")
ABData[3,4] <- sum(DataFrameClean$Order[DataFrameClean$Contraception == 'Natural'] == "A-B")
ABData[3,5] <- sum(DataFrameClean$Order[DataFrameClean$Contraception == 'other'] == "A-B")
ABData[3,6] <- sum(DataFrameClean$Order == "A-B")




BAData <- data.frame(matrix(ncol = 6, nrow = 3))
x1 <- c("Pill", "Hor. coil", "Cop.coil", "Natural", "Other", "RowTotal")
y1 <- c("No PMS", "PMS", "ColTotal")
colnames(BAData) <- x1
rownames(BAData) <- y1


BAData[1,1] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore == 0 & DataFrameClean$Contraception == 'Pill'] == "B-A")
BAData[1,2] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore == 0 & DataFrameClean$Contraception == 'Hor. Coil'] == "B-A")
BAData[1,3] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore == 0 & DataFrameClean$Contraception == 'Cop. Coil'] == "B-A")
BAData[1,4] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore == 0 & DataFrameClean$Contraception == 'Natural'] == "B-A")
BAData[1,5] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore == 0 & DataFrameClean$Contraception == 'other'] == "B-A")
BAData[1,6] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore == 0] == "B-A")


BAData[2,1] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore != 0 & DataFrameClean$Contraception == 'Pill'] == "B-A")
BAData[2,2] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore != 0 & DataFrameClean$Contraception == 'Hor. Coil'] == "B-A")
BAData[2,3] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore != 0 & DataFrameClean$Contraception == 'Cop. Coil'] == "B-A")
BAData[2,4] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore != 0 & DataFrameClean$Contraception == 'Natural'] == "B-A")
BAData[2,5] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore != 0 & DataFrameClean$Contraception == 'other'] == "B-A")
BAData[2,6] <- sum(DataFrameClean$Order[DataFrameClean$PMSScore != 0] == "B-A")



BAData[3,1] <- sum(DataFrameClean$Order[DataFrameClean$Contraception == 'Pill'] == "B-A")
BAData[3,2] <- sum(DataFrameClean$Order[DataFrameClean$Contraception == 'Hor. Coil'] == "B-A")
BAData[3,3] <- sum(DataFrameClean$Order[DataFrameClean$Contraception == 'Cop. Coil'] == "B-A")
BAData[3,4] <- sum(DataFrameClean$Order[DataFrameClean$Contraception == 'Natural'] == "B-A")
BAData[3,5] <- sum(DataFrameClean$Order[DataFrameClean$Contraception == 'other'] == "B-A")
BAData[3,6] <- sum(DataFrameClean$Order == "B-A")

########################### Get testmoment specific data ###########################
# loop over DataFrameClean
# 
# For trial specific answers:
# Check if directory exists for that participantnnumber for each time. 
# Check if driectory 2 exists
# 
# For general questionnaire data
# Export data csv's
# - Read CSVs'
# 
# do if statement on testingOrder column
# Read out data
# Make function to generate PSS score
# Make function to generate BSRI score

dataMoment1 <- as.data.frame(read.csv(file = paste0(dataDir, dateDir,"results-survey10001.csv"), head = TRUE, sep=",",  stringsAsFactors=FALSE))
dataMoment2 <- as.data.frame(read.csv(file = paste0(dataDir, dateDir,"results-survey10002.csv"), head = TRUE, sep=",",  stringsAsFactors=FALSE))

# Clean up data for faulty entries
dataMoment1 <- dataMoment1[!(dataMoment1$lastpage < 3 | is.na(dataMoment1$lastpage)), ]
dataMoment2 <- dataMoment2[!(dataMoment2$lastpage < 3 | is.na(dataMoment2$lastpage)), ]
rownames(dataMoment1) <- NULL
rownames(dataMoment2) <- NULL

# Make functions to extract questionnaire scores from the data
### PSS ###
getPSS <- function(data) {
  tempData <- data[ , grepl("PSS.P", names(data))] # Make dataset with only RRS variables
  allPSS = 0
  
  for(i in 1:nrow(data)) { # loop through participants
    PSSScore <- 0
    for(t in 1:ncol(tempData)){ # loop through questions
      temp = as.numeric(substrRight(unlist(tempData[t])[i],1)) - 1 # Take value i (participant) from RRSDATA, unlist, then take last character and turn it into a number (double) # And substract one because we use different scales
      if (t==4 | t==5 | t==7 | t==8){
        temp = 4 - temp # reserse score these
      }
      PSSScore <- PSSScore + temp
    }
    allPSS[i] <- PSSScore
  }
  return(allPSS)
}

### BSRI ####
getBSRI <- function(data) {
  tempData <- data[ , grepl("BSRI.B", names(data))] # Make dataset with only RRS variables
  allBSRI = 0
  
  for(i in 1:nrow(data)) { # loop through participants
    BSRIScore <- 0
    for(t in 1:ncol(tempData)){ # loop through questions
      temp = as.numeric(unlist(tempData[t])[i]) # Take value i (participant) from RRSDATA, unlist, then take last character and turn it into a number (double)
      BSRIScore <- BSRIScore + temp
    }
    allBSRI[i] <- BSRIScore
  }
  return(allBSRI)
}

dataMoment1$PSS = getPSS(dataMoment1)
dataMoment1$BSRI = getBSRI(dataMoment1)

dataMoment2$PSS = getPSS(dataMoment2)
dataMoment2$BSRI = getBSRI(dataMoment2)

# Get scores and add to right participants
PSS <- data.frame(PSS1 = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1), PSS2 = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1))
BSRI <- data.frame(BSRI1 = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1), BSRI2 = matrix(NA, nrow = nrow(DataFrameClean), ncol = 1))
temp = 0

for (i in 1:nrow(DataFrameClean)){ # Loop over all participant rows that filled out screening completely
  # DataMoment1
  loc = which(dataMoment1$ParticipantNo == DataFrameClean$participantNo[i]) # Check at what location every specific participantNumber is present
  # print(loc)
  if (length(loc) == 0) {
    temp = temp + 1
    # print(paste0("Something going on with participant ",toString(DataFrameClean$participantNo[i])))
  } else if (length(loc) == 1) {
    PSS$PSS1[i] <- dataMoment1$PSS[loc]
    BSRI$BSRI1[i] <- dataMoment1$BSRI[loc]
    # print(dataMoment1$BSRI[loc])
  } else {
    # If there are multiple entries for one participant, we take the last entry #check this later #@Mitchel get back here some time
    PSS$PSS1[i] <- dataMoment1$PSS[loc[length(loc)]]
    BSRI$BSRI1[i] <- dataMoment1$BSRI[loc[length(loc)]]
  }
  # DataMoment2
  loc = which(dataMoment2$ParticipantNo == DataFrameClean$participantNo[i]) # Check at what location every specific participantNumber is present
  if (length(loc) == 0) {
    # print(paste0("Something going on with participant ",toString(DataFrameClean$participantNo[i])))
  } else if (length(loc) == 1) {
    PSS$PSS2[i] <- dataMoment2$PSS[loc]
    BSRI$BSRI2[i] <- dataMoment2$BSRI[loc]
    # print(dataMoment1$BSRI[loc])
  } else {
    # If there are multiple entries for one participant, we take the last entry #check this later #@Mitchel get back here some time
    PSS$PSS2[i] <- dataMoment1$PSS[loc[length(loc)]]
    BSRI$BSRI2[i] <- dataMoment1$BSRI[loc[length(loc)]]
  }
}

DataFrameClean$folliculairPSS = ''
DataFrameClean$folliculairBSRI = ''
DataFrameClean$luteaalPSS = ''
DataFrameClean$luteaalBSRI = ''
rownames(DataFrameClean) <- NULL # Wat easier for debugging

DataFrameClean$Order[DataFrameClean$Order == ""] = 'xx'

# Add the data to the dataFrame for right spot
for (i in 1:nrow(DataFrameClean)){ 
 if (DataFrameClean$Order[i] == "A-B"){
   DataFrameClean$folliculairPSS[i] = PSS$PSS1[i]
   DataFrameClean$folliculairBSRI[i] = BSRI$BSRI1[i]
   
   DataFrameClean$luteaalPSS[i] = PSS$PSS2[i]
   DataFrameClean$luteaalBSRI[i] = BSRI$BSRI2[i]
 } else if (DataFrameClean$Order[i] == "B-A"){
   DataFrameClean$folliculairPSS[i] = PSS$PSS2[i]
   DataFrameClean$folliculairBSRI[i] = BSRI$BSRI2[i]
   
   DataFrameClean$luteaalPSS[i] = PSS$PSS1[i]
   DataFrameClean$luteaalBSRI[i] = BSRI$BSRI1[i]
 } else if (DataFrameClean$Order[i] == 'xx') { # For some reason doesn't have an order assigned yet
   DataFrameClean$folliculairPSS[i] = NA
   DataFrameClean$folliculairBSRI[i] = NA
   
   DataFrameClean$luteaalPSS[i] = NA
   DataFrameClean$luteaalBSRI[i] = NA
 } else { # Checks for non-sensical order assignments
   print("Order error")
   break
 }
}

# dataDir = "Z:/ghepmk_data/2020_Kappen_PMS//"
# dateDir = "02032021//"

write.csv(DataFrameClean, paste0(dataDir,dateDir,"cleanData.csv"), row.names = FALSE)

# backup <- as.data.frame(read.csv(file = paste0(dataDir, dateDir,"cleanData_backup.csv"), head = TRUE, sep=",",  stringsAsFactors=FALSE)) # Testing if bugs in code got fixed

IsalaSet = DataFrameClean[DataFrameClean$Isala != "",]
for (i in 1:length(isala)){
  loc = which(IsalaSet == isala[i])
  
  if (length(loc) == 0) {
    print(paste0(toString(isala[i])))
  } 

}

write.csv(IsalaSet, paste0(dataDir,dateDir,"IsalaDataScreen.csv"), row.names = FALSE)

msData <- DataFrameClean[is.na(DataFrameClean$folliculairPSS) == FALSE & is.na(DataFrameClean$luteaalPSS) == FALSE, ]
msData <- msData[msData$participantNo != 407, ] # This is a double entry. 

groupingVars <- colnames(msData)[1:24]
# Order, Moment (A-B), PSS, BSRI
msData <- reshape(msData, direction='long', 
        # varying=c('folliculairPSS', 'folliculairBSRI', 'luteaalPSS', 'luteaalBSRI'), 
        # varying=c('luteaalPSS', 'luteaalBSRI', 'folliculairPSS', 'folliculairBSRI'), 
        varying=list(c('folliculairPSS', 'luteaalPSS'),
                     c('folliculairBSRI', 'luteaalBSRI')),
        timevar='Moment',
        times=c('Foll', 'Lut'),
        v.names=c('PSS', 'BSRI'),
        # v.names=c('BSRI', 'PSS'),
        idvar='participantNo')
colnames(msData)[1] <- 'ID'
colnames(msData)[which(colnames(msData) == "DASS.Stress")] = "DASS_Stress"
colnames(msData)[which(colnames(msData) == "DASS.Anxiety")] = "DASS_Anxiety"
colnames(msData)[which(colnames(msData) == "DASS.Depresh")] = "DASS_Depression"

write.csv(msData, paste0(dataDir,dateDir,"cleanedDataMoments.csv"), row.names = FALSE)

## Wide data for the trait questionnaires
msData <- DataFrameClean
msData <- msData[msData$participantNo != 407, ] # This is a double entry. 
colnames(msData)[1] <- 'ID'
colnames(msData)[which(colnames(msData) == "DASS.Stress")] = "DASS_Stress"
colnames(msData)[which(colnames(msData) == "DASS.Anxiety")] = "DASS_Anxiety"
colnames(msData)[which(colnames(msData) == "DASS.Depresh")] = "DASS_Depression"

write.csv(msData, paste0(dataDir,dateDir,"cleanedDataTraits.csv"), row.names = FALSE)
