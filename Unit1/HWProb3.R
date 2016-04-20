#solution to the problem#3 unit1 on Unemployment

CPS=read.csv("CPSData.csv")

#How many interviewees are in the dataset?
nrow(CPS)

#Among the interviewees with a value reported for the Industry variable, what is the most common industry of employment? Please enter the name exactly how you see it.
sort(summary(CPS$Industry),decreasing = TRUE)

#Which state has the fewest interviewees?
sort(table(CPS$State))

#What proportion of interviewees are citizens of the United States?
summary(CPS$Citizenship)
(116639+7073)/(7590+116639+7073)

#For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity? (
table(CPS$Race,CPS$Hispanic)

#Which variables have at least one interviewee with a missing (NA) value?
summary(CPS)

#We can see the breakdown of whether Married is missing based on the reported value of the Region variable with the function table(CPS$Region, is.na(CPS$Married)). Which is the most accurate
table(CPS$Region, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Sex,is.na(CPS$Married))

#How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)
table(CPS$State,is.na(CPS$MetroAreaCode))


#Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table(CPS$Region,is.na(CPS$MetroAreaCode))

#similar to this is
by(is.na(CPS$MetroAreaCode),CPS$Region,mean)
#or
tapply(is.na(CPS$MetroAreaCode),CPS$Region,mean)


#for state find the non metro proportions
sort(by(is.na(CPS$MetroAreaCode),CPS$State,mean))


#reading the metro codes
MetroAreaMap=read.csv("MetroAreaCodes.csv")
CountryMap=read.csv("CountryCodes.csv")

#merege them in the main CPS
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
str(CPS)

#merege them in the main CPS
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
str(CPS)


#in the current data we have 
length(unique((CPS$MetroAreaCode)))
#265 metro codes along with NA so 264 +1
#while the dictionary has 271 code so there will be some codes whihc will not map to any row those will be Na
#also it can happen that the code does not match in the dictionary but we will keep it


#Which of the following metropolitan areas has the largest number of interviewees?
sort(table(CPS$MetroArea),decreasing = TRUE)

#Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?
#the mean will be helpful in this 
head(sort(tapply(CPS$Hispanic,CPS$MetroArea,mean),decreasing = TRUE))


#find the metro areas where atleast 20% interviews are Asian
head(sort(tapply(CPS$Race == "Asian",CPS$MetroArea,mean),decreasing = TRUE))

#find population with no high scool
head(sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE)))


#Among all interviewees born outside of North America, which country was the most common place of birth?
summary(CPS$Country)

#What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area have a country of birth that is not the United States? For this computation, don't include people from this metropolitan area who have a missing country of birth
tapply(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA",
       CPS$Country,sum,na.rm=TRUE)

uSCit= 3736
totCit=5409  #table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA")
nonUSCit=5409-3736
nonUSCit/totCit


#Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India? Hint -- remember to include na.rm=TRUE if you are using tapply() to answer this question 
head(sort(tapply(CPS$Country=="India",CPS$MetroArea,sum,na.rm=TRUE),decreasing = TRUE))


#breazil
head(sort(tapply(CPS$Country=="Brazil",CPS$MetroArea,sum,na.rm=TRUE),decreasing = TRUE))

#Somalia
head(sort(tapply(CPS$Country=="Somalia",CPS$MetroArea,sum,na.rm=TRUE),decreasing = TRUE))
