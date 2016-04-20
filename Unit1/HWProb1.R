#solution to the problem#1 unit1 on Crime in Chicago

mvt=read.csv("mvtWeek1.csv")
str(mvt)
max(mvt$ID)
min(mvt$Beat)
table(mvt$Arrest)

nrow(subset(mvt,mvt$LocationDescription=="ALLEY"))
mvt$Date[1]
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

#number of thefts per month
min(table(mvt$Month))

#weekday for thefts distribution
table(mvt$Weekday)

#number of arrest and month distribution
sort(by(mvt$Arrest,mvt$Month,sum))


hist(mvt$Date,breaks = 100)
boxplot(mvt$Date~mvt$Arrest)
#the same information cane be found numerically using 
table(mvt$Arrest,mvt$Year)

table(mvt$Arrest,mvt$Year==2001)
2152/(18517+2152)

table(mvt$Arrest,mvt$Year==2007)
1212/(13068+1212)

table(mvt$Arrest,mvt$Year==2012)
550/(13542+550)


sort(table(mvt$LocationDescription),decreasing = TRUE)

Top5=subset(mvt,mvt$LocationDescription=="STREET"|
              mvt$LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)"|
              mvt$LocationDescription=="ALLEY"|
              mvt$LocationDescription=="GAS STATION"|
              mvt$LocationDescription=="DRIVEWAY - RESIDENTIAL")

nrow(Top5)

#to remve the locations with 0 instances of theft
Top5$LocationDescription = factor(Top5$LocationDescription)



table(Top5$Arrest,Top5$LocationDescription)

tt=subset(Top5,Top5$LocationDescription=="GAS STATION")
sort(table(tt$Weekday))


tt=subset(Top5,Top5$LocationDescription=="DRIVEWAY - RESIDENTIAL")
sort(table(tt$Weekday))
