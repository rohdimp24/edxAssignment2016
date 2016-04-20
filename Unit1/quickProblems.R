#this is the solution to the quick lecture questions 

who=read.csv("WHO.csv")


#What is the mean value of the "Over60" variable?
mean(who$Over60)

#Which country has the smallest percentage of the population over 60?
who[which(who$Over60==min(who$Over60)),]

#Which country has the largest literacy rate?
who[ which(who$LiteracyRate==max(who$LiteracyRate,na.rm = TRUE)),]

#Which region has the lowest average child mortality rate across all countries in that region?
sort(by(who$ChildMortality,who$Region,mean))
