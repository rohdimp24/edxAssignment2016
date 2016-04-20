#pisa set
pisaTrain=read.csv("pisa2009train.csv")
pisaTest=read.csv("pisa2009test.csv")

#How many students are there in the training set?
nrow(pisaTrain)

# what is the average reading test score of males?
str(pisaTrain)
tapply(pisaTrain$readingScore,pisaTrain$male,mean)

summary(pisaTrain)

#removing the missing values
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

#releveling the factor variables to the most common
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")


lmScore=lm(readingScore~.,data=pisaTrain)
summary(lmScore)

#RMSE for the trainign set
SSE=sum(lmScore$residuals^2)
rmse=sqrt(SSE/nrow(pisaTrain))


#diff between grade11 and grade 9
29.542707*(11-9)


step(lmScore)

lmStep=lm(formula = readingScore ~ grade + male + raceeth + expectBachelors + 
     motherBachelors + fatherBachelors + computerForSchoolwork + 
     read30MinsADay + publicSchool + schoolSize, data = pisaTrain)

summary(lmStep)

#predictions
pred=predict(lmScore,newdata = pisaTest)

range(pred)

SSEtest=sum((pisaTest$readingScore-pred)^2)
RMSEtest=sqrt(SSEtest/nrow(pisaTest))

baseLine=mean(pisaTrain$readingScore)
SSTtest=sum((pisaTest$readingScore-baseLine)^2)

#now the R2
r2=1-(SSEtest/SSTtest)
r2
