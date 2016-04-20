#this is the climatic change problem
climatedf=read.csv("climate_change.csv")
str(climatedf)
train=subset(climatedf,climatedf$Year<2007)
test=subset(climatedf,climatedf$Year>2006)
model1=lm(Temp~.-Year-Month,data=train)
summary(model1)

cor(train)


model2=lm(Temp~MEI+TSI+Aerosols+N2O,data=train)
summary(model2)

#automatically derive the model
step(model1)

autoModel=lm(formula = Temp ~ MEI + CO2 + N2O + CFC.11 + CFC.12 + TSI +  Aerosols, data = train)
summary(autoModel)

#Using the model produced from the step function, calculate temperature
#predictions for the testing data set, using the predict function.

pred=predict(autoModel,newdata = test)

percentDeviation=((test$Temp-pred)/test$Temp)*100
#how many are less than 10%
abs(percentDeviation)<10
10/24


#sum of squared errro
SSE=sum((pred-test$Temp)^2)
#root mean squared error
rmse=sqrt(SSE/nrow(test))
#difference between the test price and the baseline price which is given by y=avg(TRAIN)..that is if we 
#consider a model that is passing the mean of the training numbers
SST=sum((mean(train$Temp)-test$Temp)^2)
r2=1-(SSE/SST)
r2


