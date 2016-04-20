poll=read.csv("Anonymitypoll.csv")
str(poll)
#How many interviewees responded that they use a smartphone?
table(poll$Smartphone)
#How many interviewees did not respond to the question, resulting in a missing value, or NA, in the summary() output?
summary(poll$Smartphone)

#Which of the following are states in the Midwest census region? (Select all that apply.)
table(poll$Region,poll$State)


#How many interviewees reported not having used the Internet and not having used a smartphone?
nrow(subset(poll,poll$Internet.Use!=1&poll$Smartphone!=1))

#How many interviewees reported having used the Internet and having used a smartphone?
nrow(subset(poll,poll$Internet.Use==1&poll$Smartphone==1))

#How many interviewees reported having used the Internet but not having used a smartphone?
nrow(subset(poll,poll$Internet.Use==1&poll$Smartphone!=1))


#this will also give the same
table(poll$Internet.Use,poll$Smartphone)



#How many interviewees have a missing value for their Internet use?
summary(poll$Internet.Use)
#How many interviewees have a missing value for their smartphone use?
summary(poll$Smartphone)


limited=subset(poll,poll$Internet.Use==1|poll$Smartphone==1)
nrow(limited)


#Which variables have missing values in the limited data frame? (Select all that apply.)
summary(limited)


#What is the average number of pieces of personal information on the Internet,
#according to the Info.On.Internet variable?
mean(limited$Info.On.Internet,na.rm=TRUE)

#How many interviewees reported a value of 0 for Info.On.Internet?
#How many interviewees reported the maximum value of 11 for Info.On.Internet?
table(limited$Info.On.Internet)

#Problem 3.4 - Summarizing Opinions about Internet Privacy
summary(limited$Worry.About.Info)
table(limited$Worry.About.Info)
386/(386+404)

#What proportion of interviewees who answered the Anonymity.Possible 
#question think it is possible to be completely anonymous on the Internet?
summary(limited$Anonymity.Possible)
table(limited$Anonymity.Possible)
278/(278+475)

#What proportion of interviewees who answered the Tried.Masking.Identity
#question have tried masking their identity on the Internet?
summary(limited$Tried.Masking.Identity)
table(limited$Tried.Masking.Identity)
128/(128+656)

#What proportion of interviewees who answered the Privacy.Laws.Effective question find United States privacy laws effective?
summary(limited$Privacy.Laws.Effective)
table(limited$Privacy.Laws.Effective)
186/(186+541)


#histogram of the ages of the interviewees
hist(limited$Age,breaks=100)
table(limited$Age)


plot(limited$Age, limited$Info.On.Internet)
max(table(limited$Age, limited$Info.On.Internet))

jitter(c(1, 2, 3))

plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

tapply(limited$Info.On.Internet,limited$Smartphone,summary)

table(limited$Smartphone,limited$Info.On.Internet)

#tapply(limited$Tried.Masking.Identity,limited$Smartphone,mean,na.rm=TRUE)
tt=subset(limited,limited$Tried.Masking.Identity!=0&limited$Smartphone==1)
nrow(tt)
tt=subset(limited,limited$Tried.Masking.Identity!=0&limited$Smartphone==0)
nrow(tt)
