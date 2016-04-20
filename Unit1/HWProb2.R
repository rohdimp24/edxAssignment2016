#solution to the problem#2 unit1 on Company Stocks

IBM=read.csv("IBMStock.csv")
GE=read.csv("GEStock.csv")
ProcterGamble=read.csv("ProcterGambleStock.csv")
CocaCola=read.csv("CocaColaStock.csv")
Boeing=read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

nrow(IBM)

summary(IBM$Date)
mean(IBM$StockPrice)
min(GE$StockPrice)
max(CocaCola$StockPrice)
median(Boeing$StockPrice)
sd(ProcterGamble$StockPrice)

plot(CocaCola$Date,CocaCola$StockPrice,type='l')
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="red")

#who share tanked more in march 2000
which(ProcterGamble$Date>'1999-01-01')
ProcterGamble[350:365,]
CocaCola[350:365,]

# we can see whose share had a large dip after march 2000


#around 1983 whose share going up
which(ProcterGamble$Date>'1983-01-01')
ProcterGamble[158:168,]
CocaCola[158:168,]


plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="green")
lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="black")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type="l", col="yellow")

#numerically check the stock for the 361:366 
CocaCola[361:365,]
IBM[361:365,]
GE[361:365,]
ProcterGamble[361:365,]
Boeing[361:365,]


#get the index for sept 1997 to nov 1997
which(ProcterGamble$Date>'1997-08-01')
CocaCola[333:335,]
IBM[333:335,]
GE[333:335,]
ProcterGamble[333:335,]
Boeing[333:335,]

plot(CocaCola$Date[333:335], CocaCola$StockPrice[333:335], type="l", col="red")
lines(Boeing$Date[333:335], Boeing$StockPrice[333:335], type="l", col="blue")
lines(IBM$Date[333:335], IBM$StockPrice[333:335], type="l", col="green")
lines(GE$Date[333:335], GE$StockPrice[333:335], type="l", col="black")
lines(ProcterGamble$Date[333:335], ProcterGamble$StockPrice[333:335], type="l", col="yellow")



#get the months for IBM

IBM$months=months(IBM$Date)
Boeing$months=months(Boeing$Date)
GE$months=months(GE$Date)
ProcterGamble$months=months(ProcterGamble$Date)
CocaCola$months=months(CocaCola$Date)




overallAvg=mean(IBM$StockPrice)
tt=sort(by(IBM$StockPrice,IBM$months,mean))
tt>overallAvg

#in a single line
sort(by(IBM$StockPrice,IBM$months,mean))>mean(IBM$StockPrice)


sort(by(GE$StockPrice,GE$months,mean))
sort(by(CocaCola$StockPrice,CocaCola$months,mean))
