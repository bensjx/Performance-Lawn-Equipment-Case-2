#1 Improving production using regression analysis
DAD = read.xlsx('Performance Lawn Equipment Database.xlsx',sheetName = 'Defects After Delivery',startRow = 4,header = TRUE)
DAD = na.omit(DAD)


#Before initiattive
BDAD = data.frame(c(1:20),c(DAD$X2010,DAD$X2011[1:8]))
names(BDAD) = c('Months','Defects')
y = BDAD$Defects
x = BDAD$Months
par(mfrow = c(2,2))

#finding the best regression model
#Linear
fit = lm(y~x)
plot(x,y,pch = 20,main = 'Linear Before Aug 2011',xlab = 'Months from Jan 1 2010',ylab = 'Defects')
abline(fit,col ='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2

#Exponential
f = function(x,a,b){a*exp(b*x)}
model0 = lm(log(y)~x)#setting the starting values
start0 = list(a=exp(coef(model0)[1]),b=coef(model0)[2])#setting the starting values
fit = nls(y~f(x,a,b),start = start0)
co = coef(fit)
plot(x,y,pch = 20,main = 'Exponential Before Aug 2011',xlab = 'Months from Jan 1 2010',ylab = 'Defects')
curve(f(x,a=co[1],b=co[2]),add=TRUE,col='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2

#Logarithmic
f=function(x,a,b){a*log(x)+b}
fit = nls(y~f(x,a,b),start = c(a=1, b=1))
co = coef(fit)
plot(x,y,pch = 20,main = 'Logarithmic Before Aug 2011',xlab = 'Months from Jan 1 2010',ylab = 'Defects')
curve(f(x,a=co[1],b=co[2]),add=TRUE,col='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2
co
curve(f(x,a=co[1],b=co[2]),from=0,to=64,main = 'Defects with implementation',xlab = 'number of months',ylab = 'number of defects')

#Polynomial 2nd
f = function(x,a,b,d){a*x**2+b*x+d}
fit = nls(y~f(x,a,b,d),start = c(a=1,b=1,d=1))
co = coef(fit)
plot(x,y,pch = 20,main = 'Polynomial 2nd Before Aug 2011',xlab = 'Months from Jan 1 2010',ylab = 'Defects')
curve(f(x,a=co[1],b=co[2],d=co[3]),add=TRUE,col='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2

#Polynomial 3rd degree
f = function(x,a,b,d,e){(a*x^3)+(b*x^2)+(d*x)+e}
fit = nls(y~f(x,a,b,d,e),start = c(a=1,b=1,d=1,e=1))
co = coef(fit)
plot(x,y,pch = 20,main = 'Polynomial 3rd Before Aug 2011',xlab = 'Months from Jan 1 2010',ylab = 'Defects')
curve(f(x,a=co[1],b=co[2],d=co[3],e=co[4]),add=TRUE,col='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2
co
curve(f(x,a=co[1],b=co[2],d=co[3],e=co[4]),from=0,to=64,main = 'Defects with implementation',xlab = 'number of months',ylab = 'number of defects')

#After initiative
ADAD = data.frame(c(1:40),c(DAD$X2011[9:12],DAD$X2012,DAD$X2013,DAD$X2014))
names(ADAD) = c('Months','Defects')
y = ADAD$Defects
x = ADAD$Months
par(mfrow = c(2,2))

#finding the best regression model
#Linear
fit = lm(y~x)
plot(x,y,pch = 20,main = 'Linear After Aug 2011',xlab = 'Months from Sept 1 2011',ylab = 'Defects')
abline(fit,col = 'red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2

#Exponential
f = function(x,a,b){a*exp(b*x)}
model0 = lm(log(y)~x)#setting the starting values
start0 = list(a=exp(coef(model0)[1]),b=coef(model0)[2])#setting the starting values
fit = nls(y~f(x,a,b),start = start0)
co = coef(fit)
plot(x,y,pch = 20,main = 'Exponential After Aug 2011',xlab = 'Months from Sept 1 2011',ylab = 'Defects')
curve(f(x,a=co[1],b=co[2]),add=TRUE,col='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2

#Logarithmic
f=function(x,a,b){a*log(x)+b}
fit = nls(y~f(x,a,b),start = c(a=1, b=1))
co = coef(fit)
plot(x,y,pch = 20,main = 'Logarithmic After Aug 2011',xlab = 'Months from Sept 1 2011',ylab = 'Defects')
curve(f(x,a=co[1],b=co[2]),add=TRUE,col='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2

#Polynomial 2nd
f = function(x,a,b,d){a*x**2+b*x+d}
fit = nls(y~f(x,a,b,d),start = c(a=1,b=1,d=1))
co = coef(fit)
plot(x,y,pch = 20,main = 'Polynomial 2nd After Aug 2011',xlab = 'Months from Sept 1 2011',ylab = 'Defects')
curve(f(x,a=co[1],b=co[2],d=co[3]),add=TRUE,col='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2
coef(fit)
curve(f(x,a=co[1],b=co[2],d=co[3]),from=0,to=60,main = 'Defects without implementation',xlab = 'number of months',ylab = 'number of defects')

#Polynomial 3rd degree
f = function(x,a,b,d,e){(a*x^3)+(b*x^2)+(d*x)+e}
fit = nls(y~f(x,a,b,d,e),start = c(a=1,b=1,d=1,e=1))
co = coef(fit)
plot(x,y,pch = 20,main = 'Polynomial 3rd After Aug 2011',xlab = 'Months from Sept 1 2011',ylab = 'Defects')
curve(f(x,a=co[1],b=co[2],d=co[3],e=co[4]),add=TRUE,col='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2


#Employee retention rate
ERR = read.xlsx('Performance Lawn Equipment Database.xlsx',sheetName = 'Employee Retention',startRow = 3,header = TRUE)
y = ERR$YearsPLE
fit = lm(y~ERR$YrsEducation+ERR$College.GPA+ERR$Age) #multiple linear regression
summary(fit)
fit = lm(y~ERR$College.GPA+ERR$Age) #multiple linear regression
summary(fit)
fit = lm(y~ERR$Age) #multiple linear regression
summary(fit)
coef(fit)


#Engines
Engine = read.xlsx('Performance Lawn Equipment Database.xlsx',sheetName = 'Engines',startRow = 3,header = TRUE)
y = Engine$Production.Time..min.
x = Engine$Sample
par(mfrow = c(2,3))

#finding the best regression model
#Linear
fit = lm(y~x)
plot(x,y,pch = 20,main = 'Linear ',xlab = 'samples',ylab = 'Production time')
abline(fit,col ='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2

#Exponential
f = function(x,a,b){a*exp(b*x)}
model0 = lm(log(y)~x)#setting the starting values
start0 = list(a=exp(coef(model0)[1]),b=coef(model0)[2])#setting the starting values
fit = nls(y~f(x,a,b),start = start0)
co = coef(fit)
plot(x,y,pch = 20,main = 'Exponential ',xlab = 'samples',ylab = 'Production time')
curve(f(x,a=co[1],b=co[2]),add=TRUE,col='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2

#Logarithmic
f=function(x,a,b){a*log(x)+b}
fit = nls(y~f(x,a,b),start = c(a=1, b=1))
co = coef(fit)
plot(x,y,pch = 20,main = 'Logarithmic ',xlab = 'samples',ylab = 'Production time')
curve(f(x,a=co[1],b=co[2]),add=TRUE,col='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2
coef(fit)

#Polynomial 2nd
f = function(x,a,b,d){a*x**2+b*x+d}
fit = nls(y~f(x,a,b,d),start = c(a=1,b=1,d=1))
co = coef(fit)
plot(x,y,pch = 20,main = 'Polynomial 2nd ',xlab = 'samples',ylab = 'Production time')
curve(f(x,a=co[1],b=co[2],d=co[3]),add=TRUE,col='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2

#Polynomial 3rd degree
f = function(x,a,b,d,e){(a*x^3)+(b*x^2)+(d*x)+e}
fit = nls(y~f(x,a,b,d,e),start = c(a=1,b=1,d=1,e=1))
co = coef(fit)
plot(x,y,pch = 20,main = 'Polynomial 3rd ',xlab = 'samples',ylab = 'Production time')
curve(f(x,a=co[1],b=co[2],d=co[3],e=co[4]),add=TRUE,col='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2


#Q2 Predicting manufacturing capacity using forecasting models of sales
#mower unit sales
MUS = read.xlsx('Performance Lawn Equipment Database.xlsx',sheetName = 'Mower Unit Sales',startRow = 3,header = TRUE)
#NA
mus.na.ts = ts(MUS$NA.,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(mus.na.ts))
plot(mus.na.ts)
fit = HoltWinters(mus.na.ts,beta = FALSE) #there is seasonality and no trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#SA
mus.sa.ts = ts(MUS$SA,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(mus.sa.ts))
plot(mus.sa.ts)
fit = HoltWinters(mus.na.ts) #additive model, there is seasonality and upward trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#Europe
mus.eur.ts = ts(MUS$Europe,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(mus.eur.ts))
plot(mus.eur.ts)
fit = HoltWinters(mus.na.ts) #additive model, there is seasonality and downward trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#Pacific
mus.pac.ts = ts(MUS$Pacific,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(mus.pac.ts))
plot(mus.pac.ts)
fit = HoltWinters(mus.pac.ts,gamma = FALSE) #there is no and upward trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#China
mus.cn.ts = ts(MUS$China,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(mus.cn.ts))
plot(mus.cn.ts)
mus.cn.ts.ttr3 = SMA(mus.cn.ts,n=3) #no trend and no seasonality,short period of time
mus.cn.ts.ttr3 = append(mus.cn.ts.ttr3,NA,after =0)
mus.cn.ts = append(mus.cn.ts,NA,after = length(mus.cn.ts))
MAD = mean(abs(mus.cn.ts.ttr3-mus.cn.ts),na.rm = TRUE)
MAD
mus.cn.ts.ttr3
plot(mus.cn.ts.ttr3)

#World
mus.world.ts = ts(MUS$World,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(mus.world.ts))
plot(mus.world.ts)
fit = HoltWinters(mus.world.ts,beta = FALSE) #there is seasonality and no trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))


#Tractor unit sales
TUS = read.xlsx('Performance Lawn Equipment Database.xlsx',sheetName = 'Tractor Unit Sales',startRow = 3,header = TRUE)
#NA
tus.na.ts = ts(TUS$NA.,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(tus.na.ts))
plot(tus.na.ts)
fit = HoltWinters(tus.na.ts, seasonal = 'mult') #multuplicative model, there is seasonality and upward trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#SA
tus.sa.ts = ts(TUS$SA,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(tus.sa.ts))
plot(tus.sa.ts)
fit = HoltWinters(tus.sa.ts,gamma = FALSE) #there is no seasonality and upward trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#Europe
tus.eur.ts = ts(TUS$Eur,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(tus.eur.ts)
new = window(tus.eur.ts,start = c(2012,1))
plot(decompose(new))
fit = HoltWinters(new, seasonal = 'mult') #multuplicative model, there is seasonality and downward trend, decreasing amplitude
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#Pacific
tus.pac.ts = ts(TUS$Pac,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(tus.pac.ts))
plot(tus.pac.ts)
fit = HoltWinters(tus.pac.ts) #additive model, there is seasonality and trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#China
tus.cn.ts = ts(TUS$China,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(tus.cn.ts))
plot(tus.cn.ts)
fit = HoltWinters(tus.cn.ts,gamma = FALSE) #there is no seasonality and an upward trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#World
tus.world.ts = ts(TUS$World,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(tus.world.ts))
plot(tus.world.ts)
fit = HoltWinters(tus.world.ts, seasonal = 'mult') #multuplicative model, there is seasonality and upward trend, decreasing amplitude
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#Industry total mower sales
ims = read.xlsx('Performance Lawn Equipment Database.xlsx',sheetName = 'Industry Mower Total Sales',startRow = 3,header = TRUE)
#NA
ims.na.ts = ts(ims$NA.,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(ims.na.ts))
plot(ims.na.ts)
fit = HoltWinters(ims.na.ts, beta = FALSE) #there is seasonality and no trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#SA
ims.sa.ts = ts(ims$SA,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(ims.sa.ts))
plot(ims.sa.ts)
fit = HoltWinters(ims.sa.ts, beta = FALSE) #there is seasonality and no trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#Europe
ims.eur.ts = ts(ims$Eur,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(ims.eur.ts))
plot(ims.eur.ts)
fit = HoltWinters(ims.eur.ts) #additive model, there is seasonality and downwards trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#Pacific
ims.pac.ts = ts(ims$Pac,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(ims.pac.ts))
plot(ims.pac.ts)
fit = HoltWinters(ims.pac.ts,gamma = FALSE) #there is no seasonality and upwards trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#World
ims.world.ts = ts(ims$World,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(ims.world.ts))
plot(ims.world.ts)
fit = HoltWinters(ims.world.ts,beta = FALSE) #there is seasonality and no trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#Industry total tractor sales
its = read.xlsx('Performance Lawn Equipment Database.xlsx',sheetName = 'Industry Tractor Total Sales',startRow = 3,header = TRUE)
#NA
its.na.ts = ts(its$NA.,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(its.na.ts))
plot(its.na.ts)
fit = HoltWinters(its.na.ts, seasonal = 'mult') #multuplicative model, there is seasonality and upward trend, increasing amplitude
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#SA
its.sa.ts = ts(its$SA,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(its.sa.ts))
plot(its.sa.ts)
fit = HoltWinters(its.sa.ts,gamma = FALSE) #there is no seasonality and upwards trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#Europe
its.eur.ts = ts(its$Eur,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(its.eur.ts)
new = window(its.eur.ts,start = c(2012,1))
plot(decompose(new))
fit = HoltWinters(new,seasonal = 'mult') #multiplicative model, there is seasonality and downwards trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#Pacific
its.pac.ts = ts(its$Pac,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(its.pac.ts))
plot(its.pac.ts)
fit = HoltWinters(its.pac.ts) #additive model, there is seasonality and upwards trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#China
its.cn.ts = ts(its$China,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(its.cn.ts))
plot(its.cn.ts)
fit = HoltWinters(its.cn.ts,gamma = FALSE) #there is no seasonality and an upward trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#World
its.world.ts = ts(its$World,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(its.world.ts)
new = window(its.world.ts,start = c(2012,1))
plot(decompose(new))
fit = HoltWinters(new, seasonal = 'mult') #multuplicative model, there is seasonality and upward trend, increasing amplitude
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#Unit production
UPC = read.xlsx('Performance Lawn Equipment Database.xlsx',sheetName = 'Unit Production Costs',rowIndex = c(3:63),colIndex = c(1:3),header = TRUE)
#tractor
ups.trac.ts = ts(UPC$Tractor,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(ups.trac.ts))
plot(ups.trac.ts)
fit = HoltWinters(ups.trac.ts,gamma = FALSE) #there is no seasonality and an upward trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))

#mower
ups.mow.ts = ts(UPC$Mower,start = c(2010,1),end = c(2014,12),frequency = 12)
plot(decompose(ups.mow.ts))
plot(ups.mow.ts)
fit = HoltWinters(ups.mow.ts,gamma = FALSE) #there is no seasonality and an upward trend
plot(fit)
accuracy(forecast(fit))
forecast(fit)
plot(forecast(fit))



#3 Generating customer insights using data-mining techniques
#dendogram
PS = read.xlsx('Performance Lawn Equipment Database.xlsx',sheetName = 'Purchasing Survey',startRow = 3, header = TRUE)
dist = dist(PS[,1:7],method = 'euclidean')
fit = hclust(dist,method = 'ward.D2')
dendo = as.dendrogram(fit)
plot(dendo,main = 'Cluster Dendrogram', xlab = 'each of 100 customers')
groups = cutree(fit,k=5)
rect.hclust(fit,k=5,border='red')
fit
groups
group1 = data.frame(PS[groups == 1,])
group2 = data.frame(PS[groups == 2,])
group3 = data.frame(PS[groups == 3,])
group4 = data.frame(PS[groups == 4,])
group5 = data.frame(PS[groups == 5,])

#cause-and-effect modelling
table = matrix(rep(NA,81),ncol = 9,byrow = TRUE)
table = as.data.frame(table)
for (i in 1:9){
  for (j in i:9){
    table[j,i] =abs(cor(PS[,i],PS[,j]))
  }
}
final.table = cbind(c('Delivery Speed','Price Level','Price Flexibility','Manufacturing image','Overall Service','Salesforce Image','Product Quality','Usage Level','Satisfaction Level'),table)
names(final.table) = c('','Delivery Speed','Price Level','Price Flexibility','Manufacturing image','Overall Service','Salesforce Image','Product Quality','Usage Level','Satisfaction Level')
DS = sum(final.table[2:7,2])/6
PL = (sum(final.table[3:7,3])+final.table[2,2])/6
PF = (sum(final.table[4:7,4])+sum(final.table[3,2:3]))/6
MI = (sum(final.table[5:7,5])+sum(final.table[4,2:4]))/6
OS = (sum(final.table[6:7,6])+sum(final.table[5,2:5]))/6
SI = (sum(final.table[7,7])+sum(final.table[6,2:6]))/6
PQ = sum(final.table[7,2:7])/6

#4
set.seed(10)
for (shift.sim in 1:1000){
  for (day in 1:260){ #260 working days per year
    if (day == 1){ #initialise day 1
      ini.invent = 100 #initial inventory
      num.shift = 1 #number of shift per day = 1
      production = num.shift*100 #total number of produced goods per day
      demand = floor(runif(1,min = 80, max = 130)) #demand fluctuates between 80 and 130
      end.invent = ini.invent + production -demand #as given in textbook
      not.enough.invent = FALSE #there is enough inventory
      
      if (end.invent < 0){ #if there is not enough inventory at the end of the day
        end.invent = 0 #invent cannot be negative
        not.enough.invent = TRUE #Now, there is not enough inventory
      }
      day.table = data.frame(day,ini.invent,production,demand,end.invent,not.enough.invent,num.shift) #table for the day
      inventory.log = day.table #main table for the day to be combined across 260 days
    } else {
      ini.invent = end.invent #leftover invent is our starting invent for the next day
      num.shift = ifelse(ini.invent <= 50,2,1) #if less than 50 invent, need to add another shift
      production = num.shift*100
      demand = floor(runif(1,min = 80, max = 130))
      end.invent = ini.invent + production -demand #as given in textbook
      not.enough.invent = FALSE #there is enough inventory
      
      if (end.invent < 0){ #if there is not enough inventory at the end of the day
        end.invent = 0 #invent cannot be negative
        not.enough.invent = TRUE #Now, there is not enough inventory
      }
      day.table = data.frame(day,ini.invent,production,demand,end.invent,not.enough.invent,num.shift) #creates the result table
      inventory.log = rbind(inventory.log,day.table) #combine all the 'table for the day'
    }
  }
  num.shift.annually = sum(inventory.log$num.shift) #total shift per year
  if (shift.sim == 1){
    sim.log = data.frame(shift.sim,num.shift.annually)
  } else {
    sim.log = rbind(sim.log,data.frame(shift.sim,num.shift.annually))
  }
}
mean(sim.log$num.shift.annually)
hist(sim.log$num.shift.annually,main = 'distribution of number of shifts annually',xlab = 'number of shifts',ylab = 'frequency')
hist(sim.log$num.shift.annually,probability = TRUE, main = 'distribution of number of shifts annually',xlab = 'number of shifts',ylab = 'probability')
lines(density(sim.log$num.shift.annually))