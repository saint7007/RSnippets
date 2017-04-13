#Author Sushant Sawant
#1.Please unzip read and explore data contained in human-resources-analytics.zip

#creating a function
readCsv <- function(file,outDir,fileToRead){
unzip(file,exdir=outDir)
df <- read.csv(paste(outDir,fileToRead,sep=""), header=TRUE,stringsAsFactors=F)
return(df)
}

#reading csv
#input 1= the zip file, input 2= path to which zip is to be extracted, input 3 = file to be read 
df <- readCsv("C:\\Users\\sukumar\\Documents\\human-resources-analytics.zip","C:\\Users\\sukumar\\Documents\\human-resources-analytics","/HR_comma_sep.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#Exploing data

sapply(df, function(x) sum(is.na(x))) #checking if there are NA's 
str(df) # Checking column types
df$salary[ df$salary == "high"  ] <- 1 #converting to numbers
df$salary[ df$salary == "medium" ] <- 0
df$salary[ df$salary == "low" ] <- -1

unique(df$salary) #unique
unique(df$sales)

table(df$salary) #count of unique
table(df$sales) 

min(df$time_spend_company) 
max(df$time_spend_company) 
min(df$salary) 
max(df$salary)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#2
#Exploratory graph - 20marks

dfLeft <- df[ which(df$left==1), ]
dfNotLeft<-  df[ which(df$left==0), ]

library(ggplot2)
par(mfrow=c(2,2))
#plot for people who left
plot(factor(dfLeft$time_spend_company)~factor(dfLeft$salary),col = c("#6465A5",  "#6975A6", "#F3E96B", "#F28A30", "#F05837"),main = "Left Company",xlab = "Salary", ylab = "Time spend in company")

#plot for people still present
plot(factor(dfNotLeft$time_spend_company)~factor(dfNotLeft$salary),col = c("#6465A5",  "#6975A6", "#F3E96B", "#F28A30", "#F05837"),main = "Not Left Company",xlab = "Salary", ylab = "Time spend in company")

#Conclusion: Higher time spent and higher salary left company
  
boxplot(dfLeft$time_spend_company ~ dfLeft$salary, data = dfLeft, ylab = "time spend",ylab = "salary")                  

#Histogram for frequency
hist(dfLeft$time_spend_company, col="#6465A5", main="Time Spent")
hist(dfLeft$average_montly_hours, col="#6975A6", main="Average Monthly Hours")

#Plot exploring data
plot(dfLeft$salary, dfLeft$time_spend_company, col="#7F2E1B", main="Salary and Time Spent")
plot(dfLeft$salary, dfLeft$average_montly_hours, col="#f2c300", main="Salary and Avg Hour")


par(mfrow=c(1,3))
hist(dfLeft$satisfaction_level,col="#6465A5", main = "Satisfaction level") 
hist(dfLeft$last_evaluation,col="#6465A5", main = "Last evaluation")
hist(dfLeft$average_montly_hours,col="#6465A5", main = "Average montly hours")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Is the number of projects a good indicator of whether employee will leave?

cor.test(df$number_project,df$left , method = "pearson")
#OUTPUT
#sample estimates:
#cor 
#0.02378719 

df$sales<-as.factor(df$sales)
df$salary<-as.factor(df$salary)
df$salary<-ordered(df$salary,levels=c("low","medium","high"))
library(corrplot)
cor(df[,1:8])

tapply(df[df$left==1,"left"],df[df$left==1,"number_project"],length)
tapply(df[df$left==0,"left"],df[df$left==0,"number_project"],length)

library(polycor)
hetcor(df$number_project,df$left  ,ML=TRUE) # Pearson

summary(aov(df$left ~ df$number_project))

#No they are not co related                  
#Are we handling workplace accidents well?

numberLeftOrNot<-tapply(df$left,df$Work_accident,length)

lapply(numberLeftOrNot, FUN = function(x) x*100/nrow(df))

#output
#$`0`
#[1] 85.53904

#$`1`
#[1] 14.46096
#Conclusion Yes 85 % people with no accidents have left. and only 14% people with accidents have left 

#further combinations of numbers
lapply(tapply(dfLeft$left,dfLeft$Work_accident,length), FUN = function(x) x*100/nrow(dfLeft))
lapply(tapply(dfNotLeft$left,dfNotLeft$Work_accident,length), FUN = function(x) x*100/nrow(dfNotLeft))

#

#3 months succession planning



dfTimeCompMean<-tapply((dfLeft$time_spend_company*12-3),dfLeft$sales,mean)/12

#output
#accounting          hr          IT  management   marketing product_mng       RandD       sales     support 
#3.544118    3.503488    3.610806    3.508242    3.607143    3.739899    3.750000    3.562623    3.683333 
#technical 
#3.709828 
mean(dfTimeCompMean)
#[1] 0.3226623


dfTimeCompNotLeftMean<-tapply((dfNotLeft$time_spend_company*12-3),dfNotLeft$sales,mean)/12
#output
#accounting          hr          IT  management   marketing product_mng       RandD       sales     support 
#3.174512    2.942748    3.106394    4.145176    3.230916    3.080966    3.002252    3.193698    2.963859 
#technical 
#2.972442

mean(dfTimeCompNotLeftMean)
#output
#[1] 3.181296

dfSatisfaction<-tapply((dfLeft$satisfaction_level),dfLeft$sales,mean)
#output
#accounting          hr          IT  management   marketing product_mng       RandD       sales     support 
#0.4025980   0.4333953   0.4118681   0.4228571   0.4531527   0.4815657   0.4328099   0.4476627   0.4509009 
#technical 
#0.4325251 
mean(dfSatisfaction)
#output
#[1] 0.4369336
dfLastEval<-tapply((dfLeft$last_evaluation),dfLeft$sales,mean)
#accounting          hr          IT  management   marketing product_mng       RandD       sales     support 
#0.6945098   0.6797209   0.7300366   0.7272527   0.6920197   0.7265657   0.7453719   0.7112426   0.7273153 
#technical 
#0.7341320 
mean(dfLastEval)
#output
#[1] 0.7168167

dfTimeCompMean[1]
str(dfTimeCompMean)
#ex for accounting

#if mean of time in company >3.544118 and mean satisfaction >0.0402589 and mean last evaluation > 0.69450,
#then start succession planning in accounting
 
