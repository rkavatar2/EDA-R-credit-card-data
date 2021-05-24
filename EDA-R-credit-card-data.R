#setting the directory for work
setwd("D:\\Jigsaw Academy Course\\Data Wrangling and EDA with R")
#reading data and replacing blank spaces with NA
cr<-read.csv("Credit.csv",na.strings=c("",NA))

# loading dplyr library and scipen to disable scientific notation
library(dplyr)
options(scipen=999)

##Now we will do Data Exploration##
#This is a credit card dataset
#We will do
#Sanity check
#Identify outliers, replace them
#Approaches to impute missing values
#Bin the data-> Quantile function, ntile() for binning
#Partitioning data: test and training samples

#checking the column names and matching it with data dictionary
names(cr)
head(cr)
summary(cr['MonthlyIncome'])
summary(cr['MonthlyIncome.1'])

#removing duplicate columns NPA status and MonthlyIncome.1
cr<-cr[,-c(1,12)]
names(cr)
summary(cr)

#Good_Bad is our target column so removing the column having null values
index<-which(is.na(cr$Good_Bad))
cr<-cr[-index,]
summary(cr)

#checking each column one by one

#RevolvingUtilizationOfUnsecuredLines

summary(cr$RevolvingUtilizationOfUnsecuredLines)

#RevolvingUtilizationOfUnsecuredLines represents percentage value, its value should be between 0 & 1

cr%>%filter(RevolvingUtilizationOfUnsecuredLines==0)%>%nrow()
#10878 columns having 0 value

cr%>%filter(RevolvingUtilizationOfUnsecuredLines>=0.99)%>%nrow()
#14383 columns having value more than or equal to 0.99

#quantile breakup to uderstand the data
quantile(cr$RevolvingUtilizationOfUnsecuredLines,p=c(1:100)/100)
#99% of data having less than or equal to 1.09 value. there's an outlier or wrong data

#Discuss with client, 2 is the limit on the number, removing the remaining rows
cr%>%filter(RevolvingUtilizationOfUnsecuredLines>2)%>%nrow()
cr%>%filter(RevolvingUtilizationOfUnsecuredLines<=2)->cr

#age
summary(cr$age)

#age value must not be below 18 for a credit card user
cr%>%filter(cr$age==0)%>%nrow()
#percentile breakup
quantile(cr$age,p=c(1:100)/100)
head(sort(cr$age, FALSE))
cr%>%filter(cr$age>100)%>%nrow()
#there's only one column having wrong data, removing that column

cr%>%filter(age!=0)->cr

#checking categorical data Gender
summary(cr$Gender)

#checking categorical data Region
summary(cr$Region)

#checking Continuous data MonthlyIncome
summary(cr$MonthlyIncome)
#29638 missing values

cr%>%filter(MonthlyIncome==0)%>%nrow()
#1632 columns having 0 monthly income, which looks bad data so considering them as missing values
cr$MonthlyIncome<-ifelse(cr$MonthlyIncome==0,NA, cr$MonthlyIncome)
summary(cr$MonthlyIncome)
#31270 missing values, we need to do the replacement for this

quantile(cr$MonthlyIncome,p=c(1:100)/100, na.rm = TRUE)
cr%>%filter(MonthlyIncome>25000)%>%nrow()
#1161 columns having income more than 25000

#best way to impute missing values in continuous data is to divide them into groups
#divideing into deciles and see the event rate
cr%>%mutate(quantile=ntile(MonthlyIncome,10))%>%group_by(Good_Bad,quantile)%>%summarize(N=n())%>%filter(Good_Bad=="Bad")->dat1
dat1
#dat1 is showing how many bad customers we have in each quantile 
#now will find out the bad rate of each quantile
cr%>%mutate(quantile=ntile(MonthlyIncome,10))%>%group_by(quantile)%>%summarize(N=n())->dat2
dat2

dat1$Percentage<-dat1$N/dat2$N
dat1
#we can see the bad rate of missing values (0.055) behaves similar to 7th group (0.0586)

#finding the values in 8th decile using quantile function
quantile(cr$MonthlyIncome,p=c(0:10)/10, na.rm = TRUE)
#value in 8th decile is from 7536.9 to 9154.6, so we can impute any 
#values between these or best is to take average but I'll impute 7700 for missing values
cr$MonthlyIncome[is.na(cr$MonthlyIncome)]<-7700
summary(cr$MonthlyIncome)


names(cr)
#Rented_OwnHouse categorical data
summary(cr$Rented_OwnHouse)

#Occupation categorical data
summary(cr$Occupation)

#Education categorical data
summary(cr$Education)

summary(cr)
#NumberOfTime30.59DaysPastDueNotWorse Behave like a categorical data
summary(cr$NumberOfTime30.59DaysPastDueNotWorse)

quantile(cr$NumberOfTime30.59DaysPastDueNotWorse, p=c(1:100)/100)
#this value must not be greater than 24 as in two year, customer cannot get more than 24 bills
cr%>%filter(cr$NumberOfTime30.59DaysPastDueNotWorse>24)%>%nrow()
#replacing it with NA and then we will impute the missing values
cr$NumberOfTime30.59DaysPastDueNotWorse<-ifelse(cr$NumberOfTime30.59DaysPastDueNotWorse>24,NA,cr$NumberOfTime30.59DaysPastDueNotWorse)
unique(cr$NumberOfTime30.59DaysPastDueNotWorse)

#for this case, we will impute the value that closely matches with the Good_Bad event rate for missing values
#Finding how many bad customers in each category and missing values
table2<-table(cr$NumberOfTime30.59DaysPastDueNotWorse,cr$Good_Bad)
table2
#Finding bad rate
bad_rate<-table2[,1]/rowSums(table2)
bad_rate

#finding bad rate for missing values
index1<-which(is.na(cr$NumberOfTime30.59DaysPastDueNotWorse))
table(cr$Good_Bad[index1])/length(index1)
#bad rate for missing values is 54% which is very close to category value 6
#so imputing all the missing value with 6
cr$NumberOfTime30.59DaysPastDueNotWorse[index1]<-6
summary(cr$NumberOfTime30.59DaysPastDueNotWorse)
unique(cr$NumberOfTime30.59DaysPastDueNotWorse)


#DebtRatio continuous data
summary(cr$DebtRatio)
quantile(cr$DebtRatio, p=c(1:100)/100)
#lots of data having ratio more than 1 which not usual for debt ratio as described in data dictionary
##Cap at 2## (After discussions with stakeholders)

cr$DebtRatio<-ifelse(cr$DebtRatio>2,2,cr$DebtRatio)
summary(cr$DebtRatio)

summary(cr)

#NumberOfOpenCreditLinesAndLoans
summary(cr$NumberOfOpenCreditLinesAndLoans)
quantile(cr$NumberOfOpenCreditLinesAndLoans, p=c(1:100)/100)
cr%>%filter(cr$NumberOfOpenCreditLinesAndLoans>24)%>%nrow()
#Higher magnitute value is wrong so replacing it with NA and then will do the missing value imputation
cr$NumberOfOpenCreditLinesAndLoans<-ifelse(cr$NumberOfOpenCreditLinesAndLoans>=24, NA, cr$NumberOfOpenCreditLinesAndLoans)
summary(cr$NumberOfOpenCreditLinesAndLoans)

#Finding how many bad customers in each category and missing values
table3<-table(cr$NumberOfOpenCreditLinesAndLoans,cr$Good_Bad)
table3
#Finding bad rate
bad_rate1<-table3[,1]/rowSums(table3)
bad_rate1

#finding bad rate for missing values
index2<-which(is.na(cr$NumberOfOpenCreditLinesAndLoans))
table(cr$Good_Bad[index2])/length(index2)
#bad rate for missing values is 68% which is very close to category value 14
#so imputing all the missing value with 14
cr$NumberOfOpenCreditLinesAndLoans[index2]<-14
summary(cr$NumberOfOpenCreditLinesAndLoans)


#NumberOfTimes90DaysLate
summary(cr$NumberOfTimes90DaysLate)
quantile(cr$NumberOfTimes90DaysLate, p=c(1:100)/100)
cr%>%filter(cr$NumberOfTimes90DaysLate>3)%>%nrow()
unique(cr$NumberOfTimes90DaysLate)
#value 96 & 98 seems incorrect as per the data dictionary, we will find the 
#bad rate for the group and replace it with closely matching data
table4<-table(cr$NumberOfTimes90DaysLate,cr$Good_Bad)
table4
#Finding bad rate
bad_rate2<-table4[,1]/rowSums(table4)
bad_rate2
#so we can replace 96 with 7 and 98 with 3
cr$NumberOfTimes90DaysLate<-ifelse(cr$NumberOfTimes90DaysLate==98,3,cr$NumberOfTimes90DaysLate)
cr$NumberOfTimes90DaysLate<-ifelse(cr$NumberOfTimes90DaysLate==96,7,cr$NumberOfTimes90DaysLate)
summary(cr$NumberOfTimes90DaysLate)


#NumberRealEstateLoansOrLines
summary(cr$NumberRealEstateLoansOrLines)
unique(cr$NumberRealEstateLoansOrLines)
quantile(cr$NumberRealEstateLoansOrLines, p=c(1:100)/100)
#99% of data has less than or equal to 4 values
cr%>%filter(cr$NumberRealEstateLoansOrLines>9)%>%nrow()
cr$NumberRealEstateLoansOrLines<-ifelse(cr$NumberRealEstateLoansOrLines>9,NA,cr$NumberRealEstateLoansOrLines)
summary(cr$NumberRealEstateLoansOrLines)
table5<-table(cr$NumberRealEstateLoansOrLines,cr$Good_Bad)
table5
#Finding bad rate
bad_rate3<-table5[,1]/rowSums(table5)
bad_rate3
#finding bad rate for missing values
index3<-which(is.na(cr$NumberRealEstateLoansOrLines))
table(cr$Good_Bad[index3])/length(index3)
#bad rate for missing values is 19.84% which is very close to category value 7
#so imputing all the missing value with 7
cr$NumberRealEstateLoansOrLines[index3]<-7
summary(cr$NumberRealEstateLoansOrLines)



#NumberOfTime60.89DaysPastDueNotWorse
summary(cr$NumberOfTime60.89DaysPastDueNotWorse)
unique(cr$NumberOfTime60.89DaysPastDueNotWorse)
quantile(cr$NumberOfTime60.89DaysPastDueNotWorse,p=c(1:100)/100)
cr%>%filter(cr$NumberOfTime60.89DaysPastDueNotWorse>10)%>%nrow()
cr$NumberOfTime60.89DaysPastDueNotWorse<-ifelse(cr$NumberOfTime60.89DaysPastDueNotWorse>10,NA,cr$NumberOfTime60.89DaysPastDueNotWorse)
summary(cr$NumberOfTime60.89DaysPastDueNotWorse)

table6<-table(cr$NumberOfTime60.89DaysPastDueNotWorse,cr$Good_Bad)
table6
#Finding bad rate
bad_rate4<-table6[,1]/rowSums(table6)
bad_rate4
#finding bad rate for missing values
index4<-which(is.na(cr$NumberOfTime60.89DaysPastDueNotWorse))
table(cr$Good_Bad[index4])/length(index4)
#bad rate for missing values is 54% which is very close to category value 3
#so imputing all the missing value with 3
cr$NumberOfTime60.89DaysPastDueNotWorse[index4]<-3
summary(cr$NumberOfTime60.89DaysPastDueNotWorse)




#NumberOfDependents
summary(cr$NumberOfDependents)
unique(cr$NumberOfDependents)
cr$NumberOfDependents<-ifelse(cr$NumberOfDependents=='Good',NA,cr$NumberOfDependents)
cr$NumberOfDependents<-ifelse(cr$NumberOfDependents=='Bad',NA,cr$NumberOfDependents)

quantile(cr$NumberOfDependents,p=c(1:100)/100, na.rm = TRUE)
cr%>%filter(cr$NumberOfDependents>4)%>%nrow()

table7<-table(cr$NumberOfDependents,cr$Good_Bad)
table7
#Finding bad rate
bad_rate5<-table7[,1]/rowSums(table7)
bad_rate5
#finding bad rate for missing values
index5<-which(is.na(cr$NumberOfDependents))
table(cr$Good_Bad[index5])/length(index5)
#bad rate for missing values is 4.57% which is very close to category value 1
#so imputing all the missing value with 1
cr$NumberOfDependents[index5]<-1
summary(cr$NumberOfDependents)
write.csv(cr,"clear_credit_card_data.csv",row.names = FALSE)
summary(cr)
