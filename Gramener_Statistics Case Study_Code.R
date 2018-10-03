# Loading Libraries

install.packages("gdata")
install.packages("dplyr")
install.packages("plyr")
install.packages("tidyr")
install.packages("splitstackshape")
install.packages("lubridate")
install.packages("raster")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("stringr")
install.packages("DescTools")
install.packages("janitor")
install.packages("anytime")
install.packages("zoo")
install.packages("outliers")
install.packages("gridExtra")
install.packages("gmodels")
install.packages("scales")
install.packages("reshape2")

library("gridExtra")
library(gdata)
library("dplyr")
library(plyr)
library(tidyr)
library(splitstackshape)
library(lubridate)
library(raster)
library(ggplot2)
library(tidyverse)
library(stringr)
library(DescTools)
library(janitor)
library("anytime")
library(zoo)
library(outliers)
library(gmodels)
library(scales)
library(reshape2)
#--------------------------------------------------------------------------------------------------------------

# Reading the file

loan <- read.csv("loan.csv", header = TRUE, stringsAsFactors = FALSE) 

# Converting it to a dataframe

loan_df <- data.frame(loan)

#---------------------------- Data Cleaning and prepping -------------------------------------------------------

#------- Removing outliers -------

# finding outliers and removing them
# There were outliers found in the income and loan amount column which affected the curve and thus were removed

#fromt the outliers function we can see that there are a lot of entries in both annual income and loan amount
# that are disturbing the normal curve and hence are presenting skewed analysis

outliers <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
}
outliers(loan_df, annual_inc)
outliers(loan_df, loan_amnt)

# Basis our analysis we then remove these outliers

m<-loan_df
m<-boxplot.stats(m$annual_inc)$out

loan_test<-loan_df
loan_test<-cbind(loan_df,1)
colnames(loan_test)[112]<-"dummy"

for(i in 1:length(loan_df$annual_inc))
{
  if(loan_df[i,14] %in% m)
  {
    loan_test[i,112]<-0
  }
  else
  {loan_test[i,112]<-1}
}

loan_test<-subset(loan_test,loan_test$dummy==1)

loan_test$dummy<-1
loan_df<-loan_test


m<-loan_df
m<-boxplot.stats(m$loan_amnt)$out

for(i in 1:length(loan_df$loan_amnt))
{
  if(loan_df[i,3] %in% m)
  {
    loan_test[i,112]<-0
  }
  else
  {loan_test[i,112]<-1}
}

loan_dataset_no_outlier<-subset(loan_test,loan_test$dummy==1)
loan_dataset_no_outlier<-loan_dataset_no_outlier[ ,-112]


# There are several columns completely filled with NA, we do not need those columns as they add no value to our analysis

loan_dataset_no_outlier <- remove_empty_cols(loan_dataset_no_outlier)

# For the term column the variables are all in months, thus we keep just the number

loan_dataset_no_outlier <- separate(data = loan_dataset_no_outlier, col = term, into = c("right", "term_months"))
loan_dataset_no_outlier$right <- NULL

# For the column emp_length, there are values <1, 10+ and na
# <1 will be replaced by 0, 10+ by 10 for the ease of analysis

loan_dataset_no_outlier <- separate(data = loan_dataset_no_outlier, col = emp_length, into = c("emp_length_years", "right", "left"))
loan_dataset_no_outlier$right <- NULL
loan_dataset_no_outlier$left <- NULL
loan_dataset_no_outlier$emp_length_years[loan_dataset_no_outlier$emp_length_years==""] <- 0
loan_dataset_no_outlier$emp_length_years[loan_dataset_no_outlier$emp_length_years=="n"] <- "n/a"

#---------------------------- Checking for duplicate id's --------------------------

# There are 3599 elements in the table hence there are no duplicates

length(unique(loan_dataset_no_outlier$id))



#----------------------------------------------Date prepping--------------------------------------

# For issue date

loan_dataset_no_outlier$issue_d<-paste0("01-",loan_dataset_no_outlier$issue_d)
loan_dataset_no_outlier$issue_d <- as.POSIXct(loan_dataset_no_outlier$issue_d, format = "%d-%b-%y")
loan_dataset_no_outlier <- separate(data = loan_dataset_no_outlier, col = issue_d, into = c("issue_dyear", "issue_dmonth"))

# For earliest_cr_line

loan_dataset_no_outlier$earliest_cr_line<-paste0("01-",loan_dataset_no_outlier$earliest_cr_line)
loan_dataset_no_outlier$earliest_cr_line <- as.POSIXct(loan_dataset_no_outlier$earliest_cr_line, format = "%d-%b-%y")
loan_dataset_no_outlier <- separate(data = loan_dataset_no_outlier, col = earliest_cr_line, into = c("earlieastcrline_year", "earliestcrline_month"))

# For lastpayment day

loan_dataset_no_outlier$last_pymnt_d<-paste0("01-",loan_dataset_no_outlier$last_pymnt_d)
loan_dataset_no_outlier$last_pymnt_d<- as.POSIXct(loan_dataset_no_outlier$last_pymnt_d, format = "%d-%b-%y")
loan_dataset_no_outlier <- separate(data = loan_dataset_no_outlier, col = last_pymnt_d, into = c("lastpayment_dyear", "lastpayment_dmonth"))

# For next payment 

loan_dataset_no_outlier$next_pymnt_d<-paste0("01-",loan_dataset_no_outlier$next_pymnt_d)
loan_dataset_no_outlier$next_pymnt_d<- as.POSIXct(loan_dataset_no_outlier$next_pymnt_d, format = "%d-%b-%y")
loan_dataset_no_outlier<- separate(data = loan_dataset_no_outlier, col = next_pymnt_d, into = c("nextpayment_dyear", "nextpayment_dmonth"))

# For last credit pull

loan_dataset_no_outlier$last_credit_pull_d<-paste0("01-",loan_dataset_no_outlier$last_credit_pull_d)
loan_dataset_no_outlier$last_credit_pull_d<- as.POSIXct(loan_dataset_no_outlier$last_credit_pull_d, format = "%d-%b-%y")
loan_dataset_no_outlier <- separate(data = loan_dataset_no_outlier, col = last_credit_pull_d, into = c("lastcreditpull_dyear", "lastcreditpull_dmonth"))



#---------------------------- removing percentage sign --------------------------------------------

loan_dataset_no_outlier$int_rate<-as.numeric(sub('%',"",loan_dataset_no_outlier$int_rate),digits = 0)
loan_dataset_no_outlier$revol_util<-as.numeric(sub('%',"",loan_dataset_no_outlier$revol_util),digits = 0)

#---------- Removing the current loan status as this does not give us clear picture of future defaults

loan_dataset_no_outlier<-subset(loan_dataset_no_outlier, loan_dataset_no_outlier$loan_status!="Current")

#Convertic categorical values to factors

loan_dataset_no_outlier$grade <- as.factor(loan_dataset_no_outlier$grade)

#---- checking for NA
# all the NA are now derived data and not empty fields

summary(is.na(loan_dataset_no_outlier))

##---- Exploratory plotting--------------------

#--------------------- Box Plot - Annual income vs Loan Status -----------------------------------#

ggplot(loan_dataset_no_outlier, aes(loan_dataset_no_outlier$loan_status, loan_dataset_no_outlier$annual_inc)) + geom_boxplot(color="black", fill="pink" )+ theme(
  panel.background = element_rect(fill = "lightblue", colour = "lightblue", size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+ylim(0,150000) +
  labs(title = "Annual income vs Loan Status", x = "Loan Status",y = "Annual income")

#--------------------------------Barplot - Verification ----------------------------#

ggplot(loan, aes(x=factor(loan$verification_status), fill = loan$loan_status)) +geom_bar(position = "dodge",color="black", fill="coral1") + 
  labs(title = "Verification",x = "Verification Status",y = "Count")+ theme(panel.background = element_rect(fill = "lavender", colour = "lavender", size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+ylim(0,150000) 

#----------------------bankrupt people analysis

# This does not give us any statistical proof that people that have declared bankruptcy have actually defaulted more

bankrupt_data<-subset(loan_dataset_no_outlier,loan_dataset_no_outlier$pub_rec_bankruptcies>0)

ggplot(bankrupt_data, aes(bankrupt_data$loan_status)) + geom_bar(color="black", fill="coral1")+labs(title="Bankruptcy statistics", x="Loan Status")+theme(panel.background = element_rect(fill = "lavender", colour = "lavender",  linetype = "solid"),
       panel.grid.major = element_line( linetype = 'solid', colour = "white"), 
          panel.grid.minor = element_line( linetype = 'solid',colour = "white")) 

########Bivariate analysis

#------interest rate and loan status

ggplot(loan_dataset_no_outlier,aes(loan_dataset_no_outlier$loan_status,loan_dataset_no_outlier$int_rate))+geom_boxplot(fill="rosybrown1",color="palevioletred2")+
  labs(title="Interest Rate vs Loan status", x="Loan Status", y="Interest Rate")+theme(panel.background = element_rect(fill = "lavender", colour = "lavender",  linetype = "solid"),
                                                                                       panel.grid.major = element_line( linetype = 'solid', colour = "white"), 
                                                                                       panel.grid.minor = element_line( linetype = 'solid',colour = "white")) 


#----interest rate and grade

ggplot(loan_dataset_no_outlier, aes(x=loan_dataset_no_outlier$grade, y=loan_dataset_no_outlier$int_rate))+geom_boxplot(fill="lightskyblue3",color="midnightblue")+
  labs(title="Interest Rate vs Loan Grade", x="Loan Grade", y="Interest Rate")+theme(panel.background = element_rect(fill = "seashell", colour = "seashell",  linetype = "solid"),
                                                                                       panel.grid.major = element_line( linetype = 'solid', colour = "white"), 
                                                                                       panel.grid.minor = element_line( linetype = 'solid',colour = "white")) 


#------ loan amount and Grade

ggplot(loan_dataset_no_outlier,aes(x=loan_dataset_no_outlier$grade, y=loan_dataset_no_outlier$loan_amnt))+geom_boxplot(fill="thistle1",color="slateblue4")+
  labs(title="Loan Amount vs Loan Grade", x="Loan Grade", y="Loan Amount")+theme(panel.background = element_rect(fill = "snow3", colour = "snow3",  linetype = "solid"),
                                                                                     panel.grid.major = element_line( linetype = 'solid', colour = "white"), 
                                                                                     panel.grid.minor = element_line( linetype = 'solid',colour = "white")) 


# term and loan status

ggplot(loan_dataset_no_outlier, aes(factor(loan_dataset_no_outlier$term_months), 
        fill=factor(loan_dataset_no_outlier$loan_status),group=loan_dataset_no_outlier$loan_status))+ 
                 geom_bar(position = "fill")+labs(title="Term and Loan Status VS Term Freq",fill="Status", x="Term", y="Term Freq")+geom_text(stat="count",aes(label=..count..),
                    position = position_fill(vjust = 0.5),size = 2.5)

# revolving credit utilisation and Loan Status

ggplot(loan_dataset_no_outlier, aes(x=loan_dataset_no_outlier$loan_status, y=loan_dataset_no_outlier$revol_util))+geom_boxplot(fill="seagreen2",color="seagreen")+
  labs(title="Revolving Credit Utilisation and Loan Status", x="Loan Status", y="Revolving Credit")+theme(panel.background = element_rect(fill = "oldlace", colour = "oldlace",  linetype = "solid"),
                                                                                                          panel.grid.major = element_line( linetype = 'solid', colour = "white"), 
                                                                                                          panel.grid.minor = element_line( linetype = 'solid',colour = "white")) 

# loanamount/ anuual income and loan status

ggplot(loan_dataset_no_outlier, aes(loan_dataset_no_outlier$loan_amnt/loan_dataset_no_outlier$annual_inc, col=loan_dataset_no_outlier$loan_status))+geom_histogram(position = "fill")+
  labs(title="Loan amount/Annual Income and Loan Status",col="Status", x="Loan amount/ annual income")

Desc(loan_dataset_no_outlier$annual_inc, main="Annual Income Distribution", plotit=1)

# crosstab and plot of verification status and loan status

CrossTable(loan_dataset_no_outlier$verification_status,loan_dataset_no_outlier$loan_status)

ggplot(loan_dataset_no_outlier, aes(loan_dataset_no_outlier$verification_status, col=loan_dataset_no_outlier$loan_status))+
  geom_bar()+labs(title="Loan Status and Verification Status",col="Loan Status", x="Verification Status")
  
# Term Months and Loan Status cross tab and ggplot

CrossTable(loan_dataset_no_outlier$term_months,loan_dataset_no_outlier$loan_status)

ggplot(loan_dataset_no_outlier, aes(loan_dataset_no_outlier$loan_status, col=loan_dataset_no_outlier$term_months))+
   geom_bar()+labs(title="Term Months and Loan Status", color="Term months", x="Loan Status")

# Grade and Loan Status

CrossTable(loan_dataset_no_outlier$grade,loan_dataset_no_outlier$loan_status)
ggplot(loan_dataset_no_outlier, aes(loan_dataset_no_outlier$grade, col=loan_dataset_no_outlier$loan_status))+
  geom_bar()+labs(title="Loan Grade and Loan Status", color="Loan Status", x="Loan Grade")

# term months and grade

CrossTable(loan_dataset_no_outlier$term_months,loan_dataset_no_outlier$grade)
ggplot(loan_dataset_no_outlier, aes(loan_dataset_no_outlier$grade, col=loan_dataset_no_outlier$term_months))+
  geom_bar()+labs(title="Loan Grade and Term Months", color="Term Months", x="Loan Grade")

#-----deliq data analysis
# no of months since last deliq analysis

delinq_month <- subset(loan_dataset_no_outlier, !is.na(loan_dataset_no_outlier$mths_since_last_delinq))
ggplot(delinq_month, aes(delinq_month$mths_since_last_delinq,col=delinq_month$loan_status))+geom_bar()+
  labs(title = "For loans that are charged off",x = "Months since last delinquence",y = "Count")

Desc(delinq_month$mths_since_last_delinq~delinq_month$loan_status, main="Months since last delinquency distribution", plotit=1)
Desc(loan_dataset_no_outlier$loan_status~loan_dataset_no_outlier$annual_inc, main="annual income and loan status", plotit=1)
#---------------

# DTI Data analysis vs Loan Status : no relevant analysis found

ggplot(loan_dataset_no_outlier, aes(loan_dataset_no_outlier$dti, col=loan_dataset_no_outlier$loan_status))+geom_histogram()+labs(x="DTI", y ="Frequency", col="Status")

# Grade vs Loan Status : Shows that with decreasing grade, the charged off cases increase

ggplot(loan_dataset_no_outlier, aes(loan_dataset_no_outlier$grade, fill=factor(loan_dataset_no_outlier$loan_status),group=loan_dataset_no_outlier$loan_status)) + 
geom_bar(position = "fill") + 
  labs(fill="Status", x="Grade", y="Grade Freq") +
  geom_text(stat="count",aes(label=..count..),position = position_fill(vjust = 0.5),size = 2.5)
