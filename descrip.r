
"
Faculty of Engineering. University of Porto
Extradition of knowledge and Machine Learning
	Authors: Samuel Arleo & Daniela Socas

Last modified 12/02/2016
Descriptive functions to start the project and pre-processing
"

#------------------ LOAD PACKAGES --------------------------

if(! "lubridate" %in% rownames(installed.packages())){
	install.packages("lubridate")
	install.packages("ggplot2")
}
library(lubridate)
library(ggplot2)

#------------------ LOAD FUNCTIONS -------------------------

#wd <- "C:/Users/Dusady/Documents/Dan/UP/ML/Loan-Prediction-Data-Analytics"
wd <- "~/Mineria/"

source(paste(wd, "descrip_fun.r", sep=""))

#------------------ LOAD DATA -------------------------

# Table files names
tables <- c("account", "card_test", "card_train", "client", "disp", "district", "loan_test", 
            "loan_train", "trans_test", "trans_train")

# Change here the path of data
path <- "~/Mineria/Data/"
#path <- "C:/Users/Dusady/Documents/Dan/UP/ML/Loan-Prediction-Data-Analytics/Data/"

# Values of empty fields in .csv files
na_values <- c(""," ","NA","?")

loadData( path ,na_values)

# Putting all tables in a list for passing them to checkNA
dframes <- list(account, card_test, card_train, client, disp, district, loan_test, 
            loan_train, trans_test, trans_train)

# Checking number of NA field for each table
checkNa(dframes)

#------------------ Factors to int -------------------------

trans_train$balance <- as.numeric(as.character(trans_train$balance))
trans_train$amount <- as.numeric(as.character(trans_train$amount))

trans_test$balance <- as.numeric(as.character(trans_test$balance))
trans_test$amount <- as.numeric(as.character(trans_test$amount))

# Adding columns to tables: 
#	client: age and gender
#	card_train: weeks (of usage)
#	account: antiquity (of the account)
#	loan_train: current_time (how long was the loan granted)

# Max date on client$birth_number, card$issued to avoid considering dates like 11-01-01 as 2011-01-01
refDate <- max(client$birth_number,card_train$issued,card_test$issued,account$date,
               trans_train$date,trans_test$date,loan_test$date,loan_train$date)

# Adding new columns of age and gender to client table
client$gender<-unlist(lapply(client$birth_number,getGender))
client$age<-unlist(lapply(client$birth_number,getAntiquity,refDate))

# Adding weeks to card
card_train$weeks <- unlist(lapply(card_train$issued,getAntiquity,refDate,"weeks"))

# Adding antiquity to account
account$antiquity <- unlist(lapply(account$date,getAntiquity,refDate,"weeks"))

# Adding current_time to loan
loan_train$current_time <- unlist(lapply(loan_train$date,getAntiquity,refDate,"weeks"))

#Exact age 
client$age<-round(client$age)

#------------------ CHANGING DATES FORMAT -------------------------

#Date columns in date POSIX type better than int 

account$date <-  ymd(account$date)
card_train$issued <- ymd(card_train$issued)
card_test$issued <- ymd(card_test$issued)
loan_train$date <- ymd(loan_train$date)
loan_test$date <- ymd(loan_test$date)
trans_train$date <- ymd(trans_train$date)
trans_test$date <- ymd(trans_test$date)

#Have to change the +50 months 
client$birth_number <- ymd(unlist(lapply(client$birth_number,formatDate)))


# Creating global user info table

# Matching users with their accounts, loans, districts and credit cards
# There will be redundant info but it will make easier to calculare for instace:
# What is the loan performance by region
global = merge(
		merge(
			merge(
				merge(disp, client[,!(names(client)%in%c("birth_number"))],by="client_id"),
					loan_train,by="account_id"),district[,!(names(district)%in%c("code"))],
					by="district_id"),card_train,by="disp_id", all.x=TRUE)


# Getting additional features from existing attributes in global table

# Impact of district over loan status


# Hacer funcion que automatice esto para cualquier feature nominal como type.x
reg_perf <- table(global$district_id,global$status)
reg_perf <- reg_perf + 1
avg <- mean(reg_perf[,1]+reg_perf[,2])
reg_perf<-(reg_perf[,1]-reg_perf[,2])/avg
reg_perf <- as.data.frame(reg_perf)
reg_perf$district_id<-rownames(reg_perf)
global<-merge(global,reg_perf, by="district_id")

# 

#------------------ PLOTS -------------------------

#---------- Account ----------
#Most frequent by far is monthly issuance
plot(account$frequency)

# Age vs Status
boxplot(age~status,data=global)

# By gender 0: Male 1: Female
boxplot(age~status,data=global[global$gender == "0",])
boxplot(age~status,data=global[global$gender == "1",])

# Contingency table for gender vs status
c1 <- table(global$gender,global$status)

# Barplot gender vs status
barplot(c1,main = "Gender-Status frequencies")

#---------- Card -------------

#Frequency types card
plot(card_train$type)

#---------- Client ------------
#We can see how the 1st district has more people
hist(client$district_id, breaks=length(client$district_id))

# By gender
# Men
hist(client[client$gender == "0",]$age)

# Women
hist(client[client$gender == "1",]$age)

# Prueba, no borrar
#mixed <- data.frame(m=client[client$gender == "0",]$age,f=client[client$gender == "1",]$age)
#ggplot(mixed, aes(length, fill = veg)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')

hist(client$age)

#---------- Disp --------------
#Way more OWNERs 
plot(disp$type)


#---------- Loan --------------

barplot(table(loan_train$duration))
barplot(table(loan_train$status))
hist(loan_train$amount)
plot(loan_train$amount,loan_train$status)
boxplot(amount~status,data=loan_train)	# The more amount to granted, the more likely to fraud
plot(loan_train$payments,loan_train$status)
boxplot(payments~status,data=loan_train) # The more months to pay, the more likely to fraud
#---------- Trans -------------

#Most are withdrawals and then credit. 
plot(trans_train$type)
plot(trans_train$operation)
hist(trans_train$amount)
hist(trans_train$balance)

str(trans_train)
summary(trans_train)
#------------------ OUTLIERS -------------------------

# Esto es para ver los outliers de las columnas numericas importantes
# Probar al final el modelo entrenado con y sin outliers
#boxplot(district[,4:16])
#boxplot(loan_train$amount)

# For cleaning the workspace
# closeAllConnections()
# rm(list=ls())