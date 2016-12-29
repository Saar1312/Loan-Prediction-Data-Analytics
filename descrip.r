"
Faculty of Engineering. University of Porto
Extradition of knowledge and Machine Learning
Authors: Samuel Arleo & Daniela Socas

Last modified 29/12/2016
Descriptive functions to start the project and pre-processing
"

#------------------ LOADING PACKAGES --------------------------

if(! "lubridate" %in% rownames(installed.packages())){
  install.packages("lubridate")
}
if(! "ggplot2" %in% rownames(installed.packages())){
  install.packages("ggplot2")
}
if(! "party" %in% rownames(installed.packages())){
  install.packages("party")
}
if(! "rpart" %in% rownames(installed.packages())){
  install.packages("rpart")
}

if(! "DMwR" %in% rownames(installed.packages())){
  install.packages("DMwR")
}
if(! "e1071" %in% rownames(installed.packages())){
  install.packages("e1071")
}
if(! "class" %in% rownames(installed.packages())){
  install.packages("class")
}
if(! "caret" %in% rownames(installed.packages())){
  install.packages("caret")
}
if(! "nnet" %in% rownames(installed.packages())){
  install.packages("nnet")
}
if(! "earth" %in% rownames(installed.packages())){
  install.packages("earth")
}
if(! "randomforest" %in% rownames(installed.packages())){
  install.packages("randomforest")
}
if(! "performanceEstimation" %in% rownames(installed.packages())){
  install.packages("performanceEstimation")
}
if(! "devtools" %in% rownames(installed.packages())){
  install.packages("devtools")
}
if(! "dplyr" %in% rownames(installed.packages())){
  install.packages("dplyr")
}
if(! "Hmisc" %in% rownames(installed.packages())){
  install.packages("Hmisc")
}
# This package could ask for an upgrade of R
if(! "Hmisc" %in% rownames(installed.packages())){
	install.packages("Hmisc")
}

library(lubridate)
library(ggplot2)
library(party)
library(rpart)
library("Hmisc")

library(DMwR)  
library(dplyr)
library(e1071) #Bayes Naive - SVMs
library(class) #K-nearest neightbors 
library(caret) #K-nearest neightbors regression
library(nnet)  #Artiicial Neural Networks
library(earth) #Mars 
library(randomForest) #random forest
library(performanceEstimation)
library(devtools) 
library(Hmisc)

#-------------------- LOADING FUNCTIONS ----------------------

#wd <- "C:/Users/Dusady/Documents/Dan/UP/ML/Loan-Prediction-Data-Analytics"
wd <- "~/Mineria/"

source(paste(wd, "descrip_fun.r", sep=""))

#---------------------- LOADING DATA -------------------------

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

names(dframes) <- c("account", "card_test", "card_train", "client", "disp", "district", 
                    "loan_test", "loan_train", "trans_test", "trans_train")

#--------------------- PREPROCESSING -------------------------

#------------- Types ---------------

# Finding out if each column is factor
findFactors(dframes)

# Getting type of each column of each table to verify types numeric, integer and factor
checkClasses(dframes)
checkTypes(dframes)

# Setting correct types (we are not setting ids as factors, 
# as they won't be included on further analysis)
# Factor to Numeric
district<-toNumeric(district,c("unemploymant.rate..95","unemploymant.rate..96","ratio.of.urban.inhabitants"))
trans_train<-toNumeric(trans_train,c("balance"))
trans_test<-toNumeric(trans_test,c("balance"))

# Integer to Factor
loan_train <- toFactor(loan_train,c("status"))
loan_test <- toFactor(loan_test,c("status"))

# Integer to string
district <- toStr(district,c("name","region"))

# Integer to Numeric
loan_test <- toNumeric(loan_test,c("amount","payments"))
loan_train <- toNumeric(loan_train,c("amount","payments"))
district <- toNumeric(district,c("average.salary"))

#------------- NAs ---------------

# Getting number of NAs for each table and column
checkNa(dframes)

# Filling NAs in district$no..of.commited.crimes..95 and district$unemploymant.rate..95 with mean
mean_crimes <- mean(district$no..of.commited.crimes..95[!is.na(district$no..of.commited.crimes..95)])
district$no..of.commited.crimes..95[is.na(district$no..of.commited.crimes..95)] <- mean_crimes
mean_unemp <- mean(district$unemploymant.rate..95[!is.na(district$unemploymant.rate..95)])
district$unemploymant.rate..95[is.na(district$unemploymant.rate..95)] <- mean_unemp

#--------------------- TRANSFORMATION -------------------------

#-------- Formatting dates ------

# Setting max date to avoid considering 11-01-01 as 2011-01-01 for instance
refDate <- max(client$birth_number,card_train$issued,card_test$issued,account$date,
               trans_train$date,trans_test$date,loan_test$date,loan_train$date)

# Adding more meaningful columns to tables based on dates:
#	TABLE 		| COLUMNS
# ---------------------------------
#	client 		| age and gender
#	card_train  | weeks (of usage)
#	account  	| antiquity (of the account)
#	loan_train  | current_time (how long was the loan granted)

# Adding new columns of age and gender to client table
client$gender<-unlist(lapply(client$birth_number,getGender))
client$age<-unlist(lapply(client$birth_number,getAntiquity,refDate))

# Adding weeks to card (weeks)
card_train$antiq_card <- unlist(lapply(card_train$issued,getAntiquity,refDate,"weeks"))
card_test$antiq_card <- unlist(lapply(card_test$issued,getAntiquity,refDate,"weeks"))

# Adding antiquity to account (weeks)
account$antiq_acc <- unlist(lapply(account$date,getAntiquity,refDate,"weeks"))

# Adding current_time to loan (weeks)
loan_train$current_time <- unlist(lapply(loan_train$date,getAntiquity,refDate,"weeks"))
loan_test$current_time <- unlist(lapply(loan_test$date,getAntiquity,refDate,"weeks"))

# Exact age 
client$age<-round(client$age)

# Date columns in date POSIX type better than int 
account$date <-  ymd(account$date)
card_train$issued <- ymd(card_train$issued)
card_test$issued <- ymd(card_test$issued)
loan_train$date <- ymd(loan_train$date)
loan_test$date <- ymd(loan_test$date)
trans_train$date <- ymd(trans_train$date)
trans_test$date <- ymd(trans_test$date)
client$birth_number <- ymd(unlist(lapply(client$birth_number,formatDate)))

#---------- Joining data frames -----------

# Creating a global tables that matche users with their accounts, loans, districts and credit cards
# to start the mining process

# Changing district table id from code to district_id to join with client table
colnames(district)[1]<-"district_id"

# Matching users with accounts
global_train <- merge(disp, client[,!(names(client)%in%c("birth_number"))],by="client_id")

# Matching users and their accounts with loans
global_test <- merge(global_train,loan_test,by="account_id")
global_train <- merge(global_train,loan_train,by="account_id")

# Matching users with their districts
global_test <- merge(global_test,district,by="district_id")
global_train <- merge(global_train,district,by="district_id")

# Matching users with credit cards
global_test <- merge(global_test,card_test,by="disp_id", all.x=TRUE)
global_train <- merge(global_train,card_train,by="disp_id", all.x=TRUE)

#------------ Adding Features ------------



# Getting additional features from existing attributes in global_train table
# Influence of district over loan status
#global_train <- featureRate(global_train,c("district_id"))

# Add column with influence of district over loan status
#tmp <- as.data.frame(global_train[c("district_id","district_id.rates")])
#avg_perf <- mean(tmp$district_id.rates)
#global_test<-merge(global_test,tmp, by="district_id", all.x=TRUE)
#dim(global_test)
#global_test$district_id.rates[is.na(global_test$district_id.rates)] <- avg_perf
#dim(global_test)
#--------- Deleting some features ---------

# Deleting features that don't give important information (columns with 
# too many NAs, IDs, etc)

# Checking types, classes and NAs of global tables
# Global Train table
checkNa(list(global_train))
checkClasses(list(global_train))
checkTypes(list(global_train))

# Global Test table
checkNa(list(global_test))
checkClasses(list(global_test))
checkTypes(list(global_test))

# Checking correlation matrix and p-values between each pair of variables 
rcorr(as.matrix(global_train[c(7,10:12,14,17:29)]))
cor(global_train[c(7,10:12,14,17:29)])

# Taking out unnecessary columns
global_test <- subset(global_test, select = c(2,4:8,10:29))
global_train <- subset(global_train, select = c(2,4:8,10:29))

global_train$id<-rownames(global_train)
global_test$id<-rownames(global_test)

#--------------------------- DESCRIPTION ----------------------------

#------------------- User -----------------

# Boxplot: Age vs Status
boxplot(age~status,data=global_train)

# Boxplot: Age vs Status by genders (0: Male 1: Female)
boxplot(age~status,data=global_train[global_train$gender == "0",])
boxplot(age~status,data=global_train[global_train$gender == "1",])

# Contingency table: gender vs status
c1 <- table(global_train$gender,global_train$status)

# Barplot: gender vs status
barplot(c1,main = "Gender-Status frequencies")

# Histograms: Age
# Men
hist(client[client$gender == "0",]$age)

# Women
hist(client[client$gender == "1",]$age)

#----------------- Account -----------------

#------------------- Disp ------------------
#Way more OWNERs 
plot(disp$type)


#------------------- Loan ------------------

barplot(table(loan_train$duration))
barplot(table(loan_train$status))
hist(loan_train$amount)
plot(loan_train$amount,loan_train$status)
boxplot(amount~status,data=loan_train)	# The more amount to granted, the more likely to fraud
plot(loan_train$payments,loan_train$status)
boxplot(payments~status,data=loan_train) # The more months to pay, the more likely to fraud

#--------------- Transactions --------------

#Most are withdrawals and then credit. 
plot(trans_train$type)
plot(trans_train$operation)
hist(trans_train$amount)
hist(trans_train$balance)

str(trans_train)
#summary(trans_train)

#--------------------------- PREDICTION ----------------------------

#-------------- Decision tree --------------

# Esto despues lo cambiamos a otro archivo

#model <- rpart( status ~ ., data=global_train ) 

#summary(model)
#plot(model)
#text(model)

#---------- Logistic regression ------------

model_reg <- glm(status~.,family=binomial(link="logit") ,data = global_train[,!(colnames(global_train)%in%c("id","client_id","loan_id","district_id","antiq_card","type.y","antiq_card","name"))])

res <- predict(model_reg,newdata=global_test[,!(colnames(global_test)%in%c("id","client_id","loan_id","district_id","antiq_card","type.y","antiq_card","name"))],type='response')

# Threshold: p>=tr --> status=1 and p<tr status=-1 
tr <- 0.80

# Formatting data with columns loan_id and status
res <- formatResults(res,global_test,c("loan_id","p"),tr)

write.table(res,file="prediction.csv" ,col.names = c("Id","Predicted"),row.names=FALSE,sep=",")

#----------- Predictive functions -----------

#Get sample by percentage 
get_sample <- function(perctg, frame ) 
{
  sp <- sample(1:nrow(frame),as.integer(perctg*nrow(frame)))
  tr <- prueba_global[sp,]
  ts <- prueba_global[-sp,]
}

#----------- Workflow for predection task  ------------

# Other global to experiment

prueba_test <- global_test
prueba_train <- global_train
#prueba_global <- rbind(global_test,global_train)
prueba_global <- select(global_train,-client_id,-loan_id,-district_id,-antiq_card,-type.y,-antiq_card,-name,-region )

#str(prueba_global)
#prueba_global$id <- as.factor(prueba_global$id)
#describe(prueba_global)

#Cross Validation Performance Estimation Experiment with SVM 

r1 <- performanceEstimation(
  PredTask(status ~ ., prueba_global),
  Workflow(learner="svm"),
  EstimationTask(metrics="err", method=CV())
)
plot(r)
summary(r)

#mas fancy 
r2 <- performanceEstimation(
  PredTask(status ~ .,prueba_global),
  workflowVariants(learner="svm",
                   learner.pars=list(cost=1:5,gamma=c(0.1,0.01))),
  EstimationTask(metrics="mse",method=CV()))
summary(r2)
plot(rs2)
topPerformers(r2)


#a pata svm
s1 <- svm(status ~ .,tr)
ps <- predict(s1,ts)
table(ps,ts$status)
regr.eval(ts$status,ps)

#a pata mars
mars <- earth(status ~ .,tr)
ps2 <- predict(mars,ts)
(mae <- mean(abs(ts$status - ps2)))

#Cross Validation Performance Estimation Experiment with rpart 

r3 <- performanceEstimation(
  PredTask(status ~ ., prueba_global),
  Workflow(learner="rpartXS"),
  EstimationTask(metrics="err", method=CV())
)
plot(r)
summary(r)

#otro

res3 <- performanceEstimation(
  PredTask(status ~ ., prueba_global),
  workflowVariants("standardWF",
                   learner=c("svm","randomForest")),
  EstimationTask(metrics="err",method=CV(nReps=2,nFolds=5)))





#- Eliminar ultimas columnas (antiq_card y type de tarjeta de credito) Consultar con Daniela
# Si se va a eliminar, para eso no hacer el join con card_train
#- En el paso "Cleaning some features", agregar columna de reg_perf
#- Cambiar nombre de reg_perf a by_region
#- Quitar disctrict_id de global_test y train


# For cleaning the workspace
# closeAllConnections()
# rm(list=ls())