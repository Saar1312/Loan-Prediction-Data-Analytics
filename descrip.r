"
Faculty of Engineering. University of Porto
Knowledge Extraction and Machine Learning
Authors: Samuel Arleo & Daniela Socas

Last modified 29/12/2016
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
  install.packages("randomForest")
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
if(! "MASS" %in% rownames(installed.packages())){
  install.packages("MASS")
}

library(lubridate)
library(ggplot2)
library(party)
library(rpart)
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
library(MASS)
#-------------------- LOADING FUNCTIONS ----------------------

#wd <- "C:/Users/Dusady/Documents/Dan/UP/ML/Loan-Prediction-Data-Analytics"
wd <- "~/Mineria/"

#source(paste(wd, "descrip_fun.r", sep=""))

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
trans_train<-toNumeric(trans_train,c("amount"))
trans_test<-toNumeric(trans_test,c("amount"))

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

# Adding number of owners to each account and deleting status columns
t <- table(disp$account_id,disp$type)
df <- data.frame(account_id=rownames(t),owners=(t[,1]+t[,2]))
disp <- merge(df,disp,by="account_id")
disp <- disp[!(disp$type == "DISPONENT"),][c(1:4)]

# Creating a global tables that matche users with their accounts, loans, districts and credit cards
# to start the mining process

# Changing district table id from code to district_id to join with client table
colnames(district)[1]<-"district_id"

# Matching users with accounts
global_train <- merge(disp, client[,!(names(client)%in%c("birth_number"))],by="client_id")
global_test <- merge(disp, client[,!(names(client)%in%c("birth_number"))],by="client_id")

# Matching users and their accounts with loans
global_test <- merge(global_test,loan_test,by="account_id")
global_train <- merge(global_train,loan_train,by="account_id")

# Matching users with their districts
global_test <- merge(global_test,district,by="district_id")
global_train <- merge(global_train,district,by="district_id")

# Matching users with credit cards
# The number of users with loans that also have cards doesn't seem to be representative
#global_test <- merge(global_test,card_test,by="disp_id", all.x=TRUE)
#global_train <- merge(global_train,card_train,by="disp_id", all.x=TRUE)

#------------ Adding Features ------------

# Data frame with the ids of accounts with loans 
acc_loan_tr <- data.frame(account_id=unique(global_train$account_id))
acc_loan_ts <- data.frame(account_id=unique(global_test$account_id))

# Matching transactions with accounts
trans_users_tr <- merge(acc_loan_tr, trans_train, by="account_id")
trans_users_ts <- merge(acc_loan_ts, trans_test, by="account_id")

# Adding new column with means of account balances to the frame of accounts with loans
acc_loan_tr$balance_mean <- applyOper(trans_users_tr,c("account_id","balance"),1)
acc_loan_ts$balance_mean <- applyOper(trans_users_ts,c("account_id","balance"),1)

# Adding new column with stand. deviation of account balances to the frame of accounts with loans
acc_loan_tr$balance_sd <- applyOper(trans_users_tr,c("account_id","balance"),2)
acc_loan_ts$balance_sd <- applyOper(trans_users_ts,c("account_id","balance"),2)

# Adding new column with max of account balances to the frame of accounts with loans
acc_loan_tr$balance_min <- applyOper(trans_users_tr,c("account_id","balance"),3)
acc_loan_ts$balance_min <- applyOper(trans_users_ts,c("account_id","balance"),3)

# Adding new column with min of account balances to the frame of accounts with loans
acc_loan_tr$balance_max <- applyOper(trans_users_tr,c("account_id","balance"),4)
acc_loan_ts$balance_max <- applyOper(trans_users_ts,c("account_id","balance"),4)

# Matching each user in global tables with the information of his account
global_test <- merge(global_test,acc_loan_ts,by="account_id")
global_train <- merge(global_train,acc_loan_tr,by="account_id")

#--------- Deleting some features ---------

# Deleting features that don't give important information (columns with 
# too many NAs, IDs, etc) or that are not representative to the model

# Checking types, classes and NAs of global tables
# Global Train table
checkNa(list(global_train))
checkClasses(list(global_train))
checkTypes(list(global_train))

# Global Test table
checkNa(list(global_test))
checkClasses(list(global_test))
checkTypes(list(global_test))

#--------------------------- DESCRIPTION ----------------------------

# Avoiding printing plots
describe <- FALSE

if(describe){
  # Boxplot: Age vs Status
  boxplot(age~status,data=global_train)

  # Boxplot: Age vs Status by genders (0: Male 1: Female)
  boxplot(age~status,data=global_train[global_train$gender == "0",])
  boxplot(age~status,data=global_train[global_train$gender == "1",])

  # Contingency table: gender vs status
  c1 <- table(global_train$gender,global_train$status)

  # Barplot: gender vs status
  barplot(c1,main = "Gender-Status frequencies")

  # Some plots to understand data features
  barplot(table(loan_train$duration))
  barplot(table(loan_train$status))
  hist(loan_train$amount)
  plot(loan_train$amount,loan_train$status)
  boxplot(amount~status,data=loan_train)	# The more amount to granted, the more likely to fraud
  plot(loan_train$payments,loan_train$status)
  boxplot(payments~status,data=loan_train) # The more months to pay, the more likely to fraud

  # Campare each feature with status using plots
  status_compare(global_train)
}

#--------------------------- PREDICTION ----------------------------

features <- c(
    "loan_id","owners","gender", "age","amount", "duration","payments", 
    "status","current_time","region", "no..of.inhabitants", 
    "no..of.municipalities.with.inhabitants...499", "no..of.municipalities.with.inhabitants.500.1999", 
    "no..of.municipalities.with.inhabitants.2000.9999", "no..of.municipalities.with.inhabitants..10000",    
    "no..of.cities", "ratio.of.urban.inhabitants","average.salary" ,"unemploymant.rate..95","unemploymant.rate..96",
    "no..of.enterpreneurs.per.1000.inhabitants", "no..of.commited.crimes..95", "no..of.commited.crimes..96",
    "balance_mean", "balance_sd","balance_min", "balance_max"                        
)

# Selecting relevant features
global_test <- global_test[features]
global_train <- global_train[features]
globals <- list(train=global_train,test=global_test)

# Splits data when using loan_train for both training and testing
# globals <- get_sample(global_train,70)

#---------- Logistic regression ------------

model_reg <- glm(status~.,family=binomial(link="logit") ,data = globals$train)

# Analysing model to filter again features
summary(model_reg)
anova(model_reg, test="Chisq")

# Applying model                change globals[2] to global_test
res <- predict(model_reg,newdata=globals$test,type='response')

# Threshold: p>=tr --> status=1 and p<tr status=-1 
tr <- 0.50

# Translating P to labels
res <- ifelse(res > tr,1,-1)

# Preparing res to create the file
res <- data.frame(Id=names(res),Predicted=res)

# Uncomment when using loan_train for both training and testing
#loan_train$Id <- rownames(loan_train)

loan_test$Id <- rownames(loan_test) # If we are using Kaggle
res$Id <- rownames(res)
res <- merge(res,loan_test,by="Id")[c("loan_id","Predicted","status")]

# Uncomment when using loan_train for both training and testing
#res <- merge(res,loan_train,by="Id")[c("loan_id","Predicted","status")]

# Confidence matrix
confMatrix(res)

# Writing the prediction
write.table(res,file="prediction.csv" ,row.names=FALSE,sep=",")

#----------- Workflow for predection task  ------------

#prueba_global <- rbind(global_test,global_train)
#prueba_global <- select(global_train,-client_id,-loan_id,-district_id,-antiq_card,-type.y,-antiq_card,-name,-region )
str(prueba_train)
prueba_train$id <- as.integer(prueba_train$id)
describe(prueba_train)

corr2 <- rcorr(as.matrix(prueba_train))
corr
#lda

l <- lda(status ~ ., tr)
preds <- predict(l,ts)
preds
(mtrx <- table(preds$class,ts$status))
(err <- 1-sum(diag(mtrx))/sum(mtrx))

#tree based model

mtree <- rpartXse(status  ~ ., global_train)
predtree <- predict(mtree,global_test, type = 'class')
mc <- table(predtree,global_test$status)
err <- (1-sum(diag(mc))/sum(mc))
err
describe(predtree)

#NBayes
nb <- naiveBayes(status ~ ., global_train,laplace=1)
(mtrx <- table(predict(nb,global_test),global_test$status))
(err <- 1-sum(diag(mtrx))/sum(mtrx))

#Knn

nn3 <- kNN(status ~ .,tr,ts,k=5,norm=TRUE)
(mtrx <- table(nn3,ts$status))
(err <- 1-sum(diag(mtrx))/sum(mtrx))

#ann
nn <- nnet(status ~ .,tr,size=5, decay =0.1, maxit=1000)
(mtrx2 <- table(predict(nn,ts,type='class'),ts$status))
summary(nn)

#Cross Validation Performance Estimation Experiment with SVM 

r1 <- performanceEstimation(
  PredTask(status ~ ., prueba_train),
  Workflow(learner="svm"),
  EstimationTask(metrics="err", method=CV())
)
plot(r1)
summary(r1)

#mas fancy 
r2 <- performanceEstimation(
  PredTask(status ~ .,prueba_train),
  workflowVariants(learner="svm",
                   learner.pars=list(cost=1:5,gamma=c(0.1,0.01))),
  EstimationTask(metrics="err",method=CV()))
summary(r2)
plot(r2)
topPerformers(r2)


#a pata svm
msvm <- svm(status ~ .,tr)
predsvm <- predict(msvm,ts)
table(predsvm,ts$status)

#a pata mars
mars <- earth(status ~ .,tr)
predmars <- predict(mars,ts)
(mae <- mean(abs(ts$status - predmars$class)))

#Cross Validation Performance Estimation Experiment with rpart 

r3 <- performanceEstimation(
  PredTask(status ~ ., prueba_global),
  Workflow(learner="rpartXS"),
  EstimationTask(metrics="err", method=CV())
)
plot(r)
summary(r)

#random forest

mrand <- randomForest(status ~ ., tr)
predrand <- predict(mrand,ts)
describe(predrand)
table(predrand,ts$status)

install.packages("pROC")
library(pROC)
auc(ts$status, predrand)

#otro

res3 <- performanceEstimation(
  PredTask(status ~ ., prueba_train),
  workflowVariants("standardWF",
                   learner=c("svm","randomForest","rpartXse"),
                   predictor.pars=list(type="class")),
  EstimationTask(metrics="err",method=CV(nReps=2,nFolds=5)))

summary(res3)
plot(res3)


#- Eliminar ultimas columnas (antiq_card y type de tarjeta de credito) Consultar con Daniela
# Si se va a eliminar, para eso no hacer el join con card_train
#- En el paso "Cleaning some features", agregar columna de reg_perf
#- Cambiar nombre de reg_perf a by_region
#- Quitar disctrict_id de global_test y train


# For cleaning the workspace
# closeAllConnections()
# rm(list=ls())