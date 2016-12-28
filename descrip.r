
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

library(lubridate)
library(ggplot2)
library(party)
library(rpart)

################################## LOAD FUNCTIONS ####################################

#wd <- "C:/Users/Dusady/Documents/Dan/UP/ML/Loan-Prediction-Data-Analytics"
wd <- "~/Mineria/"

source(paste(wd, "descrip_fun.r", sep=""))

##################################### LOAD DATA ######################################

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

################################### TO NUMERIC #######################################

trans_train$balance <- as.numeric(as.character(trans_train$balance))
trans_train$amount <- as.numeric(as.character(trans_train$amount))
trans_test$balance <- as.numeric(as.character(trans_test$balance))
trans_test$amount <- as.numeric(as.character(trans_test$amount))
district$unemploymant.rate..95 <- as.numeric(district$unemploymant.rate..95)

# Filling NAs with mean
mean_crimes <- mean(district$no..of.commited.crimes..95[!is.na(district$no..of.commited.crimes..95)])
district$no..of.commited.crimes..95[is.na(district$no..of.commited.crimes..95)] <- mean_crimes
mean_unemp <- mean(district$unemploymant.rate..95[!is.na(district$unemploymant.rate..95)])
district$unemploymant.rate..95[is.na(district$unemploymant.rate..95)] <- mean_unemp

# Setting max date to avoid considering 11-01-01 as 2011-01-01 for instance
refDate <- max(client$birth_number,card_train$issued,card_test$issued,account$date,
               trans_train$date,trans_test$date,loan_test$date,loan_train$date)

############################## TAKING INFO FROM DATES #################################
# Adding more meaningful columns to tables based on dates:
#	TABLE 		| COLUMNS
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

################################ CHANGING DATES FORMAT ################################
# Date columns in date POSIX type better than int 

account$date <-  ymd(account$date)
card_train$issued <- ymd(card_train$issued)
card_test$issued <- ymd(card_test$issued)
loan_train$date <- ymd(loan_train$date)
loan_test$date <- ymd(loan_test$date)
trans_train$date <- ymd(trans_train$date)
trans_test$date <- ymd(trans_test$date)
client$birth_number <- ymd(unlist(lapply(client$birth_number,formatDate)))

################################ global USERS TABLE ################################
# global table matches users with their accounts, loans, districts and credit cards
# to avoid join operations

# Changing district table id from code to district_id to join with client table
colnames(district)[1]<-"district_id"

# Matching users with accounts
global_train <- merge(disp, client[,!(names(client)%in%c("birth_number"))],by="client_id")

# Matching users and their accounts with loans
global_test <- merge(global_train,loan_test,by="account_id")
global_train <- merge(global_train,loan_train,by="account_id")
dim(global_train)
dim(global_test)
# Matching users with their districts
global_train <- merge(global_train,district,by="district_id")
global_test <- merge(global_test,district,by="district_id")
dim(global_train)
dim(global_test)
# Matching users with credit cards
global_train <- merge(global_train,card_train,by="disp_id", all.x=TRUE)
global_test <- merge(global_test,card_test,by="disp_id", all.x=TRUE)
dim(global_train)
dim(global_test)
# Taking out unnecessary columns
global_train <- subset(global_train, select = c(2,4:8,10:29,31,33))
global_test <- subset(global_test, select = c(2,4:8,10:29,31,33))

# Changing type to numerical
global_train[13:24]<-lapply(global_train[13:24],as.numeric)
global_test[13:24]<-lapply(global_test[13:24],as.numeric)

# Changing numeric variables to  numerical
#global_train[,c(13:24)]<-sapply(global_train[,c(13:24)],as.numeric)

# Changing cate variables to  numerical
#global_train[,c(1:4,9,11,12,26)]<-sapply(global_train[,c(1:4,9,11,12,26)],as.factor)


# Checking NAs
sapply(global_train,function(y) sum(is.na(y)))


################################ ADDITIONAL FEATURES ################################
# Getting additional features from existing attributes in global_train table

# Impact of district over loan status
reg_perf <- table(global_train$district_id,global_train$status)
reg_perf <- reg_perf + 1
avg <- mean(reg_perf[,1]+reg_perf[,2])
reg_perf<-(reg_perf[,1]-reg_perf[,2])/avg
reg_perf <- as.data.frame(reg_perf)
reg_perf$district_id<-rownames(reg_perf)
global_train<-merge(global_train,reg_perf, by="district_id")

# Taking out district_id column
global_train[,!(names(global_train)%in%c("district_id"))]

# Impact of district over loan status
avg_perf <- mean(reg_perf$reg_perf)
global_test<-merge(global_test,reg_perf, by="district_id", all.x=TRUE)
global_test$reg_perf[is.na(global_test$reg_perf)] <- avg_perf

# Taking out district_id column
global_test[,!(names(global_test)%in%c("district_id"))]

# Cuando se tenga un modelo, probar si este atributo tiene impacto sobre la prediccion
# Hacer funcion que automatice esto para cualquier feature nominal como type.x
# Sacar matriz de correlaciones cuando se tengan todos los atributos numericos, para saber
# por ejemplo si el numero de prestamos exitosos o no exitosos por region esta correlacionado
# pero por provincias, asi se quita una de las dos si hay mucha correlacion


###################################### PLOTS/TABLES ########################################

#--------------------- USER -----------------------

# Boxplot: Age vs Status
boxplot(age~status,data=global_train)

# Boxplot: Age vs Status by genders (0: Male 1: Female)
boxplot(age~status,data=global_train[global_train$gender == "0",])
boxplot(age~status,data=global_train[global_train$gender == "1",])

# Contingency table: gender vs status
c1 <- table(global_train$gender,global_train$status)

# Barplot: gender vs status
barplot(c1,main = "Gender-Status frequencies")

#--------------------- Account -----------------------
#Most frequent by far is monthly issuance
plot(account$frequency)

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
#summary(trans_train)
#------------------ OUTLIERS -------------------------

# Esto es para ver los outliers de las columnas numericas importantes
# Probar al final el modelo entrenado con y sin outliers
#boxplot(district[,4:16])
#boxplot(loan_train$amount)

# For cleaning the workspace
# closeAllConnections()
# rm(list=ls())

#################################### PREDICTIVE SECTION #####################################

#----------- Decision tree ------------

# Esto despues lo cambiamos a otro archivo

#model <- rpart( status ~ ., data=global_train ) 

#summary(model)
#plot(model)
#text(model)

# - Revisar que modelos hay
# - Revisar como hacer un ensemble
# - Hace falta validar el modelo por ejemplo con validacion cruzada?
# * Hacer cleaning luego del merge creando tabla global_train es bueno en algunos aspectos
# pero si hay que llenar un NA con una media se tiene que hacer antes del merge.

#----------- Logistic regression ------------

global_train$status<-factor(global_train$status)
global_train$gender<-factor(global_train$gender)
global_test$gender<-factor(global_test$gender)

global_train$id<-rownames(global_train)
global_test$id<-rownames(global_test)

#global_train$type.y<-factor(unlist(lapply(global_train$type.y,function(x) ifelse(is.na(x),0,1))))
# antiq_card no vale la pena usarla porque solo 10 rows la tienen, igual que type.y (esos son dos atributos de tarjetas de credito)
# Name da problemas, creo que porque son demasiados.
model_reg <- glm(status~.,family=binomial(link="logit") ,data = global_train[,!(colnames(global_train)%in%c("id","client_id","loan_id","district_id","antiq_card","type.y","antiq_card","name"))])

res <- predict(model_reg,newdata=global_test[,!(colnames(global_test)%in%c("id","client_id","loan_id","district_id","antiq_card","type.y","antiq_card","name"))],type='response')

res <- as.data.frame(res)
names(res) <- "p"
res$id<-rownames(res)
res$loan_id <- merge(global_test,res,by="id")$loan_id
res$status <- ifelse(res$p > 0.65,1,-1)

t <- table(res$loan_id,res$status)
t2 <- data.frame(loan_id = rownames(t),bad = t[,1], good=t[,2])
t2$status<-ifelse(t2$good>=t2$bad,1,-1)
rownames(t2) <- NULL
write.table(t2[c("loan_id","status")],file="prediction.csv" ,col.names = c("Id","Predicted"),row.names=FALSE,sep=",")
#misClasificError <- mean(fitted.results != test$Survived)
#print(paste('Accuracy',1-misClasificError))


#model_reg <- glm(status~.,family=binomial(link="logit") ,data = global_train[,c("age","duration","status","gender","amount","reg_perf")])
