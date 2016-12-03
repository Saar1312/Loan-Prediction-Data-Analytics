"Extradition of knowledge and Machine Learning
Samuel Arleo & Daniela Socas
Last modified 12/02/2016
Descriptive functions to start the project and pre-processing"

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

# Putting al tables in a list for passing them to checkNA
dframes <- list.files(path)

# Checking number of NA field for each table
checkNa(dframes)

#------------------ DATES -------------------------

#Date columns in date POSIX type better than int 

account$date <-  ymd(account$date)
card_train$issued <- ymd(card_train$issued)
card_test$issued <- ymd(card_test$issued)
loan_train$date <- ymd(loan_train$date)
loan_test$date <- ymd(loan_test$date)
trans_train$date <- ymd(trans_train$date)
trans_test$date <- ymd(trans_test$date)

#Have to change the +50 months 
client$birth_number <- ymd(client$birth_number)


#------------------ CLEANING AGE/GENDER -------------------------

# Max date on client$birth_number, card$issued to avoid considering dates like 11-01-01 as 2011-01-01 but 1911-01-01
# because data is older than 
refDate <- max(client$birth_number,card_train$issued,card_test$issued,account$date,
               trans_train$date,trans_test$date,loan_test$date,loan_train$date)


# Adding new columns of gender and age to client table
client$gender<-unlist(lapply(client$birth_number,getGender))
client$age<-unlist(lapply(client$birth_number,getAntiquity,refDate))

#Exact age 
client$age<-round(client$age)

# Joining disp (table that maps clients IDs to accounts IDs) and client tables by client_id
m1 = mergeTables(disp[,c("client_id","account_id")], client[,c("client_id","gender","age")],"client_id")

# Joining m1 with loan data by account_id
m2 = mergeTables(m1,loan_train[,c("account_id","status")],"account_id")

#------------------ PLOTS -------------------------

#---------- Account ----------
#Most frequent by far is monthly issuance
plot(account$frequency)

# Age vs Status
boxplot(age~status,data=m2)

# By gender
boxplot(age~status,data=m2[m2$gender == "0",])
boxplot(age~status,data=m2[m2$gender == "1",])

# Contingency table for gender vs status
c1 <- table(m2$gender,m2$status)

# Barplot gender vs status
barplot(c1,main = "Gender-Status frequencies")

#---------- Card -------------

#Frequency types card
plot(card_train.csv$type)

#---------- Client ------------
#We can see how the 1st district has more people
hist(client$district_id)

hist(client$age)

#---------- Disp --------------
#Way more OWNERs 
plot(disp$type)


#---------- Loan --------------

hist(loan_train$duration)
plot(loan_train$status)
hist(loan_train$amount)
plot(loan_train$status,loan_train$amount)
plot(loan_train$payments,loan_train$status)

#---------- Trans -------------

#Most are withdrawals and then credit. 
plot(trans_train$type)
plot(trans_train$operation)
hist(trans_train$amount)

str(trans_train)
summary(trans_train)
#------------------ OUTLIERS -------------------------

# Esto es para ver los outliers de las columnas numericas importantes
# Probar al final el modelo entrenado con y sin outliers
#boxplot(district[,4:16])
#boxplot(loan_train$amount)

