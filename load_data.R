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
path <- "./Data/"

# Values of empty fields in .csv files
na_values <- c(""," ","NA","?")

loadData( path ,na_values)

# Putting all tables in a list for passing them to checkNA
dframes <- list(account, card_test, card_train, client, disp, district, loan_test, 
                loan_train, trans_test, trans_train)

names(dframes) <- c("account", "card_test", "card_train", "client", "disp", "district", 
                    "loan_test", "loan_train", "trans_test", "trans_train")

