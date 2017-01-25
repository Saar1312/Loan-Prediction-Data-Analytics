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
#client$birth_number <- ymd(unlist(lapply(client$birth_number,formatDate)))

#---------- Joining data frames -----------

# Adding number of owners to each account and deleting status columns
t <- table(disp$account_id,disp$type)
df <- data.frame(account_id=rownames(t),owners=(t[,1]+t[,2]))
disp <- merge(df,disp,by="account_id")
disp <- disp[!(disp$type == "DISPONENT"),][c(1:4)]

# Creating global tables that matche users with their accounts, loans, districts and credit cards
# to start the mining process

# Changing district table id from code to district_id to join with client table
colnames(district)[1]<-"district_id"

# Matching users with disposition
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
global_test <- merge(global_test,card_test,by="disp_id", all.x=TRUE)
global_train <- merge(global_train,card_train,by="disp_id", all.x=TRUE)


# Matching users with accounts
global_train <- merge(global_train, account,by="account_id",all.x=TRUE)
global_test <- merge(global_test, account,by="account_id",all.x=TRUE)


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

######################################################################

# Impact of district over loan status

# Computing ratio of each region
#global_train<-featureRatio(global_train, "name", "status")
#global_train<-featureRatio(global_train, "region", "status")

# Using credit card data
#global_train[!is.na(global_train$card_id),]$card_id <- 1
#global_train[is.na(global_train$card_id),]$card_id <- 0

# Adding frequency ratio
#global_train<-featureRatio(global_train, "frequency", "status")

# Adding transaction types frequencies
tr_type<-table(trans_train$account_id,trans_train$type)
credits<-tr_type[1:(length(tr_type)%/%3)]
withd<-tr_type[(1+length(tr_type)%/%3):((2*length(tr_type))%/%3)]
withdc<-tr_type[(1+((2*length(tr_type))%/%3)):length(tr_type)]
tr_type<-data.frame(credit=credits,withdrawal = withd, withdrawal.cash=withdc)
tr_type$account_id<-rownames(table(trans_train$account_id,trans_train$type))

global_train<-merge(global_train,tr_type, by="account_id",all.x=TRUE)

tr_type<-table(trans_test$account_id,trans_test$type)
credits<-tr_type[1:(length(tr_type)%/%3)]
withd<-tr_type[(1+length(tr_type)%/%3):((2*length(tr_type))%/%3)]
withdc<-tr_type[(1+((2*length(tr_type))%/%3)):length(tr_type)]
tr_type<-data.frame(credit=credits,withdrawal = withd, withdrawal.cash=withdc)
tr_type$account_id<-rownames(table(trans_test$account_id,trans_test$type))

global_test<-merge(global_test,tr_type, by="account_id",all.x=TRUE)

# Transactions operations
tr_oper<-table(trans_train$account_id,trans_train$operation)
collection<-tr_oper[1:(length(tr_oper)%/%5)]
creditw<-tr_oper[(1+length(tr_oper)%/%5):((2*length(tr_oper))%/%5)]
creditc<-tr_oper[(1+2*length(tr_oper)%/%5):((3*length(tr_oper))%/%5)]
remittance<-tr_oper[(1+3*length(tr_oper)%/%5):((4*length(tr_oper))%/%5)]
withd <-tr_oper[(1+((4*length(tr_oper))%/%5)):length(tr_oper)]

tr_oper<-data.frame(collections=collection,creditws = creditw, creditcs=creditc, remittances=remittance, withds=withd)
tr_oper$account_id<-rownames(table(trans_train$account_id,trans_train$operation))

global_train<-merge(global_train,tr_oper, by="account_id",all.x=TRUE)


tr_oper<-table(trans_test$account_id,trans_test$operation)
collection<-tr_oper[1:(length(tr_oper)%/%5)]
creditw<-tr_oper[(1+length(tr_oper)%/%5):((2*length(tr_oper))%/%5)]
creditc<-tr_oper[(1+2*length(tr_oper)%/%5):((3*length(tr_oper))%/%5)]
remittance<-tr_oper[(1+3*length(tr_oper)%/%5):((4*length(tr_oper))%/%5)]
withd <-tr_oper[(1+((4*length(tr_oper))%/%5)):length(tr_oper)]

tr_oper<-data.frame(collections=collection,creditws = creditw, creditcs=creditc, remittances=remittance, withds=withd)
tr_oper$account_id<-rownames(table(trans_test$account_id,trans_test$operation))

global_test<-merge(global_test,tr_oper, by="account_id",all.x=TRUE)

# Region ratio
#global_train<-featureRatio(global_train,"region","status")

# District name ratio
#global_train<-featureRatio(global_train,"name","status")

# Taking out district_id column
#global_train[,!(names(global_train)%in%c("district_id"))]

# Impact of district over loan status
#avg_perf <- mean(reg_perf$reg_perf)
#global_test<-merge(global_test,reg_perf, by="district_id", all.x=TRUE)
#global_test$reg_perf[is.na(global_test$reg_perf)] <- avg_perf

#owners_status <- table(global_train$status, global_train$owners)
#owners_status <- data.frame(owners_status)
#ggplot(owners_status, aes(, y)) +
#  geom_point() +
#  geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)



#ids1<-global_train[global_train$status == 1,][1:46,]$loan_id
#ids2<-global_train[global_train$status == -1,]$loan_id
#ids <- c(ids1,ids2)
#prueba <- global_train[(global_train$loan_id %in% ids),]
#global_train<-prueba

#tmp <- global_train



#features <- c(
#  "loan_id",
#  "owners",
#  "amount",
#  "duration",
#  "average.salary",
#  "status"
#)


#global_test <- global_test[features]
#global_train <- global_train[features]


model_tree <- ctree(status ~ balance_mean + payments + owners + remittances, data = global_train)

png(file = "decision_tree.png")

plot(model_tree)

dev.off()

res = data.frame(id=global_test$loan_id)
res$Predicted = predict(model_tree, newdata=global_test, type="response")
#prob = sapply(predict(model_tree, newdata=global_test,type="prob"),'[[',2)  # obtain probability of class 1 (second element from the lists)
#nodes = predict(model_tree, newdata=global_test, type="node")

#tmp<-global_test

#tmp<-merge(global_train[c("loan_id","status")], global_test, by="loan_id")
#tmp<-global_test[c("loan_id","status.x","predClass","predProb","predNode")]
library(corrplot)                
write.table(res,"prediction.csv",sep=",")

M<-cor(global_train[,c("amount","owners","payments","remittances","collections","credit","balance_max","balance_min")])

write.csv2(file="correlation.csv",x=M)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
#model_reg <- glm(status~,family=binomial(link="logit") ,data = global_train[,!(colnames(global_train)%in%c("id","client_id","loan_id","district_id","antiq_card","type.y","antiq_card","name"))])

#res <- predict(model_reg,newdata=global_test[,!(colnames(global_test)%in%c("id","client_id","loan_id","district_id","antiq_card","type.y","antiq_card","name"))],type='response')

#res <- as.data.frame(res)
#names(res) <- "p"
#res$id<-rownames(res)
#res$loan_id <- merge(global_test,res,by="id")$loan_id
#res$status <- ifelse(res$p > 0.65,1,-1)

#t <- table(res$loan_id,res$status)
#t2 <- data.frame(loan_id = rownames(t),bad = t[,1], good=t[,2])
#t2$status<-ifelse(t2$good>=t2$bad,1,-1)
#rownames(t2) <- NULL
#write.table(t2[c("loan_id","status")],file="prediction.csv" ,col.names = c("Id","Predicted"),row.names=FALSE,sep=",")         
                         