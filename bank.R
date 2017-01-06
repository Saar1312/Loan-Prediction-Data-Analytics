###################################
#           Prediction            #
###################################

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
require(Hmisc)
library(MASS)

#----------- Data set ------------# 

beta_test = read.csv("~/Dan/UP/ML/Loan-Prediction-Data-Analytics/global_test.csv", header = TRUE)
beta_train = read.csv("~/Dan/UP/ML/Loan-Prediction-Data-Analytics/global_train.csv", header = TRUE)

# List of features
columns<- c(
  "loan_id","owners","gender", "age","date","amount", "duration","payments", 
  "status","current_time", "name","region", "no..of.inhabitants", 
  "no..of.municipalities.with.inhabitants...499", "no..of.municipalities.with.inhabitants.500.1999", 
  "no..of.municipalities.with.inhabitants.2000.9999", "no..of.municipalities.with.inhabitants..10000",    
  "no..of.cities", "ratio.of.urban.inhabitants","average.salary" ,"unemploymant.rate..95","unemploymant.rate..96",
  "no..of.enterpreneurs.per.1000.inhabitants", "no..of.commited.crimes..95", "no..of.commited.crimes..96",
  "balance_mean", "balance_sd","balance_min", "balance_max"                        
)

features <- c("loan_id","owners", "age","amount", "duration","payments", 
              "status","ratio.of.urban.inhabitants","average.salary" ,"balance_min", "balance_max")

#0.8
features <- c("loan_id","owners", "age","amount", "duration","payments","status","ratio.of.urban.inhabitants","average.salary" ,
              "balance_min","balance_max","credit","withdrawal","withdrawal.cash","collections","creditws","creditcs","remittances")                                          

features <- c("owners","amount","payments","status","credit","withdrawal","creditcs","remittances","collections")                                          

features <- c("owners","payments","status","creditcs","remittances")                                          

#0.76
features <- c("status","amount","owners","payments","remittances","collections","creditcs","balance_max","balance_mean")                      

#0.75
features <- c("status","owners","payments","remittances","collections","creditcs","balance_mean")                        

#features <- c("status","owners","payments","remittances","collections","balance_mean") 

#step
features <- c("owners","payments","status" ,"balance_mean","balance_min")
#randf
features <- c("owners","payments","duration","average.salary","status" )

#
features <- c("owners","payments","status","balance_max")

beta_train <- toFactor(beta_train,c("status"))
beta_test <- toFactor(beta_test,c("status"))


prueba_train <- beta_train[features]
prueba_test <- beta_test[features]
str(beta_train)

frame <- prueba_train
prct <- 0.7
set.seed(123)
sp <- sample(1:nrow(frame),as.integer(prct*nrow(frame)))
tr <- frame[sp,]
ts <- frame[-sp,]

#-Kaggle-

prueba_test <- global_test[features]
prueba_train <- global_train[features]
tr <- prueba_train
ts <- prueba_test



#------------ Models -------------# 

###############
#     GLM-     #
###############

glm_m3 <- glm(status ~ .,family=binomial, tr)
glm_m
glm_m
glm_m3 <- step(glm_m3)
summary(glm_m)
?step()
glm_m2$y 

glm_preds3 <- predict(glm_m3, ts,type='response')
describe(glm_preds3)
glm_preds3
boxplot(glm_preds)
glm_preds3$anova

thr <- 0.23
res = ifelse(glm_preds3 > thr,1,-1)
describe(res)

plot(res)

#cross validation 
require(boot)
cv.err <- cv.glm(tr, glm_m3)$delta
cv.err

0.65
###################
# Decision Tree-2 #
###################
require(DMwR)
require(rpart.plot)

tree_m <- rpartXse(status ~ ., tr)
tree_pred <- predict(tree_m,ts, type = "class")
describe(tree_pred)
prp(tree_m,type=4,extra=101)

printcp(tree_m)
plotcp(tree_m)

0.65901
###################
# Random Forest-3 #
###################
require(randomForest)
require(rpart.plot)

?randomForest()
randf_m <- randomForest(status ~ ., tr,importance=T)
importance(randf_m)
randf_pred <- predict(randf_m,ts)
varImpPlot(randf_m,main="Feature Relevance Scores")
describe(randf_pred)
randf_m$confusion

plot(randf_m, ylim=c(0,0.2))
legend('topright', colnames(randf_m$err.rate), col=1:3, fill=1:3)

###################
#      SVM-4      #
###################

svm_m <- svm(status ~ . , tr)
svm_pred <- predict(svm_m,ts)
describe(svm_pred)

###################
#  Naive Bayes-5  #
###################

nb_m <- naiveBayes(status ~ ., tr,laplace=1)
nb_pred <- predict(nb,ts)
describe(nb_pred)


###################
#      ANN-6      #
###################
require(nnet)

nn_m <- nnet(status ~ .,tr,size=5, decay =0.6, maxit=1000)
nn_pred <- predict(nn_m,ts,type='class')
describe(nn_pred)

###################
#    k-nearest    #
###################

#--------- Estimation P ----------# 
require(performanceEstimation)
require(e1071)
require(randomForest)
require(DMwR)


res3 <- performanceEstimation(
  PredTask(status ~ ., tr),
  workflowVariants("standardWF",
                   learner=c("svm","randomForest","rpartXse","naiveBayes"),
                   predictor.pars=list(type="class")),
  EstimationTask(metrics="err",method=CV(nReps=2,nFolds=5)))

summary(res3)
plot(res3)


#---------- Submission -----------#
pred <- randf_pred
submission <- data.frame(Id = beta_test$loan_id, Predicted = pred)
write.table(submission,file="prediction_rfrf.csv" ,col.names = TRUE ,row.names=FALSE,sep=",")


