
features <- c(
    "loan_id",
    "owners",
    "amount",
    "duration",
    "average.salary",
    "status"
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
tr <- 0.65

# Translating P to labels
res <- ifelse(res > tr,1,-1)

# Preparing res to create the file
res <- data.frame(Id=names(res),Predicted=res)

# Uncomment when using loan_train for both training and testing
#loan_train$Id <- rownames(loan_train)

loan_test$Id <- rownames(loan_test) # If we are using Kaggle
res$Id <- rownames(res)
res <- merge(res,loan_test,by="Id")[c("loan_id","Predicted")]
colnames(res)<- c("Id","Predicted")
# Uncomment when using loan_train for both training and testing
#res <- merge(res,loan_train,by="Id")[c("loan_id","Predicted","status")]

# Confidence matrix (Uncomment when using loan_train for both training and testing)
#confMatrix(res)

# Writing the prediction
write.table(res,file="~/prediction6_p.csv" ,row.names=FALSE,sep=",")

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
