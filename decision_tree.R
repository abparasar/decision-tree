
#-------------importing the data ------------------------
setwd('C:/Users/HP/Desktop/edu/Decision-Tree-20200427T154718Z-001/Decision-Tree/data')

loan_data <- read.csv('loan_approval.csv')

View(loan_data)
head(loan_data)
str(loan_data)

# convert credibility column to factor type from int type to tell the rpart function that credibility column has 2 values i.e 0 and 1
loan_data$Creditability = as.factor(loan_data$Creditability)

str(loan_data)

##--------------- Split data into training (70%) and validation (30%)--------------------
dt = sort(sample(nrow(loan_data), nrow(loan_data)*.7))
train<-loan_data[dt,]
val<-loan_data[-dt,] 
nrow(train)
nrow(val)


#-------------------make a base model without pruning-------------------
library(rpart)
tree_base_model <- rpart(Creditability~., data = train, method="class")
tree_base_model

library(rattle)
library(rpart.plot)

# plot the tree
fancyRpartPlot(tree_base_model)

#print the complexity parameter
printcp(tree_base_model)
plotcp(tree_base_model)

#do the prediction on test data
val$pred <- predict(tree_base_model, val, type = "class")

#calculate accuracy
base_accuracy <- mean(val$pred == val$Creditability)
base_accuracy

#--------------pre pruning --------------------
tree_preprocess_model <- rpart(Creditability~., data = train, method="class", control = rpart.control(minsplit = 10, minbucket = 3, maxdepth = 15, cp=0, usesurrogate = 2, xval =10 ))

fancyRpartPlot(tree_preprocess_model)
# Compute the accuracy of the base tree
val$pred <- predict(tree_preprocess_model, val, type = "class")
preprocess_accuracy <- mean(val$pred == val$Creditability)
# Checking the accuracy
preprocess_accuracy


#--------------------post pruning----------------
#identifying the best complexity parameter
printcp(tree_base_model)
# catch the best cp value where xerror is min
bestcp <- tree_base_model$cptable[which.min(tree_base_model$cptable[,"xerror"]),"CP"]
# Prune the tree using the best cp.

#use prune at best cp for post pruned model
tree_postprocess_model <- prune(tree_base_model, cp = bestcp)



fancyRpartPlot(tree_postprocess_model)


# Compute the accuracy of the pruned tree
val$pred <- predict(tree_postprocess_model, val, type = "class")
postprocess_accuracy <- mean(val$pred == val$Creditability)
# Checking the accuracy
postprocess_accuracy



#--------------------View ROC curve and cal AUC-------------------
#Validating on the train data
#Scoring
library(ROCR)
val1 = predict(tree_postprocess_model, val, type = "prob")
nrow(val1)
#Storing Model Performance Scores by changing cuttoff values
pred_val <-prediction(val1[,2],val$Creditability)
# Calculating True Positive and False Positive Rate alpha is cutoff
perf_val <- performance(pred_val, "tpr", "fpr")
# Plot the ROC curve lwd = line width
plot(perf_val, col = "green", lwd = 5)
# Calculating Area under Curve
aucVal <- performance(pred_val,"auc")
aucVal
