data = read.csv("diabetes_5050.csv") #read data
set.seed(1101)
head(data)
names(data) #get names of variables
attach(data)
diabetescount = table(Diabetes_binary) #double confirm 50:50 ratio
diabetescount

#######for ordinal######### 
table = table(Age, Diabetes_binary)
chisq.test(table)

table = table(GenHlth, Diabetes_binary)
chisq.test(table)

table = table(Education, Diabetes_binary)
chisq.test(table)

table = table(Income, Diabetes_binary)
chisq.test(table)

##########for categorical############
variables_to_analyze = c("HighBP", "HighChol", "CholCheck", "Smoker", "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "DiffWalk", "Sex")

calculate_odds_ratio = function(variable) {
  # Create a binary variable
  binary_variable = ifelse(get(variable) == 1, "yes", "no")
  
  # Create a contingency table
  table_result = table(binary_variable, Diabetes_binary)
  
  # Calculate disease odds ratio
  disease_odds_ratio = table_result[2, 2] / table_result[1, 2] / (table_result[2, 1] / table_result[1, 1])
  
  return(disease_odds_ratio)
}

# Loop through the variables and calculate odds ratios
for (variable in variables_to_analyze) {
  odds_ratio = calculate_odds_ratio(variable)
  cat("Odds ratio for", variable, ":", odds_ratio, "\n")
  
}

########for quantitative##########
boxplot(BMI ~Diabetes_binary)
cor1 = cor.test(BMI, Diabetes_binary)
cor1

boxplot(MentHlth ~ Diabetes_binary)
cor2 = cor.test(MentHlth, Diabetes_binary)
cor2

boxplot(PhysHlth ~Diabetes_binary)
cor3 = cor.test(PhysHlth, Diabetes_binary)
cor3

library(psych)
tabler = table(Diabetes_binary, AnyHealthcare)
phi(tabler)

tabler1 = table(Diabetes_binary, NoDocbcCost)
phi(tabler1)

tabler2 = table(Diabetes_binary,Sex)
phi(tabler2)

tabler3 = table(Diabetes_binary,Smoker)
phi(tabler3)

######################only getting significant variables#######################
drops = c("PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "MentHlth", "Sex", "Smoker", "AnyHealthcare", "NoDocbcCost")
data = data[,!(names(data) %in% drops)]
names(data)

diabetes_data = data[Diabetes_binary == 1, ] #separate diabetes data
non_diabetes_data = data[Diabetes_binary == 0, ] #separate non-diabetes data

######################LOGISTIC REGRESSION################################
set.seed(1101)
library(ROCR)

n_folds <- 5
folds_j <- sample(rep(1:n_folds, length.out = nrow(diabetes_data)))
LoR_acc = numeric(n_folds)
LoR_recall = numeric(n_folds)
LoR_f1 = numeric(n_folds)
LoR_auc = numeric(n_folds)

acc <- numeric(n_folds)

for (j in 1:n_folds) {
  test_1 <- which(folds_j == j)
  train_1 <- which(folds_j != j)
  
  train_data <- rbind(diabetes_data[train_1, ], non_diabetes_data[train_1, ])
  test_data <- rbind(diabetes_data[test_1, ], non_diabetes_data[test_1, ])
  
  LoR = glm(Diabetes_binary~., data = train_data, family = binomial(link="logit"))
  
  LoR_pred = predict(LoR, newdata = test_data[, 2:13], type = "response")
  pred_classes = ifelse(LoR_pred >= 0.5,1,0)
  
  LoR_confusion_matrix = table(pred_classes, test_data[, 1])
  precision_LoR_value = LoR_confusion_matrix[2,2]/(LoR_confusion_matrix[2,2] + LoR_confusion_matrix[1,2])
  recall_LoR_value = LoR_confusion_matrix[2,2]/(LoR_confusion_matrix[2,2] + LoR_confusion_matrix[2,1])
  LoR_acc[j] = sum(diag(LoR_confusion_matrix)) / sum(LoR_confusion_matrix)
  LoR_f1[j] = 2 * (precision_LoR_value * recall_LoR_value) / (precision_LoR_value + recall_LoR_value)
  
  pred=prediction(LoR_pred,test_data[,1])
  auc_val = performance(pred,measure="auc")
  LoR_auc[j]=auc_val@y.values[[1]]
}

LoR_acc
mean(LoR_acc) #0.7456288
precision_LoR_value #0.7605036
recall_LoR_value #0.7329243
LoR_f1
mean(LoR_f1) #0.7505075
LoR_auc
mean(LoR_auc) #0.822202

LoR = glm(Diabetes_binary~., data = train_data)
summary(LoR)
##########################DT############################## 
set.seed(1101)
library("rpart")
library(ROCR)

n_folds = 5

folds_j = sample(rep(1:n_folds, length.out = nrow(diabetes_data)))
DT_acc = numeric(n_folds)
DT_recall = numeric(n_folds)
DT_f1 = numeric(n_folds)
DT_auc = numeric(n_folds)

for (j in 1:n_folds) {
  test_1 = which(folds_j == j)
  train_1 = which(folds_j != j)
  train_data = rbind(diabetes_data[train_1, ], non_diabetes_data[train_1, ])
  test_data = rbind(diabetes_data[test_1, ], non_diabetes_data[test_1, ])
  
  DT = rpart(Diabetes_binary ~ .,
             method = "class", 
             data = train_data, 
             control = rpart.control(minsplit = 5000),
             parms = list(split = 'information'))
  
  DT_pred_probs = predict(DT, newdata = test_data[, 2:13], type = "prob")[,2]
  DT_pred <- ifelse(DT_pred_probs >= 0.5, 1, 0)
  
  DT_confusion_matrix = table(DT_pred, test_data[, 1])
  precision_DT_value = DT_confusion_matrix[2,2]/(DT_confusion_matrix[2,2] + DT_confusion_matrix[1,2])
  recall_DT_value = DT_confusion_matrix[2,2]/(DT_confusion_matrix[2,2] + DT_confusion_matrix[2,1])
  DT_acc[j] = sum(diag(DT_confusion_matrix)) / sum(DT_confusion_matrix)
  DT_f1[j] = 2 * (precision_DT_value * recall_DT_value) / (precision_DT_value + recall_DT_value)
  
  pred = prediction(DT_pred_probs, test_data[, 1])
  auc_val = performance(pred,measure="auc")
  DT_auc[j]=auc_val@y.values[[1]]
}

DT_acc
mean(DT_acc) #0.7226845
precision_DT_value #0.7309379
recall_DT_value #0.7057779
DT_f1
mean(DT_f1) #0.7327456
DT_auc
mean(DT_auc) #0.7562998

#plot DT 
library("rpart.plot")
DT <- rpart(Diabetes_binary ~ .,
            method = "class", data = train_data, control = rpart.control(minsplit = 5000),
            parms = list(split = 'information'))
rpart.plot(DT, type = 3, extra = 2, clip.right.labs = FALSE, varlen = 0, faclen = 0) 

#####################NAIVE BAYER###############################
set.seed(1101)
library(e1071)

n_folds = 5
folds_j = sample(rep(1:n_folds, length.out = nrow(diabetes_data)))
NB_acc = numeric(n_folds)
NB_recall = numeric(n_folds)
NB_f1 = numeric(n_folds)
NB_auc = numeric(n_folds)

acc = numeric(n_folds)

for (j in 1:n_folds) {
  test_1 = which(folds_j == j)
  train_1 = which(folds_j != j)
  
  train_data = rbind(diabetes_data[train_1, ], non_diabetes_data[train_1, ])
  test_data = rbind(diabetes_data[test_1, ], non_diabetes_data[test_1, ])
  
  NB = naiveBayes(Diabetes_binary ~., data = train_data)
  
  NB_pred_probs = predict(NB, test_data[, 2:13], type = "raw")[,2]
  NB_pred <- ifelse(NB_pred_probs >= 0.5, 1, 0)
  
  confusion_matrix = table(NB_pred, test_data[, 1])
  NB_confusion_matrix = table(NB_pred, test_data[, 1])
  precision_NB_value = NB_confusion_matrix[2,2]/(NB_confusion_matrix[2,2] + NB_confusion_matrix[1,2])
  recall_NB_value = NB_confusion_matrix[2,2]/(NB_confusion_matrix[2,2] + NB_confusion_matrix[2,1])
  NB_acc[j] = sum(diag(NB_confusion_matrix)) / sum(NB_confusion_matrix)
  NB_f1[j] = 2 * (precision_NB_value * recall_NB_value) / (precision_NB_value + recall_NB_value)
  
  pred = prediction(NB_pred_probs, test_data[, 1])
  auc_val = performance(pred,measure="auc")
  NB_auc[j]=auc_val@y.values[[1]]
}

NB_acc
mean(NB_acc) #0.7200814
precision_NB_value #0.6904796
recall_NB_value #0.724184
NB_f1
mean(NB_f1) #0.7131281
NB_auc
mean(NB_auc) #0.7943928

#########KNN##########
set.seed(1101)
library(class)

n_folds = 5
folds_j = sample(rep(1:n_folds, length.out = nrow(diabetes_data)))
kNN_acc = numeric(n_folds)
kNN_recall = numeric(n_folds)
kNN_f1 = numeric(n_folds)
kNN_auc = numeric(n_folds)

acc = numeric(n_folds)

for (j in 1:n_folds) {
  test_1 = which(folds_j == j)
  train_1 = which(folds_j != j)
  
  train_data = rbind(diabetes_data[train_1, ], non_diabetes_data[train_1, ])
  test_data = rbind(diabetes_data[test_1, ], non_diabetes_data[test_1, ])
  
  kNN <- knn(train = train_data[, 2:13], test = test_data[, 2:13], cl = train_data$Diabetes_binary, k = 5)
  kNN_numeric <- as.numeric(levels(kNN))[kNN]
  
  kNN_confusion_matrix = table(kNN_numeric, test_data[, 1])
  precision_kNN_value = kNN_confusion_matrix[2,2]/(kNN_confusion_matrix[2,2] + kNN_confusion_matrix[1,2])
  recall_kNN_value = kNN_confusion_matrix[2,2]/(kNN_confusion_matrix[2,2] + kNN_confusion_matrix[2,1])
  kNN_acc[j] = sum(diag(kNN_confusion_matrix)) / sum(kNN_confusion_matrix)
  kNN_f1[j] = 2 * (precision_kNN_value * recall_kNN_value) / (precision_kNN_value + recall_kNN_value)
  
  pred <- prediction(as.numeric(levels(kNN))[kNN], test_data[, 1])
  auc_val = performance(pred,measure="auc")
  kNN_auc[j]=auc_val@y.values[[1]]
}

kNN_acc
mean(kNN_acc) # 0.7197704
precision_kNN_value #0.7518744
recall_kNN_value #0.707064
kNN_f1
mean(kNN_f1) #0.7282064
kNN_auc
mean(kNN_auc) #0.7197704