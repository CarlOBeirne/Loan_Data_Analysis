#Jamie Hackett
#16386576

# Loading the required packages 
library(RMariaDB)
library(dplyr)
library(HistogramTools)
library(cluster)
library(factoextra)
library(gridExtra)
library(tidyverse)
library(caret)
library(analogsea)
library(future)
library(ggplot2)
library(e1071)
library(randomForest)
library(rminer)


# Establishing a connection to the Database
rmariadb.settingsfile <- "C:/Users/Jamie/OneDrive - National College of Ireland/Year 4/Web Minining/CA2/loan_data_analysis.cnf"
rmariadb.db <- "loan-data-analysis"
loanAnalysisDB <- dbConnect(RMariaDB::MariaDB(), default.file=rmariadb.settingsfile, group=rmariadb.db)

# Database Query to Select and Store all of the Rows for some intial analysis
loan_data <- dbSendQuery(loanAnalysisDB, "SELECT * FROM loan_data")
loan_data <- dbFetch(loan_data)


   ##################### Engineering features #####################

#Looking at the normality of the data
set.seed(2002)
loan_data$loan_amnt <- as.numeric(loan_data$loan_amnt)
loan_data$funded_amnt <- as.numeric(loan_data$funded_amnt)
loan_data$funded_amnt_inv <- as.numeric(loan_data$funded_amnt_inv)
loan_data$term <- as.factor(loan_data$term)
loan_data$grade <- as.factor(loan_data$grade)
loan_data$sub_grade <- as.factor(loan_data$sub_grade)
loan_data$emp_title <- as.factor(loan_data$emp_title)
loan_data$emp_length <- as.factor(loan_data$emp_length)
loan_data$home_ownership <- as.factor(loan_data$home_ownership)
loan_data$annual_inc <- as.numeric(loan_data$annual_inc)
loan_data$verification_status <- as.factor(loan_data$verification_status)
loan_data$issue_d <- as.factor(loan_data$issue_d)
loan_data$loan_status <- as.factor(loan_data$loan_status)
loan_data$purpose <- as.factor(loan_data$purpose)
loan_data$addr_state <- as.factor(loan_data$addr_state)
loan_data$application_type <- as.factor(loan_data$application_type)
loan_data$annual_inc_joint <- as.numeric(loan_data$annual_inc_joint)
loan_data$disbursement_method <- as.factor(loan_data$disbursement_method)
loan_data$debt_settlement_flag <- as.factor(loan_data$debt_settlement_flag)

##################### Creating a sample set #####################

#Create a sample ize of 5000 records from the database
loan_data_sample <- sample_n(loan_data, 1000)
write.csv(loan_data_sample, "loan_data_sample.csv")

#Creating a dataframe of just numeric data
loan_data_sample_bp <- loan_data_sample %>% c(1,2,3,5,6,11,17,18,19,20,21,23)

#Boxplot
boxplot(loan_data_sample_bp)

#Removing outlier found from boxplot
loan_data_sample <- loan_data_sample[-c(4916),]

##################### Graphical Normality tests ##################### 

#Loan Amount normality test
#shapiro.test(loan_data_sample$loan_amnt)
hist(loan_data_sample$loan_amnt)
qqnorm(loan_data_sample$loan_amnt)
qqline(loan_data_sample$loan_amnt)

#Fund amounted
#shapiro.test(loan_data_sample$funded_amnt)
hist(loan_data_sample$funded_amnt)
qqnorm(loan_data_sample$funded_amnt)
qqline(loan_data_sample$loan_amnt)

#Intrest rate
#shapiro.test(loan_data_sample$int_rate)
hist(loan_data_sample$int_rate)
qqnorm(loan_data_sample$int_rate)
qqline(loan_data_sample$int_rate)

#Doesnt look like any of the data will be normal. This means we can't do any parametric testing such as multiple linear regression.

##################### KNN Sample of 1000 ##################### 

#Completely random KNN with all numeric data for a sample of 50000
#Creating a dataframe with all the required data
KNN1 <- data.frame(loan_data_sample$loan_amnt, loan_data_sample$funded_amnt, loan_data_sample$int_rate, loan_data_sample$installment, loan_data_sample$annual_inc, loan_data_sample$out_prncp, loan_data_sample$total_pymnt, loan_data_sample$total_rec_prncp, loan_data_sample$total_rec_int)

#Scale the data
KNN1 <- scale(KNN1, center = T, scale = T)

#Look at the top of the rows
head(KNN1)

#Visulizing distance within the data
distance <- get_dist(KNN1)

#Creating a visulisation
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Running the first KNN
K1.1 <- kmeans(KNN1, centers = 4, nstart = 35)
K1.1

#Visulizing the first KNN
fviz_cluster(K1.1, data = KNN1)

#Creating a function for the sum within squares so we can see what the best number of clusters are.
wssplot <- function(data, max_clusters=15) {
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (k in 2:max_clusters){
    wss[k] <- sum(kmeans(data, centers=k)$withinss)
  }
  plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
}

#Running hte WSS plot
wssplot(KNN1, 5)

#Looking at the first plot it seems that row 4490 is an error. This is related to stock ticker SAB. Best to remove it. 



#Running multiple Kmeans with different clusters
K1.2 <- kmeans(KNN1, centers = 2, nstart = 25)
K1.3 <- kmeans(KNN1, centers = 3, nstart = 25)
K1.4 <- kmeans(KNN1, centers = 4, nstart = 25)
K1.5 <- kmeans(KNN1, centers = 5, nstart = 25)

#Plots
P1.1 <- fviz_cluster(K1.2, geom = "point", data = KNN1) + ggtitle('K=2')
P1.2 <- fviz_cluster(K1.3, geom = "point", data = KNN1) + ggtitle('K=3')
P1.3 <- fviz_cluster(K1.4, geom = "point", data = KNN1) + ggtitle('K=4')
P1.4 <- fviz_cluster(K1.5, geom = "point", data = KNN1) + ggtitle('K=5')
grid.arrange(P1.1, P1.2, P1.3, P1.4, nrow = 2)

##################### KNN with no sample ##################### 
KNN2 <- data.frame(loan_data$loan_amnt, loan_data$funded_amnt, loan_data$int_rate, loan_data$installment, loan_data$annual_inc, loan_data$out_prncp, loan_data$total_pymnt, loan_data$total_rec_prncp, loan_data$total_rec_int)

#Scale the data
KNN2 <- scale(KNN2, center = T, scale = T)

#Run WSS to figure out the required amount 
wssplot(KNN2, 10)

#Running the first KNN
K2.1 <- kmeans(KNN2, centers = 2, nstart = 35)
K2.2 <- kmeans(KNN2, centers = 3, nstart = 35)
K2.3 <- kmeans(KNN2, centers = 4, nstart = 35)
K2.4 <- kmeans(KNN2, centers = 5, nstart = 35)

#Plots
P2.1 <- fviz_cluster(K2.1, geom = "point", data = KNN2) + ggtitle('K=2')
P2.2 <- fviz_cluster(K2.2, geom = "point", data = KNN2) + ggtitle('K=3')
P2.3 <- fviz_cluster(K2.3, geom = "point", data = KNN2) + ggtitle('K=4')
P2.4 <- fviz_cluster(K2.4, geom = "point", data = KNN2) + ggtitle('K=5')
grid.arrange(P2.1, P2.2, P2.3, P2.4, nrow = 2)

##################### SVM - Linear Kernel with tuning grid##################### 

#Create a sample dataset with only numeric data
SVM.DF <- data.frame(loan_data_sample$loan_amnt, loan_data_sample$funded_amnt, loan_data_sample$installment, loan_data_sample$annual_inc, loan_data_sample$out_prncp, loan_data_sample$total_pymnt, loan_data_sample$total_rec_prncp, loan_data_sample$total_rec_int, loan_data_sample$grade)

#Scale the data
#SVM.DF <- scale(SVM.DF, center = T, scale = T)

#Creating training and test datasets with a 75% / 25% split
svm.df.train.index <- createDataPartition(
  SVM.DF$loan_data_sample.grade,
  p = .75,
  list = F
)

#Create the train datast using the indexs from the partitioning indices generated above
svm.df.train <- SVM.DF[svm.df.train.index,]
svm.df.test <- SVM.DF[-svm.df.train.index,]

Cost = 2^c(1:8)
print(Cost)

svm.control = trainControl(
  method = "cv",
  number = 10,
  summaryFunction = defaultSummary
)

svm.linear.grid <- expand.grid(
  C = Cost
)

svm.fit1 <- train( # Train a model model
  loan_data_sample.grade ~ ., # with Class as the response variable, all
  # others as explanatory
  data = svm.df.train, # ionosphere.train as the training data
  method = "svmLinear", # using the linear kernel
  trControl = svm.control, # cross-validated as configured above
  preProc = c("center", "scale","nzv"), # centre and scale the data, removing near-zero
  # variance variables
  verbose = FALSE,
  tuneGrid = svm.linear.grid # use the tuning grid created above
)

svm.fit1

#It looks like the optimum value for Cost is beterrn 64 and 128, so let's narrow that done.
Cost = c(64:128)
print(Cost)

svm.linear.grid2 <- expand.grid(
  C = Cost
)

svm.fit2 <- train(
  loan_data_sample.grade ~ .,
  data = svm.df.train, 
  method = "svmLinear",
  trControl = svm.control,
  preProc = c("center", "scale", "nzv"),
  verbose = F,
  tuneGrid = svm.linear.grid2
)

svm.fit2
#Now we somewhat have a better idea of the optimum value

table(svm.df.test$loan_data_sample.grade)

svm.linear.predict <- predict(
  svm.fit2,
  svm.df.test[,-9],
)

svm.linear.confusionmatrix <- confusionMatrix(
  data = svm.linear.predict,
  reference = svm.df.test[,9],
  positive = "A"
)

svm.linear.confusionmatrix



##################### SVM - Radial Basis Kernel with tuning grid #####################
svm.rbf.grid <- expand.grid(
  C = 2^seq(3,5,0.1),
  sigma = 2^c(-25, -20, -15,-10,-5, 0)
)

svm.rbf.fit <- train(
  loan_data_sample.grade ~ .,
  data = svm.df.train, 
  method = "svmRadial",
  trControl = svm.control,
  preProc = c("center", "scale", "nzv"),
  verbose = F,
  tuneGrid = svm.rbf.grid
)

svm.rbf.fit$bestTune

svm.rbf.predict <- predict(
  svm.rbf.fit,
  svm.df.test[,-9],
)

svm.rbf.confusionmatrix <- confusionMatrix(
  data = svm.rbf.predict,
  reference = svm.df.test[,9],
  positive = "A"
)
svm.rbf.confusionmatrix


##################### SVM - Radial basis kernel with random search #####################
svm.random.control <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = defaultSummary,
  search = "random"
)

svm.rbf.random.fit <- train(
  loan_data_sample.grade ~ .,
  data = svm.df.train,
  method = "svmRadial",
  trControl = svm.control,
  verbose = F,
  preProc = c("center", "scale", "nzv"),
  tuneLength = 60
)

svm.rbf.random.fit$bestTune

svm.rbf.random.predict <- predict(
  svm.rbf.random.fit,
  svm.df.train[,-9]
)

svm.rbf.random.confusionmatrix <- confusionMatrix(
  data = svm.rbf.random.predict,
  reference = svm.df.train[,9],
  positive = "A"
)

svm.rbf.random.confusionmatrix
#Variable importance analysis
varImp(svm.rbf.random.fit)
