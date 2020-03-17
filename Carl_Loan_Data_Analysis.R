# Workbench for Carl O'Beirne's section of the project
library(RMariaDB)
library(bigmemory)

# Establishing a connection to the Database
rmariadb.settingsfile <- "C:/temp/loan-data-analysis.cnf"
rmariadb.db <- "loan-data-analysis"
loanAnalysisDB <- dbConnect(RMariaDB::MariaDB(), default.file=rmariadb.settingsfile, group=rmariadb.db)

# Database Query to Select and Store all of the Rows for data cleansing
loan_data <- dbSendQuery(loanAnalysisDB, "SELECT * FROM loan_data")
loan_data <- dbFetch(loan_data)

# Adding NA where there is a blank in the Data
loan_data[loan_data == ''] <- NA 

# Counting NA Values
sapply(loan_data, function(x) sum(is.na(x))) 

# Removing rows that has NA in Emp_Title
loan_data <- loan_data[!is.na(loan_data$emp_title), ] 

# Saving the cleaned data to CSV to be reuploaded to the database
write.csv(loan_data, "CleanedLoanData.csv") 

loan_data <- read.csv("CleanedLoanData.csv",stringsAsFactors = TRUE)
head(loan_data)
str(loan_data)

# Beginning Analysis
library(randomForest)
library(caret)

loan_data_train <- loan_data_train[,-1]
loan_data_test <- loan_data_test[, -9]

sapply(loan_data, function(x) sum(is.na(x))) #Check for nulls


set.seed(2002)

# loan_data$annual_inc <- as.integer(loan_data$annual_inc)
# loan_data$out_prncp <- as.integer(loan_data$out_prncp)
# 
# 
# loan_data$home_ownership <- as.factor(loan_data$home_ownership)
# loan_data$emp_length <- as.factor(loan_data$emp_length)

loan_data_sample_index <- sample(1:nrow(loan_data), 0.1*nrow(loan_data), replace = F)
loan_data_sample <- loan_data[loan_data_sample_index, ]


sampleLoansIndex <- sample(1:nrow(loan_data_sample), 0.8*nrow(loan_data_sample) , replace = F)

loan_data_train <- loan_data_sample[sampleLoansIndex, ] 
loan_data_test <- loan_data_sample[-sampleLoansIndex, ] 

str(loan_data_train$int_rate)
str(loan_data_test$int_rate)

loan_data_train$int_rate <- as.integer(loan_data_train$int_rate)
loan_data_test$int_rate <- as.integer(loan_data_test$int_rate)

loan_data_train$purpose <- as.factor(loan_data_train$purpose)
loan_data_test$purpose <- as.factor(loan_data_test$purpose)


loan_data_train[is.na(loan_data_train$emp_title),] <- "Unknown"
loan_data_train[is.na(loan_data_train$purpose),] <- "Unknown"


sapply(loan_data_train, function(x) sum(is.na(x))) 


rf_model <- randomForest(home_ownership ~., loan_data_train)
varImpPlot(rf_model , col = "purple")

predRF <- predict(rf_model, newdata = loan_data_test, type="class")
HomeOwnCM <- confusionMatrix(predRF, loan_data_test[, 10])

#RF Model 2
rf_model2 <- randomForest(purpose ~., loan_data_train)
varImpPlot(rf_model2)

predRF2 <- predict(rf_model2, newdata = loan_data_test, type = "class")
PurposeCF <- confusionMatrix(predRF2, loan_data_test[,15])

table(loan_data$home_ownership)
table(loan_data_test$home_ownership)
table(loan_data_train$home_ownership)


str(loan_data_train$purpose)
