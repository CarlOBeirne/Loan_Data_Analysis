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

# Install Packages
install.packages(c("randomForest", "caret"))

# Reference Libraries
library(randomForest)
library(caret)
library(data.table)

# Saving the cleaned data to CSV to be reuploaded to the database
#write.csv(loan_data, "CleanedLoanData.csv") 

loan_data <- fread("CleanedLoanData.csv",stringsAsFactors = TRUE)

# Beginning Analysis

# Making Dataset for Carl's Analysis (Removing Column 1 & 9)
loan_data_carl <- loan_data[,-c(9,24)]

str(loan_data_carl)

sapply(loan_data_carl, function(x) sum(is.na(x))) #Check for nulls

# loan_data$annual_inc <- as.integer(loan_data$annual_inc)
# loan_data$out_prncp <- as.integer(loan_data$out_prncp)
# 
# 
# loan_data$home_ownership <- as.factor(loan_data$home_ownership)
# loan_data$emp_length <- as.factor(loan_data$emp_length)

# Creating a sample
index <- sample(1:nrow(loan_data_carl), 0.1*nrow(loan_data_carl), replace = F)
loanSample <- loan_data_carl[index, ]

# Creating train and test data from the sample
index <- sample(1:nrow(loanSample), 0.75*nrow(loanSample) , replace = F)
loanTrain <- loanSample[index, ] 
loanTest <- loanSample[-index, ] 

rm(index) # Removing index - No longer required

# loan_data_train$int_rate <- as.integer(loan_data_train$int_rate)
# loan_data_test$int_rate <- as.integer(loan_data_test$int_rate)
# 
# loan_data_train$purpose <- as.factor(loan_data_train$purpose)
# loan_data_test$purpose <- as.factor(loan_data_test$purpose)

# Model 1 to predict Home Ownership
rf_model <- randomForest(home_ownership ~., loanTrain)
varImpPlot(rf_model , col = "purple", bg = "colour")

predRF <- predict(rf_model, loanTest)
HomeOwnCM <- confusionMatrix(predRF, loanTest$home_ownership)

#RF Model 2
rf_model2 <- randomForest(sub_grade ~., loanTrain)

varImpPlot(rf_model2) 

predRF2 <- predict(rf_model2, newdata = loan_data_test, type = "class")
PurposeCF <- confusionMatrix(predRF2, loan_data_test[,15])

table(loan_data$home_ownership)
table(loan_data_test$home_ownership)
table(loan_data_train$home_ownership)


str(loan_data_train$purpose)
