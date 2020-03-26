# Workbench for Carl O'Beirne's section of the project

# Install Packages
install.packages(c("randomForest", "caret"))

# Reference Libraries
library(randomForest)
library(caret)
# library(data.table)

# Saving the cleaned data to CSV to be reuploaded to the database
#write.csv(loan_data, "CleanedLoanData.csv") 
loan_data_raw <- read.csv("CleanedLoanData.csv",stringsAsFactors = TRUE)

# Beginning Analysis

# Setting the seed for reproucable results
set.seed(16326186)

str(loan_data_raw$emp_title)


# Making Dataset for Carl's Analysis
loan_data_carl <- loan_data_raw[,-c(5,8,9,24)] #Removing Sub-Grade RF Model

sapply(loan_data_carl, function(x) sum(is.na(x))) #Check for nulls

# Setting the seed for reproucable results
set.seed(16326186)
# Creating a sample
index <- sample(1:nrow(loan_data_carl), 0.05*nrow(loan_data_carl), replace = F)
loanSample <- loan_data_carl[index, ]
# Setting the seed for reproucable results
set.seed(16326186)
# Creating train and test data from the sample
index <- sample(1:nrow(loanSample), 0.75*nrow(loanSample) , replace = F)
loanTrain <- loanSample[index, ] 
loanTest <- loanSample[-index, ] 

rm(index) # Removing index - No longer required

# Model 1 to predict Grade (Without Sub Grade)
rf_model <- randomForest(grade ~., loanTrain)

# Variable Importance Plot to identify what variables are most important in the model
varImpPlot(rf_model, main = "Variable Importance Plot - Loan Grade", col = "red", bg = "red")

#Predicting the results
predRF <- predict(rf_model, loanTest)
gradeCM <- confusionMatrix(predRF, loanTest$grade)
gradeCM

# Check how many of each variable are used in the model
varUsed(rf_model)

# Table to see correct distribution of grades
table(loanTest$grade)
table(loan_data_raw$sub_grade)
