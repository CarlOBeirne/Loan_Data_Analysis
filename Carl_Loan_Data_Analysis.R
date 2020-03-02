# Workbench for Carl O'Beirne's section of the project
library(RMariaDB)

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
