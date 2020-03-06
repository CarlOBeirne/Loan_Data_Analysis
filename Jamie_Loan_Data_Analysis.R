#Jamie Hackett
#16386576

# Loading the required packages 
install.packages("RMariaDB")
install.packages("cluster")
install.packages("factoextra")
install.packages("gridExtra")
library(RMariaDB)
library(dplyr)
library(HistogramTools)
library(cluster)
library(factoextra)
library(gridExtra)
library(tidyverse)

# Establishing a connection to the Database
rmariadb.settingsfile <- "C:/Users/Jamie/OneDrive - National College of Ireland/Year 4/Web Minining/CA2/loan_data_analysis.cnf"
rmariadb.db <- "loan-data-analysis"
loanAnalysisDB <- dbConnect(RMariaDB::MariaDB(), default.file=rmariadb.settingsfile, group=rmariadb.db)

# Database Query to Select and Store all of the Rows for some intial analysis
loan_data <- dbSendQuery(loanAnalysisDB, "SELECT * FROM loan_data")
loan_data <- dbFetch(loan_data)

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

#Create a sample ize of 5000 records from the database
loan_data_sample <- sample_n(loan_data, 75000)
write.csv(loan_data_sample, "loan_data_sample.csv")

#Creating a dataframe of just numeric data
loan_data_sample_bp <- loan_data_sample %>% c(1,2,3,5,6,11,17,18,19,20,21,23)
#Boxplot
boxplot(loan_data_sample_bp)

#Removing outlier

loan_data_sample <- loan_data_sample[-c(4916),]

#Loan Amount normality test
shapiro.test(loan_data_sample$loan_amnt)
hist(loan_data_sample$loan_amnt)
qqnorm(loan_data_sample$loan_amnt)
qqline(loan_data_sample$loan_amnt)

#Fund amounted
shapiro.test(loan_data_sample$funded_amnt)
hist(loan_data_sample$funded_amnt)
qqnorm(loan_data_sample$funded_amnt)
qqline(loan_data_sample$loan_amnt)

#Intrest rate
shapiro.test(loan_data_sample$int_rate)
hist(loan_data_sample$int_rate)
qqnorm(loan_data_sample$int_rate)
qqline(loan_data_sample$int_rate)

Test <- data.frame(data("USArrests"))

#Counducting a knn on income and gradde
KNN <- data.frame(loan_data_sample$int_rate, loan_data_sample$annual_inc)
KNN <- na.omit(KNN)
KNN <- as.numeric(KNN$loan_data_sample.grade)

k2 <- kmeans(KNN, centers = 5 , nstart = 100)
k2

fviz_cluster(k2, data = KNN)

wssplot <- function(data, max_clusters=15) {
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (k in 2:max_clusters){
    wss[k] <- sum(kmeans(data, centers=k)$withinss)
  }
  plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
}

wssplot(KNN, 10)

#Completely random KNN with all numeric data
KNN2 <- data.frame(loan_data_sample$loan_amnt, loan_data_sample$funded_amnt, loan_data_sample$int_rate, loan_data_sample$installment, loan_data_sample$annual_inc, loan_data_sample$out_prncp, loan_data_sample$total_pymnt, loan_data_sample$total_rec_prncp, loan_data_sample$total_rec_int)

#Scale the data
KNN2 <- scale(KNN2, center = T, scale = T)
head(KNN2)

distance <- get_dist(KNN2)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


K3 <- kmeans(KNN2, centers = 3, nstart = 25)
K3
fviz_cluster(K3, data = KNN2)

wssplot(KNN2, 10)
