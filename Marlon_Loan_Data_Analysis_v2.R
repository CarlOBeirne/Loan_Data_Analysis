install.packages()
library(RMariaDB)
library(fetc)

dbListTables(storiesDb)

dbListFields(storiesDb, 'loan_data')
rs = dbSendQuery(storiesDb, 'select * from loan_data')


dbRows<-dbFetch(rs)
class(dbRows)
write.csv(dbRows, file = "loan_data.csv")
library(tidyverse)
loan_data_test <- read_csv("loan_data.csv",col_names = TRUE, col_types =
                             cols(.default = col_character(),
                                  X1 = col_double(),
                                  loan_amnt = col_double(),
                                  funded_amnt = col_double(),
                                  funded_amnt_inv = col_double(),
                                  int_rate = col_double(),
                                  installment = col_double(),
                                  annual_inc = col_double(),
                                  delinq_2yrs = col_double(),
                                  out_prncp = col_double(),
                                  total_pymnt = col_double(),
                                  total_rec_prncp = col_double(),
                                  total_rec_int = col_double(),
                                  annual_inc_joint = col_double())
)


head(loan_data_test)
missingValues <- data.frame(sapply(loan_data_test, function(y) sum(length(which(is.na(y))))))
colnames(missingValues) <- c("Missing Values")
missingValues
summary(loan_data_test)

loan_data_test<- loan_data_test[,- c(1,3,4,25)]
loan_data_test <- na.omit(loan_data_test)
missingValues <- data.frame(sapply(loan_data_test, function(y) sum(length(which(is.na(y))))))
colnames(missingValues) <- c("Missing Values")
missingValues

loan_data_test$loan_status[loan_data_test$loan_status == "Fully Paid" ] <- "good"
loan_data_test$loan_status[loan_data_test$loan_status == "Current" ] <- "good"
loan_data_test$loan_status[loan_data_test$loan_status == "Default" ] <- "good"

loan_data_test$loan_status[loan_data_test$loan_status == "Late (16-30 days)" ] <- "bad"
loan_data_test$loan_status[loan_data_test$loan_status == "Late (31-120 days)" ] <- "bad"
loan_data_test$loan_status[loan_data_test$loan_status == "In Grace Period" ] <- "bad"
loan_data_test$loan_status[loan_data_test$loan_status == "Charged Off" ] <- "bad"

summary(loan_data_test)


############
library(rsample) # data splitting
library(dplyr) # data transformation
library(ggplot2) # data visualisation
library(caret) # various functions for train/test split, model training and evaluation
library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr) # alternative, this also loads %>%

head(loan_data_test)
loan_data_test <- loan_data_test %>% mutate(home_ownership  = as.factor(home_ownership ),
                                            term = as.factor(term),
                                            verification_status  = as.factor(verification_status),
                                            debt_settlement_flag  = as.factor(debt_settlement_flag),
                                            delinq_2yrs  = as.factor(delinq_2yrs),
                                            loan_status = as.factor(loan_status),
                                            grade = as.factor(grade),
                                            sub_grade = as.factor(sub_grade),
                                            debt_settlement_flag = as.factor(debt_settlement_flag))

set.seed(100)
loan_data_test <- sample_n(loan_data_test, 50000)
loan_data_test.train.index <- createDataPartition(
  loan_data_test$loan_status,
  p = .70,
  list = FALSE
)

# create the train dataset using the indexes from the partitioning above
loan_data_test.train <- loan_data_test[loan_data_test.train.index,]

# create the test dataset using all but the indexes from the partitioning above
loan_data_test.test <- loan_data_test[-loan_data_test.train.index,]  

# relative class balance in the entire dataset, the train dataset and the test dataset.
table(loan_data_test$loan_status) %>% prop.table()

table(loan_data_test.train$loan_status) %>% prop.table()

#plots of the conditional distributions of the numeric variables.
densities_loan_data_any <- loan_data_test.train %>%
  filter(loan_status == "good") %>%
  select(total_pymnt, loan_amnt,int_rate, installment, annual_inc,total_pymnt,out_prncp) %>%
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free") 
densities_loan_data_any 

densities_loan_data_mortgage <- loan_data_test.train %>%
  filter(loan_status == "bad") %>%
  select(total_pymnt, loan_amnt,int_rate, installment, annual_inc,total_pymnt,out_prncp) %>%
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free") 
densities_loan_data_mortgage



# cross-vaildation (CV) with 10 folds.
train_control <- trainControl(
  method = "cv",
  number = 10
)

# model using cross validation.
loan_data_test.train<- as.data.frame(loan_data_test.train)


nb.fit <- train(
  x = loan_data_test.train[,-13], # everything except loan_status var
  y = loan_data_test.train[,13], # only loan_status var
  method = "nb", # uses the NaiveBayes() function from the klaR 
  trControl = train_control # our train configuration
)
warnings() #the warnings if any

#confusion matrix for the fitted model.
caret::confusionMatrix(nb.fit)
#predicting on the test dataset and producing a confusion matrix.

nb.predict <- predict(nb.fit,loan_data_test.test[,-13])
loan_data_test.test <- as.data.frame(loan_data_test.test)
confusionMatrix(nb.predict, loan_data_test.test[,13])

#tuning grid model.
tuning_grid_kernel <- expand.grid(
  usekernel = TRUE,
  fL = 0:5,
  adjust = seq(0, 5, by = 0.5)
)

tuning_grid_no_kernel <- expand.grid(
  usekernel = FALSE,
  fL = 0:5,
  adjust = 0
)
tuning_grid = rbind(tuning_grid_kernel,tuning_grid_no_kernel)

#optimise a model using the tuning grid created above. using Box-Cox transform any numeric variables.
nb.fit.2 <- train(
  x = loan_data_test.train[,-13],
  y = loan_data_test.train[,13],
  method = "nb",
  trControl = train_control,
  tuneGrid = tuning_grid,
  preProc = c("BoxCox", "center", "scale")
)
nb.fit.2$results %>%
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

#confusion matrix for the cross validated model
confusionMatrix(nb.fit.2)

nb.predict.2 <- predict(nb.fit.2, newdata = loan_data_test.test[,-13])
confusionMatrix(nb.predict.2, loan_data_test.test[,13])

trellis.par.set(caretTheme())
plot(nb.fit, scales = list(x = list(log = 2)))

caret::confusionMatrix(nb.fit.2)
