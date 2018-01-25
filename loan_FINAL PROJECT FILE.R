rm(list = ls())
getwd()

########################################################################################################################
##### 1. Reading the Data and Installing the Packages ###
########################################################################################################################

### Reading the Data Set from the .csv File ###
loan_data = read.csv("loan.csv", header = TRUE)

### Installing the Required Packages ###
install.packages("survival")
install.packages("splines")
install.packages("lattice")
install.packages("gbm")
install.packages("stringr")
install.packages("DescTools")
install.packages("choroplethrMaps")
install.packages("plotly")
install.packages("dplyr")
install.packages("choroplethr")
install.packages("DT")
install.packages("randomForest")
install.packages("rpart")
install.packages("corrplot")
install.packages("caret")
install.packages("dplyr")
install.packages("VIM")
install.packages("treemap")
install.packages('e1071', dependencies=TRUE)
install.packages('caret', dependencies = TRUE)
install.packages("cran")


### Loading the Libaries ###
library(choroplethrMaps)
library(choroplethr)
library(DescTools)
library(plotly)
library(ggplot2)
library(dplyr)
library(gbm)
library(stringr)
library(Hmisc)
library(reshape2)
library(DT)
library(randomForest)
library(rpart)
library(corrplot)
library(caret)
library(dplyr)
library(VIM)
library(treemap)
library(ISLR)
library(klaR)
library(class)
library(data.table)
library(glmnet)
library(kernlab)
library(e1071)
library(pROC)

################################
summary(loan_data)
str(loan_data)
dim(loan_data)
names(loan_data) 

### Total No.of NA values before Cleaning ###
sum(is.na(loan_data))

########################################
# Plot for Visualizing Missingness and NA Columns before Cleaning
########################################
aggr(loan_data, prop = T, number = F, label = T, gap = T, only.miss = T)
########################################################################################################################
##### 2. Data Cleaning ###
########################################################################################################################

## To find out which columns have NA values
names(loan_data[, !complete.cases(t(loan_data))])
## To eliminate columns which have more than 100k NA values
drops <- c("mths_since_last_delinq" , "mths_since_last_record" , "mths_since_last_major_derog", "annual_inc_joint" , "dti_joint" ,"open_acc_6m", "open_il_6m", "open_il_12m" , "open_il_24m" , "mths_since_rcnt_il", "total_bal_il", "open_rv_12m", "open_rv_24m" , "max_bal_bc" , "all_util" , "inq_fi", "total_cu_tl", "inq_last_12m")
loan_data <- loan_data[ , !(names(loan_data) %in% drops)]
dim(loan_data)
## To eliminate columns with further missing values
drops2 <- c("total_rev_hi_lim","il_util","tot_cur_bal","acc_now_delinq","collections_12_mths_ex_med","id","member_id","collection_recovery_fee","out_prncp_inv","out_prncp","recoveries","total_rec_late_fee","application_type","tot_coll_amt","emp_title", "url", "desc", "title", "zip_code", "verification_status_joint", "earliest_cr_line", "last_pymnt_d", "next_pymnt_d","last_credit_pull_d", "policy_code")
loan_data <- loan_data[ , !(names(loan_data) %in% drops2)]
dim(loan_data)

## To replace NA values with mean value of that column
loan_data$annual_inc[is.na(loan_data$annual_inc)] <- mean(loan_data$annual_inc, na.rm = TRUE)
loan_data$delinq_2yrs[is.na(loan_data$delinq_2yrs)] <- mean(loan_data$delinq_2yrs, na.rm = TRUE)
loan_data$inq_last_6mths[is.na(loan_data$inq_last_6mths)] <- mean(loan_data$inq_last_6mths, na.rm = TRUE)
loan_data$open_acc[is.na(loan_data$open_acc)] <- mean(loan_data$open_acc, na.rm = TRUE)
loan_data$pub_rec[is.na(loan_data$pub_rec)] <- mean(loan_data$pub_rec, na.rm = TRUE)
loan_data$revol_util[is.na(loan_data$revol_util)] <- mean(loan_data$revol_util, na.rm = TRUE)
loan_data$total_acc[is.na(loan_data$total_acc)] <- mean(loan_data$total_acc, na.rm = TRUE)

## Assigning the class of all the variables..integer, factor etc. ###
sapply(loan_data[1,],class)
## Total number of na values in the loan_data after Cleaning ###
sum(is.na(loan_data)) 

### Writing the Claened datset into a .csv File ###
#write.csv(loan_data, file = "Cleaned_Loandata.csv", row.names = FALSE)
#loan_data = read.csv("Cleaned_Loandata.csv", header = TRUE)

########################################################################################################################
##### 3. Intial Exploratory Analysis ###
########################################################################################################################

########################################
# Plot for Visualizing Missingness and NA Columns after Cleaning
########################################
aggr(loan_data, prop = T, number = F, label = T, gap = T, only.miss = T)

########################################
# Plot for Visualizing the Categorized Loan_status Levels (%) before Grouping
########################################
tmp = loan_data %>% group_by(loan_status) %>% summarise(ncount = n())
tmp$ncount = 100 * tmp$ncount/nrow(loan_data)
tmp$ncount_p = str_c(round(tmp$ncount,2),"%")
ggplot(tmp,aes(x=loan_status,y=ncount,fill=loan_status)) + geom_bar(stat="identity") + geom_text(aes(label=ncount_p),vjust = -1)

########################################
# Loans Volume by Grade (A to G)
########################################
deft_loan <- loan_data %>% select(grade, loan_status, term)
g_deft = ggplot(data = deft_loan, aes(x = grade))
g_deft + geom_bar(fill = "dodgerblue") + labs(title = "Number of Loans by Grade", x = "Grade", y = "Volume") + theme_bw()

########################################
# Loans Volume by Grade for Current and Fully Paid (A to G)
########################################
deft_loan1 <- filter(deft_loan, loan_status %in% c("Current", "Fully Paid"))
deft_loan = ggplot(data = deft_loan1, aes(x = grade))
deft_loan + geom_bar(fill = "chartreuse3") + facet_wrap(~loan_status) + labs(title = "Volume Distribution in Current & Fully paid & default & late", 
                                                                             x = "Loan Status", y = "Volume") + theme_bw()
########################################
# Loans Purpose
########################################
deft_loan2 <- loan_data %>% select(purpose, loan_status, term)
g_deft = ggplot(data = deft_loan2, aes(x = purpose))
g_deft + geom_bar(fill = "dodgerblue") + labs(title = "Number of Loans by purpose", 
                                              x = "Purpose", y = "Volume") + theme_bw()

########################################
# Treemap for Loans Purpose
########################################
prp_df <- loan_data %>% select(purpose, loan_amnt) %>% na.omit() %>% group_by(purpose) %>% 
  dplyr::summarise(volume = n(), average_amnt = sum(as.numeric(loan_amnt), 
                                                    rm.na = TRUE)/n())

prp_df <- prp_df[!prp_df$purpose == "", ]

treemap(prp_df, index = "purpose", vSize = "volume", vColor = "average_amnt", 
        range = c(6000, 16000), type = "manual", palette = c("yellow", "green", 
                                                             "orange", "orange2", "firebrick"), algorithm = "pivotSize", sortID = "-size", 
        title = "Purposes of Loans", title.legend = "Avg_Amnt", fontfamily.labels = "serif", 
        fontsize.labels = 16, fontsize.legend = 10, fontface.labels = 1, position.legend = "bottom", 
        force.print.labels = T, border.col = "white")
########################################
# Loans Amount Distribution
########################################
Desc(loan_data$loan_amnt, main = "Loan amount distribution", plotit = TRUE)

########################################
# Loans Amount Gradual Increase by Year 
########################################
head(loan_data$issue_d)
loan_data$issue_d1 <- as.Date(gsub("^", "01-", loan_data$issue_d), format="%d-%b-%Y")
head(loan_data$issue_d1)
amnt_df <- loan_data %>% select(issue_d1, loan_amnt) %>% group_by(issue_d1) %>% 
  summarise(Amount = sum(loan_amnt))

ts_amnt <- ggplot(amnt_df, aes(x = issue_d1, y = Amount))
ts_amnt + geom_line() + xlab("Date issued")

########################################
# Boxplots for Loan Amount by Different Laon Statuses 
########################################
box_status <- ggplot(loan_data, aes(loan_status, loan_amnt))
box_status + geom_boxplot(aes(fill = loan_status)) +theme(axis.text.x = element_blank()) +
  labs(list(title = "Loan amount by status",x = "Status",y = "Amount"))

########################################
# Grade and Home Ownership
########################################
# Home_ownership <-> grade (bar - facet) keep only 'MORTGAGE', 'OWN', 'RENT'
mort_df <- loan_data %>% select(home_ownership, grade, sub_grade)
table(mort_df$home_ownership)
mort_df <- mort_df[mort_df$home_ownership %in% c("MORTGAGE", "OWN", "RENT"), ]  # Other catergories have only a few data

g_mort <- ggplot(mort_df, aes(grade))
g_mort + geom_bar(aes(fill = grade)) + facet_wrap(~home_ownership) + labs(x = "Grade", y = "Number of Loans", title = "Issued Loans of Different Home Ownership") + theme_bw()

########################################
# Loans Volume by Year
########################################
loan_data$issue_d1 <- as.Date(gsub("^", "01-", loan_data$issue_d), format="%d-%b-%Y")
head(loan_data$issue_d1)
loan_data$issue_d1 <- format(loan_data$issue_d, "20%y")

# Compare loans with different terms
gtvy_df <- loan_data %>% select(issue_d1, grade, term)
gtvy_df <- gtvy_df[complete.cases(gtvy_df), ]
gtvy_df <- gtvy_df[!gtvy_df$issue_yr == 2016, ]

# Two bar charts
g_gtvy <- ggplot(gtvy_df, aes(x = issue_yr))
g_gtvy + geom_bar(aes(fill = grade)) + facet_grid(~term) + labs(title = "Loan Volume by Year", 
                                                                x = "Issued Year", y = "Volume") + theme_bw()

########################################
# Plot to check if interest rate was dependent on grade, p value indicates that there is dependance
########################################
Desc(int_rate ~ grade, loan_data, digits = 2, main = "Interest vs Grade", plotit = TRUE)

########################################
# Pairs plot for the Dataset
########################################
pairs(loan_data)

#######################
### State Map
#######################
# Loan issued locations by volume or amount
locVol_df <- select(loan_data, addr_state)
locVol_df <- locVol_df %>% na.omit() %>% group_by(addr_state) %>% dplyr::summarise(value = n())

locAmt_df <- select(loan_data, addr_state, loan_amnt)
locAmt_df$loan_amnt <- as.numeric(locAmt_df$loan_amnt)  # Integer overflow: +/-2*10^9
locAmt_df <- locAmt_df %>% na.omit() %>% group_by(addr_state) %>% dplyr::summarise(value = sum(loan_amnt, 
                                                                                               na.rm = TRUE))

bad_indicators
loan_data$is_bad <- ifelse(loan_data$loan_status %in% bad_indicators, 0,
                           ifelse(loan_data$loan_status=="", NA, 1))
# turn the data into long format
loan_data.lng <- melt(loan_data[,numeric_cols], id="is_bad")

# plot the distribution for 'bad' and 'good' for each numeric variable
p <- ggplot(aes(x = value, group = is_bad, colour = factor(is_bad)), data = loan_data.lng)
plot(p)
# create the plot to check if there are any good variables that can be used in predictive models
p + geom_density() + facet_wrap(~variable, scales="free")
### It seems like Annual Income and Interest Rate are two variables that can be good predictors of how loans will behave.

########################################################################################################################
##### 4. Analyzing the Data ###
########################################################################################################################

##### Finidng no.of Applications according to the Loan_status ###
table(loan_data$loan_status)
## Columns to be converted from factor type to numeric type
loan_data$emp_length <- as.numeric(loan_data$emp_length)
loan_data$term <- as.numeric(loan_data$term)
loan_data$grade <- as.numeric(loan_data$grade)
loan_data$sub_grade <- as.numeric(loan_data$sub_grade)
loan_data$verification_status <- as.numeric(loan_data$verification_status)
loan_data$home_ownership <- as.numeric(loan_data$home_ownership)
loan_data$loan_status <- as.numeric(loan_data$loan_status)
loan_data$pymnt_plan <- as.numeric(loan_data$pymnt_plan)
loan_data$purpose <- as.numeric(loan_data$purpose)
loan_data$addr_state <- as.numeric(loan_data$addr_state)
loan_data$initial_list_status <- as.numeric(loan_data$initial_list_status)
loan_data$issue_d <- as.numeric(loan_data$issue_d)

sapply(loan_data[1,],class)

#############################
# Income and Verification Status. This is a simple analysis of the fraud detection.
############################
vrf_raw_df <- loan_data %>% select(verification_status, annual_inc)
vrf_raw_df <- vrf_raw_df[complete.cases(vrf_raw_df), ]
vrf_df <- vrf_raw_df %>% group_by(verification_status) %>% dplyr::summarise(mean = mean(annual_inc), std = sd(annual_inc))
data.frame(vrf_df)

#############################
# Run this for setting the loan_status column into two categories "Fully Paid" and "Default"
############################

##########################################
### With 2 Levels (Error: 8%)
##########################################
loan_data[loan_data$loan_status==6, "loan_status"] = "Fully Paid"
loan_data[loan_data$loan_status!="Fully Paid", "loan_status"] = "Default"
###### If we are using a 3rd Category, then we will use & below #########
#loan_data[loan_data$loan_status != 6 & loan_data$loan_status != 1 , "loan_status"] <- "3rd Category"
###### Again converting the Data Type of Loan Status(Char to Numeric)  ######
loan_data$loan_status <- as.factor(loan_data$loan_status)
loan_data$loan_status <- as.numeric(loan_data$loan_status)

##########################################
### With 4 Levels (Error: 35%)
##########################################

loan_data[loan_data$loan_status==6, "loan_status"] = "Fully Paid"
loan_data[loan_data$loan_status==5, "loan_status"] = "Fully Paid"
loan_data[loan_data$loan_status==2, "loan_status"] = "Current"
loan_data[loan_data$loan_status==8, "loan_status"] = "Current"
loan_data[loan_data$loan_status==1, "loan_status"] = "Default"
loan_data[loan_data$loan_status==3, "loan_status"] = "Default"
loan_data[loan_data$loan_status==4, "loan_status"] = "Default"
loan_data[loan_data$loan_status==7, "loan_status"] = "Late"
loan_data[loan_data$loan_status==9, "loan_status"] = "Late"
loan_data[loan_data$loan_status==10, "loan_status"] = "Late"

table(loan_data$loan_status)
loan_data$loan_status <- as.factor(loan_data$loan_status)
loan_data$loan_status <- as.numeric(loan_data$loan_status)

##########################################
### With 3 Levels (Error: 34%)
##########################################
loan_data[loan_data$loan_status==6, "loan_status"] = "Fully Paid"
loan_data[loan_data$loan_status==5, "loan_status"] = "Fully Paid"
loan_data[loan_data$loan_status==2, "loan_status"] = "Current"
loan_data[loan_data$loan_status==8, "loan_status"] = "Current"
loan_data[loan_data$loan_status==1, "loan_status"] = "Default"
loan_data[loan_data$loan_status==3, "loan_status"] = "Default"
loan_data[loan_data$loan_status==4, "loan_status"] = "Default"
loan_data[loan_data$loan_status==7, "loan_status"] = "Default"
loan_data[loan_data$loan_status==9, "loan_status"] = "Default"
loan_data[loan_data$loan_status==10, "loan_status"] = "Default"

loan_data <- loan_data[!(loan_data$loan_status =="Current"),]
table(loan_data$loan_status)
loan_data$loan_status <- as.factor(loan_data$loan_status)
loan_data$loan_status <- as.numeric(loan_data$loan_status)

########################################
# Plot for Visualizing the new Categorized Loan_status Levels (%) after Grouping
########################################
tmp = loan_data %>% group_by(loan_status) %>% summarise(ncount = n())
tmp$ncount = 100 * tmp$ncount/nrow(loan_data)
tmp$ncount_p = str_c(round(tmp$ncount,2),"%")
ggplot(tmp,aes(x=loan_status,y=ncount,fill=loan_status)) + geom_bar(stat="identity") + geom_text(aes(label=ncount_p),vjust = -1)

########################################
# Plot for Visualizing the Correlation between Variables
########################################
quartz()
corrplot(cor(loan_data,use="na.or.complete"),type = "full", title = "Correlation Plot", is.corr = FALSE)

#### Or can use this plot for Lower Trianle#####
quartz()
corrplot(cor(loan_data), method = "number", title = "Correlation Map of Subgrade & Factors", type = "lower", order = "FPC", number.cex = 0.5, tl.cex = 0.8)

########################################
# Splitting the Datsets for further Analysis
########################################
set.seed(1)
########################################
#Creating a subset of the data using the issue date > 97
########################################
loan_data <- subset(loan_data, loan_data$issue_d>97)
dim(loan_data)

train = sample(1:nrow(loan_data), nrow(loan_data)*.70)
trainset = loan_data[train,]
testset = loan_data[-train,]

head(trainset)
head(testset)
dim(trainset)
dim(testset)

loan_status_train <- trainset$loan_status
head(loan_status_train)
loan_status_test <- testset$loan_status
head(loan_status_test)

########################################################################################################################
##### 5. Applying Different Models ###
########################################################################################################################

#################################
### a. LOGISTIC MODEL
#################################
glm_model1 <- glm(loan_status ~ . , data = trainset)
summary(glm_model1)

glm_model2 <- glm(loan_status ~ . -(grade + pymnt_plan + total_pymnt + total_rec_prncp + total_rec_int + total_rec_late_fee + 
                                      recoveries + tot_coll_amt), data = trainset)
summary(glm_model2)

glm_model3 <- glm(loan_status ~ . -(grade + pymnt_plan + total_pymnt + total_rec_prncp + total_rec_int + total_rec_late_fee + 
                                      recoveries + tot_coll_amt + funded_amnt + il_util), data = trainset)
summary(glm_model3)

##### Prediction #####
predictglm <- predict(glm_model1, newdata = trainset, type = "response")
predictglm <- round(predictglm)
predictglmtest <- predict(glm_model1, newdata = testset,type = "response")
predictglmtest <- round(predictglmtest)

##### Caluclating the Train and Test Errors #####
cbind(loan_status_train,predictglm)
cbind(loan_status_test,predictglmtest)
trainerror_glm <- mean(loan_status_train != predictglm)
trainerror_glm
#8.270%  # 0.08299261
testerror_glm <- mean(loan_status_test != predictglmtest)
testerror_glm
###8.734% # 0.08701473
#################################
### b. LINEAR DISCRIMINANT ANALYSIS
#################################

lda_fit1 <- lda(loan_status ~.,data = trainset)
summary(lda_fit1)

###Prediction
predictlda <- predict(lda_fit1, newdata = trainset, type = "response")
predictlda <- as.numeric(predictlda$class)
predictldatest <- predict(lda_fit1, newdata = testset)
predictldatest <- as.numeric(predictldatest$class)

###Error
cbind(loan_status_train, predictlda)
cbind(loan_status_test, predictldatest)
trainerror_lda <- mean(loan_status_train != predictlda)
trainerror_lda # 0.0678574
testerror_lda <- mean(loan_status_test != predictldatest)
testerror_lda # 0.06994645

#################################
### c. QUADRATIC DISCRIMINANT ANALYSIS
#################################
qda_fit1 <- qda(loan_status ~.,data = trainset)
summary(qda_fit1)

###Prediction

predictqda <- predict(qda_fit1, newdata = trainset, type = "response")
predictqda <- as.numeric(predictqda$class)
names(predictqda)
head(predictqda)
predictldatest <- predict(lda_fit1, newdata = testset)
predictldatest <- as.numeric(predictldatest$class)

###Error
cbind(loan_status_train, predictlda)
cbind(loan_status_test, predictldatest)
mean(loan_status_train != predictlda)
mean(loan_status_test != predictldatest)

#################################
### d. Decision Tree
#################################
model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
fitva <- rpart(loan_status~., data = trainset, method = "class", control = model.control)

### Calculating Train and Test Errors ###
pred_singletree_train <- predict (pruned_fitva, newdata = trainset, type ="class")
head(pred_singletree_train)
pred_singletree_train <- as.numeric(pred_singletree_train)
misclass_tree_train <- sum(abs(loan_status_train - pred_singletree_train))/length(pred_singletree_train)
misclass_tree_train # 0.005164622

pred_singletree <- predict (pruned_fitva, newdata = testset, type ="class")
head(pred_singletree)
pred_singletree <- as.numeric(pred_singletree)
misclass_tree <- sum(abs(loan_status_test- pred_singletree))/length(pred_singletree)
misclass_tree # 0.01087684

x11()
quartz()
plot(fitva, uniform = T, compress = T)
text(fitva, cex = .5,  use.n = T)

x11()
plot(fitva, uniform = T, compress = T)
text(fitva, use.n = T, all = T, cex = 1)

x11()
plot(fitva, branch = .4, uniform = T, compress = T)
text(fitva, use.n = T, all = T, cex = 1)

min_cp = which.min(fitva$cptable[,4])
pruned_fitva <- prune(fitva, cp = fitva$cptable[min_cp,1])

x11()
plot(fitva$cptable[,4], main = "cp" )

x11()
plot(pruned_fitva, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fitva, cex = .5)

#################################
### e. Random Forest
#################################

rand <- randomForest(loan_status~., data = trainset, n.tree = 5000)
quartz()
varImpPlot(rand, main = "Important Variables in Random Forest")

### Calculating Train and Test Errors ###
ytrue_train <- trainset$loan_status
yhat_train <- predict(rand,newdata = trainset, type = "response")
misclass_rf_train <- sum(abs(ytrue_train - yhat_train))/length(yhat_train)
misclass_rf_train # 0.008317474

ytrue <- testset$loan_status
yhat <- predict(rand,newdata = testset, type = "response")
misclass_rf <- sum(abs(ytrue - yhat))/length(yhat)
misclass_rf # 0.01918862

#################################
### f.GBM
#################################
boost_train <- trainset;
boost_train$loan_status <-trainset$loan_status
boost_test <- testset;
boost_test$loan_status <- testset$loan_status


boostfit <- gbm((unclass(loan_status)-1)~., data = boost_train, n.trees = 2, shrinkage = .1, interaction.depth = 3, distribution = "adaboost")
boostfit2 <- gbm((unclass(loan_status)-1)~., data = boost_train, n.trees = 2, shrinkage = .6, interaction.depth = 3, distribution = "adaboost")

summary(boostfit)
# For shrinkage = .1
### Calculating Train and Test Errors ###
yhat_boost_train <- predict(boostfit, newdata = boost_train, n.trees = 2, type = "response")
misclass_boost_train <- sum(abs(ytrue_train - yhat_boost_train))/length(yhat_boost_train)
misclass_boost_train #1- 0.9691251

yhat_boost <- predict(boostfit, newdata = boost_test, n.trees = 2, type = "response")
misclass_boost <- sum(abs(ytrue - yhat_boost))/length(yhat_boost)
misclass_boost # 1- 0.9691251

# For shrinkage = .6
yhat_boost <- predict(boostfit2, newdata = boost_test, n.trees = 2, type = "response")
misclass_boost2 <- sum(abs(ytrue - yhat_boost))/length(yhat_boost)
misclass_boost2 # 1 - 0.9548469

shrink <- c(0.1, 0.4, 0.6, 0.8)
maxiter <- 2
store_error <- c()
for (i in 1:length(shrink)){
  boost.fit <- gbm((unclass(loan_status)-1) ~., data = boost_train, n.trees = maxiter, shrinkage = shrink[i], interaction.depth = 2, distribution = "adaboost")
  temp <- c()
  for (j in 1:maxiter){
    yhat <- predict(boost.fit, newdata = boost_test, n.trees = j, type = "response")
    misclass_boost <- sum(abs(ytrue - yhat_boost))/length(yhat_boost)
    temp <- c(temp, misclass_boost)
  }
  store_error <- cbind(store_error, temp)
}

colnames(store_error) <- paste("shrinkage", shrink, sep = ":")

quartz()
plot(store_error[,1], main = "Error Profiles", ylab = "error", xlab = "boosting iterations") ## set ylim after checking store_error min and max
lines(store_error[,2], col = "blue")
lines(store_error[,3], col = "green")
lines(store_error[,4], col = "red")

#################################
### g.SVM 
#################################
svm_model <- svm(loan_status ~.,data = trainset)
summary(svm_model)

predictsvm <- predict(svm_model, newdata = trainset, type = "response")
predictsvm <- round(predictsvm)
predictsvmtest <- predict(svm_model, newdata = testset,type = "response")
predictsvmtest <- round(predictsvmtest)

### Calculating Train and Test Errors ###
cbind(loan_status_train,predictsvm)
cbind(loan_status_test,predictsvmtest)
trainerror_svm <- mean(loan_status_train != predictsvm)
trainerror_svm # 0.02302561
testerror_svm <- mean(loan_status_test != predictsvmtest)
testerror_svm # 0.02945114

### ROC Curve Graph ###
rocCurve_svm = roc(response = testset$loan_status,
                   predictor = predictsvmtest) ## change the name of the test_data, preidictsvmtest  name that you are using

auc_curve = auc(rocCurve_svm)

plot(rocCurve_svm,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(SVM)")