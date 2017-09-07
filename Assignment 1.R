

install.packages("gmodels")
install.packages("Hmisc")
install.packages("pROC")
install.packages("ResourceSelection")
install.packages("car")
install.packages("caret")
install.packages("dplyr")
library(gmodels)
library(Hmisc)
library(pROC)
library(ResourceSelection)
library(car)
library(caret)
library(dplyr)
install.packages("InformationValue")
library(InformationValue)

cat("\014") 
getwd()
setwd("C:\\Users\\admin\\Desktop\\MICA\\AMMA\\DATA") #This working directory is the folder where all the bank data is stored
0

df.client <- read.csv('bank_client.csv')
str(df.client)


df.attr <- read.csv('bank_other_attributes.csv')
str(df.attr)

df.campaign <- read.csv('latest_campaign.csv')
str(df.campaign)

df.campOutcome <- read.csv('campaign_outcome.csv')
str(df.campOutcome)

df.temp1 <- merge(df.client, df.campaign, by = 'Cust_id', all.x = TRUE)
df.temp2 <- merge(df.temp1, df.attr, by = 'Cust_id', all.x = TRUE)
df.data <- merge(df.temp2, df.campOutcome, by = 'Cust_id', all.x = TRUE)
length(unique(df.data$Cust_id)) == nrow(df.data) #checking for any duplicate customer ID

rm(df.temp1,df.temp2)

head(df.data)


summary(df.data)

str(df.data)

CrossTable(df.data$y)


set.seed(1234)
df.data$rand <- runif(nrow(df.data))
df.train <- df.data[df.data$rand <= 0.7,]
df.test <- df.data[df.data$rand > 0.7,]
nrow(df.train)
nrow(df.test)

CrossTable(df.train$job, df.train$y)
CrossTable(df.train$marital, df.train$y)
CrossTable(df.train$education, df.train$y)
CrossTable(df.train$default, df.train$y)
CrossTable(df.train$housing, df.train$y)
CrossTable(df.train$loan, df.train$y)
CrossTable(df.train$poutcome, df.train$y)

hist(df.train$age)
hist(df.train$balance)
hist(df.train$duration)
hist(df.train$campaign)
hist(df.train$pdays)
hist(df.train$previous)
describe(df.train[c("age", "balance", "duration", "campaign", "pdays", "previous")])


df.train$yact = ifelse(df.train$y == 'yes',1,0)
full.model <- glm(formula = yact ~ age + balance + duration + campaign + pdays + previous +
                    job + marital + education + default + housing + loan + poutcome, 
                  data=df.train, family = binomial)
summary(full.model)

# check for vif
fit <- lm(formula <- yact ~ age + balance + duration + campaign + pdays + previous +
            job + marital + education + default + housing + loan + poutcome, 
          data=df.train)
vif(fit)

backward <- step(full.model, direction = 'backward')
summary(backward)

df.train$prob = predict(full.model, type=c("response"))
class(df.train)
nrow(df.train)
q <- roc(y ~ prob, data = df.train)
plot(q)
auc(q)

varImp(full.model, scale = FALSE)

df.train$ypred = ifelse(df.train$prob>=.5,'pred_yes','pred_no')
table(df.train$ypred,df.train$y)

df.test$prob = predict(full.model, newdata = df.test, type=c("response"))

df.test$ypred = ifelse(df.test$prob>=.5,'pred_yes','pred_no')
table(df.test$ypred,df.test$y)


ks_plot(actuals=df.train$y, predictedScores=df.train$ypred)



View(df.data)

df.data_final <- df.data
df.data_final$yact = ifelse(df.data$y == 'yes',1,0) #Loading 1s for 'yes' and 0s for 'no'
nrow(df.data_final)

df.data_final <- df.data_final[!apply(df.data_final[,c("age", "balance", "duration", "campaign", "pdays", "previous", "job","marital", "education", "default", "housing", "loan", "poutcome")], 1, anyNA),]
nrow(df.data_final)
View(df.data_final)

set.seed(1234) # for reproducibility
df.data_final$rand <- runif(nrow(df.data_final))

df.train_Divyansh_model <- df.data_final[df.data_final$rand <= 0.9,]
df.test_Divyansh_model <- df.data_final[df.data_final$rand > 0.9,]
nrow(df.train_Divyansh_model)

gc()

result_tentative_trainDivyansh_model <- glm(formula = yact ~ age + balance + duration + campaign + pdays + previous +
                                      job + marital + education + default + housing + loan + poutcome, 
                                    data=df.train_Divyansh_model, family = binomial)
summary(result_tentative_trainDivyansh_model)

df.train_Divyansh_model_onlysig <- df.train_Divyansh_model[df.train_Divyansh_model$job!="unknown",]
result_tentative_trainDivyansh_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + pdays + previous +
                                           job + marital + education + default + housing + loan + poutcome, 
                                         data=df.train_Divyansh_model_onlysig, family = binomial)

df.test_Divyansh_model_onlysig <- df.test_Divyansh_model[df.test_Divyansh_model$job!="unknown",]

summary(result_tentative_trainDivyansh_model_sig1)

df.train_Divyansh_model_onlysig$pdays <-NULL
result_tentative_trainDivyansh_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + default + housing + loan + poutcome, 
                                         data=df.train_Divyansh_model_onlysig, family = binomial)


df.test_Divyansh_model_onlysig$pdays <-NULL

summary(result_tentative_trainDivyansh_model_sig1)

df.train_Divyansh_model_onlysig <- df.train_Divyansh_model_onlysig[df.train_Divyansh_model_onlysig$marital!="single",]
result_tentative_trainDivyansh_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + default + housing + loan + poutcome, 
                                         data=df.train_Divyansh_model_onlysig, family = binomial)

df.test_Divyansh_model_onlysig <- df.test_Divyansh_model_onlysig[df.test_Divyansh_model_onlysig$marital!="single",]

summary(result_tentative_trainDivyansh_model_sig1)

df.train_Divyansh_model_onlysig <- df.train_Divyansh_model_onlysig[df.train_Divyansh_model_onlysig$marital!="yes",]
result_tentative_trainDivyansh_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + housing + loan + poutcome, 
                                         data=df.train_Divyansh_model_onlysig, family = binomial)

df.test_Divyansh_model_onlysig <- df.test_Divyansh_model_onlysig[df.test_Divyansh_model_onlysig$marital!="yes",]

summary(result_tentative_trainDivyansh_model_sig1)

df.train_Divyansh_model_onlysig <- df.train_Divyansh_model_onlysig[df.train_Divyansh_model_onlysig$job!="management",]
result_tentative_trainDivyansh_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + housing + loan + poutcome, 
                                         data=df.train_Divyansh_model_onlysig, family = binomial)

df.test_Divyansh_model_onlysig <- df.test_Divyansh_model_onlysig[df.test_Divyansh_model_onlysig$job!="management",]

summary(result_tentative_trainDivyansh_model_sig1)

df.train_Divyansh_model_onlysig <- df.train_Divyansh_model_onlysig[df.train_Divyansh_model_onlysig$poutcome!="other",]
result_tentative_trainDivyansh_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + housing + loan + poutcome, 
                                         data=df.train_Divyansh_model_onlysig, family = binomial)

df.test_Divyansh_model_onlysig <- df.test_Divyansh_model_onlysig[df.test_Divyansh_model_onlysig$poutcome!="other",]

summary(result_tentative_trainDivyansh_model_sig1)

df.train_Divyansh_model_onlysig <- df.train_Divyansh_model_onlysig[df.train_Divyansh_model_onlysig$job!="entrepreneur",]
result_tentative_trainDivyansh_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + housing + loan + poutcome, 
                                         data=df.train_Divyansh_model_onlysig, family = binomial)

df.test_Divyansh_model_onlysig <- df.test_Divyansh_model_onlysig[df.test_Divyansh_model_onlysig$job!="entrepreneur",]

summary(result_tentative_trainDivyansh_model_sig1)

df.train_Divyansh_model_onlysig <- df.train_Divyansh_model_onlysig[df.train_Divyansh_model_onlysig$education!="unknown",]
result_tentative_trainDivyansh_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + housing + loan + poutcome, 
                                         data=df.train_Divyansh_model_onlysig, family = binomial)

df.test_Divyansh_model_onlysig <- df.test_Divyansh_model_onlysig[df.test_Divyansh_model_onlysig$education!="unknown",]

summary(result_tentative_trainDivyansh_model_sig1)

df.train_Divyansh_model_onlysig <- df.train_Divyansh_model_onlysig[df.train_Divyansh_model_onlysig$job!="student",]
result_tentative_trainDivyansh_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + housing + loan + poutcome, 
                                         data=df.train_Divyansh_model_onlysig, family = binomial)

df.test_Divyansh_model_onlysig <- df.test_Divyansh_model_onlysig[df.test_Divyansh_model_onlysig$job!="student",]

summary(result_tentative_trainDivyansh_model_sig1)

df.train_Divyansh_model_onlysig <- df.train_Divyansh_model_onlysig[df.train_Divyansh_model_onlysig$job!="unemployed",]
result_tentative_trainDivyansh_model_sig1 <- glm(formula = yact ~ age + balance + duration + campaign + previous +
                                           job + marital + education + housing + loan + poutcome, 
                                         data=df.train_Divyansh_model_onlysig, family = binomial)

df.test_Divyansh_model_onlysig <- df.test_Divyansh_model_onlysig[df.test_Divyansh_model_onlysig$job!="unemployed",]

summary(result_tentative_trainDivyansh_model_sig1)



result_Divyansh_model_sig1 <- result_tentative_trainDivyansh_model_sig1
class(result_Divyansh_model_sig1)
print(result_Divyansh_model_sig1)
plot(result_Divyansh_model_sig1)

plot(result_Divyansh_model_sig1)
varImp(result_Divyansh_model_sig1, scale = FALSE)


fit_Divyansh_model <- lm(formula <- yact ~ age + balance + duration + campaign + previous +
                   job + marital + education + housing + loan + poutcome, 
                 data=df.train_Divyansh_model_onlysig)
vif(fit_Divyansh_model)

backward_Divyansh_model <- step(result_Divyansh_model_sig1, direction = 'backward')
summary(backward_Divyansh_model)

result_Divyansh_model_probs <- df.train_Divyansh_model_onlysig
nrow(result_Divyansh_model_probs)
class(result_Divyansh_model_probs)
result_Divyansh_model_probs$prob = predict(result_Divyansh_model_sig1, type=c("response"))
q_Divyansh_model <- roc(y ~ prob, data = result_Divyansh_model_probs)
plot(q_Divyansh_model)
auc(q_Divyansh_model)

CrossTable(df.train_Divyansh_model_onlysig$job, df.train_Divyansh_model_onlysig$y)
CrossTable(df.train_Divyansh_model_onlysig$marital, df.train_Divyansh_model_onlysig$y)
CrossTable(df.train_Divyansh_model_onlysig$education, df.train_Divyansh_model_onlysig$y)
CrossTable(df.train_Divyansh_model_onlysig$default, df.train_Divyansh_model_onlysig$y)
CrossTable(df.train_Divyansh_model_onlysig$housing, df.train_Divyansh_model_onlysig$y)
CrossTable(df.train_Divyansh_model_onlysig$loan, df.train_Divyansh_model_onlysig$y)
CrossTable(df.train_Divyansh_model_onlysig$poutcome, df.train_Divyansh_model_onlysig$y)


hist(df.train_Divyansh_model_onlysig$age)
hist(df.train_Divyansh_model_onlysig$balance)
hist(df.train_Divyansh_model_onlysig$duration)
hist(df.train_Divyansh_model_onlysig$campaign)
hist(df.train_Divyansh_model_onlysig$previous)


result_Divyansh_model_probs$ypred = ifelse(result_Divyansh_model_probs$prob>=.5,'pred_yes','pred_no')
table(result_Divyansh_model_probs$ypred,result_Divyansh_model_probs$y)

df.test_Divyansh_model_onlysig$prob = predict(result_Divyansh_model_sig1, newdata = df.test_Divyansh_model_onlysig, type=c("response"))

df.test_Divyansh_model_onlysig$ypred = ifelse(df.test_Divyansh_model_onlysig$prob>=.5,'pred_yes','pred_no')
table(df.test_Divyansh_model_onlysig$ypred,df.test_Divyansh_model_onlysig$y)


ks_plot(actuals=result_Divyansh_model_probs$y, predictedScores=result_Divyansh_model_probs$ypred)



