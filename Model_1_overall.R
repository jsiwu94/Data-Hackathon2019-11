#code for overall ---- iteration 1

install.packages('caTools')
library(caTools)

library(ade4)
library(dplyr)
library(data.table)


#dependent variable if the change was positive two days in a row!
###########################

# CUSTOMIZE DATA FILE: read.table reads a file in table format and creates a data frame from it

df <- read.table("df_final_ish.csv", sep=",", stringsAsFactors = FALSE,header = TRUE)
head(df)
df['zero_previous'] <- ifelse(df$previous == 0,1,0)
head(df)
df['pdays'] <- ifelse(df$pdays == -1,1,0)
head(df)
df_new <- subset(df,select = -c(day,month,pdays,previous,job_unknown,education_unknown,contact_unknown,poutcome_unknown,marital_single,X,zero_previous) )
head(df_new)

normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}

a = df_new$balance    
quantile(a, c(.99)) #13164.9 

b = df_new$duration    
quantile(b, c(.95)) #751

df_new['balance_new']<- ifelse(df_new$balance > 13164.9,13164.9,df_new$balance)
df_new['duration_new']<- ifelse(df_new$duration > 751,751,df_new$duration)
df_new1 <- subset(df_new,select = -c(balance,duration) )

data <- sapply(df_new1,normalize)
head(data)
#df['dep'] <- ifelse(df$y == "yes",1,0)
set.seed(33333) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  

head(data)

sample <- sample.int(n = nrow(data), size = floor(.80*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]



head(train)

head(test)

train1 <- as.data.frame(train)
class(train1)
test1 <- as.data.frame(test)
class(test1)
#logistic regression model
model <- glm (y_yes ~ ., data = train1, family = binomial)
summary(model)

predict <- predict(model, type = 'response')

#confusion matrix
table(train1$y_yes, predict > 0.5)

pred <- predict(model, newdata = test1, type = "response")
hist(pred)
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- test1$y_yes
y_act
table(y_pred_num,y_act)
mean(y_pred == y_act)


#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, train1$y_yes)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

