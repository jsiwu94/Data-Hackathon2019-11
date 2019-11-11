library(readxl)
library(ggplot2)
library(mosaic)
library(wesanderson)
library(forecast)
library(dplyr)
library(tidyverse)
library(rmarkdown)
library(stats)
library(gmodels)
library(randomForest)

#start reading the data
df <- read.csv(file = "~/Documents/Data-Hackathon-ModelTeam/bank_full_cleaned.csv"
               ,header=TRUE,sep=",")

head(df)
colnames(df)
summary(df, na.rm = TRUE)
view(df)


a <- df$duration
quantile(a,0.99)
quantile(a,0.95)

df$duration <- ifelse(df$duration >= 751, 751,df$duration)

prev <- data.frame(df %>%
  group_by("Number of Previous Contact" = previous) %>%
  summarise(freq = n()))

ggplot(df,aes(x=Number.of.Previous.Contact, fill = freq)) + geom_bar() +
  ggtitle("Number of Contact") + xlab("Number of Previous Contact") + 
  ylab("Loan") + labs(fill= "y")



#overall baseline
##quick skim through of the freq dist
job <- table(df['job'])
marital <- table(df['marital'])


age <- data.frame(Variable.name = "age",
           df %>%
             summarise(N = n(),
                       Min = min(age),
                       Med = median(age),
                       Max = max(age),
                       Mean = round(mean(age),1),
                       Std.Dev = round(sd(age),1),
                       Var = round((sd(age))^2,1),
                       Min_zscore = round(min(age),1),
                       Max_zscore = round(max(age),1) ))

normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}




####Model
train <- read.csv(file = "~/Documents/Data-Hackathon-ModelTeam/data_normalised.csv"
                  ,header=TRUE,sep=",")

set.seed(100)
train_s <- sample_n(train[train$y == 0,], 5289)
train_y <- train[train$y == 1,]

combined <- rbind(train_s,train_y)
rownames(combined) <- seq(length=nrow(combined))
nrow(combined)

#write.csv(combined,"~/Documents/Data-Hackathon-ModelTeam/random_samp.csv", row.names = FALSE)

combined <- read.csv(file = "~/Documents/Data-Hackathon-ModelTeam/random_samp.csv"
         ,header=TRUE,sep=",")

split <- sample(nrow(combined), 0.8*nrow(combined))
train_m <- combined[split,]
test_m <- combined[-split,]

summary(train$duration)
colnames(train_m)

bank.logistic <- glm(y_yes~., 
                     data = train_m, family = binomial)

#predict:: train
predictions.logistic.train <- predict(bank.logistic,train_m,type = "response")
predictions.logistic.binary.train <- ifelse(predictions.logistic.train > .5,"yes","no")

#predict:: test
predictions.logistic.test <- predict(bank.logistic,test_m,type = "response")
predictions.logistic.binary.test <- ifelse(predictions.logistic.test > .5,"yes","no")

#Confusion matrix::
con.mat.log.train <- prop.table(table(train_m$y,predictions.logistic.binary.train))
con.mat.log.test <- prop.table(table(test_m$y,predictions.logistic.binary.test))


pr <- prediction(predictions.logistic.binary.test,train_m$y_yes)
perf <- performance(pr,measure = "tpr",x.measure = "fpr")
plot(perf)
auc(new_test$Survived,log_predict) #0.8008

summary(bank.logistic)

####asklfjsdkfjgskdjf
table(test_m$y,predictions.logistic.binary.test)


# Display variable importance measures for a random forest model
importance(forest, type=2)

# predict the target using the evaluate data
forest.pred <- predict(forest, df.validate)
forest.perf <- table(df.validate$target, forest.pred,
                     dnn=c("Actual", "Predicted"))
forest.perf



##plotting

#descriptive stat::
ggplot(df, aes(x=job, fill = y)) + geom_bar() +
  ggtitle("Job types vs decision",size = 20) + xlab("Job Types",size = 20) + 
  ylab("Title",size = 20) + labs(fill= "y")

ggplot(df, aes(x=education, fill = y)) + geom_bar() + facet_wrap(~marital)+
  ggtitle("Marital status vs education") + xlab("Educational qualification") + 
  ylab("Count") + labs(fill= "y")

ggplot(df, aes(x=housing, fill = y)) + geom_bar() + facet_wrap(~default)+
  ggtitle("Housing loan vs default") + xlab("Housing loan") + 
  ylab("Count") + labs(fill= "y")

ggplot(df, aes(x=loan, fill = y)) + geom_bar() + facet_wrap(~default)+
  ggtitle("Personal loan vs default") + xlab("Personal loan") + 
  ylab("Count") + labs(fill= "y")


ggplot(df,aes(x=poutcome, fill = y)) + geom_bar() +
  ggtitle("Success vs previous outcome") + xlab("Previous outcome") + 
  ylab("Count") + labs(fill= "y")


ggplot(df,aes(x=age, fill = y)) + geom_bar() +
  ggtitle("Success vs previous outcome") + xlab("Previous outcome") + 
  ylab("Count") + labs(fill= "y")


table(df$job)
##Dummy Vars for Job
df['admin'] <- ifelse(df$job == "admin.",1,0)
df['blue-collar']<- ifelse(df$job == "blue-collar",1,0)
df['entrepreneur']<- ifelse(df$job == "entrepreneur",1,0)
df['housemaid']<- ifelse(df$job == "housemaid",1,0)
df['management']<- ifelse(df$job == "management",1,0)
df['retired']<- ifelse(df$job == "retired",1,0)
df['self-employed']<- ifelse(df$job == "self-employed",1,0)
df['services']<- ifelse(df$job == "services",1,0)
df['student']<- ifelse(df$job == "student",1,0)
df['technician']<- ifelse(df$job == "technician",1,0)
df['unemployed']<- ifelse(df$job == "unemployed",1,0)
df['unknown']<- ifelse(df$job == "unknown",1,0)

##Dummy Vars for marital
df['divorced']<- ifelse(df$marital == "divorced",1,0)
df['married']<- ifelse(df$marital == "married",1,0)
df['single']<- ifelse(df$marital == "single",1,0)
#df['normage'] <- normalize(df$age)

df['dependent']<-ifelse(df$y == "yes",1,0)
df['pdays_neg1'] <- ifelse(df$pdays <= -1,1,0)

df['zero_previous'] <- ifelse(df$previous == 0,1,0)
df['unknown_poutcome'] <- ifelse(df$poutcome == -1,1,0)


df_1<- select(df,c('age','balance','day','duration','campaign','pdays','previous'))

cormat <- round(cor(df_1),2)
head(cormat)

library(reshape2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       space = "Lab", 
                       name="Pearson\nCorrelation")

#all independent var cortest
#df_cor <- df[,18:32]
cortest <- data.frame(cor(df_cor, method="pearson",use="complete.obs"))
view(cortest) 

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()



data.frame(Variable.name = "balance",
           df %>%
             summarise(N = n(),
                       Min = min(balance),
                       q1 = quartile(balance,.25),
                       Med = median(balance),
                       Max = max(balance),
                       Mean = round(mean(balance),1),
                       Std.Dev = round(sd(balance),1),
                       Var = round((sd(balance))^2,1)
                       ))

