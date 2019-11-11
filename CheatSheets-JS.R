########################Libraries
library(readxl)
library(ggplot2)
library(mosaic)
library(wesanderson)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stats4)
library(rgl)
library("plot3D")
library(devtools)
library(rgl)
library(mosaic)
library(tidyverse)
library(rpart)
library("quantmod")
library(statisticalModeling)
library("partykit")
library(tree)
library(randomForest)


########################Basic DataFrames

#readcsv
df <- read.csv(file = "/x",header=TRUE,sep=",")
head(df)
colnames(df)

#1 : finding columns with na's & null 
df <- df %>%
  sapply(function(x) any(is.na(x) | x == "" | x == "?")) %>%
  which()  %>%
  names() 

#2 : ifelse
df$column <- ifelse(df$column  =="", "undefined",as.character(df$column))

#3 : groupby
df %>% 
  group_by(column) %>%
  summarise(rowcnt = n())

#selecting and filtering df
df <- df %>% 
  select(-column)

df<- filter(df,!is.na(column))

#mutate
df <- mutate(frror=ifelse(min(column)!=1,1,0))

#function
normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}

#########################plotting
#boxplot
ggplot(df, aes(x= , y =  , fill = ))+ 
  stat_boxplot(geom ='errorbar') + 
  scale_fill_brewer(palette="Paired")+
  labs(x = "...", y = "..."
       , title = "...") +
  geom_boxplot() 

#barplot
ggplot(df, aes(x= ,y= , color = , fill = )) +
  geom_bar(stat = "identity",position="dodge") +
  labs(x= "...", y = "...", title = "...") 

#Linear Correlation
ggplot(df, aes(x= ,y= ))+
  geom_point()+
  geom_smooth(type = "lm")+
  labs(title = "...")


#########################Model

##kmeans
#function
kmeans(df, 6, nstart = 100)

#sse curve
aflsdk;

##linear regression
#packages & plotting
install.packages("rcompanion")
library(rcompanion)
plotNormalHistogram(job.satisfaction.data$score)
#model
summary(fitted.model<- lm(y ~ x1 + x2 +x3, 
                           data=y))
#predict
print(predict(fitted.model, data.frame(x1="F", x2=40, x3="bachelor")))


##binary logistic regression
#choosing which factor is more important for the Y outcome
approach.rel<- relevel(companies.data$approach, ref="comp")
#model
summary(fitted.model<- glm(approach.rel ~ x1 + x2, 
                           data=df, family=binomial(link=logit)))
#predict
print(predict(fitted.model, type="response", 
              data.frame(x1="sole", x2=40)))


##Poisson Model
#fitting Poisson model
summary(fitted.model<- glm(y ~ x1 + x2 + x3, 
                           data=df, family=poisson(link=log)))
#using fitted model for prediction
print(predict(fitted.model, data.frame(x1="M", x2=55, x3="no"), 
              type="response"))


##Gamma Regression = For right skewed data
#plotting histogram with fitted normal density
library(rcompanion)
plotNormalHistogram(Y)
#fitting gamma regression 
summary(fitted.model<- glm(y ~ x1 + x2, data=df,
                           family=Gamma(link=log)))

#using fitted model for prediction
print(predict(fitted.model, type="response", data.frame(x1=4, x2=2)))


##Decision Tree 
model <- rpart(y~., data=df, method="anova" ,
      control=rpart.control(minsplit=60, minbucket=30, maxdepth=4 ))
plot(as.party(model))
print(model)
summary(model)
#cp
model$cptable
opt <- which.min(model$cptable [, "xerror"])
cp<-model$cptable[opt,"CP"]
#prune
pruned.model<-prune(model,cp=cp)
plot(as.party(pruned.model)   )
print(pruned.model)
summary(pruned.model)

##Random Forest
forest <- randomForest(y~.,data = df, ntree = 500)
plot(forest)
forest$predicted
#plot error rate
oob.error.data <- data.frame(
  Trees=rep(1:nrow(forest$err.rate),times=3),
  Type=rep(c("OOB","y","n"),each=nrow(forest$err.rate)),
  Error=c(model$err.rate[,"OOB"],
          model$err.rate[,"y"],
          model$err.rate[,"n"]))
ggplot(data = oob.error.data, aes(x=Trees,y=Error))+
  geom_line(aes(color=Type))