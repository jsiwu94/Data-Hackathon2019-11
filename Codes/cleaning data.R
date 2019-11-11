df<- bank_full_cleaned
df <- read.csv(file = 'bank_full_cleaned.csv')

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
library(data.table)
#install.packages('corrgram')

# library(corrgram)
# corrgram(df$day, order=TRUE, lower.panel=panel.shade,
#          upper.panel=panel.pie, text.panel=panel.txt,
#          main=df)

#df %>% select(1:16) -> df

#dummyVars(" ~ .", data = df_pca) = c(job,marital,education,contact,poutcome))
library(data.table)
library(mltools)
df_pca <- one_hot(as.data.table(df))

# df_pca.pca <- prcomp(df_pca, center = TRUE,scale. = TRUE)
# 
# summary(df_pca.pca)

# library(ggfortify)
# autoplot(prcomp(df_pca.pca),colour = clustering_data$segment,legend = TRUE, legendLabs = c("CLuster1","Cluster2","Cluster3","Cluster4"),label = clustering_data$segment,loadings = TRUE, loadings.colour = 'blue',
#          loadings.label = TRUE, loadings.label.size = 4)


dfs = subset(df_pca, select = -c(default_no,housing_no,loan_no,y_no,month_apr,month_may,month_jan,month_feb,month_mar,month_jun,month_jul,month_aug,month_sep,month_oct,month_nov,month_dec) )
df_month = select(df,month)
head(df_month)
dim(df_month)
df_month = df_month[1]
df_final = cbind(dfs,df_month)

tst <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
for(i in tst){
  for(j in df_month$month){
    if(i == j){
      mutate(df_month, month = index(i),inplace = TRUE)
    }
  }
}
