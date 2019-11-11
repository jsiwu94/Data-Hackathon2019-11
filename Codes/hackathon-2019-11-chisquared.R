library(ade4)
library(dplyr)
library(data.table)

df <- read.table("~/Documents/Data-Hackathon-ModelTeam/bank_full_cleaned.csv", sep=",", stringsAsFactors = FALSE,header = TRUE)
head(df)

chisq.test(df$job, df$y, correct=FALSE)

# Pearson's Chi-squared test
# 
# data:  df$job and df$y
# X-squared = 836.11, df = 11, p-value < 2.2e-16

chisq.test(df$marital, df$y, correct=FALSE)

# Pearson's Chi-squared test
# 
# data:  df$marital and df$y
# X-squared = 196.5, df = 2, p-value < 2.2e-16

chisq.test(df$education, df$y, correct=FALSE)
# Pearson's Chi-squared test
# 
# data:  df$education and df$y
# X-squared = 238.92, df = 3, p-value < 2.2e-16

chisq.test(df$default, df$y, correct=FALSE)
# Pearson's Chi-squared test
# 
# data:  df$default and df$y
# X-squared = 22.724, df = 1, p-value = 1.871e-06


chisq.test(df$housing, df$y, correct=FALSE)
# Pearson's Chi-squared test
# 
# data:  df$housing and df$y
# X-squared = 875.69, df = 1, p-value < 2.2e-16

chisq.test(df$contact, df$y, correct=FALSE)
# Pearson's Chi-squared test
# 
# data:  df$contact and df$y
# X-squared = 1035.7, df = 2, p-value < 2.2e-16

chisq.test(df$loan, df$y, correct=FALSE)
# Pearson's Chi-squared test
# 
# data:  df$loan and df$y
# X-squared = 210.19, df = 1, p-value < 2.2e-16


chi_squared_df <- data.frame(x_var = c('job','marital','education','default','housing','contact','loan'),
             Chisquared = c(836.11,196.5,238.92,22.724,875.69,1035.7,210.19), 
              df = c(11,2,3,1,1,2,1), 
              pvalue = c('< 2.2e-16','< 2.2e-16','< 2.2e-16','1.871e-06','< 2.2e-16','< 2.2e-16','< 2.2e-16'))


view(chi_squared_df)

