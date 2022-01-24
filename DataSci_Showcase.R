# A. Benjamin Diaz
# Sept 2021
# Data Scientist Assessment

library(devtools)
library(foreign)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(texreg)
library(xtable)
library(magrittr)
library(tibble)
library(readxl)
library(forcats)
library(ggrepel)
library(reprex)
library(datapasta)
library(lmtest)
library(broom)
# install.packages("caret")
library(caret)
# install.packages('Boruta')
library(Boruta)
# install.packages('Rcpp')
library(Rcpp)
# install.packages("varImp")
library(varImp)




rm(list=ls())
setwd('PATH_TO_DATA')
df <- read_excel('Data Scientist Skill Assessment Data Example 2021.xlsx')

df %>%
  select(everything()) %>%
  summarize_all(funs(sum(is.na(.))))


df$SAT_Total[is.na(df$SAT_Total)] <- round(mean(df$SAT_Total, na.rm = TRUE)) #replace NA with avg

mean(df$SAT_Total)
mean(df$HS_GPA)

v <- df %>% select(HS_GPA, SAT_Total)
summary(v)

ggplot(data = v, mapping = aes(x=SAT_Total)) + 
  geom_histogram(aes(y=..density..),fill="bisque",color="white",alpha=0.7) + 
  geom_density() +
  geom_rug() +
  labs(x='SAT Scores') +
  theme_minimal()

ggplot(data = v, mapping = aes(x=SAT_Total)) + 
  geom_histogram(aes(y=..density..),fill="bisque",color="white",alpha=0.7) + 
  geom_density() +
  geom_rug() +
  labs(x='SAT Scores') +
  theme_minimal()

#vgroup <- paste(v, df$Grad_6yr)
# df$vgroup <- NULL

v$ss <- df$Grad_6yr

##box plot code

# ggplot(data = vgroup, mapping = aes(x=tag,y=log10(MeanHouseholdIncome))+ 
#   geom_jitter(aes(color='blue'),alpha=0.2) +
#   geom_boxplot(fill="bisque",color="black",alpha=0.3) + 
#   labs(x='mean education per house') +
#   guides(color=FALSE) +
#   theme_minimal() 





~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #2
  
#campus wide comparison  
df$Grad_4yr <-as.factor(df$Grad_4yr)  
df$Grad_5yr <-as.factor(df$Grad_5yr) 
df$Grad_6yr <-as.factor(df$Grad_6yr) 
  

xtabs(~Grad_4yr +Math_All_4yr_HS, data=df)  
xtabs(~Grad_5yr +Math_All_4yr_HS, data=df)  
xtabs(~Grad_6yr +Math_All_4yr_HS, data=df)  

#prediction from HS GPA
model1 <- glm(Grad_6yr ~ HS_GPA , data=df, family="binomial")
summary(model1)
#p-value
coef(summary(model1))[,4]

#residual plots

plot(density(resid(model1, type='response')))
lines(density(resid(model1, type='pearson')), col='red')

plot(density(resid(model1, type='deviance')))


scatter.smooth(1:1000, rstandard(model1, type='deviance'), col='gray')
scatter.smooth(predict(model1, type='response'), rstandard(model1, type='deviance'), col='gray')

scatter.smooth(sqrt(predict(model1, type='response')), qresid(m0), col='gray')
#there seems to be a pattern so the data may be skewed and therefore cannot be trusted, approach with caution


#now we focus on the ACTUAL task: HHS cluster
df_HHS <-df %>% filter(Cluster_Admitted == "Health & Human Svcs")

##HHS test - Sex

model_sex <- glm(Grad_6yr ~ Sex, data=df_HHS, family = "binomial")
summary(model_sex)
unique(df$Cluster_Admitted)


# 
# df_HHS %>%
#   select(everything()) %>%
#   summarize_all(funs(sum(is.na(.))))
#there's an NA in the inst of origin variable (na.action=na.omit)
df_HHS$Sex <-as.factor(df_HHS$Sex) 
#now time for some logistic regression:

model1hhs <- glm(Grad_6yr ~ HS_GPA + SAT_Total + Transfer_Units_Earned + Sex +
                   Institution_of_Origin+ Degree_Objective, data = df_HHS,na.action=na.omit,
                 family = "binomial")
summary(model1hhs)
tidy(model1hhs)

plot_coeffs <- function(mlr_model) {
  coeffs <- coefficients(mlr_model)
  mp <- barplot(coeffs, col="#3F97D0", xaxt='n', main="Regression Coefficients")
  lablist <- names(coeffs)
  text(mp, par("usr")[3], labels = lablist, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
}

plot_coeffs(model1hhs)

unique(df_HHS$Institution_of_Origin)
unique(df_HHS$Degree_Objective)
unique(df_HHS$Sex)
#convert character data into numeric with corresponding values:
convert_institution <- function(x) {
  B <- factor(x, levels=c( "CA Public HS", "CA Private HS", "CSU", NA))
  
  values <- c(1, 2, 3, 4)
  values[B]
}
########
convert_objective <- function(x) {
  C <- factor(x, levels=c( "BS", "BA", "Other Bachelors", "None"))
  
  values <- c(1, 2, 3, 4)
  values[C]
}

convert_sex <- function(x) {
  D <- factor(x, levels=c( "M", "F"))
  
  values <- c(1, 0)
  values[D]
}

df_HHS$Institution_of_Origin <- lapply(df_HHS$Institution_of_Origin, convert_institution)


df_HHS$Degree_Objective <- lapply(df_HHS$Degree_Objective, convert_objective)

df_HHS$Sex <- lapply(df_HHS$Sex, convert_sex)

sapply(df_HHS, class)
df_HHS[, c(3,5,6)] <- sapply(df_HHS[, c(3,5,6)], as.numeric)

#fixed x must be numeric issue
correlationMatrix <- cor(df_HHS[,3:9])

print(correlationMatrix)
#######
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# # train the model
# model00 <- train(Grad_6yr~., data=df_HHS, method="lvq", preProcess="scale", trControl=control, na.action = na.exclude
# # estimate variable importance
# importance <- varImp(model00, scale=FALSE)
# # summarize importance
# print(importance)
# # plot importance
# plot(importance)

drops <- c("Grad_4yr", "Grad_5yr", "ID", "Year_Admitted", "Cluster_Admitted")
df_HHS <- df_HHS[, !(names(df_HHS) %in% drops)]
boruta_output <- Boruta(Grad_6yr ~ ., data=na.omit(df_HHS), doTrace=0)  

names(boruta_output)
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  


roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

#########
regressor <- randomForest(Grad_6yr ~ . , data=na.omit(df_HHS), importance=TRUE) # fit the random forest with default parameter

caret::varImp(regressor) # get variable importance, based on mean decrease in accuracy

caret::varImp(regressor, conditional=TRUE) # conditional=True, adjusts for correlations between predictors


# varImpAUC(regressor) # more robust towards class imbalance. (not working)

##XG boost

xgregress=train(Grad_6yr ~ . , data=na.omit(df_HHS), method = "xgbTree",trControl = trainControl("cv", number = 10),scale=T)

caret::varImp(xgregress)

####XGboost and RF both gave similar results so we can now decide to go with that

#now to do stats test:

modelhhs2 <- glm(Grad_6yr ~  Transfer_Units_Earned + Math_All_4yr_HS+ HS_GPA , data = df_HHS,na.action=na.omit,
                 family = "binomial")
summary(modelhhs2)

# plot_coeffs <- function(mlr_model) {
#   coeffs <- coefficients(mlr_model)
#   mp <- barplot(coeffs, col="#3F97D0", xaxt='n', main="Regression Coefficients")
#   lablist <- names(coeffs)
#   text(mp, par("usr")[3], labels = lablist, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
# }
# 
# plot_coeffs(modelhhs2)

#p-value
coef(summary(modelhhs2))[,4]

#residual plots

plot(density(resid(modelhhs2, type='response')))
lines(density(resid(modelhhs2, type='pearson')), col='red')

plot(density(resid(modelhhs2, type='deviance')))


scatter.smooth(1:1000, rstandard(modelhhs2, type='deviance'), col='gray')
scatter.smooth(predict(modelhhs2, type='response'), rstandard(modelhhs2, type='deviance'), col='gray')

scatter.smooth(sqrt(predict(modelhhs2, type='response')), resid(modelhhs2), col='gray')

#let's grab P value

coef(summary(modelhhs2))[,4]
#let's check chi-square

hhs2chi <- modelhhs2$null.deviance - modelhhs2$deviance
hhs2chi

#let's find degrees of freedom by hand

modelhhs2.freedom <- modelhhs2$df.null - modelhhs2$df.residual
modelhhs2.freedom

#check chi probability

chisq.probhhs2 <- 1 - pchisq(hhs2chi, modelhhs2.freedom)
chisq.probhhs2

###########model with 2 DoF

modelhhs3 <- glm(Grad_6yr ~  Transfer_Units_Earned + Math_All_4yr_HS, data = df_HHS,na.action=na.omit,
                 family = "binomial")
summary(modelhhs3)

#p-value
coef(summary(modelhhs3))[,4]

#residual plots

plot(density(resid(modelhhs3, type='response')))
lines(density(resid(modelhhs3, type='pearson')), col='red')

plot(density(resid(modelhhs3, type='deviance')))


scatter.smooth(1:1000, rstandard(modelhhs3, type='deviance'), col='gray')
scatter.smooth(predict(modelhhs3, type='response'), rstandard(modelhhs3, type='deviance'), col='gray')

scatter.smooth(sqrt(predict(modelhhs3, type='response')), resid(modelhhs3), col='gray')

#let's grab P value

coef(summary(modelhhs3))[,4]
#let's check chi-square

hhs3chi <- modelhhs3$null.deviance - modelhhs3$deviance
hhs3chi

#let's find degrees of freedom by hand

modelhhs3.freedom <- modelhhs3$df.null - modelhhs3$df.residual
modelhhs3.freedom

#check chi probability

chisq.probhhs3 <- 1 - pchisq(hhs3chi, modelhhs3.freedom)
chisq.probhhs3



###only math all 4 years
modelhhs4 <- glm(Grad_6yr ~  Math_All_4yr_HS, data = df_HHS,na.action=na.omit,
                 family = "binomial")
summary(modelhhs4)





#p-value
coef(summary(modelhhs4))[,4]

#residual plots

plot(density(resid(modelhhs4, type='response')))
lines(density(resid(modelhhs4, type='pearson')), col='red')

plot(density(resid(modelhhs4, type='deviance')))


scatter.smooth(1:1000, rstandard(modelhhs4, type='deviance'), col='gray')
scatter.smooth(predict(modelhhs4, type='response'), rstandard(modelhhs4, type='deviance'), col='gray')

scatter.smooth(sqrt(predict(modelhhs4, type='response')), resid(modelhhs4), col='gray')

#let's grab P value

coef(summary(modelhhs4))[,4]
#let's check chi-square

hhs4chi <- modelhhs4$null.deviance - modelhhs4$deviance
hhs4chi

#let's find degrees of freedom by hand

modelhhs4.freedom <- modelhhs4$df.null - modelhhs4$df.residual
modelhhs4.freedom

#check chi probability

chisq.probhhs4 <- 1 - pchisq(hhs4chi, modelhhs4.freedom)
chisq.probhhs4
