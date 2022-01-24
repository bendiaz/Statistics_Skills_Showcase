# A. Benjamin Diaz
# Sept 2021
# Data Science Assessment - Viz ONLY

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
# library(datapasta)
# library(lmtest)
# library(broom)
# # install.packages("caret")
# library(caret)
# # install.packages('Boruta')
# library(Boruta)
# # install.packages('Rcpp')
# library(Rcpp)
# # install.packages("varImp")
# library(varImp)

# install.packages("esquisse")


rm(list=ls())
setwd('PATH_TO_DATA')
df <- read_excel('Data Scientist Skill Assessment Data Example 2021.xlsx')

# df %>%
#   select(everything()) %>%
#   summarize_all(funs(sum(is.na(.))))


df$SAT_Total[is.na(df$SAT_Total)] <- round(mean(df$SAT_Total, na.rm = TRUE)) #replace NA with avg


df$Grad_6yr <-as.factor(df$Grad_6yr) 
df$Sex <-as.factor(df$Sex)


# convert_institution <- function(x) {
#   B <- factor(x, levels=c( "CA Public HS", "CA Private HS", "CSU", NA))
#   
#   values <- c(1, 2, 3, 4)
#   values[B]
# }
# ########
# convert_objective <- function(x) {
#   C <- factor(x, levels=c( "BS", "BA", "Other Bachelors", "None"))
#   
#   values <- c(1, 2, 3, 4)
#   values[C]
# }
# 
# convert_sex <- function(x) {
#   D <- factor(x, levels=c( "M", "F"))
#   
#   values <- c(1, 0)
#   values[D]
# }
# df$Institution_of_Origin <- lapply(df$Institution_of_Origin, convert_institution)
# sapply(df, class)
# df[, c(3,5,6)] <- sapply(df[, c(3,5,6)], as.numeric)


drops <- c("Grad_4yr", "Grad_5yr", "ID", "Year_Admitted")
df <- df[, !(names(df) %in% drops)]





library(ggplot2)

ggplot(df) +
 aes(x = Grad_6yr, y = SAT_Total, fill = Grad_6yr) +
 geom_boxplot(shape = "circle") +
 scale_fill_manual(values = c(`0` = "#D61A0C", 
`1` = "#1BD952")) +
 labs(x = "6 Year Graduation Rate", y = "SAT Score") +
 theme_classic()

df %>%
 filter(Institution_of_Origin %in% c("CA Public HS", "CA Private HS")) %>%
 ggplot() +
 aes(x = Grad_6yr, y = SAT_Total, fill = Grad_6yr) +
 geom_boxplot(shape = "circle") +
 scale_fill_manual(values = c(`0` = "#D61A0C", 
`1` = "#1BD952")) +
 labs(x = "6 Year Graduation Rate", y = "SAT Score") +
 theme_classic() +
 facet_grid(vars(Sex), 
 vars(Institution_of_Origin))

df %>%
 filter(Institution_of_Origin %in% c("CA Public HS", "CA Private HS")) %>%
 ggplot() +
 aes(x = Grad_6yr, y = SAT_Total, fill = Grad_6yr) +
 geom_boxplot(shape = "circle") +
 scale_fill_manual(values = c(`0` = "#D61A0C", 
`1` = "#1BD952")) +
 labs(x = "6 Year Graduation Rate", y = "SAT Score") +
 theme_classic() +
 facet_grid(vars(Sex), 
 vars(Institution_of_Origin))



esquisse::esquisser(df)

library(dplyr)
library(ggplot2)

df %>%
 filter(Institution_of_Origin %in% c("CA Public HS", "CA Private HS")) %>%
 ggplot() +
 aes(x = Grad_6yr, y = HS_GPA, fill = Grad_6yr) +
 geom_boxplot(shape = "circle") +
 scale_fill_hue(direction = 1) +
 labs(x = "6 Year Graduation Rate", y = "HS GPA", fill = "Graduated") +
 theme_classic() +
 facet_grid(vars(Sex), 
 vars(Institution_of_Origin))

##let's count these things up now

# xtabs(~Grad_6yr+ HS_GPA + SAT_Total, data=df )
GPA_ss<- df %>%
  group_by(HS_GPA,Grad_6yr) %>%
  summarise(counts=n())
GPA_ss
