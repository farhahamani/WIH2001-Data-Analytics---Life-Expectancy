install.packages("dplyr")
library(dplyr)
install.packages("data.table")
library(data.table)
install.packages("DT")
library(DT)
install.packages("kableExtra")
library(kableExtra)
install.packages("knitr")
library(knitr)
install.packages("tidyverse")
library(tidyverse)
install.packages("scales")
library(scales)
install.packages("caret")
library(caret)
install.packages("psych")
library(psych)
install.packages("stats")
library(stats)
install.packages("leaps")
library(leaps)
install.packages("GGally")
library(GGally)
install.packages("MASS")
library(MASS)
install.packages("lmtest")
library(lmtest)
install.packages("car")
library(car)
install.packages("MLmetrics")
library(MLmetrics)


#read life life expectancy data .csv
X <- read.csv("C:/Users/ASUS/Desktop/sem 4/WIH2001 Data Analytic/Individual Assignment/life expectancy data.csv")


#data inspection
head(X) #first 6 row
tail(X) #last 6 row
dim(X) #check for columns and row

#data cleaning
sum(is.na(X))
cbind(lapply(lapply(X, is.na), sum))

#checking for data type
#checking data type and do correction of data type
str(X)
X <- X %>%
  mutate(Status = as.factor(Status))
str(X)

#missing data is found in X data, rows with missing data will be removed and put into new variable called X_clean.
X_clean <- na.omit(X)
anyNA(X_clean)

sum(is.na(X))
cbind(lapply(lapply(X, is.na), sum))

show(X_clean)
sum(is.na(X_clean))
cbind(lapply(lapply(X_clean, is.na), sum))

summary(X_clean)

#Outlier analysis
#Plotting box plots of life expectancy to understand outliers
boxplot(X_clean$Life.expectancy, xlab="Life Expectancy")

#removing outlier
outliers <- boxplot(X_clean$Life.expectancy, plot=FALSE)$out

X_clean<- X_clean[-which(X_clean$Life.expectancy %in% outliers),]

dim(X_clean)

#descriptive
remove.packages(GGally)
install.packages("GGally")
library(GGally)

remove.packages("rlang")
install.packages("rlang")
library(rlang)
library(dplyr)

ggcorr(X_clean, label = T, size =3)

#descriptive analysis
summary(X_clean)
X_clean %>% summarise(Min = min(Life.expectancy,na.rm = TRUE),
                                 Q1 = quantile(Life.expectancy,probs = .25,na.rm = TRUE),
                                 Median = median(Life.expectancy, na.rm = TRUE),
                                 Q3 = quantile(Life.expectancy,probs = .75,na.rm = TRUE),
                                 Max = max(Life.expectancy,na.rm = TRUE),
                                 Mean = mean(Life.expectancy, na.rm = TRUE),
                                 SD = sd(Life.expectancy, na.rm = TRUE),
                                 n = n(),
                                 Missing = sum(is.na(Life.expectancy)))
#histogram
life_expectancy <- X_clean$Life.expectancy
hist(life_expectancy)

hist(life_expectancy,
     main="Life Expectancy",
     xlab="Years",
     xlim=c(20,100),
     col="darkmagenta",
     freq=FALSE
)

#linear regression
#data preprocessing
#split the data into train and test for model building
n_train <- round(0.8 * nrow(X_clean))
train_indices <- sample(1:nrow(X_clean), n_train)
train_data <- X_clean[train_indices, ]
test_data <- X_clean[-train_indices, ]

#first model
algo1 <- as.formula("Life.expectancy ~ Alcohol + percentage.expenditure + Hepatitis.B + Measles +  BMI + under.five.deaths + Polio+ Total.expenditure + Diphtheria  + thinness..1.19.years + thinness.5.9.years + GDP + Income.composition.of.resources")
model1 <- lm(algo1, data = train_data)
summary(model1)

r_sq1 <- summary(model1)$r.squared
prediction1 <- predict(model1, newdata = test_data)
residuals1 <- test_data$Life.expectancy - prediction1
rmse1 <- sqrt(mean(residuals1^2, na.rm=TRUE))
summary(model1$coefficients)



#second model
#dropping insignificant variables like measles, percentage.expenditure, Hepatitis.B, Under.five.deaths, Thinness.5.9.years
formula2 <- as.formula("Life.expectancy ~  Alcohol +  Diphtheria  +  BMI +  Polio + Total.expenditure + thinness..1.19.years +  Income.composition.of.resources")
model2 <- lm(formula2, data = train_data)
summary(model2)

r_sq2 <- summary(model2)$r.squared
prediction2 <- predict(model2, newdata = test_data)
residuals2 <- test_data$Life.expectancy - prediction2
rmse2 <- sqrt(mean(residuals2^2, na.rm=TRUE))

#comparing models

print(paste0("R-squared for first model:", round(r_sq1, 4)))
print(paste0("R-squared for second model: ", round(r_sq2, 4)))
print(paste0("RMSE for first model: ", round(rmse1, 2)))
print(paste0("RMSE for second model: ", round(rmse2, 2)))

confint(model2, level=0.95)

#prediction
#prediction
test_data$prediction <- predict(model2, newdata = test_data)
ggplot(test_data, aes(x = prediction, y = Life.expectancy)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")


#residuals vs linear model prediction
test_data$residuals <- test_data$Life.expectancy - test_data$prediction
ggplot(data = test_data, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "purple", alpha = 0.7) + geom_hline(yintercept = 0, linetype = 4, color = "red") +
  ggtitle("Residuals vs. Linear model prediction")


#histogram for residuals
ggplot(test_data, aes(x = residuals)) + 
  geom_histogram(bins = 15, fill = "light blue") +
  ggtitle("Histogram of residuals")

GainCurvePlot(test_data, "prediction", "Life.expectancy", "Model2")

#test1
Danial <- data.frame(  Country = "Malaysia",
                     Alcohol = 5.28,
                     Diphtheria = 86,
                     BMI = 38.9,
                     Polio = 98,
                     Total.expenditure = 11.14,
                     thinness..1.19.years = 2.1,
                     Income.composition.of.resources = 0.741)
print(paste0("Life expectancy for Danial: ", round(predict(model2, Danial), 2)))

#test2
Dani <- data.frame(  Country = "Malaysia",
                     Alcohol = 6.28,
                     Diphtheria = 90,
                     BMI = 40.0,
                     Polio = 90,
                     Total.expenditure = 12.14,
                     thinness..1.19.years = 3.1,
                     Income.composition.of.resources = 0.741)
print(paste0("Life expectancy for Dani: ", round(predict(model2, Dani), 2)))

install.packages("plotly")
library(plotly)
life_expectancy_vs_Hepatitis_B <- ggplot(X_clean, aes(Hepatitis.B, Life.expectancy)) + 
  geom_jitter(color = "purple", alpha = 0.5) + theme_light()

life_expectancy_vs_Diphtheria  <- ggplot(X_clean, aes(Diphtheria, Life.expectancy)) +
  geom_jitter(color = "orange", alpha = 0.5) + theme_light()

life_expectancy_vs_Polio  <- ggplot(X_clean, aes(Polio, Life.expectancy)) + geom_jitter(color = "pink", alpha = 0.5) + theme_grey()

p <- plot_grid(life_expectancy_vs_Hepatitis_B, life_expectancy_vs_Diphtheria, life_expectancy_vs_Polio ) 
title <- ggdraw() + draw_label("Correlation between Immunizations and life expectancy", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
