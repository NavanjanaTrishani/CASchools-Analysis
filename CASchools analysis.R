## Structure of the data set ##
str(CASchools)


##Rename the data set##
data(CASchools)
cas <- CASchools

##First few rows of the data set##
head(cas)

## Remove the first column and get a new data set##
cas1 = subset(cas, select = -c(1) )
cas1


##last few rows of the data set##
tail(cas1)

##Variables in the data set##
names(cas1)

##summary about cas1##
summary(cas1)

##install packages##

install.packages("psych")
install.packages("Hmisc")
install.packages("relaimpo")

library(ggplot2)
library(psych)
library(Hmisc)

##for more extensive summary statistics##
Hmisc::describe(cas1)

# compute STR and append it to cas1
cas1$STR <- cas1$students/cas1$teachers 

# compute TestScore and append it to cas1
cas1$score <- (cas1$read + cas1$math)/2     

cas1

head(cas1)

# compute sample averages of STR and score
avg_STR <- mean(cas1$STR) 
avg_score <- mean(cas1$score)

# compute sample standard deviations of STR and score
sd_STR <- sd(cas1$STR) 
sd_score <- sd(cas1$score)

# set up a vector of percentiles and compute the quantiles 
quantiles <- c(0.10, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9)
quant_STR <- quantile(cas1$STR, quantiles)
quant_score <- quantile(cas1$score, quantiles)

# gather everything in a data.frame 
cas1_summary <- data.frame(Average = c(avg_STR, avg_score), 
                                  StandardDeviation = c(sd_STR, sd_score), 
                                  quantile = rbind(quant_STR, quant_score))

# print the summary to the console
cas1_summary


##scatter plot for STR~score##
plot(score ~ STR, 
     data = cas1,
     main = "Scatterplot of TestScore and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)")

##corellation between str and score##
cor(cas1$STR, cas1$score)

##relationship between math and reading scores##
ggplot(cas1, aes(read, math)) + geom_point() + geom_smooth()
library(ggplot2)
 ##correlation between math and reading scores##
cor(cas1$math, cas1$read)


data("CASchools")

cas <- CASchools
cas1 = subset(cas, select = -c(1) )
cas1$STR <- cas1$students/cas1$teachers
cas1$score <- (cas1$read + cas1$math)/2 

# prepare the data
library(AER)                                                     
data(cas1)
cas1$STR <- cas1$students/cas1$teachers
cas1$score <- (cas1$read + cas1$math) / 2       

cor(cas1$income, cas1$score)

# fit a simple linear model
linear_model<- lm(score ~ income, data = cas1)

# plot the observations
plot(cas1$income, cas1$score,
     col = "steelblue",
     pch = 20,
     xlab = "income of the districts (thousands of dollars)", 
     ylab = "Test Score",
     cex.main = 0.9,
     main = "Test Score vs. District Income ")

# add the regression line to the plot
abline(linear_model, 
       col = "red", 
       lwd = 2)

# fit the quadratic Model
score_quadratic_model <- lm(score ~ income + I(income^2), data = cas1)

# obtain the model summary
coeftest(score_quadratic_model, vcov. = vcovHC, type = "HC1")



# draw a scatterplot of the observations for income and test score
plot(cas1$income, cas1$score,
     col  = "steelblue",
     pch = 20,
     xlab = "District Income (thousands of dollars)",
     ylab = "Test Score",
     main = "Estimated Linear and Quadratic Regression Functions")

# add a linear function to the plot
abline(linear_model, col = "black", lwd = 2)

# add quatratic function to the plot
order_id <- order(cas1$income)

lines(x = cas1$income[order_id], 
      y = fitted(quadratic_model)[order_id],
      col = "red", 
      lwd = 2) 










































