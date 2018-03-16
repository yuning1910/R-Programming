## 1.	Recode Education column from numeric to character where 1 represents "Undergrad,"
##    2 represents "Graduate," and 3 represents "Advanced." (2 points)
## 2.	Make Education an ordered factor variable. (2 points)
## 3.	Provide a bar chart of average income by level of education. (4 points)
## 4.	Create a scatter plot of average spending on credit cards (CCAvg) vs. income.
##    CCavg should be the y-variable while income should be the x-variable.
##    Use different point colors to represent the varying levels of education. 
##    (4 points)
## 5.	Create 3 linear regression models of CCavg (y) vs. Income (x)
##    for each education level. Show the regression coefficients of each model. 
##    (4 points)
## 6.	What was the customer ID and corresponding residual value of the customer 
##    that had the largest deviation (positive or negative)) from the predicted value?
##    (2 points)
## 7.	How many customers had a predicted value within 10% (plus or minus) 
##    from their actual value? (2 points)

# Load all related package
library(dplyr)
library(ggplot2)
library(tidyr)
library(xlsx)

#Read the bank_customers data to RStudio
Bank_customers <- read.csv("bank_customers.csv")

#Show the structure of the dataset
str(Bank_customers)

#check whether there is NA value
sum(is.na(Bank_customers))

#Question 1: recode Education column to character with Undergrad, Graduate, Advanced

# Create a new vector that takes value from Education column of Bank customer data
Education_M <- Bank_customers$Education

# Replace 1 with Undergrad, 2 with Graduate, 3 with Advanced
Education_M[Education_M == 1] <- "Undergrad"
Education_M[Education_M == 2] <- "Graduate"
Education_M[Education_M == 3] <- "Advanced"

# Combine Education_M with Bank_customers and assign to a new data.frame
# Bank_customer_New
Bank_customers_new <- cbind(Bank_customers, Education_M)

# Subsetting Bank_customers_new to exclude the original Education column
Bank_customers_new_one <- Bank_customers_new[, c(1,2,3,4,5,6,7,9)]

# Modify the colname of column 9 in Bank_customers_new_one to Edcuation
colnames(Bank_customers_new_one)[8] <- "Education"

# Show the structure of Bank_customers_new_one
str(Bank_customers_new_one)

# View Bank_customers_new_one
View(Bank_customers_new_one)

# Question 2: Make Education column an ordered factor

Bank_customers_new_one$Education <- factor(Bank_customers_new_one$Education, 
                                           levels = c("Undergrad", "Graduate","Advanced"),
                                           ordered = TRUE)

# Check the levels of Education use the first 15 row 
head(Bank_customers_new_one$Education, 15)

# Question 3: Create a bar chart of average income by level of education

# Create a table with two column, 1st column is the education level in order
# 2nd column is the average income
P2Table1 <- Bank_customers_new_one %>%
             group_by(Education)%>%      # group by education level           
             summarise(average_income_education = mean(Income)*1000) # average income

#Show P2Table1
View(P2Table1)

#Create a bar chart for table P2Table1
plot2 <- ggplot(P2Table1, aes(Education, average_income_education, fill = Education))
plot2 + geom_bar(stat = "identity")+ggtitle("Average Income By Education Level") + ylab("Average Income") + 
  theme(plot.title = element_text(hjust = 0.5, size = 18,color = "dark blue" ))+
  scale_y_continuous(labels = scales::dollar)

# Question 4: Create a scatter plot of average spending vs income
plot3 <- ggplot(Bank_customers_new_one, aes(Income*1000, CCAvg*1000, color = Education))
plot3 + geom_point()+ ggtitle("Average Credit Card Spending vs.Income")+               
  theme(plot.title = element_text(hjust=0.5, size = 18, color = "dark blue")) + 
  scale_y_continuous(labels = scales::dollar)+
  scale_x_continuous(labels = scales::dollar)+ ylab("Average Credit Card Spending") +
  xlab("Income")

# Question 5: Create three linear regression model for CCAvg vs.Income for each
# education level

# Create a table that isolated Education level = Undergrad
P2Table2 <- Bank_customers_new_one %>% filter(Education == "Undergrad")

# Convert Income and CCAvg in Thousands
P2Table2$Income <- P2Table2$Income*1000
P2Table2$CCAvg <- P2Table2$CCAvg*1000

# Create a linear regression model for Education level = Undergrad
P2model1 <- lm(P2Table2$CCAvg ~ P2Table2$Income)

#Show a Summary of this model
summary(P2model1)

# Show the regression coef 
coef(P2model1)

# Create a table that isolated Education level = Graduate
P2Table3 <- Bank_customers_new_one %>% filter(Education == "Graduate")

# Convert Income and CCAvg in Thousands
P2Table3$Income <- P2Table3$Income*1000
P2Table3$CCAvg <- P2Table3$CCAvg*1000

# Create a linear regression model for Education level = Graduate
P2model2 <- lm(P2Table3$CCAvg ~ P2Table3$Income)

# Show a summary of this model
summary(P2model2)

# Show the regression coef 
coef(P2model2)

# Create a table that isolated Education level = Advanced
P2Table4 <- Bank_customers_new_one %>% filter(Education == "Advanced")

# Convert Income and CCAvg in Thousands
P2Table4$Income <- P2Table4$Income*1000
P2Table4$CCAvg <- P2Table4$CCAvg*1000


# Create a linear regression model for Education level = Advanced
P2model3 <- lm(P2Table4$CCAvg ~ P2Table4$Income)

# Show a summary of this model
summary(P2model3)

# Show the regression coef 
coef(P2model3)

# Question 6: Find customer ID and corresponding residual value that has the
# largest deviation from predicted value

# Create residual value and predicted value for P2model1 for education level =
# Undergrad

residual_Undergrad <- residuals(P2model1)
predict_Undergrad <- predict(P2model1)

# Add residual and predict value to P2Table2
P2Table2_new <- cbind(P2Table2, residual_Undergrad, predict_Undergrad)

# View P2Table2_new
View(P2Table2_new)

# Find the maximum absolute value of residual
Max_abs_undergrad<- max(abs(P2Table2_new$residual_Undergrad))

# Find Row with Max_abs value
Undergrad_largest_deviation <- P2Table2_new %>% filter(abs(residual_Undergrad) == Max_abs_undergrad)

# Show only ID and Corresponding residual vaule
Undergrad_largest_deviation[c(1,9)]

# Create residual value and predicted value for P2model2 for education level =
# Graduate

residual_Graduate <- residuals(P2model2)
predict_Graduate <- predict(P2model2)

# Add residual and predict value to P2Table3
P2Table3_new <- cbind(P2Table3, residual_Graduate, predict_Graduate)

# View P2Table3_new
View(P2Table3_new)

# Find the maximum absolute value of residual
Max_abs_Graduate<- max(abs(P2Table3_new$residual_Graduate))

# Find Row with Max_abs value
Graduate_largest_deviation <- P2Table3_new %>% filter(abs(residual_Graduate) == Max_abs_Graduate)

# Show only ID and Corresponding residual vaule
Graduate_largest_deviation[c(1,9)]

# Create residual value and predicted value for P2model2 for education level =
# Advanced

residual_Advanced <- residuals(P2model3)
predict_Advanced <- predict(P2model3)

# Add residual and predict value to P2Table4
P2Table4_new <- cbind(P2Table4, residual_Advanced, predict_Advanced)

# View P2Table4_new
View(P2Table4_new)

# Find the maximum absolute value of residual
Max_abs_Advanced<- max(abs(P2Table4_new$residual_Advanced))

# Find Row with Max_abs value
Advanced_largest_deviation <- P2Table4_new %>% filter(abs(residual_Advanced) == Max_abs_Advanced)

# Show only ID and Corresponding residual vaule
Advanced_largest_deviation[c(1,9)]

# Question 7: How many customers has their predicted value within 10% +/- 
# from their actual value

# Calculate the percentage of each customer's predicted value from their
# actual value - Undergraduate
Percent_value_Undergraduate <-(abs(P2Table2_new$residual_Undergrad)/P2Table2_new$CCAvg)*100

# Select the customer with percentage less or equal than 10
PVU_Ten <- Percent_value_Undergraduate[Percent_value_Undergraduate <= 10]

# Calculate how many are in that group
length(PVU_Ten)

# Calculate the percentage of each customer's predicted value from their
# actual value - Graduate
Percent_value_Graduate <-(abs(P2Table3_new$residual_Graduate)/P2Table3_new$CCAvg)*100

# Select the customer with percentage less or equal than 10
PVG_Ten <- Percent_value_Graduate[Percent_value_Graduate <= 10]

# Calculate how many are in that group
length(PVG_Ten)

# Calculate the percentage of each customer's predicted value from their
# actual value - Advanced
Percent_value_Advanced <-(abs(P2Table4_new$residual_Advanced)/P2Table4_new$CCAvg)*100

# Select the customer with percentage less or equal than 10
PVA_Ten <- Percent_value_Advanced[Percent_value_Advanced <= 10]

# Calculate how many are in that group
length(PVA_Ten)

# Export tables to Excel
write.xlsx(Bank_customers_new_one, file = "MSA 8105 Final Exam Problem 2.xlsx", sheetName="Question 1")
write.xlsx(P2Table1, file = "MSA 8105 Final Exam Problem 2.xlsx", sheetName="Question 3", append = TRUE)
