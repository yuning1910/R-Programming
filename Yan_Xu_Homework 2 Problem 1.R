## Table 1 should be grouped by Product Category (prod_cat) and contain information 
## on the total amount spent per year and the average amount spent per transaction. 
## This table will have 3 columns and 5 rows.

#Read transition.csv file 
transactions <- read.csv("Transactions.csv")
#use summarize and pipe operator to perform the sum, average function, store to table 1
table1 <- transactions %>%
  group_by(prod_cat)%>%                     # Group by Product Category 
  summarise(Total_Sales = sum(amount, na.rm = TRUE),      # calculate the sum for each category
            Average_Sales = mean(amount, na.rm = TRUE))   # calculate the average for each category
# show table1
table1