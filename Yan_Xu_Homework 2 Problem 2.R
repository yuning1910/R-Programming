## For Table 2, you should group transactions as "low," "medium," and "high", 
## based on the amount spent per transaction. based on the amount spent per 
## transaction.An amount that is greater than or equal to 1000 should be
## classified as "high," between 100 and 999 (inclusive) should be classified 
## as "medium," and anything less than 100 should be classified as "low." Name this 
## variable as transaction_level. Table 2 should be grouped by transaction_level 
## and contain a column that shows the % of credit card (CC) type transactions and
## the % of transactions that were of product category A, the % of transactions
## that were of product category B, the % of transactions that were of product 
## category C, and the % of transactions that were of product category D.
## This table will have 6 columns and 4 rows

# Create a vector named transaction_level
transaction_level <- vector(mode = "character", length = nrow(transactions))

# Use a for loop, included a if else loop to define the three categories

for(i in 1: nrow(transactions)){
  transaction_level[i] <- if(transactions$amount[i] >= 1000){
    "high"
  } else if(transactions$amount[i] >= 100 && transactions$amount[i] <= 999){
      "medium"
  } else {
      "low"
    }
}

#Check whether for loop works
sum_high <- sum(transaction_level == "high")
sum_medium <-sum(transaction_level == "medium")
sum_low <- sum(transaction_level == "low")

# Add transaction_level to transactions
transactions <- cbind(transactions, transaction_level)


transactions$payment_method <- as.character(transactions$payment_method)

# Find out the number of transactions with CC, prod_a, prod_b, prod_c, prod_d

Table2<- transactions %>%
   group_by(transaction_level)%>%
   summarise(Percent_CC = round((sum(payment_method == "CC")/n())*100, 2),
             Percent_A = round((sum(prod_cat == "A")/n())*100, 2),
             Percent_B = round((sum(prod_cat == "B")/n())*100, 2),
             Percent_C = round((sum(prod_cat == "C")/n())*100, 2),
             Percent_D  = round((sum(prod_cat == "D")/n())*100 ,2))
# Show Table2
Table2
