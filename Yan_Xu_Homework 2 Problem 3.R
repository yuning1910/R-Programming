# Elimated the rows with NA

transactions <- transactions[complete.cases(transactions),]

# Tranform trans_dt to Date format
transactions$trans_dt <- as.Date(transactions$trans_dt,format = "%m/%d/%Y")

#format it to Text

transactions$trans_dt <- format(transactions$trans_dt,format = "%B %d %Y")

# Seperate trans_dt to Month, Date, and Year
transactions <- transactions %>% separate(col = trans_dt, into = c("Month","Date", "Year"), sep= " ")

# Convert Month column to factors

transactions$Month <- factor(transactions$Month, levels = month.name)

# Use Pipe Operator to calculate the Total Sales per month, and total purchase from CC
tabl3 <- transactions %>%
  group_by(Month)%>%
  summarise(Total_Sales_By_Month = sum(amount))
tabl4 <- transactions %>%
  group_by(Month)%>%
  filter(payment_method == "CC")%>%
  summarise(Total_Sales_CC = sum(amount))
# Combine those two table to form table with 3 column: Month, total sales by month
# total sales CC
tabl5 <-data.frame(tabl3, tabl4$Total_Sales_CC)

# build a table for sum of each product cat
Tba <- transactions%>%
       group_by(Month) %>%
       filter(prod_cat == "A")%>%
       summarise(SaleA = sum(amount))
Tbb <- transactions%>%
       group_by(Month) %>%
       filter(prod_cat == "B")%>%
       summarise(SaleB = sum(amount))
Tbc <- transactions%>%
       group_by(Month) %>%
       filter(prod_cat == "C")%>%
       summarise(SaleC = sum(amount))
Tbd <- transactions%>%
       group_by(Month) %>%
       filter(prod_cat == "D")%>%
      summarise(SaleD = sum(amount))

#Combine sales amount for each product type with tabl5


Table3 <- data.frame(Tba$SaleA, Tbb$SaleB, Tbc$SaleC, Tbd$SaleD )


#colnames(Table3) <- c("Month", "TS_Month", "TS_CC", "Sale_A","Sale_B", "Sale_C","Sale_D")


                 