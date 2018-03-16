##1.	Show a table of average sales by month. (2 points)
##2.	Show a table of average sales by year. (2 points)
##3.	Show a table that lists all years, the month that contained the maximum sales amount for the 
##    entire year, the corresponding amount. (2 points)
##4.	Assume the following quarters: Q1 (Jan, Feb, Mar), Q2 (April, May, June), 
## Q3 (July, Aug, Sep), and Q4 (Oct, Nov, Dec).). Based on this data, 
## a bar chart of average sales by quarter. (4 points)


#load all related packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(xlsx)

#Read the seasonal_data to RStudio
Seasonal <- read.csv("seasonal_data.csv")

#Show the structure of the dataset
str(Seasonal)

#check whether there is any NA

is.na(Seasonal)

#Convert mth column to Date format

Seasonal$mth <- as.Date(Seasonal$mth, format = "%m/%d/%Y")

#format it to text

Seasonal$mth <- format(Seasonal$mth, format = "%B %d %Y")

#Seperate mth column into three column with month, day and year in a new variable 
# Seasonal_sep
Seasonal_sep <- Seasonal %>% separate(col = mth, into = c("Month", "Date", "Year"))

# Convert Month column to factor

Seasonal_sep$Month <- factor(Seasonal_sep$Month, levels = month.name, ordered = TRUE)

# Question 1: Create a table that shows the average sales by month. 
Finaltable1 <- Seasonal_sep %>%
              group_by(Month)%>%       # Group by month
               summarise(average_sales_month = mean(sales))%>% #calculate average sales
               arrange(Month) #arrange average sales by month in acsending order

#Show Finaltable1
View(Finaltable1)

# Question 2: Create a table that shows the average sales by year

Finaltable2 <- Seasonal_sep%>%
               group_by(Year)%>%
               summarise(average_sales_year = mean(sales)) %>%
               arrange(Year)

#Show Finaltable2
View(Finaltable2)

#Question 3: Create a table list all year, with month that contained the highest sales

#Create a copy of Seasonal_sep to still have original copy
Seasonal_sep2 <- Seasonal_sep
#Find max sales for 2004, return month, year, sales
Year_2004 <- subset(Seasonal_sep2, Year == 2004)
Max_month_2004 <- Year_2004[which.max(Year_2004$sales), c(1,3,4)]
#Find max sales for 2005, return month, year, sales
Year_2005 <- subset(Seasonal_sep2, Year == 2005)
Max_month_2005 <- Year_2005[which.max(Year_2005$sales), c(1,3,4)]
#Find max sales for 2006, return month, year, sales
Year_2006 <- subset(Seasonal_sep2, Year == 2006)
Max_month_2006 <- Year_2006[which.max(Year_2006$sales), c(1,3,4)]
#Find max sales for 2007, return month, year, sales
Year_2007 <- subset(Seasonal_sep2, Year == 2007)
Max_month_2007 <- Year_2007[which.max(Year_2007$sales), c(1,3,4)]
#Find max sales for 2008, return month, year, sales
Year_2008 <- subset(Seasonal_sep2, Year == 2008)
Max_month_2008 <- Year_2008[which.max(Year_2008$sales), c(1,3,4)]
#Find max sales for 2009, return month, year, sales
Year_2009 <- subset(Seasonal_sep2, Year == 2009)
Max_month_2009 <- Year_2009[which.max(Year_2009$sales), c(1,3,4)]
#Find max sales for 2010, return month, year, sales
Year_2010 <- subset(Seasonal_sep2, Year == 2010)
Max_month_2010 <- Year_2010[which.max(Year_2010$sales), c(1,3,4)]
#Find max sales for 2011, return month, year, sales
Year_2011 <- subset(Seasonal_sep2, Year == 2011)
Max_month_2011 <- Year_2011[which.max(Year_2011$sales), c(1,3,4)]
#Find max sales for 2012, return month, year, sales
Year_2012 <- subset(Seasonal_sep2, Year == 2012)
Max_month_2012 <- Year_2012[which.max(Year_2012$sales), c(1,3,4)]
#Find max sales for 2013, return month, year, sales
Year_2013 <- subset(Seasonal_sep2, Year == 2013)
Max_month_2013 <- Year_2013[which.max(Year_2013$sales), c(1,3,4)]
#Find max sales for 2014, return month, year, sales
Year_2014 <- subset(Seasonal_sep2, Year == 2014)
Max_month_2014 <- Year_2014[which.max(Year_2014$sales), c(1,3,4)]
#Find max sales for 2015, return month, year, sales
Year_2015 <- subset(Seasonal_sep2, Year == 2015)
Max_month_2015 <- Year_2015[which.max(Year_2015$sales), c(1,3,4)]

#Create the table

Finaltable3 <-rbind(Max_month_2004,Max_month_2005, Max_month_2006, Max_month_2007,
                    Max_month_2008, Max_month_2009, Max_month_2010, Max_month_2011,
                    Max_month_2012, Max_month_2013, Max_month_2014, Max_month_2015)
#Show the table
View(Finaltable3)

#Question 4: 

#Create an empty vector Quarters to store which quarter does the month belong to
Quarters <- vector(mode = "character", length = nrow(Seasonal_sep))

#Use a for loop to assign each month to its corresponding Quarter
for(i in 1:nrow(Seasonal_sep)){
  Quarters[i] <- if(Seasonal_sep$Month[i] == "January" || Seasonal_sep$Month[i] == "February" || Seasonal_sep$Month[i] == "March" ){
    "Q1"
  }else if(Seasonal_sep$Month[i] == "April" || Seasonal_sep$Month[i] == "May" || Seasonal_sep$Month[i] == "June" ){
    "Q2"
  }else if(Seasonal_sep$Month[i] == "July" || Seasonal_sep$Month[i] == "August" || Seasonal_sep$Month[i] == "September" ){
    "Q3"
  }else {
    "Q4"
  }
}

# Add Quarters to Seasonal_sep 

Seasonal_sep1 <-cbind(Seasonal_sep, Quarters)

# Create a table shows the average sales by Quarter

Finaltable4 <- Seasonal_sep1%>%
               group_by(Quarters)%>%
               summarise(average_sales_quarter = mean(sales))

#Show Finaltable4
View(Finaltable4)

# Create a Barchat to show Sales by Quarter

plot1 <- ggplot(Finaltable4, aes(Quarters, average_sales_quarter, fill = Quarters))
plot1 + geom_bar(stat = "identity")+ggtitle("Average Sales By Quarter") + ylab("Average Sales") + 
  theme(plot.title = element_text(hjust = 0.5, size = 18,color = "dark blue" ))+
  scale_y_continuous(labels = scales::dollar)

# Export tables to Excel
write.xlsx(Finaltable1, file = "MSA 8105 Final Exam Problem 1.xlsx", sheetName="Question 1")
write.xlsx(Finaltable2, file = "MSA 8105 Final Exam Problem 1.xlsx", sheetName="Question 2", append = TRUE)
write.xlsx(Finaltable3, file = "MSA 8105 Final Exam Problem 1.xlsx", sheetName="Question 3", append = TRUE)
write.xlsx(Finaltable4, file = "MSA 8105 Final Exam Problem 1.xlsx", sheetName="Question 4", append = TRUE)