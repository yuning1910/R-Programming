### LOAD LIBRARIES AND READ DATA ####

### install ggpubr
install.packages('ggpubr')

### Load Libraries
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(ggpubr)

### Read in data for MLS Dataframe
MLS <- read.csv("2016 MLS Players.csv") 
dim(MLS)

# Read the MLS Player csv file to MLS_Players2016 object
MLS_Players2016 <- read.csv("2016 MLS Players.csv")





### MANIPULATING DATA ###

### Remove POOL and No-Team Players
MLS <- filter(MLS, Club != "")
MLS <- filter(MLS, Club != "POOL")


#Cast MLS_Players2016 Minutes, Goals as numerics

MLS_Players2016$Minutes <- as.numeric(MLS_Players2016$Minutes)
MLS_Players2016$Goals <- as.numeric(MLS_Players2016$Goals)

### Filter Out "N/A"  players from MLS Dataframe 
MLS <- filter(MLS, MLS$Minutes != "#N/A")

### Cast MLS Dataframe Goals, Minutes as numerics

MLS$Minutes <- as.numeric(MLS$Minutes)
MLS$Goals <- as.numeric(MLS$Goals)


### Add per 90 to MLS Dataframe 

score_rate <- as.numeric(MLS$Goals) / as.numeric(MLS$Minutes)
goals_per_90 <- score_rate * 90
MLS <- cbind(MLS, goals_per_90)

### Group Hybrid Forwards into Fowards category in MLS Dataframe

MLS$Pos[MLS$Pos == 'M-F'] <- "F"
MLS$Pos[MLS$Pos == 'F-M'] <- "F"
MLS$Pos[MLS$Pos == 'F/M'] <- "F"
MLS$Pos[MLS$Pos == 'M/F'] <- "F"

### Group Hybrid Middies into Middies category in MLS Dataframe
MLS$Pos[MLS$Pos == 'D-M'] <- "M"
MLS$Pos[MLS$Pos == 'M-D'] <- "M"

### IN MLS_Players2016 Dataframe, 
### Create a new column for re group player to GK, D, M, F, Hybrid. Hybrid =
### players with M-F, "F-M", "D-M", "M-D", "M/F", "F/M"

MLS_Players2016$Pos <- as.character(MLS_Players2016$Pos)
Position <- MLS_Players2016$Pos
Position[Position == "M-F"] <- "Hybrid"
Position[Position == "F-M"] <- "Hybrid"
Position[Position == "D-M"] <- "Hybrid"
Position[Position == "M-D"] <- "Hybrid"
Position[Position == "M/F"] <- "Hybrid"
Position[Position == "F/M"] <- "Hybrid"

### Bind MLS_Players2016 Dataframe with new position category

New_position <- cbind(MLS_Players2016, Position)
New_position <- filter(New_position, New_position$Club != "")
New_position <- filter(New_position, New_position$Club != "POOL")


### Isolate FWDS from MLS Dataframe, Create FWDS Dataframe for later analysis
FWDS <- filter(MLS, MLS$Pos == "F")


### In FWDS, Filter Out Injured, convert goals and minutes to numeric
FWDS <- filter(FWDS, FWDS$Minutes != "#N/A")
FWDS$Minutes <- as.numeric(FWDS$Minutes)
FWDS$Goals <- as.numeric(FWDS$Goals)

### In FWDS, Filter out low minutes players
z_minutes <- scale(FWDS$Minutes)
FWDS <- cbind(FWDS, z_minutes)
FWDS <- filter(FWDS, FWDS$z_minutes > -1)

### In FWDS, create Salary in 1000s column  
salary_in_ks <- FWDS$Base.Salary / 1000
cbind(FWDS, salary_in_ks)

### In FWDS, Create Salary Cap Hit column 
cap_hit <- salary_in_ks
cap_hit[cap_hit > 400] <- 400
max(cap_hit)
cap_hit
FWDS <- cbind(FWDS, cap_hit)


### With MLS Dataframe, filter N/A values and create MLS2 Dataframe
MLS2 <- filter(MLS, MLS$Minutes != "#N/A")





### Using New Position Dataframe, Visualize Relationship Between Salary and Position ###

#Create a table show the percentage of each position against overall salary
table1 <- New_position %>%
  group_by(Position)%>%
  summarise(percent_salary = round(sum(Base.Salary)/sum(MLS_Players2016$Base.Salary),digits = 3)*100)%>%
  arrange(percent_salary)

#Piechart of Salary vs Position
SPPie <- ggpie(table1, "percent_salary",fill = "Position", color = "white", label = "percent_salary",
               main = "Percentage Salary vs Position Across All Clubs")
SPPie + theme(legend.position = "right", plot.title = element_text(hjust = 0.5, size = 18,color = "dark blue" ))              


### Analyze relationship between Forwards, Goals, and Salary ###

## summary df for salary, minutes, and goals by position
position_summary <- MLS2 %>%
  group_by(Club, Pos) %>%
  summarise(pos_salary = round(sum(Base.Salary),0),
            pos_minutes = sum(Minutes),
            pos_goals = sum(Goals))
colnames(position_summary) <- c("Club", "Position", "Salary", "Minutes", "Goals")


# summary for all forward (cor = 0.04)
forward_labels <- c("F", "M-F", "F-M", "M/F", "F/M")
all_forwards <- filter(MLS2, Pos %in% forward_labels)
cor(all_forwards$Base.Salary, all_forwards$Goals)

# summary for pure forward (cor = 0.05)
pure_forwards <- filter(MLS2, Pos == "F")
attach(pure_forwards)
cor(Base.Salary, Goals)
qplot(Base.Salary, Goals)
p <- ggplot(pure_forwards, aes(Base.Salary, Goals))
p + geom_point() + ggtitle(
  "Non-Hybrid Forward Goals Scored by Salary") +
  geom_smooth(method = "lm") + scale_x_continuous(labels = scales:: comma) + 
  xlab("Base Salary")


# pure_forwards by club (cor = 0.17)
pure_forwards_by_club <- pure_forwards %>%
  group_by(Club) %>%
  summarise(fwd_salary = round(sum(Base.Salary),0),
            fwd_mts = sum(Minutes),
            fwd_goals = sum(Goals)) %>%
  arrange(Club)
colnames(pure_forwards_by_club) <- c("Club", "Salaries", "Minutes", "Goals")
str(pure_forwards_by_club)
cor(pure_forwards_by_club$Salaries, pure_forwards_by_club$Goals)
attach(pure_forwards_by_club)
p <- ggplot(pure_forwards_by_club, aes(Salaries, Goals))
p + geom_point(aes(color = factor(Club))) + ggtitle(
  "Non-Hybrid Forward Goals Scored by Salary, by Club") +
  geom_smooth(method = "lm") + scale_x_continuous(labels = scales:: comma) + 
  xlab("Salary")


# all_forwards by club (cor = 0.13)
all_forwards_by_club <- all_forwards %>%
  group_by(Club) %>%
  summarise(allfwd_salary = sum(Base.Salary),
            allfwd_mts = sum(Minutes),
            allfwd_goals = sum(Goals)) %>%
  arrange(desc(allfwd_salary))
colnames(all_forwards_by_club) <- c("Club", "Salaries", "Minutes", "Goals")
str(all_forwards_by_club)
qplot(Salaries, Goals, data = all_forwards_by_club)
boxplot(all_forwards_by_club$Salaries, main = "Salary by Team")
cor(all_forwards_by_club$Salaries, all_forwards_by_club$Goals)


# percentage of salary allocated to forwards vs. goals scored
# total salary, goals, and minutes by club (cor - 0.16)
club_summary <- MLS2 %>%
  group_by(Club) %>%
  summarise(club_salary = round(sum(Base.Salary),0),
            club_minutes = sum(Minutes),
            club_goals = sum(Goals)) %>%
  arrange(Club)
club_summary <- filter(club_summary, club_summary$Club != "")
cor(club_summary$club_salary, club_summary$club_goals)
summary(club_summary$club_salary)
attach(club_summary)
p <- ggplot(club_summary, aes(club_salary, club_goals))
p + geom_point(aes(color = factor(Club))) + ggtitle(
  "Goals Scored by Team Salary") +
  geom_smooth(method = "lm") + scale_x_continuous(labels = scales:: comma) + 
  xlab("Club Salary") + ylab("Club Goals") + labs(color = "Club")


# add column for % of club salaries invested in forwards
forward_percentage <-
  cbind(
    club_summary,
    pure_forwards_by_club$Salaries,
    pure_forwards_by_club$Minutes,
    pure_forwards_by_club$Goals
  )
colnames(forward_percentage) <-
  c(
    "Club",
    "Total Salary",
    "Total Minutes",
    "Total Goals",
    "Fwd Salary",
    "Fwd Minutes",
    "Fwd Goals"
  )

# plot and correlate percentage of salary by club with 
# goals scored by forwards (cor = 0.35)
attach(forward_percentage)
forward_percentage <- mutate(forward_percentage, Fwd_Salary_PCT = 100*(`Fwd Salary` / `Total Salary`))
p <- ggplot(forward_percentage, aes(Fwd_Salary_PCT, `Fwd Goals`))
p + geom_point(aes(color = factor(Club))) + ggtitle(
  "Forwards' Percentage of Team Salary to Goals Scored by Forwards") +
  geom_smooth(method = "lm") + xlab("Forward Salary Percentage") + 
  labs(color = "Club") + ylab("Number of Goals by Forwards")

cor(forward_percentage$Fwd_Salary_PCT, `Fwd Goals`)









### USING FWDS DATAFRAME, CREATE GOSS AND ROSS MODELS ###



### Create Scatterplot of Goals Vs. Salary in 1000s
p <- ggplot(FWDS, aes(salary_in_ks, Goals))
p + geom_point() + ggtitle("Individual Forward Salaries vs. Goals") + 
  geom_smooth(method = 'lm') + xlab("Salary in 1000s")

### create expected goals model 
attach(FWDS)
exp_goals <- lm (Goals ~ salary_in_ks)

### Add column for Goals Over Similar Salary  (GOSS)
GOSS <- exp_goals$residuals
FWDS <- cbind(FWDS, GOSS)

### Create Score Rate model
attach(FWDS)
length(goals_per_90)
length(Base.Salary)
score_rate_model <- lm(FWDS$goals_per_90 ~ Base.Salary)

### Create Stat for Rate Over Similar Salary (ROSS)
ROSS <- score_rate_model$residuals
FWDS <- cbind(FWDS, ROSS)



### DISPLAYING GOSS AND ROSS ###




### Group teams, show GOSS, Goals and Salary 
GOSS_Table <- FWDS %>%
  group_by(Club) %>%
  summarise(Mean_Goss = 
              mean(GOSS, na.rm = TRUE),
            Mean_Ross = 
              mean(ROSS, na.rm = TRUE),
            Mean_Salary = 
              round(mean(Base.Salary, na.rm = TRUE)),
            Total_Salary = 
              sum(Base.Salary, na.rm = TRUE), 
            Total_Goals = 
              sum(Goals, na.rm = TRUE))


### Create Bar Plot of GOSS Values 
s <- ggplot(GOSS_Table, aes(Club, Mean_Goss))
s + geom_col(aes(fill = factor(Club))) + ggtitle("Mean Goss and Goals Scored by Team") +
  geom_text(aes(label = Total_Goals)) + ylab("Mean GOSS") + labs(fill = "Club")

### Create Plot for GOSS and Salary 
q <- ggplot(GOSS_Table, aes(Mean_Salary, Mean_Goss))
q + geom_point() + ggtitle("Mean GOSS vs. Mean Salary") + 
  geom_smooth(method = 'lm') 

### Create Bar Plot of ROSS Values 
t <- ggplot(GOSS_Table, aes(Club, Mean_Ross))
t + geom_col(aes(fill = factor(Club))) + ggtitle("Mean ROSS and Avg Forward Salary by Team") + 
  geom_text(aes(label = Mean_Salary)) + ylab("Mean ROSS") + labs(fill = "Club")

### Create Plot for ROSS and Salary 
u <- ggplot(GOSS_Table, aes(Mean_Salary, Mean_Ross))
u + geom_point() + ggtitle("Mean ROSS vs. Mean Salary") + 
  geom_smooth(method = 'lm')


### EXAMINE RELATIONSHIP BETWEEN MINUTES AND SALARY ###


### Create Table to show Average Cost Per Minute by Position

minutes_table <- MLS %>%
  group_by(Pos) %>%
  summarise(mean_minutes = mean(Minutes, na.rm = TRUE), 
            mean_salary = mean(Base.Salary, na.rm = TRUE), 
            mean_cost_per_minute = mean_salary / mean_minutes, 
            cost_per_goal = (sum(Base.Salary))/(sum(Goals)))

### Create Plot for Minutes vs. Salary 

v <- ggplot(MLS, aes(Base.Salary, Minutes))
v + geom_point(aes(color = factor(Pos))) + ggtitle("Base Salary vs. Minutes in MLS Players") + 
  geom_smooth(method = 'lm') + scale_x_continuous(labels = scales:: comma) + 
  xlab("Base Salary") + labs(color = "Position") + labs(caption = "Note: Hybrid players have been grouped with their forwardmost positions.")

### Create Model for Minutes vs. Salary 

MS_Model <- lm(Minutes ~ Base.Salary)
MS_Model




### EXTRA CAP HIT STUFF, NOT USED IN FINAL POWERPOINT ###

### Create Scatterplot of Goals Vs. Cap Hit
r <- ggplot(FWDS, aes(cap_hit, Goals))
r + geom_point() + ggtitle("Cap Hit  vs. Goals") + 
  geom_smooth(method = 'lm')





