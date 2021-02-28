#Gradeable Project - Retail Analysis with Walmart Data
#https://lms.simplilearn.com/courses/3813/PG-DS---Data-Science-with-R/syllabus

# library(plyr) #cleaning
library(dplyr) 
# library(e1071)
library(caret)
# library(mlbench)
# library(ggplot2)
# library(tidyverse)
# library(funModeling) 
# library(Hmisc)
library(car) #vif
# library(tinytex)
# library(anytime)
# library(MASS)
library(randomForest)


setwd("C:/OLD_LAPTOP/Gaurav/Purdue/4. Data Science with R/5. Gradable_Projects")
Walmart_data<- read.csv("C:/OLD_LAPTOP/Gaurav/Purdue/4. Data Science with R/5. Gradable_Projects/Walmart_Store_sales.csv", header=TRUE)

dim(Walmart_data)
names(Walmart_data)
str(Walmart_data)
# View(Walmart_data)

summary(Walmart_data)

#Covert variables:
library(lubridate)
Walmart_data$Date<- dmy(Walmart_data$Date)  #factor to Date
str(Walmart_data)

#Add New variables 
Walmart_data$days <- yday(Walmart_data$Date) - 1 # No. of Days from start of the year
Walmart_data$WeekDay<- wday(Walmart_data$Date, label = TRUE, abbr = FALSE) # Weekday (Only Friday's data available)
Walmart_data$Month<-format(Walmart_data$Date,"%m") #add Month
Walmart_data$Quarter<-quarters(as.Date(Walmart_data$Date)) #add Quarter
Walmart_data$Year<-format(Walmart_data$Date,"%Y") #add Year
Walmart_data$Year_Month<-format(Walmart_data$Date,"%Y-%m") #combine and add Year-Month
Walmart_data$Year_Quarter<-paste(Walmart_data$Year,"-", Walmart_data$Quarter) #combine and add Year-Quarter

# Add Holiday Events
Walmart_data$Event<- ifelse(Walmart_data$Date == "2010-02-12" | Walmart_data$Date == "2011-02-11" | Walmart_data$Date =="2012-02-10" | Walmart_data$Date == "2013-02-08", "SuperBowl", 
ifelse(Walmart_data$Date == "2010-09-10" | Walmart_data$Date == "2011-09-09" | Walmart_data$Date =="2012-09-07" | Walmart_data$Date == "2013-09-06", "LabourDay",
ifelse(Walmart_data$Date == "2010-11-26" | Walmart_data$Date == "2011-11-25" | Walmart_data$Date =="2012-11-23" | Walmart_data$Date == "2013-11-29", "Thanksgiving",
ifelse(Walmart_data$Date == "2010-12-31" | Walmart_data$Date == "2011-12-30" | Walmart_data$Date =="2012-12-28" | Walmart_data$Date == "2013-12-27", "Christmas",
"Non_Holiday"))))

# Add Half-year variable
Walmart_data$HalfYear<- ifelse(Walmart_data$Month >= "01" & Walmart_data$Month <= "06", "1st_HalfYear", "2nd_HalfYear")
# View(Walmart_data)


str(Walmart_data)

# View(Walmart_data)
# write.csv(Walmart_data, "Walmart_data_tranformed_in_R.csv")


# Task1 (part1) :Store with has maximum sales : 

# Step1: Summarise data by Store:

Store_Sales<- Walmart_data %>%
  select(Store, Weekly_Sales) %>%
  group_by(Store) %>%
  summarise(Total_Sales = sum(Weekly_Sales)) %>%
  arrange(desc(Total_Sales))

Store_Sales<- as.data.frame(Store_Sales)
# View(Store_Sales)
names(Store_Sales)

# Step2: Apply Max function:
Max_Sales_Store <- Store_Sales %>% 
  filter(Total_Sales == max(Total_Sales))

Max_Sales_Store #Store #20 with Sales :301397792


# Task1 (part2) :Store with has maximum Standard Deviation & coefficient of mean to standard deviation

library(magrittr)
library(operators)

# Step1: Summarise data by Store:
Weekly_Store_Sales<- Walmart_data %>%
  select(Store, Date, Weekly_Sales) %>%
  group_by(Store, Date) %>%
  summarise(Total_Sales = sum(Weekly_Sales)) %>%
  arrange(Store, Date)

Weekly_Store_Sales<- as.data.frame(Weekly_Store_Sales)
# View(Weekly_Store_Sales)
names(Weekly_Store_Sales)

# Step2: Find SD for each store:
SD_Store <- Weekly_Store_Sales %>%
  select(Store, Total_Sales) %>%
  group_by(Store) %>%
  summarise(Mean = mean(Total_Sales), SD = sd(Total_Sales), CV = Mean / SD) # CV= Coffecient of Variation

SD_Store<- as.data.frame(SD_Store)
SD_Store

# Step3: Idenfiy store with Max SD function:
Max_SD_Store <- SD_Store %>% 
  filter(SD == max(SD))

Max_SD_Store 

#Insight : Store #14 with Max SD : 317569.9. It has mean sales of 6.363884 and CV of 6.363884


#Task1 (part3) : Stores with good quarterly growth rate in Q3'2012:

Store_Qtryly_Sales<- Walmart_data %>%
  select(Store, Year_Quarter, Weekly_Sales) %>%
  group_by(Year_Quarter, Store) %>%
  summarise(Total_Sales = sum(Weekly_Sales)) %>%
  arrange(desc(Total_Sales))

# View(Store_Qtryly_Sales)

Q32012_Sales_Store<- Store_Qtryly_Sales %>% 
  select(Store, Year_Quarter, Total_Sales_Q32012=Total_Sales) %>%
  filter(Year_Quarter == "2012 - Q3") %>%
  arrange(Store)
Q32012_Sales_Store


Q32011_Sales_Store<- Store_Qtryly_Sales %>% 
  select(Store, Year_Quarter, Total_Sales_Q32011=Total_Sales) %>%
  filter(Year_Quarter == "2011 - Q3") %>%
  arrange(Store)
Q32011_Sales_Store

Combine_Sales_Store<- cbind(Q32012_Sales_Store, Q32011_Sales_Store)
# View(Combine_Sales_Store)

Combine_Sales_Store$Perc_Diff<- Combine_Sales_Store$Total_Sales_Q32012 / Combine_Sales_Store$Total_Sales_Q32011 - 1
# View(Combine_Sales_Store)

# Combine_Sales_Store_sorted <- Combine_Sales_Store[with(Combine_Sales_Store, order(- Combine_Sales_Store$Perc_Diff)), ]
# Combine_Sales_Store_sorted

Good_Q32012_Growth_Stores <- Combine_Sales_Store %>% 
  select (Store, Year_Quarter, Total_Sales_Q32012, Total_Sales_Q32011, Perc_Diff) %>%
  filter(Perc_Diff > 0) %>%
  arrange(desc(Perc_Diff))

Good_Q32012_Growth_Stores

# There are 6 stores with more than 0% Sales Growth rate  - Store #44, 38, 18, 39, 3 and 41


# Task1 (part4) : Holidays which have higher sales than the mean sales in non-holiday season for all stores together

# Filter 2012 data
All_Events_Sales<- Walmart_data %>%
  select(Event, Year, Weekly_Sales) %>%
  group_by(Event, Year, Weekly_Sales) %>%
  filter(Year == "2012")
All_Events_Sales
All_Events_Sales<- as.data.frame(All_Events_Sales)
All_Events_Sales

Event_wise_Sales<- All_Events_Sales %>%
  select(Event, Weekly_Sales) %>%
  group_by(Event) %>%
  summarise(Mean_Sales = mean(Weekly_Sales))
Event_wise_Sales
Event_wise_Sales<- as.data.frame(Event_wise_Sales)
Event_wise_Sales

#Plot Mean Sales by Events
Event_wise_Sales_Plot<- ggplot(Event_wise_Sales, aes(x=Event,y = Mean_Sales)) +
  geom_bar(stat = "identity", fill="blue")
Event_wise_Sales_Plot

#Insight : LabourDay and Superbowl have higher mean sales as compared to mean sales in Non-Holiday season.

# Key Events Trend Analysis

names(Walmart_data)
# Key Events (Christmas, Thanksgiving, SuperBowl, Labourday) Analysis for 3 years
KeyEvent_Sales<- Walmart_data %>%
  select(Year_Month, Month, Year, Event, Weekly_Sales) %>%
  group_by(Year_Month,Month,Year, Event) %>%
  summarise(Sales = sum(Weekly_Sales)) %>%
  arrange(Year_Month,Month, Event) %>%
  filter(Event != "Non_Holiday")

KeyEvent_Sales
KeyEvent_Sales<- as.data.frame(KeyEvent_Sales)
KeyEvent_Sales


ggplot(KeyEvent_Sales, aes(x = Year, y = Sales, fill= Event)) +
  geom_bar(position = position_stack(), stat="identity") +
  ggtitle("Holiday Events by Year") +
  coord_flip() +
  theme_minimal()


# Non-Holiday Event Trends for 3 years
Event_Monthly_Sales<- Walmart_data %>%
  select(Year_Month, Month, Year, Event, Weekly_Sales) %>%
  group_by(Year_Month,Month,Year, Event) %>%
  summarise(Monthly_Sales = sum(Weekly_Sales)) %>%
  arrange(Year_Month,Month, Event) %>%
  filter(Event == "Non_Holiday")

Event_Monthly_Sales
Event_Monthly_Sales<- as.data.frame(Event_Monthly_Sales)
Event_Monthly_Sales

ggplot(Event_Monthly_Sales, aes(x = Month, y = Monthly_Sales)) +
  geom_line(aes(color = Year, group=Year), size = 1) +
  scale_color_manual(values = c("blue", "black", "green")) +
  ggtitle("Non-Holiday Event - Sales Trend") +
  theme_minimal()

# Tak1 (part5): Provide a monthly and semester view of sales in units and give insights

# Monthly Sales Trend
Monthly_Sales<- Walmart_data %>%
  select(Month, HalfYear, Year, Weekly_Sales) %>%
  group_by(Month, HalfYear, Year) %>%
  summarise(Monthly_Sales = sum(Weekly_Sales))

Monthly_Sales
Monthly_Sales<- as.data.frame(Monthly_Sales)
# View(Monthly_Sales)


# Monthly Sales Trend Plot
Monthly_Sales %>%
  ggplot( aes(x=Month, y=Monthly_Sales, group=Year, color=Year)) +
  geom_line() +
  xlab("Month") + ylab("Sales") + # Set axis labels
  ggtitle("Monthly Sales Trend")


# Half-yearly Sales Trend

HY_Sales<- Walmart_data %>%
  select(HalfYear, Year, Weekly_Sales) %>%
  group_by(HalfYear, Year) %>%
  summarise(HYSales = sum(Weekly_Sales))

HY_Sales
HY_Sales<- as.data.frame(HY_Sales)
names(HY_Sales)

# Half-yearly Sales Trend Plot
HY_Sales %>%
  ggplot( aes(x=HalfYear, y=HYSales, group=Year, color=Year)) +
  geom_line() +
  xlab("Half-Year") + ylab("Sales") + # Set axis labels
  ggtitle("Half-yearly Sales Trend")


# Insights:
# December has highest sales in 2010 and 2011 and Jan has the lowest sales in 2011 and 2012
# Mar and May are the low Seasonal months
# Apr, Jul and Dec are high Seasonal months
# Sales increased from 1st to 2nd Half year in 2010 and 2011
# Sales declined from 1st to 2nd Half year in 2012 primarily due to missing Dec'12 sales data

# Store-Sales Range analysis:

# Sales bin - 2010
All_Store_Sales2010<- Walmart_data %>%
  select(Store, Weekly_Sales, Year) %>%
  group_by(Store, Year) %>%
  summarise(Total_Sales = sum(Weekly_Sales)) %>%
  filter(Year == 2010)

All_Store_Sales2010

# Sales bin - 2011
All_Store_Sales2011<- Walmart_data %>%
  select(Store, Weekly_Sales, Year) %>%
  group_by(Store, Year) %>%
  summarise(Total_Sales = sum(Weekly_Sales)) %>%
  filter(Year == 2011)

All_Store_Sales2011

# Sales bin - 2012
All_Store_Sales2012<- Walmart_data %>%
  select(Store, Weekly_Sales, Year) %>%
  group_by(Store, Year) %>%
  summarise(Total_Sales = sum(Weekly_Sales)) %>%
  filter(Year == 2012)

All_Store_Sales2012

# Plots - Sales bucket movement -2010 to 2012
par(mfrow=c(1,3))

All_Store_Sales2010_plot<- hist(All_Store_Sales2010$Total_Sales, breaks = 5, col = "blue", 
                                main="Stores by Sales bucket - 2010",
                                xlab="Total Sales-2010", ylab = "No. of Stores")
text(All_Store_Sales2010_plot$mids,All_Store_Sales2010_plot$counts,labels=All_Store_Sales2010_plot$counts, adj=c(0.5, -0.5))

All_Store_Sales2011_plot<- hist(All_Store_Sales2011$Total_Sales, breaks = 5, col = "brown", 
                                main="Stores by Sales bucket - 2011",
                                xlab="Total Sales-2011", ylab = "No. of Stores")
text(All_Store_Sales2011_plot$mids,All_Store_Sales2011_plot$counts,labels=All_Store_Sales2011_plot$counts, adj=c(0.5, -0.5))

All_Store_Sales2012_plot<- hist(All_Store_Sales2012$Total_Sales, breaks = 5, col = "dark green", 
                                main="Stores by Sales bucket - 2012",
                                xlab="Total Sales-2012", ylab = "No. of Stores")
text(All_Store_Sales2012_plot$mids,All_Store_Sales2012_plot$counts,labels=All_Store_Sales2012_plot$counts, adj=c(0.5, -0.5))

dev.off()

# Insights:
# Most of the stores has sales between 2-6mn. units in all 3 years.
# In 2012 there are more no. of stores (~15) with 4-6mn. sales units as compared to previous years


# Task2 (part1) For Store 1 - Build  prediction models to forecast demand

# Step1: Subset Store 1 Sales data:

Store_1_Sales<- Walmart_data %>%
  select(Store, Date, Weekly_Sales, Holiday_Flag, Temperature, Fuel_Price, CPI, Unemployment, days, WeekDay, Month, Quarter, Year, Year_Month, Year_Quarter, Event, HalfYear) %>%
  group_by(Store, Date, Holiday_Flag, Temperature, Fuel_Price, CPI, Unemployment, days, WeekDay, Month, Quarter, Year, Year_Month, Year_Quarter, Event, HalfYear) %>%
  summarise(Total_Sales = sum(Weekly_Sales)) %>%
  filter(Store == 1)

Store_1_Sales<- as.data.frame(Store_1_Sales)
# View(Store_1_Sales)
# names(Store_1_Sales)
# str(Store_1_Sales)
# dim(Store_1_Sales)

Store_1_model_data<- Store_1_Sales[-c(1,9,11:14,16)]
# View(Store_1_model_data)
str(Store_1_model_data)
names(Store_1_model_data)

# Restructure Dates - Assign sequential numbers/ ID to Date:
Store_1_model_data$DateNum<- row_number(Store_1_model_data$Date)
str(Store_1_model_data)

# EDA 
library(corrplot)
Store_1_cor_data<- Store_1_model_data[-c(1,8,9)] #Data with numerical variables 
str(Store_1_cor_data)
cor = cor(Store_1_cor_data)
corrplot(cor, method="number", type = "upper", order = "hclust", 
         tl.col = "black")

library(PerformanceAnalytics)
chart.Correlation(Store_1_cor_data)

# Insights :
# 1. Fuel Price and CPI has high Positive correlation +0.76
# 2. CPI and Uemployement has high Negative correlation -0.81
# 3. DateNum and Uemployement has high Negative correlation -0.79


# Split data: Train-Test:
## set the seed to make your partition reproducible
# set.seed(123)
# dt = sort(sample(nrow(Store_1_model_data), nrow(Store_1_model_data)*.7))
# train<-Store_1_model_data[dt,]
# test<-Store_1_model_data[-dt,]
# 
# train$Event2<- as.factor(train$Event)
# test$Event2<- as.factor(test$Event)
# 
# dim(train)
# str(train)
# dim(test)
# str(test)

str(Store_1_model_data)

Store1_M1<- lm(formula= Total_Sales ~., data=Store_1_model_data)
summary(Store1_M1)

Store1_M2<-lm(Total_Sales ~ Date + days + Holiday_Flag + Month + CPI + Event + Fuel_Price + Unemployment, Store_1_model_data)
summary(Store1_M2)

Store1_M3<-lm(Total_Sales ~ DateNum + days + Holiday_Flag + Month + CPI + Event, Store_1_model_data)
summary(Store1_M3)
# DateNum is not a significant variable as it is an sequential number.

Store1_M5<-lm(Total_Sales ~ days + Holiday_Flag + Month + CPI + Event, Store_1_model_data)
summary(Store1_M5)

Compare_Models<- anova(Store1_M1, Store1_M2, Store1_M3, Store1_M5) # ANOVA - F stats
Compare_Models

predict_Store1<- predict(Store1_M5, Store_1_model_data)
predict_Store1

combined_Store1<- cbind(Store_1_model_data, Predicted_Sales = predict_Store1)
combined_Store1
str(combined_Store1)

# Significant Variables:
library(randomForest)
Imp_variables <-varImp(Store1_M5, scale = FALSE)
Imp_variables

# Insights:
# Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
# CPI has impact on Sales - with 1 unit increase in CPI, Sales would increase by 10.7k units
# Unemployment and Fuel price do not have an impact on Sales

# Model Performance Metrics:
RMSE<-RMSE(predict_Store1, Store_1_model_data$Total_Sales)
RMSE
MAE<- MAE(predict_Store1, Store_1_model_data$Total_Sales)
MAE
MAPE<- mean(abs(predict_Store1/Store_1_model_data$Total_Sales- 1))
MAPE

Accuracy<- 1-MAPE
Accuracy 

# Forecast Sales : TIme Series Model
fore_data <- ts(Walmart_data$Weekly_Sales, start=2010, end=2012,frequency=52)
plot(fore_data)

# Holt Winters - Exponential Smoothing
library(forecast)
hw <- HoltWinters(fore_data, gamma = TRUE, beta = FALSE)
plot(hw, lty.predicted=2)

#Forecast
pred_hw<- predict(hw, 26, prediction.interval = TRUE, level = 0.95)

plot(hw, pred_hw, intervals = TRUE, col = 1, lty = 1,lty.predicted=2, lty.intervals = 7,
     xlab="Week", ylab = "Sales - Actual / Forecast",
     main = "Holt-Winters Exponential Smoothing")

# Model Interpretation:
# Model shows actual line (black) and predicted line (red dotted) for historical period (2010-2012) and forecast for next 6 months


# All Stores Prediction:

# All_Stores_Sales<- Walmart_data %>%
#   select(Store, Date, Weekly_Sales, Holiday_Flag, Temperature, Fuel_Price, CPI, Unemployment, days, WeekDay, Month, Quarter, Year, Year_Month, Year_Quarter, Event, HalfYear) %>%
#   group_by(Store, Date, Holiday_Flag, Temperature, Fuel_Price, CPI, Unemployment, days, WeekDay, Month, Quarter, Year, Year_Month, Year_Quarter, Event, HalfYear) %>%
#   summarise(Total_Sales = sum(Weekly_Sales))
# 
# All_Stores_Sales<- as.data.frame(All_Stores_Sales)
# # View(Store_1_Sales)
# names(All_Stores_Sales)
# str(All_Stores_Sales)
# 
# All_Stores_model_data<- All_Stores_Sales[-c(1,9,11:14,16)]
# str(All_Stores_model_data)
# # dim(All_Stores_model_data)
# 
# All_Stores_M1<- lm(formula= Total_Sales ~., data=All_Stores_model_data)
# summary(All_Stores_M1)
# # vif(All_Store_M1)
# 
# All_Stores_M3<-lm(Total_Sales ~ Holiday_Flag + Month + CPI + Fuel_Price + Unemployment+ Temperature + Event, All_Stores_model_data)
# summary(All_Stores_M3)
# 

# library(corrplot)
# All_Stores_cor_data<- All_Stores_model_data[-c(6,7)] #Data with numerical variables 
# str(All_Stores_cor_data)
# cor = cor(All_Stores_cor_data)
# corrplot(cor, method="number", type = "upper", order = "hclust", 
#          tl.col = "black")

# There seems to be no correlation between variables



