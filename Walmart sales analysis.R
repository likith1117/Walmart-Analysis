getwd()
data1 <- read.csv("D:/R Project/R Project/archive (4)/Walmart.csv")
library("dplyr") 
library("ggplot2") 
library("scales")
library("zoo")
library("tidyverse")
library("tidyr")
library("lubridate")
require(stats)
head(data1)
dim(data1)
class(data1)
str(data1)
summary(data1)
table(data1$Store)
table(data1$Holiday_Flag)
colSums(is.na(data1))
all(duplicated(data1) == TRUE)
Store_Sales<- aggregate(Weekly_Sales ~ Store, data = data1, sum)
colnames(Store_Sales)[2] <- "Total_Sales_by_Store"
Store_Sales <-arrange(Store_Sales, desc(Total_Sales_by_Store)) 
Store_Sales[1,]
print(paste('Store no.', Store_Sales[1,]$Store,
            'has the maximum sales and the value is = ', Store_Sales[1,]$Total_Sales_by_Store))
Store_Sales$Store <- as.character(Store_Sales$Store)
Store_Sales$Store <- factor(Store_Sales$Store, levels=unique(Store_Sales$Store))
options(repr.plot.width = 14, repr.plot.height = 8)

a<-ggplot(data=Store_Sales, aes(x=Store, y=Total_Sales_by_Store)) + geom_bar(stat="identity",fill="steelblue") +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=0.5))+ scale_x_discrete(breaks = data1$Store)+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+ ggtitle('Store vs Sales')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Stores") + ylab("Total Sales")
a

Store_Sales_Variation<-summarise(group_by(data1,Store),sd(Weekly_Sales), mean(Weekly_Sales))

colnames(Store_Sales_Variation)[2] <- "StandardDeviation_Sales_by_Store"
colnames(Store_Sales_Variation)[3] <- "Mean_Sales_by_Store"
Store_Sales_Variation<- mutate(Store_Sales_Variation,CV_Sales_by_Store = (StandardDeviation_Sales_by_Store/Mean_Sales_by_Store)*100)
Store_Sales_Variation[which.max(Store_Sales_Variation$StandardDeviation_Sales_by_Store), ]


store_sales_max_std <- Store_Sales_Variation[which.max(Store_Sales_Variation$StandardDeviation_Sales_by_Store), ]$Store

max_sd <- Store_Sales_Variation[which.max(Store_Sales_Variation$StandardDeviation_Sales_by_Store), ]$StandardDeviation_Sales_by_Store

CV_max_sd <- Store_Sales_Variation[which.max(Store_Sales_Variation$StandardDeviation_Sales_by_Store), ]$CV_Sales_by_Store


print(paste('Store no. ', store_sales_max_std,
            'has the maximum standard deviation of ', max_sd, 'Coefficient of Variation = ',CV_max_sd ))
Store_14 <- data1[data1$Store == 14, ]
p <- ggplot(Store_14, aes(x=Weekly_Sales)) + geom_density(color="darkblue", fill="lightblue",alpha=0.2)+
  geom_vline(aes(xintercept= mean(Weekly_Sales)),color="steelblue", linetype="dashed", size=1)+
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5))+ scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6))+ ggtitle('Store 14 Sales distribution')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Weekly Sales") + ylab("Density")
p

data2<-data1

#Creating a month- year column in data2 
data2$month_Year = substr(data2$Date, 4, 10)

#Subsetting Q3-2012 data (i.e, 07-2012,08-2012,09-2012), Q2-2012 data (i.e, 04-2012,05- 2012,06-2012)
Q3_2012 <- filter(data2,month_Year == "07-2012" | month_Year== "08-2012" | month_Year== "09-2012")
Q2_2012 <- filter(data2,month_Year == "04-2012" | month_Year== "05-2012" | month_Year== "06-2012")

#Aggregating sales by store for Q3-2012 
Q3_2012_Sales<-summarise(group_by(Q3_2012,Store),sum(Weekly_Sales))

#Changing column names
colnames(Q3_2012_Sales)[2] <- "Q3_2012_Sales_by_Store"

#Aggregating sales by store each Q2-2012 
Q2_2012_Sales<-summarise(group_by(Q2_2012,Store),sum(Weekly_Sales))

#Changing column names
colnames(Q2_2012_Sales)[2] <- "Q2_2012_Sales_by_Store"

#merging two quarters data by store
Q3_2012_Growthrate <- merge ( Q2_2012_Sales , Q3_2012_Sales , by = 'Store')

#Creating Growth rate column for Sales by Store in the above dataframe 
Q3_2012_Growthrate <- mutate(Q3_2012_Growthrate, Growth_Rate = ((Q3_2012_Sales_by_Store - Q2_2012_Sales_by_Store)*100) / Q2_2012_Sales_by_Store)

#Creating only positive growth rates
positive_growthrate <- filter(Q3_2012_Growthrate, Growth_Rate > 0 ) 
positive_growthrate<-arrange(positive_growthrate, desc(Growth_Rate)) 
View(positive_growthrate)
a<- positive_growthrate$Store

#printing the output
print(paste(c('The positive growth rate Stores are', a),collapse=" " )) 
print(paste('Store',positive_growthrate[1,1], 'has highest growth rate & it is',positive_growthrate[1,4]))

options(repr.plot.width = 14, repr.plot.height = 8)

# Visual representation of growth rates
c<-ggplot(data=Q3_2012_Growthrate, aes(x=Store, y=Growth_Rate)) +geom_bar(stat ="identity",fill="steelblue")+
  ggtitle('Growth rates of Q3- 2012')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Stores") + ylab("Growth rate(%)") +
  scale_x_continuous("Stores", labels = as.character(Q3_2012_Growthrate$Store), breaks =
                       Q3_2012_Growthrate$Store)+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=0.5))
c
Holiday_date <- c("12-02-2010", "11-02-2011", "10-02-2012", "08-02-2013","10-09-2010", "09-09-2011", "07-09-2012", "06-09-2013","26-11-2010", "25-11-2011", "23-11-2012", "29- 11-2013","31-12-2010", "30-12-2011", "28-12-2012", "27-12-2013")
Events <-c(rep("Super Bowl", 4), rep("Labour Day", 4),rep("Thanksgiving", 4), rep("Christmas", 4))
Holidays_Data <- data.frame(Events,Holiday_date)

#merging both dataframes
data3<-merge(data1,Holidays_Data, by.x= "Date", by.y="Holiday_date", all.x = TRUE)

#Replacing null values in Event with No_Holiday 
data3$Events = as.character(data3$Events) 
data3$Events[is.na(data3$Events)]= "No_Holiday" 
head(data3)
Holiday_Sales<-aggregate(Weekly_Sales ~ Events, data = data3, mean)
#Changing column names
colnames(Holiday_Sales)[2] <- "Mean_Sales_by_Event_Type"
View(Holiday_Sales)
Holiday_date <- filter(data3,Holiday_Flag ==1)
Holiday_Date_Sales<-summarise(group_by(Holiday_date,Date),mean(Weekly_Sales))

#Caluclating mean of Weekly Sales for non holidays
mean_non_holiday_sales <- mean(filter(data3,Holiday_Flag ==0)$Weekly_Sales) 
Holiday_Date_Sales$higher_than_non_holiday <- Holiday_Date_Sales[,2] > mean_non_holiday_sales
View(Holiday_Date_Sales)
weekly_sales <- aggregate(Weekly_Sales~Date, data=data1,mean)
weekly_sales$Date <-as.Date(weekly_sales$Date, "%d-%m-%Y")
weekly_sales <-arrange(weekly_sales,Date)
weekly_sales$Date <-factor(weekly_sales$Date)


options(repr.plot.width = 14, repr.plot.height = 8)

# plotting weekly mean sales
d <- ggplot(data=weekly_sales, aes(x=Date, y=Weekly_Sales, group=1)) +
  geom_line(color="steelblue")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(breaks = levels(weekly_sales$Date)[c(T, rep(F, 9))])+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Week") + ylab("Mean Sales of Week")
d +ggtitle('CHRISTMAS')+
  geom_point(aes(x = factor("2010-12-31"), y = 898500.4), color = "red", size = 2) +
  geom_point(aes(x = factor("2011-12-30"), y = 1023165.8), color = "red", size = 2) +
  geom_hline(aes(yintercept = mean_non_holiday_sales), linetype="dashed")

#Plotting Labourday
d + ggtitle('LABOUR DAY')+
  geom_point(aes(x = factor("2010-09-10"), y = 1014097.7), color = "deeppink", size = 2) +
  geom_point(aes(x = factor("2011-09-09"), y = 1039182.8), color = "deeppink", size = 2) +
  geom_point(aes(x = factor("2012-09-07"), y = 	1074001.3), color = "deeppink", size = 2) +
  geom_hline(aes(yintercept = mean_non_holiday_sales), linetype="dashed")

#Plotting Thanks Giving
d + ggtitle('THANKS GIVING')+
  geom_point(aes(x = factor("2010-11-26"), y = 	1462689.0), color = "indianred4", size = 2) +
  geom_point(aes(x = factor("2011-11-25"), y = 1479857.9), color = "indianred4", size = 2) +
  geom_hline(aes(yintercept = mean_non_holiday_sales), linetype="dashed")

#Plotting Superbowl
d + ggtitle('SUPER BOWL')+
  geom_point(aes(x = factor("2010-02-12"), y = 	1074148.4), color = "goldenrod4", size = 2) +
  geom_point(aes(x = factor("2011-02-11"), y = 1051915.4), color = "goldenrod4", size = 2) +
  geom_point(aes(x = factor("2012-02-10"), y = 1111320.2), color = "goldenrod4", size = 2) +
  geom_hline(aes(yintercept = mean_non_holiday_sales), linetype="dashed")

