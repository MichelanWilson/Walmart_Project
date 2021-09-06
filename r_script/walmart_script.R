#Set Working Directory
setwd("/Users/michy/OneDrive/Documents/Simplilearn")
getwd()

#Read file
Walmart <- read.csv("Walmart_Store_sales.csv")
View(Walmart)
str(Walmart)

#Change date data type
Walmart$Date<- as.character(Walmart$Date)
class(Walmart$Date)
library(lubridate) # load lubridate

#Change date format
Walmart$Date<- parse_date_time(x = Walmart$Date,
                orders = c("d-m-y", "d/m/y"),
                locale = "eng")

Walmart$Year <- year(Walmart$Date)#Extract year from date

#Maximum sales - store 14
Walmart[which.max(Walmart$Weekly_Sales),]
#Summary Statistics
library(dplyr)
summary_data <- ddply(Walmart, "Store", summarise,
               mean = mean(Weekly_Sales),
               sd   = sd(Weekly_Sales),
               se   = sd /mean
)
summary_data
#Maximum standard deviation
summary_data[which.max(summary_data$sd),]

#Organize data quarterly
by_month <- month(Walmart$Date, label=TRUE) #Extract month from date
by_month
str(by_month)
Walmart<- cbind(Walmart, month_col = by_month) # add month column
Walmart$Quarter <- ifelse(by_month== "Jan"|by_month=="Feb"|by_month=="Mar", "Q1", ifelse(by_month== "Apr"|by_month=="May"|by_month=="Jun","Q2",ifelse(by_month=="Jul"|by_month=="Aug"|by_month=="Sep","Q3","Q4"))) #identify quarters
arrange(Walmart, Quarter) #arrange by quarters
#Walmart[order(as.Date(Walmart$Date, format="%Y/%m/%d"))]
#Aggregate data
?aggregate
#Load dplyr package

quarterly_sales<- aggregate(Walmart$Weekly_Sales, by= list(Walmart$Quarter, Walmart$Store,format(Walmart$Date, format = "%Y")), FUN=sum) #aggregate monthly data by quarters
by_year <- year(Walmart$Date) #Extract year from date
by_year
Walmart<- cbind(Walmart, year_col = by_year) #Combine year dataframe with Walmart dtatframe (merge dataframes)
Walmart$year_col<-as.character(Walmart$year_col) # Convert year to character
str(Walmart)
joined<- Walmart %>% left_join(quarterly_sales, by= c("Store"="Group.2", "year_col"= "Group.3", "Quarter"="Group.1"))#Merge quarterly sales to master dataset by store, year and quarter (there are repeated values for each week in the same quarter)
View(joined)
Walmart[with(Walmart, order("Store", "Quarter"))]

####Growth rate
quarterly_sales2<- aggregate(joined$x, by= list(joined$year_col, joined$Store, joined$Quarter), FUN=mean) # Filtering single quarterly sales for each store for each year from repeated values
quarterly_sales2<- dplyr::rename(quarterly_sales2, Year = Group.1,Store = Group.2, Quarter=Group.3) #Rename Columns
View(quarterly_sales2)
quarterly_sales2<- arrange(quarterly_sales2, list("Store"), by_group=TRUE)
quarterly_sales2_ordered <-quarterly_sales2[order(quarterly_sales2$Store, quarterly_sales2$Quarter, quarterly_sales2$Year),] #Order by each store, quarter and then year
View(quarterly_sales2_ordered)
##Filter the year and quarters of interest
quarterly_sales2_ordered<-quarterly_sales2_ordered %>% filter(Group.1=="2012") #filter 2012
quarterly_sales2_ordered<- quarterly_sales2_ordered %>% filter(Group.3=="Q2"|Group.3=="Q3") #filter quarters 2 and 3 for each store
#Calculate growth rate
growth_rate<-quarterly_sales2_ordered %>% group_by(Group.2)%>% mutate(growth1 = ((x/lag(x))-1)*100) #calculate and add column for growth rate
View(growth_rate)
growth_rate<- growth_rate %>% filter(growth1 != "NA") #Remove quarter 2 growth rates which are NA by default
View(growth_rate)
arrange(desc(growth_rate), growth1, by_group=TRUE) # arrange growth rate from higest to lowest
growth_rate_description<- growth_rate %>% arrange(desc(growth1)) %>% head(20) # view top 20 growth rates
growth_rate_description$description<-  ifelse(growth_rate_description$growth1 >0, "good", "bad")
growth_rate_description$growth1<-as.numeric(growth_rate_description$growth1)
View(growth_rate_description)
###Identify holidays with higher sales than average week
Walmart$month_col<- as.character(Walmart$month_col) #change month column from factor to character
Walmart$Holiday<- ifelse(Walmart$month_col=="Feb" & Walmart$Holiday_Flag==1,"Super Bowl", ifelse(Walmart$month_col=="Sep" & Walmart$Holiday_Flag==1,"Labour Day",ifelse(Walmart$month_col=="Nov" & Walmart$Holiday_Flag==1,"Thanksgiving", ifelse(Walmart$month_col=="Dec" & Walmart$Holiday_Flag==1,"Christmas","No Holiday")))) #Assign Holiday names and non-holiday
Walmart_non_holiday<- Walmart[Walmart$Holiday_Flag==0,c(2,3,4,13)] # filter non-holidays
View(Walmart_non_holiday)
Average<- mean(Walmart_non_holiday$Weekly_Sales) #find average weekly sales for non-holidays
Walmart_holiday<-Walmart[Walmart$Holiday_Flag==1,c(2,3,4,13)] #filter holidays for each year
View(Walmart_holiday)
Average_Holiday2 <- aggregate(Walmart_holiday$Weekly_Sales, by= list(Walmart_holiday$Holiday), FUN=mean)
#Average_Holiday<-Walmart_holiday %>% group_by(Holiday)%>% summarise(average_holiday = mean(Weekly_Sales)) #find mean weekly sales for each holiday
Average_Holiday2<-rename(Average_Holiday2, Holiday = Group.1, Average_Sales=x)
Average_Holiday2$rank<-ifelse (Average_Holiday2$Average_Sales>Average, print("higher"), print("lower")) # compare holiday weekly sales to non-holiday weekly sales
View(Average_Holiday2)

####Provide a monthly and semester view of sales in units
monthly<- aggregate(Walmart$Weekly_Sales, by=list(by_month <- month(Walmart$Date, label=TRUE)), FUN=sum) #aggregate monthly sales across all years
monthly<- monthly %>% rename(Month = Group.1, Sales=x) #rename groups
View(monthly)
install.packages("ggplot2") #Install ggplot
library(ggplot2)#Load ggplot
month_chart<- ggplot(monthly,aes(x=Month, y=Sales))+
  geom_bar(stat="identity",fill="steelblue") #Create chart of monthly sales
month_chart #View monthly chart
for_semester<- aggregate(Walmart$Weekly_Sales, by=list(by_semester<- semester(Walmart$Date, with_year = TRUE)), FUN=sum) #Aggregate sales by semesters for 3 years combined
for_semester<- for_semester %>% rename(Semester = Group.1, Sales=x) #rename groups
View(for_semester)
install.packages("tidyverse") #Install tidyverse
library(tidyr)#Load tidyverse
for_semester$Semester<- as.character(for_semester$Semester)
for_semester$Sales<- as.numeric(for_semester$Sales)
for_semester<-for_semester %>% mutate(sem=sapply(strsplit(for_semester$Semester, split=".", fixed=TRUE), `[`, 2)) #split year-semester into year and semester
for_semester<-for_semester %>% mutate(yr=sapply(strsplit(for_semester$Semester, split=".", fixed=TRUE), `[`, 1)) #split year-semester into year and semester
semester_chart<- ggplot(for_semester,aes(fill = sem, x=yr, y=Sales))+
  geom_bar(stat="identity",position = "dodge") #Create chart for semester sales
semester_chart #view semester chart




#####Statistical Model

Walmart$Week_num <- ave(Walmart$Weekly_Sales,
                        Walmart$Store, Walmart$Year,
                       FUN = seq_along)
#Regress weekly sales on unemployment
mod1 <-lm(formula = Weekly_Sales~Unemployment, data = Walmart)
mod1
summary(mod1)

#Regress weekly sales on unemployment by store
model2<- lm(Weekly_Sales~Unemployment + as.factor(Store), data=Walmart)
model2
summary(model2)

#Regress weekly sales on unemployment and temperature
mod3 <-lm(formula = Weekly_Sales~Unemployment+Temperature, data = Walmart)
mod3
summary(mod3)

#Regress weekly sales on unemployment and holiday
mod4 <-lm(formula = Weekly_Sales~Unemployment+Holiday_Flag, data = Walmart)
mod4
summary(mod4)

#Regress wekly sales on unemployment and holiday by store
model5<- lm(Weekly_Sales~Unemployment + Holiday_Flag+ as.factor(Store), data=Walmart)
model5
summary(model5)

#Regress weekly sales on unemployment, temperature and holiday by store
model6<- lm(Weekly_Sales~Unemployment + Temperature + Holiday_Flag+ as.factor(Store), data=Walmart)
model6
summary(model6)

mod7 <-lm(formula = Weekly_Sales~Unemployment+Holiday_Flag + Week_num, data = Walmart)
mod7
summary(mod7)

#Regress wekly sales on unemployment, holiday and week number in the year by store
model8<- lm(Weekly_Sales~Unemployment + Holiday_Flag + Week_num + as.factor(Store), data=Walmart)
model8
summary(model8)

#Regress wekly sales on unemployment, holiday, temperature and week number in the year by store
model9<- lm(Weekly_Sales~Unemployment + Holiday_Flag + Temperature + Week_num + as.factor(Store), data=Walmart)
model9
summary(model9)

#Does day of the year matter?
Walmart$Day <- format(Walmart$Date, format= "%d") #Extract day from date

#Regress wekly sales on unemployment, holiday, temperature and week number in the year and day of the month by store
model10<- lm(Weekly_Sales~Unemployment + Holiday_Flag + Temperature + Week_num + Day+ as.factor(Store), data=Walmart)
model10
summary(model10)
#Days at the beginning and end of the moth are significant
#Model 10 has the highest adjusted r-squared
#In model 10, unemployment, holiday and temperature are highly significant
#Model 10 is significant at the 1%level

#Regress wekly sales on CPI, holiday, temperature and week number in the year and day of the month by store
model11<- lm(Weekly_Sales~ CPI + Holiday_Flag + Temperature + Week_num + Day+ as.factor(Store), data=Walmart)
model11
summary(model11)
#CPI is insignificant

#Regress wekly sales on unemployment, holiday, temperature, fuel price and week number in the year and day of the month by store
model12<- lm(Weekly_Sales~Unemployment + Holiday_Flag + Temperature + Fuel_Price + Week_num + Day+ as.factor(Store), data=Walmart)
model12
summary(model12)
#Fuel price is significant
#Fuel price does not increase explanatory power of model

#Regress wekly sales on CPI, holiday, temperature and week number in the year and day of the month by store
model13<- lm(Weekly_Sales~ Unemployment + CPI + Holiday_Flag + Temperature + Week_num + Day+ as.factor(Store), data=Walmart)
model13
summary(model13)
###When CPI and unemployment are regressed together, CPI becomes significant



knitr::stitch('Project 1. r')


library(rmarkdown)

