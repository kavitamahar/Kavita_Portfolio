
## installing the library
install.packages("tidyverse")
install.packages("")

#loading the packages 
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)


dailyActivity <- read.csv("Downloads/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
intensity <- read.csv("Downloads/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
#sleep <- read.csv("Downloads/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
calories <- read.csv("Downloads/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
weight <- read.csv("Downloads/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

sleepDay_merged <- read_csv("Downloads/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")

#view the datasets that are imported. 
#already checked the data in excel but review the data
head(dailyActivity_merged)

dailyActivity_merged$ActivityDate=as.POSIXct(dailyActivity_merged$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
dailyActivity_merged$date <- format(dailyActivity_merged$ActivityDate, format = "%m/%d/%y")
head(Daily_Activity)
Daily_Activity = dailyActivity_merged

#rename the heartRate_seconds_merged dataset
heartRate = heartrate_seconds_merged
heartRate$Timw=as.POSIXct(heartRate$Time, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
heartRate$date <- format(heartRate$Time, format = "%m/%d/%y")

head(heartRate)

# intensities
intensities = hourlyIntensities_merged
intensities$ActivityHour=as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")
# calories
calories = hourlyCalories_merged
calories$ActivityHour=as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")

# sleep
sleep = minuteSleep_merged
sleep$date=as.POSIXct(sleep$date, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$time <- format(sleep$date, format = "%I:%M:%S %p")
head(sleep)

#total number of people participated
n_distinct(Daily_Activity$Id)
n_distinct(heartRate$Id)
n_distinct(intensities$Id)
n_distinct(calories$Id)
n_distinct(sleep$Id)
n_distinct(weightLogInfo_merged)

#
Daily_Activity %>%  select(TotalSteps,TotalDistance, 
                         SedentaryMinutes, Calories) %>%
  summary()

# explore num of active minutes per category
Daily_Activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

#calories
calories %>% select(Calories) %>% summary()

# sleep
sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()
# weight
weightLogInfo_merged %>%
  select(WeightKg, BMI) %>%
  summary()
#heart
heartRate %>% select (Time, Value) %>% summary()

#merging
merge_data <- merge(calories, heartRate ,by=c('Id')) #, 'date'))
head(merge_data)


#Visulation
ggplot(data= Daily_Activity)+
  geom_point(mapping = aes(x= TotalSteps, y = Calories), color= "Pink")+
   labs(title="Total Steps VS. Calories")+ 
    geom_smooth(mapping = aes(x= TotalSteps, y = Calories),method = "lm", se = FALSE)


ggplot(data= heartRate)+
  geom_bar(mapping = aes(x= Value),color="Pink") +scale_x_continuous( name="HeartRate",trans= 'log10')+
  scale_y_continuous( name="No. Of People",trans= 'log10')+
  labs(title="HeartRate VS. No. Of People")

head(intensities)



int_new <- intensities %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

ggplot(data=int_new, aes(x=time, y=mean_total_int)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Total Intensity vs. Time")

head(sleep)
sleep_new <- sleep %>%
  group_by(date) %>%
  drop_na() %>%
  summarise(mean_total_int = mean(value))

ggplot(data=sleep_new)+geom_bar(mapping =  aes(x=mean_total_int))+
  scale_y_continuous( name="No. Of People",trans= 'log10')+
  labs(title="Sleep Value vs. No Of People") 



                    