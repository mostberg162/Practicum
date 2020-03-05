### Richard M. Ostberg
### MSDS 692 Data Science Practicum
### Church Attendance Analysis
### 02/17/2020

#install.packages("imputeTS")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("forecast")
#install.packages("zoo")
#install.packages("lubridate")
library(forecast)
library(zoo)
library(imputeTS)
library(dplyr)
library(ggplot2)
library(lubridate)

## Read in the data and take care of formatting and missing data
Churchdata <- read.csv("/Users/mostberg/Desktop/RegisPracticum/ChurchDataForR.csv")
Churchdata$single.service <- factor(Churchdata$single.service, levels = c("Pre", "Single", "Post"))
Churchdata$Date <- as.Date(Churchdata$Date, '%m/%d/%y')

## distinguish between missing data and zeros
Churchdata <-
  Churchdata %>% mutate(X1st.Service = ifelse(Services == 0 , 0, X1st.Service)) 
Churchdata <-
  Churchdata %>% mutate(X2nd.Service = ifelse(Services < 2 , 0, X2nd.Service)) 
Churchdata <-
  Churchdata %>% mutate(third.xmas = ifelse(Services < 3 , 0, third.xmas)) 

## interpolate missing data
Churchdata$X1st.Service <- na_interpolation(Churchdata$X1st.Service, option = "linear")
Churchdata$X2nd.Service <- na_interpolation(Churchdata$X2nd.Service, option = "linear")
Churchdata$third.xmas <- na_interpolation(Churchdata$third.xmas, option = "linear")

## make averages per service
Churchdata <-
  Churchdata %>%
  rowwise() %>%
  mutate(Totalattendance = sum(X1st.Service,X2nd.Service,third.xmas, na.rm=TRUE))
Churchdata <- mutate(Churchdata,averageservice = Totalattendance/Services)

## A useful subset is Sundays only
Sundays <- 
  Churchdata %>%
  filter(Day.of.Week == 1 & Services > 0)

## Check to see if the single service period impacted attendance
SundayAnova <- Sundays[,c(5,17)]
fit <- lm(Totalattendance ~ ., data = SundayAnova)
anova(fit)

boxplot(Totalattendance ~ single.service, data = SundayAnova, main="Did Single Service Affect Attendance?", xlab = "Single Service", ylab='Total Attendance')

## ANOVA may skewed due to Easter, let's remove its influence to be sure
SundayNoHolidays <-
  Sundays %>%
  filter(Religious.Holiday == 0)

fit2 <- lm(Totalattendance ~ single.service, data = SundayAnova)
anova(fit2)

boxplot(Totalattendance ~ single.service, data = SundayNoHolidays, main="Did Single Service Affect Attendance?", xlab = "Single Service No Easter/Christmas", ylab='Total Attendance')

## The difference looks like it may be significant, let's follow up with a t-test
PrePost <-
  SundayNoHolidays %>%
  filter(single.service == "Pre" | single.service == "Post")

t.test(PrePost$Totalattendance~PrePost$single.service)
PrePost$single.service <- factor(PrePost$single.service, levels = c("Pre", "Post"))
boxplot(Totalattendance ~ single.service, data = PrePost, main="Did Single Service Affect Attendance?", xlab = "Single Service No Easter/Christmas", ylab='Total Attendance')

ggplot(PrePost, aes(Totalattendance, fill = single.service)) + geom_density(alpha = 0.2) + labs(title = "Did Single Service Affect Attendance?", y='% of Sundays', x='Total Attendance')

## Let's look at 2019-2020

PrePost2019 <-
  PrePost %>%
  filter(Date > "2019-01-01")

t.test(PrePost2019$Totalattendance~PrePost2019$single.service)
PrePost2019$single.service <- factor(PrePost2019$single.service, levels = c("Pre", "Post"))
boxplot(Totalattendance ~ single.service, data = PrePost2019, main="Did Single Service Affect Attendance in 2019-20?", xlab = "Single Service No Easter/Christmas", ylab='Total Attendance')

ggplot(PrePost2019, aes(Totalattendance, fill = single.service)) + geom_density(alpha = 0.2) + labs(title = "Did Single Service Affect Attendance in 2019-20?", y='% of Sundays', x='Total Attendance')


## Side analysis, there are a few Wednesday services, let's look into the effect on those.
WednesdayServices <-
  Churchdata %>%
  filter(Day.of.Week == 4 & Services > 0)

PrePostWednesday <- 
  WednesdayServices %>%
  filter(single.service == "Pre" | single.service == "Post")

t.test(PrePostWednesday$Totalattendance~PrePostWednesday$single.service)
ggplot(PrePostWednesday, aes(Totalattendance, fill = single.service)) + geom_density(alpha = 0.2)+ labs(title = "Did Single Service Affect Wednesday?", y='% of Wednesdays', x='Total Attendance')

## Growth Rate for total attendance
ggplot(Sundays, aes(Date,Totalattendance)) + geom_point(color = 'blue') +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Total Attendance Growth", y='Total Attendance')


## Simple regression Time series
LinearModel <- lm(Totalattendance~Date, data = SundayNoHolidays)
summary(LinearModel)

## Growth Rate
growthrate <- function(x)(x/lag(x,52)-1)
Sundays$growthrate <- growthrate(Sundays$Totalattendance)

summary(Sundays$growthrate)
## Simple ARIMA models for second view
ChurchTS <- ts(as.numeric(Sundays$Totalattendance), freq=365.25/7, 
               start=decimal_date(ymd("2016-05-29")))
ggtsdisplay(ChurchTS)
ggtsdisplay(diff(ChurchTS))
ggtsdisplay(diff(diff(ChurchTS)))

## I tried a number of different parameters, using guidelines from https://otexts.com/fpp2/arima.html
## This one fit best and had reasonable margins of error
fit2 <- Arima(ChurchTS, order = c(0,1,3), seasonal = list(order = c(2,1,0), period = 52))
print(fit2)
res <- residuals(fit2)
ggtsdisplay(res)
Box.test(res, lag=52, fitdf=2, type="Ljung")

plot(ChurchTS)
plot(forecast(fit2,h=159, bootstrap = TRUE), main="Total Attendance Forecast (Seasonal Model)", xlab = "Weekly View", ylab='Total Attendance')
nextyear <- forecast(fit2, h = 53, bootstrap = TRUE)
volumes <- as.data.frame(nextyear$mean)
print(volumes)

## Average service modeling to answer when to get a new church
ggplot(Sundays, aes(Date,averageservice)) + geom_point(color = 'blue') +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Average Attendance Growth", y='Average Attendance per Service')

## We need to take out the influence of "Single Service" 
## and interpolate the data for average service in those weeks

# Pull out the average for single service
SundaysNormalized <- Sundays %>%
  rowwise() %>%
  mutate(averageservice = ifelse(single.service == "Single", NA, averageservice))

# Turn it into a time series for Kalman Smoothing
NormalizedAvgTS <- ts(as.numeric(SundaysNormalized$averageservice), freq=365.25/7, 
                  start=decimal_date(ymd("2016-05-29")))

# Replace original with Kalman smoothing
usermodel <- arima(NormalizedAvgTS, order = c(53,0,3))$model
SundaysNormalized$averageservice <- na_kalman(NormalizedAvgTS, model = usermodel)

## Average service with smoothed (ARIMA Estimated) data
ggplot(SundaysNormalized, aes(Date,averageservice)) + geom_point(color = 'blue') +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Average Attendance Growth", y='Average Attendance per Service')
  
ChurchAvgTS <- ts(as.numeric(SundaysNormalized$averageservice), freq=365.25/7, 
                  start=decimal_date(ymd("2016-05-29")))
ggtsdisplay(ChurchAvgTS)
ggtsdisplay(diff(ChurchAvgTS))
ggtsdisplay(diff(diff(ChurchAvgTS)))

## I tried a number of different parameters, using guidelines from https://otexts.com/fpp2/arima.html
## This one fit best and had reasonable margins of error
fit2 <- Arima(ChurchAvgTS, order = c(4,1,0), seasonal = list(order = c(0,1,3), period = 52))
print(fit2)
res <- residuals(fit2)
ggtsdisplay(res)
Box.test(res, lag=53, fitdf=2, type="Ljung")

plot(ChurchAvgTS)
plot(forecast(fit2,h=159, bootstrap = TRUE), main="Average Attendance Forecast (Seasonal Model)", xlab = "Weekly View", ylab='Average Attendance')
next3year <- forecast(fit2, h = 159, bootstrap = TRUE)
volumes <- as.data.frame(nextyear$mean)
print(volumes)                        
print(nextyear$upper)

### Multiple regression to test extraneous variables for effect
RegressionSet <- Sundays[,8:17]
MultiLinearModelAvg <- lm(Totalattendance~., data = RegressionSet)
summary(MultiLinearModelAvg)

## Second model without religious holidays, since those effects are well known
SecondRegressionSet <- SundayNoHolidays[,8:17]
MultiLinearModelAvg <- lm(Totalattendance~., data = SecondRegressionSet)
summary(MultiLinearModelAvg)
