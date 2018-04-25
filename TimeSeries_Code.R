library(dplyr)
install.packages("forecast")
library("forecast")
library(tseries)

#Creating the data set
load("WA_SIDC_2013_CORE.rdat")
data2013 <- temp
colnames(data2013)
data2013 <- as.data.frame(data2013[,c("age","ageday","agemonth","died",  "dx1",  "dx2","dx3","dx4","dx5","dx6",  "dx7","dx8","dx9","dx10",  "dx11", "dx12",  "dx13",  "dx14",  "dx15",  "dx16", "dx17",  "dx18",  "dx19",  "dx20",  "dx21", "dx22",  "dx23", "dx24",  "dx25",  "female","nchronic","npr","orproc","ayear", "amonth")], drop=false)

load("WA_SIDC_2012_CORE.rdat")
data2012 <- temp

data2012 <- as.data.frame(data2012[,c("age","ageday","agemonth","died",  "dx1",  "dx2","dx3","dx4","dx5","dx6",  "dx7","dx8","dx9","dx10",  "dx11", "dx12",  "dx13",  "dx14",  "dx15",  "dx16", "dx17",  "dx18",  "dx19",  "dx20",  "dx21", "dx22",  "dx23", "dx24",  "dx25",  "female","nchronic","npr","orproc","ayear", "amonth")], drop=false)

load("WA_SIDC_2011_CORE.rdat")
data2011 <- temp

data2011 <- as.data.frame(data2011[,c("age","ageday","agemonth","died",  "dx1",  "dx2","dx3","dx4","dx5","dx6",  "dx7","dx8","dx9","dx10",  "dx11", "dx12",  "dx13",  "dx14",  "dx15",  "dx16", "dx17",  "dx18",  "dx19",  "dx20",  "dx21", "dx22",  "dx23", "dx24",  "dx25",  "female","nchronic","npr","orproc","ayear", "amonth")], drop=false)

load("WA_SIDC_2010_CORE.rdat")
data2010 <- temp

data2010 <- as.data.frame(data2010[,c("age","ageday","agemonth","died",  "dx1",  "dx2","dx3","dx4","dx5","dx6",  "dx7","dx8","dx9","dx10",  "dx11", "dx12",  "dx13",  "dx14",  "dx15",  "dx16", "dx17",  "dx18",  "dx19",  "dx20",  "dx21", "dx22",  "dx23", "dx24",  "dx25",  "female","nchronic","npr","orproc","ayear", "amonth")], drop=false)

load("FinalCore2009.rdat")
data2009 <- temp
colnames(data2009)
data2009 <- as.data.frame(data2009[,c("age","ageday","agemonth","died",  "dx1",  "dx2","dx3","dx4","dx5","dx6",  "dx7","dx8","dx9","dx10",  "dx11", "dx12",  "dx13",  "dx14",  "dx15",  "dx16", "dx17",  "dx18",  "dx19",  "dx20",  "dx21", "dx22",  "dx23", "dx24",  "dx25",  "female","nchronic","npr","orproc","ayear", "amonth")], drop=false)


load("WA_SIDC_2014_CORE.rdat")
data2014 <- temp
colnames(data2014)
data2014 <- as.data.frame(data2014[,c("AGE","AGEDAY","AGEMONTH","DIED",  "DX1",  "DX2","DX3","DX4","DX5","DX6",  "DX7","DX8","DX9","DX10",  "DX11", "DX12",  "DX13",  "DX14",  "DX15",  "DX16", "DX17",  "DX18",  "DX19",  "DX20",  "DX21", "DX22",  "DX23", "DX24",  "DX25",  "FEMALE","NCHRONIC","NPR","ORPROC","AYEAR", "AMONTH")], drop=false)
#changing column names to lower case
colnames(data2014) <- c("age","ageday","agemonth","died",  "dx1",  "dx2","dx3","dx4","dx5","dx6",  "dx7","dx8","dx9","dx10",  "dx11", "dx12",  "dx13",  "dx14",  "dx15",  "dx16", "dx17",  "dx18",  "dx19",  "dx20",  "dx21", "dx22",  "dx23", "dx24",  "dx25",  "female","nchronic","npr","orproc","ayear", "amonth")
colnames(data2014)

data <- bind_rows(data2009,data2010,data2011,data2012,data2013,data2014)
nrow(data)
colnames(data)

#Loading libs
library(dplyr)
library(tidyr)
library(ggplot2)


#Combining all the columns dxn columns for grep
data_comb_diag <- unite(data,diagnosis,dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,dx9,dx10,dx11,dx12,dx13,dx14,dx15,dx16,dx17,dx18,dx19,dx20,dx21,dx22,dx23,dx24,dx25, sep=",")
colnames(data)
mrsa_cases <- data_comb_diag[grep("(?=.*04112)", data_comb_diag$diagnosis, ignore.case = TRUE, perl = TRUE), ]
colnames(mrsa_cases)
nrow(mrsa_cases)
class(mrsa_cases)
eczema_cases <- data_comb_diag[grep("(?=.*6929)", data_comb_diag$diagnosis, ignore.case = TRUE, perl = TRUE), ]
nrow(eczema_cases)
eczema_mrsa_cases <- data_comb_diag[grep("(?=.*04112)(?=.*6929)", data_comb_diag$diagnosis, ignore.case = TRUE, perl = TRUE), ]
nrow(eczema_mrsa_cases)
local_skin_inf_cases <- data_comb_diag[grep("(?=.*6869)", data_comb_diag$diagnosis, ignore.case = TRUE, perl = TRUE), ]
nrow(local_skin_inf_cases)
surgery_cases <- data[,orproc]
surgery_cases
count(surgery_cases)
NROW(surgery_cases)
nrow(data)
colnames(data)
nrow(data)
which(data$orproc == "CA")
sum(na.omit(data$orproc) == TRUE)

data %>%
  filter(age>=0 & age <=3) %>%
  #group_by(age) %>%
  summarize(total.count=n())

data %>%
  filter(age>=60) %>%
  summarize(total.count=n())

colnames(mrsa_cases)
mrsa_cases_over_years <- mrsa_cases %>%
  group_by(ayear) %>%
  summarize(total.count=n())
colnames(mrsa_cases_over_years)

eczema_cases_over_years <- eczema_cases %>%
  group_by(ayear) %>%
  summarize(total.count=n())
colnames(eczema_cases_over_years)

ayear <- c(2009, 2010, 2011, 2012, 2013, 2014)
population <- c(6667000,6744000,6822000,6896000,6974000,7062000)
newDF <- data.frame(ayear, population)
colnames(newDF)

mrsa_cases_over_years <- merge(mrsa_cases_over_years, newDF, by=c("ayear"))
norm <- within (mrsa_cases_over_years  , { norm = (total.count/population)*1000000})
norm$cond <- rep("mrsa", each=1)
norm

eczema_cases_over_years <- merge(eczema_cases_over_years, newDF, by=c("ayear"))
normEczema <- within (eczema_cases_over_years  , { norm = (total.count/population)*1000000 })
normEczema$cond <- rep("eczema", each=1)
normEczema

mrsa_eczema_cases_over_years <- rbind(norm, normEczema)
nrow(mrsa_eczema_cases_over_years)

ggplot(mrsa_eczema_cases_over_years, aes(ayear, norm)) +   
  geom_bar(aes(fill = cond), position = "dodge", stat="identity") + scale_x_continuous(breaks=ayear) + 
  scale_fill_manual(values = alpha(c("blue", "red"), 0.5)) +
  xlab("Years") + ylab("Count Per 1 Million Population") + 
  ggtitle("Number of MRSA and Eczema cases over the years 2009-2014") 

mrsa_cases_over_years

# Doing TS on 2009 Data for MRSA Cases
mrsa_cases
colnames(data2009)
mrsa_cases_2009 <- data_comb_2009[grep("(?=.*04112)", data_comb_2009$diagnosis, ignore.case = TRUE, perl = TRUE), ]
class(mrsa_cases_2009)
colnames(mrsa_cases_2009)
nrow(mrsa_cases_2009)
data_comb_2009 <- unite(data2009,diagnosis,dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,dx9,dx10,dx11,dx12,dx13,dx14,dx15,dx16,dx17,dx18,dx19,dx20,dx21,dx22,dx23,dx24,dx25, sep=",")

mrsa_cases_2009_over_months <- mrsa_cases_2009 %>%
  group_by(amonth) %>%
  summarize(total.count=n())
data2009$amonth
mrsa_cases_2009_over_months

ts_2009 <- ts(mrsa_cases_2009_over_months$total.count, start =1, frequency = 1 )
ts_2009_quarterly <- ts(mrsa_cases_2009_over_months$total.count, start =1, frequency = 4 )
print(ts_2009_quarterly)
plot(ts_2009_quarterly)
print(ts_2009)
plot(ts_2009)

# Doing TS on Master data for MRSA Cases
mrsa_cases_over_years
time_series <- ts(mrsa_cases_over_years$total.count, start = c(2009,1), frequency = 12)
monthly <- ts(mydata,start=c(1990,1),frequency=12)
quarterly <- aggregate(time_series, nfrequency=4)
print(time_series)
plot(time_series)
plot(data$mrsa_cases)
print(quarterly)
plot(quarterly)
mrsa_cases_over_years_quarterly <- mrsa_cases %>%
  group_by(ayear) %>%
  summarize(total.count=n())
nrow(data2009$amonth)

# Doing TS on 2010 Data for MRSA Cases
data_comb_2010 <- unite(data2010,diagnosis,dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,dx9,dx10,dx11,dx12,dx13,dx14,dx15,dx16,dx17,dx18,dx19,dx20,dx21,dx22,dx23,dx24,dx25, sep=",")
mrsa_cases_2010 <- data_comb_2010[grep("(?=.*04112)", data_comb_2010$diagnosis, ignore.case = TRUE, perl = TRUE), ]
mrsa_cases_2010_over_months <- mrsa_cases_2010 %>%
  group_by(amonth) %>%
  summarize(total.count=n())
mrsa_cases_2010_over_months
ts_2010 <- ts(mrsa_cases_2010_over_months$total.count, start =1, frequency = 1 )
print(ts_2010)
plot(ts_2010)

# Doing TS on 2011 Data for MRSA Cases
data_comb_2011 <- unite(data2011,diagnosis,dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,dx9,dx10,dx11,dx12,dx13,dx14,dx15,dx16,dx17,dx18,dx19,dx20,dx21,dx22,dx23,dx24,dx25, sep=",")
mrsa_cases_2011 <- data_comb_2011[grep("(?=.*04112)", data_comb_2011$diagnosis, ignore.case = TRUE, perl = TRUE), ]
mrsa_cases_2011_over_months <- mrsa_cases_2011 %>%
  group_by(amonth) %>%
  summarize(total.count=n())
mrsa_cases_2011_over_months
ts_2011 <- ts(mrsa_cases_2011_over_months$total.count, start =1, frequency = 1 )
print(ts_2011)
plot(ts_2011)

# Doing TS on 2012 Data for MRSA Cases
data_comb_2012 <- unite(data2012,diagnosis,dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,dx9,dx10,dx11,dx12,dx13,dx14,dx15,dx16,dx17,dx18,dx19,dx20,dx21,dx22,dx23,dx24,dx25, sep=",")
mrsa_cases_2012 <- data_comb_2012[grep("(?=.*04112)", data_comb_2012$diagnosis, ignore.case = TRUE, perl = TRUE), ]
mrsa_cases_2012_over_months <- mrsa_cases_2012 %>%
  group_by(amonth) %>%
  summarize(total.count=n())
ts_2012 <- ts(mrsa_cases_2012_over_months$total.count, start =1, frequency = 1 )
print(ts_2012)
plot(ts_2012)

# Doing TS on 2013 Data for MRSA Cases
data_comb_2013 <- unite(data2013,diagnosis,dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,dx9,dx10,dx11,dx12,dx13,dx14,dx15,dx16,dx17,dx18,dx19,dx20,dx21,dx22,dx23,dx24,dx25, sep=",")
mrsa_cases_2013 <- data_comb_2013[grep("(?=.*04112)", data_comb_2013$diagnosis, ignore.case = TRUE, perl = TRUE), ]
colnames(mrsa_cases_2013)
mrsa_cases_2013_over_months <- mrsa_cases_2013 %>%
  group_by(amonth) %>%
  summarize(total.count=n())
mrsa_cases_2013_over_months
plot(mrsa_cases_2013_over_months, type = 'l')
colnames(data_comb_2013)
ts_2013 <- ts(nrow(mrsa_cases_2013), start = 1, frequency = 1 )
print(ts_2013)
plot(ts_2013)

# Doing TS on 2014 Data for MRSA Cases
data_comb_2014 <- unite(data2014,diagnosis,dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,dx9,dx10,dx11,dx12,dx13,dx14,dx15,dx16,dx17,dx18,dx19,dx20,dx21,dx22,dx23,dx24,dx25, sep=",")
mrsa_cases_2014 <- data_comb_2014[grep("(?=.*04112)", data_comb_2014$diagnosis, ignore.case = TRUE, perl = TRUE), ]
mrsa_cases_2014_over_months <- mrsa_cases_2014 %>%
  group_by(amonth) %>%
  summarize(total.count=n())
mrsa_cases_2014_over_months
ts_2014 <- ts(mrsa_cases_2014_over_months$total.count, start =1, frequency = 1 )
print(ts_2014)
plot(ts_2014)

# TS monthly for Master Data
colnames(mrsa_cases)
# mrsa_cases<-mrsa_cases[(ayear!="2008"), ]
mrsa_cases_new <- subset(mrsa_cases, ayear!= "2008")
head(mrsa_cases$ayear)
mrsa_cases_over_months <- mrsa_cases_new %>%
  group_by(ayear, amonth) %>%
  summarize(total.count=n())
mrsa_cases_over_months
tail(mrsa_cases_over_months)

time_series <- ts(mrsa_cases_over_months$total.count, start = c(2009,1), frequency = 12, end = c(2014,12))
monthly <- ts(mydata,start=c(1990,1),frequency=12)
quarterly <- aggregate(time_series, nfrequency=4)
print(time_series)
plot(time_series)

plot(data$mrsa_cases)
print(quarterly)
plot(quarterly)
# Seasonal Decomposition
fit <- stl(time_series, s.window="period")
plot(fit)
fit5 <- decompose(time_series)
plot(fit5)
monthplot(time_series)
hw <- HoltWinters(time_series, beta=FALSE, gamma=FALSE)
plot(hw)
forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast)



plot(fit2)
fit3 <- auto.arima(time_series)
plot(fit3)
fit6 <- arima(time_series, order = c(acf(), pacf(), ccf()))
fit4 <- ets(time_series)
plot(fit4)

nrow(data2009[ayear==2009 & amonth == c(1,2,3,4,5), ])
mrsa_cases_over_years_quarterly <- mrsa_cases %>%
  group_by(ayear) %>%
  summarize(total.count=n())

# Holt Winters Method for 2009-2013
data_new <- bind_rows(data2009,data2010,data2011,data2012,data2013)
data_comb_diag_new <- unite(data_new,diagnosis,dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,dx9,dx10,dx11,dx12,dx13,dx14,dx15,dx16,dx17,dx18,dx19,dx20,dx21,dx22,dx23,dx24,dx25, sep=",")
colnames(data_comb_diag_new)
tail(data_comb_diag_new$ayear)
mrsa_cases_training <- data_comb_diag_new[grep("(?=.*04112)", data_comb_diag_new$diagnosis, ignore.case = TRUE, perl = TRUE), ]
tail(mrsa_cases_training$ayear)
mrsa_cases_new_training <- subset(mrsa_cases_training, ayear!= "2008")
mrsa_cases_training_over_months <- mrsa_cases_new_training %>%
  group_by(ayear, amonth) %>%
  summarize(total.count=n())
tail(mrsa_cases_training_over_months)
time_series_training <- ts(mrsa_cases_training_over_months$total.count, start = c(2009,1), frequency = 12, end = c(2013,12))
print(time_series_training)
plot(time_series_training)
hw_training <- HoltWinters(time_series_training, beta=FALSE, gamma=FALSE)
forecast_training <- predict(hw_training, n.ahead = 12, prediction.interval = T, level = 0.95)
hw <- HoltWinters(time_series, beta=FALSE, gamma=FALSE)
plot(hw_training, forecast_training, main = "Holt Winters Model with Prediction")
plot(hw, main = "Holt Winters Model For Observed Data")
par(mfrow = c(1,2))
summary(hw)
summary(hw_training)
summary(forecast_training)

# KPSS Test for Trend Stationarity
kpss.test(time_series, null = "Trend")

#data:  time_series
# KPSS Trend = 0.040858, Truncation lag parameter = 1, p-value = 0.1

# ADF Test
adf.test(time_series, alternative= "stationary")
?adf.test
adf.test(mrsa_cases_over_years$total.count, alternative= "stationary")
