gg = getwd()
#setwd("C:/Users/esengie/Downloads/Odnoklassniki Hackathon/R")

basic.timestamp.statistics <- function(tt){ 
	tt = tt %/% 1000 #to seconds
	a_minute = 60
	an_hour = 3600
	a_day = 3600*24
	a_week = a_day * 7
	a_month = a_day*31
	a_year = a_day*366
	second = tt 
	minute = tt %/% a_minute
	week = tt %/% a_week
	#minute = (tt %% an_hour) %/% a_minute
	hour = tt %/%an_hour
	#hour = (tt %% a_day)%/%an_hour
	day = tt%/%a_day
	#day = (tt %% a_week)%/%a_day
	month = tt %/% a_month
	#month = (tt %% a_year) %/% a_month
	year = (tt %/% a_year - 21)
	
  data.frame(hour = hour, month = month, year = year, minute = minute, second = day, day = week)
}


extract.features3 <- function(input, output) {
  
  data1 <- read.csv(input)
  features <- basic.timestamp.statistics(data1$timestamp)
  data1 = data1[,-4]
  training.data.features <- cbind(data1, features) #?????????? timestamp
  write.csv(training.data.features, output, row.names = FALSE)  
}
extract.features4 <- function(input, output) {
  
  data1 <- read.csv(input)
  features <- basic.timestamp.statistics(data1$timestamp)
  data1 = data1[,-3]
  training.data.features <- cbind(data1, features) #?????????? timestamp
  write.csv(training.data.features, output, row.names = FALSE)  
}

extract.features3("../data/processed/train_features2.csv", "../data/processed/train_features2.csv")
extract.features4("../data/processed/test_features.csv", "../data/processed/test_features2.csv")
setwd(gg)