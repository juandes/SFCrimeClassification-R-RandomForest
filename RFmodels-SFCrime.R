require(randomForest)
setwd("~/Development/R/SFCrimeClassification")
train <- read.csv("~/Development/R/SFCrimeClassification/train.csv")
test <- read.csv("~/Development/R/SFCrimeClassification/test.csv")

# Random forest model using the day of the week and the district of the crime
# as the predictors.
rf <- randomForest(Category ~ DayOfWeek + PdDistrict, data = train, ntree = 25)

# Make a new column containing the hour (24h format) of the crime
train$Hour <- sapply(train$Dates, function(x) as.integer(strftime(x, format = "%H")))

# Another random forest model using the same predictors as before, plus the hour of
# the crime
rf <- randomForest(Category ~ DayOfWeek + PdDistrict + Hour, data = train, 
                   ntree = 25)

# Function that returns the time of the day (early morning, morning, afternoon or
# or night) according to the hour.
timeoftheday <- function(hour) {
  if (hour >= 1 && hour <= 6) { return (as.factor("early morning"))}
  else if (hour >= 7 && hour <= 11) { return (as.factor("morning"))}
  else if (hour >= 12 && hour <= 19) { return (as.factor("afternoon"))}
  else return (as.factor("night"))
}

train$TimeOfDay <- sapply(train$Hour, timeoftheday)

rf <- randomForest(Category ~ DayOfWeek + PdDistrict + TimeOfDay, data = train, 
                   ntree = 25)

# Add the hour column to the test set.
test$Hour <- sapply(test$Dates, function(x) as.integer(strftime(x, format = "%H")))
predictions.result <- predict(rf, test)

# Prepare the output file
results.for.submission <- table(1:length(predictions.result), predictions.result)
rownames(results.for.submission) <- 0:884261
write.csv(results.for.submission, file = "results.csv")