library(ggfortify)
library(ggplot2)
library(ggbiplot)
library(dplyr)
library(depmixS4)
library(plyr)

### Load Data
setwd("C:/Users/lebri/Documents/cmpt318/")
getwd()
file <- "household_power_consumption.txt"
data <- read.csv(file, header = TRUE, sep = ";")
data <- na.omit(data)

# Convert the Date and Time columns to a POSIXct time stamp
# data$DateTime <- as.POSIXct(paste(data$Date, data$Time), format="%d/%m/%Y %H:%M:%S")
# data$days <- as.POSIXlt(data$Date, format="%d/%m/%Y")$wday
# data$hour <- as.POSIXlt(data$Time, format="%H:%M:%S")
# data$Date <-as.POSIXlt(data$Date, format="%d/%m/%Y")
# data$Time <-as.POSIXlt(data$Time, format="%H:%M:%S")

data_numeric <- subset(data, select = -c(Date, Time))
# data_numeric <- subset(data, select = -c(Date, Time, DateTime))
columns_to_interpolate <- c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
head(which(is.na(data_numeric$Global_active_power)))
data_numeric[columns_to_interpolate] <- lapply(data_numeric[columns_to_interpolate], function(x) as.numeric(as.character(x)))

# Apply feature scaling to the numeric dataset
# stan_data <- as.data.frame(scale(data_numeric, center = TRUE, scale = TRUE))
norm_data <- as.data.frame(apply(data_numeric, 2, function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))))

pca <- prcomp(norm_data, center = TRUE, scale = TRUE)
summary(pca)
print(pca)

# Create graphs
# https://cran.r-project.org/web/packages/ggbiplot/readme/README.html
# https://www.youtube.com/watch?v=0Jp4gsfOLMs
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
ggscreeplot(pca)
ggbiplot(pca, choices=c(1,2), circle = TRUE, obs.scale = 0.5, var.scale = 1, alpha = 0)

# Loadings scores
loading_scores <- pca$rotation[,1]
scores <- abs(loading_scores)
scores_ranked <- sort(scores, decreasing=TRUE)
print(scores_ranked)

# Reattach DateTime
# stan_data$DateTime <- data$DateTime
# norm_data$DateTime <- data$DateTime
norm_data$Date <- data$Date
norm_data$Time <- data$Time

# Perform HMM
set.seed(1)

# Get test and train data
training_data <- subset(data, format(as.POSIXct(norm_data$Date, format="%d/%m/%Y"), format="%Y") != "2010")
test_data <- subset(data, format(as.POSIXct(norm_data$Date, format="%d/%m/%Y"), format="%Y") == "2010")

training_data$days <- as.POSIXlt(training_data$Date, format="%d/%m/%Y")$wday
training_data$hour <- as.POSIXlt(training_data$Time, format="%H:%M:%S") 
training_data_series <- subset(training_data, training_data$days == 6 & training_data$hour$hour >= 9 & training_data$hour$hour < 15)
# times <- count(training_data_series, "Date")$freq
training_data_series <- na.omit(training_data_series)
times <- as.data.frame(table(training_data_series$Date))$Freq

test_data$days <- as.POSIXlt(test_data$Date, format="%d/%m/%Y")$wday
test_data$hour <- as.POSIXlt(test_data$Time, format="%H:%M:%S") 
test_data_series <- subset(test_data, test_data$days == 6 & test_data$hour$hour >= 9 & test_data$hour$hour < 15)
test_data_series <- na.omit(test_data_series)
times_test <- as.data.frame(table(test_data_series$Date))$Freq

# Discretize
# columns_to_interpolate <- names(training_data_series)[3:9]
# for(col in columns_to_interpolate) {
#   training_data_series[[col]] <- round_any(as.numeric(training_data_series[[col]]), 0.5)
# }
# 
# # Discretize
# columns_to_interpolate <- names(test_data_series)[3:9]
# for(col in columns_to_interpolate) {
#   test_data_series[[col]] <- round_any(as.numeric(test_data_series[[col]]), 0.5)
# }

# numeric
columns_to_interpolate <- names(training_data_series)[3:9]
for(col in columns_to_interpolate) {
  training_data_series[[col]] <- as.numeric(training_data_series[[col]])
}

columns_to_interpolate <- names(test_data_series)[3:9]
for(col in columns_to_interpolate) {
  test_data_series[[col]] <- as.numeric(test_data_series[[col]])
}

states_range <- list(4, 8, 12, 16)
log_likelihood <- numeric(length(states_range))
BIC <- numeric(length(states_range))

# !!! Model5 is the best model. Do not have to run all models
# 
# model1 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1, Sub_metering_3 ~ 1), data = training_data_series, family=list(multinomial("identity"), multinomial("identity"), multinomial("identity")), nstates = 4, ntimes=times)
# fitModel1 <- fit(model1)
# log_likelihood[1] <- logLik(fitModel1)
# BIC[1] <- BIC(fitModel1)
# 
# model2 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1, Sub_metering_3 ~ 1), data = training_data_series, family=list(multinomial("identity"), multinomial("identity"), multinomial("identity")), nstates = 8, ntimes=times)
# fitModel2 <- fit(model2)
# log_likelihood[2] <- logLik(fitModel2)
# BIC[2] <- BIC(fitModel2)
# 
# model3 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1, Sub_metering_3 ~ 1), data = training_data_series, family=list(multinomial("identity"), multinomial("identity"), multinomial("identity")), nstates = 12, ntimes=times)
# fitModel3 <- fit(model3)
# log_likelihood[3] <- logLik(fitModel3)
# BIC[3] <- BIC(fitModel3)
# 
# model4 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1, Sub_metering_3 ~ 1), data = training_data_series, family=list(multinomial("identity"), multinomial("identity"), multinomial("identity")), nstates = 16, ntimes=times)
# fitModel4 <- fit(model4)
# log_likelihood[4] <- logLik(fitModel4)
# BIC[4] <- BIC(fitModel4)
# 
# model5 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1, Sub_metering_3 ~ 1), data = training_data_series, family=list(multinomial("identity"), multinomial("identity"), multinomial("identity")), nstates = 20, ntimes=times)
# fitModel5 <- fit(model5)
# log_likelihood[5] <- logLik(fitModel5)
# BIC[5] <- BIC

model1 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1, Sub_metering_3 ~ 1), data = training_data_series, family=list(gaussian(), gaussian(), gaussian()), nstates = 4, ntimes=times)
fitModel1 <- fit(model1)
log_likelihood[1] <- logLik(fitModel1)
BIC[1] <- BIC(fitModel1)

model2 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1, Sub_metering_3 ~ 1), data = training_data_series, family=list(gaussian(), gaussian(), gaussian()), nstates = 8, ntimes=times)
fitModel2 <- fit(model2)
log_likelihood[2] <- logLik(fitModel2)
BIC[2] <- BIC(fitModel2)

model3 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1, Sub_metering_3 ~ 1), data = training_data_series, family=list(gaussian(), gaussian(), gaussian()), nstates = 12, ntimes=times)
fitModel3 <- fit(model3)
log_likelihood[3] <- logLik(fitModel3)
BIC[3] <- BIC(fitModel3)


model4 <- depmix(list(Global_intensity ~ 1, Global_active_power ~ 1, Sub_metering_3 ~ 1), data = training_data_series, family=list(gaussian(), gaussian(), gaussian()), nstates = 16, ntimes=times)
fitModel4 <- fit(model4)
log_likelihood[4] <- logLik(fitModel4)
BIC[4] <- BIC(fitModel4)

# Plot log-likelihood and BIC values
png("Plot.png")
par(mfrow=c(2, 1))
plot(states_range, log_likelihood, type = "b", xlab = "Number of States", ylab = "Log-Likelihood", main = "Log-Likelihood vs Number of States")
plot(states_range, BIC, type = "b", xlab = "Number of States", ylab = "BIC", main = "BIC vs Number of States")

# Find the model with the highest log-likelihood and lowest BIC
best_model_index <- which.max(log_likelihood)
best_model_states <- states_range[best_model_index]
best_model_log_likelihood <- log_likelihood[best_model_index]
best_model_BIC <- BIC[best_model_index]
cat("Best Model:\n")
cat("Number of States:", best_model_states[[1]], "\n")
cat("Log-Likelihood:", best_model_log_likelihood, "\n")
cat("BIC:", best_model_BIC, "\n")

dev.off()

# Save data
log_BIC <- as.data.frame(log_likelihood)
log_BIC$BIC <- as.data.frame(BIC)
colnames(log_BIC)[1] <- "loglik"
colnames(log_BIC)[2] <- "BIC"
write.table(log_BIC, "log_BIC.txt", sep = ",", row.names = FALSE, col.names = TRUE)

# Testing with best model
# https://stackoverflow.com/questions/31019622/evaluating-sequence-with-a-fitted-model-using-depmixs4-in-r
# https://search.r-project.org/CRAN/refmans/depmixS4/html/depmix-methods.html
model3_test = depmix(list(Global_intensity ~ 1, Global_active_power ~ 1, Sub_metering_3 ~ 1), data = test_data_series, family=list(gaussian(), gaussian(), gaussian()), nstates = 12, ntimes=times_test)
model3_test = setpars(model3_test, getpars(fitModel3))
model_forwardbackward3 <- forwardbackward(model3_test)

model4_test = depmix(list(Global_intensity ~ 1, Global_active_power ~ 1, Sub_metering_3 ~ 1), data = test_data_series, family=list(gaussian(), gaussian(), gaussian()), nstates = 16, ntimes=times_test)
model4_test = setpars(model4_test, getpars(fitModel4))
model_forwardbackward <- forwardbackward(model4_test)

normalized_train_m3 <- logLik(fitModel3)/nrow(training_data_series)
normalized_test_m3 <- logLik(model3_test)/nrow(test_data_series)


normalized_train_m4 <- logLik(fitModel4)/nrow(training_data_series)
normalized_test_m4 <- logLik(model4_test)/nrow(test_data_series)


# Anomaly Detection

# Assuming the test time window is what's being partitioned
test_subset1 <- test_data_series[1:1584,]
test_subset2 <- test_data_series[1585:(2*1584),]
test_subset3 <- test_data_series[3169:(3*1584),]
test_subset4 <- test_data_series[4753:(4*1584),]
test_subset5 <- test_data_series[6337:(5*1584),]
test_subset6 <- test_data_series[7921:(6*1584),]
test_subset7 <- test_data_series[9505:(7*1584),]
test_subset8 <- test_data_series[11089:(8*1584),]
test_subset9 <- test_data_series[12673:(9*1584),]
test_subset10 <- test_data_series[14257:15840,]

test_subsets <- list(test_subset1,test_subset2,test_subset3,test_subset4,test_subset5,test_subset6,test_subset7,test_subset8,test_subset9,test_subset10)
subsetLogs <- 1:10
for(i in 1:10) {
  set.seed(1)
  subset <- test_subsets[[i]]
  times_subset <- as.data.frame(table(as.data.frame(subset)$Date))$Freq
  model = depmix(list(Global_intensity ~ 1, Global_active_power ~ 1, Sub_metering_3 ~ 1), data = test_subsets[[i]], family=list(gaussian(), gaussian(), gaussian()), nstates = 16, ntimes=times_subset)
  model = setpars(model, getpars(fitModel4))
  modelforwardbackward <- forwardbackward(model)
  cat('Log likelyhood subset', i, ': ', modelforwardbackward$logLik/nrow(test_subsets[[i]]), '\n')
  subsetLogs[i] = modelforwardbackward$logLik/nrow(test_subsets[[i]])
}
# maximum deviation from the train log-likelihood value
subsetLogsDev <- 1:10
for(i in 1:10) {
  cat('Log likelihood threshold: ', abs(normalized_train_m4 - subsetLogs[[i]]), '\n')
  subsetLogsDev[i] <- abs(normalized_train_m4 - subsetLogs[[i]])
}

cat('Log likelihood threshold: ', max(subsetLogsDev))







