# Load libraries 
library("readxl")
library("dplyr")
library("ggplot2")
library("reshape2")
library("gridExtra")
library("neuralnet")
library("grid")
library("MASS")
library("MLmetrics")

# Import the dataset using the read_excel() function
data <- read_excel("ExchangeUSD.xlsx")
head(dataset)
dim(dataset)

# Take only the exchange rate column
dataset <- as.data.frame(data[[3]])
head(dataset)

# Data Preparation

#Function used to normalize using the Min-max normalization method
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#Function used to de-normalize(Min-max normalization)
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min )
}

# Normalizing the dataset
datast_norm <- as.data.frame(lapply(dataset, normalize))
head(datast_norm)

# Splitting the normalized data into training and testing sets
data_train <- datast_norm[1:400, ]
data_test <- datast_norm[401:500, ]

# Creating lagged features for 1 lag
t1_train <- bind_cols(previous_Day1 = lag(data_train, 1),
                      t1_expected = data_train)
t1_test <- bind_cols(previous_Day1 = lag(data_test, 1),
                     t1_expected = data_test)

# Removing rows with missing values
t1_train <- t1_train[complete.cases(t1_train), ]
t1_test <- t1_test[complete.cases(t1_test), ]

# Displaying the first few rows of the training dataset with lagged features (1 lag)
head(t1_train)

# Displaying the first few rows of the testing dataset with lagged features (1 lag)
head(t1_test)

# Creating lagged features for 2 lag
t2_train <- bind_cols(previous_Day2 = lag(data_train, 2),
                      previous_Day1 = lag(data_train, 1),
                      t2_expected = data_train)
t2_test <- bind_cols(previous_Day2 = lag(data_test, 2),
                     previous_Day1 = lag(data_test, 1),
                     t2_expected = data_test)


# Removing rows with missing values
t2_train <- t2_train[complete.cases(t2_train), ]
t2_test <- t2_test[complete.cases(t2_test), ]

# Displaying the first few rows of the training dataset with lagged features (2 lags)
head(t2_train)

# Displaying the first few rows of the testing dataset with lagged features (2 lags)
head(t2_test)

# Creating lagged features for 3 lags in the training dataset
t3_train <- bind_cols(previous_Day3 = lag(data_train, 3),
                      previous_Day2 = lag(data_train, 2),
                      previous_Day1 = lag(data_train, 1),
                      t3_expected = data_train)

# Creating lagged features for 3 lags in the testing dataset
t3_test <- bind_cols(previous_Day3 = lag(data_test, 3),
                     previous_Day2 = lag(data_test, 2),
                     previous_Day1 = lag(data_test, 1),
                     t3expected = data_test)

# Removing rows with missing values
t3_train <- t3_train[complete.cases(t3_train), ]
t3_test <- t3_test[complete.cases(t3_test), ]

# Displaying the first few rows of the training dataset with lagged features (3 lags)
head(t3_train)

# Displaying the first few rows of the testing dataset with lagged features (3 lags)
head(t3_test)

# For 1 lagged value
modelt1 <- neuralnet(t1_expected ~ previous_Day1, hidden = c(2, 3),
                     data = t1_train, linear.output = TRUE)

# Plotting the neural network model for the 1 lag time horizon
plot(modelt1)

# Displaying the result matrix of the trained neural network model
modelt1$result.matrix

# Making predictions on the test set using the trained model
resultt1 <- compute(modelt1, t1_test[1])

# Extracting normalized predictions from the neural network output
norm_predictiont1 <- resultt1$net.result

# Denormalizing the normalized predictions to obtain actual values
denorm_predictiont1 <- unnormalize(norm_predictiont1, min(dataset), max(dataset))
denorm_predictiont1 <- round(denorm_predictiont1, 4)

# Denormalizing the test set to obtain actual values
denorm_resultt1 <- unnormalize(t1_test, min(dataset), max(dataset))
denorm_resultt1 <- round(denorm_resultt1, 4)

# Creating a summary table of expected and predicted values
summaryt1 <- cbind(denorm_resultt1[2], denorm_predictiont1)
colnames(summaryt1) <- c("Expected_T1", "Prediction1")

# Displaying the summary table
summaryt1

# Evaluation metrics for t1 lagged values
rmse_t1 <- sqrt(mean((summaryt1$Expected_T1 - summaryt1$Prediction1)^2))
mae_t1 <- mean(abs(summaryt1$Expected_T1 - summaryt1$Prediction1))
mape_t1 <- mean(abs((summaryt1$Expected_T1 - summaryt1$Prediction1) / summaryt1$Expected_T1)) * 100
smape_t1 <- mean(2 * abs(summaryt1$Expected_T1 - summaryt1$Prediction1) / (abs(summaryt1$Expected_T1) + abs(summaryt1$Prediction1))) * 100

# Print evaluation metrics for t1 lagged values
cat("RMSE:", rmse_t1, "\n")
cat("MAE:", mae_t1, "\n")
cat("MAPE:", mape_t1, "\n")
cat("s-MAPE:", smape_t1, "\n")

# For 2 lagged values
modelt2 <- neuralnet(t2_expected ~ previous_Day1 + previous_Day2, hidden = 2,
                     data = t2_train, linear.output = TRUE)

# Plotting the neural network model for the 2 lag time horizon
plot(modelt2)

# Displaying the result matrix of the trained neural network model for 2 lags
modelt2$result.matrix

# Making predictions on the test set using the trained model for 2 lags
resultt2 <- compute(modelt2, t2_test[1:2])

# Extracting normalized predictions from the neural network output
norm_predictiont2 <- resultt2$net.result

# Denormalizing the normalized predictions to obtain actual values
denorm_predictiont2 <- unnormalize(norm_predictiont2, min(dataset), max(dataset))
denorm_predictiont2 <- round(denorm_predictiont2, 4)

# Denormalizing the test set to obtain actual values
denorm_resultt2 <- unnormalize(t2_test, min(dataset), max(dataset))
denorm_resultt2 <- round(denorm_resultt2, 4)

# Creating a summary table of expected and predicted values for 2 lags
summaryt2 <- cbind(denorm_resultt2[2], denorm_predictiont2)
colnames(summaryt2) <- c("Expected_T2", "Prediction2")

# Displaying the summary table for 2 lags
summaryt2

# Evaluation metrics for t2 lagged values
rmse_t2 <- sqrt(mean((summaryt2$Expected_T2 - summaryt2$Prediction2)^2))
mae_t2 <- mean(abs(summaryt2$Expected_T2 - summaryt2$Prediction2))
mape_t2 <- mean(abs((summaryt2$Expected_T2 - summaryt2$Prediction2) / summaryt2$Expected_T2)) * 100
smape_t2 <- mean(2 * abs(summaryt2$Expected_T2 - summaryt2$Prediction2) / (abs(summaryt2$Expected_T2) + abs(summaryt2$Prediction2))) * 100

# Print evaluation metrics for t2 lagged values
cat("RMSE:", rmse_t2, "\n")
cat("MAE:", mae_t2, "\n")
cat("MAPE:", mape_t2, "\n")
cat("s-MAPE:", smape_t2, "\n")

# For 3 lagged values
modelt3 <- neuralnet(t3_expected ~ previous_Day1 + previous_Day2 + previous_Day3, hidden = c(4, 5),
                     data = t3_train, linear.output = TRUE)

# Plotting the neural network model for the 3 lag time horizon
plot(modelt3)

# Displaying the result matrix of the trained neural network model for 3 lags
modelt3$result.matrix


# Making predictions on the test set using the trained model for 3 lags
resultt3 <- compute(modelt3, t3_test[1:3])

# Extracting normalized predictions from the neural network output
norm_predictiont3 <- resultt3$net.result

# Denormalizing the normalized predictions to obtain actual values
denorm_predictiont3 <- unnormalize(norm_predictiont3, min(dataset), max(dataset))
denorm_predictiont3 <- round(denorm_predictiont3, 4)

# Denormalizing the test set to obtain actual values
denorm_resultt3 <- unnormalize(t3_test, min(dataset), max(dataset))
denorm_resultt3 <- round(denorm_resultt3, 4)

# Creating a summary table of expected and predicted values for 3 lags
summaryt3 <- cbind(denorm_resultt3[2], denorm_predictiont3)
colnames(summaryt3) <- c("Expected_T3", "Prediction3")

# Displaying the summary table for 3 lags
summaryt3

# Evaluation metrics for t3 lagged values
rmse_t3 <- sqrt(mean((summaryt3$Expected_T3 - summaryt3$Prediction3)^2))
mae_t3 <- mean(abs(summaryt3$Expected_T3 - summaryt3$Prediction3))
mape_t3 <- mean(abs((summaryt3$Expected_T3 - summaryt3$Prediction3) / summaryt3$Expected_T3)) * 100
smape_t3 <- mean(2 * abs(summaryt3$Expected_T3 - summaryt3$Prediction3) / (abs(summaryt3$Expected_T3) + abs(summaryt3$Prediction3))) * 100

# Print evaluation metrics for t3 lagged values
cat("RMSE:", rmse_t3, "\n")
cat("MAE:", mae_t3, "\n")
cat("MAPE:", mape_t3, "\n")
cat("s-MAPE:", smape_t3, "\n")

# Scatter Plot of predicted vs. expected values
par(mfrow = c(1, 1))
plot(summaryt2$Expected_T2, summaryt2$Prediction2, col = 'red', main = 'Real vs predicted NN', pch = 18, cex = 0.7, xlab = "Expected", ylab = "Predicted")
abline(a = 0, b = 1, h = 90, v = 90)

# Line chart of predicted vs. expected values
x <- 1:length(summaryt2$Expected_T2)
plot(x, summaryt2$Expected_T2, col = "red", type = "l", lwd = 2,
     main = "exchange rate prediction")
lines(x, summaryt2$Prediction2, col = "blue", lwd = 2)
legend("topleft", legend = c("original-rate", "predicted-rate"),
       fill = c("red", "blue"), col = 2:3, adj = c(0, 0.6))
grid()
