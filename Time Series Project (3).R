#loading the dataset 
install.packages("xts")
install.packages("corrplot")
install.packages("car")
install.packages("gridExtra")
install.packages("e1071")
install.packages("glmnet")
library(car)
library(xts)
library(zoo)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(e1071)
library(dplyr)
library(tseries)
library(forecast)
library(glmnet)
gold_data <- read.csv("gold_price_data.csv")
gold_data
head(gold_data)
str(gold_data)
# Checking out any missing values in the data 
colSums(is.na(gold_data)) #[1] 0

# Converting the date of gold prices into required format 
gold_data$Date <- as.Date(gold_data$Date, format="%m/%d/%Y")
gold_data

# Checking out the correlation between the column 
#  If two or more columns are correlated with each other and none of them is a target variable 
# then we must use a method to remove this correlation. 
# Remove the Date column
gold_num <- gold_data[ , !(names(gold_data) %in% c("Date")) ]
gold_num
#Compute correlation matrix
correlation_matrix <- cor(gold_num)
correlation_matrix
#            SPX         GLD        USO        SLV     EUR.USD
#SPX      1.00000000  0.04934504 -0.5915726 -0.2740547 -0.67201742
#GLD      0.04934504  1.00000000 -0.1863602  0.8666319 -0.02437547
#USO     -0.59157260 -0.18636016  1.0000000  0.1675471  0.82931745
#SLV     -0.27405473  0.86663188  0.1675471  1.0000000  0.32163127
#EUR.USD -0.67201742 -0.02437547  0.8293175  0.3216313  1.00000000
corrplot(correlation_matrix, method = "color",type = "upper", addCoef.col = "black",tl.col = "black",number.cex=0.7,tl.cex=0.8) 
# Dropping a column usually makes sense if:
#It is highly correlated (multicollinearity) with another predictor and you plan to build a model.
#Here:
#GLD & SLV correlation ≈ 0.87 → high, so they carry overlapping information.

# Checking Multicollinearity
model <- lm(GLD ~ SLV + SPX + USO + EUR.USD, data=gold_data)
vif(model)
#  SLV      SPX      USO  EUR.USD 
#1.167741 1.854727 3.353192 4.128340 
# SINCE ALL ARE LESS THAN 5 , THEN THERE IS NO MULTICOLLINEARITY IN THE DATASET

# Also since correlation between SPX and EUR.USD to predict GLD is low , almost 
# negligible By principle of Parsimony , we will drop these column to simplify 
# our model
summary(model)
# DROPPING TWO COLUMNS 
gold_data_new <- gold_data[ , !(names(gold_data) %in% c("SPX", "EUR.USD")) ]
gold_data_new

# Now converting the dataset into time series 
gold_data <- xts(gold_data_new$GLD, order.by=gold_data_new$Date)
gold_data
plot(gold_data, main="Plot of Gold Prices", xlab="Time", ylab="GLD Prices")
# Now for measurement of Trend we will use Method of Moving  Averages 
# It is also known as rolling means method 

# for measurement of trend and smoothing of data 
# apply rolling mean with window size of 3

# Apply rolling means with  30 window sizes
gold_ma30  <- rollmean(gold_data, k=30, fill=NA)
gold_ma30 
plot(gold_ma30, main="Trend in price of gold through date", xlab="Time" )
# Plot the original gold prices
plot(gold_data, main="Gold Prices with Rolling Means", 
     xlab="Time", ylab="GLD Price", col="black", lwd=1)
# Add rolling means
lines(gold_ma30, col="red", lwd=2)

# plotting histograms + density curves for all numeric columns except Date:
# to check distribution of Data across the columns 
# Remove the Date column (non-numeric)
temp_cols <- setdiff(names(gold_data_new), "Date")
temp_cols # "GLD" "USO" "SLV"
#Create a list to store plots
plot_list <- list()
plot_list
for (i in temp_cols) {
  p <- ggplot(gold_data_new, aes_string(x=i)) +
    geom_histogram(aes(y=..density..), fill="skyblue", color="black", bins=30, alpha=0.7) +
    geom_density(color="red", size=1) +
    labs(title=paste("Distribution of", i), x=i, y="Density") +
    theme_minimal()
  plot_list[[i]] <- p
}
grid_plot <- grid.arrange(grobs=plot_list, nrow=2, ncol=3,
                          top="Distribution of data across columns")
# to check the skewness of data 
skewness_values <- sapply(gold_data_new[temp_cols], skewness, na.rm=TRUE)
skewness_values
# GLD       USO       SLV 
#0.3337007 1.6971054 1.1521300 

# Now applying log transformation to reduce skewness 
gold_data_new$USO_log <- log1p(gold_data_new$USO)
gold_data_new$SLV_log <- log1p(gold_data_new$SLV)
# Rechecking out the skewness 
skewness_values_after <- sapply(gold_data_new[c("USO_log", "SLV_log")], skewness, na.rm=TRUE)
skewness_values_after

# Now Re plotting to get Visual of distribution of data 
# USO_log
ggplot(gold_data_new, aes(x=USO_log)) +
  geom_histogram(aes(y=..density..), bins=30, fill="skyblue", color="black", alpha=0.7) +
  geom_density(color="red", size=1) +
  ggtitle("Distribution of USO after log transform")

# SLV_log
ggplot(gold_data_new, aes(x=SLV_log)) +
  geom_histogram(aes(y=..density..), bins=30, fill="skyblue", color="black", alpha=0.7) +
  geom_density(color="red", size=1) +
  ggtitle("Distribution of SLV after log transform")

# HANDLING OUTLIERS 
# Function to detect outliers (before capping)
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm=TRUE)
  Q3 <- quantile(x, 0.75, na.rm=TRUE)
  IQR_value <- IQR(x, na.rm=TRUE)
  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value
  which(x < lower | x > upper)
}

# Columns to check
cols_to_check <- c("GLD", "USO_log", "SLV_log")

# Detect and count
outliers_list <- lapply(gold_data_new[cols_to_check], detect_outliers)
outliers_count_before <- sapply(outliers_list, length)
print(outliers_count_before)
# GLD USO_log SLV_log 
# 115       0      20 
for (col in cols_to_check) {
  p <- ggplot(gold_data_new, aes_string(y=col)) +
    geom_boxplot(fill="skyblue") +
    labs(title=paste("Boxplot of", col, "before removing outliers")) +
    theme_minimal()
  print(p)
}
cap_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm=TRUE)
  Q3 <- quantile(x, 0.75, na.rm=TRUE)
  IQR_value <- IQR(x, na.rm=TRUE)
  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

# Apply capping
gold_data_capped <- gold_data_new
for (col in cols_to_check) {
  gold_data_capped[[col]] <- cap_outliers(gold_data_capped[[col]])
}

outliers_list_after <- lapply(gold_data_capped[cols_to_check], detect_outliers)
outliers_count_after <- sapply(outliers_list_after, length)
print(outliers_count_after) 

#  GLD USO_log SLV_log 
#0       0       0 

for (col in cols_to_check) {
  p <- ggplot(gold_data_capped, aes_string(y=col)) +
    geom_boxplot(fill="lightgreen") +
    labs(title=paste("Boxplot of", col, "after capping outliers")) +
    theme_minimal()
  print(p)
}

# # Drop the original skewed columns
gold_data_new1 <- gold_data_capped[, !(names(gold_data_capped) %in% c("USO", "SLV"))]
gold_data_new1
gold_data1 <- xts(gold_data_new1$GLD, order.by=gold_data_new1$Date)
gold_data1
plot(gold_data1, main="Plot of Gold Prices", xlab="Time", ylab="GLD Prices")


# Since our data after cleaning has become small so we will aggregate this 
# into monthly data 
gold_monthly <- gold_data_new1 %>%mutate(month=as.yearmon(Date)) %>%group_by(month) %>%summarise(GLD=mean(GLD))
head(gold_monthly )
# Create ts object: frequency=12 (monthly)
gold_monthly_ts <- ts(gold_monthly$GLD, start=c(2008,1), frequency=12)
plot(gold_monthly_ts)
adf.test(gold_monthly_ts)

#Augmented Dickey-Fuller Test

#data:  gold_monthly_ts
#Dickey-Fuller = -1.5654, Lag order = 4, p-value = 0.7571
#alternative hypothesis: stationary
# Ho:- The Time series has a unit root(i.e.,it is non-stationary)
# H1:-  The Time series has a non unit root(i.e.,it is stationary)

# ALSO applying PP.test 
PP.test(gold_monthly_ts )
#Phillips-Perron Unit Root Test
#data:  gold_monthly_ts
#Dickey-Fuller = -1.4544, Truncation lag parameter = 4, p-value = 0.8033


# DECOMPOSING THE TIME SERIES 
gold_stl <- stl(gold_monthly_ts, s.window="periodic")
plot(gold_stl)
acf(gold_monthly_ts , main =" ACF OF GOLD MONTHLY DATA", xlab="Lag", ylab ="Monthly Time Series Values")
pacf(gold_monthly_ts , main ="PACF OF GOLD MONTHLY DATA ", xlab ="Lag", ylab  ="Monthly Time Series Values" )

# Seasonal differencing (lag=12)
gold_diff_seasonal <- diff(gold_monthly_ts, lag=12)
plot(gold_diff_seasonal, main="After seasonal differencing (lag=12)")

# First difference to remove trend
gold_diff_final <- diff(gold_diff_seasonal, differences=1)
plot(gold_diff_final, main="After seasonal + first differencing")

# Check stationarity
adf.test(na.omit(gold_diff_final))
#Augmented Dickey-Fuller Test

#data:  na.omit(gold_diff_final)
#Dickey-Fuller = -3.7483, Lag order = 4, p-value = 0.02397
#alternative hypothesis: stationary
# p-value < 0.05 → data is now stationary.
PP.test(gold_diff_final)
#Phillips-Perron Unit Root Test

#data:  gold_diff_final
#Dickey-Fuller = -8.889, Truncation lag parameter = 4, p-value = 0.01
# Fitting of the model 

fit_model <- auto.arima(gold_monthly_ts, stepwise=FALSE, approximation=FALSE)
summary(fit_model)

#Series: gold_monthly_ts 
#ARIMA(0,1,1) 

#Coefficients:
#  ma1
#0.2368
#s.e.  0.0862

#sigma^2 = 16.96:  log likelihood = -350.99
#AIC=705.98   AICc=706.08   BIC=711.62

#Training set error measures:
#  ME     RMSE      MAE       MPE     MAPE      MASE        ACF1
#Training set 0.2289691 4.085259 3.349196 0.1718651 2.808417 0.2136145 0.001599452


# Diagnostic plots
checkresiduals(fit_model)

# TO CHECK RESIDUALS from your fitted model resemble white noise

Box.test(residuals(fit_model), lag=log(length(residuals(fit_model))), type="Ljung-Box", fitdf =1)
#Box-Ljung test
#data:  residuals(fit_model)
#X-squared = 0.27633, df = 3, p-value = 0.9644

#If p-value > 0.05 → residuals look like white noise → model is adequate.

tsdiag(fit_model)  # Residuals roughly behave like white noise.
#Residuals are mostly uncorrelated — good sign that ARIMA captured the serial dependence.
#Model residuals are approximately white noise; the ARIMA model seems adequate.
# FORECASTING 
fit <- predict(fit_model , n.ahead = 12*2)
p <- fit$pred
p
plot(gold_monthly_ts , xlim=c(2008, 2020+0.5))
lines(p,col="red", lwd=3, type="l")
time <- 2 * 12 
gold_forecast <- forecast(fit_model, h=time)
gold_forecast
# Plot forecast
plot(gold_forecast, main="Forecast of Gold Prices for Next 2 Years")

# APPLYING ML
# Lasso & Ridge regression (regularized linear models) to predict GLD using other variables (USO_log, SLV_log, etc.).

# Prepare data for ML
#Split into train/test.
#Since this is time series, we’ll split by date (earlier data for training, later data for testing).

# # Ensure data is sorted by Date
gold_data_new1 <- gold_data_new1[order(gold_data_new1$Date), ]
gold_data_new1
head(gold_data_new1)
#      Date   GLD  USO_log  SLV_log
#1 2008-01-02 84.86 4.375380 2.783776
#2 2008-01-03 85.57 4.374121 2.790244
#3 2008-01-04 85.13 4.360675 2.782972
#4 2008-01-07 84.77 4.337291 2.775896
#5 2008-01-08 86.78 4.344584 2.808800
#6 2008-01-09 86.55 4.334017 2.804572


# Define index to split ~80% train, 20% test
split_idx <- floor(0.8 * nrow(gold_data_new1))
split_idx  #  1832 

train_data <- gold_data_new1[1:split_idx, ]
train_data
head(train_data)
#Date   GLD  USO_log  SLV_log
#1 2008-01-02 84.86 4.375380 2.783776
#2 2008-01-03 85.57 4.374121 2.790244
#3 2008-01-04 85.13 4.360675 2.782972
#4 2008-01-07 84.77 4.337291 2.775896
#5 2008-01-08 86.78 4.344584 2.808800
#6 2008-01-09 86.55 4.334017 2.804572
test_data  <- gold_data_new1[(split_idx+1):nrow(gold_data_new1), ]
test_data
head(test_data)
# Date    GLD  USO_log  SLV_log
#1833 2016-04-14 117.11 2.440606 2.793616
#1834 2016-04-18 117.74 2.413232 2.798500
#1835 2016-04-19 119.58 2.437116 2.840247
#1836 2016-04-20 118.97 2.464704 2.841415
#1837 2016-04-21 119.42 2.461297 2.845491
#1838 2016-04-25 118.23 2.446685 2.843164
# Check sizes
nrow(train_data)  # 1832 = 80% rows
nrow(test_data)   # 458 = 20% rows

# Select predictors & target
x_train <- as.matrix(train_data[, c("USO_log", "SLV_log")])
head(x_train)
# USO_log  SLV_log
#1 4.375380 2.783776
#2 4.374121 2.790244
#3 4.360675 2.782972
#4 4.337291 2.775896
#5 4.344584 2.808800
#6 4.334017 2.804572
y_train <- train_data$GLD
head(y_train)
x_test <- as.matrix(test_data[, c("USO_log", "SLV_log")])
y_test <- test_data$GLD
head(x_test)
# USO_log  SLV_log
#1833 2.440606 2.793616
#1834 2.413232 2.798500
#1835 2.437116 2.840247
#1836 2.464704 2.841415
#1837 2.461297 2.845491
#1838 2.446685 2.843164
head(y_test)  #117.11 117.74 119.58 118.97 119.42 118.23 

#train Ridge regression
#α=0 → Ridge
set.seed(123)
# cv.glmnet does cross-validation to choose lambda
ridge_model <- cv.glmnet(x_train, y_train, alpha=0)
ridge_model

#cv.glmnet(x = x_train, y = y_train, alpha = 0) 
#Measure: Mean-Squared Error 

#Lambda Index Measure    SE Nonzero
#min  2.322   100   66.25 1.901       2
#1se  2.548    99   67.18 1.884       2


# Best lambda
ridge_lambda <- ridge_model$lambda.min
print(ridge_lambda)  #2.32164

# Plot cross-validation curve
plot(ridge_model)

#Predict and evaluate Ridge
ridge_pred <- predict(ridge_model, s=ridge_lambda, newx=x_test)
ridge_pred
# Evaluation
ridge_rmse <- sqrt(mean((ridge_pred - y_test)^2))
ridge_rmse # 6.051019
ridge_mae  <- mean(abs(ridge_pred - y_test))
ridge_mae # 5.311087

lasso_model <- cv.glmnet(x_train, y_train, alpha=1)
lasso_model
#Call:  cv.glmnet(x = x_train, y = y_train, alpha = 1) 
#Measure: Mean-Squared Error 

#Lambda Index Measure    SE Nonzero
#min 0.1053    59   61.17 2.039       2
#1se 0.8947    36   63.16 1.990       2
lasso_lambda <- lasso_model$lambda.min
lasso_lambda # 0.1052835

plot(lasso_model)

lasso_pred <- predict(lasso_model, s=lasso_lambda, newx=x_test)
lasso_pred
lasso_rmse <- sqrt(mean((lasso_pred - y_test)^2))
lasso_rmse #  6.706304
lasso_mae  <- mean(abs(lasso_pred - y_test))
lasso_mae #  5.97625


results_df <- data.frame(
  Date = test_data$Date,
  Actual = y_test,
  Ridge_Pred = as.numeric(ridge_pred),
  Lasso_Pred = as.numeric(lasso_pred)
)


ggplot() +
  geom_line(data=results_df, aes(x=Date, y=Actual, color="Actual", linetype="Actual"), size=1) +
  geom_line(data=results_df, aes(x=Date, y=Ridge_Pred, color="Ridge", linetype="Ridge"), size=1) +
  geom_line(data=results_df, aes(x=Date, y=Lasso_Pred, color="Lasso", linetype="Lasso"), size=1) +
  scale_color_manual(values=c("Actual"="black", "Ridge"="blue", "Lasso"="red")) +
  scale_linetype_manual(values=c("Actual"="solid", "Ridge"="dashed", "Lasso"="dotted")) +
  labs(title="Actual vs Ridge & Lasso Predictions", y="GLD Price", x="Date") +
  theme_minimal() +
  theme(legend.title=element_blank())

# Comparison 
model_comparison <- data.frame(
  Model = c("ARIMA", "Ridge", "Lasso"),
  RMSE = c( round(accuracy(fit_model)[2],2), round(ridge_rmse,2), round(lasso_rmse,2) ),
  MAE  = c( round(accuracy(fit_model)[3],2), round(ridge_mae,2), round(lasso_mae,2) )
)
print(model_comparison)

#   Model RMSE  MAE
#1 ARIMA 4.09 3.35
#2 Ridge 6.05 5.31
#3 Lasso 6.71 5.98



#ARIMA	Ridge & Lasso
#RMSE & MAE (both) are lower	higher	
#Better at predicting gold price’s future time series values	worse at predicting actual GLD compared to ARIMA	

#Interpretation:
# The ARIMA model, which is built on the time series itself, captured the trend & seasonality better and produced more accurate forecasts (lower RMSE and MAE).
#Ridge and Lasso, which tried to predict GLD from other variables (USO_log, SLV_log), had noticeably higher errors.

































































































































































