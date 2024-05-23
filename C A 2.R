#loading the data
library(readxl)
data <- read_excel("Dataset_2024.xlsx")

#Display first few rows of the dataset
head(data)

# Check the current column names
colnames(data)

# Rename columns forease of use 
colnames(data) <- c("Age", "BodyFat", "Chest", "Density", "Knee", "Weight")

# confirm the changes
colnames(data)
head(data)

# Check the data types of each column
str(data)

#DATA PREPROCESSING 

# Check for missing values
sum(is.na(data))

#Handling missing values
data <- na.omit(data)

#See a Descriptive statistics of the data
summary_stats <- summary(data)
summary_stats

#Perform Linearity checks on data
#Using scatter pplot
windows(20,10)
# BodyFat vs Age
ggplot(data, aes(x=Age, y=BodyFat)) +
  geom_point() +
  geom_smooth(method="lm", col="red") +
  ggtitle("Body Fat vs Age") +
  xlab("Age (years)") +
  ylab("Body Fat (%)")

windows(20,10)
# BodyFat vs Chest
ggplot(data, aes(x=Chest, y=BodyFat)) +
  geom_point() +
  geom_smooth(method="lm", col="red") +
  ggtitle("Body Fat vs Chest Circumference") +
  xlab("Chest Circumference (cm)") +
  ylab("Body Fat (%)")

# BodyFat vs Density
windows(20,10)
ggplot(data, aes(x=Density, y=BodyFat)) +
  geom_point() +
  geom_smooth(method="lm", col="red") +
  ggtitle("Body Fat vs Density") +
  xlab("Density (g/cm³)") +
  ylab("Body Fat (%)")

# BodyFat vs Knee
windows(20,10)
ggplot(data, aes(x=Knee, y=BodyFat)) +
  geom_point() +
  geom_smooth(method="lm", col="red") +
  ggtitle("Body Fat vs Knee Circumference") +
  xlab("Knee Circumference (cm)") +
  ylab("Body Fat (%)")

# BodyFat vs Weight
windows(20,10)
ggplot(data, aes(x=Weight, y=BodyFat)) +
  geom_point() +
  geom_smooth(method="lm", col="red") +
  ggtitle("Body Fat vs Weight") +
  xlab("Weight (lbs)") +
  ylab("Body Fat (%)")

#check for corelation
# Print correlation coefficients
# Calculate correlation coefficients
cor_age <- cor(data$Age, data$BodyFat)
cor_chest <- cor(data$Chest, data$BodyFat)
cor_density <- cor(data$Density, data$BodyFat)
cor_knee <- cor(data$Knee, data$BodyFat)
cor_weight <- cor(data$Weight, data$BodyFat)

# Print correlation coefficients
cat("Correlation between Body Fat and Age: ", cor_age, "\n")
cat("Correlation between Body Fat and Chest Circumference: ", cor_chest, "\n")
cat("Correlation between Body Fat and Density: ", cor_density, "\n")
cat("Correlation between Body Fat and Knee Circumference: ", cor_knee, "\n")
cat("Correlation between Body Fat and Weight: ", cor_weight, "\n")

#Boxplots
# Boxplot for BodyFat
windows(20,10)
ggplot(data, aes(y=BodyFat)) +
  geom_boxplot() +
  ggtitle("Boxplot of Body Fat") +
  ylab("Body Fat (%)")

# Boxplot for Age
windows(20,10)
ggplot(data, aes(y=Age)) +
  geom_boxplot() +
  ggtitle("Boxplot of Age") +
  ylab("Age (years)")

# Boxplot for Chest Circumference
windows(20,10)
ggplot(data, aes(y=Chest)) +
  geom_boxplot() +
  ggtitle("Boxplot of Chest Circumference") +
  ylab("Chest Circumference (cm)")

# Boxplot for Density
windows(20,10)
ggplot(data, aes(y=Density)) +
  geom_boxplot() +
  ggtitle("Boxplot of Density") +
  ylab("Density (g/cm³)")

# Boxplot for Knee Circumference
windows(20,10)
ggplot(data, aes(y=Knee)) +
  geom_boxplot() +
  ggtitle("Boxplot of Knee Circumference") +
  ylab("Knee Circumference (cm)")

# Boxplot for Weight
windows(20,10)
ggplot(data, aes(y=Weight)) +
  geom_boxplot() +
  ggtitle("Boxplot of Weight") +
  ylab("Weight (lbs)")

#We observe the boxplots to identify:
  #The central tendency (median)
#Spread of the data (interquartile range)
#Presence of outliers

# Define function to remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Apply the function to remove outliers
data$BodyFat <- remove_outliers(data$BodyFat)
data$Age <- remove_outliers(data$Age)
data$Chest <- remove_outliers(data$Chest)
data$Density <- remove_outliers(data$Density)
data$Knee <- remove_outliers(data$Knee)
data$Weight <- remove_outliers(data$Weight)

# Remove rows with NA values after outlier removal
data <- na.omit(data)

# Histogram for Chest Circumference
ggplot(data, aes(x=Chest)) +
  geom_histogram(binwidth=5, fill="blue", color="black", alpha=0.7) +
  ggtitle("Histogram of Chest Circumference") +
  xlab("Chest Circumference (cm)") +
  ylab("Frequency")

# Histogram for Density
ggplot(data, aes(x=Density)) +
  geom_histogram(binwidth=0.01, fill="blue", color="black", alpha=0.7) +
  ggtitle("Histogram of Density") +
  xlab("Density (g/cm³)") +
  ylab("Frequency")

# Histogram for Knee Circumference
ggplot(data, aes(x=Knee)) +
  geom_histogram(binwidth=5, fill="blue", color="black", alpha=0.7) +
  ggtitle("Histogram of Knee Circumference") +
  xlab("Knee Circumference (cm)") +
  ylab("Frequency")

# Histogram for Weight
ggplot(data, aes(x=Weight)) +
  geom_histogram(binwidth=10, fill="blue", color="black", alpha=0.7) +
  ggtitle("Histogram of Weight") +
  xlab("Weight (lbs)") +
  ylab("Frequency")

# Calculate skewness for each variable
library(e1071)
windows(20,10)
skewness_age <- skewness(data$Age, na.rm = TRUE)
skewness_bodyfat <- skewness(data$BodyFat, na.rm = TRUE)
skewness_chest <- skewness(data$Chest, na.rm = TRUE)
skewness_density <- skewness(data$Density, na.rm = TRUE)
skewness_knee <- skewness(data$Knee, na.rm = TRUE)
skewness_weight <- skewness(data$Weight, na.rm = TRUE)


# Print skewness values
cat("Skewness of Age: ", skewness_age, "\n")
cat("Skewness of Body Fat: ", skewness_bodyfat, "\n")
cat("Skewness of Chest Circumference: ", skewness_chest, "\n")
cat("Skewness of Density: ", skewness_density, "\n")
cat("Skewness of Knee Circumference: ", skewness_knee, "\n")
cat("Skewness of Weight: ", skewness_weight, "\n")

# Calculate the correlation matrix
install.packages("corrplot")
library(corrplot)
cor_matrix <- cor(data, use="complete.obs")

# Print the correlation matrix
print(cor_matrix)

# Visualize the correlation matrix
corrplot(cor_matrix, method="circle", type="upper", tl.col="black", tl.srt=45)
#"High correlation between predictors may indicate multicollinearity.

# Identify high multicollinearity and decide to remove 'Density'
# Remove the highly correlated variable 'Density'
data_reduced <- data[, !colnames(data) %in% "Density"]

# Display the first few rows of the modified dataset
head(data_reduced)
# Recalculate the correlation matrix
cor_matrix_reduced <- cor(data_reduced, use="complete.obs")
print(cor_matrix_reduced)

# Visualize the reduced correlation matrix
corrplot(cor_matrix_reduced, method="circle", type="upper", tl.col="black", tl.srt=45)
#Identified high multicollinearity between Density and BodyFat.
#Removed the highly correlated variable (Density) from the dataset.
#Recalculated and visualized the correlation matrix to ensure multicollinearity is reduced.
#Interpreted the new correlation matrix to confirm the absence of high multicollinearity.

# Perform Shapiro-Wilk test for each variable in data_reduced
shapiro_age <- shapiro.test(data_reduced$Age)
shapiro_bodyfat <- shapiro.test(data_reduced$BodyFat)
shapiro_chest <- shapiro.test(data_reduced$Chest)
shapiro_knee <- shapiro.test(data_reduced$Knee)
shapiro_weight <- shapiro.test(data_reduced$Weight)

# Print the Shapiro-Wilk test results
cat("Shapiro-Wilk Test Results:\n")
cat("Age: W = ", shapiro_age$statistic, ", p-value = ", shapiro_age$p.value, "\n")
cat("Body Fat: W = ", shapiro_bodyfat$statistic, ", p-value = ", shapiro_bodyfat$p.value, "\n")
cat("Chest: W = ", shapiro_chest$statistic, ", p-value = ", shapiro_chest$p.value, "\n")
cat("Knee: W = ", shapiro_knee$statistic, ", p-value = ", shapiro_knee$p.value, "\n")
cat("Weight: W = ", shapiro_weight$statistic, ", p-value = ", shapiro_weight$p.value, "\n")

# Q-Q plots for each variable in the reduced dataset
library(car)
# Increase the size of the plotting window in RStudio
options(repr.plot.width=8, repr.plot.height=8)

# Reset plotting parameters
par(mfrow=c(1,1))
par(mar=c(5, 5, 4, 2) + 0.1)

qqPlot(data_reduced$Age, main="Q-Q Plot of Age")
qqPlot(data_reduced$BodyFat, main="Q-Q Plot of BodyFat")
qqPlot(data_reduced$Chest, main="Q-Q Plot of Chest")
qqPlot(data_reduced$Knee, main="Q-Q Plot of Knee")
qqPlot(data_reduced$Weight, main="Q-Q Plot of Weight")

# Fitting the regression model
full_model <- lm(BodyFat ~ Age + Chest + Knee + Weight, data=data_reduced)

# Summarize the model
summary(full_model)

# Display model summary
model_summary <- summary(full_model)

# Extract and print Adjusted R², Intercept, regression coefficients, and p-values
adjusted_r2 <- model_summary$adj.r.squared
intercept <- model_summary$coefficients[1, ]
coefficients <- model_summary$coefficients[-1, ]

cat("Adjusted R²: ", adjusted_r2, "\n")
cat("Intercept: Estimate = ", intercept[1], ", Std. Error = ", intercept[2], ", t-value = ", intercept[3], ", p-value = ", intercept[4], "\n")

cat("Regression Coefficients:\n")
print(coefficients)

# Calculate VIF for each predictor
vif_values <- vif(full_model)

# Print VIF values
cat("Variance Inflation Factor (VIF) for each predictor:\n")
print(vif_values)

