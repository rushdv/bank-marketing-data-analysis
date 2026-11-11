
# ============================================================
# 1. LOAD AND INSPECT THE DATASET
# ============================================================

library(dplyr)

bank <- read.csv(
  file.choose(),
  sep = ";",
  stringsAsFactors = FALSE
)

View(bank)
str(bank)
head(bank)
dim(bank)


# ============================================================
# 2. DELETE VALUES AND HANDLE MISSING VALUES
# ============================================================

# Select required features
bank <- bank %>%
  select(
    age, job, marital, education, default,
    balance, housing, loan, contact, day,
    month, duration, campaign, pdays,
    previous, poutcome, y
  )

# Save selected dataset
write.csv(bank, "bank_selected.csv", row.names = FALSE)

file.exists("bank_selected.csv")
getwd()


# 2.1 Define numerical and categorical features

numeric_features <- c(
  "age", "balance", "day", "duration",
  "campaign", "pdays", "previous"
)

categorical_features <- c(
  "job", "marital", "education", "default",
  "housing", "loan", "contact", "month",
  "poutcome", "y"
)


# 2.2 Clean categorical values

for (col in categorical_features) {
  bank[[col]] <- tolower(trimws(bank[[col]]))
}

# Convert unknown values into NA
# In this dataset, unknown means information is unavailable.

missing_categories <- c(
  "job", "education", "contact", "poutcome"
)

for (col in missing_categories) {
  bank[[col]][bank[[col]] == "unknown"] <- NA
}


# 2.3 Check existing missing values

colSums(is.na(bank))

missing_percentage <- round(
  colSums(is.na(bank)) / nrow(bank) * 100,
  2
)

missing_percentage


# 2.4 Artificially introduce missing values

missing_data <- bank

missing_data$age[c(2, 5, 8)] <- NA

missing_data$job[c(3, 6, 9)] <- NA

head(missing_data, 10)

colSums(is.na(missing_data))

missing_percentage <- round(
  colSums(is.na(missing_data)) / nrow(missing_data) * 100,
  2
)

missing_percentage

sum(is.na(missing_data))


# 2i. Discard instances with missing values

discard_data <- na.omit(missing_data)

cat("Original Rows:", nrow(missing_data), "\n")
cat("After Removing Missing Rows:", nrow(discard_data), "\n")
cat("Removed Rows:", nrow(missing_data) - nrow(discard_data), "\n")

colSums(is.na(discard_data))

View(discard_data)


# 2ii. Replace missing values using Mean/Median and Mode

replace_data <- missing_data

# Function to calculate mode
get_mode <- function(x) {
  x <- x[!is.na(x)]
  names(which.max(table(x)))
}

# Replace numerical missing values with median
for (col in numeric_features) {
  replace_data[[col]][is.na(replace_data[[col]])] <-
    median(replace_data[[col]], na.rm = TRUE)
}

# Replace categorical missing values with mode
for (col in categorical_features) {
  mode_value <- get_mode(replace_data[[col]])
  
  replace_data[[col]][is.na(replace_data[[col]])] <-
    mode_value
}

# Verify missing values
colSums(is.na(replace_data))

sum(is.na(replace_data))

View(replace_data)

write.csv(discard_data, "bank_discard_data.csv", row.names = FALSE)

write.csv(replace_data, "bank_replaced_data.csv", row.names = FALSE)


# ============================================================
# 3. ADD NOISE VALUES AND HANDLE NOISY DATA
# ============================================================

noise_data <- replace_data


# 3.1 Add numerical noise

noise_data$balance[20] <- 9999999

noise_data$balance[20]


# 3.2 Add categorical noise

noise_data$job[11] <- "ADMIN."
noise_data$job[12] <- " management "
noise_data$job[13] <- "managment"

noise_data$job[11:13]


# 3.3 Handle numerical noise

# Use the median from the clean dataset
median_balance <- median(replace_data$balance)

noise_data$balance[20] <- median_balance

noise_data$balance[20]


# 3.4 Check categorical noise

table(noise_data$job)


# 3.5 Handle categorical noise

noise_data$job <- tolower(trimws(noise_data$job))

# Correct spelling mistake
noise_data$job[noise_data$job == "managment"] <- "management"

unique(noise_data$job)

table(noise_data$job)


# 3.6 Check duplicate records

sum(duplicated(noise_data))


# 3.7 Add a duplicate record

noise_data <- rbind(
  noise_data,
  noise_data[30, ]
)

sum(duplicated(noise_data))


# 3.8 Remove duplicate records

noise_data <- distinct(noise_data)

sum(duplicated(noise_data))

# Final verification
noise_data$balance[20]
noise_data$job[11:13]

colSums(is.na(noise_data))

dim(noise_data)

View(noise_data)

write.csv(noise_data, "bank_noise_cleaned.csv", row.names = FALSE)


# ============================================================
# 4. CHECK INVALID DATA
# ============================================================

invalid_data <- noise_data


# 4.1 Check numerical invalid values

summary(invalid_data[numeric_features])

# Check age
which(invalid_data$age <= 0 | invalid_data$age > 100)

# Check balance
which(invalid_data$balance < -1000000)

# Check day
which(invalid_data$day < 1 | invalid_data$day > 31)

# Check duration
which(invalid_data$duration < 0)

# Check campaign
which(invalid_data$campaign < 1)

# Check previous
which(invalid_data$previous < 0)


# 4.2 Check categorical values

unique(invalid_data$job)
unique(invalid_data$marital)
unique(invalid_data$education)
unique(invalid_data$default)
unique(invalid_data$housing)
unique(invalid_data$loan)
unique(invalid_data$contact)
unique(invalid_data$month)
unique(invalid_data$poutcome)
unique(invalid_data$y)


# 4.3 Check marital status frequencies

table(invalid_data$marital)


# 4.4 Introduce and correct an invalid category

invalid_data$marital[15] <- "maried"

# Check invalid category
unique(invalid_data$marital)

# Correct spelling
invalid_data$marital[
  invalid_data$marital == "maried"
] <- "married"

# Verify
unique(invalid_data$marital)

table(invalid_data$marital)


# 4.5 Final verification

summary(invalid_data)

colSums(is.na(invalid_data))

nrow(invalid_data)

View(invalid_data)

write.csv(invalid_data, "bank_invalid_cleaned.csv", row.names = FALSE)


# ============================================================
# 5. DATA NORMALIZATION AND TRANSFORMATION
# ============================================================

transformed_data <- invalid_data


# 5.1 Min-Max Normalization

transformed_data$Age_Norm <- (
  transformed_data$age - min(transformed_data$age)
) / (
  max(transformed_data$age) - min(transformed_data$age)
)

head(transformed_data$Age_Norm)

min(transformed_data$Age_Norm)
max(transformed_data$Age_Norm)


# 5.2 Standardization

transformed_data$Balance_Std <- as.numeric(
  scale(transformed_data$balance)
)

head(transformed_data$Balance_Std)

mean(transformed_data$Balance_Std)
sd(transformed_data$Balance_Std)


# 5.3 Encoding categorical variables

transformed_data$y <- as.factor(transformed_data$y)

transformed_data$job <- as.factor(transformed_data$job)

transformed_data$marital <- as.factor(transformed_data$marital)

levels(transformed_data$y)
levels(transformed_data$job)
levels(transformed_data$marital)

transformed_data$Risk_Code <- as.numeric(transformed_data$y)

transformed_data$Job_Code <- as.numeric(transformed_data$job)

transformed_data$Marital_Code <- as.numeric(transformed_data$marital)

head(transformed_data[, c(
  "y", "Risk_Code", "job", "Job_Code",
  "marital", "Marital_Code"
)])


# 5.4 Feature Engineering

# Customer age group
transformed_data$Young_Customer <- ifelse(
  transformed_data$age < 30, 1, 0
)

# Whether customer has a positive bank balance
transformed_data$Positive_Balance <- ifelse(
  transformed_data$balance > 0, 1, 0
)

# Whether customer was contacted previously
transformed_data$Previously_Contacted <- ifelse(
  transformed_data$pdays == -1, 0, 1
)

head(transformed_data)


# 5.5 Final verification

str(transformed_data)

head(transformed_data)

dim(transformed_data)

View(transformed_data)

write.csv(
  transformed_data,
  "bank_transformed_data.csv",
  row.names = FALSE
)


# ============================================================
# 6. CHECK DATA OUTLIERS
# ============================================================

outlier_data <- invalid_data


# 6.1 Visualize outliers using boxplots

boxplot(outlier_data$age,
        main = "Age Outliers",
        ylab = "Age")

boxplot(outlier_data$balance,
        main = "Balance Outliers",
        ylab = "Balance")

boxplot(outlier_data$duration,
        main = "Duration Outliers",
        ylab = "Duration")

boxplot(outlier_data$campaign,
        main = "Campaign Outliers",
        ylab = "Campaign")


# 6.2 Detect outliers using IQR method

# Age
Q1_age <- quantile(outlier_data$age, 0.25)
Q3_age <- quantile(outlier_data$age, 0.75)
IQR_age <- IQR(outlier_data$age)

lower_age <- Q1_age - 1.5 * IQR_age
upper_age <- Q3_age + 1.5 * IQR_age

age_outliers <- which(
  outlier_data$age < lower_age |
    outlier_data$age > upper_age
)


# Balance
Q1_balance <- quantile(outlier_data$balance, 0.25)
Q3_balance <- quantile(outlier_data$balance, 0.75)
IQR_balance <- IQR(outlier_data$balance)

lower_balance <- Q1_balance - 1.5 * IQR_balance
upper_balance <- Q3_balance + 1.5 * IQR_balance

balance_outliers <- which(
  outlier_data$balance < lower_balance |
    outlier_data$balance > upper_balance
)


# Duration
Q1_duration <- quantile(outlier_data$duration, 0.25)
Q3_duration <- quantile(outlier_data$duration, 0.75)
IQR_duration <- IQR(outlier_data$duration)

lower_duration <- Q1_duration - 1.5 * IQR_duration
upper_duration <- Q3_duration + 1.5 * IQR_duration

duration_outliers <- which(
  outlier_data$duration < lower_duration |
    outlier_data$duration > upper_duration
)


# Campaign
Q1_campaign <- quantile(outlier_data$campaign, 0.25)
Q3_campaign <- quantile(outlier_data$campaign, 0.75)
IQR_campaign <- IQR(outlier_data$campaign)

lower_campaign <- Q1_campaign - 1.5 * IQR_campaign
upper_campaign <- Q3_campaign + 1.5 * IQR_campaign

campaign_outliers <- which(
  outlier_data$campaign < lower_campaign |
    outlier_data$campaign > upper_campaign
)


# 6.3 Count outliers

length(age_outliers)

length(balance_outliers)

length(duration_outliers)

length(campaign_outliers)

# View outlier records
outlier_data[age_outliers, ]
outlier_data[balance_outliers, ]
outlier_data[duration_outliers, ]
outlier_data[campaign_outliers, ]

dim(outlier_data)

View(outlier_data)

write.csv(outlier_data, "bank_outlier_checked.csv", row.names = FALSE)


# ============================================================
# 7. DESCRIPTIVE STATISTICS AND DATA VISUALIZATION
# ============================================================

# Load required packages
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(corrplot)) {
  install.packages("corrplot")
  library(corrplot)
}

# Use numerical features
numeric_data <- invalid_data %>%
  select(age, balance, day, duration, campaign, pdays, previous)


# 7.1 Numerical descriptive statistics

summary(numeric_data)

# Mean
sapply(numeric_data, mean)

# Median
sapply(numeric_data, median)

# Mode
sapply(numeric_data, function(x) {
  as.numeric(names(which.max(table(x))))
})

# Variance
sapply(numeric_data, var)

# Standard deviation
sapply(numeric_data, sd)

# Range
sapply(numeric_data, range)

# IQR
sapply(numeric_data, IQR)


# 7.2 Histograms using base R

hist(invalid_data$age,
     main = "Age Distribution",
     xlab = "Age",
     col = "lightblue")

hist(invalid_data$balance,
     main = "Balance Distribution",
     xlab = "Balance",
     col = "lightgreen")

hist(invalid_data$duration,
     main = "Duration Distribution",
     xlab = "Duration",
     col = "lightpink")

hist(invalid_data$campaign,
     main = "Campaign Distribution",
     xlab = "Campaign",
     col = "lightyellow")


# 7.3 Histogram using ggplot2

ggplot(invalid_data, aes(x = balance)) +
  geom_histogram(
    bins = 10,
    fill = "skyblue",
    color = "black"
  ) +
  labs(
    title = "Balance Distribution",
    x = "Balance",
    y = "Frequency"
  )


# 7.4 Boxplots using base R

boxplot(invalid_data$age,
        main = "Age Boxplot",
        col = "lightblue")

boxplot(invalid_data$balance,
        main = "Balance Boxplot",
        col = "lightgreen")

boxplot(invalid_data$duration,
        main = "Duration Boxplot",
        col = "lightpink")

boxplot(invalid_data$campaign,
        main = "Campaign Boxplot",
        col = "lightyellow")


# 7.5 Age boxplot with color

boxplot(
  invalid_data$age,
  main = "Age Distribution with Outliers",
  ylab = "Age",
  col = "pink"
)


# 7.6 Boxplot of balance by subscription status

ggplot(invalid_data, aes(x = y, y = balance, fill = y)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Balance by Subscription Status",
    x = "Subscribed to Term Deposit",
    y = "Balance"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# 7.7 Scatterplot: Age vs Balance

ggplot(invalid_data, aes(x = age, y = balance)) +
  geom_point() +
  labs(
    title = "Age vs Balance",
    x = "Age",
    y = "Balance"
  )


# 7.8 Scatterplot with linear trend line

ggplot(invalid_data, aes(x = age, y = balance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Age vs Balance with Linear Trend",
    x = "Age",
    y = "Balance"
  )


# 7.9 Correlation analysis

cor(
  invalid_data$age,
  invalid_data$balance,
  use = "complete.obs"
)

correlation_matrix <- cor(
  numeric_data,
  use = "complete.obs"
)

correlation_matrix

corrplot(
  correlation_matrix,
  method = "color",
  type = "upper"
)


# 7.10 Scatterplot matrix

pairs(numeric_data)


# 7.11 Final verification

dim(invalid_data)

str(invalid_data)

View(invalid_data)

write.csv(
  invalid_data,
  "bank_descriptive_statistics.csv",
  row.names = FALSE
)


# ============================================================
# 8. DATASET SPLITTING (80% TRAINING, 20% TESTING)
# ============================================================


# 8.1 Set random seed

set.seed(123)


# 8.2 Calculate training size

train_size <- round(0.80 * nrow(invalid_data))

train_size


# 8.3 Randomly select training rows

train_index <- sample(
  1:nrow(invalid_data),
  train_size
)

train_data <- invalid_data[train_index, ]

test_data <- invalid_data[-train_index, ]


# 8.4 Verify dataset splitting

cat("Total Rows:", nrow(invalid_data), "\n")

cat("Training Rows:", nrow(train_data), "\n")

cat("Testing Rows:", nrow(test_data), "\n")

cat(
  "Training Percentage:",
  round(nrow(train_data) / nrow(invalid_data) * 100, 2),
  "%\n"
)

cat(
  "Testing Percentage:",
  round(nrow(test_data) / nrow(invalid_data) * 100, 2),
  "%\n"
)

View(train_data)

View(test_data)

write.csv(train_data, "bank_train_data.csv", row.names = FALSE)

write.csv(test_data, "bank_test_data.csv", row.names = FALSE)


# ============================================================
# 9. FEATURE SELECTION
# ============================================================


# Select relevant features and target variable

selected_data <- invalid_data %>%
  select(
    y,
    age,
    job,
    marital,
    education,
    balance,
    housing,
    loan,
    duration,
    campaign
  )


# 9.1 Verify selected features

head(selected_data)

str(selected_data)

dim(selected_data)

View(selected_data)

write.csv(
  selected_data,
  "bank_selected_features.csv",
  row.names = FALSE
)


# ============================================================
# PROJECT COMPLETED
# ============================================================

cat("Bank Marketing Data Preprocessing Completed Successfully!")
