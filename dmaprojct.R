library(readr)
library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)
library(janitor)

data <- read.csv(file.choose())

head(data)

# Check number of rows and columns
dim(data)

# Display column names
names(data)

# Understand data types
str(data)

# Statistical summary
summary(data)




# DATA CLEANING

# Remove unnecessary columns

clean_data <- data

clean_data$ID <- NULL
clean_data$Delivery_person_ID <- NULL

clean_data$Restaurant_latitude <- NULL
clean_data$Restaurant_longitude <- NULL

clean_data$Delivery_location_latitude <- NULL
clean_data$Delivery_location_longitude <- NULL

clean_data$delivery_speed <- NULL

# Check the cleaned dataset

dim(clean_data)
names(clean_data)



# Check missing values

colSums(is.na(clean_data))

# Remove rows containing missing values

clean_data <- na.omit(clean_data)

# Check dataset again

dim(clean_data)

# Check missing values again

colSums(is.na(clean_data))


# Save cleaned dataset

write.csv(clean_data,
          "cleaned_food_delivery.csv",
          row.names = FALSE)

# Check duplicate records

sum(duplicated(clean_data))
# Graph 1: Delivery Time Distribution

ggplot(clean_data, aes(x = Time_taken..min.)) +
  geom_histogram(fill = "skyblue",
                 color = "black",
                 bins = 10) +
  labs(title = "Distribution of Delivery Time",
       x = "Delivery Time (Minutes)",
       y = "Number of Orders")

# Graph 2: Delivery Time vs Traffic Density

ggplot(clean_data,
       aes(x = Road_traffic_density,
           y = Time_taken..min.)) +
  
  geom_boxplot(fill = "lightgreen") +
  
  labs(title = "Delivery Time vs Traffic Density",
       x = "Traffic Density",
       y = "Delivery Time (Minutes)")

# Graph 3: Delivery Time vs Weather Condition
ggplot(clean_data,
       aes(x = Weather_conditions,
           y = Time_taken..min.)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Delivery Time vs Weather Conditions",
       x = "Weather Conditions",
       y = "Delivery Time (Minutes)")
 

# Graph 4: Delivery Time vs Distance

ggplot(clean_data,
       aes(x = distance_km,
           y = Time_taken..min.)) +
  
  geom_point(color = "blue",
             alpha = 0.4) +
  
  geom_smooth(method = "lm",
              color = "red") +
  
  labs(title = "Delivery Time vs Distance",
       x = "Distance (KM)",
       y = "Delivery Time (Minutes)")


# Graph 5: Delivery Time vs Order Type

ggplot(clean_data,
       aes(x = Type_of_order,
           y = Time_taken..min.)) +
  
  geom_boxplot(fill = "pink") +
  
  labs(title = "Delivery Time vs Order Type",
       x = "Order Type",
       y = "Delivery Time (Minutes)")

# ORDER PATTERN ANALYSIS

# Average delivery time by order type
aggregate(Time_taken..min. ~ Type_of_order,
          data = clean_data,
          FUN = mean)

# Average delivery time by traffic density
aggregate(Time_taken..min. ~ Road_traffic_density,
          data = clean_data,
          FUN = mean)


# RANDOM FOREST MODEL

# Convert categorical columns into factors
clean_data$Road_traffic_density <- as.factor(clean_data$Road_traffic_density)
clean_data$Weather_conditions <- as.factor(clean_data$Weather_conditions)
clean_data$Type_of_order <- as.factor(clean_data$Type_of_order)

# Split data into training and testing sets
set.seed(123)

train_index <- createDataPartition(
  clean_data$Time_taken..min.,
  p = 0.8,
  list = FALSE
)

train_data <- clean_data[train_index, ]
test_data <- clean_data[-train_index, ]

# Train Random Forest model
rf_model <- randomForest(
  Time_taken..min. ~ .,
  data = train_data,
  ntree = 100,
  importance = TRUE
)

# Predict delivery time
predictions <- predict(rf_model, test_data)

# Evaluate model
actual <- test_data$Time_taken..min.

RMSE <- sqrt(mean((actual - predictions)^2))
MAE <- mean(abs(actual - predictions))
R2 <- 1 - sum((actual - predictions)^2) /
  sum((actual - mean(actual))^2)

print(RMSE)
print(MAE)
print(R2)


# Actual vs Predicted Delivery Time
plot(actual, predictions,
     main = "Actual vs Predicted Delivery Time",
     xlab = "Actual Time (Minutes)",
     ylab = "Predicted Time (Minutes)",
     col = "blue",
     pch = 16)

abline(0, 1, col = "red", lwd = 2)

# Variable importance
varImpPlot(rf_model)




# PREDICTING A NEW ORDER

# Take input from user

traffic <- readline("Enter Traffic Density: ")
qdw



weather <- readline("Enter Weather Conditions: ")
order_type <- readline("Enter Order Type: ")
distance <- as.numeric(readline("Enter Distance in KM: "))

# Create a new order using all model features
new_order <- train_data[1, ]

# Fill the four user inputs
new_order$Road_traffic_density <- factor(
  traffic,
  levels = levels(train_data$Road_traffic_density)
)

new_order$Weather_conditions <- factor(
  weather,
  levels = levels(train_data$Weather_conditions)
)

new_order$Type_of_order <- factor(
  order_type,
  levels = levels(train_data$Type_of_order)
)

new_order$distance_km <- distance

# Remove target column
new_order$Time_taken..min. <- NULL

# Predict delivery time
predicted_time <- predict(rf_model, new_order)

# Display result
cat("\nPredicted Delivery Time:",
    round(predicted_time, 2), "minutes")
