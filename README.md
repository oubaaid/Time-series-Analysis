```
# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
```
```
# Load the data
file_path <- "C:/Users/ou_ba/Downloads/2023-11-Elec-train.xlsx"
# replace with your file path
data <- read_excel(file_path)

# Rename the columns
colnames(data) <- c("Timestamp", "Power", "Temp")

# Convert Timestamp to the correct format
data$Timestamp <- as.POSIXct(data$Timestamp, format="%m/%d/%Y %H:%M")

# Remove the first line of data
data <- data[-1, ]
```

```
# Convert the relevant Power rows to NA
data$Power[4603:4613] <- NA

# Prepare the surrounding data for interpolation (40 rows before and after)
before_data <- data[(4603-40):(4603-1), ]
after_data <- data[(4613+1):(4613+40), ]

# Combine before and after data
surrounding_data <- rbind(before_data, after_data)

# Fit a polynomial model (degree = 3 as an example)
model <- lm(Power ~ poly(1:nrow(surrounding_data), 3), data = surrounding_data)

# Predict the missing values
predicted_values <- predict(model, newdata = data.frame('1:nrow(surrounding_data)' = 4603:4613))

# Fill the missing values in the original data
data$Power[4603:4613] <- predicted_values
```

```
# Step 3: Data Visualization
# Plot Power over time
ggplot(data, aes(x = Timestamp, y = Power)) + 
  geom_line() + 
  labs(title = "Power Consumption Over Time", x = "Time", y = "Power (kW)")

# Plot Temp over time
ggplot(data, aes(x = Timestamp, y = Temp)) + 
  geom_line(color = 'red') + 
  labs(title = "Temperature Over Time", x = "Time", y = "Temperature (°C)")
```

# Step 4: Data Splitting
train_size <- floor(0.8 * nrow(data))
train_data <- data[1:train_size, ]
test_data <- data[(train_size + 1):nrow(data), ]
