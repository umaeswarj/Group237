# Load required libraries
library(ggplot2)
library(dplyr)

# Load the dataset (replace with the correct path to your file)
dataset <- read.csv("C:/Users/jpava/Downloads/reviews.csv")

# Check if the file exists (optional step)
if (!file.exists("C:/Users/jpava/Downloads/reviews.csv")) {
  stop("File does not exist!")
}

# View the first few rows of the dataset
head(dataset)

# Convert the dependent variable 'Overall_Rating' to numeric if not already
dataset$Overall_Rating <- as.numeric(as.character(dataset$Overall_Rating))

# Remove rows with NA in 'Overall_Rating'
dataset <- na.omit(dataset)

# Check the structure of the 'Overall_Rating' column
str(dataset$Overall_Rating)

# Summary statistics for 'Overall_Rating'
summary(dataset$Overall_Rating)

# Test for normality using the Shapiro-Wilk test
shapiro_test <- shapiro.test(dataset$Overall_Rating)
print(shapiro_test)

# Plot a histogram with a normal curve overlay
plot <- ggplot(data.frame(Overall_Rating = dataset$Overall_Rating), aes(x = Overall_Rating)) +
  geom_histogram(aes(y = ..density..), bins = 15, color = "black", fill = "skyblue") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$Overall_Rating, na.rm = TRUE),
                                         sd = sd(dataset$Overall_Rating, na.rm = TRUE)),
                color = "red", linewidth = 1) +
  labs(title = "Histogram of Overall Rating with Normal Curve Overlay",
       x = "Overall Rating",
       y = "Density") +
  theme_minimal()

# Display the plot
print(plot)

# Save the plot as a PNG file
setwd("C:/Users/jpava/Downloads")  # Set working directory
ggsave("normality_histogram.png", plot = plot, width = 8, height = 6, dpi = 300)
