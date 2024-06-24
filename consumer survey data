# Load necessary libraries
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

# Create a data frame with the provided consumer survey data
survey_data <- data.frame(
  Product = c("A", "B", "C", "D", "E"),
  Price = c(50, 70, 60, 45, 55),
  Rating = c(4.2, 3.8, 4.0, 4.5, 3.9),
  Age_Group = c("25-35", "35-45", "18-25", "45-55", "25-35")
)
# Convert Age Group to a factor for ordered levels
survey_data$Age_Group <- factor(survey_data$Age_Group, levels = c("18-25", "25-35", "35-45", "45-55"))

# 1. How do product ratings vary with both price and age group?
ggplot(survey_data, aes(x = Price, y = Rating, color = Age_Group)) +
  geom_point(size = 4) +
  labs(title = "Product Ratings vs Price and Age Group", x = "Price ($)", y = "Rating (out of 5)") +
  theme_minimal()

# 2. 3D scatter plot to visualize the relationship between price, rating, and age group
plot_ly(survey_data, x = ~Price, y = ~Rating, z = ~Age_Group, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~Rating, colorscale = 'Viridis')) %>%
  layout(title = '3D Scatter Plot of Price, Rating, and Age Group',
         scene = list(xaxis = list(title = 'Price ($)'),
                      yaxis = list(title = 'Rating (out of 5)'),
                      zaxis = list(title = 'Age Group')))

# 3. Identify correlation between age group, product price, and consumer ratings based on the 3D plot
# Correlation can be visually inspected from the 3D scatter plot

# 4. 3D surface plot to show how product ratings change with variations in both price and age group
# Convert Age_Group to a numeric scale for modeling
survey_data$Age_Group_Num <- as.numeric(factor(survey_data$Age_Group, levels = c("18-25", "25-35", "35-45", "45-55")))

# Generate a grid for price and age group
price_seq <- seq(min(survey_data$Price), max(survey_data$Price), length.out = 30)
age_group_seq <- seq(min(survey_data$Age_Group_Num), max(survey_data$Age_Group_Num), length.out = 30)
grid <- expand.grid(Price = price_seq, Age_Group_Num = age_group_seq)

# Fit a linear model for rating
fit <- lm(Rating ~ Price * Age_Group_Num, data = survey_data)
grid$Rating <- predict(fit, newdata = grid)

# 3D surface plot
plot_ly(grid, x = ~Price, y = ~Age_Group_Num, z = ~Rating, type = 'surface') %>%
  layout(title = '3D Surface Plot of Rating',
         scene = list(xaxis = list(title = 'Price ($)'),
                      yaxis = list(title = 'Age Group'),
                      zaxis = list(title = 'Rating (out of 5)')),
         yaxis = list(tickvals = 1:4, ticktext = c("18-25", "25-35", "35-45", "45-55")))

# 5. Compare the 3D plots of product ratings against both price and age group separately
# Rating vs Price
plot_ly(survey_data, x = ~Price, y = ~Rating, type = 'scatter', mode = 'lines+markers', marker = list(color = ~Rating, colorscale = 'Viridis')) %>%
  layout(title = 'Rating vs Price',
         xaxis = list(title = 'Price ($)'),
         yaxis = list(title = 'Rating (out of 5)'))

# Rating vs Age Group
plot_ly(survey_data, x = ~Age_Group_Num, y = ~Rating, type = 'scatter', mode = 'lines+markers', marker = list(color = ~Rating, colorscale = 'Viridis')) %>%
  layout(title = 'Rating vs Age Group',
         xaxis = list(title = 'Age Group', tickvals = 1:4, ticktext = c("18-25", "25-35", "35-45", "45-55")),
         yaxis = list(title = 'Rating (out of 5)'))
