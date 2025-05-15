# VISUALIZATION 1: Box Plot using Iris dataset
library(ggplot2)

# Box plot of sepal length by species
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  labs(title = "Distribution of Sepal Length by Species",
       x = "Species",
       y = "Sepal Length (cm)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# VISUALIZATION 2: Time Series of German Wine Production
library(ggplot2)

# Create wine production data based on production statistics
wine_data <- data.frame(
  year = 2010:2023,
  production = c(6.9, 9.1, 9.0, 8.4, 9.2, 8.8, 9.0, 7.6, 10.3, 8.2, 8.4, 8.7, 8.9, 8.6)
)

# Time series visualization
ggplot(wine_data, aes(x = year, y = production)) +
  geom_line(color = "#8A0303", linewidth = 1.2) +  # Wine red color
  geom_point(color = "#8A0303", size = 3) +
  labs(title = "Wine Production in Germany (2010-2023)",
       x = "Year",
       y = "Production (Million Hectoliters)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2010, 2023, 2))


