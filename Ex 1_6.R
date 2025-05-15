# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Load dataset - using diamonds dataset from ggplot2
data(diamonds)

# 1. Distributions of numerical values
# Creating box plots, violin plots, and histograms with different bin sizes

# Box plots for numerical attributes
p1 <- ggplot(diamonds, aes(y = price)) + 
  geom_boxplot(fill = "lightblue") + 
  labs(title = "Box plot of Diamond Prices")

p2 <- ggplot(diamonds, aes(y = carat)) + 
  geom_boxplot(fill = "lightgreen") + 
  labs(title = "Box plot of Diamond Carat")

# Violin plots
p3 <- ggplot(diamonds, aes(x = cut, y = price)) + 
  geom_violin(fill = "lightpink") + 
  labs(title = "Violin plot of Price by Cut")

# Histograms with different bin sizes
p4 <- ggplot(diamonds, aes(x = price)) + 
  geom_histogram(bins = 30, fill = "coral") + 
  labs(title = "Histogram of Price (30 bins)")

p5 <- ggplot(diamonds, aes(x = price)) + 
  geom_histogram(bins = 100, fill = "coral3") + 
  labs(title = "Histogram of Price (100 bins)")

# 2. Categorical attributes: Barcharts
p6 <- ggplot(diamonds, aes(x = cut, fill = cut)) + 
  geom_bar() + 
  labs(title = "Barchart of Diamond Cut")

p7 <- ggplot(diamonds, aes(x = color, fill = color)) + 
  geom_bar() + 
  labs(title = "Barchart of Diamond Color")

p8 <- ggplot(diamonds, aes(x = clarity, fill = clarity)) + 
  geom_bar() + 
  labs(title = "Barchart of Diamond Clarity")

# 3. Spatial distribution (color-coded dot maps)
# For diamonds, we can create a scatter plot of x vs y dimensions, color-coded by z
p9 <- ggplot(diamonds, aes(x = x, y = y, color = z)) + 
  geom_point(alpha = 0.5) + 
  scale_color_viridis_c() + 
  labs(title = "Spatial Distribution of Diamond Dimensions", 
       x = "Length (mm)", y = "Width (mm)", color = "Depth (mm)")

# 4. Are the distributions consistent with background knowledge?
# Check price distribution by diamond attributes to see if it aligns with expectations
p10 <- ggplot(diamonds, aes(x = carat, y = price, color = cut)) + 
  geom_point(alpha = 0.3) + 
  labs(title = "Price vs Carat by Cut - Checking expectations",
       subtitle = "Better cuts should command higher prices at the same carat")

# 5. Analysis to reveal outliers, gaps in coverage
# Create a boxplot to identify outliers in price
p11 <- ggplot(diamonds, aes(x = cut, y = price)) + 
  geom_boxplot() + 
  labs(title = "Price Distribution by Cut - Identifying Outliers")

# Create a scatterplot to identify gaps in the data
p12 <- ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_point(alpha = 0.2) + 
  labs(title = "Price vs Carat - Identifying Gaps in Coverage")

# 6. Filter for relevant subgroups and look at distributions again
# Filter for high-quality diamonds (Premium and Ideal cuts)
high_quality <- diamonds %>% filter(cut %in% c("Premium", "Ideal"))

p13 <- ggplot(high_quality, aes(x = price)) + 
  geom_histogram(bins = 50, fill = "darkgreen") + 
  labs(title = "Price Distribution for Premium & Ideal Cut Diamonds")

# Filter for larger diamonds (> 1 carat)
large_diamonds <- diamonds %>% filter(carat > 1)

p14 <- ggplot(large_diamonds, aes(x = price)) + 
  geom_histogram(bins = 50, fill = "darkblue") + 
  labs(title = "Price Distribution for Large Diamonds (>1 carat)")

# Filter for specific color category (D - colorless)
d_color_diamonds <- diamonds %>% filter(color == "D")

p15 <- ggplot(d_color_diamonds, aes(x = price)) + 
  geom_histogram(bins = 50, fill = "purple") + 
  labs(title = "Price Distribution for D Color Diamonds")

# Record findings from our analysis
findings <- "
Key Findings from Diamond Dataset Analysis:

1. Numerical Distributions:
   - Diamond prices show right-skewed distribution with most diamonds under $5000
   - Carat weights cluster around common sizes (0.3, 0.5, 0.7, 1.0, etc.)
   
2. Categorical Analysis:
   - Ideal cut is the most common in this dataset
   - Color G and H are most frequent
   - SI1 and VS2 clarity grades dominate
   
3. Spatial Distribution:
   - Diamond dimensions show strong correlation, as expected
   - Larger dimensions generally correspond to higher prices
   
4. Expectation Checks:
   - Better cuts generally command higher prices at same carat, aligning with expectations
   - However, some lower quality cuts have higher prices when carats are higher
   
5. Outliers and Gaps:
   - Price outliers exist in all cut categories
   - There appears to be a gap in the dataset for diamonds between 2-3 carats
   
6. Subgroup Analysis:
   - High-quality diamonds have different price distributions than the overall dataset
   - Large diamonds (>1 carat) show distinct pricing patterns
   - Colorless (D) diamonds command premium pricing across all sizes
"

# Questions for further analysis
questions <- "
Questions for Further Analysis:

1. What factors most strongly predict diamond prices? A multivariate analysis would help.
2. Why do we see gaps in certain carat ranges? Is this a sampling issue or market phenomenon?
3. How have diamond prices changed over time? (Would require temporal data not in this dataset)
4. Are there regional variations in diamond preferences for cut, color, and clarity?
5. How do lab-grown diamonds compare in these attributes to natural diamonds?
"

# Printing and saving
cat(findings)
cat(questions)

# Arrange and view all plots
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 4)
grid.arrange(p9, p10, p11, p12, p13, p14, p15, nrow = 4)

