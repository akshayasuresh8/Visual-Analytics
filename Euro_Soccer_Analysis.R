[download the database](https://1drv.ms/u/s!AlrZt1pKHg25gch_i-b1mAbOtWU44Q?e=AMhg1B) and place it in the same folder as this .Rmd file. You can then use the `RSQLite::dbConnect()` function to connect to the database. 



# Load libraries (they need to be installed on the first run via install.packages)
# You do not need to use these libraries, though
library(RSQLite) # provides SQLite interface in R
library(stringr) 
library(ggplot2)
library(dplyr) #select, mutate, arrange, and summarize to transform and analyze data frames efficiently.
library(tidyr) #Complements dplyr by providing functions to reshape data between wide and long formats
library(forcats)#working with categorical variables
library(lubridate)#works with date and time

# connect to database
con <- dbConnect(SQLite(), dbname = "EuropeanSoccer.sqlite")

# table queries
match <- tbl_df(dbGetQuery(con,"SELECT * FROM Match"))
league <- tbl_df(dbGetQuery(con,"SELECT * FROM League"))


1. The first leagues of Spain, England, Germany and Italy are considered the four most attractive football leagues in Europe. In which of the four leagues were the most or the fewest goals scored per game on average? 


# Define the league IDs from the 'top_leagues' table in Image 5
top_league_ids <- c(1729, 7809, 10257, 21518)  # England, Germany, Italy, Spain

# Create a data frame with match data from the 'match' table
matches <- match %>%
  filter(league_id %in% top_league_ids)

# Join with league data
league_matches <- matches %>%
  left_join(league %>% select(id, name), by = c("league_id" = "id"))

# Calculate average goals per match
#Collapse many rows of data into a single summary row 
avg_goals_by_league <- league_matches %>%
  group_by(name) %>%
  summarize(
    total_matches = n(),
    total_goals = sum(home_team_goal + away_team_goal),
    avg_goals_per_match = round(sum(home_team_goal + away_team_goal) / n(), 2)
  ) %>%
  arrange(desc(avg_goals_per_match))

# Plot using ggplot2
ggplot(avg_goals_by_league, aes(x = reorder(name, avg_goals_per_match), y = avg_goals_per_match)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = avg_goals_per_match), vjust = -0.5) +
  labs(title = "Average Goals per Match in Top European Leagues",
       subtitle = "Spain, England, Germany, and Italy",
       x = "League",
       y = "Average Goals per Match") +
  theme_minimal() +
  coord_flip()

2. In this task, we refer again to the four most attractive European leagues from Task 1. Compare the average and the standard deviation of goals scored per match between the four most attractive European leagues on one side and the remaining leagues on the other side.

# Define the league IDs for the top 4 leagues
top_league_ids <- c(1729, 7809, 10257, 21518)  # England, Germany, Italy, Spain

# Categorize leagues
league_categories <- league %>%
  mutate(category = ifelse(id %in% top_league_ids, "Top 4 Leagues", "Other Leagues"))

# Calculate statistics by league category
match_with_categories <- match %>%
  left_join(league_categories %>% select(id, category), by = c("league_id" = "id"))

league_stats <- match_with_categories %>%
  group_by(category) %>%
  summarize(
    total_matches = n(),
    total_goals = sum(home_team_goal + away_team_goal),
    avg_goals_per_match = round(mean(home_team_goal + away_team_goal), 2),
    std_dev_goals = round(sd(home_team_goal + away_team_goal), 2)
  )

# Print the results
print(league_stats)

# Create visualization data (long format)
viz_data <- league_stats %>%
  select(category, avg_goals_per_match, std_dev_goals) %>%
  pivot_longer(cols = c(avg_goals_per_match, std_dev_goals),
               names_to = "metric",
               values_to = "value") %>%
  mutate(metric = ifelse(metric == "avg_goals_per_match", "Average Goals per Match", "Standard Deviation of Goals"))

# Create a grouped bar chart
ggplot(viz_data, aes(x = category, y = value, fill = metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Comparison of Goal Statistics in European Football Leagues",
       subtitle = "Top 4 Leagues vs Other Leagues",
       x = "",
       y = "Value",
       fill = "Metric") +
  theme_minimal()

# Create a boxplot for distribution comparison
match_with_categories %>%
  mutate(total_goals_per_match = home_team_goal + away_team_goal) %>%
  ggplot(aes(x = category, y = total_goals_per_match, fill = category)) +
  geom_boxplot() +
  labs(title = "Distribution of Goals per Match",
       subtitle = "Top 4 Leagues vs Other Leagues",
       x = "",
       y = "Total Goals per Match") +
  theme_minimal() +
  theme(legend.position = "none")

3. Is there really a home advantage? Use a box plot to show the number of goals scored by home and away teams.

# Assuming match data is already loaded into R

# Create a data frame for goals scored
goals_data <- match %>%
  select(home_team_goal, away_team_goal)

# Convert to long format for plotting
goals_long <- goals_data %>%
  pivot_longer(cols = c(home_team_goal, away_team_goal),
               names_to = "team_type",
               values_to = "goals") %>%
  mutate(team_type = ifelse(team_type == "home_team_goal", "Home Team", "Away Team"))

# Create a simple box plot
ggplot(goals_long, aes(x = team_type, y = goals, fill = team_type)) +
  geom_boxplot() +
  labs(title = "Home Advantage in Football: Goals Scored Comparison",
       x = "",
       y = "Number of Goals Scored") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("Home Team" = "#3498db", "Away Team" = "#e74c3c"))


4. _"All soccer players are fair-weather players!"_ Check the assertion with a line chart: Do on average more goals fall per game in the summer months than in the rest of the year?

# Assuming match data is already loaded as 'match' dataframe

# Process dates and calculate goals
match_with_dates <- match %>%
  mutate(
    date = as.Date(substr(date, 1, 10)),
    total_goals = home_team_goal + away_team_goal,
    month = month(date),
    month_name = month(date, label = TRUE),
    is_summer = month %in% c(6, 7, 8)  # Northern Hemisphere summer
  )

# Calculate average goals per game by month
monthly_avg_goals <- match_with_dates %>%
  group_by(month, month_name, is_summer) %>%
  summarize(
    total_matches = n(),
    total_goals = sum(total_goals),
    avg_goals_per_match = total_goals / total_matches,
    .groups = 'drop'
  ) %>%
  arrange(month)

# Print the summary
print(monthly_avg_goals)

# Calculate overall averages for summer vs. non-summer
season_comparison <- match_with_dates %>%
  group_by(is_summer) %>%
  summarize(
    total_matches = n(),
    total_goals = sum(total_goals),
    avg_goals_per_match = total_goals / total_matches,
    .groups = 'drop'
  )

print(season_comparison)

# Create the line chart
ggplot(monthly_avg_goals, aes(x = month, y = avg_goals_per_match, group = 1)) +
  geom_line(size = 1) +
  geom_point(aes(color = is_summer), size = 3) +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "orangered"),
                    labels = c("Other seasons", "Summer")) +
  geom_text(aes(label = round(avg_goals_per_match, 2)), vjust = -1) +
  scale_x_continuous(breaks = 1:12, 
                    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  labs(title = "Average Goals per Match by Month",
       subtitle = "Testing the 'fair-weather players' hypothesis",
       x = "Month",
       y = "Average Goals per Match",
       color = "Season") +
  theme_minimal() +
  # Add a reference line for the overall average
  geom_hline(yintercept = mean(monthly_avg_goals$avg_goals_per_match), 
            linetype = "dashed", color = "gray50") +
  # Highlight summer months
  annotate("rect", xmin = 5.5, xmax = 8.5, ymin = 0, 
          ymax = max(monthly_avg_goals$avg_goals_per_match) + 0.5,
          alpha = 0.1, fill = "orange")


5. Use an estimated density function curve AND a QQ-Plot to check whether the `home_team_possession` variable is (approximately) normally distributed.

# Assuming match data is already loaded as 'match' dataframe

# Filter out nulls
possession_data <- match %>%
  filter(!is.na(home_team_possession)) %>%
  select(home_team_possession)

# Load necessary packages for statistics
library(moments)  # For skewness and kurtosis
library(gridExtra)  # For arranging plots

# Basic summary statistics
summary_stats <- summary(possession_data$home_team_possession)
print(summary_stats)

# Calculate additional statistics for normality assessment
mean_possession <- mean(possession_data$home_team_possession)
sd_possession <- sd(possession_data$home_team_possession)
skewness <- skewness(possession_data$home_team_possession)
kurtosis <- kurtosis(possession_data$home_team_possession)

print(paste("Mean:", round(mean_possession, 2)))
print(paste("Standard Deviation:", round(sd_possession, 2)))
print(paste("Skewness:", round(skewness, 4)))
print(paste("Kurtosis:", round(kurtosis, 4)))

# Perform Shapiro-Wilk test for normality (sample if needed)
if(nrow(possession_data) > 5000) {
  # Sample 5000 observations for Shapiro-Wilk test (due to size limitation)
  set.seed(123)  # For reproducibility
  shapiro_sample <- sample(possession_data$home_team_possession, 5000)
  shapiro_test <- shapiro.test(shapiro_sample)
} else {
  shapiro_test <- shapiro.test(possession_data$home_team_possession)
}
print(shapiro_test)

# Create density plot
density_plot <- ggplot(possession_data, aes(x = home_team_possession)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  # Add normal distribution curve for comparison
  stat_function(fun = dnorm, 
                args = list(mean = mean_possession, sd = sd_possession),
                color = "darkgreen", size = 1, linetype = "dashed") +
  labs(title = "Density Plot of Home Team Possession",
       subtitle = "With normal distribution curve (dashed green line)",
       x = "Home Team Possession (%)",
       y = "Density") +
  theme_minimal() +
  annotate("text", x = max(possession_data$home_team_possession) * 0.8, 
           y = max(density(possession_data$home_team_possession)$y) * 0.8,
           label = paste("Mean =", round(mean_possession, 2), "\nSD =", round(sd_possession, 2)),
           color = "black", hjust = 0)

# Create QQ plot
qq_plot <- ggplot(possession_data, aes(sample = home_team_possession)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ Plot for Home Team Possession",
       subtitle = "Testing for normality",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Arrange plots side by side
combined_plots <- grid.arrange(density_plot, qq_plot, ncol = 2)

------
Dataset:

- https://1drv.ms/u/s!AlrZt1pKHg25gch_i-b1mAbOtWU44Q?e=AMhg1B  
(For database schema and explanation of variables, see: https://www.kaggle.com/hugomathien/soccer)
