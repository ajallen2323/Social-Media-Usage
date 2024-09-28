# Author:  Angelina
# Date:    2024-05-07
# Purpose: Portfolio 2 - Analysis
#-------------------------------------------------------------------------------

# Load packages
suppressPackageStartupMessages(library("tidyverse")); theme_set(theme_bw())
library("knitr")
library("kableExtra")

# Calculate summary statistics by wool-tension combination
data <- read_csv("Social_Media_Usage.csv")

breaks <- c(0,20,40, Inf)
labels <- c("Teen", "Young Adult", "Adult")


newdata <-data |>
  mutate(age_group = cut(age, 
                         breaks = breaks, 
                         labels = labels, 
                         right = FALSE))

s <- newdata |>
  group_by(gender, age_group)|>
  summarize(
     count = n(),
     mean_time_spent = mean(time_spent),
     median_time_spent = median(time_spent),
     min_time_spent = min(time_spent),
     max_time_spent = max(time_spent),
     sd_time_spent = sd(time_spent),
     .groups = 'drop'
  )
total_observations <- nrow(newdata)

summary_stats <- s |>
  mutate(total_observations = total_observations)

# Create HTML table from summary statistics
kable(summary_stats, 
      format = "html", 
      caption = "Summary Statistic of age group, timespent, gender variables",
      col.names = c("gender",
                    "age level",
                    "count",
                    "mean(time)",
                    "median(time)", 
                    "min(time",
                    "max(time)",
                    "sd(time)",
                    "total obs")) |>
  kable_styling()


# Create exploratory plot of the data
ggplot(data = newdata, aes(x = age_group,
                        y = time_spent,
                        color = gender)) + 
            geom_boxplot() +
            labs(titlel = "Explanatory Plot of the Response vs the Explanatory", 
                 x      = "Age Group", 
                 y      = "Time Spent(hrs)", 
                 color  = "Gender")

# Fit linear regression model 
m            <- lm(time_spent ~ gender + age_group, 
                   data = newdata)

#model with interaction
m2 <- lm(time_spent ~ gender * age_group, 
                   data = newdata)

#interaction regression model
nd <- newdata |>
  select(gender, age_group) |>
  unique()

ci2 <- bind_cols(
  nd,
  predict(m2,
          newdata = nd,
          interval = "confidence") |>
    as.data.frame() |>
    rename(time_spent = fit)
)

ggplot(data = newdata, aes(x = age_group,
                           y = time_spent,
                           color = gender)) + 
  geom_jitter()+
  geom_pointrange(
    data = ci2,
    aes(
      ymin = lwr,
      ymax = upr
    ),
    position = position_dodge(width = 0.1)
  )+ 
  geom_line(
    data = ci2,
    aes(
      group = gender
    )) +
  labs(
    x = "Age Group",
    y = "Time Spent (hrs)",
    title = "Estimated Mean Time Spent with 95% Confidence Intervals",
    subtitle = "Simple Linear Regression Model with Interaction"
  )


nd <- newdata |>
  select(gender, age_group) |>
  unique()

# Construct confidence intervals
ci <- bind_cols(
  nd,
  predict(m,
          newdata = nd,
          interval = "confidence") |>
    as.data.frame() |>
    rename(time_spent = fit)
)

ggplot(data = newdata, aes(x = age_group,
                               y = time_spent,
                               color = gender)) + 
  geom_jitter()+
  geom_pointrange(
    data = ci,
    aes(
      ymin = lwr,
      ymax = upr
    ),
    position = position_dodge(width = 0.1)
  )+ 
  geom_line(
    data = ci,
    aes(
      group = gender
    )
  ) +
  labs(
    x = "Age Group",
    y = "Time Spent (hrs)",
    title = "Estimated Mean Time Spent with 95% Confidence Intervals",
    subtitle = "Simple Linear Regression Model")

ci <- bind_cols(
  nd,
  predict(m,
          newdata = nd,
          interval = "confidence") |>
    as.data.frame() |>
    rename(time_spent = fit)
)

ci |>
  kable(format = "html", 
        caption = "95% confidence intervals for each level",
        col.names = c("gender",
                      "age level",
                      "estimated mean(time)",
                      "lwr ci", 
                      "upr ci")) |>
  kable_styling()
