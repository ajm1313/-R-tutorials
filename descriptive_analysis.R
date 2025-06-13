# Descriptive analysis of regional indicators
library(tidyverse)
library(lubridate)
library(zoo)
library(broom)

# Path to CSV file containing the data
# The CSV should have a column named 'Period' in the form '18-Jan', '18-Feb', etc.
# and columns for each region/indicator named for example:
#   NE_Underweight, NE_Anaemic, NE_ExclusiveBF, NE_LBW,
#   Northern_Underweight, Northern_Anaemic, ... etc.

data <- read_csv("region_data.csv")

# Convert the period string to a Date
# assuming the format YY-Mon (e.g., '18-Jan')
data <- data %>% mutate(Date = as.Date(as.yearmon(Period, "%y-%b")))

# Define the intervention date (modify as needed)
intervention_date <- as.Date("2020-03-01")

regions <- c("NE", "Northern", "Savannah", "Upper_East", "Upper_West")
indicators <- c("Underweight", "Anaemic", "ExclusiveBF", "LBW")

for (r in regions) {
  for (ind in indicators) {
    col <- paste(r, ind, sep = "_")
    if (!col %in% names(data)) next
    df <- data %>% select(Date, !!sym(col)) %>% rename(value = !!sym(col))
    p <- ggplot(df, aes(Date, value)) +
      geom_line() +
      geom_vline(xintercept = intervention_date, linetype = "dashed", colour = "red") +
      labs(title = paste(ind, "in", r), y = ind)
    ggsave(paste0(r, "_", ind, ".png"), plot = p, width = 7, height = 4)

    pre <- df %>% filter(Date < intervention_date)
    post <- df %>% filter(Date >= intervention_date)
    stats <- tibble(
      Region = r,
      Indicator = ind,
      Period = c("Pre", "Post"),
      Mean = c(mean(pre$value, na.rm = TRUE), mean(post$value, na.rm = TRUE)),
      SD = c(sd(pre$value, na.rm = TRUE), sd(post$value, na.rm = TRUE))
    )
    print(stats)

    df_model <- df %>%
      arrange(Date) %>%
      mutate(
        time = row_number(),
        post = ifelse(Date >= intervention_date, 1, 0)
      )
    intervention_time <- min(df_model$time[df_model$post == 1])
    df_model <- df_model %>%
      mutate(time_after = ifelse(post == 1, time - intervention_time, 0))

    model <- lm(value ~ time + post + time_after, data = df_model)
    model_summary <- tidy(model)
    write_csv(model_summary, paste0(r, "_", ind, "_itsa.csv"))
    print(model_summary)

    level_change <- coef(model)["post"]
    slope_change <- coef(model)["time_after"]

    df_model <- df_model %>%
      mutate(
        predicted = predict(model, newdata = .),
        counterfactual = predict(model, newdata = mutate(., post = 0, time_after = 0)),
        cumulative_effect = ifelse(post == 1, level_change + slope_change * time_after, NA),
        relative_change = ifelse(post == 1, (predicted - counterfactual) / counterfactual * 100, NA)
      )

    write_csv(df_model, paste0(r, "_", ind, "_itsa_effects.csv"))
    print(head(df_model))
  }
}
