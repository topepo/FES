# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 4.2 at
# https://bookdown.org/max/FES/visualizations-for-numeric-data-exploring-train-ridership-data.html
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(tidymodels)
library(gridExtra)
library(lubridate)
library(ggiraph)
library(heatmaply)
library(RColorBrewer)
library(scales)

l10_breaks <- scales::trans_breaks("log10", function(x) 10^x)
l10_labels <- scales::trans_format("log10", scales::math_format(10^.x))

# ------------------------------------------------------------------------------

load(file.path("..", "Data_Sets", "Chicago_trains", "chicago.RData"))
load(file.path("..", "Data_Sets", "Chicago_trains", "stations.RData"))

# ------------------------------------------------------------------------------

train_plot_data <- 
  training %>% 
  mutate(date = train_days)

# https://bookdown.org/max/FES/visualizations-for-numeric-data-exploring-train-ridership-data.html#fig:eda-s-40380-boxplot

fig_4_2 <- 
  ggplot(train_plot_data, aes(x = "", y = s_40380)) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  ylab("Clark/Lake Rides (x1000)") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  coord_flip() +
  ylim(-2, 29)

# ------------------------------------------------------------------------------

y_hist <- 
  ggplot(train_plot_data, aes(s_40380)) +   
  geom_histogram(binwidth = .7, col = "#D53E4F", fill = "#D53E4F", alpha = .5) +  
  xlab("Clark/Lake Rides (x1000)") +
  ylab("Frequency") +
  ggtitle("(a)") +
  xlim(-2,29) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

y_box <-
  ggplot(train_plot_data, aes(x = "", y = s_40380)) +
  geom_boxplot(alpha = 0.2) +
  ylab("Clark/Lake Rides (x1000)") +
  ggtitle("(b)") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  coord_flip() +
  ylim(-2,29)

y_violin <-
  ggplot(train_plot_data, aes(x = "", y = s_40380)) +
  geom_violin(alpha = 0.2) +
  ylab("Clark/Lake Rides (x1000)") +
  ggtitle("(c)") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  coord_flip() +
  ylim(-2,29)

# https://bookdown.org/max/FES/visualizations-for-numeric-data-exploring-train-ridership-data.html#fig:eda-s-40380-distribution

# grid.arrange(y_hist, y_box, y_violin, nrow = 3, ncol = 1, heights = c(2, 1, 1))

# ------------------------------------------------------------------------------

train_plot_data <- 
  train_plot_data %>% 
  mutate(
    pow = ifelse(dow %in% c("Sat", "Sun"), "Weekend", "Weekday"),
    pow = factor(pow)
  )

station_names <- 
  stations %>%
  mutate(lag_14_name = gsub("s_", "l14_", station_id))

set.seed(149334)
station_subsets <- 
  station_names %>% 
  sample_frac(1/3) %>% 
  bind_rows(
    station_names %>% 
      dplyr::filter(name %in% c("Clark/Lake", "Lake", "Addison", "UIC-Halsted"))
  ) %>% 
  dplyr::distinct(description, name, lag_14_name)


station_data <- 
  train_plot_data %>%
  filter(date >= ymd("2016-01-01") & pow == "Weekday") %>%
  dplyr::select(matches("^L14_[0-9]"), date) %>% 
  gather(lag_14_name, Rides, -date) %>%
  inner_join(station_subsets, by = "lag_14_name") %>%
  mutate(description = factor(description),
         description = reorder(description, -Rides, median))

box_stats <- 
  station_data %>%
  mutate(description = reorder(description, -Rides, median)) %>%
  group_by(description, name) %>%
  summarize(
    iqr = IQR(Rides),
    y25 = quantile(Rides, 0.25),
    y50 = median(Rides),
    y75 = quantile(Rides, 0.75)
  ) %>%
  mutate(lower = y25 - 1.5 * iqr,
         upper = y75 + 1.5 * iqr,
         lower = ifelse(lower < 0, 0, lower)) %>%
  mutate(index = as.numeric(description)) %>%
  mutate(name = gsub("'", "", name))

outlier_data <- 
  station_data %>%
  mutate(description = reorder(description, -Rides, median)) %>%
  dplyr::select(-name) %>%
  inner_join(box_stats, by = "description") %>%
  filter(Rides < lower | Rides > upper) %>%
  dplyr::select(Rides, description, name, index, date) %>%
  mutate(label = paste0(as.character(date), " ", name, ": ",  round(Rides, 1), "K"))  %>%
  mutate(name = gsub("'", "", name))


station_plot <- ggplot(box_stats, aes(x = index)) + 
  geom_boxplot_interactive(
    aes(ymin = lower, lower = y25, middle = y50, 
        upper = y75, ymax = upper, tooltip = name),
    stat = "identity"
  ) +
  geom_point_interactive(
    data = outlier_data,
    aes(x = index, y = Rides, tooltip = label),
    cex = .4
  ) +
  ylab("Rides (x1000)") +
  xlab("Station") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


# https://bookdown.org/max/FES/visualizations-for-numeric-data-exploring-train-ridership-data.html#fig:eda-rides-distribution
station_plot <- ggiraph(code = print(station_plot))

# ------------------------------------------------------------------------------

# # https://bookdown.org/max/FES/visualizations-for-numeric-data-exploring-train-ridership-data.html#fig:eda-s-40380-distribution-POW

fig_4_5 <- 
  all_pred %>% 
  mutate(pow = as.factor(ifelse(dow %in% c("Sat","Sun"), "Weekend", "Weekday"))) %>% 
  ggplot(aes(s_40380 * 1000, fill = pow, col = pow)) + 
  facet_wrap( ~ pow, nrow = 2, scales = "free_y") +
  geom_histogram(binwidth = .03, alpha = .5) +
  scale_fill_manual(values = c("#D53E4F", "#3ed5c4")) +
  scale_color_manual(values = c("#D53E4F", "#3ed5c4")) +
  scale_x_log10(breaks = l10_breaks, labels = l10_labels) +
  xlab("Clark/Lakes Rides") +
  ylab("Frequency") +
  theme(legend.position="none")

# ------------------------------------------------------------------------------

# https://bookdown.org/max/FES/visualizations-for-numeric-data-exploring-train-ridership-data.html#fig:eda-s-40380-vs-l14-s40380

train_plot_data %>% 
  ggplot(aes(l14_40380,s_40380, col = pow)) +
  geom_point(alpha=0.5) +
  scale_color_manual(values = c("#D53E4F", "#3ed5c4")) +
  xlab("Two-week Lag in Ridership (x1000)") +
  ylab("Current Day Ridership (x1000)") + 
  theme(legend.title=element_blank()) + 
  coord_equal()

# ------------------------------------------------------------------------------

# https://bookdown.org/max/FES/visualizations-for-numeric-data-exploring-train-ridership-data.html#fig:eda-weekly-lag
train_plot_data %>% 
  ggplot(aes(l14_40380,s_40380, col = pow)) +
  geom_point(alpha=0.5) +
  scale_color_manual(values = c("#D53E4F", "#3ed5c4")) +
  xlab("Two-week Lag in Ridership (x1000)") +
  ylab("Current Day Ridership (x1000)") + 
  theme(legend.title=element_blank()) + 
  coord_equal()


# ------------------------------------------------------------------------------

heatmap_data <- 
  all_pred  %>% 
  mutate(pow = as.factor(ifelse(dow %in% c("Sat","Sun"), "Weekend", "Weekday"))) %>% 
  dplyr::select(date, s_40380, pow) %>% 
  mutate(
    mmdd = format(as.Date(date), "%m-%d"),
    yyyy = format(as.Date(date), "%Y"),
    lt10 = ifelse(s_40380 < 10 & pow=="Weekday", 1, 0)
  ) 

break_vals <- 
  c("01-01","01-15","02-01","02-15","03-01","03-15","04-01",
    "04-15","05-01","05-15","06-01","06-15", "07-01","07-15",
    "08-01", "08-15","09-01","09-15","10-01","10-15","11-01",
    "11-15","12-01","12-15")


# https://bookdown.org/max/FES/visualizations-for-numeric-data-exploring-train-ridership-data.html#fig:eda-s-40380-low-weekday
ggplot(heatmap_data, aes(yyyy, mmdd)) +
  geom_tile(aes(fill = lt10), colour = "white") +
  scale_fill_gradient(low = "transparent", high = "red") +
  scale_y_discrete(
    breaks = break_vals
  ) +
  xlab("Year") +
  ylab("Month & Day") +
  theme_bw() + 
  theme(legend.position = "none")

# ------------------------------------------------------------------------------

commonHolidays <- 
  c("USNewYearsDay", "Jan02_Mon_Fri", "USMLKingsBirthday", 
    "USPresidentsDay", "USMemorialDay", "USIndependenceDay", 
    "Jul03_Mon_Fri", "Jul05_Mon_Fri", "USLaborDay", "USThanksgivingDay", 
    "Day_after_Thx", "ChristmasEve", "USChristmasDay", "Dec26_wkday", 
    "Dec31_Mon_Fri")

any_holiday <- 
  train_plot_data %>% 
  dplyr::select(date, !!commonHolidays) %>% 
  gather(holiday, value, -date) %>% 
  group_by(date) %>% 
  summarize(common_holiday = max(value)) %>% 
  ungroup() %>% 
  mutate(common_holiday = ifelse(common_holiday == 1, "Holiday", "Non-holiday")) %>% 
  inner_join(train_plot_data, by = "date")
  
holiday_values <- 
  any_holiday %>% 
  dplyr::select(date, common_holiday)

make_lag <- function(x, lag = 14) {
  x$date <- x$date + days(lag)
  prefix <- ifelse(lag < 10, paste0("0", lag), lag)
  prefix <- paste0("l", prefix, "_holiday")
  names(x) <- gsub("common_holiday", prefix, names(x))
  x
}

lag_hol <- make_lag(holiday_values, lag = 14)

# https://bookdown.org/max/FES/visualizations-for-numeric-data-exploring-train-ridership-data.html#fig:eda-weekly-lag

holiday_data <-
  any_holiday %>% 
  left_join(lag_hol, by = "date") %>% 
  mutate(
    year = factor(year),
    l14_holiday = ifelse(is.na(l14_holiday), "Non-holiday", l14_holiday)
    ) 

no_holiday_plot <-
  holiday_data %>% 
  dplyr::filter(common_holiday == "Non-holiday" & l14_holiday == "Non-holiday") %>% 
  ggplot(aes(l14_40380, s_40380, col = pow)) +
  geom_point(alpha=0.5) +
  scale_color_manual(values = c("#D53E4F", "#3ed5c4")) +
  xlab("14-Day Lag") +
  ylab("Current Day") +
  theme(legend.title=element_blank())+ 
  coord_equal()

# ------------------------------------------------------------------------------

cor_mat <- 
  holiday_data %>% 
  dplyr::filter(year == "2016") %>%
  dplyr::select(matches("l14_[0-9]"), pow, common_holiday) %>%
  dplyr::filter(pow == "Weekday" & common_holiday == "Non-holiday") %>%
  dplyr::select(-pow, -common_holiday) %>% 
  cor()

# https://bookdown.org/max/FES/visualizations-for-numeric-data-exploring-train-ridership-data.html#fig:eda-chicago-corrMatrix
cor_map <- 
  heatmaply_cor(
    cor_mat, 
    symm = TRUE, 
    cexRow = .0001, 
    cexCol = .0001, 
    branches_lwd = .1
  ) 

# ------------------------------------------------------------------------------

year_cols <- colorRampPalette(colors = brewer.pal(n = 9, "YlOrRd")[-1])(16)

# https://bookdown.org/max/FES/visualizations-for-numeric-data-exploring-train-ridership-data.html#fig:eda-monthly-ridership
mean_rides <- 
  holiday_data %>% 
  dplyr::filter(common_holiday == "Non-holiday") %>%
  dplyr::mutate(year = factor(year)) %>%
  group_by(
    month = lubridate::month(date, label = TRUE, abbr = TRUE), 
    year, 
    pow
  ) %>%
  dplyr::summarize(average_ridership = mean(s_40380, na.rm = TRUE)) %>% 
  ggplot(aes(month, average_ridership)) +
  facet_wrap( ~ pow, ncol = 2) +
  geom_line(aes(group = year, col = year), size = 1.1) +
  xlab("") +
  ylab("Geometric Mean Ridership (x1000)") +
  scale_color_manual(values = year_cols) +
  guides(
    col = guide_legend(
      title = "",
      nrow = 2,
      byrow = TRUE
    )
  ) + 
  theme(legend.position = "top")

# ------------------------------------------------------------------------------

# https://bookdown.org/max/FES/visualizations-for-numeric-data-exploring-train-ridership-data.html#fig:eda-monthly-gas-prices
monthly_gas_averages_plot <- 
  holiday_data %>% 
  dplyr::filter(common_holiday == "Non-holiday") %>%
  mutate(year = factor(year)) %>%
  group_by(
    month = lubridate::month(date, label = TRUE, abbr = TRUE), 
    year
  ) %>%
  dplyr::summarize(average_l14_gas_price = mean(l14_gas_price, na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = average_l14_gas_price)) +
  geom_line(aes(group = year, col = year), size = 1.3) +
  xlab("") +
  ylab("Average Gas Price/Gallon ($)") +
  scale_color_manual(values = year_cols) +
  guides(
    col = guide_legend(
      title = "",
      nrow = 2,
      byrow = TRUE
    )
  ) + 
  theme(legend.position = "top")

# ------------------------------------------------------------------------------

# https://bookdown.org/max/FES/visualizations-for-numeric-data-exploring-train-ridership-data.html#fig:eda-monthly-gas-prices-and-ridership
rides_and_gas_plot <-
  holiday_data %>% 
  dplyr::filter(common_holiday == "Non-holiday") %>%
  mutate(year = factor(year)) %>%
  group_by(month = lubridate::month(date, label = TRUE, abbr = TRUE),
           year,
           pow) %>%
  dplyr::summarize(
    ridership = mean(s_40380, na.rm = TRUE),
    l14_gas_price = mean(l14_gas_price, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = l14_gas_price, y = ridership)) +
  facet_wrap( ~ pow, ncol = 2) + 
  geom_point(aes(group = year, col = year), alpha = .8) +
  xlab("Average Gas Price per Gallon ($, 2-week lag)") +
  ylab("Average Ridership (x1000)") + 
  scale_color_manual(values = year_cols) +
  guides(
    col = guide_legend(
      title = "",
      nrow = 2,
      byrow = TRUE
    )
  ) + 
  theme(legend.position = "top")

# ------------------------------------------------------------------------------

lag_14_data <- 
  holiday_data %>% 
  dplyr::select(matches("l14_[0-9]"))

PCA_station <- prcomp(lag_14_data)

var_explained <- c(0, PCA_station$sdev ^ 2)
cumulative_var <- cumsum(var_explained)
pct_var_explained <- 100 * cumulative_var / max(cumulative_var)

var_df <- tibble(
  Component = seq_along(pct_var_explained) - 1,
  pct_var_explained = pct_var_explained
)
score_data <- 
  tibble(
    y = holiday_data$s_40380,
    year = factor(holiday_data$year),
    pow = holiday_data$pow,
    PC1 = PCA_station$x[, 1],
    PC2 = PCA_station$x[, 2],
    dow = holiday_data$dow
  )

pca_rng <- extendrange(c(score_data$PC1, score_data$PC2))

var_plot <- 
  var_df %>% 
  dplyr::filter(Component <= 50) %>% 
  ggplot(aes(x = Component, y = pct_var_explained)) +
  geom_line(size = 1.3) +
  ylim(0, 100) +
  xlab("Component") +
  ylab("Percent Variance Explained") +
  ggtitle("(a)")

score_plot12 <- 
  ggplot(score_data, aes(PC1,PC2)) +
  geom_point(size = 1, alpha = 0.25) +
  xlab("Component 1") +
  ylab("Component 2") +
  xlim(pca_rng) + ylim(pca_rng) +
  ggtitle("(b)")

score1_vs_day <- 
  ggplot(score_data, aes(x = dow, y = PC1)) + 
  geom_violin(adjust = 1.5) + 
  ylab("Component 1") +
  xlab("") + 
  ylim(pca_rng) +
  ggtitle("(c)") 

score2_vs_year <- 
  ggplot(score_data, aes(x = year, y = PC2, col = year)) + 
  geom_violin(adjust = 1.5)  + 
  ylab("Component 2") +
  xlab("") + 
  # ylim(pca_rng) +
  scale_color_manual(values = year_cols)+
  theme(legend.position = "none") +
  ggtitle("(d)") 

lay <- rbind(c(1,2),
             c(1,2),
             c(3,3),
             c(4,4))
# https://bookdown.org/max/FES/visualizations-for-numeric-data-exploring-train-ridership-data.html#fig:eda-PCA-stations
pca_plots <-
  grid.arrange(var_plot,
               score_plot12,
               score1_vs_day,
               score2_vs_year,
               layout_matrix = lay)
# ------------------------------------------------------------------------------

sessionInfo()

