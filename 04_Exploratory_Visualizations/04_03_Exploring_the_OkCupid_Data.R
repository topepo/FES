# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 4.3 at
# https://bookdown.org/max/FES/visualizations-for-categorical-data-exploring-the-okcupid-data.html
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(tidymodels)
library(gridExtra)
library(mgcv)
library(scales)
library(FactoMineR)
library(vcd)
library(colorspace)

l10_breaks <- scales::trans_breaks("log10", function(x) 10^x)
l10_labels <- scales::trans_format("log10", scales::math_format(10^.x))

theme_set(theme_bw())

# ------------------------------------------------------------------------------

load(file.path("..", "Data_Sets", "OkCupid", "okc.RData"))

# ------------------------------------------------------------------------------

binom_stats <- function(x, ...) {
  x <- x$Class[!is.na(x$Class)]
  res <- prop.test(x = sum(x == "stem"), n = length(x), ...)
  data.frame(Proportion  = unname(res$estimate), 
             Lower = res$conf.int[1],
             Upper = res$conf.int[2])
}

stem_rate <- mean(okc_train$Class == "stem")

religion_rates <- 
  okc_train %>%
  group_by(religion) %>%
  do(binom_stats(.)) %>%
  arrange(Proportion) %>%
  ungroup() %>%
  mutate(religion = gsub("religion_", "", religion),
         religion = reorder(factor(religion), Proportion))

okc_train <- 
  okc_train %>% 
  mutate(
    religion2 = gsub("religion_", "", as.character(religion)),
    religion2 = factor(religion2, levels = as.character(religion_rates$religion))
  )

bars <- 
  ggplot(okc_train, aes(x = religion2, fill = Class)) +
  geom_bar(position = position_dodge()) + scale_fill_brewer(palette = "Paired") +
  xlab("") +
  theme(legend.position = "top", axis.text = element_text(size = 8)) +
  ggtitle("(a)")

stacked_vars <-
  ggplot(okc_train, aes(x = religion2, fill = Class)) + geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Paired") +
  xlab("") + ylab("Proportion") +
  theme(legend.position = "none", axis.text = element_text(size = 8)) +
  ggtitle("(b)")

ci_plots <- 
  ggplot(religion_rates, aes(x = religion, y = Proportion)) +
  geom_hline(yintercept = stem_rate, col = "red", alpha = .35, lty = 2) + 
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = .1) +
  theme(axis.text = element_text(size = 8)) +
  xlab("") +
  ggtitle("(c)")

# https://bookdown.org/max/FES/visualizations-for-categorical-data-exploring-the-okcupid-data.html#fig:eda-religion
# grid.arrange(bars, stacked_vars, ci_plots, ncol = 1, heights= c(4, 3, 3))

# ------------------------------------------------------------------------------

gam_dat <- 
  okc_train %>% 
  dplyr::select(essay_length, Class) %>% 
  arrange(essay_length)

gam_small <- 
  gam_dat %>%
  distinct(essay_length) 

gam_mod <- mgcv::gam(Class ~ s(essay_length), data = gam_dat, family = binomial())

gam_small <- gam_small %>%
  mutate(
    link = -predict(gam_mod, gam_small, type = "link"),
    se = predict(gam_mod, gam_small, type = "link", se.fit = TRUE)$se.fit,
    upper = link + qnorm(.975) * se,
    lower = link - qnorm(.975) * se,
    lower = binomial()$linkinv(lower),
    upper = binomial()$linkinv(upper),
    probability = binomial()$linkinv(link)
  )

brks <- l10_breaks(exp(okc_train$essay_length))

essay_hist <- 
  ggplot(okc_train, aes(x = exp(essay_length))) + 
  geom_histogram(binwidth = .1, col = "#FEB24C", fill = "#FED976") + 
  facet_wrap(~ Class, ncol = 1) + 
  scale_x_log10(breaks = brks, labels = l10_labels) +
  xlab("Essay Character Length") + 
  theme_bw() +
  theme(plot.margin = unit(c(0,1,0,1.2), "cm")) + 
  ggtitle("(a)")

essay_gam <- 
  ggplot(gam_small, aes(x = exp(essay_length))) + 
  geom_line(aes(y = probability)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey", alpha = .5) + 
  geom_hline(yintercept = stem_rate, col = "red", alpha = .35, lty = 2)  + 
  scale_x_log10(breaks = brks, labels = l10_labels) +
  theme_bw() + 
  xlab("") +
  theme(plot.margin = unit(c(0,1,0,1.2), "cm"))+ 
  ggtitle("(b)")

# https://bookdown.org/max/FES/visualizations-for-categorical-data-exploring-the-okcupid-data.html#fig:eda-essay-length
# grid.arrange(essay_hist, essay_gam, ncol = 1, heights= c(2, 1.25))

# ------------------------------------------------------------------------------

okc_train <- 
  okc_train %>% 
  mutate(
    drugs = factor(as.character(drugs),
                   levels = c("drugs_missing", "never", "sometimes", "often")),
    drinks = factor(as.character(drinks),
                    levels = c("drinks_missing", "not_at_all", "rarely", 
                               "socially", "often", "very_often", "desperately"))
  )

dd_tab <- table(okc_train$drugs, okc_train$drinks, dnn = c("Drugs", "Alcohol"))


# Formatting for slightly better printing
plot_tab <- dd_tab
dimnames(plot_tab)[[1]][1] <- "missing"
dimnames(plot_tab)[[2]] <- gsub("_", " ", dimnames(plot_tab)[[2]])
dimnames(plot_tab)[[2]][1] <- "missing"
dimnames(plot_tab)[[2]][6] <- "often\n"
dimnames(plot_tab)[[2]][6] <- "very often"
dimnames(plot_tab)[[2]][7] <- "\ndesperately"

# https://bookdown.org/max/FES/visualizations-for-categorical-data-exploring-the-okcupid-data.html#fig:eda-mosaic
mosaic(
  t(plot_tab),
  highlighting = TRUE,
  highlighting_fill = rainbow_hcl,
  margins = unit(c(6, 1, 1, 8), "lines"),
  labeling = labeling_border(
    rot_labels = c(90, 0, 0, 0),
    just_labels = c("left", "right",
                    "center",  "right"),
    offset_varnames = unit(c(3, 1, 1, 4), "lines")
  ),
  keep_aspect_ratio = FALSE
)

# ------------------------------------------------------------------------------

ca_obj <- CA(dd_tab, graph = FALSE)

ca_drugs <- as.data.frame(ca_obj$row$coord)
ca_drugs$label <- gsub("_", " ", rownames(ca_drugs))
ca_drugs$Variable <- "Drugs"

ca_drinks <- as.data.frame(ca_obj$col$coord)
ca_drinks$label <- gsub("_", " ", rownames(ca_drinks))
ca_drinks$Variable <- "Alcohol"

ca_rng <- extendrange(c(ca_drinks$`Dim 1`, ca_drinks$`Dim 2`))
ca_x <- paste0("Dimension #1 (",
               round(ca_obj$eig["dim 1", "percentage of variance"], 0),
               "%)")
ca_y <- paste0("Dimension #2 (",
               round(ca_obj$eig["dim 2", "percentage of variance"], 0),
               "%)")

ca_coord <- rbind(ca_drugs, ca_drinks)

# https://bookdown.org/max/FES/visualizations-for-categorical-data-exploring-the-okcupid-data.html#fig:eda-ca
ca_plot <-
  ggplot(ca_coord, aes(x = `Dim 1`, y = `Dim 2`, col = Variable)) + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  geom_text(aes(label = label)) + 
  xlim(ca_rng) + ylim(ca_rng) + 
  xlab(ca_x) + ylab(ca_y) + 
  coord_equal()

# ------------------------------------------------------------------------------

sessionInfo()

