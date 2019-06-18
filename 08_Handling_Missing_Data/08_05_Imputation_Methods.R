# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# 
# Code for Section 8.5 at
# https://bookdown.org/max/FES/imputation-methods.html
#
# ------------------------------------------------------------------------------
# 
# Code requires these packages: 

library(caret)
library(tidymodels)
library(ipred)

# Data used --------------------------------------------------------------------

data(scat)

# ------------------------------------------------------------------------------

scat_missing <- 
  scat %>%
  mutate(
    was_missing = ifelse(is.na(Diameter)| is.na(Mass), "yes", "no"),
    was_missing = factor(was_missing, levels = c("yes", "no"))
  )

# Impute with K-nearest neighbors

imp_knn <- 
  recipe(Species ~ ., data = scat) %>%
  step_knnimpute(Diameter, Mass, 
                 impute_with = 
                   imp_vars(Month, Year, Site, Location, 
                            Age, Number, Length, ropey,
                            segmented, scrape)) %>%
  prep(training = scat, retain = TRUE) %>%
  juice(Diameter, Mass) %>% 
  set_names(c("diam_imp", "mass_imp")) %>%
  mutate(method = "5-Nearest Neighbors")

scat_knn <- bind_cols(scat_missing, imp_knn)

# Fit the models like this to get the out-of-bag estimates of performance. 
# step_bagimpute could also be used. 
set.seed(3453)
diam_fit <- bagging(Diameter ~  ., data = scat[, -1],
                    nbagg = 50, coob = TRUE)
diam_res <- getModelInfo("treebag")[[1]]$oob(diam_fit)

set.seed(3453)
mass_fit <- bagging(Mass ~  ., data = scat[, -1],
                    nbagg = 50, coob = TRUE)
mass_res <- getModelInfo("treebag")[[1]]$oob(mass_fit)

scat_bag <- 
  scat_missing %>%
  mutate(method = "Bagged Tree",
         diam_imp = Diameter, mass_imp = Mass)
scat_bag$diam_imp[is.na(scat_bag$Diameter)] <- 
  predict(diam_fit, scat[is.na(scat$Diameter),])
scat_bag$mass_imp[is.na(scat_bag$Mass)] <- 
  predict(mass_fit, scat[is.na(scat$Mass),])

imputed <- bind_rows(scat_knn, scat_bag)


# Figure 8.7 -------------------------------------------------------------------
# https://bookdown.org/max/FES/imputation-methods.html#fig:missing-imputed-scat

ggplot(imputed, aes(col = was_missing)) + 
  geom_point(aes(x = diam_imp, y = mass_imp), alpha = .5, cex = 2) + 
  geom_rug(data = imputed[is.na(imputed$Mass),], 
           aes(x = Diameter), 
           sides = "b",
           lwd = 1) + 
  geom_rug(data = imputed[is.na(imputed$Diameter),], 
           aes(y = Mass), 
           sides = "l",
           lwd = 1) + 
  theme(legend.position = "top") + 
  xlab("Diameter") + ylab("Mass") + 
  facet_wrap(~method)

# ------------------------------------------------------------------------------

sessionInfo()
