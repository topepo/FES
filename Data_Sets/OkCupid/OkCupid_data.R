library(lubridate)
library(caret)
library(tidymodels)
library(tidytext)
library(SnowballC)
library(textfeatures)
library(sessioninfo)

# ------------------------------------------------------------------------------

data(stop_words)

stop_words <- stop_words %>% filter(lexicon == "SMART")

# ------------------------------------------------------------------------------
## Some levels of predictors have spaces or symbols; fix these
## and replace "" with "missing"

fix_levels <- function(x, add_missing = "missing") {
  x <- gsub("&rsquo;", "", x)
  x <- gsub("[[:space:]]", "_", x)
  x <- gsub("[[:punct:]]", "_", x)
  x <- gsub("__", "_", x, perl = TRUE)
  x <- gsub("__", "_", x, perl = TRUE)
  x <- gsub("__", "_", x, perl = TRUE)
  if (!is.null(add_missing))
    x[x == ""] <- add_missing
  resort_lvl(x)
}

# ------------------------------------------------------------------------------
## resort categorical predictors so that "missing" is the first level

resort_lvl <- function(x) {
  x <- as.character(x)
  miss_val <- grep("missing$", unique(x), value = TRUE)
  if(length(miss_val) > 0)
    lv <- c(miss_val, sort(unique(x[x != miss_val])))
  else
    lv <- sort(unique(x))
  factor(x, levels = lv)
}

# ------------------------------------------------------------------------------
## Unpackage the data and read in. A compressed version of the csv
## file is at https://github.com/rudeboybert/JSE_OkCupid

raw <- 
  read.csv("profiles.csv", stringsAsFactors = FALSE) %>% 
  dplyr::select(-sex, -body_type, -orientation)

# ------------------------------------------------------------------------------
## Compute the number of days since last online

tmp_last <- ymd(substring(raw$last_online, 1, 10))
tmp_last <- difftime(max(tmp_last), tmp_last, units = "days")
raw$last_online <- as.numeric(tmp_last)

# ------------------------------------------------------------------------------
## encode the "easy" categorical predictors

raw$drinks <- fix_levels(raw$drinks, "drinks_missing")
raw$drugs <- fix_levels(raw$drugs, "drugs_missing")
raw$education <- fix_levels(raw$education, "ed_missing")
raw$diet <- fix_levels(raw$diet,"diet_missing")
raw$job <- fix_levels(raw$job, "missing")
raw$offspring <- fix_levels(raw$offspring, "kids_missing")
raw$pets <- fix_levels(raw$pets, "pets_missing")
raw$smokes <- fix_levels(raw$smokes, "smokes_missing")
raw$status <- fix_levels(raw$status)

# ------------------------------------------------------------------------------
## Income is basically encoded categorical so we will make it a factor

test <- ifelse(raw$income == -1, NA, raw$income)
test <- factor(paste0("inc", test), levels = c("missing", paste0("inc", sort(unique(test)))))
test[is.na(test)] <- "missing"
raw$income <- test

# ------------------------------------------------------------------------------
## Split their location into city and state. There are some R functions
## (ahem, randomForest) that can only handle predictors with <=52
## levels so we take the long tail of the distribution and truncate
## some cities to "other"

tmp_where <- strsplit(raw$location, split = ", ")
where_state <- 
  map_chr(tmp_where, function(x) if(length(x) == 2) x[2] else "missing")
where_town <- 
  map_chr(tmp_where, function(x) if(length(x) == 2) x[1] else "missing")

town_tab <- sort(-table(where_town))
where_town[!(where_town %in% names(town_tab)[1:50])] <- "other"

raw$where_state <- factor(gsub(" ", "_", where_state))
raw$where_town <- factor(gsub(" ", "_", where_town))
raw$location <- NULL

# ------------------------------------------------------------------------------
## Some predictors have values and modifiers that describe how
## serious they are about their choice. We will create predictors
## for both characteristics of their answer

## for religion, split religion and modifier
tmp_relig_split <- strsplit(raw$religion, split = " ")
tmp_relig <- unlist(lapply(tmp_relig_split, function(x) x[1]))
tmp_relig[tmp_relig == ""] <- "religion_missing"
tmp_relig[is.na(tmp_relig)] <- "religion_missing"
raw$religion <- resort_lvl(tmp_relig)

rel_mod <- function(x) {
  if (length(x) > 1) {
    paste(x[-1], collapse = "_")
  } else {
    "religion_mod_missing"
  }
}

raw$religion_modifer <- map_chr(tmp_relig_split, rel_mod)
raw$religion_modifer <- resort_lvl(raw$religion_modifer)

# ------------------------------------------------------------------------------
## Same for sign

raw$sign <- gsub("&rsquo;", "", raw$sign)
tmp_sign_split <- strsplit(raw$sign, split = " ")
tmp_sign <- map_chr(tmp_sign_split, function(x) x[1])
sign_lvl <- sort(unique(tmp_sign))
tmp_sign[tmp_sign == ""] <- "sign_missing"
tmp_sign[is.na(tmp_sign)] <- "sign_missing"
raw$sign <- resort_lvl(tmp_sign)

sign_mod <- function(x) {
  if (length(x) > 1) {
    paste(x[-1], collapse = "_")
  } else {
    "sign_mod_missing"
  }
}
raw$sign_modifer <- map_chr(tmp_sign_split, sign_mod)
raw$sign_modifer <- resort_lvl(raw$sign_modifer)

# ------------------------------------------------------------------------------
## They are allowed to list multiple languages so we will pre-split
## these into dummy variables since they might have multiple choices.
## Also, "c++" and "lisp" !

tmp_speaks <- gsub("(", "", raw$speaks, fixed = TRUE)
tmp_speaks <- gsub(")", "", tmp_speaks, fixed = TRUE)
tmp_speaks <- gsub("c++", "cpp", tmp_speaks, fixed = TRUE)
tmp_speaks_split <- strsplit(tmp_speaks, split = ",")
tmp_speaks_split <- lapply(tmp_speaks_split, function(x) gsub("^ ", "", x))
tmp_speaks_split <- lapply(tmp_speaks_split, function(x) gsub(" ", "_", x))
speaks_values <- sort(unique(unlist(tmp_speaks_split)))
# just keep prog langs
speaks_values <- grep("(cpp)|(lisp)", speaks_values, value = TRUE)
for (i in speaks_values) 
  raw[, i] <- ifelse(
    map_chr(tmp_speaks_split, function(x, sp) any(x == sp), sp = i), 
    1, 0
  )
raw$speaks <- NULL

# ------------------------------------------------------------------------------
## Similaly, ethnicity is pre-split into dummy variables

tmp_eth <- gsub(", ", ",", raw$ethnicity)
tmp_eth <- gsub("/ ", "", tmp_eth)
tmp_eth <- gsub(" ", "_", tmp_eth)
tmp_eth_split <- strsplit(tmp_eth, split = ",")
eth_lvl <- sort(unique(unlist(tmp_eth_split)))
for (i in eth_lvl) 
  raw[, i] <- ifelse(
    map_chr(tmp_eth_split, function(x, eth) any(x == eth), eth = i), 
    1, 0
  )
raw$ethnicity <- NULL

# ------------------------------------------------------------------------------
## Get the essay data

is_essay <- grepl("^essay", names(raw))
essay_names <- names(raw)[is_essay]

ess <- apply(raw[, essay_names],  1, paste, collapse = " ")
ess <- gsub("\n", " ", ess, fixed = TRUE)

raw$essay_length <- log10(nchar(ess) - 8)

raw <- raw[, !is_essay]
raw$essays <- ess

# ------------------------------------------------------------------------------
## There are very few missing values for continuous fields so 
## remove them and convert the job field to the outcome. 

okc <- raw[complete.cases(raw),]

okc$Class <- factor(ifelse(grepl("(computer)|(science)", okc$job), "stem", "other"),
                    levels = c("stem", "other"))


okc <- okc[okc$job != "missing",]
okc$job <- NULL
okc$profile <- 1:nrow(okc)

basic_features <-
  textfeatures(
    okc$essays,
    normalize = FALSE,
    export = FALSE,
    word_dims = 0,
    threads = 4
  ) %>%
  dplyr::select(-n_caps, -id, -n_nonasciis, -n_capsp, -n_chars) %>%
  mutate(profile = okc$profile)


# ------------------------------------------------------------------------------
## Split the data

set.seed(6830)
in_test <- createDataPartition(okc$Class, p = .25, list = FALSE)

okc_test  <- okc[ in_test, ]
okc_train <- okc[-in_test, ]

basic_features_train  <- basic_features[-in_test, ]
basic_features_train$profile <- okc_train$profile
basic_features_test   <- basic_features[ in_test, ]
basic_features_test$profile <- okc_test$profile

set.seed(7520)
okc_down <- downSample(x = okc_train[, names(okc_train) != "Class"],
                       y = okc_train$Class)

basic_features_down <-
  textfeatures(
    okc_down$essays,
    normalize = FALSE,
    export = FALSE,
    word_dims = 0,
    threads = 4
  ) %>%
  dplyr::select(-n_caps, -id, -n_nonasciis, -n_capsp) %>%
  mutate(profile = okc_down$profile)

# ------------------------------------------------------------------------------
## Create the smaller downsampled set of data for the keyword analysis

set.seed(4808)
sampled_rows <- 
  createDataPartition(okc_down$Class, p = 10000/nrow(okc_down), list = FALSE)

okc_sampled <- okc_down[sampled_rows,]

sampled_summaries <- okc_down[sampled_rows, c("Class", "essay_length")]

essays <- okc_down[sampled_rows, c("Class", "essays", "profile")]
char_dist <- nchar(essays$essays)
no_essays <- sum(char_dist == 9)
char_dist <- char_dist[char_dist > 9] - 9

has_link <- ifelse(grepl("(href)|(www\\.[[:alpha:]])", essays$essays), 1, 0)

essays <- 
  essays %>%
  mutate(essays = gsub("\n", " ", essays, fixed = TRUE),
         essays = gsub("<br />", " ", essays),
         essays = gsub("<.*?>", " ", essays),# find a way to keep hrefs?
         essays = gsub("/", " ", essays),
         essays = gsub("[[:punct:]]", " ", essays),
         essays = gsub("[[:digit:]]", " ", essays)) 

# ------------------------------------------------------------------------------

link_tab <- 
  table(
    has_link, 
    okc_sampled$Class
    )

# ------------------------------------------------------------------------------
## Get the individual words and stem

words <- 
  essays %>%
  unnest_tokens(word, essays) %>%
  ## remove repeated characters like 'aaaaaaaa'
  dplyr::filter(!grepl("\\b(\\S+?)\\1\\S*\\b", word, perl = TRUE)) 

all_words_n <- length(unique(words$word))

stopped_words <- 
  words %>%
  anti_join(stop_words, by = "word")

stopped_words_n <- length(unique(stopped_words$word))

stemmed_words <- 
  stopped_words %>%
  ## stem the words
  mutate(word = wordStem(word))

stemmed_words_n <- length(unique(stemmed_words$word))

# ------------------------------------------------------------------------------
## Odds-ratio analysis

calc_or <- function(x) {
  if (nrow(x) != 2) 
    return(data.frame(or = NA, 
                      pvalue = NA,
                      stem = NA,
                      other = NA))
  dat <- x[, c("yes", "no")]
  mat <- as.matrix(dat)
  tab <- as.table(mat)
  rownames(tab) <- x$Class
  res <- fisher.test(tab)
  data.frame(or = unname(res$estimate), 
             pvalue = res$p.value,
             stem = tab["stem", "yes"]/sum(tab["stem",]),
             other = tab["other", "yes"]/sum(tab["other",]),
             events = sum(tab[, "yes"]))
}

total_words_by_class <- 
  words %>%
  group_by(Class) %>%
  summarize(total = length(unique(profile)))

word_counts <- 
  words %>%
  group_by(Class, word) %>%
  summarize(yes = length(unique(profile))) %>%
  full_join(total_words_by_class) %>%
  mutate(no = total - yes)

low_events <- 
  word_counts %>%
  group_by(word) %>%
  summarize(events = sum(yes)) %>%
  dplyr::filter(events < 50) %>%
  dplyr::select(-events)

keyword_results <- 
  word_counts %>%
  anti_join(low_events) %>%
  group_by(word) %>%
  do(calc_or(.)) 

keyword_results$FDR <- p.adjust(keyword_results$pvalue)

# ggplot(keyword_results, aes(x = or, y = -log10(FDR))) + 
#   geom_point(alpha = .2, aes(size = sqrt(events))) + 
#   scale_x_log10()

hits <- 
  keyword_results %>% 
  filter(-log10(FDR) > 5 & (or > 2 | or < 1/2)) %>%
  arrange(FDR)

keywords <- hits$word

# ------------------------------------------------------------------------------
## Binary keyword indicators (for word embedding model)

binary_key <- recipe(~ essays + profile, data = okc_train %>% head())

for (i in keywords) 
  binary_key <- 
  binary_key %>%
  step_regex(essays, pattern = paste0(" ", i), result = i)

binary_key <- 
  binary_key %>%
  step_rm(essays)

binary_key <- prep(binary_key, training = okc_train)
okc_train_binary <- juice(binary_key) 
okc_test_binary <- bake(binary_key, okc_test) 

# ------------------------------------------------------------------------------
## Counts of keyword occurances

count_key <- recipe(~ essays + profile, data = okc_train %>% head())

for (i in keywords) 
  count_key <- 
  count_key %>%
  step_count(essays, pattern = paste0(" ", i), result = paste0("n_", i))

count_key <- 
  count_key %>%
  step_rm(essays) %>%
  step_corr(matches("^n_"), threshold = .90)

count_key <- prep(count_key, training = okc_train)
okc_train_count <- juice(count_key)
okc_test_count <- bake(count_key, okc_test)
okc_down_count <- 
  okc_train_count %>%
  dplyr::filter(profile %in% okc_down$profile)

# ------------------------------------------------------------------------------
## Save the various objects

save(no_essays, char_dist, hits, link_tab,
     all_words_n, stopped_words_n, stemmed_words_n,
     sampled_summaries,
     keyword_results, keywords,
     file = "okc_info.RData")

save(okc_train_binary, okc_test_binary, file = "okc_binary.RData")
save(okc_train_count, okc_test_count, file = "okc_counts.RData")
save(basic_features_train, basic_features_test, file = "okc_features.RData")

okc_test$essays <- NULL
okc_train$essays <- NULL
okc_down$essays <- NULL

save(okc_test, okc_train, okc_down, okc_sampled, file = "okc.RData")
save(ess, file = "essays.RData")
save(speaks_values, eth_lvl, file = "okc_other.RData")

# ------------------------------------------------------------------------------
##

print(session_info())

if (!interactive())
  q("no")
