# process_chunk.R
library(data.table)

args <- commandArgs(trailingOnly=TRUE)
input_file <- args[1]
output_file <- args[2]

# Expected column names
colnames_vec <- c(
  "recommendationid", "appid", "game", "author_steamid", "author_num_games_owned",
  "author_num_reviews", "author_playtime_forever", "author_playtime_last_two_weeks",
  "author_playtime_at_review", "author_last_played", "language", "review",
  "timestamp_created", "timestamp_updated", "voted_up", "votes_up", "votes_funny",
  "weighted_vote_score", "comment_count", "steam_purchase", "received_for_free",
  "written_during_early_access", "hidden_in_steam_china", "steam_china_location"
)

# Read the file smartly (based on header detection)
first_line <- readLines(input_file, n = 1)

if (grepl("recommendationid", first_line)) {
  df <- fread(input_file, header = TRUE)
} else {
  df <- fread(input_file, header = FALSE, col.names = colnames_vec)
}

# Filter to only English reviews
df <- df[language == "english"]

# Feature engineering
df[, review_length := nchar(review)]

# Logistic Regression
logit_model <- glm(voted_up ~ review_length, data = df, family = "binomial")
coef_logit <- coef(summary(logit_model))["review_length", "Estimate"]
pval_logit <- coef(summary(logit_model))["review_length", "Pr(>|z|)"]

# Linear Regression
lm_model <- lm(votes_up ~ review_length, data = df)
coef_lm <- coef(summary(lm_model))["review_length", "Estimate"]
r_squared_lm <- summary(lm_model)$r.squared

# Save output
output_dt <- data.table(
  coef_logit_review_length = coef_logit,
  pval_logit_review_length = pval_logit,
  coef_lm_review_length = coef_lm,
  r_squared_lm = r_squared_lm
)

fwrite(output_dt, output_file)

