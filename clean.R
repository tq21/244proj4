library(tidyverse)

setwd("practice/")
df <- map(list.files(), read.csv) %>%
  bind_rows()
df <- select(df, -id.practice)


f_names_practice <- list.files("practice/")
f_names_year <- list.files("practice_year/")
idx_practice <- substring(f_names_practice, 15, 18)
idx_year <- substring(f_names_year, 20, 23)
idx <- intersect(idx_practice, idx_year)

for (i in 1:length(idx)) {
  f_name_practice <- paste0("practice/acic_practice_", idx[i], ".csv")
  f_name_year <- paste0("practice_year/acic_practice_year_", idx[i], ".csv")
  tmp_practice <- read.csv(f_name_practice)
  tmp_year <- read.csv(f_name_year)
  tmp_year <- reshape(tmp_year, idvar = "id.practice", timevar = "year", direction = "wide")
  tmp_df <- left_join(tmp_practice, tmp_year, by = "id.practice")
  write.csv(tmp_df, paste0("merged_dfa/", i, ".csv"))
}

setwd("merged_dfa/")
tmp = list.files(pattern="*.csv")
myfiles = lapply(tmp, read.csv)
df <- do.call(rbind, myfiles)

# propensity score



write.csv(df, "data.csv")

df <- read.csv("./merged_data/data.csv")

library(dplyr)
imp_0 <- read.csv("importance0.csv")
imp_1 <- read.csv("importance1.csv")

imp_0 <- imp_0 %>% 
  arrange(desc(X.IncMSE)) %>% 
  top_n(10) %>% 
  select(-IncNodePurity)

imp_1 <- imp_1 %>% 
  arrange(desc(X.IncMSE)) %>% 
  top_n(10) %>% 
  select(-IncNodePurity)

imp <- cbind(imp_0, imp_1)

library(xtable)

xtable(imp)







