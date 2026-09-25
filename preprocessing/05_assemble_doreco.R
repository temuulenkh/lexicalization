# This R file reads DoReCo raw data files and assemble counts for English lemmas from transcription.

library(tidyverse)
library(tidytext)
library(here)

doreco_path <- here("rawdata", "doreco")
output_path <- here("data", "doreco_counts")

# Create folder if it doesn't exist
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

matching_files <- list.files(path = doreco_path, pattern = "*_wd.csv", recursive = TRUE, full.names = TRUE)

# Check if the files exist
if (length(matching_files) == 0) {
  stop("Unable to find any DoReCo data files in rawdata/doreco/")
  # There should normally be 52 files, but we exclude Gurindji and Hoocak data sets for copyright reasons
} else if (!(length(matching_files) %in% c(50, 52))) {
  stop("Unable to find all DoReCo data files in rawdata/doreco/")
}

# stan1290 (French) is removed from raw data folder because it has no translation and the code was throwing an error.

read_doreco_file <- function(path) {

  prefix <- basename(path) %>%
  str_remove("_wd\\.csv" )

  d <- suppressWarnings(
    read_csv(path, show_col_types = FALSE)
  )

  df <- d %>%
    filter(ft != lag(ft, default = first(tx))) %>%
    filter(!str_detect(ft, "^<.*>$")) %>%
    mutate(ftorig = ft) %>%
    mutate(ft = str_replace(ft, " <.*>", "")) %>%
    mutate(ft = str_replace(ft, ":", " ")) %>%
    select(lang, ft)

  df_tokens <- df %>%
    unnest_tokens(word, ft) %>%
    count(lang, word, sort = TRUE) %>%
    write_csv(file.path(output_path, paste0(prefix, "_counts.csv")))
}

output <- lapply(matching_files, read_doreco_file)

# Now we assemble counts for all languages into one dataframe.

dopath <- here("data", "doreco_counts")

file_list <- list.files(path = dopath, pattern = "doreco_.*_counts.csv", full.names = TRUE)

do <- file_list %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE)) %>%
  group_by(lang) %>%
  mutate(size = sum(n)) %>%
  ungroup()

trans <- do %>%
  arrange(desc(n)) %>%
  group_by(lang) %>%
  summarise(words = paste(word, collapse = ", ")) %>%
  ungroup()

check <- read_csv(here("rawdata/doreco", "doreco_yura1255_extended_v1.3", "doreco_yura1255_wd.csv"), show_col_types = FALSE) %>%
  filter(ft != lag(ft, default = first(tx))) %>%
  filter(!str_detect(ft, "^<.*>$")) %>%
  mutate(ftorig = ft) %>%
  mutate(ft = str_replace(ft, " <.*>", "")) %>%
  select(lang, ft)

# these languages do not have English translation
langs <- c("lowe1385", "nisv1234", "resi1247",
           "yong1270", "yuca1254", "yura1255")

do2 <- do %>%
  select(lang, word, n) %>%
  filter(!(lang %in% langs)) %>%
  rename(count = n) %>%
  write_csv(here("data", "doreco_counts.csv"))
