# This R file gets contextual diversity information for English component of the Worldlex data.

library(readr)
library(dplyr)
library(here)

worldlexpath <- here("rawdata", "worldlex")
outpath <- here("data", "english_counts.csv")

lcpath <- here("rawdata", "worldlex2isocode.csv")
names <- read_csv(here("rawdata", "worldlexlanguages.csv"), show_col_types = FALSE)

lc <- read_csv(lcpath, show_col_types = FALSE) %>%
  left_join(names, by = "icode")

wcodes <- lc %>% filter(language == "English") %>% select(wcode) %>% pull()

countlist <- list()

for (wci in 1:length(wcodes)) {
  wc <- wcodes[wci]
  print(wc)

  countfile <- dir(path = worldlexpath,
                   pattern = sprintf("^%s\\.Freq\\.2\\.txt$", wc))

  if (length(countfile) > 0) {
    counts <- read_tsv(file.path(worldlexpath, countfile[1]), quote = "", show_col_types = FALSE) %>%
      rename(term = Word) %>%
      mutate(
        BlogSize = sum(BlogFreq, na.rm = TRUE),
        TwitterSize = sum(TwitterFreq, na.rm = TRUE),
        NewsSize = sum(NewsFreq, na.rm = TRUE)
      )

    countlist[[wci]] <- counts
  } else {
    message("No file found for wcode: ", wc)
  }
}

allcounts <- bind_rows(countlist)

write_csv(allcounts, outpath)
