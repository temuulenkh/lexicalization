# This R file gets corpus size for each Worldlex language.

library(tidyverse)
library(here)

lcpath  <- here("rawdata", "worldlex2isocode.csv")
outpath <- here("data", "worldlex_corpsize.csv")

worldlexpath <- here("rawdata", "worldlex")

names <- read_csv(here("rawdata", "worldlexlanguages.csv"), show_col_types = FALSE)

lc <- read_csv(lcpath, show_col_types = FALSE) %>%
  left_join(names, by = "icode") %>%
  mutate(icode = ifelse(icode == "zh-CN", "zh", icode))

wl <- read_csv(here("rawdata", "limetal_worldlexfrequency.txt"),
               col_types = cols(
                 lemma = col_character(),
                 .default = col_double()
               ),
               na = c("", "na"))

icodes <- setdiff(colnames(wl), "lemma")

wcodes <- lc %>%
  filter(icode %in% icodes) %>%
  select(wcode) %>%
  unique() %>%
  pull()

countlist <- list()

for (wci in seq_along(wcodes)) {
  wc <- wcodes[wci]
  print(paste("Processing:", wc))

  countfile <- dir(
    path = worldlexpath,
    pattern = sprintf("^%s\\.Freq\\.2\\.txt$", wc)
  )

  # check if the file exists
  if (length(countfile) == 0) {
    warning(
      sprintf(
        "No Worldlex frequency file found for wcode = '%s' in %s",
        wc, worldlexpath
      )
    )
    next  # skip this code and continue loop
  }

  counts <- tryCatch(
    {
      suppressWarnings(
        read_tsv(
          file.path(worldlexpath, countfile[1]),
          quote = "",
          show_col_types = FALSE
        )
      ) %>%
        mutate(
          blogsize = sum(BlogFreq, na.rm = TRUE),
          twittersize = sum(TwitterFreq, na.rm = TRUE),
          wcode = wc
        ) %>%
        select(wcode, twittersize, blogsize) %>%
        unique()
    },
    error = function(e) {
      warning(
        sprintf(
          "Error reading file for wcode = '%s': %s",
          wc, conditionMessage(e)
        )
      )
      NULL
    }
  )

  if (!is.null(counts)) {
    countlist[[wci]] <- counts
  }
}


allcounts <- bind_rows(countlist) %>%
  left_join(lc, by = "wcode") %>%
  group_by(icode) %>%
  summarise(twittersize = sum(twittersize),
            blogsize = sum(blogsize),
            size = twittersize + blogsize) %>%
  ungroup()

write_csv(allcounts, outpath)
