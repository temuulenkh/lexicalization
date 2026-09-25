# This R file prepares a dataframe that contains predictor variables.

library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(testthat)
library(lingtypology)
library(xtable)

all <- read_csv(here("data", "synonym_mapping_manual.csv"), show_col_types = FALSE) %>%
  rename(lemma = synonym)

name <- read_csv(here("data", "lemma_mapping.csv"), show_col_types = FALSE) %>%
  rename(lemma = name_clean)

res <- name %>%
  left_join(all, by = c("concepticon_id", "lemma")) %>%
  filter(is.na(same_synset)) %>%
  select(concepticon_id, lemma) %>%
  mutate(in_synonym = "absent")

lemmas <- all %>%
  select(concepticon_id, lemma, annotation, comment) %>%
  bind_rows(res) %>%
  filter(annotation == 1 | is.na(annotation)) %>%
  # we will exclude these lemmas originally provided by IDS and WOLD
  filter(!(concepticon_id == "142" & lemma == "go"),
         !(concepticon_id == "266" & lemma == "scale"),
         !(concepticon_id == "281" & lemma == "pole"),
         !(concepticon_id == "291" & lemma == "skin"),
         !(concepticon_id == "838" & lemma == "remains"),
         !(concepticon_id == "851" & lemma == "stable"),
         !(concepticon_id == "855" & lemma == "grasp"),
         !(concepticon_id == "873" & lemma == "tribute"),
         !(concepticon_id == "874" & lemma == "sweets"),
         !(concepticon_id == "951" & lemma == "arms"),
         !(concepticon_id == "989" & lemma == "plain"),
         !(concepticon_id == "1022" & lemma == "spoils"),
         !(concepticon_id == "1032" & lemma == "subject"),
         !(concepticon_id == "1081" & lemma == "reckoning"),
         !(concepticon_id == "1103" & lemma == "offering"),
         !(concepticon_id == "1268" & lemma == "affirmative"),
         !(concepticon_id == "1269" & lemma == "negative"),
         !(concepticon_id == "1296" & lemma == "staff"),
         !(concepticon_id == "1323" & lemma == "timepiece"),
         !(concepticon_id == "1325" & lemma == "companion"),
         !(concepticon_id == "1364" & lemma == "well"),
         !(concepticon_id == "1387" & lemma == "reach"),
         !(concepticon_id == "1426" & lemma == "dear"),
         !(concepticon_id == "1508" & lemma == "ruler"),
         !(concepticon_id == "1541" & lemma == "till"),
         !(concepticon_id == "1558" & lemma == "like"),
         !(concepticon_id == "1594" & lemma == "meal"),
         !(concepticon_id == "1604" & lemma == "temples"),
         !(concepticon_id == "1662" & lemma == "point"),
         !(concepticon_id == "1818" & lemma == "judge"),
         !(concepticon_id == "1862" & lemma == "ass"),
         !(concepticon_id == "2023" & lemma == "multitude"),
         !(concepticon_id == "2024" & lemma == "conspiracy"),
         !(concepticon_id == "2144" & lemma == "spring"),
         !(concepticon_id == "3190" & lemma == "broken"),
         !(concepticon_id == "3769" & lemma == "vine"),
         !(concepticon_id == "3820" & lemma == "calm")) %>%
  select(concepticon_id, lemma) %>%
  filter(!is.na(lemma)) %>%
  distinct()

expect_equal(length(unique(lemmas$concepticon_id)), 1154)

# 68 concepts were excluded
1222-1154

check1 <- lemmas %>%
    group_by(concepticon_id) %>%
    summarise(uniq = n_distinct(lemma)) %>%
    ungroup() %>%
    group_by(uniq) %>%
    summarise(n = n()) %>%
    ungroup()

check2 <- lemmas %>%
  group_by(lemma) %>%
  summarise(uniq = n_distinct(concepticon_id)) %>%
  ungroup() %>%
  group_by(uniq) %>%
  summarise(n = n()) %>%
  ungroup()

# prepare frequency data for Worldlex:

# we represent normalized frequency as per million
N <- 1000000

wl <- read_csv(here("rawdata", "limetal_worldlexfrequency.txt"),
               col_types = cols(
                 lemma = col_character(),
                 .default = col_double()
               ),
               na = c("", "na"))

wl1 <- lemmas %>%
  select(concepticon_id, lemma) %>%
  inner_join(wl, by = "lemma")

wl2 <- wl1 %>%
  # combine counts at concept level
  group_by(concepticon_id) %>%
  summarise(zh=sum(zh), ko=sum(ko), id=sum(id), ms=sum(ms), en=sum(en), nl=sum(nl), de=sum(de),
            da=sum(da), no=sum(no), sv=sum(sv), fi=sum(fi), lt=sum(lt), pl=sum(pl), ru=sum(ru), uk=sum(uk),
            mk=sum(mk), el=sum(el), ro=sum(ro), it=sum(it), fr=sum(fr), ca=sum(ca), es=sum(es), pt=sum(pt)) %>%
  ungroup()

size <- read_csv(here("data", "worldlex_corpsize.csv"), show_col_types = FALSE) %>%
  select(icode, size) %>% unique()

# Worldlex has 43.6 million tokens per language, on average
check <- wl2 %>%
  pivot_longer(cols = c(zh:pt), names_to = "icode", values_to = "normcount") %>%
  inner_join(size, by = "icode") %>%
  select(icode, size) %>%
  unique()
expect_equal(mean(check$size),43639534)

wl3 <- wl2 %>%
  pivot_longer(cols = c(zh:pt), names_to = "icode", values_to = "normcount") %>%
  inner_join(size, by = "icode") %>%
  # note that we do not consider NAs as zeros because they do not represent true lexical gaps
  filter(!is.na(size), !is.na(normcount)) %>%
  # Lim et al. data has normalized count, so we estimate row counts in order to apply add-one smoothing
  mutate(count = normcount * size) %>%
  rename(total = size) %>%
  # apply smoothing
  group_by(icode) %>%
  mutate(
    count = count + 1,
    total = total + n()
  ) %>%
  ungroup() %>%
  select(-normcount)

# let's convert iso2 code to glottocode

wik2 <- read_tsv(here("rawdata", "wik_twocodes.tsv"), show_col_types = FALSE) %>%
  rename(iso2 = Code, language_name = "Canonical name")

wik3 <- read_tsv(here("rawdata", "wiktionary_langs.tsv"), show_col_types = FALSE) %>%
  inner_join(wik2, by = "language_name") %>%
  inner_join(glottolog %>% select(glottocode, iso), by = "iso") %>%
  select(iso2, glottocode) %>% unique()

wl4 <- wl3 %>%
  left_join(wik3 %>% rename(icode = iso2), by = "icode") %>%
  mutate(glottocode = case_when(
    icode == "zh" ~ "mand1415",
    icode == "ms" ~ "stan1306",
    TRUE ~ glottocode
  )) %>%
  select(-icode)

wl5 <- wl4 %>%
  group_by(concepticon_id) %>%
  mutate(nlangs = n_distinct(glottocode)) %>%
  ungroup() %>%
  # include concepts that have at least 3 language data
  filter(nlangs >= 3) %>%
  select(-nlangs)

wl_langs <- wl5 %>%
  select(glottocode) %>%
  unique() %>%
  left_join(glottolog %>%
              mutate(langfamily_split= str_split(affiliation, ", ")) %>%
              mutate(langfamily = map_chr(langfamily_split, 1)) %>%
              select(glottocode, area, langfamily), by = "glottocode") %>%
  write_csv(here("data", "wl_langs.csv"))

# there are 23 languages in Worldlex
expect_equal(length(unique(wl5$glottocode)),23)

# prepare mean and variance of usage frequency
wlex <- wl5  %>%
  mutate(normcount = count / total * N) %>%
  # write English only data
  mutate(en = ifelse(glottocode == "stan1293", normcount, NA)) %>%
  group_by(concepticon_id) %>%
  summarise(wl_mean = mean(normcount),
            wl_cvar   = sd(normcount) / mean(normcount),
            en = sum(en, na.rm = TRUE)) %>%
  # round to 6 decimal points
  mutate(across(wl_mean:en, ~ round(.x, 6))) %>%
  ungroup()

# prepare frequency data for Doreco:

do <- read_csv(here("data", "doreco_counts.csv"), show_col_types = FALSE)

ledo <- read_tsv(here("data", "lemma_doreco.tsv"), show_col_types = FALSE) %>%
  rename(word = original_word, lemma = lemmatized_word)

do2 <- do %>%
  # apply lemmatizer: it also takes out non-English words that happen to be in translation part
  inner_join(ledo, by = "word") %>%
  filter(!is.na(lemma)) %>%
  group_by(lang, lemma) %>%
  # combine counts by lemma
  summarise(count = sum(count)) %>%
  ungroup() %>%
  group_by(lang) %>%
  mutate(total = sum(count)) %>%
  ungroup()

# DoReCo has 13 thousand tokens per language, on average
check <- do2 %>%
  select(lang, total) %>%
  unique()
expect_equal(round(mean(check$total),0),13405)

do3 <- lemmas %>%
  select(concepticon_id, lemma) %>%
  inner_join(do2, by = "lemma", relationship = "many-to-many") %>%
  group_by(concepticon_id, lang, total) %>%
  # combine counts by concept
  summarise(count = sum(count)) %>%
  ungroup() %>%
  group_by(concepticon_id) %>%
  mutate(nlangs = n_distinct(lang)) %>%
  ungroup() %>%
  # include concepts that have at least 3 language data
  filter(nlangs >= 3) %>%
  select(-nlangs)

do4 <- do3 %>%
  select(concepticon_id, lang, count) %>%
  # assign 0 counts
  pivot_wider(names_from = lang, values_from = count, values_fill = 0) %>%
  pivot_longer(cols = -concepticon_id, names_to = "lang", values_to = "count") %>%
  left_join(do3 %>% select(lang, total) %>% unique(), by = "lang") %>%
  rename(glottocode=lang)

do5 <- do4 %>%
  # apply smoothing
  group_by(glottocode) %>%
  mutate(
    count = count + 1,
    total = total + n()
  ) %>%
  ungroup()

do_langs <- do5 %>%
  select(glottocode) %>%
  unique() %>%
  left_join(glottolog %>%
              mutate(langfamily_split= str_split(affiliation, ", ")) %>%
              mutate(langfamily = map_chr(langfamily_split, 1)) %>%
              select(glottocode, area, langfamily), by = "glottocode") %>%
  write_csv(here("data", "do_langs.csv"))

# English translation is available for 46 languages
expect_equal(length(unique(do5$glottocode)),46)

dore <- do5 %>%
  mutate(normcount = count / total * N) %>%
  group_by(concepticon_id) %>%
  summarise(do_mean = mean(normcount),
            do_cvar = sd(normcount) / mean(normcount)
            ) %>%
  # round to 5 decimal points
  mutate(across(do_mean:do_cvar, ~ round(.x, 5))) %>%
  ungroup()

# Now prepare other predictor variables which will be used for main analyses

leot <- read_tsv(here("data", "lemma_others.tsv"), show_col_types = FALSE) %>%
  rename(word = original_word, lemma = lemmatized_word)

# word association data

swow <- read_csv(here("rawdata/swow", "responseStats.SWOW-EN.20180827.csv"), show_col_types = FALSE) %>%
  clean_names() %>%
  select(response, freq_r123) %>%
  filter(!is.na(freq_r123)) %>%
  mutate(total = sum(freq_r123)) %>%
  rename(word = response) %>%
  # need to implement lemmatization
  left_join(leot, by = "word") %>%
  filter(!is.na(lemma)) %>%
  group_by(lemma, total) %>%
  summarise(freq_r123 = sum(freq_r123)) %>%
  ungroup() %>%
  inner_join(lemmas, by = "lemma") %>%
  group_by(concepticon_id, total) %>%
  summarise(freq_r123 = sum(freq_r123)) %>%
  ungroup() %>%
  mutate(ins = freq_r123 / total) %>%
  # round to 6 decimal points
  mutate(ins = round(ins, 6)) %>%
  select(-freq_r123)

# contextual diversity data

# we represent contextual diversity as per 10000
N_cd <- 10000

cd <- read_csv(here("data", "English_counts.csv"), show_col_types = FALSE) %>%
  clean_names()

# find total number of documents for blog and twitter

total_blog <- cd %>%
  mutate(ratio = blog_cd / blog_cd_pc) %>%
  filter(blog_cd_pc == max(blog_cd_pc, na.rm = TRUE)) %>%
  mutate(ratio = round(ratio, 0)) %>%
  pull(ratio)

total_twitter <- cd %>%
  mutate(ratio = twitter_cd / twitter_cd_pc) %>%
  filter(twitter_cd_pc == max(twitter_cd_pc, na.rm = TRUE)) %>%
  mutate(ratio = round(ratio, 0)) %>%
  pull(ratio)

total_cd <- total_blog + total_twitter

cd <- cd %>%
  rename(word=term) %>%
  # lemmatize words
  inner_join(leot, by = "word") %>%
  # for each lemma, combine CD values
  group_by(lemma) %>%
  summarise(blog_cd = sum(blog_cd),
            twitter_cd = sum(twitter_cd)) %>%
  ungroup() %>%
  # take the intersection of CD data and lexicalization data
  inner_join(lemmas, by = "lemma") %>%
  # for each concept, combine CD values
  group_by(concepticon_id) %>%
  summarise(blog_cd = sum(blog_cd),
            twitter_cd = sum(twitter_cd)) %>%
  ungroup() %>%
  # we take sum of blog and twitter CD
  mutate(cd = blog_cd + twitter_cd,
         total = total_cd) %>%
  # apply add one smoothing
  mutate(cd = cd + 1,
         total = total + n()) %>%
  # normalize by total number of documents
  mutate(cd =  cd / total * N_cd) %>%
  # round to 6 decimal points
  mutate(cd = round(cd, 6)) %>%
  select(-blog_cd, -twitter_cd, -total)

aoakup <- read_excel(here("rawdata", "kupermanetal_aoa.xlsx")) %>%
  rename(lemma = Lemma_highest_PoS, aoa_self = AoA_Kup_lem) %>%
  filter(!is.na(aoa_self), aoa_self != "NA") %>%
  # round to 6 decimal points
  mutate(aoa_self = round(as.numeric(aoa_self), 6)) %>%
  select(lemma, aoa_self) %>%
  unique()

check <- lemmas %>%
  inner_join(aoakup, by = "lemma") #%>%
  #filter(concepticon_id %in% dlex$concepticon_id)

expect_equal(length(unique(check$concepticon_id)),1116)

aoawb <- readRDS(here("rawdata", "portelanceetal_aoa.rds")) %>%
  select(language, uni_lemma, aoa) %>%
  unique() %>%
  group_by(language, uni_lemma) %>%
  # some language has multiple AoA for the same lemma, so we take the lowest
  summarise(aoa = min(aoa)) %>%
  ungroup() %>%
  group_by(uni_lemma) %>%
  # average AoA across languages
  summarise(aoa = mean(aoa),
            # round to 6 decimal points
            aoa = round(aoa, 6),
            langs = n_distinct(language)) %>%
  ungroup() %>%
  rename(lemma = uni_lemma, aoa_parent = aoa)

check <- lemmas %>%
  inner_join(aoawb, by = "lemma") #%>%
   #filter(concepticon_id %in% dlex$concepticon_id)

expect_equal(length(unique(check$concepticon_id)),366)

conc <- read_excel(here("rawdata", "brysbaertetal_concreteness.xlsx")) %>%
  clean_names() %>%
  select(word, conc_m) %>%
  rename(lemma = word, conc = conc_m) %>%
  filter(!is.na(lemma), !is.na(conc))

sens <- read_csv(here("rawdata", "lynottetal_sensorimotor.csv"), show_col_types = FALSE) %>%
  clean_names() %>%
  # we use aggregated measure for sensorimotor strength
  select(word, minkowski3_sensorimotor) %>%
  rename(lemma= word, sens = minkowski3_sensorimotor) %>%
  filter(!is.na(lemma), !is.na(sens)) %>%
  mutate(lemma = str_to_lower(lemma))

d_add <- lemmas %>%
  inner_join(swow, by = "concepticon_id") %>%
  inner_join(aoakup, by = "lemma") %>%
  inner_join(conc, by = "lemma") %>%
  inner_join(cd, by = "concepticon_id") %>%
  inner_join(sens, by = "lemma") %>%
  group_by(concepticon_id) %>%
  summarise(
            # these do not affect in-strength and CD score because it was already estimated for the concept level
            ins = mean(ins),
            cd = mean(cd),
            # we take min for AoA
            aoa_self = min(aoa_self),
            # we take max for these predictors
            conc = max(conc),
            sens = max(sens)
  ) %>%
  ungroup()

dlex <- read_csv(here("data", "lex_wold.csv"), show_col_types = FALSE) %>%
  inner_join(read_csv(here("data", "lex_ids.csv"), show_col_types = FALSE) %>%
               select(concepticon_id, lex_ids),
             by = "concepticon_id")

dlex_strict <- read_csv(here("data", "lex_wold_strict.csv"), show_col_types = FALSE)

labels <- lemmas %>%
  filter(concepticon_id %in% dlex$concepticon_id) %>%
  group_by(concepticon_id) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  group_by(n) %>%
  summarise(count=n()) %>%
  ungroup()

d_full <- dlex %>%
    inner_join(wlex, by = "concepticon_id") %>%
    inner_join(dore, by = "concepticon_id") %>%
    inner_join(d_add, by = "concepticon_id")
expect_false(any(is.na(d_full)))

expect_equal(length(unique(d_full$concepticon_id)),854)

d_full %>% write_csv(here("data", "d_full.csv"))

d_full_strict <- dlex_strict %>%
  inner_join(wlex, by = "concepticon_id") %>%
  inner_join(dore, by = "concepticon_id") %>%
  inner_join(d_add, by = "concepticon_id")
expect_false(any(is.na(d_full_strict)))

expect_equal(length(unique(d_full_strict$concepticon_id)),488)

d_full_strict %>% write_csv(here("data", "d_full_strict.csv"))

d_add_dev <- lemmas %>%
  inner_join(swow, by = "concepticon_id") %>%
  inner_join(aoakup, by = "lemma") %>%
  inner_join(aoawb, by = "lemma") %>%
  inner_join(conc, by = "lemma") %>%
  inner_join(cd, by = "concepticon_id") %>%
  inner_join(sens, by = "lemma") %>%
  group_by(concepticon_id) %>%
  summarise(ins = mean(ins),
            cd = mean(cd),
            # we take min for AoA
            aoa_self = min(aoa_self),
            aoa_parent = min(aoa_parent),
            # we take max for these predictors
            conc = max(conc),
            sens = max(sens)
  ) %>%
  ungroup()

d_dev <- dlex %>%
  inner_join(wlex, by = "concepticon_id") %>%
  inner_join(dore, by = "concepticon_id") %>%
  inner_join(d_add_dev, by = "concepticon_id")
expect_false(any(is.na(d_dev)))

expect_equal(length(unique(d_dev$concepticon_id)),324)

d_dev %>% write_csv(here("data", "d_dev.csv"))

d_dev_strict <- dlex_strict %>%
  inner_join(wlex, by = "concepticon_id") %>%
  inner_join(dore, by = "concepticon_id") %>%
  inner_join(d_add_dev, by = "concepticon_id")
expect_false(any(is.na(d_dev_strict)))

expect_equal(length(unique(d_dev_strict$concepticon_id)),164)

d_dev_strict %>% write_csv(here("data", "d_dev_strict.csv"))


# Now prepare other predictor variables which will be used for additional analyses

# First add taxonomic depth

tax <- read_tsv(here("data", "taxon_depth.tsv"), show_col_types = FALSE) %>%
  rename(tax = taxonomic_depth) %>%
  select(concepticon_id, tax) %>%
  unique() %>%
  filter(!is.na(tax))

overlap <- d_full %>%
  inner_join(tax, by = "concepticon_id")

# Now add relevant variables in NoRaRe

norare <- read_csv(here("rawdata/norare", "norare.csv"), show_col_types = FALSE) %>%
  clean_names()
glosses <- read_csv(here("rawdata/norare", "glosses.csv"), show_col_types = FALSE) %>%
  clean_names()
variables <- read_csv(here("rawdata/norare", "variables.csv"), show_col_types = FALSE) %>%
  clean_names()

rep <- norare %>%
  left_join(glosses %>% rename(unit_id = id), by = "unit_id") %>%
  filter(parameter_id %in% overlap$concepticon_id) %>%
  group_by(variable_id) %>%
  summarise(n = n_distinct(parameter_id)) %>%
  ungroup() %>%
  arrange(desc(n))

# We selected relevant predictor variables one by one from the one with the most representation for number of concepts
# until the resulting data frame has at least 500 concepts.

selected <- read_csv(here("data", "norare_selected_manual.csv"), show_col_types = FALSE)

# first prepare frequency and contextual diversity variables in NoRaRe
# we treat frequency and contextual diversity variables as the same as we do Worldlex and DoReCo
# by applying add-one smoothing and normalizing by size.

# NoRaRe has both raw and normalized counts for some datasets, we use them to infer total size

a <- norare %>%
  inner_join(selected %>%
               filter(rename %in% c("freq_subtitleus", "cd_subtitleus", "freq_spanish", "freq_german")) %>%
               select(variable_id, rename, comment), by = "variable_id") %>%
  left_join(glosses %>% rename(unit_id = id) %>%
              select(-comment), by = "unit_id") %>%
  select(rename, parameter_id, comment, value) %>%
  mutate(variable = paste0(rename, "_", comment),
         value = as.numeric(value)) %>%
  select(-rename, -comment) %>%
  rename(concepticon_id = parameter_id) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(
    freq_subtitleus_total = freq_subtitleus_raw / freq_subtitleus_normalized,
    cd_subtitleus_total   = cd_subtitleus_raw / cd_subtitleus_normalized,
    freq_spanish_total    = freq_spanish_raw / freq_spanish_normalized,
    freq_german_total     = freq_german_raw / freq_german_normalized
  )

# these totals are exact same across all concepts
freq_subtitleus_total <- a$freq_subtitleus_total[1] * 1000000 # frequency was represented as per million
cd_subtitleus_total <- a$cd_subtitleus_total[1] * 100 # CD was represented as per 100
freq_spanish_total <- a$freq_spanish_total[2] * 1000000 # frequency was represented as per million

# there seems to be some inconsistency for German total frequency, so we take the value from what was stated in the paper
freq_german_total <- 25.399 * 1000000 # frequency was represented as per million

# van Heuven et al. data does not have normalized values in NoRaRe, so we use its original data to estimate total frequency and CD

suppressWarnings(
  subuk <- read_xlsx(here("rawdata", "vanheuvenetal_subtlexuk.xlsx"))
)

freq_bbc_total <- sum(subuk$FreqCount, na.rm = TRUE)
freq_bnc_total <- sum(subuk$BNC_freq, na.rm = TRUE)
freq_cbeebies_total <- sum(subuk$Cbeebies_freq, na.rm = TRUE)
freq_cbbc_total <- sum(subuk$CBBC_freq, na.rm = TRUE)

b <- subuk %>%
  mutate(cd_bbc_total = CD_count / CD,
         cd_cbeebies_total = CD_count_Cbeebies / CD_cbeebies,
         cd_cbbc_total = CD_count_CBBC / CD_cbbc)

cd_bbc_total <- b$cd_bbc_total[which.max(b$CD)] %>% round(0) # CD was represented as per 1
cd_cbeebies_total <- b$cd_cbeebies_total[1] %>% round(0) # CD was represented as per 1
cd_cbbc_total <- b$cd_cbbc_total[1] %>% round(0) # CD was represented as per 1

# now prepare frequency and CD data from NoRaRe

freq_cd <- norare %>%
  inner_join(selected %>%
               filter(type %in% c("frequency of use", "contextual diversity") &
                        comment == "raw") %>%
               select(variable_id, rename, comment), by = "variable_id") %>%
  left_join(glosses %>% rename(unit_id = id) %>%
              select(-comment), by = "unit_id") %>%
  select(rename, parameter_id, comment, value) %>%
  mutate(variable = paste0(rename, "_", comment),
         value = as.numeric(value)) %>%
  select(-rename, -comment) %>%
  rename(concepticon_id = parameter_id) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  # retain concepts that have values for all frequency and CD variables
  drop_na() %>%
  # take the intersection with lexicalization data
  filter(concepticon_id %in% lemmas$concepticon_id) %>%
  # add total size data
  mutate(freq_subtitleus_total = freq_subtitleus_total,
         cd_subtitleus_total = cd_subtitleus_total,
         freq_german_total = freq_german_total,
         freq_spanish_total = freq_spanish_total,
         freq_bbc_total = freq_bbc_total,
         freq_cbeebies_total = freq_cbeebies_total,
         freq_cbbc_total = freq_cbbc_total,
         freq_bnc_total = freq_bnc_total,
         cd_bbc_total = cd_bbc_total,
         cd_cbeebies_total = cd_cbeebies_total,
         cd_cbbc_total = cd_cbbc_total) %>%
  # apply add-one smoothing
  mutate(freq_subtitleus_raw = freq_subtitleus_raw + 1,
         cd_subtitleus_raw = cd_subtitleus_raw + 1,
         freq_german_raw = freq_german_raw + 1,
         freq_spanish_raw = freq_spanish_raw + 1,
         freq_bbc_raw = freq_bbc_raw + 1,
         freq_cbeebies_raw = freq_cbeebies_raw + 1,
         freq_cbbc_raw = freq_cbbc_raw + 1,
         freq_bnc_raw = freq_bnc_raw + 1,
         cd_bbc_raw = cd_bbc_raw + 1,
         cd_cbeebies_raw = cd_cbeebies_raw + 1,
         cd_cbbc_raw = cd_cbbc_raw + 1,
         freq_subtitleus_total = freq_subtitleus_total + n(),
         cd_subtitleus_total = cd_subtitleus_total + n(),
         freq_german_total = freq_german_total + n(),
         freq_spanish_total = freq_spanish_total + n(),
         freq_bbc_total = freq_bbc_total + n(),
         freq_cbeebies_total = freq_cbeebies_total + n(),
         freq_cbbc_total = freq_cbbc_total + n(),
         freq_bnc_total = freq_bnc_total + n(),
         cd_bbc_total = cd_bbc_total + n(),
         cd_cbeebies_total = cd_cbeebies_total + n(),
         cd_cbbc_total = cd_cbbc_total + n()) %>%
  # normalize by size
  mutate(freq_subtitleus = freq_subtitleus_raw / freq_subtitleus_total * N,
         cd_subtitleus = cd_subtitleus_raw / cd_subtitleus_total * N_cd,
         freq_german = freq_german_raw / freq_german_total * N,
         freq_spanish = freq_spanish_raw / freq_spanish_total * N,
         freq_bbc = freq_bbc_raw / freq_bbc_total * N,
         freq_cbeebies = freq_cbeebies_raw / freq_cbeebies_total * N,
         freq_cbbc = freq_cbbc_raw / freq_cbbc_total * N,
         freq_bnc = freq_bnc_raw / freq_bnc_total * N,
         cd_bbc = cd_bbc_raw / cd_bbc_total * N_cd,
         cd_cbeebies = cd_cbeebies_raw / cd_cbeebies_total * N_cd,
         cd_cbbc = cd_cbbc_raw / cd_cbbc_total * N_cd) %>%
  select(concepticon_id, freq_subtitleus, cd_subtitleus, freq_german, freq_spanish, freq_bbc, freq_cbeebies,
         freq_cbbc, freq_bnc, cd_bbc, cd_cbeebies, cd_cbbc) %>%
  # round to 6 decimal points
  mutate(across(freq_subtitleus:cd_cbbc, ~ round(.x, 6)))

# prepare NoRaRe variables other than frequency and contextual diversity

other <- norare %>%
  inner_join(selected %>%
               filter(!(type %in% c("frequency of use", "contextual diversity"))) %>%
               select(variable_id, rename), by = "variable_id") %>%
  left_join(glosses %>% rename(unit_id = id), by = "unit_id") %>%
  select(rename, parameter_id, value) %>%
  # round to 6 decimal points
  mutate(value = round(as.numeric(value), 6)) %>%
  filter(!is.na(value)) %>%
  rename(concepticon_id = parameter_id) %>%
  pivot_wider(names_from = rename, values_from = value) %>%
  drop_na()

combined <- overlap %>%
  inner_join(freq_cd, by = "concepticon_id") %>%
  inner_join(other, by = "concepticon_id")
expect_false(any(is.na(combined)))

# the extended set of variables is availabe for 535 concepts
expect_equal(nrow(combined), 535)

combined %>% write_csv(here("data", "d_add.csv"))

# prepare single language frequency

sin_wl <- wl5 %>%
  # normalize by corpus size
  mutate(normcount = count / total * N) %>%
  # round to 6 decimal points
  mutate(normcount = round(normcount, 6)) %>%
  select(-total, -count) %>%
  pivot_wider(names_from = glottocode, values_from = normcount) %>%
  drop_na()

d_sin_wl <- dlex %>%
  inner_join(sin_wl, by = "concepticon_id")

expect_false(any(is.na(d_sin_wl)))
expect_equal(length(unique(d_sin_wl$concepticon_id)),175)

d_sin_wl %>% write_csv(here("data", "d_sin_wl.csv"))

sin_do <- do5 %>%
  # normalize by corpus size
  mutate(normcount = count / total * N) %>%
  # round to 6 decimal points
  mutate(normcount = round(normcount, 6)) %>%
  select(-total, -count) %>%
  pivot_wider(names_from = glottocode, values_from = normcount) %>%
  drop_na()

d_sin_do <- dlex %>%
  inner_join(sin_do, by = "concepticon_id")

expect_false(any(is.na(d_sin_do)))
expect_equal(length(unique(d_sin_do$concepticon_id)),918)

d_sin_do %>% write_csv(here("data", "d_sin_do.csv"))



## Explore languages in the lexicalization and usage frequency datasets

wo <- read_csv(here("data", "wold_langs.csv"), show_col_types = FALSE) %>%
  mutate(langfamily = ifelse(glottocode == "tzot1264", "Mayan", langfamily)) %>%
  mutate(area = ifelse(glottocode == "stan1293", "Eurasia", area))

ids <- read_csv(here("data", "ids_langs.csv"), show_col_types = FALSE) %>%
  rename(glottocode = tip) %>%
  left_join(glottolog %>% select(glottocode, longitude, latitude), by = "glottocode") %>%
  mutate(area = ifelse(glottocode == "stan1293", "Eurasia", area))

wl <- read_csv(here("data", "wl_langs.csv"), show_col_types = FALSE) %>%
  left_join(glottolog %>% select(glottocode, longitude, latitude), by = "glottocode") %>%
  mutate(area = ifelse(glottocode == "stan1293", "Eurasia", area))

wlarea <- wl %>%
  group_by(area) %>%
  summarise(n = n()) %>%
  ungroup()

wlfam <- wl %>%
  group_by(langfamily) %>%
  summarise(n = n()) %>%
  ungroup()

do <- read_csv(here("data", "do_langs.csv"), show_col_types = FALSE) %>%
  left_join(glottolog %>% select(glottocode, longitude, latitude), by = "glottocode") %>%
  mutate(langfamily = ifelse(glottocode %in% c("movi1243", "savo1255"), "isolate", langfamily)) %>%
  mutate(longitude = ifelse(glottocode == "ligh1234", 131.05, longitude),
         latitude = ifelse(glottocode == "ligh1234", -20.10, latitude)) %>%
  mutate(area = case_when(
    glottocode == "movi1243" ~ "South America",
    glottocode == "savo1255" ~ "Papunesia",
    glottocode == "stan1293" ~ "Eurasia",
    glottocode == "sout3282" ~ "Eurasia",
    TRUE ~ area
  ))

doarea <- do %>%
  group_by(area) %>%
  summarise(n = n()) %>%
  ungroup()

dofam <- do %>%
  group_by(langfamily) %>%
  summarise(n = n()) %>%
  ungroup()

wo_wl <- intersect(wo$glottocode, wl$glottocode) # 5 langs
ids_wl <- intersect(ids$glottocode, wl$glottocode) # 16 langs

wo_do <- intersect(wo$glottocode, do$glottocode) # one lang
ids_do <- intersect(ids$glottocode, do$glottocode) # 2 langs

# Save tables for the list of concepts:

co <- read_csv(here("rawdata", "concepticon", "concepticon.csv"), show_col_types = FALSE) %>%
  clean_names() %>%
  rename(concepticon_id = id, concepticon_gloss = name) %>%
  mutate(concepticon_gloss = str_to_lower(concepticon_gloss)) %>%
  distinct(concepticon_id, concepticon_gloss)

d_full_table <- d_full %>%
  select(concepticon_id, semantic_field, semantic_category) %>%
  left_join(co, by = "concepticon_id") %>%
  select(concepticon_id, concepticon_gloss, semantic_field, semantic_category) %>%
  mutate(concepticon_gloss = paste0("\\concept{", concepticon_gloss, "}"))

t_table <- xtable(d_full_table, digits = c(0, 0, 0, 0, 0))
print(t_table, file = here("output", "tables", "d_full_concepts.tex"), include.rownames = FALSE, comment = FALSE)

d_dev_table <- d_dev %>%
  select(concepticon_id, semantic_field, semantic_category) %>%
  left_join(co, by = "concepticon_id") %>%
  select(concepticon_id, concepticon_gloss, semantic_field, semantic_category) %>%
  mutate(concepticon_gloss = paste0("\\concept{", concepticon_gloss, "}"))

t_table <- xtable(d_dev_table, digits = c(0, 0, 0, 0, 0))
print(t_table, file = here("output", "tables", "d_dev_concepts.tex"), include.rownames = FALSE, comment = FALSE)
