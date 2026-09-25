# This R file contains code to prepare lexicalization data.

# Run these to install phyloWeights and glottoTrees packages:
# devtools::install_github("erichround/phyloWeights",
#                          dependencies = T,
#                          INSTALL_opts = c("--no-multiarch"))
# devtools::install_github("erichround/glottoTrees",
#                          dependencies = T,
#                          INSTALL_opts = c("--no-multiarch"))

library(tidyverse)
library(here)
library(lingtypology)
library(janitor)
library(stringdist)
library(phyloWeights)
library(glottoTrees)
library(ape)
library(testthat)
library(xtable)



# 1) Prepare lexicalization data on WOLD

#WOLD was downloaded from its github page on 25 Nov 2024.
wfo <- read_csv(here("rawdata/wold", "forms.csv"), show_col_types = FALSE,
                col_types = cols(etymological_note = col_character(),
                                 numeric_frequency = col_character())) %>%
  clean_names()
wla <- read_csv(here("rawdata/wold", "languages.csv"), show_col_types = FALSE) %>%
  clean_names()
wpa <- read_csv(here("rawdata/wold", "parameters.csv"), show_col_types = FALSE) %>%
  clean_names()

#WOLD represents 24 families and 6 macro areas
wlangs <- wfo %>%
  select(language_id) %>% unique() %>%
  left_join(wla %>% rename(language_id=id), by = "language_id") %>%
  left_join(glottolog %>%
              mutate(langfamily_split= str_split(affiliation, ", ")) %>%
              mutate(langfamily = map_chr(langfamily_split, 1)) %>%
              select(glottocode, area, langfamily), by = "glottocode") %>%
  mutate(langfamily = ifelse(glottocode == "tzot1264", "Mayan", langfamily))

narea <- wlangs %>%
  separate_rows(area, sep = ";") %>%
  distinct(area) %>%
  count() %>%
  pull()

wlangs <- wlangs %>%
  mutate(narea = narea,
         nfam = n_distinct(langfamily)) %>%
  write_csv(here("data", "wold_langs.csv"))

# write tables for WOLD languages
wo_table <- wlangs %>%
  # retain single area assigment for English
  mutate(area = ifelse(glottocode == "stan1293", "Eurasia", area)) %>%
  select(ID=language_id, Language=name, Glottocode=glottocode, `Language family`=langfamily, `Macro area`=area) %>%
  arrange(ID) %>%
  mutate(Source="WOLD")
t_table <- xtable(wo_table, digits = c(0, 0, 0, 0, 0, 0, 0))
print(t_table, file = here("output", "tables", "wold_langs.tex"), include.rownames = FALSE, comment = FALSE)

check <- wlangs %>%
  group_by(area) %>%
  summarise(n = n()) %>%
  ungroup()

# Let's use Concepticon ID mapped to WOLD concepts in NoRaRe dataset.
glosses <-read_csv(here("rawdata/norare", "glosses.csv"), show_col_types = FALSE) %>%
  clean_names() %>%
  rename(unit_id=id)

norare <- read_csv(here("rawdata/norare", "norare.csv"), show_col_types = FALSE) %>%
  clean_names() %>%
  filter(variable_id == "Haspelmath-2009-1460-BORROWING_SCORE") %>%
  left_join(glosses, by = "unit_id") %>%
  select(parameter_id, form) %>%
  rename(concepticon_id = parameter_id, name = form)

woldpa <- norare %>%
  left_join(wpa, by = "name") %>%
  rename(parameter_id = id) %>%
  select(concepticon_id, parameter_id, name) %>%
  # the same concepticon ID is assigned to dinner and supper, we retain dinner
  filter(name != "the supper")

# WOLD has 1458 unique concepts
expect_equal(length(unique(woldpa$concepticon_id)), 1458)

woldpa <- woldpa %>%
  mutate(
    name_clean = name %>%
      str_remove("^(the|a|to|to be)\\s+") %>%
      str_remove("\\s*\\(.*\\)") %>%
      str_replace_all("/", ", ") %>%
      str_remove_all("\\bto\\s+") %>%
      str_remove_all("\\?") %>%
      str_replace_all(" or ", ", ") %>%
      str_squish()
  ) %>%
  # before filtering out compounds, we use these lemmas which are used in IDS
  mutate(name_clean = case_when(
    name == "the low tide" ~ "lowtide",
    name == "the high tide" ~ "hightide",
    name == "the bolt of lightning" ~ "lightning",
    name == "the nasal mucus" ~ "mucus",
    name == "the molar tooth" ~ "molartooth",
    name == "to carry in hand" ~ "carry-in-hand",
    name == "to carry on shoulder" ~ "carry-on-shoulder",
    name == "to carry on head" ~ "carry-on-head",
    name == "to carry under the arm" ~ "carry-underarm",
    name == "to look for" ~ "seek",
    name == "to let go" ~ "release",
    name == "in front of" ~ "front",
    name == "for a long time" ~ "long-time",
    name == "the day after tomorrow" ~ "day-after-tomorrow",
    name == "the day before yesterday" ~ "day-before-yesterday",
    name == "the good luck" ~ "luck",
    name == "the bad luck" ~ "misfortune",
    name == "the walking stick" ~ "staff",
    TRUE ~ name_clean
  )) %>%
  mutate(name_clean = str_split(name_clean, ", ")) %>%
  unnest(name_clean) %>%
  mutate(name_clean = str_replace(name_clean, "^be ", ""))

co <- read_csv(here("rawdata", "concepticon", "concepticon.csv"), show_col_types = FALSE) %>%
  clean_names() %>%
  rename(concepticon_id = id, concepticon_gloss = name) %>%
  mutate(concepticon_gloss = str_to_lower(concepticon_gloss))

compounds_wold <- woldpa %>%
  group_by(concepticon_id) %>%
  filter(all(str_detect(name_clean, " "))) %>%
  summarise(
    names = paste(unique(name), collapse = ", "),
    compound_lemmas = paste(unique(name_clean), collapse = ", "),
    .groups = "drop"
  ) %>%
  left_join(
    co %>% select(concepticon_id, concepticon_gloss),
    by = "concepticon_id"
  )  %>%
  mutate(concepticon_gloss = paste0("\\concept{", concepticon_gloss, "}")) %>%
  select(concepticon_id, concepticon_gloss, compound_lemmas)

t_table <- xtable(compounds_wold, digits = c(0, 0, 0, 0))
print(t_table, file = here("output", "tables", "wold_compounds.tex"), include.rownames = FALSE, comment = FALSE)

woldpa <- woldpa %>%
  # take out compounds
  filter(!str_detect(name_clean, " "))

# there are 1389 concepts left after filtering out concepts expressed by compounds
expect_equal(length(unique(woldpa$concepticon_id)), 1389)

# 69 compounds were excluded
1458-1389

# now let's explore classifications of missing entries
missing <- read_csv(here("rawdata/wold", "missing.csv"),
                    col_names = c("language", "concept_id", "unknown1", "reason", "name", "unknown2"), show_col_types = FALSE)

miss1 <- missing %>%
  group_by(language, reason) %>%
  summarise(n = n_distinct(concept_id)) %>%
  ungroup()  %>%
  pivot_wider(names_from = reason, values_from = n, values_fill = 0)
# Dutch is not included in this table because it has no blank entry

miss2 <- bind_rows(miss1,
                   summarise(miss1, language = "Total", across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))) %>%
  clean_names() %>%
  mutate(true_gaps = meaning_irrelevant_to_speakers + no_counterpart,
         total = insufficient_information + true_gaps,
         per_questionable = round(insufficient_information / total, 2),
         per_true = round(true_gaps / total, 2))

# in total 25% of blank entries are questionable and the rest seems to be true gaps
# we keep questionable cases as NA, and the remaining as true lexical gaps

missing2 <- missing %>%
  left_join(woldpa %>% select(name, concepticon_id), by = "name", relationship = "many-to-many") %>%
  filter(!is.na(concepticon_id))

woldla <- wfo %>%
  select(language_id) %>%
  unique() %>%
  left_join(wla %>% rename(language_id= id), by = "language_id")

matches <- sapply(miss2$language, function(lang) {
  woldla$name[which.min(stringdist::stringdist(lang, woldla$name, method = "jw"))] # Jaro-Winkler distance
})

miss3 <- miss2 %>%
  mutate(matched_name = matches) %>%
  mutate(matched_name = case_when(
    language == "Berber" ~ "Tarifiyt Berber",
    language == "Tzotzil of Zinacantan" ~ "Zinacantán Tzotzil",
    TRUE ~ matched_name
  )) %>%
  left_join(woldla %>% rename(matched_name = name), by = "matched_name") %>%
  select(language, glottocode)

missing3 <- missing2 %>%
  left_join(miss3, by = "language") %>%
  select(concepticon_id, glottocode, reason) %>%
  mutate(lex = ifelse(reason == "Insufficient information", NA, 0))

# now prepare lexicalization data
wfo2 <- wfo %>%
  select(parameter_id, language_id) %>%
  left_join(wla %>% rename(language_id = id) %>%
              select(language_id, glottocode), by = "language_id") %>%
  # take out concepts that are expressed by compounds
  inner_join(woldpa, by = "parameter_id", relationship = "many-to-many") %>%
  select(concepticon_id, glottocode) %>%
  unique() %>%
  mutate(value_present = 1) %>%
  pivot_wider(names_from = glottocode,
              values_from = value_present,
              values_fill = 0) %>%
  pivot_longer(
    cols = -c(concepticon_id),
    names_to = "glottocode",
    values_to = "lex"
  )

wfo3 <- wfo2 %>%
  left_join(missing3, by = c("concepticon_id", "glottocode")) #%>%
  #filter(lex.x == 0, is.na(reason))
# apart from two cases, all blank entries have reason specified

wfo4 <- wfo3 %>%
  mutate(lex = ifelse(is.na(reason), lex.x, lex.y))

wold_data <- wfo4  %>%
  rename(tip = glottocode) %>%
  select(concepticon_id, tip, lex) %>%
  distinct() %>%
  filter(!is.na(lex)) %>%
  mutate(tip = ifelse(tip == "tzot1264", "tzot1259", tip))
# this data will be used as main data to estimate phylogenetic mean later

# now let's explore word-meaning relationship information:
wmr <- read_csv(here("rawdata/wold", "word_meaning_relation.csv"), show_col_types = FALSE)

# out of 24794 entries, the proportions look as follows:
wmr %>%
  filter(!is.na(word_meaning_relation)) %>%
  group_by(word_meaning_relation) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(total = sum(n),
         prop = n / total)

check <- wmr %>%
  group_by(language, word_meaning_relation) %>%
  summarise(n = n_distinct(lwt_code)) %>%
  ungroup()
# no info about word-meaning relationship was given for Bezhta and Old High German, otherwise contributors included some information
# some contributors gave information on only sub-, super-, and para-counterparts. They might have considered the remaining as exact-counterparts, but we have no way to tell for sure. So we'll exclude these data.

wmr2 <- wmr %>%
  filter(!is.na(word_meaning_relation)) %>%
  group_by(language, lwt_code, lwt_label) %>%
  summarise(
    forms = paste(unique(word_form), collapse = "; "),
    relations = paste(unique(word_meaning_relation), collapse = "; "),
    has_exact = any(word_meaning_relation == "Exact counterpart"),
    .groups = "drop"
  )

# there are cases where the concept has synonymuous forms in a given language, they all have different relationships with the concept
# we'll include cases where at least one form has exact counterpart relation with the concept

wmr3 <- wmr2 %>%
  filter(has_exact == TRUE) %>%
  distinct(language, lwt_code, lwt_label) %>%
  # add glottocode information
  left_join(wla %>% distinct(language=name, glottocode), by = "language") %>%
  mutate(glottocode = case_when(
    language == "Kali’na" ~ "gali1262",
    language == "Q’eqchi’" ~ "kekc1242",
    TRUE ~ glottocode
  )) %>%
  # map concepticon ID
  # it also takes out concepts that are expressed by compounds
  inner_join(woldpa %>% distinct(lwt_label = name, concepticon_id, parameter_id), by = "lwt_label")

gaps <- wold_data %>%
  filter(lex == 0)

# now prepare final data that has strict criteria of exact counterpart for phylogenetic mean estimation
wold_data_strict <- wmr3 %>%
  select(concepticon_id, tip= glottocode) %>%
  mutate(tip = ifelse(tip == "tzot1264", "tzot1259", tip)) %>%
  mutate(lex = 1) %>%
  # add gap information
  bind_rows(gaps) %>%
  distinct() %>%
  # let's see how many language data is available for each concept
  group_by(concepticon_id) %>%
  mutate(n = n_distinct(tip)) %>%
  ungroup() %>%
  # retain concepts that have at least 10 language data
  filter(n >= 10) %>%
  select(-n)

length(unique(wold_data_strict$tip)) # 41 languages
length(unique(wold_data_strict$concepticon_id)) # 894 concepts




# 2) Prepare lexicalization data on IDS

# IDS was downloaded from its github page on 11 Nov 2024.
ch <- read_csv(here("rawdata/ids", "chapters.csv"), show_col_types = FALSE)%>%
  clean_names()
fo <- read_csv(here("rawdata/ids", "forms.csv"), show_col_types = FALSE)%>%
  clean_names()
la <- read_csv(here("rawdata/ids", "languages.csv"), show_col_types = FALSE)%>%
  clean_names()
pa <- read_csv(here("rawdata/ids", "parameters.csv"), show_col_types = FALSE)%>%
  clean_names()

# let's explore issues related to language information

# there are 319 language varieties
length(unique(la$id))

# there are 5 language ID with no glottocode assigned and we were unable to assign any reliable glottocode. So, we filter them out.
la2 <- la %>%
  filter(!is.na(glottocode))

# the same glottocode has multiple language IDs because they are dialects
multid <- la2 %>%
  group_by(glottocode) %>%
  summarise(n = n_distinct(id)) %>%
  ungroup() %>%
  #filter(n > 1) %>%
  left_join(glottolog %>% select(glottocode, level), by = "glottocode")
# there are 269 unique glottocodes, of which 25 has multiple IDs, producing 314 unique IDs.

# some language varieties do not have macroarea information, we'll assign them manually
noarea <- la2 %>% filter(is.na(macroarea)) %>%
  left_join(glottolog %>% select(glottocode, area), by = "glottocode")

la3 <- la2 %>%
  mutate(macroarea = case_when(
    glottocode == "cuoi1242" ~ "Eurasia",
    glottocode == "west2394" ~ "Eurasia",
    glottocode == "pala1336" ~ "Eurasia",
    glottocode == "poly1242" ~ "Papunesia",
    glottocode == "aust1307" ~ "Papunesia",
    TRUE ~ macroarea
  ))

# let's explore issues related to concept information
# let's add chapter information
ch2 <- ch %>%
  rename(chapter = id, chapter_name = "description") %>%
  mutate(chapter = as.character(chapter))

pa2 <- pa %>%
  rename(parameter_id = id) %>%
  mutate(concepticon_gloss = tolower(concepticon_gloss))  %>%
  separate_wider_delim(parameter_id, "-", names=c("chapter", "concept_id")) %>%
  left_join(ch2, by = "chapter")

multid <- pa2 %>%
  group_by(concepticon_id, concepticon_gloss) %>%
  summarise(n = n_distinct(concept_id)) %>%
  ungroup()  %>%
  filter(n > 1)
# there are 1308 concepticon ID but 1310 concept ID. The concept tree and dinner is mapped with two different IDs.
# we decided to choose one for each concept: 600 for tree (and delete 420) and 440 for dinner (and delete 450)

pa3 <- pa2 %>%
  filter(!(concepticon_id == 1833 & concept_id == 450),
         !(concepticon_id == 906 & concept_id == 420))

# there are 1308 unique concepts
length(unique(pa3$concepticon_id))

fo2 <- fo %>%
  separate_wider_delim(parameter_id, "-", names=c("chapter", "concept_id")) %>%
  select(language_id, chapter, concept_id, value, form) %>%
  # filter those 5 language with no glottocode, and add language related information
  inner_join(la3 %>% rename(language_id = id) %>%
               select(language_id, glottocode, glottolog_name, macroarea, family), by = "language_id") %>%
  # filter concept IDs related to tree and dinner, and add gloss and chapter information
  inner_join(pa3, by = c("chapter", "concept_id"))

# no missing values should be found
expect_true(all(!is.na(fo2)))

# prepare lemmas for IDS concepts

idspa <- pa3 %>%
  select(concepticon_id, concepticon_gloss, name) %>%
  mutate(
    name_clean = name %>%
      str_remove("\\s*\\(.*\\)") %>%
      str_replace_all("/", ", ") %>%
      str_remove_all("\\bto\\s+") %>%
      str_remove_all("\\?") %>%
      str_replace_all(" or ", ", ") %>%
      str_replace_all("=", ", ") %>%
      str_squish()
  ) %>%
  # use these lemmas which are used in WOLD
  mutate(name_clean = case_when(
    name == "break wind" ~ "fart",
    name == "egg yolk" ~ "yolk",
    name == "woman's dress" ~ "dress",
    name == "pound with fist" ~ "pound",
    name == "two times" ~ "twice",
    name == "tell story" ~ "tell",
    TRUE ~ name_clean
  )) %>%
  mutate(name_clean = str_split(name_clean, ", ")) %>%
  unnest(name_clean) %>%
  mutate(name_clean = str_replace(name_clean, "^be ", ""))

compounds_ids <- idspa %>%
  group_by(concepticon_id) %>%
  filter(all(str_detect(name_clean, " "))) %>%
  summarise(
    concepticon_gloss = first(concepticon_gloss),
    compound_lemmas = paste(unique(name_clean), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(concepticon_gloss = paste0("\\concept{", concepticon_gloss, "}")) %>%
  select(concepticon_id, concepticon_gloss, compound_lemmas)

t_table <- xtable(compounds_ids, digits = c(0, 0, 0, 0))
print(t_table, file = here("output", "tables", "ids_compounds.tex"), include.rownames = FALSE, comment = FALSE)

idspa <- idspa %>%
  # take out compounds
  filter(!str_detect(name_clean, " "))

# there are 1251 concepts after filtering out compounds
expect_equal(length(unique(idspa$concepticon_id)), 1251)

# 57 compounds were excluded
1308-1251

# now prepare lexicalization data on IDS
lex <- fo2 %>%
  mutate(value_present = 1) %>%
  select(concepticon_id, language_id, value_present) %>%
  # take out concepts expressed by compounds
  filter(concepticon_id %in% idspa$concepticon_id) %>%
  unique() %>%
  pivot_wider(names_from = language_id,
              values_from = value_present,
              values_fill = 0)  %>%
  pivot_longer(
    cols = -concepticon_id,
    names_to = "language_id",
    values_to = "lex"
  ) %>%
  left_join(la3 %>% select(id, glottocode, family, macroarea) %>%
              rename(language_id=id) %>% mutate(language_id = as.character(language_id)), by = "language_id")

# we filter out languages that have relatively low representation of concepts
check1 <- lex %>%
  group_by(language_id) %>%
  summarise(lex = sum(lex)) %>%
  ungroup() %>%
  arrange(lex)

# some languages seem to have missing records, we set a threshold to filter out those languages based on WOLD

thresholdtable <- wfo4 %>%
  filter(lex != 0, !is.na(lex)) %>%
  filter(concepticon_id %in% idspa$concepticon_id) %>%
  group_by(glottocode) %>%
  summarise(n = n_distinct(concepticon_id)) %>%
  ungroup()
threshold <- min(thresholdtable$n)

retlang1 <- check1 %>%
  # we use the lowest number of concepts represented by WOLD languages as a threshold
  filter(lex >= threshold) %>%
  select(language_id) %>%
  pull() %>%
  unique()

lex1 <- lex %>%
  filter(language_id %in% retlang1)

# there are 262 language varieties
expect_equal(length(unique(lex1$language_id)), 262)

# write tables for IDS languages
ids_table <- la3 %>%
  filter(id %in% retlang1) %>%
  select(ID=id, Language=name, Glottocode=glottocode, `Language family`=family, `Macro area`=macroarea) %>%
  arrange(ID) %>%
  mutate(Source="IDS")
t_table <- xtable(ids_table, digits = c(0, 0, 0, 0, 0, 0, 0))
print(t_table, file = here("output", "tables", "ids_langs.tex"), include.rownames = FALSE, comment = FALSE)

ilang <- lex1 %>%
  select(glottocode, language_id) %>%
  unique() %>%
  left_join(glottolog %>% select(glottocode, language, level, affiliation), by = "glottocode") %>%
  mutate(last = word(affiliation, -1, sep = ", ")) %>%
  left_join(glottolog %>% select(language, glottocode) %>% rename(last = language, gcode = glottocode), by = "last") %>%
  # use language glottocode for families
  # we chose the first language appearing in Glottolog
  mutate(tip = case_when(
    glottocode == "cuoi1242" ~ "hung1275",
    glottocode == "leng1262" ~ "nort2971",
    glottocode == "west2394" ~ "cent2314",
    glottocode == "poly1242" ~ "anut1237",
    TRUE ~ glottocode
  )) %>%
  # use language glottocode for dialects
  mutate(tip = ifelse(level == "dialect", gcode, tip)) %>%
  select(glottocode, language_id, tip) %>%
  left_join(glottolog %>% select(glottocode, language, level, affiliation) %>%
              rename(tip = glottocode), by = "tip") %>%
  # there are still 4 dialects left, let's change it manually
  mutate(tip = case_when(
    tip == "nort3280" ~ "darg1241",
    tip == "sanz1247" ~ "sout3261",
    tip == "lowe1465" ~ "andi1255",
    tip == "aqus1234" ~ "darg1241",
    TRUE ~ tip
  )) %>%
  select(glottocode, language_id, tip) %>%
  left_join(glottolog %>% select(glottocode, language, level) %>%
              rename(tip = glottocode), by = "tip")

# all tips have to be language glottocode
expect_equal(unique(ilang$level), "language")

ils <- ilang %>%
  group_by(tip) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  left_join(glottolog %>%
              mutate(langfamily_split= str_split(affiliation, ", ")) %>%
              mutate(langfamily = map_chr(langfamily_split, 1)) %>%
              select(glottocode, area, langfamily) %>%
              rename(tip = glottocode), by = "tip") %>%
  # some languages do not have macro area, let's assign them manually
  mutate(area = case_when(
    tip == "karo1304" ~ "North America",
    tip == "movi1243" ~ "South America",
    tip == "puin1248" ~ "South America",
    tip == "pume1238" ~ "South America",
    tip == "seri1257" ~ "North America",
    tip == "waor1240" ~ "South America",
    tip == "yuwa1244" ~ "South America",
    tip == "zuni1245" ~ "North America",
    TRUE ~ area
  ))

# 5 macro areas
narea <- ils %>%
  separate_rows(area, sep = ";") %>%
  distinct(area) %>%
  count() %>%
  pull()

# 31 language families and 12 isolates
nfam <- length(unique(ils$langfamily))
niso <- ils %>% filter(langfamily == "") %>% nrow()

ils <- ils %>%
  mutate(langfamily = ifelse(langfamily == "", paste0("isolate_", tip), langfamily)) %>%
  write_csv(here("data", "ids_langs.csv"))

check <- ils %>%
  group_by(langfamily) %>%
  summarise(n = sum(n)) %>%
  ungroup()

check <- ils %>%
  group_by(area) %>%
  summarise(n = sum(n)) %>%
  ungroup()




# we estimate lexicalization score for each concept using genealogically sensitive average

# create a supertree on macroareas
macroarea <- lex1 %>%
  group_by(macroarea) %>%
  summarise(n = n_distinct(glottocode)) %>%
  ungroup()

mymacro <- list( "Eurasia", "South America", "North America", "Papunesia", "Africa")
supertree <- assemble_supertree(macro_groups = mymacro)
supertree_a <- abridge_labels(supertree)
# this gives warning of labels without glottocodes were detected but they all refer to macro areas, so it should be fine

# transform nodes to tips
supertree_b <- keep_as_tip(supertree_a, label = unique(ilang$tip))
# collapse non-branching nodes
supertree_c <- collapse_node(supertree_b, label = nonbranching_nodes(supertree_b))

# make clones for dialects
dialects <- ilang %>%
  group_by(tip) %>%
  summarise(n = n_distinct(language_id)) %>%
  ungroup() %>%
  mutate(n = n - 1) %>%
  filter(n != 0)

for (i in 1:nrow(dialects)) {

  tip <- dialects$tip[i]
  n_clones <- dialects$n[i]

  supertree_c <- clone_tip(
    supertree_c,
    label = tip,
    n = n_clones,
    subgroup = TRUE
  )
}

supertree_d <- apply_duplicate_suffixes(supertree_c)
supertree_e <- rescale_branches_exp(supertree_d)
ids_tree <- rescale_deepest_branches(supertree_e, 1/40)

full_names <- ilang$language[match(ids_tree$tip.label, ilang$tip)]
name_tree <- ids_tree
name_tree$tip.label <- full_names
plot(ladderize(name_tree, right = FALSE), type = "phylogram",
     cex = 0.3, label.offset = 0.002, edge.width = 0.5)

# check if the languages are properly mapped in the tree
a <- ids_tree$tip.label
b <- ilang %>%
  group_by(tip) %>%
  summarise(n = n_distinct(language_id)) %>%
  ungroup() %>% filter(n==1) %>% select(tip) %>% pull()
setdiff(a, b)
setdiff(b, a)

# compute BM averages:

ids_data <- ilang %>%
  group_by(tip) %>%
  mutate(tip = if(n() > 1) paste0(tip, "-", row_number()) else tip) %>%
  ungroup() %>%
  select(language_id, tip) %>%
  left_join(lex1 %>% select(concepticon_id, language_id, lex), by = "language_id") %>%
  select(concepticon_id, tip, lex)

# we focus on 1222 concepts that shared across both WOLD and IDS

shared_ids <- intersect(wold_data$concepticon_id, ids_data$concepticon_id)

ids_data <- ids_data %>%
  # retain the shared concepts only
  filter(concepticon_id %in% shared_ids) %>%
  pivot_wider(names_from = concepticon_id, values_from = lex)

ids_results <- phylo_average(phy = ids_tree, data = ids_data)

ids_all <- ids_results$BM_averages %>%
  pivot_longer(cols = -tree, names_to = "concepticon_id", values_to = "lex_ids")  %>%
  select(-tree)

co <- read_csv(here("rawdata", "concepticon", "concepticon.csv"), show_col_types = FALSE) %>%
  clean_names() %>%
  rename(concepticon_id = id, concepticon_gloss = name) %>%
  mutate(concepticon_gloss = str_to_lower(concepticon_gloss),
         concepticon_gloss = str_replace(concepticon_gloss, "\\s*\\(.*\\)", "")) %>%
  select(concepticon_id, concepticon_gloss, semantic_field, ontological_category)

pos <- woldpa %>%
  select(concepticon_id, parameter_id) %>%
  left_join(wpa %>% rename(parameter_id = id) %>%
              select(parameter_id, semantic_category), by = "parameter_id") %>%
  select(concepticon_id, semantic_category) %>%
  unique()

lex_ids <- ids_all %>%
  left_join(co %>% mutate(concepticon_id = as.character(concepticon_id)), by = "concepticon_id") %>%
  left_join(pos %>% mutate(concepticon_id = as.character(concepticon_id)), by = "concepticon_id") %>%
  select(concepticon_id, concepticon_gloss, semantic_field, ontological_category,
         semantic_category, lex_ids) %>%
  # The phyloWeights package relies on floating-point arithmetic, which can produce
  # very small numerical artifacts (e.g., 1e-16 instead of 0, or 0.999999999999 instead of 1).
  # To eliminate these artifacts and standardize results, we round lexicalization scores to 6 decimals.
  mutate(lex_ids = round(lex_ids, 6))

# there are some missing entries for POS
# let's use ontological category to fill out POS
lex_ids2 <- lex_ids %>%
  mutate(semantic_category = case_when(
    ontological_category == "Action/Process" ~ "Verb",
    ontological_category == "Number" ~ "Function word",
    ontological_category == "Other" ~ "Function word",
    ontological_category == "Person/Thing" ~ "Noun",
    ontological_category == "Property" ~ "Adjective",
    TRUE ~ semantic_category
  )) %>%
  write_csv(here("data", "lex_ids.csv"))

expect_equal(any(is.na(lex_ids2)), FALSE)
# there should be 1222 unique concepts
expect_equal(length(unique(lex_ids2$concepticon_id)), 1222)


# now we estimate phylogenetic mean for WOLD
# because concepts have different representations of languages,
# we need to create a tree and estimate phylogenetic mean for each concept reiteratively

# first create supertree on macro areas
warea <- wold_data %>%
  select(tip) %>%
  unique() %>%
  left_join(glottolog %>% select(glottocode, area) %>%
              rename(tip = glottocode), by = "tip") %>%
  group_by(area) %>%
  summarise(n = n_distinct(tip)) %>%
  ungroup()

wmacro <- list("Eurasia", "Africa", "South America", "North America", "Papunesia", "Australia")
supertree <- assemble_supertree(macro_groups = wmacro)
supertree_a <- abridge_labels(supertree)
# this gives warning of labels without glottocodes were detected but they all refer to macro areas, so it should be fine

# create a concept list
concepts <- shared_ids
results_list <- list()

pdf(NULL)
for (concept in concepts) {

  print(concept)

  wold_data_filtered <- wold_data %>%
    filter(concepticon_id == concept)

  supertree_b <- keep_as_tip(supertree_a, label = wold_data_filtered$tip)
  supertree_c <- collapse_node(supertree_b, label = nonbranching_nodes(supertree_b))
  supertree_d <- rescale_branches_exp(supertree_c)
  wold_tree <- rescale_deepest_branches(supertree_d, 1/40)

  wold_results <- phylo_average(phy = wold_tree, data = wold_data_filtered)
  bm <- wold_results$BM_averages$lex

  results <- tibble(
    concepticon_id = concept,
    lex_wold = bm
  )

  results_list[[concept]] <- results
}
dev.off()

wold_results <- bind_rows(results_list)

lex_wold <- wold_results %>%
  left_join(co, by = "concepticon_id") %>%
  left_join(pos, by = "concepticon_id") %>%
  select(concepticon_id, concepticon_gloss, semantic_field, ontological_category, semantic_category, lex_wold) %>%
  # The phyloWeights package relies on floating-point arithmetic, which can produce
  # very small numerical artifacts (e.g., 1e-16 instead of 0, or 0.999999999999 instead of 1).
  # To eliminate these artifacts and standardize results, we round lexicalization scores to 6 decimals.
  mutate(lex_wold = round(lex_wold, 6)) %>%
  write_csv(here("data", "lex_wold.csv"))

# there should not be any missing entries
expect_equal(any(is.na(lex_wold)), FALSE)
# there should be 1222 unique concepts
expect_equal(length(unique(lex_wold$concepticon_id)), 1222)



# now we estimate phylogenetic mean for WOLD data that has stricter criteria on word-meaning relationship
warea <- wold_data_strict %>%
  select(tip) %>%
  unique() %>%
  left_join(glottolog %>% select(glottocode, area) %>%
              rename(tip = glottocode), by = "tip") %>%
  group_by(area) %>%
  summarise(n = n_distinct(tip)) %>%
  ungroup()

wmacro <- list("Eurasia", "Africa", "South America", "North America", "Papunesia", "Australia")
supertree <- assemble_supertree(macro_groups = wmacro)
supertree_a <- abridge_labels(supertree)
# this gives warning of labels without glottocodes were detected but they all refer to macro areas, so it should be fine

# create a concept list
concepts <- intersect(shared_ids, unique(wold_data_strict$concepticon_id))
results_list <- list()

pdf(NULL)
for (concept in concepts) {

  print(concept)

  wold_data_filtered <- wold_data_strict %>%
    filter(concepticon_id == concept)

  supertree_b <- keep_as_tip(supertree_a, label = wold_data_filtered$tip)
  supertree_c <- collapse_node(supertree_b, label = nonbranching_nodes(supertree_b))
  supertree_d <- rescale_branches_exp(supertree_c)
  wold_tree <- rescale_deepest_branches(supertree_d, 1/40)

  wold_results <- phylo_average(phy = wold_tree, data = wold_data_filtered)
  bm <- wold_results$BM_averages$lex

  results <- tibble(
    concepticon_id = concept,
    lex_wold_strict = bm
  )

  results_list[[concept]] <- results
}
dev.off()

wold_strict_results <- bind_rows(results_list)

lex_wold_strict <- wold_strict_results %>%
  left_join(co, by = "concepticon_id") %>%
  left_join(pos, by = "concepticon_id") %>%
  select(concepticon_id, concepticon_gloss, semantic_field, ontological_category, semantic_category, lex_wold_strict) %>%
  # The phyloWeights package relies on floating-point arithmetic, which can produce
  # very small numerical artifacts (e.g., 1e-16 instead of 0, or 0.999999999999 instead of 1).
  # To eliminate these artifacts and standardize results, we round lexicalization scores to 6 decimals.
  mutate(lex_wold_strict = round(lex_wold_strict, 6)) %>%
  write_csv(here("data", "lex_wold_strict.csv"))

# there should not be any missing entries
expect_equal(any(is.na(lex_wold_strict)), FALSE)
# there should be 762 unique concepts
expect_equal(length(unique(lex_wold_strict$concepticon_id)), 762)






# 3) to deal with synonymy and polysemy issues

co <- read_csv(here("rawdata", "concepticon", "concepticon.csv"), show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(name = str_to_lower(name)) %>%
  rename(concepticon_id = id)

wordnet <- read_tsv(here("rawdata", "concepticon", "wordnet.tsv"), show_col_types = FALSE) %>%
  clean_names()

allpa <- idspa %>%
  select(-concepticon_gloss, -name, - name_clean) %>%
  bind_rows(woldpa %>% select(-parameter_id, -name, - name_clean)) %>%
  unique() %>%
  left_join(co %>% select(concepticon_id, name, description), by = "concepticon_id") %>%
  left_join(wordnet %>% select(concepticon_id, wordnet_synset, wordnet_gloss, wordnet_definition), by = "concepticon_id") %>%
  rename(pwn_synset = wordnet_synset) %>%
  filter(concepticon_id %in% shared_ids) %>%
  write_csv(here("data", "wordnet_mapping.csv"))

names <- idspa %>%
  select(concepticon_id, name_clean) %>%
  bind_rows(woldpa %>% select(concepticon_id, name_clean)) %>%
  unique() %>%
  filter(concepticon_id %in% shared_ids) %>%
  write_csv(here("data", "lemma_mapping.csv"))





# 4) Prepare data for identifying implicational universals

co <- read_csv(here("rawdata", "concepticon", "concepticon.csv"), show_col_types = FALSE) %>%
  clean_names() %>%
  rename(concepticon_id = id, concepticon_gloss = name) %>%
  mutate(concepticon_gloss = str_to_lower(concepticon_gloss),
         concepticon_gloss = str_replace(concepticon_gloss, "\\s*\\(.*\\)", "")) %>%
  select(concepticon_id, concepticon_gloss, semantic_field, ontological_category)

a <- wold_data %>%
  left_join(co, by = "concepticon_id") %>%
  select(-ontological_category) %>%
  rename(glottocode = tip) %>%
  left_join(glottolog %>% select(glottocode, language), by = "glottocode") %>%
  left_join(wlangs %>% distinct(glottocode, area, langfamily), by = "glottocode") %>%
  # manually assign area and family for Zinacantán Tzotzil
  mutate(area = ifelse(glottocode == "tzot1259", "North America", area),
         langfamily = ifelse(glottocode == "tzot1259", "Mayan", langfamily)) %>%
  # English has multiple assignments of area, we retain Eurasia only
  mutate(area = ifelse(glottocode == "stan1293", "Eurasia", area)) %>%
  rename(family= langfamily) %>%
  write_csv(here("data", "wold_data.csv"))

b <- lex1 %>%
  filter(concepticon_id %in% shared_ids) %>%
  left_join(co, by = "concepticon_id") %>%
  select(-ontological_category, -family, -macroarea) %>%
  left_join(la3 %>% select(id, name, macroarea, family) %>% rename(language_id = id) %>%
              mutate(language_id = as.character(language_id)), by = "language_id") %>%
  select(concepticon_id, language_id, glottocode, langname = name, lex, concepticon_gloss, semantic_field, area=macroarea, family) %>%
  write_csv(here("data", "ids_data.csv"))

