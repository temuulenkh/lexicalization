# Raw data

This folder contains the raw data used for preprocessing.

## Lexicalization data

#### WOLD

* Files in `wold` folder : WOLD version 2009 was downloaded from (https://wold.clld.org/download) on 25 Nov 2024. The file `missing.csv` was provided directly by Martin Haspelmath, and `word_meaning_relation.csv` was obtained from raw FileMaker Pro files of the WOLD dataset, also provided by him.

#### IDS

* Files in`ids` folder : IDS version 4.3 was downloaded from (https://zenodo.org/records/7701635) on 11 Nov 2024.

## Usage frequency data

#### Worldlex

* `limetal_worldlexfrequency.txt` : Preprocessed data on 23 languages was downloaded from https://zenodo.org/records/10417742 and its original name was `word_normalized_frequency_23.txt`.
* Files in `worldlex` folder : Raw Worldlex data as downloaded from http://worldlex.lexique.org.

#### DoReCo

* Files in `doreco` : DoReCo 2.0 was downloaded from https://doreco.huma-num.fr/languages on 24 Feb 2025. Note that Hoocąk data is not included in this folder due to copyright reasons, but can be downloaded from the link provided.

## Data on other predictors

#### Concreteness

* `brysbaertetal_concreteness.xlsx` : downloaded from https://link.springer.com/article/10.3758/s13428-013-0403-5#MOESM1.

#### Word association

* Files in `swow` : downloaded from https://github.com/SimonDeDeyne/SWOWEN-2018/tree/master/output.

#### Age of acquisition (self report)

* `kupermanetal_aoa.xlsx` : downloaded from https://osf.io/kz2px/ and its original name was `AoA_51715_words.xlsx`. The self-reported AoA measure was originally derived from Kuperman et al. 2012.

#### Age of acquisition (parent report)

* `portelanceetal_aoa.rds` : downloaded from https://github.com/evaportelance/multilingual-aoa-prediction/tree/c0724bc3f22dcebafb52dd8df6c10b8d70fef0e3/Analyses/data and its original name was `aoa_predictor_data.rds`.

#### Sensorimotor

* `lynottetal_sensorimotor.csv` : downloaded from https://osf.io/rwhs6/files/osfstorage on 8 May 2025.

#### NoRaRe

* Files in `norare` folder : downloaded on 5 March 2025 from https://zenodo.org/records/14925245.

#### SUBTLEX-UK data

* `vanheuvenetal_subtlexuk.xlsx` : downloaded from https://osf.io/zq49t/ on 22 September 2025 and its original name was `SUBTLEX-UK.xlsx`.

## Data sets used for pair analyses

* `clics.sqlite` : downloaded from https://zenodo.org/records/3687530 on 15 May 2025.

## Other miscellaneous data

#### Data used to add concept description and map WOLD, IDS concepts with Concepticon ID and WordNet synsets

* Files in folder `concepticon` : downloaded from Concepticon version 3.3.0 (https://zenodo.org/records/14622303) except for `wordnet.tsv` which was retrieved from Concepticon 2.6.0.

#### Word sense disambiguation data on COHA

* `wsd_coha.pkl` : downloaded from https://osf.io/dmgh6/.

#### Data used to extract information from raw Worldlex files

* `worldlex2isocode.csv` : manually created to match Worldlex language code with ISO code.
* `worldlexlanguages.csv` : manually created to match Worldlex language code with language name.

#### Data used to map ISO two-letter code to glottocode

* `wiktionary_langs.tsv`: manually extracted from Wiktionary (https://en.wiktionary.org/wiki/Wiktionary:List_of_languages)
* `wik_twocodes.tsv` : manually extracted from Wiktionary (https://en.wikipedia.org/wiki/List_of_ISO_639_language_codes)

## References

* Kuperman, V., Stadthagen-Gonzalez, H., & Brysbaert, M. (2012). Age-of-acquisition ratings for 30,000 English words. Behavior research methods, 44, 978-990.
