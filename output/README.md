## Outputs

This folder contains:

* Figures produced by analysis scripts in `figures` folder
* Tables produced by analysis scripts in `tables` folder
* Results produced by analysis scripts in `results` folder: the lists of candidate implicational universals, the Bayesian model results, and the frequentist logistic regression results with macro area as a random effect.

### Candidate implicational universals

We identified cases in which the pattern "If a language has a word for concept $c_2$, then it also has a word for concept $c_1$" held across languages. The lists of candidate implicational universals are saved as `imp_wold.csv` (based on WOLD data) and `imp_ids.csv` (based on IDS data) in the `results` folder. See the explanations of columns as follows: 

* `semantic_field` : The domain within which the pattern was identified

* `concepticon_id1` : Concepticon ID for concept $c_1$

* `concepticon_gloss1` : Concepticon gloss for concept $c_1$

* `concepticon_id2` : Concepticon ID for concept $c_2$

* `concepticon_gloss2` : Concepticon gloss for concept $c_2$

* `evidence` : The number of languages which attest the pattern

