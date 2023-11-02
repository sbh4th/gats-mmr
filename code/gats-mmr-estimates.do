import spss using "data-clean/FOR MULTINOMIAL MULTILEVEL REGRESSION.sav"

svyset gatscluster [pweight= gatsweight], strata(gatsstrata)
svydes

tab polytob_recode
svy: tabulate polytob_recode
svy: mlogit polytob_recode
svyset gatscluster [pweight= gatsweight], strata(gatsstrata) singleunit(scaled)
svy: mlogit polytob_recode
svyset gatscluster [pweight= gatsweight], strata(gatsstrata) singleunit(centered)
svy: mlogit polytob_recode
margins
