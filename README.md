# NPmeta_ms_data
Data and code for "Leaf and whole-plant responses to nitrogen and phosphorus addition are governed by climate, acquisition strategy, and additive interactions" led by Evan Perkowski and others. 

Manuscript is soon to be submitted to *Global Change Biology*.

Repository contents:
 - `data` = folder contents include the compiled data file (`data/CNPmeta_data_compiled.csv`), the dataset including log-response ratios (`data/CNPmeta_logr_results.csv`), the dataset including interaction effect sizes (`data/CNPmeta_results_int.csv`), and all associated meta-data (`data/CNPmeta_data_compiled_metadata.xlsx`, `data/CNPmeta_results_logr.xlsx`, and `data/CNPmeta_results_int.xlsx`).

 - `functions` = folder contents include a custom function for iterating meta-regression models across a series of variables for individual (`functions/analyse_meta.R`) and interaction (`functions/calc_intxn_effSize_meta.R`). Custom function was modified from Benjamin D. Stocker- original R code can be found [here](https://github.com/geco-bern/lt_cn_review/blob/main/R/analyse_meta.R).

 - `plots` = folder contents include all figures included in manuscript. Supplemental figures are also supplied in the `supplement` subfolder.

 - `scripts` = folder contents include an R script for individual and interaction analyses (`data/CNPmeta_analysis.R`), individual climate and plant functional type moderator analyses (`data/CNPmeta_moderators_individual.R`), and interaction climate and plant functional type moderator analyses (`data/CNPmeta_moderators_interaction.R`). Folder also includes an R script for plot-making (`data/CNPmeta_plots.R`).

 - `tables` = folder contents include all table outputs from scripts.

For all questions and requests about data usage, please contact Evan Perkowski at [evan.a.perkowski@ttu.edu](evan.a.perkowski@ttu.edu).
