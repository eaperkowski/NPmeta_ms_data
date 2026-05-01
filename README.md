# NPmeta_ms_data
Data and code for "Leaf and whole-plant responses to nitrogen and phosphorus addition are governed by climate, acquisition strategy, and additive interactions" led by Evan Perkowski and others and soon to be submitted to *Global Change Biology*.

Repository contents:
 - `data`          = folder contents include the compiled data file (data/CNPmeta_compiled_data.csv) and metadata (data/CNPmeta_compiled_metadata.csv)

 - `scripts`       = folder contents include an R script for individual and interaction analyses (data/CNPmeta_analysis.R), individual climate and plant functional type moderator analyses (data/CNPmeta_moderator_ind.R), and interaction climate and plant functional type moderator analyses (data/CNPmeta_moderator_int.R). Folder also includes an R script for plot-making (data/CNPmeta_plots.R)

 - `functions`     = folder contents include a custom function for iterating meta-regression models across a series of variables for individual (functions/meta_analyse.R) and interaction (functions/meta_analyse_int.R). Custom function was modified from Benjamin D. Stocker and can be sourced [here](https://github.com/geco-bern/lt_cn_review/blob/main/R/analyse_meta.R).

For all questions and requests about data usage, please contact Evan Perkowski at [evan.a.perkowski@ttu.edu](evan.a.perkowski@ttu.edu).
