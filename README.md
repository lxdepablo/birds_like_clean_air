# Birds Like Clean Air
A case study using SAGE/CROCUS data to study the effect of the 2025 Chicago haboob on bird species richness in the Chicago area. There are three steps in the analysis pipeline. First, there is a [data pulling script](https://github.com/lxdepablo/birds_like_clean_air/blob/main/code/pull_crocus_data.R) which downloads data from the CROCUS API, using the [sageDataR](https://github.com/lxdepablo/sageDataR) library. These data are written to the [data folder](https://github.com/lxdepablo/birds_like_clean_air/tree/main/data). Next, a [data wrangling script](https://github.com/lxdepablo/birds_like_clean_air/blob/main/code/data_wrangling.R) reads in the previously downloaded data and prepares it for analysis, including summarizing, reshaping, and joining. Clean data are again written to the data folder. Finally, [the modeling script](https://github.com/lxdepablo/birds_like_clean_air/blob/main/code/modeling_and_vis.R) uses the clean data to fit models and generate visualizations.

# License
GNU GPL v3.0

# Authorship
All code was written and conceived by Luis X. de Pablo, with assistance from a large language model.
