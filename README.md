# Gossip in Hungarian high schools

This repository contains the replication package for the article:
Estévez, J. L., Kisfalusi, D., & Takács, K. (under review). More than one's negative ties: The role of friends' antipathies in high school gossip. *journal*. DOI: .

**Software requirements**

R (code was last run with R version 4.1.2 in RStudio 2021.09.0)
- statnet (2019.6)
- Bergm (5.0.2)
- tidyverse (1.3.1)
- ggplot2 (3.3.5)
- ggrepel (0.9.1)
- ineq (0.2-13)
- networkD3 (0.4)

**File list**

- 1.1_Ancillary_analyses_and_extra_plots.R
- 1.2_Data_tidying.R
- 2.1_Descriptive_stats.R
- 2.2_Matrix_overlap_and_ineq_comparison.R
- 2.3_BERGM_analysis.R
- 2.4_BERGM_analysis_null.R
- 3.1_Results.R
- 3.2_Results_null.R
- data.RData

**Instructions**

Open RStudio and create a new project, preferably in a new directory (the name of this project is up to you). This creates a new folder with a single .Rproj file named after the name you chose. In this folder, drag all the files mentioned in the file list. After this, both the data and code must be available within RStudio (see tab “files”) and can be accessed by simply clicking on them.

The code needs to be run in order. This means, at this first stage, only the files 1.1_Ancillary_analyses_and_extra_plots.R and 1.2_Data_tidying.R can be run. Once 1.2_Data_tidying.R has been run, it automatically creates a new .RData file called tidieddata.RData. This new file enables running all the files starting with 2. Similarly, running 2.3_BERGM_analysis.R creates a new .RData file called bergm_results.RData which is the input for 3.1_Results.R. And running 2.4_BERGM_analysis_null.R creates bergm_results_null.RData which is the input for 3.2_Results_null.R.

Besides .RDdata files, multiple .jpeg images and .csv files are produced. These are the figures and tables reported in the article. 

For descriptive analyses, run  2.1_Descriptive_stats.R and 2.2_Matrix_overlap_and_ineq_comparison.R.

For the main analyses, run 2.3_BERGM_analysis.R followed by 3.1_Results.R.

For the main analyses without control variables, run 2.4_BERGM_analysis_null.R followed by 3.2_Results_null.R.
