# Gossip in Hungarian high schools

[![R language](https://img.shields.io/badge/language-R-blue)](https://www.r-project.org/)
[![ERC funding](https://img.shields.io/badge/funding-ERC-green)](https://cordis.europa.eu/project/id/648693)
[![CC BY 4.0](https://img.shields.io/badge/license-CC_BY_4.0-red)](https://creativecommons.org/licenses/by/4.0/)

This repository contains the replication package for the article:
- Estévez, J. L., Kisfalusi, D., & Takács, K. (2022). More than one's negative ties: The role of friends' antipathies in high school gossip. *Social Networks, 70*, 77-89. https://doi.org/10.1016/j.socnet.2021.11.009.

Please note that this dataset contains only a subset of the variables collected. The complete dataset is freely available upon registration at:
- Vörös, A., Boda, Z., Néray, B., Pál, J., Kisfalusi, D., Samu, F., Vit, E., Radó, M., Habsz, L., Csaba, Z., Lőrincz, L., Mandácskó, E., Panyik, B., Varga, K., Mezei, G., Makovi, K., Boldvai-Pethes, L., Havelda, A., Bartus, T., & Takács, K. (2022). *Wired into Each Other: Network Dynamics of Adolescents in Hungarian Secondary Schools: 2010-2013*. [data collection]. UK Data Service. SN: 855460, https://doi.org/10.5255/UKDA-SN-855460.

## Software requirements

R (code was last run with R version 4.1.2 in RStudio 2021.09.0)
- statnet (2019.6)
- Bergm (5.0.2)
- tidyverse (1.3.1)
- ggplot2 (3.3.5)
- ggrepel (0.9.1)
- ineq (0.2-13)
- networkD3 (0.4)

## File list

- 1.1_Ancillary_analyses_and_extra_plots.R
- 1.2_Data_tidying.R
- 2.1_Descriptive_stats.R
- 2.2_Matrix_overlap_and_ineq_comparison.R
- 2.3_BERGM_analysis.R
- 2.4_BERGM_analysis_null.R
- 3.1_Results.R
- 3.2_Results_null.R
- data.RData
