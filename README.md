### ADOPT_FLSDB_analysis

Repo contains R code that was used to clean and analyse FLS data pre- and post-AI implementation. 
The R code contains inline instructions describing analyses steps.



## Requirements
- R
- Required packages: tidyverse, readxl, visdat, mice, ggmice, ggpubr, plyr

## Usage
- The code was applied to patient-level FLS data from three FLSs that implemented that ADOPT AI pathway (active sites) and nine sites that did not implement that AI pathway and were matched to each active site based on their pre-AI vertebral fracture (VF) identification (control sites)
- Code was used to calculate performance measures for the active sites pre- and post-AI implementation for use in the health economic models
- Similarly for control sites, measures were calculated as comparators for the active sites. Instead of pre- and post-AI, the periods for the control sites were matched to the dates at which the active sites had the AI pathway turned on.
- Some of the extracted measures were also the FLS-DB KPIs and were subsequently summarised and compared in the main manuscript.

- 
```r
source("script_name.R")
