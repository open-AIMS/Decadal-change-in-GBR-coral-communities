# Decadal-change-in-GBR-coral-communities
Code in support of manuscript analyses

The data associated with this repo are housed in a Figshare repository https://figshare.com/s/e469ed703629d2d0a785

All analyses were comducted in R version 4.4.1

##Code Functionality/analysis workflow 

The analysis code implements a reproducible workflow for importing, cleaning and analysing long-term coral reef monitoring data. In brief, the scripts load benthic and coral community datasets, standardise taxonomic and sampling identifiers, remove incomplete records, and aggregate observations to the reef, site, transect or survey-year or half-decadal scale as required. The code then derives response variables including hard coral cover, coral genus cover, species richness and multivariate assemblage matrices. Statistical models are fitted to estimate temporal changes in cover, richness and assemblage composition while accounting for the hierarchical sampling design. Model outputs are used to calculate contrasts among pre-disturbance, post-disturbance and recovery periods, including uncertainty intervals and recovery probabilities. Additional scripts perform multivariate analyses of community composition, quantify compositional change through time, and generate all figures and supplementary tables reported in the manuscript.


In order to run the code, the specific datasets need to be added to the appropriate folders. these are: 

Fig 1 
- comp2021_Oct2025_df.RData, 
- reeflatlong.RData
- reefsec.RData
- subregion_dist_recovery.csv
- dist_lookup_short.RData
- reefs.RData
- sectors.RData
- towns.sf.RData
        
Fig 2 
- comp2021.w.RData

Fig 3
- long_series_full.RData
- site.scores.combined_full.RData
- vec.labels.RData

Fig 4
- site.scores.combined_full.RData
- subregion_recovery_centroid_dist.RData

Fig 5
- comp2021_Oct2025.RData
- group_cover_transect.RData
- comp21_broad_groups_lookup.csv

Fig 6
- old.growth.weeds.RData
        
Code for each figure is included in its separate file in this repo. 
