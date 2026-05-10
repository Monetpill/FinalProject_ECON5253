### ECON 5253 Final Project | University of Oklahoma | For the Economics Wizard  (Dr. R)
This repository is my submission for Dr. Ransom's Final Project. 

```
 ／￣￣ヽ                    🐝
（ • ω •）   🐝    
 ／|🍯|ヽ
  bzz bzz           🐝
```

## Steps for replicating the results of this project ##

1. The .data folder in this repository contains the cleaned datasets cps_clean.rds and qcew_cps_panel.rds. You don't have to use them, but I included them as an alternative route in case you didn't have 1.5 hours to gather the QCEW data.
2. If you choose not to use the provided datasets, I created a separate R script dedicated to gathering the data from scratch. You can find it in the .R folder as 01_build_data.R.
3. If you choose to use the provided datasets, the script you would run is 02_analysis.R.
4. Make sure to set your working directory to the same location as the saved files so you can load them via readRDS().
5. Run the respective R script based on your choice in step 1.

NOTE: Once you finish running 01_build_data.R, you can proceed with 02_analysis.R to replicate results

