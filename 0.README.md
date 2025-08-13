# WFH_Income_Consumption
Assessing the effect of WFH on income and consumption using difference-in-differences technique

## Code package: R. Rohijas (2025). Assessing the effect of WFH on income and consumption.

## Data for Empirical Project
FOr this empirical project, we used the repeated cross-sectional data from the UK Living Cost and Food Survey (LCFS) produced by the Office for National Statistics.
The dataset and the data dictionary is available as part of the download package from the UK Data Services. Search for the Series Number: SN 2000028

## Estimation files
The full project script is uploaded together with the report under 5 separate files (3 scripts for pre-processing and 2 scripts for estimation)

### Pre-processing steps
We utilised three machine learning models to create a pseudo-WFH indicator for the pre-2020 LCFS dataset. This step is necessary as the WFH ability indicator was only present in surveys from 2020 onwards. We predicted the likelihood of respondents in the pre-2020 dataset using classification algorithms trained on the 2020 LCFS dataset.

### Estimation file
We estimated the effect of WFH on income and consumption using the difference-in-differences technique. Each step is labelled accordingly in the project script.
Replication codes come without technical support of any kind.
