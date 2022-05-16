# Adaptive-deprivation-measure

## Aims
The aim of this work is to show how deprivation scales can be implemented much more efficiently in household surveys using insights from psychology and educational research which have led to the development of adaptive measures. These have their basis in Item Response Theory, a foundation shared by deprivation scales. The approach is applied to the deprivation data collected annually by the UK Government through the Family Resources Survey (FRS). 

## Outputs
The work has been published in the journal 'Social Indicators Research' (Jan 2020 - Open Access). https://doi.org/10.1007/s11205-020-02283-1

## Data
Data for the FRS survey were obtained under licence from the UK Data Service (UKDS). The data were obtained as two linked data collections: the FRS data (DWP/NatCen/ONS 2019) and the associated HBAI dataset which contains derived variables used in the construction of the HBAI reports (DWP 2019b). The data are linked using unique reference numbers for each case. 

## Files
Dataset preparation was done using SPSS. The .sps file produces a csv file containing data for each child on material deprivation items lacked. 

Analytical work was done using R v3.6.0 (R Core Team, 2013) with packages ‘dplyr’ (Wickham et al 2019), ‘tidyverse’ (Wickham 2017), and ‘ltm’ (Rizopoulos 2006) current at May 2019. The .R file reads in the .csv file and produces all relevant tables and figures contained in the journal paper. 
