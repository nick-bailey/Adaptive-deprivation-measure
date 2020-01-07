# Adaptive-deprivation-measure

## Aims
The aim of this work is to show how deprivation scales can be implemented much more efficiently in household surveys using insights from psychology and educational research which have led to the development of adaptive measures. These have their basis in Item Response Theory, a foundation shared by deprivation scales. A paper from this work will be available shortly explaining the theoretical basis and illustrating the approach using data from the UK's Family Resources Survey (FRS). 

## Outputs
The work has been presented at a couple of conferences. A journal paper is due for publication shortly (2020). Details will be added here after final acceptance. 

## Data
Data for the FRs survey were obtained under licence from the UK Data Service (UKDS). Data were obtained from the UK Data Service (UKDS) as two linked data collections: the FRS data (DWP/NatCen/ONS 2019) and the associated HBAI dataset which contains derived variables used in the construction of the HBAI reports (DWP 2019b). The data are linked using unique reference numbers for each case. 

## Files
Dataset preparation was done using SPSS. The .sps file produces a csv file containing data for each child on material deprivation items lacked. 

Analytical work was done using R v3.6.0 (R Core Team, 2013) with packages ‘dplyr’ (Wickham et al 2019), ‘tidyverse’ (Wickham 2017), and ‘ltm’ (Rizopoulos 2006) current at May 2019. The .R file reads in the .csv file and produces all relevant tables and figures contained in the journal paper. 
