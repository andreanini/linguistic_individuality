# A Theory of Linguistic Individuality for Authorship Analysis
[![DOI](https://zenodo.org/badge/573513067.svg)](https://zenodo.org/badge/latestdoi/573513067)

Data and code accompanying the monograph "A Theory of Linguistic Individuality for Authorship Analysis":

 > Nini, A. (2023). A Theory of Linguistic Individuality for Authorship Analysis. Elements in Forensic Linguistics. Cambridge, UK: Cambridge University Press.

The repository contains two R scripts and a zipped folder with the data sets.

## Main Script
The three main functions are:
- `test_coefficients()`: this function takes as input one folder of texts and returns the results for all the coefficients tested in the monograph for a single combination of parameters.
- `calibrate.llr()`: this function takes as input two result tables, one for the background data and one for the test data and returns scores calibrated into log-likelihood ratios.
- `extract_unique_ngrams()`: takes a folder of texts as input and returns the set of n-grams used by only one author in at least 2 different texts.

More explanations and the values for the parameters are explained in the comments in the script itself. The functions are sourced from the `functions.R` script.

## Data
The two data sets used in the monograph are the English part of the _refocor_ corpus and the _c50_ corpus. References to both of them are in the monograph. The Data folder contains these two corpora in their plain and POS-tagged versions.
