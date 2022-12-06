setwd(" ") #set working directory

source("functions.R") #this should be in the same folder used for working directory

#this performs the test for all coefficients for one configuration of the parameters
#the output is a list containing one data frame with all the coefficients and
#another data frame with the rank of the correct author for each coefficient
res = test_coefficients(corpus = 'data/', #folder containing corpus to test
                         feature = 'hyb', #feature type (words, chars, POS, or hyb (called 'frames' in the Element)). For POS or hybrid the data has to be POS-tagged
                         n = 2, #length of n-gram
                         q.sample = 500, #length of simulated disputed sample
                         k.sample = 10000) #length of comparison data per candidate

#this function calibrates the results above into log-likelihood ratios
llr.res = calibrate.llr(background = res$coefficient_table %>% slice_head(prop = 0.5), #result table to be used as background population; here simulating top half of table for background
                        test = res$coefficient_table %>% slice_tail(prop = 0.5), #result table to be used for test; using remaining half of table
                        coeff = "simpson") #coefficient to isolate from results table

#this is for qualitative explorations of the unique n-grams
#the final list contains features that are only used by one author in at least 2 texts
list = extract_unique_ngrams(corpus = 'data/', #folder containing corpus to test
                             feature = 'hyb', #feature type (words, chars, POS, or hyb (called frames in paper)). For POS or hybrid the data has to be POS-tagged
                             n = 2) #length of n-gram

                        