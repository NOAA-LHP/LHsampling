#  for Eva 30Mar
#	Erin updated the LH_sample function to fix the bug that was causing the error on S3_A5
#		which was happening because in some instances R was trying to supplemental sample when none
#		were available (depending on supp_min_length)
#	  I also changed the FOS and POS supplemental sampling and sample reduction so that the number of
#	  	samples per length bin were more even and to ensure that supplemental samples were collected
#		from missing bins if they were bracketed by non-missing bins.
#		I don't think this will make much difference, but is is much prettier and more correct.

#	I also made a new POS supplemental sampling approach to eliminate the variation in number of samples per bin
#		between bootstraps that was happening before. I think the sample_n() function is not robust enough to use
#		the large vector of weights (nrows of harvest, so 10s of thousands) to select samples in a reproducible manner
#		also, if two bins had the same weight (prop1) then it would have been random between the two which got sampled,
#		causing the variation between runs.

# ----- To use the NEW POS sampling:
#   source("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_Functions\\MUS_Functions_Code_POS_new.R")
# ----- To use the old POS sampling:
#   source("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_Functions\\MUS_Functions_Code_POS_old.R")

#  In POS_new, binl and propl have been changed to binL and propL to avoid ambiguous l letter vs. number

#	Run times are longer now

#	I added an output to the sampling function that contains histograms for 9 random bootstraps
#		this way, we can easily check and make sure the sampling is producing what we had in mind.

#	see the new function LH_plot(). The only argument is the output from the LH_sample function.
#		it will create several plots that you can check out to make sure the simulation and the sampling are behaving
#		as you expected.


# rm(list=ls())

#  load packages
library(reshape)
library(dplyr)
library(ggplot2)
library(magrittr)
library(assertthat)

# read functions from the source R script
source("Documents\MUS_Sample_Design\\MUS_Functions\\MUS_Functions_Code_POS_new.R")

# or load the workspace containing the function.

# in the future, we can replace this step with a package 

# load the simulated populations
# load("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_examples\\Mar29_simulated_pops.RData")



#  ---------- re-run all S3_Auric_highF
#	will run a lot faster if save_bootstraps = FALSE

##### S3_Auric_highF ######

S1_A1 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2)

S1_A2 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A3 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A4 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A5 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A6 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A7 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A8 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A9 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 100, samp_size = 300, 
                   sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A10 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A11 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A12 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A13 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A14 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A15 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A16 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A17 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A18 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A19 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A20 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A21 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A22 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A23 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A24 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 




##### S3_Auric_highF ######

S3_A1 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2)

S3_A2 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A3 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A4 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A5 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A6 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A7 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A8 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A9 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 100, samp_size = 300, 
                   sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A10 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A11 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A12 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A13 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A14 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A15 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A16 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A17 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A18 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A19 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A20 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A21 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A22 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A23 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A24 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 





##### S4_Auric_lowF ######

S4_A1 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2)

S4_A2 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A3 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A4 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A5 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A6 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A7 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A8 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A9 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 100, samp_size = 300, 
                   sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A10 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A11 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A12 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A13 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A14 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A15 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A16 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A17 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A18 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A19 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A20 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A21 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A22 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A23 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A24 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 




S6_A1 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2)

S6_A2 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A3 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A4 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A5 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A6 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A7 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A8 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A9 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 100, samp_size = 300, 
                   sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A10 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A11 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A12 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A13 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A14 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A15 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A16 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A17 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A18 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A19 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A20 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A21 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A22 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A23 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S6_A24 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 




#  use LH_plot all_plots=TRUE to show us characteristics of the simulation input, population, and catch.
#	only need to do this once since those plots will be same for all S3:
LH_plot(S3_A1, all_plots=TRUE)

#  highly recommended to examine some bootstraps for each sampling approach, to ensure that the sample distribution
#	looks as we expect.
 

LH_plot(S3_A1, all_plots=TRUE)
LH_plot(S3_A2, all_plots=FALSE)
LH_plot(S3_A3, all_plots=FALSE)
LH_plot(S3_A4, all_plots=FALSE)
LH_plot(S3_A5, all_plots=FALSE)
LH_plot(S3_A6, all_plots=FALSE)
LH_plot(S3_A7, all_plots=FALSE)
LH_plot(S3_A8, all_plots=FALSE)
LH_plot(S3_A9, all_plots=FALSE)
LH_plot(S3_A10, all_plots=FALSE)
LH_plot(S3_A11, all_plots=FALSE)
LH_plot(S3_A12, all_plots=FALSE)
LH_plot(S3_A13, all_plots=FALSE)
LH_plot(S3_A14, all_plots=FALSE)
LH_plot(S3_A15, all_plots=FALSE)
LH_plot(S3_A16, all_plots=FALSE)
LH_plot(S3_A17, all_plots=FALSE)
LH_plot(S3_A18, all_plots=FALSE)
LH_plot(S3_A19, all_plots=FALSE)
LH_plot(S3_A20, all_plots=FALSE)
LH_plot(S3_A21, all_plots=FALSE)
LH_plot(S3_A22, all_plots=FALSE)
LH_plot(S3_A23, all_plots=FALSE)
LH_plot(S3_A24, all_plots=FALSE)








# in this example, the user asked for a large number of 'supplemental' samples compared to the FOS design
#	a warning message lets the user know this might be weird.
S3_testing <- LH_sample(sim_output = S3_Auric_highF, n_boots = 100, samp_size = 75, 
                   sample_type = 'FOS', supp_large = TRUE, supp_large_n_per_bin = 10,
                   supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 5,
                   supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 
LH_plot(S3_testing, all_plots=FALSE)






# make some plot jpgs for Eva

list_outputs <- c('S3_A1',
'S3_A2',
'S3_A3',
'S3_A4',
'S3_A5',
'S3_A6',
'S3_A7',
'S3_A8',
'S3_A9',
'S3_A10',
'S3_A11',
'S3_A12',
'S3_A13',
'S3_A14',
'S3_A15',
'S3_A16',
'S3_A17',
'S3_A18',
'S3_A19',
'S3_A20',
'S3_A21',
'S3_A22',
'S3_A23',
'S3_A24')

pdf(file="C:/Users/Erin.Bohaboy/Documents/MUS_Sample_Design/MUS_examples/S3_bootstrap_examples_30MarB.pdf")

  par(mfrow=c(3,3))
  par(omi=c(0.3,0.3,0.3,0.1)) #set outer margins
  par(mai=c(0.2,0.2, 0.3, 0)) #set inner margins

for (o in 1:length(list_outputs)) {
	look_at_me <- get(list_outputs[o])

  for (j in 1:9) {
	sample_size <- sum(look_at_me$list_some_boot_samps[[j]]$counts)
	title_temp <- paste("samp boot",j,"n=",sample_size,sep=" ")
	plot(look_at_me$list_some_boot_samps[[j]], main = title_temp)
   		}
	mtext(side=1,"Length",line=1,outer=TRUE)
	mtext(side=2, "Frequency",line=1,outer=TRUE)
	main_title <- paste(list_outputs[o], look_at_me$params_input_output$sim_output_name,
				look_at_me$params_input_output$sample_type,
				"supp sm", look_at_me$params_input_output$supp_small,
				"supp lg", look_at_me$params_input_output$supp_large,
				sep=" ")
	mtext(side=3, main_title, line=0,outer=TRUE)

  	}
dev.off()


# Erin save this just in case
save.image("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_examples\\Mar31_A3_new_POS.RData")




# --------------------------------------------------------------------------------------------------------------
# repeat using the old POS sampling approach...
rm(list=ls())
source("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_Functions\\MUS_Functions_Code_POS_old.R")
# load("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_examples\\Mar29_simulated_pops.RData")

S3_A1 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2)

S3_A2 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A3 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A4 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A5 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A6 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A7 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A8 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A9 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 300, 
                   sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A10 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A11 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A12 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A13 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A14 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A15 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A16 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A17 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A18 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A19 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A20 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A21 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A22 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A23 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A24 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 500, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 


#  use LH_plot all_plots=TRUE to show us characteristics of the simulation input, population, and catch.
#	only need to do this once since those plots will be same for all S3:
LH_plot(S3_A1, all_plots=TRUE)

#  highly recommended to examine some bootstraps for each sampling approach, to ensure that the sample distribution
#	looks as we expect.
 

LH_plot(S3_A1, all_plots=TRUE)
LH_plot(S3_A2, all_plots=FALSE)
LH_plot(S3_A3, all_plots=FALSE)
LH_plot(S3_A4, all_plots=FALSE)
LH_plot(S3_A5, all_plots=FALSE)
LH_plot(S3_A6, all_plots=FALSE)
LH_plot(S3_A7, all_plots=FALSE)
LH_plot(S3_A8, all_plots=FALSE)
LH_plot(S3_A9, all_plots=FALSE)
LH_plot(S3_A10, all_plots=FALSE)
LH_plot(S3_A11, all_plots=FALSE)
LH_plot(S3_A12, all_plots=FALSE)
LH_plot(S3_A13, all_plots=FALSE)
LH_plot(S3_A14, all_plots=FALSE)
LH_plot(S3_A15, all_plots=FALSE)
LH_plot(S3_A16, all_plots=FALSE)
LH_plot(S3_A17, all_plots=FALSE)
LH_plot(S3_A18, all_plots=FALSE)
LH_plot(S3_A19, all_plots=FALSE)
LH_plot(S3_A20, all_plots=FALSE)
LH_plot(S3_A21, all_plots=FALSE)
LH_plot(S3_A22, all_plots=FALSE)
LH_plot(S3_A23, all_plots=FALSE)
LH_plot(S3_A24, all_plots=FALSE)


# in this example, the user asked for a large number of 'supplemental' samples compared to the FOS design
#	a warning message lets the user know this might be weird.
S3_testing <- LH_sample(sim_output = S3_Auric_highF, n_boots = 100, samp_size = 75, 
                   sample_type = 'FOS', supp_large = TRUE, supp_large_n_per_bin = 10,
                   supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 5,
                   supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 
LH_plot(S3_testing, all_plots=FALSE)






# make some plot pdfs for Eva

list_outputs <- c('S3_A1',
'S3_A2',
'S3_A3',
'S3_A4',
'S3_A5',
'S3_A6',
'S3_A7',
'S3_A8',
'S3_A9',
'S3_A10',
'S3_A11',
'S3_A12',
'S3_A13',
'S3_A14',
'S3_A15',
'S3_A16',
'S3_A17',
'S3_A18',
'S3_A19',
'S3_A20',
'S3_A21',
'S3_A22',
'S3_A23',
'S3_A24')

pdf(file="C:/Users/Erin.Bohaboy/Documents/MUS_Sample_Design/MUS_examples/S3_bootstrap_examples_31Mar_OLD_POS.pdf")

  par(mfrow=c(3,3))
  par(omi=c(0.3,0.3,0.3,0.1)) #set outer margins
  par(mai=c(0.2,0.2, 0.3, 0)) #set inner margins

for (o in 1:length(list_outputs)) {
	look_at_me <- get(list_outputs[o])

  for (j in 1:9) {
	sample_size <- sum(look_at_me$list_some_boot_samps[[j]]$counts)
	title_temp <- paste("samp boot",j,"n=",sample_size,sep=" ")
	plot(look_at_me$list_some_boot_samps[[j]], main = title_temp)
   		}
	mtext(side=1,"Length",line=1,outer=TRUE)
	mtext(side=2, "Frequency",line=1,outer=TRUE)
	main_title <- paste(list_outputs[o], look_at_me$params_input_output$sim_output_name,
				look_at_me$params_input_output$sample_type,
				"supp sm", look_at_me$params_input_output$supp_small,
				"supp lg", look_at_me$params_input_output$supp_large,
				sep=" ")
	mtext(side=3, main_title, line=0,outer=TRUE)

  	}
dev.off()



















#  ----------------- junk below: used for troubleshooting.

look_at_me <- S3_A11

#  trouble shooting
LH_plot(S3_A14)
names(S3_A14)
head(look_at_me$parameter_outputs)

plot(look_at_me$parameter_outputs$K1, look_at_me$parameter_outputs$Linf)



S3_A14 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A14$list_boot_samps[[88]]
POS <- S3_A14$list_boot_samps[[89]]
S3_A14$list_boot_samps[[90]]

#  -----------------  arguments, typical values
	sim_output <- S3_Auric_highF		# output of the simulate_population_harvest function
	n_boots <- 1000			# number of bootstraps
   	samp_size <- 75			# sample size for each bootstrap
	sample_type <- 'POS'		# define as 'POS' or 'FOS'
 
   	supp_large <- TRUE		# do we want to supplement with large fish?
   	supp_large_n_per_bin <- 3	# number supplemental samples per large bin
   	supp_small <- FALSE		# do we want to supplement with small fish?
   	supp_small_n_per_bin <- 3	# number supplemental samples per small bin
   	supp_min_length <- 10		# for supplemental small sampling, what is the smallest fish (cm) we could get?

   	constrained <- FALSE		# is t0 constrained?
   	t0 <- 0				# if constrainted, define t0

   	save_bootstraps <- FALSE	# should all bootstrap sample draws be saved? if n_boots or samp_size are large, this should be false
   	Amax <- 32				# maximum theoretical age (prop population input parameter)
   	age_max <- 15			# choose age_max (the plus group for pop dynamics) a priori
   	Lbin_width <- 2			# length bin width




S3_A14 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = FALSE, Amax = 32, age_max = 15, Lbin_width = 2) 














