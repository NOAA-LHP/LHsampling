#  ------------------------------------------------------------------------------------------------------

#	use functions to run simulations and bootstrap sampling approaches


#  ------------------------------------------------------------------------------------------------------

# rm(list=ls())

# load packages
  library(reshape)
  library(dplyr)
  library(ggplot2)
  library(magrittr)
  library(assertthat)



# --------------------------------------------------------------------------------------------------------------------------------------
#	SIMULATE SOME POPS


## S1: Auricilla, catch small, F = 0.5M

	S1_Auric_lowF <- simulate_population_harvest(Linf = 32.5, Linf_sd = 2.5, 
						M = 0.18, F = 0.09, Lorenzen = TRUE, 
						mincat = 10, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
						L0 = 10, L0_sd = 2.5, k = 0.6, k_sd = 0, 
						Amax = 32, age_max = 15, N = 100000)

summary(S1_Auric_lowF$harvest$length)
str(S1_Auric_lowF$harvest$length)
S1_hist_length_harv <- hist(S1_Auric_lowF$harvest$length, breaks = seq(0,50,2),include.lowest=TRUE, right=FALSE,plot=FALSE)

#med F
S2_Auric_medF <- simulate_population_harvest(Linf = 32.5, Linf_sd = 2.5, 
                                             M = 0.18, F = 0.18, Lorenzen = TRUE, 
                                             mincat = 10, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
                                             L0 = 10, L0_sd = 2.5, k = 0.6, k_sd = 0, 
                                             Amax = 32, age_max = 15, N = 100000)



## S3: Auricilla, catch small, F = 2M

	S3_Auric_highF <- simulate_population_harvest(Linf = 32.5, Linf_sd = 2.5, 
						M = 0.18, F = 0.36, Lorenzen = TRUE, 
						mincat = 10, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
						L0 = 10, L0_sd = 2.5, k = 0.6, k_sd = 0, 
						Amax = 32, age_max = 15, N = 100000)
summary(S3_Auric_highF$harvest$length)
str(S3_Auric_highF$harvest$length)
S3_hist_length_harv <- hist(S3_Auric_highF$harvest$length, breaks = seq(0,50,2),include.lowest=TRUE, right=FALSE,plot=FALSE)





##	S4A: Auricilla, no F (F=0)
		
	S4A_Auric_noF <- simulate_population_harvest(Linf = 32.5, Linf_sd = 2.5, 
						M = 0.18, F = 0, Lorenzen = TRUE, 
						mincat = 20, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
						L0 = 10, L0_sd = 2.5, k = 0.6, k_sd = 0, 
						Amax = 32, age_max = 15, N = 100000)

summary(S4A_Auric_noF$population$age)
S4A_hist_age_pop <- hist(S4A_Auric_noF$population$age, breaks = seq(0,31,1),include.lowest=TRUE, right=FALSE, plot=FALSE)
sum(S4A_hist_age_pop$density)

summary(S4A_Auric_noF$population$length)
str(S4A_Auric_noF$population$length)
S4A_hist_length_pop <- hist(S4A_Auric_noF$population$length, breaks = seq(0,50,2),include.lowest=TRUE, right=FALSE,plot=FALSE)



##	S4: Auricilla, low F (F=0.5*M), no small in catch
		
	S4_Auric_lowF <- simulate_population_harvest(Linf = 32.5, Linf_sd = 2.5, 
						M = 0.18, F = 0.09, Lorenzen = TRUE, 
						mincat = 20, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
						L0 = 10, L0_sd = 2.5, k = 0.6, k_sd = 0, 
						Amax = 32, age_max = 15, N = 100000)

S4_hist_age_harv <- hist(S4_Auric_lowF$harvest$age, breaks = seq(0,31,1),include.lowest=TRUE, right=FALSE,plot=FALSE)
S4_length_harv <- hist(S4_Auric_lowF$harvest$length, breaks = seq(0,50,2),include.lowest=TRUE, right=FALSE,plot=FALSE)

#med
S5_Auric_medF <- simulate_population_harvest(Linf = 32.5, Linf_sd = 2.5, 
                                              M = 0.18, F = 0.18, Lorenzen = TRUE, 
                                              mincat = 20, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
                                              L0 = 10, L0_sd = 2.5, k = 0.6, k_sd = 0, 
                                              Amax = 32, age_max = 15, N = 100000)

##	S6: Auricilla, high F (F = 2*M), no small in catch

	S6_Auric_highF <- simulate_population_harvest(Linf = 32.5, Linf_sd = 2.5, 
						M = 0.18, F = 0.36, Lorenzen = TRUE, 
						mincat = 20, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
						L0 = 10, L0_sd = 2.5, k = 0.6, k_sd = 0, 
						Amax = 32, age_max = 15, N = 100000)

S6_hist_age_harv <- hist(S6_Auric_highF$harvest$age, breaks = seq(0,31,1),include.lowest=TRUE, right=FALSE,plot=FALSE)
S6_length_harv <- hist(S6_Auric_highF$harvest$length, breaks = seq(0,50,2),include.lowest=TRUE, right=FALSE,plot=FALSE)




## 	S7: Onaga, low F (F = 0.5*M), small get caught

	S7_onaga_lowF <- simulate_population_harvest(Linf = 100, Linf_sd = 2.5, 
						M = 0.125, F = 0.0625, Lorenzen = TRUE, 
						mincat = 10, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
						L0 = 10, L0_sd = 2.5, k = 0.14, k_sd = 0, 
						Amax = 55, age_max = 20, N = 100000)
summary(S7_onaga_lowF$harvest$length)
str(S7_onaga_lowF$harvest$length)
S7_hist_length_harv <- hist(S7_onaga_lowF$harvest$length, breaks = seq(0,130,5),include.lowest=TRUE, right=FALSE,plot=FALSE)


#S8
S7_onaga_medF <- simulate_population_harvest(Linf = 100, Linf_sd = 2.5, 
                                             M = 0.125, F = 0.125, Lorenzen = TRUE, 
                                             mincat = 10, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
                                             L0 = 10, L0_sd = 2.5, k = 0.14, k_sd = 0, 
                                             Amax = 55, age_max = 20, N = 100000)

## 	S9: Onaga, high F (F = 2*M), small get caught

	S9_onaga_highF <- simulate_population_harvest(Linf = 100, Linf_sd = 2.5, 
						M = 0.125, F = 0.25, Lorenzen = TRUE, 
						mincat = 10, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
						L0 = 10, L0_sd = 2.5, k = 0.14, k_sd = 0, 
						Amax = 55, age_max = 20, N = 100000)

summary(S9_onaga_highF$harvest$length)
str(S9_onaga_highF$harvest$length)
S9_hist_length_harv <- hist(S9_onaga_highF$harvest$length, breaks = seq(0,130,5),include.lowest=TRUE, right=FALSE,plot=FALSE)


## 	S10: Onaga, high F (F = .5M), small get caught

S10_onaga_lowF <- simulate_population_harvest(Linf = 100, Linf_sd = 2.5, 
                                              M = 0.125, F = 0.0625, Lorenzen = TRUE, 
                                              mincat = 30, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
                                              L0 = 10, L0_sd = 2.5, k = 0.14, k_sd = 0, 
                                              Amax = 55, age_max = 20, N = 100000)

summary(S10_POS$harvest$length)
str(S10_POS$harvest$length)
S10_hist_length_harv <- hist(S10_POS$harvest$length, breaks = seq(0,130,5),include.lowest=TRUE, right=FALSE,plot=FALSE)

#med
S11_onaga_medF <- simulate_population_harvest(Linf = 100, Linf_sd = 2.5, 
                                              M = 0.125, F = 0.125, Lorenzen = TRUE, 
                                              mincat = 30, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
                                              L0 = 10, L0_sd = 2.5, k = 0.14, k_sd = 0, 
                                              Amax = 55, age_max = 20, N = 100000)


## 	S12: Onaga, high F (F = 2M), small get caught

S12_onaga_highF <- simulate_population_harvest(Linf = 100, Linf_sd = 2.5, 
                                               M = 0.125, F = 0.25, Lorenzen = TRUE, 
                                               mincat = 30, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
                                               L0 = 10, L0_sd = 2.5, k = 0.14, k_sd = 0, 
                                               Amax = 55, age_max = 20, N = 100000)

summary(S12_onaga_highF$harvest$length)
str(S12_onaga_highF$harvest$length)
S12_hist_length_harv <- hist(S12_onaga_highF$harvest$length, breaks = seq(0,130,5),include.lowest=TRUE, right=FALSE,plot=FALSE)





save.image("MUS_sim_pops_April1.RData")
















## 	S10A: Onaga, no F (F = 0)

	S10A_onaga_noF <- simulate_population_harvest(Linf = 100, Linf_sd = 2.5, 
						M = 0.125, F = 0, Lorenzen = TRUE, 
						mincat = 30, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
						L0 = 10, L0_sd = 2.5, k = 0.14, k_sd = 0, 
						Amax = 55, age_max = 20, N = 100000)


## 	S10: Onaga, low F (F = 0.5*M), no small in catch

	S10_onaga_lowF <- simulate_population_harvest(Linf = 100, Linf_sd = 2.5, 
	           M = 0.125, F = 0.0625, Lorenzen = TRUE, 
						mincat = 30, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
						L0 = 10, L0_sd = 2.5, k = 0.14, k_sd = 0, 
						Amax = 55, age_max = 20, N = 100000)

## 	S10B: Onaga,  low F (F = 0.5*M), Amax = 55

	S10B_onaga_lowF <- simulate_population_harvest(Linf = 100, Linf_sd = 2.5, 
						M = 0.125, F = 0.0625, Lorenzen = TRUE, 
						mincat = 30, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
						L0 = 10, L0_sd = 2.5, k = 0.14, k_sd = 0, 
						Amax = 55, age_max = 20, N = 100000)

## 	S12A: Onaga, no F (F = 0)

	S12A_onaga_noF <- simulate_population_harvest(Linf = 100, Linf_sd = 2.5, 
						M = 0.125, F = 0, Lorenzen = TRUE, 
						mincat = 30, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
						L0 = 10, L0_sd = 2.5, k = 0.14, k_sd = 0, 
						Amax = 55, age_max = 20, N = 100000)

## 	S12: Onaga, high F (F = 2*M), no small in catch

	S12_onaga_highF <- simulate_population_harvest(Linf = 100, Linf_sd = 2.5, 
						M = 0.125, F = 0.25, Lorenzen = TRUE, 
						mincat = 30, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
						L0 = 10, L0_sd = 2.5, k = 0.14, k_sd = 0, 
						Amax = 55, age_max = 20, N = 100000)


## 	S12B: Onaga, high F (F = 2*M), Amax = 55, M = 0.125, no small in catch

	S12B_onaga_highF <- simulate_population_harvest(Linf = 100, Linf_sd = 2.5, 
						M = 0.125, F = 0.25, Lorenzen = TRUE, 
						mincat = 30, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
						L0 = 10, L0_sd = 2.5, k = 0.14, k_sd = 0, 
						Amax = 55, age_max = 20, N = 100000)

S12B_onaga_highF <- S12_onaga_highF



# calculate prop at length

S10A_hist_age_pop <- hist(S10A_onaga_noF$population$age, breaks = seq(0,56,1),include.lowest=TRUE, right=FALSE,plot=FALSE)
S10A_hist_length_pop <- hist(S10A_onaga_noF$population$length, breaks = seq(0,130,5),include.lowest=TRUE, right=FALSE,plot=FALSE)

S10B_hist_age_harv <- hist(S10B_onaga_lowF$harvest$age, breaks = seq(0,56,1),include.lowest=TRUE, right=FALSE,plot=FALSE)
S10B_hist_length_harv <- hist(S10B_onaga_lowF$harvest$length, breaks = seq(0,130,5),include.lowest=TRUE, right=FALSE,plot=FALSE)

S12B_hist_age_harv <- hist(S12B_onaga_highF$harvest$age, breaks = seq(0,56,1),include.lowest=TRUE, right=FALSE,plot=FALSE)
S12B_hist_length_harv <- hist(S12B_onaga_highF$harvest$length, breaks = seq(0,130,5),include.lowest=TRUE, right=FALSE,plot=FALSE)




S10A_length_hist_length_pop_10 <- hist(S10A_onaga_noF$population$length, breaks = seq(0,130,10),plot=FALSE)
S10B_length_hist_length_harv_10 <- hist(S10B_onaga_lowF$harvest$length, breaks = seq(0,130,10),plot=FALSE)
S12B_length_hist_length_harv_10 <- hist(S12B_onaga_highF$harvest$length, breaks = seq(0,130,10),plot=FALSE)




# clean, save workspace
# rm(list=ls()[15])
# save.image("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_examples\\Oct23_simulated_pops.RData")
# load("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_examples\\Oct21_simulated_pops.RData")







# calculate some summary metrics for the results text
str(S4_Auric_lowF)
old_low <- subset(S4_Auric_lowF$population, age >= 15)	# tot 250455 fish
nrow(old_low) / 250455
sd(old_low$length) / mean(old_low$length)				# 0.0750

str(S5_Auric_medF)
old_med <- subset(S5_Auric_medF$population, age >= 15)	# tot 204905 fish  #str(S2B_Auric_03F)
nrow(old_med ) /  204905
sd(old_med$length) / mean(old_med$length)				# 0.0760

str(S6_Auric_highF)
old_high <- subset(S6_Auric_highF$population, age >= 15)	# tot 158539fish   #str(S2C_Auric_05F)
nrow(old_high) / 183978
sd(old_high$length) / mean(old_high$length)			# 0.0677


str(S4_Auric_lowF)
old_low <- subset(S4_Auric_lowF$harvest, age >= 15)	# tot 12239 fish
nrow(old_low) / 12239
sd(old_low$length) / mean(old_low$length)				# 0.0750

str(S5_Auric_medF)
old_med <- subset(S5_Auric_medF$harvest, age >= 15)	# tot 18821  fish  #str(S2B_Auric_03F)
nrow(old_med ) /  18821 
sd(old_med$length) / mean(old_med$length)				# 0.0760

str(S6_Auric_highF)
old_high <- subset(S6_Auric_highF$harvest, age >= 15)	# tot25722fish   #str(S2C_Auric_05F)
nrow(old_high) / 25722
sd(old_high$length) / mean(old_high$length)			# 0.0677




str(S10_onaga_lowF)
old_low <- subset(S10_onaga_lowF$harvest, age >= 20)	# tot 6728 fish
nrow(old_low) / 6728
sd(old_low$length) / mean(old_low$length)				# 0.0750

str(S11_onaga_medF)
old_med <- subset(S11_onaga_medF$harvest, age >= 20)	# tot 7801 fish  #str(S2B_Auric_03F)
nrow(old_med ) /  7801
sd(old_med$length) / mean(old_med$length)				# 0.0760

str(S12_onaga_highF)
old_high <- subset(S12_onaga_highF$harvest, age >= 20)	# tot 15198fish   #str(S2C_Auric_05F)
nrow(old_high) / 13276
sd(old_high$length) / mean(old_high$length)			# 0.0677


str(S10_onaga_lowF)
old_low <- subset(S10_onaga_lowF$population, age >= 20)	# tot210593 fish
nrow(old_low) / 210593
sd(old_low$length) / mean(old_low$length)				# 0.0750

str(S11_onaga_medF)
old_med <- subset(S11_onaga_medF$population, age >= 20)	# tot 154009 fish  #str(S2B_Auric_03F)
nrow(old_med ) /  154009
sd(old_med$length) / mean(old_med$length)				# 0.0760

str(S12_onaga_highF)
old_high <- subset(S12_onaga_highF$population, age >= 20)	# tot151443fish   #str(S2C_Auric_05F)
nrow(old_high) / 151443
sd(old_high$length) / mean(old_high$length)			# 0.0677

# --------------------------------------------------------------------------------------------------------------------------------------
#####  A. S1_Auric_lowF#######


S1_A1 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A2 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A3 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                   save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A4 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                   save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A5 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A6 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A7 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A8 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A9 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A10 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A11 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A12 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A13 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A14 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A15 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A16 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A17 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A18 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A19 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A20 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A21 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A22 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A23 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S1_A24 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 


str(S1_A24_new)
S1_A24_new$parameter_summary_all_boots
S1_extract_all_approach$S1_A24


look_at_me <- S1_A5
str(S1_A5)
look_at_me$params_input_output
head(look_at_me$parameter_outputs)
hist(look_at_me$list_boot_samps[[1]]$length, breaks=seq(0,50,2))
look_at_me$simulation_params
look_at_me$list_boot_CVs[[1]]
look_at_me$list_boot_samps[[1]]
hist(look_at_me$list_boot_samps[[1]]$binl, breaks=seq(0,50,2))



# --------------------------------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------------------------------
# for each sampling approach, extract out summary quantities for each parameter over all bootstraps.


each_approach_name <- c('S1_A1','S1_A2','S1_A3','S1_A4','S1_A5','S1_A6','S1_A7','S1_A8',
                        'S1_A9','S1_A10','S1_A11','S1_A12','S1_A13','S1_A14','S1_A15','S1_A16',
                        'S1_A17','S1_A18','S1_A19','S1_A20','S1_A21','S1_A22','S1_A23','S1_A24')

S1_extract_all_approach <- list()

for (i in 1:length(each_approach_name)) {
  
  extract_thing <- get(each_approach_name[i])
  extract_summary <- data.frame(parm_name = character(), lower95 = numeric(), lower50 = numeric(), 
                                avg = numeric(), upper50 = numeric(), upper95 = numeric(), stringsAsFactors=FALSE)
  
  for (j in 1: ncol(extract_thing$parameter_outputs)) {
    extract_summary[j,1] <- colnames(extract_thing$parameter_outputs)[j]
    extract_summary[j,2] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.025, na.rm = TRUE))
    extract_summary[j,3] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.25, na.rm = TRUE))
    extract_summary[j,4] <- mean(extract_thing$parameter_outputs[,j],na.rm=TRUE)
    extract_summary[j,5] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.75, na.rm = TRUE))
    extract_summary[j,6] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.975, na.rm = TRUE))
  }
  
  S1_extract_all_approach[[i]] <- extract_summary
  names(S1_extract_all_approach)[[i]] <- each_approach_name[i]
  
}


#  get the true population values for S1

S1_true <- c(S1_Auric_lowF$parameters$Linf, S1_Auric_lowF$parameters$k, S1_Auric_lowF$parameters$L0,
             S1_Auric_lowF$parameters$CV_L_0, S1_Auric_lowF$parameters$CV_L_age_max)



#  -------------------------------------------------------------------------------------------------------------------
# make a figure: 1 column, 25 sampling approaches per pane.


plot_me <- S1_extract_all_approach
parm_true <- S1_true
each_approach_name <- c('S1_A1','S1_A2','S1_A3','S1_A4','S1_A5','S1_A6','S1_A7','S1_A8',
                        'S1_A9','S1_A10','S1_A11','S1_A12','S1_A13','S1_A14','S1_A15','S1_A16',
                        'S1_A17','S1_A18','S1_A19','S1_A20','S1_A21','S1_A22','S1_A23','S1_A24')

xnames_all <- c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15',
                'A16','A17','A18','A19','A20','A21','A22','A23','A24')
xnames_odd <- c('A1','','A3','','A5','','A7','','A9','','A11','','A13','','A15',
                '','A17','','A19','','A21','','A23','')
xnames_even <- c('','A2','','A4','','A6','','A8','','A10','','A12','','A14','',
                 'A16','','A18','','A20','','A22','','A24')
xnames_brief <- c('A1','','','','A5','','','','A9','','','','A13','','','','A17','','','','A21','','','')


# ---------   show me min and max of each parm
Linf_low <- as.numeric()
Linf_high <- as.numeric()
k_low <- as.numeric()
k_high <- as.numeric()
L0_low <- as.numeric()
L0_high <- as.numeric()
CV0_low <- as.numeric()
CV0_high <- as.numeric()
CVmax_low <- as.numeric()
CVmax_high <- as.numeric()

for (i in 1: length(plot_me)) {
  Linf_low[i] <- plot_me[[i]][1,2]
  Linf_high[i] <- plot_me[[i]][1,6]
  k_low[i] <- plot_me[[i]][2,2]
  k_high[i] <- plot_me[[i]][2,6]
  L0_low[i] <- plot_me[[i]][6,2]
  L0_high[i] <- plot_me[[i]][6,6]
  CV0_low[i] <- plot_me[[i]][4,2]
  CV0_high[i] <- plot_me[[i]][4,6]
  CVmax_low[i] <- plot_me[[i]][5,2]
  CVmax_high[i] <- plot_me[[i]][5,6]
}

c(min(Linf_low), max(Linf_high))
c(min(k_low), max(k_high))
c(min(L0_low), max(L0_high))
c(min(CV0_low), max(CV0_high))
c(min(CVmax_low), max(CVmax_high))

# assign yaxis limits for each parm.
Linf_ylim <- c(30,40)
K_ylim <- c(0.04, 1.00)
L0_ylim <- c(0, 22)
CV_0_ylim <- c(0,0.6)
CV_max_ylim <- c(0,0.25)


setwd('C:/Users/Erin.Bohaboy/Documents/MUS_Sample_Design/MUS_examples')


#  tiff(file="sample_fig_Sep25.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=300)
tiff(file="S1_auric_low_fig_march.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=200)
par(mfcol=c(5,1))
par(omi=c(0.25,0.2,0.25,0.1)) #set outer margins
par(mai=c(0.15,0.15,.15,0)) #set inner margins

xaxis_plotvals <- seq(1,24,1)	#because there are 25 different approaches: A1-A25



# -------------------  A. Linf

p = 1	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=Linf_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[1]], (max(xaxis_plotvals) + 0.5), parm_true[[1]], lty=2, col="black")

# draw axes

axis(1, pos=Linf_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=Linf_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(30,40,2), cex=1.1)

mtext(side=3,adj=.05,line=-2,"A", cex=.75)
mtext(expression(italic('L'~infinity)), side=2, line=1.2, cex=0.9)

mtext(side=3,adj=0.5,line=0,"S1", cex=1)


# -------------------  B. k

p = 2	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=K_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[2]], (max(xaxis_plotvals) + 0.5), parm_true[[2]], lty=2, col="black")

# draw axes

axis(1, pos=K_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=K_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(0.0, 1.0, 0.1), cex=1.1)

mtext(side=3,adj=.05,line=-1.3,"B", cex=.75)

mtext(expression(italic(K)), side=2, line=1.2, cex=0.9)


# -------------------  C. L0

p = 6	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=L0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[3]], (max(xaxis_plotvals) + 0.5), parm_true[[3]], lty=2, col="black")

# draw axes

axis(1, pos=L0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=L0_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(0,26,4), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"C", cex=.75)
mtext(expression(italic('L'[0])), side=2, line=1.2, cex=0.9)




# -------------------  D. CV LINF

p = 5	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_max_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[5]], (max(xaxis_plotvals) + 0.5), parm_true[[5]], lty=2, col="black")

# draw axes

axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_max_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(0,0.25,.05), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"D", cex=.75)
mtext(expression(italic("CV  L"~infinity)), side=2, line=1.2, cex=0.9)




# -------------------  E. CV L0

p = 4	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)


#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), S1_extract_all_approach[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[4]], (max(xaxis_plotvals) + 0.5), parm_true[[4]], lty=2, col="black")




# draw axes

axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_0_ylim[1], at=xaxis_plotvals, labels= xnames_odd, las=1, cex.axis=0.9)
axis(1, pos=CV_0_ylim[1], at=xaxis_plotvals, labels= xnames_even, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(-.2,0.8,.1), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"E", cex=.75)
mtext(expression(italic("CV  L"[0])), side=2, line=1.2, cex=0.9)

dev.off()



########
##### S3_Auric_highF ######


S3_A1 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A2 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A3 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                   save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A4 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                   save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A5 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A6 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A7 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A8 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                   sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                   supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                   supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A9 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                   sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                   save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A10 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A11 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A12 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A13 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A14 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A15 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A16 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A17 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A18 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A19 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A20 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A21 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A22 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A23 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S3_A24 <- LH_sample(sim_output = S3_Auric_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 


str(S3_A24_new)
S3_A24_new$parameter_summary_all_boots
S3_extract_all_approach$S3_A24


look_at_me <- S3_A5
str(S3_A5)
look_at_me$params_input_output
head(look_at_me$parameter_outputs)
hist(look_at_me$list_boot_samps[[1]]$length, breaks=seq(0,50,2))
look_at_me$simulation_params
look_at_me$list_boot_CVs[[1]]
look_at_me$list_boot_samps[[1]]
hist(look_at_me$list_boot_samps[[1]]$binl, breaks=seq(0,50,2))

 --------------------------------------------------------------------------------------------------------------------------------------
# for each sampling approach, extract out summary quantities for each parameter over all bootstraps.

each_approach_name <- c('S3_A1','S3_A2','S3_A3','S3_A4','S3_A5','S3_A6','S3_A7','S3_A8',
                          'S3_A9','S3_A10','S3_A11','S3_A12','S3_A13','S3_A14','S3_A15','S3_A16',
                          'S3_A17','S3_A18','S3_A19','S3_A20','S3_A21','S3_A22','S3_A23','S3_A24')


S3_extract_all_approach <- list()

for (i in 1:length(each_approach_name)) {
  
  extract_thing <- get(each_approach_name[i])
  extract_summary <- data.frame(parm_name = character(), lower95 = numeric(), lower50 = numeric(), 
                                avg = numeric(), upper50 = numeric(), upper95 = numeric(), stringsAsFactors=FALSE)
  
  for (j in 1: ncol(extract_thing$parameter_outputs)) {
    extract_summary[j,1] <- colnames(extract_thing$parameter_outputs)[j]
    extract_summary[j,2] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.025, na.rm = TRUE))
    extract_summary[j,3] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.25, na.rm = TRUE))
    extract_summary[j,4] <- mean(extract_thing$parameter_outputs[,j],na.rm=TRUE)
    extract_summary[j,5] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.75, na.rm = TRUE))
    extract_summary[j,6] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.975, na.rm = TRUE))
  }
  
  S3_extract_all_approach[[i]] <- extract_summary
  names(S3_extract_all_approach)[[i]] <- each_approach_name[i]
  
}


#  get the true population values for S3

S3_true <- c(S3_Auric_highF$parameters$Linf, S3_Auric_highF$parameters$k, S3_Auric_highF$parameters$L0,
             S3_Auric_highF$parameters$CV_L_0, S3_Auric_highF$parameters$CV_L_age_max)



#  -------------------------------------------------------------------------------------------------------------------
# make a figure: 1 column, 25 sampling approaches per pane.


plot_me <- S3_extract_all_approach
parm_true <- S3_true
each_approach_name <- c('S3_A1','S3_A2','S3_A3','S3_A4','S3_A5','S3_A6','S3_A7','S3_A8',
                        'S3_A9','S3_A10','S3_A11','S3_A12','S3_A13','S3_A14','S3_A15','S3_A16',
                        'S3_A17','S3_A18','S3_A19','S3_A20','S3_A21','S3_A22','S3_A23','S3_A24')

xnames_all <- c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15',
                'A16','A17','A18','A19','A20','A21','A22','A23','A24')
xnames_odd <- c('A1','','A3','','A5','','A7','','A9','','A11','','A13','','A15',
                '','A17','','A19','','A21','','A23','')
xnames_even <- c('','A2','','A4','','A6','','A8','','A10','','A12','','A14','',
                 'A16','','A18','','A20','','A22','','A24')
xnames_brief <- c('A1','','','','A5','','','','A9','','','','A13','','','','A17','','','','A21','','','')


# ---------   show me min and max of each parm
Linf_low <- as.numeric()
Linf_high <- as.numeric()
k_low <- as.numeric()
k_high <- as.numeric()
L0_low <- as.numeric()
L0_high <- as.numeric()
CV0_low <- as.numeric()
CV0_high <- as.numeric()
CVmax_low <- as.numeric()
CVmax_high <- as.numeric()

for (i in 1: length(plot_me)) {
  Linf_low[i] <- plot_me[[i]][1,2]
  Linf_high[i] <- plot_me[[i]][1,6]
  k_low[i] <- plot_me[[i]][2,2]
  k_high[i] <- plot_me[[i]][2,6]
  L0_low[i] <- plot_me[[i]][6,2]
  L0_high[i] <- plot_me[[i]][6,6]
  CV0_low[i] <- plot_me[[i]][4,2]
  CV0_high[i] <- plot_me[[i]][4,6]
  CVmax_low[i] <- plot_me[[i]][5,2]
  CVmax_high[i] <- plot_me[[i]][5,6]
}

c(min(Linf_low), max(Linf_high))
c(min(k_low), max(k_high))
c(min(L0_low), max(L0_high))
c(min(CV0_low), max(CV0_high))
c(min(CVmax_low), max(CVmax_high))

# assign yaxis limits for each parm.
Linf_ylim <- c(30,40)
K_ylim <- c(0.04, 1.00)
L0_ylim <- c(0, 22)
CV_0_ylim <- c(0,0.6)
CV_max_ylim <- c(0,0.25)


setwd('C:/Users/Erin.Bohaboy/Documents/MUS_Sample_Design/MUS_examples')


#  tiff(file="sample_fig_Sep25.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=300)
tiff(file="S3_auric_low_fig_dec4.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=200)
par(mfcol=c(5,1))
par(omi=c(0.25,0.2,0.25,0.1)) #set outer margins
par(mai=c(0.15,0.15,.15,0)) #set inner margins

xaxis_plotvals <- seq(1,24,1)	#because there are 25 different approaches: A1-A25



# -------------------  A. Linf

p = 1	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=Linf_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[1]], (max(xaxis_plotvals) + 0.5), parm_true[[1]], lty=2, col="black")

# draw axes

axis(1, pos=Linf_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=Linf_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(30,40,2), cex=1.1)

mtext(side=3,adj=.05,line=-2,"A", cex=.75)
mtext(expression(italic('L'~infinity)), side=2, line=1.2, cex=0.9)

mtext(side=3,adj=0.5,line=0,"S3", cex=1)


# -------------------  B. k

p = 2	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=K_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[2]], (max(xaxis_plotvals) + 0.5), parm_true[[2]], lty=2, col="black")

# draw axes

axis(1, pos=K_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=K_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(0.0, 1.0, 0.1), cex=1.1)

mtext(side=3,adj=.05,line=-1.3,"B", cex=.75)

mtext(expression(italic(K)), side=2, line=1.2, cex=0.9)


# -------------------  C. L0

p = 6	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=L0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[3]], (max(xaxis_plotvals) + 0.5), parm_true[[3]], lty=2, col="black")

# draw axes

axis(1, pos=L0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=L0_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(0,26,4), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"C", cex=.75)
mtext(expression(italic('L'[0])), side=2, line=1.2, cex=0.9)




# -------------------  D. CV LINF

p = 5	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_max_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[5]], (max(xaxis_plotvals) + 0.5), parm_true[[5]], lty=2, col="black")

# draw axes

axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_max_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(0,0.25,.05), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"D", cex=.75)
mtext(expression(italic("CV  L"~infinity)), side=2, line=1.2, cex=0.9)




# -------------------  E. CV L0

p = 4	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), S3_extract_all_approach[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[4]], (max(xaxis_plotvals) + 0.5), parm_true[[4]], lty=2, col="black")

# draw axes

axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_0_ylim[1], at=xaxis_plotvals, labels= xnames_odd, las=1, cex.axis=0.9)
axis(1, pos=CV_0_ylim[1], at=xaxis_plotvals, labels= xnames_even, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(-.2,0.8,.1), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"E", cex=.75)
mtext(expression(italic("CV  L"[0])), side=2, line=1.2, cex=0.9)

dev.off()








#  A. S4_Auric_lowF


	S4_A1 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A2 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A3 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 
	
	S4_A4 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A5 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A6 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A7 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A8 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A9 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A10 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A11 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A12 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A13 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A14 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 
	
	S4_A15 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A16 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A17 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 
	
	S4_A18 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A19 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A20 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A21 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A22 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A23 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S4_A24 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 


		str(S4_A24_new)
		S4_A24_new$parameter_summary_all_boots
		S4_extract_all_approach$S4_A24


	look_at_me <- S4_A5
	str(S4_A5)
	look_at_me$params_input_output
	head(look_at_me$parameter_outputs)
	hist(look_at_me$list_boot_samps[[1]]$length, breaks=seq(0,50,2))
	look_at_me$simulation_params
	look_at_me$list_boot_CVs[[1]]
	look_at_me$list_boot_samps[[1]]
	hist(look_at_me$list_boot_samps[[1]]$binl, breaks=seq(0,50,2))


#  save.image("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_examples\\Sep29_simulated_pops_samps.RData")
# load("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_examples\\Sep29_simulated_pops_samps.RData")

#  save.image("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_examples\\Oct1_simulated_pops_funcs_only.RData")
#	load("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_examples\\Oct1_simulated_pops_funcs_only.RData")

# --------------------------------------------------------------------------------------------------------------------------------------
 
#rm(list=ls()[5:21])



#  Sep30: Erin put this extract step into the main LH_sampling function. Return extract object with each run. But this code will also
#		still work.
# --------------------------------------------------------------------------------------------------------------------------------------
# for each sampling approach, extract out summary quantities for each parameter over all bootstraps.


each_approach_name <- c('S4_A1','S4_A2','S4_A3','S4_A4','S4_A5','S4_A6','S4_A7','S4_A8',
				'S4_A9','S4_A10','S4_A11','S4_A12','S4_A13','S4_A14','S4_A15','S4_A16',
				'S4_A17','S4_A18','S4_A19','S4_A20','S4_A21','S4_A22','S4_A23','S4_A24')

S4_extract_all_approach <- list()

for (i in 1:length(each_approach_name)) {

	extract_thing <- get(each_approach_name[i])
	extract_summary <- data.frame(parm_name = character(), lower95 = numeric(), lower50 = numeric(), 
				avg = numeric(), upper50 = numeric(), upper95 = numeric(), stringsAsFactors=FALSE)

 for (j in 1: ncol(extract_thing$parameter_outputs)) {
	  extract_summary[j,1] <- colnames(extract_thing$parameter_outputs)[j]
	  extract_summary[j,2] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.025, na.rm = TRUE))
	  extract_summary[j,3] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.25, na.rm = TRUE))
	  extract_summary[j,4] <- mean(extract_thing$parameter_outputs[,j],na.rm=TRUE)
	  extract_summary[j,5] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.75, na.rm = TRUE))
	  extract_summary[j,6] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.975, na.rm = TRUE))
	  }

	S4_extract_all_approach[[i]] <- extract_summary
	names(S4_extract_all_approach)[[i]] <- each_approach_name[i]

  }


#  get the true population values for S4

S4_true <- c(S4_Auric_lowF$parameters$Linf, S4_Auric_lowF$parameters$k, S4_Auric_lowF$parameters$L0,
			S4_Auric_lowF$parameters$CV_L_0, S4_Auric_lowF$parameters$CV_L_age_max)



#  -------------------------------------------------------------------------------------------------------------------
# make a figure: 1 column, 25 sampling approaches per pane.
 

   plot_me <- S4_extract_all_approach
   parm_true <- S4_true
   each_approach_name <- c('S4_A1','S4_A2','S4_A3','S4_A4','S4_A5','S4_A6','S4_A7','S4_A8',
				'S4_A9','S4_A10','S4_A11','S4_A12','S4_A13','S4_A14','S4_A15','S4_A16',
				'S4_A17','S4_A18','S4_A19','S4_A20','S4_A21','S4_A22','S4_A23','S4_A24')

   xnames_all <- c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15',
				'A16','A17','A18','A19','A20','A21','A22','A23','A24')
   xnames_odd <- c('A1','','A3','','A5','','A7','','A9','','A11','','A13','','A15',
				'','A17','','A19','','A21','','A23','')
   xnames_even <- c('','A2','','A4','','A6','','A8','','A10','','A12','','A14','',
				'A16','','A18','','A20','','A22','','A24')
   xnames_brief <- c('A1','','','','A5','','','','A9','','','','A13','','','','A17','','','','A21','','','')


 # ---------   show me min and max of each parm
    Linf_low <- as.numeric()
    Linf_high <- as.numeric()
    k_low <- as.numeric()
    k_high <- as.numeric()
    L0_low <- as.numeric()
    L0_high <- as.numeric()
    CV0_low <- as.numeric()
    CV0_high <- as.numeric()
    CVmax_low <- as.numeric()
    CVmax_high <- as.numeric()

      for (i in 1: length(plot_me)) {
	  Linf_low[i] <- plot_me[[i]][1,2]
	  Linf_high[i] <- plot_me[[i]][1,6]
	  k_low[i] <- plot_me[[i]][2,2]
	  k_high[i] <- plot_me[[i]][2,6]
        L0_low[i] <- plot_me[[i]][6,2]
	  L0_high[i] <- plot_me[[i]][6,6]
        CV0_low[i] <- plot_me[[i]][4,2]
	  CV0_high[i] <- plot_me[[i]][4,6]
        CVmax_low[i] <- plot_me[[i]][5,2]
	  CVmax_high[i] <- plot_me[[i]][5,6]
	}

	c(min(Linf_low), max(Linf_high))
      c(min(k_low), max(k_high))
      c(min(L0_low), max(L0_high))
      c(min(CV0_low), max(CV0_high))
      c(min(CVmax_low), max(CVmax_high))

  # assign yaxis limits for each parm.
	Linf_ylim <- c(30,50)
	K_ylim <- c(0.04, 1.30)
	L0_ylim <- c(-4, 28)
	CV_0_ylim <- c(-.2,0.8)
	CV_max_ylim <- c(0,0.25)


setwd('C:/Users/Erin.Bohaboy/Documents/MUS_Sample_Design/MUS_examples')


#  tiff(file="sample_fig_Sep25.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=300)
  tiff(file="S4_auric_low_fig_dec4.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=200)
  par(mfcol=c(5,1))
  par(omi=c(0.25,0.2,0.25,0.1)) #set outer margins
  par(mai=c(0.15,0.15,.15,0)) #set inner margins

  xaxis_plotvals <- seq(1,24,1)	#because there are 25 different approaches: A1-A25



# -------------------  A. Linf

  p = 1	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
	plot(0,0,ylim=Linf_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
  for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
	}

#  add 50% box
  for (i in 1:length(each_approach_name)) {
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
		c(plot_me[[i]][p,3],plot_me[[i]][p,3],
			plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
	}

#  add mean  
  for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
	}

#  add true
  segments(0.5, parm_true[[1]], (max(xaxis_plotvals) + 0.5), parm_true[[1]], lty=2, col="black")

# draw axes

  axis(1, pos=Linf_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
  axis(1, pos=Linf_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
  axis(2, pos=0.5, at=seq(30,48,2), cex=1.1)

mtext(side=3,adj=.05,line=-2,"A", cex=.75)
mtext(expression(italic('L'~infinity)), side=2, line=1.2, cex=0.9)

 mtext(side=3,adj=0.5,line=0,"S4", cex=1)


# -------------------  B. k

  p = 2	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
	plot(0,0,ylim=K_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
  for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
	}

#  add 50% box
  for (i in 1:length(each_approach_name)) {
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
		c(plot_me[[i]][p,3],plot_me[[i]][p,3],
			plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
	}

#  add mean  
  for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
	}

#  add true
  segments(0.5, parm_true[[2]], (max(xaxis_plotvals) + 0.5), parm_true[[2]], lty=2, col="black")

# draw axes

  axis(1, pos=K_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
  axis(1, pos=K_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
  axis(2, pos=0.5, at=seq(0.0, 1.30, 0.1), cex=1.1)

  mtext(side=3,adj=.05,line=-1.3,"B", cex=.75)
  
mtext(expression(italic(K)), side=2, line=1.2, cex=0.9)


# -------------------  C. L0

  p = 6	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
	plot(0,0,ylim=L0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
  for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
	}

#  add 50% box
  for (i in 1:length(each_approach_name)) {
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
		c(plot_me[[i]][p,3],plot_me[[i]][p,3],
			plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
	}

#  add mean  
  for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
	}

#  add true
  segments(0.5, parm_true[[3]], (max(xaxis_plotvals) + 0.5), parm_true[[3]], lty=2, col="black")

# draw axes

  axis(1, pos=L0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
  axis(1, pos=L0_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
  axis(2, pos=0.5, at=seq(-4,28,4), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"C", cex=.75)
mtext(expression(italic("L"[0])), side=2, line=1.2, cex=0.9)




# -------------------  D. CV LINF

  p = 5	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
	plot(0,0,ylim=CV_max_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
  for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
	}

#  add 50% box
  for (i in 1:length(each_approach_name)) {
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
		c(plot_me[[i]][p,3],plot_me[[i]][p,3],
			plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
	}

#  add mean  
  for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
	}

#  add true
  segments(0.5, parm_true[[5]], (max(xaxis_plotvals) + 0.5), parm_true[[5]], lty=2, col="black")

# draw axes

  axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
  axis(1, pos=CV_max_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
  axis(2, pos=0.5, at=seq(-0.8,0.25,.05), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"D", cex=.75)
mtext(expression(italic("CV  L"~infinity)), side=2, line=1.2, cex=0.9)




# -------------------  E. CV L0

  p = 4	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
	plot(0,0,ylim=CV_0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
  for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), S4_extract_all_approach[[i]][p,6], lwd=1)
	}

#  add 50% box
  for (i in 1:length(each_approach_name)) {
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
		c(plot_me[[i]][p,3],plot_me[[i]][p,3],
			plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
	}

#  add mean  
  for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
	}

#  add true
  segments(0.5, parm_true[[4]], (max(xaxis_plotvals) + 0.5), parm_true[[4]], lty=2, col="black")

# draw axes
  
  axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
  axis(1, pos=CV_0_ylim[1], at=xaxis_plotvals, labels= xnames_odd, las=1, cex.axis=0.9)
  axis(1, pos=CV_0_ylim[1], at=xaxis_plotvals, labels= xnames_even, las=1, cex.axis=0.9)
  axis(2, pos=0.5, at=seq(-.2,0.8,.1), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"E", cex=.75)
mtext(expression(italic("CV  L"[0])), side=2, line=1.2, cex=0.9)

dev.off()












# --------------------------------------------------------------------------------------------------------------------------------------

#  B. S6_Auric_highF


	S6_A1 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A2 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A3 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 
	
	S6_A4 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A5 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A6 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A7 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A8 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A9 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A10 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A11 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A12 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A13 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A14 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 
	
	S6_A15 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A16 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A17 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 
	
	S6_A18 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A19 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A20 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A21 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A22 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A23 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

	S6_A24 <- LH_sample(sim_output = S6_Auric_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 


	look_at_me <- S6_A5
	look_at_me$params_input_output
	head(look_at_me$parameter_outputs)
	head(look_at_me$parameter_outputs$L0)
	hist(look_at_me$parameter_outputs$L0)
	hist(look_at_me$parameter_outputs$K)
	median(look_at_me$parameter_outputs$L0, na.rm=TRUE)
min(look_at_me$parameter_outputs$L0, na.rm=TRUE)
median(look_at_me$parameter_outputs$L0, na.rm=TRUE)

hist(look_at_me$list_boot_samps[[1]]$length, breaks = seq(0,50,2))
	look_at_me$list_boot_samps[[1]]
	look_at_me$simulation_params
	look_at_me$list_boot_CVs[[1]]
	str(look_at_me)

	look_at_me$list_boot_samps[[581]]
	look_at_me$list_boot_mods[[581]]
	look_at_me$list_boot_preds[[581]]


# rediculous: 779, 581


#  save.image("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_examples\\Sep29_simulated_pops_samps.RData")
#  save.image("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_examples\\Sep30_simulated_pops_samps.RData")
# load("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_examples\\Sep30_simulated_pops_samps.RData")

# --------------------------------------------------------------------------------------------------------------------------------------

#  Oct 1: Erin divide this workspace up it is getting annoyingly big.

# rm(list=ls())
# load("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_examples\\Oct1_S4_runs.RData")
# save.image("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_examples\\Oct1_S6_runs.RData")



# --------------------------------------------------------------------------------------------------------------------------------------
# for each sampling approach, extract out summary quantities for each parameter over all bootstraps.


each_approach_name <- c('S6_A1','S6_A2','S6_A3','S6_A4','S6_A5','S6_A6','S6_A7','S6_A8',
				'S6_A9','S6_A10','S6_A11','S6_A12','S6_A13','S6_A14','S6_A15','S6_A16',
				'S6_A17','S6_A18','S6_A19','S6_A20','S6_A21','S6_A22','S6_A23','S6_A24')

S6_extract_all_approach <- list()

for (i in 1:length(each_approach_name)) {

	extract_thing <- get(each_approach_name[i])
	extract_summary <- data.frame(parm_name = character(), lower95 = numeric(), lower50 = numeric(), 
				avg = numeric(), upper50 = numeric(), upper95 = numeric(), stringsAsFactors=FALSE)

 for (j in 1: ncol(extract_thing$parameter_outputs)) {
	  extract_summary[j,1] <- colnames(extract_thing$parameter_outputs)[j]
	  extract_summary[j,2] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.025, na.rm = TRUE))
	  extract_summary[j,3] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.25, na.rm = TRUE))
	  extract_summary[j,4] <- mean(extract_thing$parameter_outputs[,j],na.rm=TRUE)
	  extract_summary[j,5] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.75, na.rm = TRUE))
	  extract_summary[j,6] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.975, na.rm = TRUE))
	  }

	S6_extract_all_approach[[i]] <- extract_summary
	names(S6_extract_all_approach)[[i]] <- each_approach_name[i]

  }


#  get the true population values for S4

S6_true <- c(S6_Auric_highF$parameters$Linf, S6_Auric_highF$parameters$k, S6_Auric_highF$parameters$L0,
			S6_Auric_highF$parameters$CV_L_0, S6_Auric_highF$parameters$CV_L_age_max)





#  -------------------------------------------------------------------------------------------------------------------
# make a figure: 1 column, 25 sampling approaches per pane.
 

   plot_me <- S6_extract_all_approach
   parm_true <- S6_true
   each_approach_name <- c('S6_A1','S6_A2','S6_A3','S6_A4','S6_A5','S6_A6','S6_A7','S6_A8',
				'S6_A9','S6_A10','S6_A11','S6_A12','S6_A13','S6_A14','S6_A15','S6_A16',
				'S6_A17','S6_A18','S6_A19','S6_A20','S6_A21','S6_A22','S6_A23','S6_A24')


   xnames_all <- c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15',
				'A16','A17','A18','A19','A20','A21','A22','A23','A24')
   xnames_odd <- c('A1','','A3','','A5','','A7','','A9','','A11','','A13','','A15',
				'','A17','','A19','','A21','','A23','')
   xnames_even <- c('','A2','','A4','','A6','','A8','','A10','','A12','','A14','',
				'A16','','A18','','A20','','A22','','A24')
   xnames_brief <- c('A1','','','','A5','','','','A9','','','','A13','','','','A17','','','','A21','','','')


 # ---------   show me min and max of each parm
    Linf_low <- as.numeric()
    Linf_high <- as.numeric()
    k_low <- as.numeric()
    k_high <- as.numeric()
    L0_low <- as.numeric()
    L0_high <- as.numeric()
    CV0_low <- as.numeric()
    CV0_high <- as.numeric()
    CVmax_low <- as.numeric()
    CVmax_high <- as.numeric()

      for (i in 1: length(plot_me)) {
	  Linf_low[i] <- plot_me[[i]][1,2]
	  Linf_high[i] <- plot_me[[i]][1,6]
	  k_low[i] <- plot_me[[i]][2,2]
	  k_high[i] <- plot_me[[i]][2,6]
        L0_low[i] <- plot_me[[i]][6,2]
	  L0_high[i] <- plot_me[[i]][6,6]
        CV0_low[i] <- plot_me[[i]][4,2]
	  CV0_high[i] <- plot_me[[i]][4,6]
        CVmax_low[i] <- plot_me[[i]][5,2]
	  CVmax_high[i] <- plot_me[[i]][5,6]
	}

	c(min(Linf_low), max(Linf_high))
      c(min(k_low), max(k_high))
      c(min(L0_low), max(L0_high))
      c(min(CV0_low), max(CV0_high))
      c(min(CVmax_low), max(CVmax_high))

  # assign yaxis limits for each parm.
	Linf_ylim <- c(30,48)
	K_ylim <- c(0.0, 1.0)
	L0_ylim <- c(-2, 26)
	CV_0_ylim <- c(-.2,1.0)
	CV_max_ylim <- c(-.05,0.35)


#setwd('C:/Users/Erin.Bohaboy/Documents/MUS_Sample_Design/MUS_examples')


#  tiff(file="sample_fig_Sep25.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=300)
#  tiff(file="S6_auric_high_fig_Sep29.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=200)
  tiff(file="S6_auric_high_fig_dec4.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=200)

  par(mfcol=c(5,1))
  par(omi=c(0.25,0.2,0.2,0.1)) #set outer margins
  par(mai=c(0.15,0.15,.15,0)) #set inner margins

  xaxis_plotvals <- seq(1,24,1)	#because there are 24 different approaches: A1-A24



# -------------------  A. Linf

  p = 1	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
	plot(0,0,ylim=Linf_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
  for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
	}

#  add 50% box
  for (i in 1:length(each_approach_name)) {
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
		c(plot_me[[i]][p,3],plot_me[[i]][p,3],
			plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
	}

#  add mean  
  for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
	}

#  add true
  segments(0.5, parm_true[[1]], (max(xaxis_plotvals) + 0.5), parm_true[[1]], lty=2, col="black")

# draw axes

  axis(1, pos=Linf_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
  axis(1, pos=Linf_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
  axis(2, pos=0.5, at=seq(30,48,2), cex=1.1)

mtext(side=3,adj=0.05,line=-1,"A", cex=.75)
mtext(expression(italic('L'~infinity)), side=2, line=1.2, cex=0.9)

 mtext(side=3,adj=0.5,line=0,"S6", cex=1)


# -------------------  B. k

  p = 2	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
	plot(0,0,ylim=K_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
  for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
	}

#  add 50% box
  for (i in 1:length(each_approach_name)) {
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
		c(plot_me[[i]][p,3],plot_me[[i]][p,3],
			plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
	}

#  add mean  
  for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
	}

#  add true
  segments(0.5, parm_true[[2]], (max(xaxis_plotvals) + 0.5), parm_true[[2]], lty=2, col="black")

# draw axes

  axis(1, pos=K_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
  axis(1, pos=K_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
  axis(2, pos=0.5, at=seq(0, 1, 0.1), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"B", cex=.75)
mtext(expression(italic(K)), side=2, line=1.2, cex=0.9)


# -------------------  C. L0

  p = 6	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
	plot(0,0,ylim=L0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
  for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
	}

#  add 50% box
  for (i in 1:length(each_approach_name)) {
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
		c(plot_me[[i]][p,3],plot_me[[i]][p,3],
			plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
	}

#  add mean  
  for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
	}

#  add true
  segments(0.5, parm_true[[3]], (max(xaxis_plotvals) + 0.5), parm_true[[3]], lty=2, col="black")

# draw axes

  axis(1, pos=L0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
  axis(1, pos=L0_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
  axis(2, pos=0.5, at=seq(-2,26,4), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"C", cex=.75)
mtext(expression(italic("L"[0])), side=2, line=1.2, cex=0.9)




# -------------------  D. CV LINF

  p = 5	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
	plot(0,0,ylim=CV_max_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
  for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
	}

#  add 50% box
  for (i in 1:length(each_approach_name)) {
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
		c(plot_me[[i]][p,3],plot_me[[i]][p,3],
			plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
	}

#  add mean  
  for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
	}

#  add true
  segments(0.5, parm_true[[5]], (max(xaxis_plotvals) + 0.5), parm_true[[5]], lty=2, col="black")

# draw axes

  axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
  axis(1, pos=CV_max_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
  axis(2, pos=0.5, at=seq(-.05,0.35,.05), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"D", cex=.75)
mtext(expression(italic("CV  L"~infinity)), side=2, line=1.2, cex=0.9)




# -------------------  E. CV L0

  p = 4	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
	plot(0,0,ylim=CV_0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
  for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
	}

#  add 50% box
  for (i in 1:length(each_approach_name)) {
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
		c(plot_me[[i]][p,3],plot_me[[i]][p,3],
			plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
	}

#  add mean  
  for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
	}

#  add true
  segments(0.5, parm_true[[4]], (max(xaxis_plotvals) + 0.5), parm_true[[4]], lty=2, col="black")

# draw axes
  
  axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
  axis(1, pos=CV_0_ylim[1], at=xaxis_plotvals, labels= xnames_odd, las=1, cex.axis=0.9)
  axis(1, pos=CV_0_ylim[1], at=xaxis_plotvals, labels= xnames_even, las=1, cex.axis=0.9)
  axis(2, pos=0.5, at=seq(-0.2,1,.2), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"E", cex=.75)
mtext(expression(italic("CV  L"[0])), side=2, line=1.2, cex=0.9)

dev.off()

##########auricilla 8 scenario plot######
each_approach_name <- c('S4_A1','S4_A2','S4_A9','S4_A10','S4_A13','S4_A14', 'S4_A17','S4_A18')



S4_extract_all_approach <- list()

for (i in 1:length(each_approach_name)) {
  
  extract_thing <- get(each_approach_name[i])
  extract_summary <- data.frame(parm_name = character(), lower95 = numeric(), lower50 = numeric(), 
                                avg = numeric(), upper50 = numeric(), upper95 = numeric(), stringsAsFactors=FALSE)
  
  for (j in 1: ncol(extract_thing$parameter_outputs)) {
    extract_summary[j,1] <- colnames(extract_thing$parameter_outputs)[j]
    extract_summary[j,2] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.025, na.rm = TRUE))
    extract_summary[j,3] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.25, na.rm = TRUE))
    extract_summary[j,4] <- mean(extract_thing$parameter_outputs[,j],na.rm=TRUE)
    extract_summary[j,5] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.75, na.rm = TRUE))
    extract_summary[j,6] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.975, na.rm = TRUE))
  }
  
  S4_extract_all_approach[[i]] <- extract_summary
  names(S4_extract_all_approach)[[i]] <- each_approach_name[i]
  
}

plot_me <- S4_extract_all_approach

#  get the true population values for S4

S4_true <- c(S4_Auric_lowF$parameters$Linf, S4_Auric_lowF$parameters$k, S4_Auric_lowF$parameters$L0,
             S4_Auric_lowF$parameters$CV_L_0, S4_Auric_lowF$parameters$CV_L_age_max)





parm_true <- S4_true
each_approach_name <- c('S4_A1','S4_A2','S4_A9','S4_A10','S4_A13','S4_A14', 'S4_A17','S4_A18')
xnames_all <- c('','','',' ','','','','')
xnames_odd <-c('','','',' ','','','','')
  xnames_even<-c('','','',' ','','','','')
xnames_brief<-c('','','',' ','','','','')

xnames_all <- c('FOS','FOS','POS','POS ','POSlarge','POSlarge','POSsmall','POSsmall')

xnames_odd <- c('FOS','FOS','POS','POS ','POSlarge','POSlarge','POSsmall','POSsmall')

xnames_even <- c('FOS','FOS','POS','POS ','POSlarge','POSlarge','POSsmall','POSsmall')

xnames_brief <-c('','FOS','','POS ','','POSlarge','','POSsmall')

#xnames_all <- c('300','75','300','75','300','75','300','75')
#xnames_odd <-c('300','75','300','75','300','75','300','75')
#xnames_even <- c('300','75','300','75','300','75','300','75')
#xnames_brief <- c('300','75','300','75','300','75','300','75')

#xnames_all <- c('A1','A2','A9','A10','A13','A14','A17','A18')
#xnames_odd <-c('A1','A2','A9','A10','A13','A14','A17','A18')
#xnames_even <- c('A1','A2','A9','A10','A13','A14','A17','A18')
#xnames_brief <- c('A1','A2','A9','A10','A13','A14','A17','A18')



# ---------   show me min and max of each parm
Linf_low <- as.numeric()
Linf_high <- as.numeric()
k_low <- as.numeric()
k_high <- as.numeric()
L0_low <- as.numeric()
L0_high <- as.numeric()
CV0_low <- as.numeric()
CV0_high <- as.numeric()
CVmax_low <- as.numeric()
CVmax_high <- as.numeric()

for (i in 1: length(plot_me)) {
  Linf_low[i] <- plot_me[[i]][1,2]
  Linf_high[i] <- plot_me[[i]][1,6]
  k_low[i] <- plot_me[[i]][2,2]
  k_high[i] <- plot_me[[i]][2,6]
  L0_low[i] <- plot_me[[i]][6,2]
  L0_high[i] <- plot_me[[i]][6,6]
  CV0_low[i] <- plot_me[[i]][4,2]
  CV0_high[i] <- plot_me[[i]][4,6]
  CVmax_low[i] <- plot_me[[i]][5,2]
  CVmax_high[i] <- plot_me[[i]][5,6]
}

c(min(Linf_low), max(Linf_high))
c(min(k_low), max(k_high))
c(min(L0_low), max(L0_high))
c(min(CV0_low), max(CV0_high))
c(min(CVmax_low), max(CVmax_high))

# assign yaxis limits for each parm.
Linf_ylim <- c(30,48)
K_ylim <- c(0.0, 1.0)
L0_ylim <- c(-2, 26)
CV_0_ylim <- c(-.2,1.0)
CV_max_ylim <- c(-.05,0.35)


setwd('C:/Users/Erin.Bohaboy/Documents/MUS_Sample_Design/MUS_examples')


#  tiff(file="sample_fig_Sep25.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=300)
#  tiff(file="S6_auric_high_fig_Sep29.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=200)
tiff(file="S4_S6_8scenario_Dec4_names_cex_large.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=200)

par(mfcol=c(5,2))
par(omi=c(0.25,0.3,0.2,0.1)) #set outer margins
par(mai=c(0.15,0.15,.15,0)) #set inner margins

xaxis_plotvals <- seq(1,8,1)	#because there are 8 different approaches: A1-A8



# -------------------  A. Linf

p = 1	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=Linf_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}



  polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
          c(plot_me[[2]][p,3],plot_me[[2]][p,3],
            plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")
  
  polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
          c(plot_me[[4]][p,3],plot_me[[4]][p,3],
            plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

  polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
          c(plot_me[[6]][p,3],plot_me[[6]][p,3],
            plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")
  
  polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
          c(plot_me[[8]][p,3],plot_me[[8]][p,3],
            plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")
#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[1]], (max(xaxis_plotvals) + 0.5), parm_true[[1]], lty=2, col="black")

# draw axes

axis(1, pos=Linf_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 1,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(30,48,2), cex=1.1)

mtext(side=3,adj=0.05,line=-1.2,"A", cex=.75)
mtext(expression(italic('L'~infinity)), side=2, line=1.2, cex=1.0)

mtext(side=3,adj=0.5,line=0,"S4", cex=1)


# -------------------  B. k

p = 2	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=K_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}



polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")
#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[2]], (max(xaxis_plotvals) + 0.5), parm_true[[2]], lty=2, col="black")

# draw axes

axis(1, pos=K_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.1,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(0, 1, 0.1), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"B", cex=.75)
mtext(expression(italic(K)), side=2, line=1.2, cex=1.0)


# -------------------  C. L0

p = 6	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=L0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[3]], (max(xaxis_plotvals) + 0.5), parm_true[[3]], lty=2, col="black")

# draw axes

axis(1, pos=L0_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 1.2,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(-2,26,4), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"C", cex=.75)
mtext(expression(italic("L"[0])), side=2, line=1.2, cex=1.0)




# -------------------  D. CV LINF

p = 5	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_max_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[5]], (max(xaxis_plotvals) + 0.5), parm_true[[5]], lty=2, col="black")

# draw axes

#axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_max_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.025,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(-.05,0.35,.05), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"D", cex=.75)
mtext(expression(italic("CV  L"~infinity)), side=2, line=1.2, cex=1.0)




# -------------------  E. CV L0

p = 4	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[4]], (max(xaxis_plotvals) + 0.5), parm_true[[4]], lty=2, col="black")

# draw axes

#axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_0_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.1,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(-0.2,1,.2), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"E", cex=.75)
mtext(expression(italic("CV  L"[0])), side=2, line=1.2, cex=1.0)



####S6####

each_approach_name <- c('S6_A1','S6_A2','S6_A9','S6_A10','S6_A13','S6_A14', 'S6_A17','S6_A18')


S6_extract_all_approach <- list()

for (i in 1:length(each_approach_name)) {
  
  extract_thing <- get(each_approach_name[i])
  extract_summary <- data.frame(parm_name = character(), lower95 = numeric(), lower50 = numeric(), 
                                avg = numeric(), upper50 = numeric(), upper95 = numeric(), stringsAsFactors=FALSE)
  
  for (j in 1: ncol(extract_thing$parameter_outputs)) {
    extract_summary[j,1] <- colnames(extract_thing$parameter_outputs)[j]
    extract_summary[j,2] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.025, na.rm = TRUE))
    extract_summary[j,3] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.25, na.rm = TRUE))
    extract_summary[j,4] <- mean(extract_thing$parameter_outputs[,j],na.rm=TRUE)
    extract_summary[j,5] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.75, na.rm = TRUE))
    extract_summary[j,6] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.975, na.rm = TRUE))
  }
  
  S6_extract_all_approach[[i]] <- extract_summary
  names(S6_extract_all_approach)[[i]] <- each_approach_name[i]
  
}


#  get the true population values for S6

S6_true <- c(S6_Auric_highF$parameters$Linf, S6_Auric_highF$parameters$k, S6_Auric_highF$parameters$L0,
             S6_Auric_highF$parameters$CV_L_0, S6_Auric_highF$parameters$CV_L_age_max)


plot_me <- S6_extract_all_approach

parm_true <- S6_true


each_approach_name <- c('S6_A1','S6_A2','S6_A9','S6_A10','S6_A13','S6_A14', 'S6_A17','S6_A18')

#xnames_all <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

xnames_all <- c('FOS','FOS','POS','POS ','POS+large','POS+large','POS+small','POS+small')

xnames_odd <- c('FOS','FOS','POS','POS ','POS+large','POS+large','POS+small','POS+small')

xnames_even <- c('FOS','FOS','POS','POS ','POS+large','POS+large','POS+small','POS+small')

xnames_brief <- c('FOS','FOS','POS','POS ','POS+large','POS+large','POS+small','POS+small')

xnames_all <- c('A1','A2','A9','A10','A13','A14','A17','A18')
xnames_odd <-c('A1','A2','A9','A10','A13','A14','A17','A18')
xnames_even <- c('A1','A2','A9','A10','A13','A14','A17','A18')
xnames_brief <- c('A1','A2','A9','A10','A13','A14','A17','A18')


#xnames_odd <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

#xnames_even <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

#xnames_brief <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')


# ---------   show me min and max of each parm
Linf_low <- as.numeric()
Linf_high <- as.numeric()
k_low <- as.numeric()
k_high <- as.numeric()
L0_low <- as.numeric()
L0_high <- as.numeric()
CV0_low <- as.numeric()
CV0_high <- as.numeric()
CVmax_low <- as.numeric()
CVmax_high <- as.numeric()

for (i in 1: length(plot_me)) {
  Linf_low[i] <- plot_me[[i]][1,2]
  Linf_high[i] <- plot_me[[i]][1,6]
  k_low[i] <- plot_me[[i]][2,2]
  k_high[i] <- plot_me[[i]][2,6]
  L0_low[i] <- plot_me[[i]][6,2]
  L0_high[i] <- plot_me[[i]][6,6]
  CV0_low[i] <- plot_me[[i]][4,2]
  CV0_high[i] <- plot_me[[i]][4,6]
  CVmax_low[i] <- plot_me[[i]][5,2]
  CVmax_high[i] <- plot_me[[i]][5,6]
}

c(min(Linf_low), max(Linf_high))
c(min(k_low), max(k_high))
c(min(L0_low), max(L0_high))
c(min(CV0_low), max(CV0_high))
c(min(CVmax_low), max(CVmax_high))

# assign yaxis limits for each parm.
Linf_ylim <- c(30,48)
K_ylim <- c(0.0, 1.0)
L0_ylim <- c(-2, 26)
CV_0_ylim <- c(-.2,1.0)
CV_max_ylim <- c(-.05,0.35)



# -------------------  A. Linf

p = 1	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=Linf_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[1]], (max(xaxis_plotvals) + 0.5), parm_true[[1]], lty=2, col="black")

# draw axes

#axis(1, pos=Linf_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=Linf_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 1,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(30,48,2), cex=1.1)

mtext(side=3,adj=0.05,line=-1.2,"F", cex=.75)
mtext(expression(italic('L'~infinity)), side=2, line=1.2, cex=1.0)

mtext(side=3,adj=0.5,line=0,"S6", cex=1)


# -------------------  B. k

p = 2	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=K_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")


#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[2]], (max(xaxis_plotvals) + 0.5), parm_true[[2]], lty=2, col="black")

# draw axes

#axis(1, pos=K_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=K_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.1,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(0, 1, 0.1), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"G", cex=.75)
mtext(expression(italic(K)), side=2, line=1.2, cex=1.0)


# -------------------  C. L0

p = 6	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=L0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[3]], (max(xaxis_plotvals) + 0.5), parm_true[[3]], lty=2, col="black")

# draw axes

#axis(1, pos=L0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=L0_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 1.2,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(-2,26,4), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"H", cex=.75)
mtext(expression(italic("L"[0])), side=2, line=1.2, cex=1.0)




# -------------------  D. CV LINF

p = 5	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_max_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[5]], (max(xaxis_plotvals) + 0.5), parm_true[[5]], lty=2, col="black")

# draw axes

#axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_max_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.025,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(-.05,0.35,.05), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"I", cex=.75)
mtext(expression(italic("CV  L"~infinity)), side=2, line=1.2, cex=1.0)




# -------------------  E. CV L0

p = 4	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[4]], (max(xaxis_plotvals) + 0.5), parm_true[[4]], lty=2, col="black")

# draw axes

#axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_0_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.1,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(-0.2,1,.2), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"J", cex=.75)
mtext(expression(italic("CV  L"[0])), side=2, line=1.2, cex=1.0)

dev.off()

#  -------------------------------------------------------------------------------------------------------------------
#  -------------------------------------------------------------------------------------------------------------------
#  -------------------------------------------------------------------------------------------------------------------


##export results

write.csv(S6_extract_all_approach, "S6_results_march.csv")
write.csv(S4_extract_all_approach, "S4_results_March.csv")
write.csv(S3_extract_all_approach, "S3_results_March.csv")
write.csv(S1_extract_all_approach, "S1_results_March.csv")



#######Representative Index (RI)######

S4A_Auric_noF <- simulate_population_harvest(Linf = 32.5, Linf_sd = 2.5, 
                                             M = 0.18, F = 0, Lorenzen = TRUE, 
                                             mincat = 20, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
                                             L0 = 10, L0_sd = 2.5, k = 0.6, k_sd = 0, 
                                             Amax = 32, age_max = 15, N = 100000)

summary(S4A_Auric_noF$population$age)
S4A_hist_age_pop <- hist(S4A_Auric_noF$population$age, breaks = seq(0,31,1),include.lowest=TRUE, right=FALSE, plot=FALSE)
sum(S4A_hist_age_pop$density)

summary(S4A_Auric_noF$population$length)
str(S4A_Auric_noF$population$length)
S4A_hist_length_pop <- hist(S4A_Auric_noF$population$length, breaks = seq(0,50,2),include.lowest=TRUE, right=FALSE,plot=FALSE)
S4_length_harv <- hist(S4_Auric_lowF$harvest$length, breaks = seq(0,50,2),include.lowest=TRUE, right=FALSE,plot=FALSE)
S6_length_harv <- hist(S6_Auric_highF$harvest$length, breaks = seq(0,50,2),include.lowest=TRUE, right=FALSE,plot=FALSE)

sum_pop<-sum(S4A_hist_length_pop$counts)
prop_pop<-S4A_hist_length_pop$counts/sum_pop

sum_low<-sum(S4_length_harv$counts)
harv_low<-S4_length_harv$counts/sum_low

sum_high<-sum(S6_length_harv$counts)
harv_high<-S6_length_harv$counts/sum_high

lbin<-seq(0,48,2)
length(lbin)
prop_at_lenth<-cbind(lbin,prop_pop,harv_low,harv_high)
prop_at_lenth<-as.data.frame(prop_at_lenth)


#onaga

summary(S10A_onaga_noF$population$length)
str(S10A_onaga_noF$population$length)

S10A_hist_length_pop <- hist(S10A_onaga_noF$population$length, breaks = seq(0,110,5),include.lowest=TRUE, right=FALSE,plot=FALSE)
S10_length_harv <- hist(S10_onaga_lowF$harvest$length, breaks = seq(0,110,5),include.lowest=TRUE, right=FALSE,plot=FALSE)
S12_length_harv <- hist(S12_onaga_highF$harvest$length, breaks = seq(0,110,5),include.lowest=TRUE, right=FALSE,plot=FALSE)

osum_pop<-sum(S10A_hist_length_pop$counts)
oprop_pop<-S10A_hist_length_pop$counts/sum_pop

osum_low<-sum(S10_length_harv$counts)
oharv_low<-S10_length_harv$counts/sum_low

osum_high<-sum(S12_length_harv$counts)
oharv_high<-S12_length_harv$counts/sum_high

olbin<-seq(0,105,5)
length(olbin)
oprop_at_lenth<-cbind(olbin,oprop_pop,oharv_low,oharv_high)
oprop_at_lenth<-as.data.frame(oprop_at_lenth)



col2rgb("black")
col2rgb("darkgrey")
col2rgb("lightgrey")
col2rgb("white")
mycol_dg <- rgb(25, 25, 25, max = 255, alpha = 125)
mycol_white <- rgb(255, 255, 255, max = 255, alpha = 125)

tiff(file="RI_fig.tiff",width=12, height=10, units = "cm", pointsize = 10, res=200)
par(mfcol=c(2,2))
par(omi=c(0.1,0.1,0.1,0.1)) #set outer margins
par(mai=c(0.7,0.7,.3,.3)) 
#auricilla
barplot(height=prop_at_lenth$harv_low, names=prop_at_lenth$lbin, col=mycol_dg, xlim=c(0,25),ylab="Proportion", xlab="Length Bin (cm)", ylim=c(0,.30))
barplot(prop_at_lenth$prop_pop,col=mycol_white, add=TRUE)
mtext(side=3,adj=0.5,line=0,"S4", col="black", cex=1)
barplot(height=prop_at_lenth$harv_high, names=prop_at_lenth$lbin, col=mycol_dg, xlim=c(0,25),ylab="Proportion",xlab="Length Bin (cm)", ylim=c(0,.30))
barplot(prop_at_lenth$prop_pop,col=mycol_white, add=TRUE)
mtext(side=3,adj=0.5,line=0,"S6", col="black", cex=1)
#onaga
barplot(height=oprop_at_lenth$oharv_low, names=oprop_at_lenth$olbin, col=mycol_dg, xlim=c(0,24),ylab="Proportion", xlab="Length Bin (cm)", ylim=c(0,.10))
barplot(oprop_at_lenth$oprop_pop,col=mycol_white, add=TRUE)
mtext(side=3,adj=0.5,line=0,"S10", col="black", cex=1)
barplot(height=oprop_at_lenth$oharv_high, names=oprop_at_lenth$olbin, col=mycol_dg, xlim=c(0,24),ylab="Proportion",xlab="Length Bin (cm)", ylim=c(0,.10))
barplot(oprop_at_lenth$oprop_pop,col=mycol_white, add=TRUE)
mtext(side=3,adj=0.5,line=0,"S12", col="black", cex=1)
dev.off()

####onage S7 ######

S7_A1 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A2 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A3 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A4 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A5 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A6 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A7 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A8 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A9 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A10 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 75, 
                     sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A11 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A12 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 75, 
                     sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

# error at first, not sure what happened, ran again w/ success.
S7_A13 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = FALSE, constrained = FALSE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A14 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 75, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = FALSE, constrained = FALSE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A15 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = FALSE, constrained = TRUE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A16 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 75, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = FALSE, constrained = TRUE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A17 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A18 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 75, 
                     sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A19 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A20 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 75, 
                     sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A21 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A22 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 75, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A23 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S7_A24 <- LH_sample(sim_output = S7_onaga_lowF, n_boots = 1000, samp_size = 75, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 


look_at_me <- S7_A5
look_at_me$params_input_output
head(look_at_me$parameter_outputs)
hist(look_at_me$list_boot_samps[[1]]$length, breaks=seq(0,125,5))
look_at_me$list_boot_samps[[1]]
look_at_me$simulation_params
look_at_me$list_boot_CVs[[1]]


# just spent 2 hours trouble shooting. Make sure to go back and fix min. capture length for auricilla


ls()
# rm(list=ls()[27:81])




each_approach_name <- c('S7_A1','S7_A2','S7_A3','S7_A4','S7_A5','S7_A6','S7_A7','S7_A8',
                        'S7_A9','S7_A10','S7_A11','S7_A12','S7_A13','S7_A14','S7_A15','S7_A16',
                        'S7_A17','S7_A18','S7_A19','S7_A20','S7_A21','S7_A22','S7_A23','S7_A24')

S7_extract_all_approach <- list()

for (i in 1:length(each_approach_name)) {
  
  extract_thing <- get(each_approach_name[i])
  extract_summary <- data.frame(parm_name = character(), lower95 = numeric(), lower50 = numeric(), 
                                avg = numeric(), upper50 = numeric(), upper95 = numeric(), stringsAsFactors=FALSE)
  
  for (j in 1: ncol(extract_thing$parameter_outputs)) {
    extract_summary[j,1] <- colnames(extract_thing$parameter_outputs)[j]
    extract_summary[j,2] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.025, na.rm = TRUE))
    extract_summary[j,3] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.25, na.rm = TRUE))
    extract_summary[j,4] <- mean(extract_thing$parameter_outputs[,j],na.rm=TRUE)
    extract_summary[j,5] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.75, na.rm = TRUE))
    extract_summary[j,6] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.975, na.rm = TRUE))
  }
  
  S7_extract_all_approach[[i]] <- extract_summary
  names(S7_extract_all_approach)[[i]] <- each_approach_name[i]
  
}


#  get the true population values for S7

S7_true <- c(S7_onaga_lowF$parameters$Linf, S7_onaga_lowF$parameters$k, S7_onaga_lowF$parameters$L0,
              S7_onaga_lowF$parameters$CV_L_0, S7_onaga_lowF$parameters$CV_L_age_max)


# rm(list=ls()[])


#  -------------------------------------------------------------------------------------------------------------------
# make a figure: 1 column, 25 sampling approaches per pane.


plot_me <- S7_extract_all_approach
parm_true <- S7_true
each_approach_name <- c('S7_A1','S7_A2','S7_A3','S7_A4','S7_A5','S7_A6','S7_A7','S7_A8',
                        'S7_A9','S7_A10','S7_A11','S7_A12','S7_A13','S7_A14','S7_A15','S7_A16',
                        'S7_A17','S7_A18','S7_A19','S7_A20','S7_A21','S7_A22','S7_A23','S7_A24')


xnames_all <- c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15',
                'A16','A17','A18','A19','A20','A21','A22','A23','A24')
xnames_odd <- c('A1','','A3','','A5','','A7','','A9','','A11','','A13','','A15',
                '','A17','','A19','','A21','','A23','')
xnames_even <- c('','A2','','A4','','A6','','A8','','A10','','A12','','A14','',
                 'A16','','A18','','A20','','A22','','A24')
xnames_brief <- c('A1','','','','A5','','','','A9','','','','A13','','','','A17','','','','A21','','','')


# ---------   show me min and max of each parm
Linf_low <- as.numeric()
Linf_high <- as.numeric()
k_low <- as.numeric()
k_high <- as.numeric()
L0_low <- as.numeric()
L0_high <- as.numeric()
CV0_low <- as.numeric()
CV0_high <- as.numeric()
CVmax_low <- as.numeric()
CVmax_high <- as.numeric()

for (i in 1: length(plot_me)) {
  Linf_low[i] <- plot_me[[i]][1,2]
  Linf_high[i] <- plot_me[[i]][1,6]
  k_low[i] <- plot_me[[i]][2,2]
  k_high[i] <- plot_me[[i]][2,6]
  L0_low[i] <- plot_me[[i]][6,2]
  L0_high[i] <- plot_me[[i]][6,6]
  CV0_low[i] <- plot_me[[i]][4,2]
  CV0_high[i] <- plot_me[[i]][4,6]
  CVmax_low[i] <- plot_me[[i]][5,2]
  CVmax_high[i] <- plot_me[[i]][5,6]
}

c(min(Linf_low), max(Linf_high))
c(min(k_low), max(k_high))
c(min(L0_low), max(L0_high))
c(min(CV0_low), max(CV0_high))
c(min(CVmax_low), max(CVmax_high))

# assign yaxis limits for each parm.
Linf_ylim <- c(95,110)
K_ylim <- c(0.1, 0.16)
L0_ylim <- c(8, 18)
CV_0_ylim <- c(0,0.3)
CV_max_ylim <- c(-0.02,0.05)


setwd('C:/Users/Erin.Bohaboy/Documents/MUS_Sample_Design/MUS_examples')


tiff(file="S7_onaga_lowF_fig_Dec4.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=200)
par(mfcol=c(5,1))
par(omi=c(0.25,0.2,0.2,0.1)) #set outer margins
par(mai=c(0.15,0.15,.15,0)) #set inner margins

xaxis_plotvals <- seq(1,24,1)	#because there are 24 different approaches: A1-A24



# -------------------  A. Linf

p = 1	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=Linf_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[1]], (max(xaxis_plotvals) + 0.5), parm_true[[1]], lty=2, col="black")

# draw axes

axis(1, pos=Linf_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=Linf_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(95,110,5), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"A", cex=.75)
mtext(expression(italic('L'~infinity)), side=2, line=1.2, cex=0.9)

mtext(side=3,adj=0.5,line=0,"S7", cex=1)


# -------------------  B. k

p = 2	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=K_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[2]], (max(xaxis_plotvals) + 0.5), parm_true[[2]], lty=2, col="black")

# draw axes

axis(1, pos=K_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=K_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(0.1, 0.16, 0.02), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"B", cex=.75)
mtext(expression(italic(K)), side=2, line=1.2, cex=0.9)


# -------------------  C. L0

p = 6	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=L0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[3]], (max(xaxis_plotvals) + 0.5), parm_true[[3]], lty=2, col="black")

# draw axes

axis(1, pos=L0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=L0_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(8,18,2), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"C", cex=.75)
mtext(expression(italic("L"[0])), side=2, line=1.2, cex=0.9)


# -------------------  D. CV LINF

p = 5	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_max_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[5]], (max(xaxis_plotvals) + 0.5), parm_true[[5]], lty=2, col="black")

# draw axes

axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_max_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(-0.02,0.05,.01), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"D", cex=.75)
mtext(expression(italic("CV  L"~infinity)), side=2, line=1.2, cex=0.9)




# -------------------  E. CV L0

p = 4	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[4]], (max(xaxis_plotvals) + 0.5), parm_true[[4]], lty=2, col="black")

# draw axes

axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_0_ylim[1], at=xaxis_plotvals, labels= xnames_odd, las=1, cex.axis=0.9)
axis(1, pos=CV_0_ylim[1], at=xaxis_plotvals, labels= xnames_even, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(0,0.3,.05), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"E", cex=.75)
mtext(expression(italic("CV  L"[0])), side=2, line=1.2, cex=0.9)

dev.off()









#  C. S10_POS	#str(S10_POS)	S10_POS$Avg_age

	S10_A1 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A2 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A3 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 
	
	S10_A4 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A5 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A6 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A7 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A8 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A9 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A10 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A11 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A12 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	# error at first, not sure what happened, ran again w/ success.
	S10_A13 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A14 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 
	
	S10_A15 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A16 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A17 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 
	
	S10_A18 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A19 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A20 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A21 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A22 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A23 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S10_A24 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 


	look_at_me <- S10_A5
	look_at_me$params_input_output
	head(look_at_me$parameter_outputs)
		hist(look_at_me$list_boot_samps[[1]]$length, breaks=seq(0,125,5))
	look_at_me$list_boot_samps[[1]]
	look_at_me$simulation_params
	look_at_me$list_boot_CVs[[1]]


# just spent 2 hours trouble shooting. Make sure to go back and fix min. capture length for auricilla


ls()
# rm(list=ls()[27:81])




 each_approach_name <- c('S10_A1','S10_A2','S10_A3','S10_A4','S10_A5','S10_A6','S10_A7','S10_A8',
				'S10_A9','S10_A10','S10_A11','S10_A12','S10_A13','S10_A14','S10_A15','S10_A16',
				'S10_A17','S10_A18','S10_A19','S10_A20','S10_A21','S10_A22','S10_A23','S10_A24')

S10_extract_all_approach <- list()

for (i in 1:length(each_approach_name)) {

	extract_thing <- get(each_approach_name[i])
	extract_summary <- data.frame(parm_name = character(), lower95 = numeric(), lower50 = numeric(), 
				avg = numeric(), upper50 = numeric(), upper95 = numeric(), stringsAsFactors=FALSE)

 for (j in 1: ncol(extract_thing$parameter_outputs)) {
	  extract_summary[j,1] <- colnames(extract_thing$parameter_outputs)[j]
	  extract_summary[j,2] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.025, na.rm = TRUE))
	  extract_summary[j,3] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.25, na.rm = TRUE))
	  extract_summary[j,4] <- mean(extract_thing$parameter_outputs[,j],na.rm=TRUE)
	  extract_summary[j,5] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.75, na.rm = TRUE))
	  extract_summary[j,6] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.975, na.rm = TRUE))
	  }

	S10_extract_all_approach[[i]] <- extract_summary
	names(S10_extract_all_approach)[[i]] <- each_approach_name[i]

  }


#  get the true population values for S10

S10_true <- c(S10_onaga_lowF$parameters$Linf, S10_onaga_lowF$parameters$k, S10_onaga_lowF$parameters$L0,
			S10_onaga_lowF$parameters$CV_L_0, S10_onaga_lowF$parameters$CV_L_age_max)


# rm(list=ls()[])


#  -------------------------------------------------------------------------------------------------------------------
# make a figure: 1 column, 25 sampling approaches per pane.
 

   plot_me <- S10_extract_all_approach
   parm_true <- S10_true
   each_approach_name <- c('S10_A1','S10_A2','S10_A3','S10_A4','S10_A5','S10_A6','S10_A7','S10_A8',
				'S10_A9','S10_A10','S10_A11','S10_A12','S10_A13','S10_A14','S10_A15','S10_A16',
				'S10_A17','S10_A18','S10_A19','S10_A20','S10_A21','S10_A22','S10_A23','S10_A24')


   xnames_all <- c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15',
				'A16','A17','A18','A19','A20','A21','A22','A23','A24')
   xnames_odd <- c('A1','','A3','','A5','','A7','','A9','','A11','','A13','','A15',
				'','A17','','A19','','A21','','A23','')
   xnames_even <- c('','A2','','A4','','A6','','A8','','A10','','A12','','A14','',
				'A16','','A18','','A20','','A22','','A24')
   xnames_brief <- c('A1','','','','A5','','','','A9','','','','A13','','','','A17','','','','A21','','','')


 # ---------   show me min and max of each parm
    Linf_low <- as.numeric()
    Linf_high <- as.numeric()
    k_low <- as.numeric()
    k_high <- as.numeric()
    L0_low <- as.numeric()
    L0_high <- as.numeric()
    CV0_low <- as.numeric()
    CV0_high <- as.numeric()
    CVmax_low <- as.numeric()
    CVmax_high <- as.numeric()

      for (i in 1: length(plot_me)) {
	  Linf_low[i] <- plot_me[[i]][1,2]
	  Linf_high[i] <- plot_me[[i]][1,6]
	  k_low[i] <- plot_me[[i]][2,2]
	  k_high[i] <- plot_me[[i]][2,6]
        L0_low[i] <- plot_me[[i]][6,2]
	  L0_high[i] <- plot_me[[i]][6,6]
        CV0_low[i] <- plot_me[[i]][4,2]
	  CV0_high[i] <- plot_me[[i]][4,6]
        CVmax_low[i] <- plot_me[[i]][5,2]
	  CVmax_high[i] <- plot_me[[i]][5,6]
	}

	c(min(Linf_low), max(Linf_high))
      c(min(k_low), max(k_high))
      c(min(L0_low), max(L0_high))
      c(min(CV0_low), max(CV0_high))
      c(min(CVmax_low), max(CVmax_high))

  # assign yaxis limits for each parm.
	Linf_ylim <- c(95,110)
	K_ylim <- c(0.1, 0.16)
	L0_ylim <- c(8, 18)
	CV_0_ylim <- c(0,0.3)
	CV_max_ylim <- c(-0.02,0.05)


setwd('C:/Users/Erin.Bohaboy/Documents/MUS_Sample_Design/MUS_examples')


  tiff(file="S10_POS_fig_dec4.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=200)
  par(mfcol=c(5,1))
  par(omi=c(0.25,0.2,0.2,0.1)) #set outer margins
  par(mai=c(0.15,0.15,.15,0)) #set inner margins
  
  xaxis_plotvals <- seq(1,24,1)	#because there are 24 different approaches: A1-A24



# -------------------  A. Linf

  p = 1	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
	plot(0,0,ylim=Linf_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
  for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
	}

#  add 50% box
  for (i in 1:length(each_approach_name)) {
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
		c(plot_me[[i]][p,3],plot_me[[i]][p,3],
			plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
	}

#  add mean  
  for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
	}

#  add true
  segments(0.5, parm_true[[1]], (max(xaxis_plotvals) + 0.5), parm_true[[1]], lty=2, col="black")

# draw axes

  axis(1, pos=Linf_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
  axis(1, pos=Linf_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
  axis(2, pos=0.5, at=seq(95,110,5), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"A", cex=.75)
mtext(expression(italic('L'~infinity)), side=2, line=1.2, cex=0.9)

 mtext(side=3,adj=0.5,line=0,"S10", cex=1)


# -------------------  B. k

  p = 2	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
	plot(0,0,ylim=K_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
  for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
	}

#  add 50% box
  for (i in 1:length(each_approach_name)) {
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
		c(plot_me[[i]][p,3],plot_me[[i]][p,3],
			plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
	}

#  add mean  
  for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
	}

#  add true
  segments(0.5, parm_true[[2]], (max(xaxis_plotvals) + 0.5), parm_true[[2]], lty=2, col="black")

# draw axes

  axis(1, pos=K_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
  axis(1, pos=K_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
  axis(2, pos=0.5, at=seq(0.1, 0.16, 0.02), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"B", cex=.75)
mtext(expression(italic(K)), side=2, line=1.2, cex=0.9)


# -------------------  C. L0

  p = 6	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
	plot(0,0,ylim=L0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
  for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
	}

#  add 50% box
  for (i in 1:length(each_approach_name)) {
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
		c(plot_me[[i]][p,3],plot_me[[i]][p,3],
			plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
	}

#  add mean  
  for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
	}

#  add true
  segments(0.5, parm_true[[3]], (max(xaxis_plotvals) + 0.5), parm_true[[3]], lty=2, col="black")

# draw axes

  axis(1, pos=L0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
  axis(1, pos=L0_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
  axis(2, pos=0.5, at=seq(8,18,2), cex=1.1)

  mtext(side=3,adj=0.05,line=-1.5,"C", cex=.75)
mtext(expression(italic("L"[0])), side=2, line=1.2, cex=0.9)


# -------------------  D. CV LINF

  p = 5	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
	plot(0,0,ylim=CV_max_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
  for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
	}

#  add 50% box
  for (i in 1:length(each_approach_name)) {
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
		c(plot_me[[i]][p,3],plot_me[[i]][p,3],
			plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
	}

#  add mean  
  for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
	}

#  add true
  segments(0.5, parm_true[[5]], (max(xaxis_plotvals) + 0.5), parm_true[[5]], lty=2, col="black")

# draw axes

  axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
  axis(1, pos=CV_max_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
  axis(2, pos=0.5, at=seq(-0.02,0.05,.01), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"D", cex=.75)
mtext(expression(italic("CV  L"~infinity)), side=2, line=1.2, cex=0.9)




# -------------------  E. CV L0

  p = 4	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
	plot(0,0,ylim=CV_0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
  for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
	}

#  add 50% box
  for (i in 1:length(each_approach_name)) {
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
		c(plot_me[[i]][p,3],plot_me[[i]][p,3],
			plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
	}

#  add mean  
  for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
	}

#  add true
  segments(0.5, parm_true[[4]], (max(xaxis_plotvals) + 0.5), parm_true[[4]], lty=2, col="black")

# draw axes
  
  axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
  axis(1, pos=CV_0_ylim[1], at=xaxis_plotvals, labels= xnames_odd, las=1, cex.axis=0.9)
  axis(1, pos=CV_0_ylim[1], at=xaxis_plotvals, labels= xnames_even, las=1, cex.axis=0.9)
  axis(2, pos=0.5, at=seq(0,0.3,.05), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"E", cex=.75)
mtext(expression(italic("CV  L"[0])), side=2, line=1.2, cex=0.9)

dev.off()



####S9 Onaga high F small included####
S9_A1 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A2 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A3 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A4 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                    save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A5 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A6 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A7 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A8 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 75, 
                    sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
                    supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                    supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A9 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A10 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 75, 
                     sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A11 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A12 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 75, 
                     sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A13 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = FALSE, constrained = FALSE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A14 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 75, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = FALSE, constrained = FALSE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A15 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = FALSE, constrained = TRUE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A16 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 75, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = FALSE, constrained = TRUE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A17 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A18 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 75, 
                     sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A19 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A20 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 75, 
                     sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A21 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A22 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 75, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A23 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S9_A24 <- LH_sample(sim_output = S9_onaga_highF, n_boots = 1000, samp_size = 75, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 


look_at_me <- S9_A1
look_at_me$params_input_output
head(look_at_me$parameter_outputs)
hist(look_at_me$list_boot_samps[[300]]$length, breaks=seq(0,125,5))
look_at_me$list_boot_samps[[1]]
look_at_me$simulation_params
look_at_me$list_boot_CVs[[1]]



ls()
# rm(list=ls()[27:81])



each_approach_name <- c('S9_A1','S9_A2','S9_A3','S9_A4','S9_A5','S9_A6','S9_A7','S9_A8',
                        'S9_A9','S9_A10','S9_A11','S9_A12','S9_A13','S9_A14','S9_A15','S9_A16',
                        'S9_A17','S9_A18','S9_A19','S9_A20','S9_A21','S9_A22','S9_A23','S9_A24')

S9_extract_all_approach <- list()

for (i in 1:length(each_approach_name)) {
  
  extract_thing <- get(each_approach_name[i])
  extract_summary <- data.frame(parm_name = character(), lower95 = numeric(), lower50 = numeric(), 
                                avg = numeric(), upper50 = numeric(), upper95 = numeric(), stringsAsFactors=FALSE)
  
  for (j in 1: ncol(extract_thing$parameter_outputs)) {
    extract_summary[j,1] <- colnames(extract_thing$parameter_outputs)[j]
    extract_summary[j,2] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.025, na.rm = TRUE))
    extract_summary[j,3] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.25, na.rm = TRUE))
    extract_summary[j,4] <- mean(extract_thing$parameter_outputs[,j],na.rm=TRUE)
    extract_summary[j,5] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.75, na.rm = TRUE))
    extract_summary[j,6] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.975, na.rm = TRUE))
  }
  
  S9_extract_all_approach[[i]] <- extract_summary
  names(S9_extract_all_approach)[[i]] <- each_approach_name[i]
  
}


#  get the true population values for S9

S9_true <- c(S9_onaga_highF$parameters$Linf, S9_onaga_highF$parameters$k, S9_onaga_highF$parameters$L0,
              S9_onaga_highF$parameters$CV_L_0, S9_onaga_highF$parameters$CV_L_age_max)


# rm(list=ls()[])


#  -------------------------------------------------------------------------------------------------------------------
# make a figure: 1 column, 25 sampling approaches per pane.


plot_me <- S9_extract_all_approach
parm_true <- S9_true
each_approach_name <- c('S9_A1','S9_A2','S9_A3','S9_A4','S9_A5','S9_A6','S9_A7','S9_A8',
                        'S9_A9','S9_A10','S9_A11','S9_A12','S9_A13','S9_A14','S9_A15','S9_A16',
                        'S9_A17','S9_A18','S9_A19','S9_A20','S9_A21','S9_A22','S9_A23','S9_A24')


xnames_all <- c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15',
                'A16','A17','A18','A19','A20','A21','A22','A23','A24')
xnames_odd <- c('A1','','A3','','A5','','A7','','A9','','A11','','A13','','A15',
                '','A17','','A19','','A21','','A23','')
xnames_even <- c('','A2','','A4','','A6','','A8','','A10','','A12','','A14','',
                 'A16','','A18','','A20','','A22','','A24')
xnames_brief <- c('A1','','','','A5','','','','A9','','','','A13','','','','A17','','','','A21','','','')


# ---------   show me min and max of each parm
Linf_low <- as.numeric()
Linf_high <- as.numeric()
k_low <- as.numeric()
k_high <- as.numeric()
L0_low <- as.numeric()
L0_high <- as.numeric()
CV0_low <- as.numeric()
CV0_high <- as.numeric()
CVmax_low <- as.numeric()
CVmax_high <- as.numeric()

for (i in 1: length(plot_me)) {
  Linf_low[i] <- plot_me[[i]][1,2]
  Linf_high[i] <- plot_me[[i]][1,6]
  k_low[i] <- plot_me[[i]][2,2]
  k_high[i] <- plot_me[[i]][2,6]
  L0_low[i] <- plot_me[[i]][6,2]
  L0_high[i] <- plot_me[[i]][6,6]
  CV0_low[i] <- plot_me[[i]][4,2]
  CV0_high[i] <- plot_me[[i]][4,6]
  CVmax_low[i] <- plot_me[[i]][5,2]
  CVmax_high[i] <- plot_me[[i]][5,6]
}

c(min(Linf_low), max(Linf_high))
c(min(k_low), max(k_high))
c(min(L0_low), max(L0_high))
c(min(CV0_low), max(CV0_high))
c(min(CVmax_low), max(CVmax_high))

# assign yaxis limits for each parm.
Linf_ylim <- c(90,125)
K_ylim <- c(0.1, 0.18)
L0_ylim <- c(8, 18)
CV_0_ylim <- c(0,0.3)
CV_max_ylim <- c(-0.02,0.06)


#setwd('C:/Users/Erin.Bohaboy/Documents/MUS_Sample_Design/MUS_examples')


tiff(file="S9_onaga_highF_fig_dec4.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=200)
par(mfcol=c(5,1))
par(omi=c(0.25,0.2,0.2,0.1)) #set outer margins
par(mai=c(0.15,0.15,.15,0)) #set inner margins

xaxis_plotvals <- seq(1,24,1)	#because there are 24 different approaches: A1-A24



# -------------------  A. Linf

p = 1	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=Linf_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[1]], (max(xaxis_plotvals) + 0.5), parm_true[[1]], lty=2, col="black")

# draw axes

axis(1, pos=Linf_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=Linf_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(90,125,5), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"A", cex=.75)
mtext(expression(italic('L'~infinity)), side=2, line=1.2, cex=0.9)

mtext(side=3,adj=0.5,line=0,"S9", cex=1)


# -------------------  B. k

p = 2	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=K_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[2]], (max(xaxis_plotvals) + 0.5), parm_true[[2]], lty=2, col="black")

# draw axes

axis(1, pos=K_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=K_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(0.1, 0.18, 0.02), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"B", cex=.75)
mtext(expression(italic(K)), side=2, line=1.2, cex=0.9)


# -------------------  C. L0

p = 6	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=L0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[3]], (max(xaxis_plotvals) + 0.5), parm_true[[3]], lty=2, col="black")

# draw axes

axis(1, pos=L0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=L0_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(8,18,2), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"C", cex=.75)
mtext(expression(italic("L"[0])), side=2, line=1.2, cex=0.9)


# -------------------  D. CV LINF

p = 5	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_max_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[5]], (max(xaxis_plotvals) + 0.5), parm_true[[5]], lty=2, col="black")

# draw axes

axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_max_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(-0.02,0.06,.01), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"D", cex=.75)
mtext(expression(italic("CV  L"~infinity)), side=2, line=1.2, cex=0.9)




# -------------------  E. CV L0

p = 4	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[4]], (max(xaxis_plotvals) + 0.5), parm_true[[4]], lty=2, col="black")

# draw axes

axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_0_ylim[1], at=xaxis_plotvals, labels= xnames_odd, las=1, cex.axis=0.9)
axis(1, pos=CV_0_ylim[1], at=xaxis_plotvals, labels= xnames_even, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(0,0.3,.05), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"E", cex=.75)
mtext(expression(italic("CV  L"[0])), side=2, line=1.2, cex=0.9)

dev.off()


######onaga 8 scenario plot#####
each_approach_name <- c('S10_A1','S10_A2','S10_A9','S10_A10','S10_A13','S10_A14', 'S10_A17','S10_A18')



S10_extract_all_approach <- list()

for (i in 1:length(each_approach_name)) {
  
  extract_thing <- get(each_approach_name[i])
  extract_summary <- data.frame(parm_name = character(), lower95 = numeric(), lower50 = numeric(), 
                                avg = numeric(), upper50 = numeric(), upper95 = numeric(), stringsAsFactors=FALSE)
  
  for (j in 1: ncol(extract_thing$parameter_outputs)) {
    extract_summary[j,1] <- colnames(extract_thing$parameter_outputs)[j]
    extract_summary[j,2] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.025, na.rm = TRUE))
    extract_summary[j,3] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.25, na.rm = TRUE))
    extract_summary[j,4] <- mean(extract_thing$parameter_outputs[,j],na.rm=TRUE)
    extract_summary[j,5] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.75, na.rm = TRUE))
    extract_summary[j,6] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.975, na.rm = TRUE))
  }
  
  S10_extract_all_approach[[i]] <- extract_summary
  names(S10_extract_all_approach)[[i]] <- each_approach_name[i]
  
}

plot_me <- S10_extract_all_approach

#  get the true population values for S10

S10_true <- c(S10_onaga_lowF$parameters$Linf, S10_onaga_lowF$parameters$k, S10_onaga_lowF$parameters$L0,
             S10_onaga_lowF$parameters$CV_L_0, S10_onaga_lowF$parameters$CV_L_age_max)

parm_true <- S10_true
each_approach_name <- c('S10_A1','S10_A2','S10_A9','S10_A10','S10_A13','S10_A14', 'S10_A17','S10_A18')

xnames_all <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

xnames_odd <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

xnames_even <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

xnames_brief <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

xnames_all <- c('300','75','300','75','300','75','300','75')
xnames_odd <-c('300','75','300','75','300','75','300','75')
xnames_even <- c('300','75','300','75','300','75','300','75')
xnames_brief <- c('300','75','300','75','300','75','300','75')

xnames_all <- c('A1','A2','A9','A10','A13','A14','A17','A18')
xnames_odd <-cc('A1','A2','A9','A10','A13','A14','A17','A18')
xnames_even <- c('A1','A2','A9','A10','A13','A14','A17','A18')
xnames_brief <- c('A1','A2','A9','A10','A13','A14','A17','A18')

# ---------   show me min and max of each parm
Linf_low <- as.numeric()
Linf_high <- as.numeric()
k_low <- as.numeric()
k_high <- as.numeric()
L0_low <- as.numeric()
L0_high <- as.numeric()
CV0_low <- as.numeric()
CV0_high <- as.numeric()
CVmax_low <- as.numeric()
CVmax_high <- as.numeric()

for (i in 1: length(plot_me)) {
  Linf_low[i] <- plot_me[[i]][1,2]
  Linf_high[i] <- plot_me[[i]][1,6]
  k_low[i] <- plot_me[[i]][2,2]
  k_high[i] <- plot_me[[i]][2,6]
  L0_low[i] <- plot_me[[i]][6,2]
  L0_high[i] <- plot_me[[i]][6,6]
  CV0_low[i] <- plot_me[[i]][4,2]
  CV0_high[i] <- plot_me[[i]][4,6]
  CVmax_low[i] <- plot_me[[i]][5,2]
  CVmax_high[i] <- plot_me[[i]][5,6]
}

c(min(Linf_low), max(Linf_high))
c(min(k_low), max(k_high))
c(min(L0_low), max(L0_high))
c(min(CV0_low), max(CV0_high))
c(min(CVmax_low), max(CVmax_high))

# assign yaxis limits for each parm.
Linf_ylim <- c(90,125)
K_ylim <- c(0.1, 0.18)
L0_ylim <- c(8, 18)
CV_0_ylim <- c(0,0.3)
CV_max_ylim <- c(-0.02,0.06)


tiff(file="S0_S12_onaga_8scenario_fig_Dec4.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=200)
par(mfcol=c(5,2))
par(omi=c(0.25,0.2,0.2,0.1)) #set outer margins
par(mai=c(0.15,0.15,.15,0)) #set inner margins

xaxis_plotvals <- seq(1,8,1)	#because there are 24 different approaches: A1-A24



# -------------------  A. Linf

p = 1	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=Linf_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[1]], (max(xaxis_plotvals) + 0.5), parm_true[[1]], lty=2, col="black")

# draw axes

#axis(1, pos=Linf_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=Linf_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 1,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 0.9)
axis(2, pos=0.5, at=seq(90,125,5), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"A", cex=.75)
mtext(expression(italic('L'~infinity)), side=2, line=1.2, cex=0.9)

mtext(side=3,adj=0.5,line=0,"S10", cex=1)


# -------------------  B. k

p = 2	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=K_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[2]], (max(xaxis_plotvals) + 0.5), parm_true[[2]], lty=2, col="black")

# draw axes

#axis(1, pos=K_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=K_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 1,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 0.9)
axis(2, pos=0.5, at=seq(0.1, 0.18, 0.02), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"B", cex=.75)
mtext(expression(italic(K)), side=2, line=1.2, cex=0.9)


# -------------------  C. L0

p = 6	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=L0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[3]], (max(xaxis_plotvals) + 0.5), parm_true[[3]], lty=2, col="black")

# draw axes

#axis(1, pos=L0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=L0_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 1,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 0.9)
axis(2, pos=0.5, at=seq(8,18,2), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"C", cex=.75)
mtext(expression(italic("L"[0])), side=2, line=1.2, cex=0.9)


# -------------------  D. CV LINF

p = 5	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_max_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[5]], (max(xaxis_plotvals) + 0.5), parm_true[[5]], lty=2, col="black")

# draw axes

#axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_max_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 1,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 0.9)
axis(2, pos=0.5, at=seq(-0.02,0.06,.01), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"D", cex=.75)
mtext(expression(italic("CV  L"~infinity)), side=2, line=1.2, cex=0.9)




# -------------------  E. CV L0

p = 4	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[4]], (max(xaxis_plotvals) + 0.5), parm_true[[4]], lty=2, col="black")

# draw axes

#axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_0_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 1,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 0.9)
axis(2, pos=0.5, at=seq(0,0.3,.05), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"E", cex=.75)
mtext(expression(italic("CV  L"[0])), side=2, line=1.2, cex=0.9)

dev.off()






#  -------------------------------------------------------------------------------------------------------------------
#  -------------------------------------------------------------------------------------------------------------------
#  -------------------------------------------------------------------------------------------------------------------

#  D. S12_onaga_highF	#str(S12_onaga_highF)

	S12_A1 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A2 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A3 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 
	
	S12_A4 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A5 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A6 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A7 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A8 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'FOS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A9 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A10 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A11 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A12 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A13 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A14 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = FALSE, constrained = FALSE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 
	
	S12_A15 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A16 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = FALSE, constrained = TRUE, 
							save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A17 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 
	
	S12_A18 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A19 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A20 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A21 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A22 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A23 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 300, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

	S12_A24 <- LH_sample(sim_output = S12_onaga_highF, n_boots = 1000, samp_size = 75, 
							sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
							supp_small = TRUE, constrained = TRUE, supp_small_n_per_bin = 3,
							supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 


	look_at_me <- S12_A1
	look_at_me$params_input_output
	head(look_at_me$parameter_outputs)
		hist(look_at_me$list_boot_samps[[300]]$length, breaks=seq(0,125,5))
	look_at_me$list_boot_samps[[1]]
	look_at_me$simulation_params
	look_at_me$list_boot_CVs[[1]]

write.csv(S7_extract_all_approach, "S7_results.csv")	
write.csv(S9_extract_all_approach, "S9_results.csv")
write.csv(S10_extract_all_approach, "S10_results.csv")
write.csv(S12_extract_all_approach, "S12_results.csv")	


ls()
# rm(list=ls()[27:81])



each_approach_name <- c('S12_A1','S12_A2','S12_A3','S12_A4','S12_A5','S12_A6','S12_A7','S12_A8',
                        'S12_A9','S12_A10','S12_A11','S12_A12','S12_A13','S12_A14','S12_A15','S12_A16',
                        'S12_A17','S12_A18','S12_A19','S12_A20','S12_A21','S12_A22','S12_A23','S12_A24')

S12_extract_all_approach <- list()

for (i in 1:length(each_approach_name)) {
  
  extract_thing <- get(each_approach_name[i])
  extract_summary <- data.frame(parm_name = character(), lower95 = numeric(), lower50 = numeric(), 
                                avg = numeric(), upper50 = numeric(), upper95 = numeric(), stringsAsFactors=FALSE)
  
  for (j in 1: ncol(extract_thing$parameter_outputs)) {
    extract_summary[j,1] <- colnames(extract_thing$parameter_outputs)[j]
    extract_summary[j,2] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.025, na.rm = TRUE))
    extract_summary[j,3] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.25, na.rm = TRUE))
    extract_summary[j,4] <- mean(extract_thing$parameter_outputs[,j],na.rm=TRUE)
    extract_summary[j,5] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.75, na.rm = TRUE))
    extract_summary[j,6] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.975, na.rm = TRUE))
  }
  
  S12_extract_all_approach[[i]] <- extract_summary
  names(S12_extract_all_approach)[[i]] <- each_approach_name[i]
  
}


#  get the true population values for S12

S12_true <- c(S12_onaga_highF$parameters$Linf, S12_onaga_highF$parameters$k, S12_onaga_highF$parameters$L0,
              S12_onaga_highF$parameters$CV_L_0, S12_onaga_highF$parameters$CV_L_age_max)


# rm(list=ls()[])


#  -------------------------------------------------------------------------------------------------------------------
# make a figure: 1 column, 25 sampling approaches per pane.


plot_me <- S12_extract_all_approach
parm_true <- S12_true
each_approach_name <- c('S12_A1','S12_A2','S12_A3','S12_A4','S12_A5','S12_A6','S12_A7','S12_A8',
                        'S12_A9','S12_A10','S12_A11','S12_A12','S12_A13','S12_A14','S12_A15','S12_A16',
                        'S12_A17','S12_A18','S12_A19','S12_A20','S12_A21','S12_A22','S12_A23','S12_A24')


xnames_all <- c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15',
                'A16','A17','A18','A19','A20','A21','A22','A23','A24')
xnames_odd <- c('A1','','A3','','A5','','A7','','A9','','A11','','A13','','A15',
                '','A17','','A19','','A21','','A23','')
xnames_even <- c('','A2','','A4','','A6','','A8','','A10','','A12','','A14','',
                 'A16','','A18','','A20','','A22','','A24')
xnames_brief <- c('A1','','','','A5','','','','A9','','','','A13','','','','A17','','','','A21','','','')


# ---------   show me min and max of each parm
Linf_low <- as.numeric()
Linf_high <- as.numeric()
k_low <- as.numeric()
k_high <- as.numeric()
L0_low <- as.numeric()
L0_high <- as.numeric()
CV0_low <- as.numeric()
CV0_high <- as.numeric()
CVmax_low <- as.numeric()
CVmax_high <- as.numeric()

for (i in 1: length(plot_me)) {
  Linf_low[i] <- plot_me[[i]][1,2]
  Linf_high[i] <- plot_me[[i]][1,6]
  k_low[i] <- plot_me[[i]][2,2]
  k_high[i] <- plot_me[[i]][2,6]
  L0_low[i] <- plot_me[[i]][6,2]
  L0_high[i] <- plot_me[[i]][6,6]
  CV0_low[i] <- plot_me[[i]][4,2]
  CV0_high[i] <- plot_me[[i]][4,6]
  CVmax_low[i] <- plot_me[[i]][5,2]
  CVmax_high[i] <- plot_me[[i]][5,6]
}

c(min(Linf_low), max(Linf_high))
c(min(k_low), max(k_high))
c(min(L0_low), max(L0_high))
c(min(CV0_low), max(CV0_high))
c(min(CVmax_low), max(CVmax_high))

# assign yaxis limits for each parm.
Linf_ylim <- c(90,125)
K_ylim <- c(0.1, 0.18)
L0_ylim <- c(8, 18)
CV_0_ylim <- c(0,0.3)
CV_max_ylim <- c(-0.02,0.06)




tiff(file="S12_onaga_highF_fig_dec4.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=200)
par(mfcol=c(5,1))
par(omi=c(0.25,0.2,0.2,0.1)) #set outer margins
par(mai=c(0.15,0.15,.15,0)) #set inner margins

xaxis_plotvals <- seq(1,24,1)	#because there are 24 different approaches: A1-A24



# -------------------  A. Linf

p = 1	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=Linf_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[1]], (max(xaxis_plotvals) + 0.5), parm_true[[1]], lty=2, col="black")

# draw axes

#axis(1, pos=Linf_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=Linf_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
axis(1, pos=Linf_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(90,125,5), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"A", cex=.75)
mtext(expression(italic('L'~infinity)), side=2, line=1.2, cex=0.9)

mtext(side=3,adj=0.5,line=0,"S12", cex=1)


# -------------------  B. k

p = 2	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=K_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[2]], (max(xaxis_plotvals) + 0.5), parm_true[[2]], lty=2, col="black")

# draw axes

#axis(1, pos=K_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=K_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)

axis(1, pos=K_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)

axis(2, pos=0.5, at=seq(0.1, 0.18, 0.02), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"B", cex=.75)
mtext(expression(italic(K)), side=2, line=1.2, cex=0.9)


# -------------------  C. L0

p = 6	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=L0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[3]], (max(xaxis_plotvals) + 0.5), parm_true[[3]], lty=2, col="black")

# draw axes

axis(1, pos=L0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=L0_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
axis(2, pos=0.5, at=seq(8,18,2), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"C", cex=.75)
mtext(expression(italic("L"[0])), side=2, line=1.2, cex=0.9)


# -------------------  D. CV LINF

p = 5	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_max_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[5]], (max(xaxis_plotvals) + 0.5), parm_true[[5]], lty=2, col="black")

# draw axes

axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_max_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(-0.02,0.06,.01), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"D", cex=.75)
mtext(expression(italic("CV  L"~infinity)), side=2, line=1.2, cex=0.9)




# -------------------  E. CV L0

p = 4	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[4]], (max(xaxis_plotvals) + 0.5), parm_true[[4]], lty=2, col="black")

# draw axes

#axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_0_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
axis(1, pos=CV_0_ylim[1], at=xaxis_plotvals, labels= xnames_brief, las=1, cex.axis=0.9)
axis(2, pos=0.5, at=seq(0,0.3,.05), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"E", cex=.75)
mtext(expression(italic("CV  L"[0])), side=2, line=1.2, cex=0.9)

dev.off()


######onaga 8 scenario plot#####
each_approach_name <- c('S10_A1','S10_A2','S10_A9','S10_A10','S10_A13','S10_A14', 'S10_A17','S10_A18')



S10_extract_all_approach <- list()

for (i in 1:length(each_approach_name)) {
  
  extract_thing <- get(each_approach_name[i])
  extract_summary <- data.frame(parm_name = character(), lower95 = numeric(), lower50 = numeric(), 
                                avg = numeric(), upper50 = numeric(), upper95 = numeric(), stringsAsFactors=FALSE)
  
  for (j in 1: ncol(extract_thing$parameter_outputs)) {
    extract_summary[j,1] <- colnames(extract_thing$parameter_outputs)[j]
    extract_summary[j,2] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.025, na.rm = TRUE))
    extract_summary[j,3] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.25, na.rm = TRUE))
    extract_summary[j,4] <- mean(extract_thing$parameter_outputs[,j],na.rm=TRUE)
    extract_summary[j,5] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.75, na.rm = TRUE))
    extract_summary[j,6] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.975, na.rm = TRUE))
  }
  
  S10_extract_all_approach[[i]] <- extract_summary
  names(S10_extract_all_approach)[[i]] <- each_approach_name[i]
  
}

plot_me <- S10_extract_all_approach

#  get the true population values for S10


parm_true <- S10_true
each_approach_name <- c('S10_A1','S10_A2','S10_A9','S10_A10','S10_A13','S10_A14', 'S10_A17','S10_A18')

xnames_all <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

xnames_odd <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

xnames_even <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

xnames_brief <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

xnames_all <- c('300','75','300','75','300','75','300','75')
xnames_odd <-c('300','75','300','75','300','75','300','75')
xnames_even <- c('300','75','300','75','300','75','300','75')
xnames_brief <- c('300','75','300','75','300','75','300','75')

xnames_all <- c('A1','A2','A9','A10','A13','A14','A17','A18')
xnames_odd <-c('A1','A2','A9','A10','A13','A14','A17','A18')
xnames_even <- c('A1','A2','A9','A10','A13','A14','A17','A18')
xnames_brief <- c('A1','A2','A9','A10','A13','A14','A17','A18')

# ---------   show me min and max of each parm
Linf_low <- as.numeric()
Linf_high <- as.numeric()
k_low <- as.numeric()
k_high <- as.numeric()
L0_low <- as.numeric()
L0_high <- as.numeric()
CV0_low <- as.numeric()
CV0_high <- as.numeric()
CVmax_low <- as.numeric()
CVmax_high <- as.numeric()

for (i in 1: length(plot_me)) {
  Linf_low[i] <- plot_me[[i]][1,2]
  Linf_high[i] <- plot_me[[i]][1,6]
  k_low[i] <- plot_me[[i]][2,2]
  k_high[i] <- plot_me[[i]][2,6]
  L0_low[i] <- plot_me[[i]][6,2]
  L0_high[i] <- plot_me[[i]][6,6]
  CV0_low[i] <- plot_me[[i]][4,2]
  CV0_high[i] <- plot_me[[i]][4,6]
  CVmax_low[i] <- plot_me[[i]][5,2]
  CVmax_high[i] <- plot_me[[i]][5,6]
}

c(min(Linf_low), max(Linf_high))
c(min(k_low), max(k_high))
c(min(L0_low), max(L0_high))
c(min(CV0_low), max(CV0_high))
c(min(CVmax_low), max(CVmax_high))

# assign yaxis limits for each parm.
Linf_ylim <- c(90,125)
K_ylim <- c(0.1, 0.18)
L0_ylim <- c(8, 18)
CV_0_ylim <- c(0,0.3)
CV_max_ylim <- c(-0.02,0.06)


tiff(file="S0_S12_onaga_8scenario_fig_Dec4_blue_large_cex.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=200)
par(mfcol=c(5,2))
par(omi=c(0.25,0.2,0.2,0.1)) #set outer margins
par(mai=c(0.15,0.15,.15,0)) #set inner margins

xaxis_plotvals <- seq(1,8,1)	#because there are 24 different approaches: A1-A24



# -------------------  A. Linf

p = 1	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=Linf_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[1]], (max(xaxis_plotvals) + 0.5), parm_true[[1]], lty=2, col="black")

# draw axes

#axis(1, pos=Linf_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=Linf_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 3,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(90,125,5), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"A", cex=.75)
mtext(expression(italic('L'~infinity)), side=2, line=1.2, cex=0.9)

mtext(side=3,adj=0.5,line=0,"S10", cex=1)


# -------------------  B. k

p = 2	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=K_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[2]], (max(xaxis_plotvals) + 0.5), parm_true[[2]], lty=2, col="black")

# draw axes

#axis(1, pos=K_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=K_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - .01,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(0.1, 0.18, 0.02), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"B", cex=.75)
mtext(expression(italic(K)), side=2, line=1.2, cex=0.9)


# -------------------  C. L0

p = 6	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=L0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[3]], (max(xaxis_plotvals) + 0.5), parm_true[[3]], lty=2, col="black")

# draw axes

#axis(1, pos=L0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=L0_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 1,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(8,18,2), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"C", cex=.75)
mtext(expression(italic("L"[0])), side=2, line=1.2, cex=0.9)


# -------------------  D. CV LINF

p = 5	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_max_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[5]], (max(xaxis_plotvals) + 0.5), parm_true[[5]], lty=2, col="black")

# draw axes

#axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_max_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.01,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(-0.02,0.06,.01), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"D", cex=.75)
mtext(expression(italic("CV  L"~infinity)), side=2, line=1.2, cex=0.9)




# -------------------  E. CV L0

p = 4	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[4]], (max(xaxis_plotvals) + 0.5), parm_true[[4]], lty=2, col="black")

# draw axes

#axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_0_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.025,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(0,0.3,.05), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"E", cex=.75)
mtext(expression(italic("CV  L"[0])), side=2, line=1.2, cex=0.9)

###S12 8 scenarios
each_approach_name <- c('S12_A1','S12_A2','S12_A9','S12_A10','S12_A13','S12_A14', 'S12_A17','S12_A18')



S12_extract_all_approach <- list()

for (i in 1:length(each_approach_name)) {
  
  extract_thing <- get(each_approach_name[i])
  extract_summary <- data.frame(parm_name = character(), lower95 = numeric(), lower50 = numeric(), 
                                avg = numeric(), upper50 = numeric(), upper95 = numeric(), stringsAsFactors=FALSE)
  
  for (j in 1: ncol(extract_thing$parameter_outputs)) {
    extract_summary[j,1] <- colnames(extract_thing$parameter_outputs)[j]
    extract_summary[j,2] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.025, na.rm = TRUE))
    extract_summary[j,3] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.25, na.rm = TRUE))
    extract_summary[j,4] <- mean(extract_thing$parameter_outputs[,j],na.rm=TRUE)
    extract_summary[j,5] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.75, na.rm = TRUE))
    extract_summary[j,6] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.975, na.rm = TRUE))
  }
  
  S12_extract_all_approach[[i]] <- extract_summary
  names(S12_extract_all_approach)[[i]] <- each_approach_name[i]
  
}

plot_me <- S12_extract_all_approach

#  get the true population values for S12

S12_true <- c(S12_onaga_highF$parameters$Linf, S12_onaga_highF$parameters$k, S12_onaga_highF$parameters$L0,
              S12_onaga_highF$parameters$CV_L_0, S12_onaga_highF$parameters$CV_L_age_max)


parm_true <- S12_true
each_approach_name <- c('S12_A1','S12_A2','S12_A9','S12_A10','S12_A13','S12_A14', 'S12_A17','S12_A18')

xnames_all <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

xnames_odd <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

xnames_even <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

xnames_brief <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

xnames_all <- c('300','75','300','75','300','75','300','75')
xnames_odd <-c('300','75','300','75','300','75','300','75')
xnames_even <- c('300','75','300','75','300','75','300','75')
xnames_brief <- c('300','75','300','75','300','75','300','75')

xnames_all <- c('A1','A2','A9','A10','A13','A14','A17','A18')
xnames_odd <-c('A1','A2','A9','A10','A13','A14','A17','A18')
xnames_even <- c('A1','A2','A9','A10','A13','A14','A17','A18')
xnames_brief <- c('A1','A2','A9','A10','A13','A14','A17','A18')

# ---------   show me min and max of each parm
Linf_low <- as.numeric()
Linf_high <- as.numeric()
k_low <- as.numeric()
k_high <- as.numeric()
L0_low <- as.numeric()
L0_high <- as.numeric()
CV0_low <- as.numeric()
CV0_high <- as.numeric()
CVmax_low <- as.numeric()
CVmax_high <- as.numeric()

for (i in 1: length(plot_me)) {
  Linf_low[i] <- plot_me[[i]][1,2]
  Linf_high[i] <- plot_me[[i]][1,6]
  k_low[i] <- plot_me[[i]][2,2]
  k_high[i] <- plot_me[[i]][2,6]
  L0_low[i] <- plot_me[[i]][6,2]
  L0_high[i] <- plot_me[[i]][6,6]
  CV0_low[i] <- plot_me[[i]][4,2]
  CV0_high[i] <- plot_me[[i]][4,6]
  CVmax_low[i] <- plot_me[[i]][5,2]
  CVmax_high[i] <- plot_me[[i]][5,6]
}

c(min(Linf_low), max(Linf_high))
c(min(k_low), max(k_high))
c(min(L0_low), max(L0_high))
c(min(CV0_low), max(CV0_high))
c(min(CVmax_low), max(CVmax_high))

# assign yaxis limits for each parm.
Linf_ylim <- c(90,125)
K_ylim <- c(0.1, 0.18)
L0_ylim <- c(8, 18)
CV_0_ylim <- c(0,0.3)
CV_max_ylim <- c(-0.02,0.06)




# -------------------  A. Linf

p = 1	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=Linf_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[1]], (max(xaxis_plotvals) + 0.5), parm_true[[1]], lty=2, col="black")

# draw axes

#axis(1, pos=Linf_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=Linf_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 3,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(90,125,5), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"F", cex=.75)
mtext(expression(italic('L'~infinity)), side=2, line=1.2, cex=0.9)

mtext(side=3,adj=0.5,line=0,"S12", cex=1)


# -------------------  B. k

p = 2	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=K_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[2]], (max(xaxis_plotvals) + 0.5), parm_true[[2]], lty=2, col="black")

# draw axes

#axis(1, pos=K_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=K_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.01,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(0.1, 0.18, 0.02), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"G", cex=.75)
mtext(expression(italic(K)), side=2, line=1.2, cex=0.9)


# -------------------  C. L0

p = 6	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=L0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[3]], (max(xaxis_plotvals) + 0.5), parm_true[[3]], lty=2, col="black")

# draw axes

#axis(1, pos=L0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=L0_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 1,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(8,18,2), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"H", cex=.75)
mtext(expression(italic("L"[0])), side=2, line=1.2, cex=0.9)


# -------------------  D. CV LINF

p = 5	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_max_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[5]], (max(xaxis_plotvals) + 0.5), parm_true[[5]], lty=2, col="black")

# draw axes

#axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_max_ylim[1], at=c(0.5,2.5,4.5,6.5,8.5), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - .01,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(-0.02,0.06,.01), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"I", cex=.75)
mtext(expression(italic("CV  L"~infinity)), side=2, line=1.2, cex=0.9)




# -------------------  E. CV L0

p = 4	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

polygon(c(2-0.3,2+0.3,2+0.3,2-0.3),
        c(plot_me[[2]][p,3],plot_me[[2]][p,3],
          plot_me[[2]][p,5],plot_me[[2]][p,5]),col = "lightblue")

polygon(c(4-0.3,4+0.3,4+0.3,4-0.3),
        c(plot_me[[4]][p,3],plot_me[[4]][p,3],
          plot_me[[4]][p,5],plot_me[[4]][p,5]),col = "lightblue")

polygon(c(6-0.3,6+0.3,6+0.3,6-0.3),
        c(plot_me[[6]][p,3],plot_me[[6]][p,3],
          plot_me[[6]][p,5],plot_me[[6]][p,5]),col = "lightblue")

polygon(c(8-0.3,8+0.3,8+0.3,8-0.3),
        c(plot_me[[8]][p,3],plot_me[[8]][p,3],
          plot_me[[8]][p,5],plot_me[[8]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[4]], (max(xaxis_plotvals) + 0.5), parm_true[[4]], lty=2, col="black")

# draw axes

axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels= FALSE)
#axis(1, pos=Linf_ylim[1], at=c(1.5,3.5, 5.5,7.5), labels= c("FOS", "POS","POS Large", "POS Small"), las=1, cex.axis=0.9)
text(x = c(1.5,3.5, 5.5,7.5),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.025,
     ## Use names from the data list.
     labels= c("FOS", "POS","POS Large", "POS Small"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1.2)
axis(2, pos=0.5, at=seq(0,0.3,.05), cex=1.1)

mtext(side=3,adj=0.05,line=-1.5,"J", cex=.75)
mtext(expression(italic("CV  L"[0])), side=2, line=1.2, cex=0.9)
dev.off()

#  -------------------------------------------------------------------------------------------------------------------
#  -------------------------------------------------------------------------------------------------------------------
#  -------------------------------------------------------------------------------------------------------------------

#

#########supplemental sampling figure ######
S4_POS <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                      sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                      supp_small = FALSE, constrained = FALSE, 
                      save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A13_3 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A13_5 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 5,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 

S4_A13_10 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                    sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 10,
                    supp_small = FALSE, constrained = FALSE, 
                    save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 



S4_A18_3 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = FALSE, supp_small_n_per_bin = 3,
                     supp_small = TRUE, constrained = FALSE, 
                     save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2) 


S4_A18_5 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = FALSE, supp_small_n_per_bin = 5,
                     supp_small = TRUE, constrained = FALSE, 
                     save_bootstraps = TRUE, Amax = 32, age_max = 25, Lbin_width = 2)

S4_A18_10 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = FALSE, supp_small_n_per_bin = 10,
                     supp_small = TRUE, constrained = FALSE, 
                     save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2)


S4_A21_3 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                        sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                        supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                        save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2)

S4_A21_5 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                       sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 5,
                       supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 5,
                       save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2)
S4_A21_10 <- LH_sample(sim_output = S4_Auric_lowF, n_boots = 1000, samp_size = 300, 
                       sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 10,
                       supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 10,
                       save_bootstraps = TRUE, Amax = 32, age_max = 15, Lbin_width = 2)


#onaga
S10_POS <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
                       sample_type = 'POS', supp_large = FALSE,
                       supp_small = FALSE, constrained = FALSE, 
                       save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5)

S10_A13_3 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                     supp_small = FALSE, constrained = FALSE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5)

S10_A13_5 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 5,
                     supp_small = FALSE, constrained = FALSE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5)

S10_A13_10 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 10,
                     supp_small = FALSE, constrained = FALSE, 
                     save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5)


S10_A18_3 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
                     sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 3,
                     supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                     supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S10_A18_5 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
                       sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 5,
                       supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 5,
                       supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S10_A18_10 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
                       sample_type = 'POS', supp_large = FALSE, supp_large_n_per_bin = 10,
                       supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 10,
                       supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S10_A21_3 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
                        sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 3,
                        supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 3,
                        supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 

S10_A21_5 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
                       sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 5,
                       supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 5,
                       supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 


S10_A21_10 <- LH_sample(sim_output = S10_onaga_lowF, n_boots = 1000, samp_size = 300, 
                       sample_type = 'POS', supp_large = TRUE, supp_large_n_per_bin = 10,
                       supp_small = TRUE, constrained = FALSE, supp_small_n_per_bin = 10,
                       supp_min_length = 10, save_bootstraps = TRUE, Amax = 55, age_max = 20, Lbin_width = 5) 


#plot the auricilla and onaga supplemental sampling
each_approach_name <- c('S4_POS','S4_A13_3','S4_A13_5','S4_A13_10','S4_A18_3','S4_A18_5','S4_A18_10', 'S4_A21_3','S4_A21_5', 'S4_A21_10')



S4_sup_extract_all_approach <- list()

for (i in 1:length(each_approach_name)) {
  
  extract_thing <- get(each_approach_name[i])
  extract_summary <- data.frame(parm_name = character(), lower95 = numeric(), lower50 = numeric(), 
                                avg = numeric(), upper50 = numeric(), upper95 = numeric(), stringsAsFactors=FALSE)
  
  for (j in 1: ncol(extract_thing$parameter_outputs)) {
    extract_summary[j,1] <- colnames(extract_thing$parameter_outputs)[j]
    extract_summary[j,2] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.025, na.rm = TRUE))
    extract_summary[j,3] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.25, na.rm = TRUE))
    extract_summary[j,4] <- mean(extract_thing$parameter_outputs[,j],na.rm=TRUE)
    extract_summary[j,5] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.75, na.rm = TRUE))
    extract_summary[j,6] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.975, na.rm = TRUE))
  }
  
  S4_sup_extract_all_approach[[i]] <- extract_summary
  names(S4_sup_extract_all_approach)[[i]] <- each_approach_name[i]
  
}

plot_me <- S4_sup_extract_all_approach

#  get the true population values for S10


parm_true <- S4_true


xnames_all <- c('POS', '3','5','10','3','5','10','3','5', '10')

#xnames_odd <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

#xnames_even <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

#xnames_brief <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')


# ---------   show me min and max of each parm
Linf_low <- as.numeric()
Linf_high <- as.numeric()
k_low <- as.numeric()
k_high <- as.numeric()
L0_low <- as.numeric()
L0_high <- as.numeric()
CV0_low <- as.numeric()
CV0_high <- as.numeric()
CVmax_low <- as.numeric()
CVmax_high <- as.numeric()

for (i in 1: length(plot_me)) {
  Linf_low[i] <- plot_me[[i]][1,2]
  Linf_high[i] <- plot_me[[i]][1,6]
  k_low[i] <- plot_me[[i]][2,2]
  k_high[i] <- plot_me[[i]][2,6]
  L0_low[i] <- plot_me[[i]][6,2]
  L0_high[i] <- plot_me[[i]][6,6]
  CV0_low[i] <- plot_me[[i]][4,2]
  CV0_high[i] <- plot_me[[i]][4,6]
  CVmax_low[i] <- plot_me[[i]][5,2]
  CVmax_high[i] <- plot_me[[i]][5,6]
}

c(min(Linf_low), max(Linf_high))
c(min(k_low), max(k_high))
c(min(L0_low), max(L0_high))
c(min(CV0_low), max(CV0_high))
c(min(CVmax_low), max(CVmax_high))

# assign yaxis limits for each parm.

Linf_ylim <- c(30,40)
K_ylim <- c(0.0, 1.2)
L0_ylim <- c(0, 30)
CV_0_ylim <- c(-.2,1.0)
CV_max_ylim <- c(0,0.35)

tiff(file="S4_S10_supsDec7.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=200)

par(mfcol=c(5,2))
par(omi=c(0.25,0.3,0.2,0.1)) #set outer margins
par(mai=c(0.15,0.15,.15,0)) #set inner margins

xaxis_plotvals <- seq(1,10,1)	#because there are 9 different sup options compared to the regular POS



# -------------------  A. Linf

p = 1	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=Linf_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}

polygon(c(1-0.3,1+0.3,1+0.3,1-0.3),
        c(plot_me[[1]][p,3],plot_me[[1]][p,3],
          plot_me[[1]][p,5],plot_me[[1]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[1]], (max(xaxis_plotvals) + 0.5), parm_true[[1]], lty=2, col="black")

#add POS with out supplemental samples
#segments(0.5, 32.1216699, (max(xaxis_plotvals) + 0.5), 32.1216699, lty=2, col="blue")

# draw axes

axis(1, pos=Linf_ylim[1], at=c(0.5,xaxis_plotvals), labels=c("","POS","3",  "5",  "10", "3",  "5" , "10", "3" , "5" , "10"))
text(x = c(3,6, 9),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 2.5,
     ## Use names from the data list.
     labels= c("Large", "Small","Both"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1)

axis(2, pos=0.5, at=seq(30,40,2), cex=1.1)

mtext(side=3,adj=0.05,line=-1.2,"A", cex=.75)
mtext(expression(italic('L'~infinity)), side=2, line=1.2, cex=0.9)

mtext(side=3,adj=0.5,line=0,"S4", cex=1)


# -------------------  B. k

p = 2	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=K_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(1-0.3,1+0.3,1+0.3,1-0.3),
        c(plot_me[[1]][p,3],plot_me[[1]][p,3],
          plot_me[[1]][p,5],plot_me[[1]][p,5]),col = "lightblue")

#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[2]], (max(xaxis_plotvals) + 0.5), parm_true[[2]], lty=2, col="black")

#add POS with out supplemental samples
#segments(0.5, 0.5950209, (max(xaxis_plotvals) + 0.5), 0.5950209, lty=2, col="blue")

# draw axes

#axis(1, pos=K_ylim[1], at=c(0.5,xaxis_plotvals), labels= xnames_all)
axis(1, pos=K_ylim[1], at=c(0.5,xaxis_plotvals), labels=c("","POS","3",  "5",  "10", "3",  "5" , "10", "3" , "5" , "10"))
text(x = c(3,6, 9),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.25,
     ## Use names from the data list.
     labels= c("Large", "Small","Both"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1)

axis(2, pos=0.5, at=seq(0, 1.2, 0.2), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"B", cex=.75)
mtext(expression(italic(K)), side=2, line=1.2, cex=0.9)


# -------------------  C. L0

p = 6	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=L0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(1-0.3,1+0.3,1+0.3,1-0.3),
        c(plot_me[[1]][p,3],plot_me[[1]][p,3],
          plot_me[[1]][p,5],plot_me[[1]][p,5]),col = "lightblue")


#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[3]], (max(xaxis_plotvals) + 0.5), parm_true[[3]], lty=2, col="black")
#add POS with out supplemental samples
#segments(0.5, 16.44, (max(xaxis_plotvals) + 0.5), 16.44, lty=2, col="blue")

# draw axes


#axis(1,pos=L0_ylim[1], at=c(0.5,xaxis_plotvals), labels= xnames_all)
axis(1, pos=L0_ylim[1], at=c(0.5,xaxis_plotvals), labels=c("","POS","3",  "5",  "10", "3",  "5" , "10", "3" , "5" , "10"))
text(x = c(3,6, 9),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 5.75,
     ## Use names from the data list.
     labels= c("Large", "Small","Both"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1)


axis(2, pos=0.5, at=seq(0,30,5), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"C", cex=.75)
mtext(expression(italic("L"[0])), side=2, line=1.2, cex=0.9)




# -------------------  D. CV LINF

p = 5	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_max_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(1-0.3,1+0.3,1+0.3,1-0.3),
        c(plot_me[[1]][p,3],plot_me[[1]][p,3],
          plot_me[[1]][p,5],plot_me[[1]][p,5]),col = "lightblue")


#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[5]], (max(xaxis_plotvals) + 0.5), parm_true[[5]], lty=2, col="black")
#add POS with out supplemental samples
#segments(0.5, 0.055, (max(xaxis_plotvals) + 0.5), 0.055, lty=2, col="blue")

# draw axes


#axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels= xnames_all)
axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels=c("","POS","3",  "5",  "10", "3",  "5" , "10", "3" , "5" , "10"))
text(x = c(3,6, 9),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.08,
     ## Use names from the data list.
     labels= c("Large", "Small","Both"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1)


axis(2, pos=0.5, at=seq(-.05,0.35,.05), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"D", cex=.75)
mtext(expression(italic("CV  L"~infinity)), side=2, line=1.2, cex=0.9)




# -------------------  E. CV L0

p = 4	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(1-0.3,1+0.3,1+0.3,1-0.3),
        c(plot_me[[1]][p,3],plot_me[[1]][p,3],
          plot_me[[1]][p,5],plot_me[[1]][p,5]),col = "lightblue")


#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[4]], (max(xaxis_plotvals) + 0.5), parm_true[[4]], lty=2, col="black")
#add POS with out supplemental samples
#segments(0.5, 0.12, (max(xaxis_plotvals) + 0.5), 0.12, lty=2, col="blue")

# draw axes

#axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels= xnames_all)
axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels=c("","POS","3",  "5",  "10", "3",  "5" , "10", "3" , "5" , "10"))
text(x = c(3,6, 9),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - .25,
     ## Use names from the data list.
     labels= c("Large", "Small","Both"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1)


axis(2, pos=0.5, at=seq(-0.2,1,.2), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"E", cex=.75)
mtext(expression(italic("CV  L"[0])), side=2, line=1.2, cex=0.9)


#onaga
each_approach_name <- c('S10_POS','S10_A13_3','S10_A13_5','S10_A13_10','S10_A18_3','S10_A18_5','S10_A18_10', 'S10_A21_3','S10_A21_5', "S10_A21_10")



S10_sup_extract_all_approach <- list()

for (i in 1:length(each_approach_name)) {
  
  extract_thing <- get(each_approach_name[i])
  extract_summary <- data.frame(parm_name = character(), lower95 = numeric(), lower50 = numeric(), 
                                avg = numeric(), upper50 = numeric(), upper95 = numeric(), stringsAsFactors=FALSE)
  
  for (j in 1: ncol(extract_thing$parameter_outputs)) {
    extract_summary[j,1] <- colnames(extract_thing$parameter_outputs)[j]
    extract_summary[j,2] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.025, na.rm = TRUE))
    extract_summary[j,3] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.25, na.rm = TRUE))
    extract_summary[j,4] <- mean(extract_thing$parameter_outputs[,j],na.rm=TRUE)
    extract_summary[j,5] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.75, na.rm = TRUE))
    extract_summary[j,6] <- as.numeric(quantile(extract_thing$parameter_outputs[,j],probs = 0.975, na.rm = TRUE))
  }
  
  S10_sup_extract_all_approach[[i]] <- extract_summary
  names(S10_sup_extract_all_approach)[[i]] <- each_approach_name[i]
  
}

plot_me <- S10_sup_extract_all_approach

#  get the true population values for S10

S10_true <- c(S10_onaga_lowF$parameters$Linf, S10_onaga_lowF$parameters$k, S10_onaga_lowF$parameters$L0,
             S10_onaga_lowF$parameters$CV_L_0, S10_onaga_lowF$parameters$CV_L_age_max)





parm_true <- S10_true

xnames_all <- c('POS','3','5','10','3','5','10','3','5', '10')

#xnames_odd <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

#xnames_even <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')

#xnames_brief <- c('FOS 300','FOS 75','POS 300','POS 75','POS+large 300','POS+large 75','POS+small 300','POS+small 75')


# ---------   show me min and max of each parm
Linf_low <- as.numeric()
Linf_high <- as.numeric()
k_low <- as.numeric()
k_high <- as.numeric()
L0_low <- as.numeric()
L0_high <- as.numeric()
CV0_low <- as.numeric()
CV0_high <- as.numeric()
CVmax_low <- as.numeric()
CVmax_high <- as.numeric()

for (i in 1: length(plot_me)) {
  Linf_low[i] <- plot_me[[i]][1,2]
  Linf_high[i] <- plot_me[[i]][1,6]
  k_low[i] <- plot_me[[i]][2,2]
  k_high[i] <- plot_me[[i]][2,6]
  L0_low[i] <- plot_me[[i]][6,2]
  L0_high[i] <- plot_me[[i]][6,6]
  CV0_low[i] <- plot_me[[i]][4,2]
  CV0_high[i] <- plot_me[[i]][4,6]
  CVmax_low[i] <- plot_me[[i]][5,2]
  CVmax_high[i] <- plot_me[[i]][5,6]
}

c(min(Linf_low), max(Linf_high))
c(min(k_low), max(k_high))
c(min(L0_low), max(L0_high))
c(min(CV0_low), max(CV0_high))
c(min(CVmax_low), max(CVmax_high))

# assign yaxis limits for each parm.
Linf_ylim <- c(90,110)
K_ylim <- c(0.1, 0.18)
L0_ylim <- c(8, 18)
CV_0_ylim <- c(0,0.3)
CV_max_ylim <- c(0,0.06)


# -------------------  A. Linf

p = 1	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=Linf_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(1-0.3,1+0.3,1+0.3,1-0.3),
        c(plot_me[[1]][p,3],plot_me[[1]][p,3],
          plot_me[[1]][p,5],plot_me[[1]][p,5]),col = "lightblue")



#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[1]], (max(xaxis_plotvals) + 0.5), parm_true[[1]], lty=2, col="black")

# draw axes

axis(1, pos=Linf_ylim[1], at=c(0.5,xaxis_plotvals), labels=c("","POS","3",  "5",  "10", "3",  "5" , "10", "3" , "5" , "10"))
text(x = c(3,6, 9),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 6,
     ## Use names from the data list.
     labels= c("Large", "Small","Both"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1)

axis(2, pos=0.5, at=seq(90,110,5), cex=1.1)

mtext(side=3,adj=0.05,line=-1.2,"A", cex=.75)
mtext(expression(italic('L'~infinity)), side=2, line=1.2, cex=0.9)

mtext(side=3,adj=0.5,line=0,"S10", cex=1)


# -------------------  B. k

p = 2	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=K_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(1-0.3,1+0.3,1+0.3,1-0.3),
        c(plot_me[[1]][p,3],plot_me[[1]][p,3],
          plot_me[[1]][p,5],plot_me[[1]][p,5]),col = "lightblue")


#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[2]], (max(xaxis_plotvals) + 0.5), parm_true[[2]], lty=2, col="black")

# draw axes

#axis(1, pos=K_ylim[1], at=c(0.5,xaxis_plotvals), labels= xnames_all)
axis(1, pos=K_ylim[1], at=c(0.5,xaxis_plotvals), labels=c("","POS","3",  "5",  "10", "3",  "5" , "10", "3" , "5" , "10"))
text(x = c(3,6, 9),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.0175,
     ## Use names from the data list.
     labels= c("Large", "Small","Both"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1)

axis(2, pos=0.5, at=seq(0.1, .18, 0.01), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"B", cex=.75)
mtext(expression(italic(K)), side=2, line=1.2, cex=0.9)


# -------------------  C. L0

p = 6	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=L0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(1-0.3,1+0.3,1+0.3,1-0.3),
        c(plot_me[[1]][p,3],plot_me[[1]][p,3],
          plot_me[[1]][p,5],plot_me[[1]][p,5]),col = "lightblue")


#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[3]], (max(xaxis_plotvals) + 0.5), parm_true[[3]], lty=2, col="black")

# draw axes


#axis(1,pos=L0_ylim[1], at=c(0.5,xaxis_plotvals), labels= xnames_all)
axis(1, pos=L0_ylim[1], at=c(0.5,xaxis_plotvals), labels=c("","POS","3",  "5",  "10", "3",  "5" , "10", "3" , "5" , "10"))
text(x = c(3,6, 9),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 2,
     ## Use names from the data list.
     labels= c("Large", "Small","Both"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1)


axis(2, pos=0.5, at=seq(8,18,2), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"C", cex=.75)
mtext(expression(italic("L"[0])), side=2, line=1.2, cex=0.9)




# -------------------  D. CV LINF

p = 5	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_max_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(1-0.3,1+0.3,1+0.3,1-0.3),
        c(plot_me[[1]][p,3],plot_me[[1]][p,3],
          plot_me[[1]][p,5],plot_me[[1]][p,5]),col = "lightblue")


#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[5]], (max(xaxis_plotvals) + 0.5), parm_true[[5]], lty=2, col="black")

# draw axes


#axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels= xnames_all)
axis(1, pos=CV_max_ylim[1], at=c(0.5,xaxis_plotvals), labels=c("","POS","3",  "5",  "10", "3",  "5" , "10", "3" , "5" , "10"))
text(x = c(3,6, 9),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.0125,
     ## Use names from the data list.
     labels= c("Large", "Small","Both"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1)


axis(2, pos=0.5, at=seq(0,0.06,.01), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"D", cex=.75)
mtext(expression(italic("CV  L"~infinity)), side=2, line=1.2, cex=0.9)




# -------------------  E. CV L0

p = 4	# define line number of parameter we are plotting

#  establish plot space (pch="" so nothing appears)
plot(0,0,ylim=CV_0_ylim, xlim = c(0.5,length(xaxis_plotvals)+0.5),pch="", axes=FALSE)

#  add 95% CI  
for (i in 1:length(each_approach_name)) {
  segments((i), plot_me[[i]][p,2], (i), plot_me[[i]][p,6], lwd=1)
}

#  add 50% box
for (i in 1:length(each_approach_name)) {
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),
          c(plot_me[[i]][p,3],plot_me[[i]][p,3],
            plot_me[[i]][p,5],plot_me[[i]][p,5]),col = "white")
}
polygon(c(1-0.3,1+0.3,1+0.3,1-0.3),
        c(plot_me[[1]][p,3],plot_me[[1]][p,3],
          plot_me[[1]][p,5],plot_me[[1]][p,5]),col = "lightblue")


#  add mean  
for (i in 1:length(each_approach_name)) {
  segments((i-0.3), plot_me[[i]][p,4], (i+0.3), plot_me[[i]][p,4], lwd=1)
}

#  add true
segments(0.5, parm_true[[4]], (max(xaxis_plotvals) + 0.5), parm_true[[4]], lty=2, col="black")

# draw axes

#axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels= xnames_all)
axis(1, pos=CV_0_ylim[1], at=c(0.5,xaxis_plotvals), labels=c("","POS","3",  "5",  "10", "3",  "5" , "10", "3" , "5" , "10"))
text(x = c(3,6, 9),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - .07,
     ## Use names from the data list.
     labels= c("Large", "Small","Both"),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 0,
     ## Adjust the labels to almost 100% right-justified.
     adj = .5,
     ## Increase label size.
     cex = 1)


axis(2, pos=0.5, at=seq(0,0.35,0.05), cex=1.1)

mtext(side=3,adj=0.05,line=-1.3,"E", cex=.75)
mtext(expression(italic("CV  L"[0])), side=2, line=1.2, cex=0.9)

dev.off()

#  ------------------------------------------------------------------------------------------------------