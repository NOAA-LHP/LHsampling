
library(devtools)

devtools::install_github("NOAA-LHP/LHsampling")

library(LHsampling)

#Explore the package functions
?  simulate_population_harvest
? LH_sample
? LH_plot

# Look at package example data
? S1_Auric_lowF
S1_Auric_lowF

#Simulate a population
S1_Auric_lowF <- simulate_population_harvest(Linf = 32.5, Linf_sd = 2.5, M = 0.18, F = 0.09, Lorenzen = TRUE, mincat = 10, catsd = 2.5, maxcat = 200, maxcatsd = 0, L0 = 10, L0_sd = 2.5, k = 0.6, k_sd = 0, Amax = 32, age_max = 15, N = 10000)

#Use S1_Auric_lowF$parameters to check on the input parameters

head(S1_Auric_lowF$population)
head(S1_Auric_lowF$harvest)

plot(S1_Auric_lowF$population$length~S1_Auric_lowF$population$age)

plot(S1_Auric_lowF$harvest$length~S1_Auric_lowF$harvest$age)

#Sample from the harvest
S1_POS <-LH_sample(sim_output = S1_Auric_lowF, n_boots = 100, samp_size = 300, sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, save_bootstraps = TRUE,  Lbin_width = 2)

S1_FOS <-LH_sample(sim_output = S1_Auric_lowF, n_boots = 100, samp_size = 300, sample_type = 'FOS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, save_bootstraps = TRUE,  Lbin_width = 2)

LH_plot(S1_POS, output_type = "none")

#Another plot check
list_outputs <- c('S1_POS','S1_FOS')
par(mfrow=c(3,3))
par(omi=c(0.3,0.3,0.3,0.1)) #set outer margins
par(mai=c(0.2,0.2, 0.3, 0)) #set inner margins

for (o in 1:length(list_outputs)) {
  look_at_me <- get(list_outputs[o])

  for (j in 1:2) {
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

