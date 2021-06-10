#  ------------------------------------------------------------------------------------------------------

#	use simulate_population_harvest and LHsample functions to run simulations and bootstrap sampling approaches
source(MUS_Functions_Code_POS_new.R)

#  ------------------------------------------------------------------------------------------------------

# rm(list=ls())

# load packages
library(reshape)
library(dplyr)
library(ggplot2)
library(magrittr)
library(assertthat)

# --------------------------------------------------------------------------------------------------------------------------------------
#	SIMULATE POPULATION

##Example for Pristipomiodes auricilla using life history parameters from O'Malley et al. 2019
## S1: Yelloweye, small fish in catch, F = 0.5M

S1_Auric_lowF <- simulate_population_harvest(Linf = 32.5, Linf_sd = 2.5, 
                                             M = 0.18, F = 0.09, Lorenzen = TRUE, 
                                             mincat = 10, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
                                             L0 = 10, L0_sd = 2.5, k = 0.6, k_sd = 0, 
                                             Amax = 32, age_max = 15, N = 100000)

summary(S1_Auric_lowF$harvest$length)
str(S1_Auric_lowF$harvest$length)
S1_hist_length_harv <- hist(S1_Auric_lowF$harvest$length, breaks = seq(0,50,2),include.lowest=TRUE, right=FALSE,plot=FALSE)

# --------------------------------------------------------------------------------------------------------------------------------------
#	Life History Sampling Strategy & Approach
#Compare FOS to POS, minimum size selectivity, supplemental sampling, sample size, and constraining t0=0 in the VBGF


#####  A. S1_Auric_lowF#######
#FOS examples with differing sample size, constained vs unconstrained, supplemental samples added
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

#POS examples with differing sample size, constained vs unconstrained, supplemental samples added
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



######Review outputs & Plotting Example for S1_Auric_lowF#######

look_at_me <- S1_A5 #choose a LHsampling result to look at
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



tiff(file="S1_auric_lowF.tiff",width=18.3, height=20, units = "cm", pointsize = 10, res=200)
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


#######Representative Index (RI)######

S1A_Auric_noF <- simulate_population_harvest(Linf = 32.5, Linf_sd = 2.5, 
                                             M = 0.18, F = 0, Lorenzen = TRUE, 
                                             mincat = 20, catsd = 2.5, maxcat = 200, maxcatsd = 0, 
                                             L0 = 10, L0_sd = 2.5, k = 0.6, k_sd = 0, 
                                             Amax = 32, age_max = 15, N = 100000)

summary(S1A_Auric_noF$population$age)
S1A_hist_age_pop <- hist(S1A_Auric_noF$population$age, breaks = seq(0,31,1),include.lowest=TRUE, right=FALSE, plot=FALSE)
sum(S1A_hist_age_pop$density)

summary(S1A_Auric_noF$population$length)
str(S1A_Auric_noF$population$length)
S1A_hist_length_pop <- hist(S1A_Auric_noF$population$length, breaks = seq(0,50,2),include.lowest=TRUE, right=FALSE,plot=FALSE)
S1_length_harv <- hist(S1_Auric_lowF$harvest$length, breaks = seq(0,50,2),include.lowest=TRUE, right=FALSE,plot=FALSE)
S6_length_harv <- hist(S6_Auric_highF$harvest$length, breaks = seq(0,50,2),include.lowest=TRUE, right=FALSE,plot=FALSE)

sum_pop<-sum(S1A_hist_length_pop$counts)
prop_pop<-S1A_hist_length_pop$counts/sum_pop

sum_low<-sum(S1_length_harv$counts)
harv_low<-S1_length_harv$counts/sum_low

sum_high<-sum(S6_length_harv$counts)
harv_high<-S6_length_harv$counts/sum_high

lbin<-seq(0,48,2)
length(lbin)
prop_at_lenth<-cbind(lbin,prop_pop,harv_low,harv_high)
prop_at_lenth<-as.data.frame(prop_at_lenth)

col2rgb("black")
col2rgb("darkgrey")
col2rgb("lightgrey")
col2rgb("white")
mycol_dg <- rgb(25, 25, 25, max = 255, alpha = 125)
mycol_white <- rgb(255, 255, 255, max = 255, alpha = 125)

#plot to compare catch and population length frequency
par(mfcol=c(1,1))
par(omi=c(0.25,0.3,0.2,0.1))
par(mai=c(0.25,0.7,.25,0.25))
barplot(height=prop_at_lenth$harv_low, names=prop_at_lenth$lbin, col=mycol_dg, xlim=c(0,25),ylab="Proportion", xlab="Length Bin (cm)", ylim=c(0,.30))
barplot(prop_at_lenth$prop_pop,col=mycol_white, add=TRUE)
mtext(side=3,adj=.5,line=1,expression(italic("P. auricilla")), col="black", cex=.8)
mtext(side=3,adj=0.5,line=-0.2,"S1, Low F", col="black", cex=.8)





