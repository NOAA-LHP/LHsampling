#  ------------------------------------------------------------------------------------------------------
#  Life History population and fishery simulation with sampling approach analysis 
#	---------   NEW POS SAMPLE ROUTINE:
# 	eliminate variation in number of samples per length bin
#
#
#  Latest update Mar30, 2021
#	binl and propl have been changed to binL and propL to avoid ambiguous l letter vs. number


#  ------------------------------------------------------------------------------------------------------

# rm(list=ls())

#  load packages
library(reshape)
library(dplyr)
library(ggplot2)
library(magrittr)
library(assertthat)



# --------------------------------------------------------------------------------------------------------------------------------------
# ------- READ IN POPULATION SIMULATION FUNCTION  -----------------------
#	updated Sep24: put a floor on L0 draws to avoid negative length fish, should reduce/eliminate warnings()
#	add a true population CV at A0, Amax, Age_max
#	age_max <- 20				# choose age_max (the plus group for pop dynamics) a priori
#					# this is only used for calculating CV_L


simulate_population_harvest <- function(Linf,Linf_sd, M, Lorenzen, F, mincat, catsd, maxcat, maxcatsd, 
			L0, L0_sd, k, k_sd, Amax, age_max, N){
  
  # create Amax + 1 cohorts of N Age zero fish, put them all into a list object
  all_cohorts <- list()
  all_harvest <- list()
#  hr = 1-exp(-F)
#  mr = 1-exp(-M)

  # --------  IF USING LORENZEN LENGTH-DEPENDENT NATURAL MORTALITY 
   # Solve for M1 now
	if (Lorenzen == TRUE) {
	survship <- data.frame(Ages = seq(0,Amax,1), L_age=9999, M_L_hat=9999, 
			Mr_L_hat = 9999, N_age=9999)
	# calc A0
	A0 = log(1-L0/Linf)/k

		# grow the fish
		for (i in 1:(Amax+1)) {
			survship[i,2] = Linf*(1-exp(-k*(survship[i,1]-A0)))
			}
		
		M1 = 10		# reset M1 to something reasonable
					# set up a function to do the Lorenzen distribution
		sq_resid_M1 = function(M1) {
			for (i in 1:(Amax+1)) {
				survship[i,3] = M1/survship[i,2]
				}
			survship$Mr_L_hat = 1-exp(-1*survship$M_L_hat)
			survship[1,5] = 10000
			for (i in 2:(Amax+1)) {
				survship[i,5] = survship[(i-1),5]*(1-survship[(i-1),4])
				}
			M_overall_hat = -1*(log(survship[(Amax+1),5]/10000)/Amax)
			sq_resid = (M - M_overall_hat)^2
			return(sq_resid)
			}

		# run optimize for 1 parmater to fit M1
		fit = optimize(sq_resid_M1, interval=c(0.01,100))
		M1 <- fit$minimum 
		}
   # -----------  END solve Lorenzen M1

   # ---- Create Amax+1 Cohorts

  for (i in 1:(Amax+1)) {
    cohort <- matrix(nrow=N,ncol=Amax+4)
   
    # assign L0, Linf, k to each fish, calculate A0
    cohort[,3] = rnorm(N, mean = k, sd = k_sd)
    cohort[,4] = pmax(rnorm(N, mean = L0, sd = L0_sd),rep(0.1*L0,N))
    cohort[,2] = rnorm(N, mean = Linf, sd = Linf_sd)
    cohort[,1]= (log(1-(cohort[,4]/cohort[,2])))/cohort[,3]
    
    # ----- step A. grow the fish, all survive
    for (A in 1:Amax) {
      cohort[,A+4] = cohort[,2]*(1-exp(-(cohort[,3])*(A-cohort[,1])))
    }
    # ----  step B. apply natural mortality - Baranov - depends on fixed or Lorenzen

    selex_cohort = pmin((1-pnorm(mincat,cohort[,3:(Amax+3)],catsd)),pnorm(maxcat ,cohort[,3:(Amax+3)],maxcatsd))   
    
    if (Lorenzen == FALSE) {
	mp_cohort <- (M/(F*selex_cohort + M))*(1-exp(-F*selex_cohort - M))
    		} else {
	M_L_cohort <- M1 / cohort[,4:(Amax+4)]
	mp_cohort <- (M_L_cohort/(F*selex_cohort + M_L_cohort))*(1-exp(-F*selex_cohort - M_L_cohort))
	  	}
    # --- Nat mort survival probabilities
	surv_mr = matrix(rbinom(N*(Amax+1), size = 1, prob = 1-(mp_cohort)),nrow=N, ncol=Amax+1)

    # all fish used to survive, now we kill them based on length. So N needs to be big.

    # modify mr into a cumulative cross-product
    for (A in 1:(Amax)) {
      surv_mr[,A+1]=surv_mr[,A]*surv_mr[,A+1]
    }
    # lengths for fish that survive natural mortality, 0s for those that don't
    mr_surv_lengths = cohort[,4:(Amax+4)]*surv_mr
    
    # ----- step C. apply fishing mortality and collect catch
    # will depend on fishery selectivity at length as well as Lorenzen vs. not
    
    if (Lorenzen == FALSE) {
	hp_cohort <- ((F*selex_cohort)/(F*selex_cohort+M))*(1-exp(-F*selex_cohort-M))
    		} else {
	hp_cohort <- ((F*selex_cohort)/(F*selex_cohort+M_L_cohort))*(1-exp(-F*selex_cohort-M_L_cohort))
		}

	catch_cohort = matrix(rbinom(N*(Amax+1), size = 1, prob = hp_cohort),nrow=N, ncol=Amax+1)
    
    #  each fish only gets caught once
    # modify catch_cohort into additive vector over ages so once a fish gets "caught" the second time, catch_cohort > 1
    for (A in 1:(Amax)) {
      catch_cohort[,A+1]=catch_cohort[,A]+catch_cohort[,A+1]
    }
    # this is goofy- take another additive product so that the age a fish gets caught first = 1, subsequent ages > 1
    for (A in 1:(Amax)) {
      catch_cohort[,A+1]=catch_cohort[,A]+catch_cohort[,A+1]
    }
    # the age a fish gets caught first = 1, all others = 0
    catch_once_cohort <- replace(catch_cohort, catch_cohort > 1, 0)
    
    # lengths for fish that survive natural mortality AND GET CAUGHT, 0s for those that don't
    catch_lengths = mr_surv_lengths*catch_once_cohort
    #  save it in the all_harvest list
    all_harvest[[i]] <- catch_lengths
    
    # apply fishing mortality by taking the fish that got caught (identified above) out of the population of fish that survived natural mortality
    hr_surv <- 1-catch_once_cohort
    # turn this into a cumulative cross-product
    for (A in 1:(Amax)) {
      hr_surv[,A+1]=hr_surv[,A]*hr_surv[,A+1]
    }
    # lengths for fish that survive, 0s for those that don't
    cohort_lengths = mr_surv_lengths*hr_surv
    #  save it
    all_cohorts[[i]] <- cohort_lengths
  }
  
  # now pull one age from each cohort to build the overall population
  # create a population object using Age0
  population <- data.frame(age=0, length = all_cohorts[[1]][,1])
  #drop dead fish
  population <- subset(population , length != 0)
  
  for (i in 1:(Amax)) {
    each_age <- data.frame(age=i, length = all_cohorts[[(i+1)]][,(i+1)])
    #drop dead fish
    each_age <- subset(each_age , length != 0)
    #save
    population <- rbind(population, each_age)
  }
  #  we have a population
  
  #  repeat for harvest 
  harvest <- data.frame(age=0, length = all_harvest[[1]][,1])
  #drop dead fish
  harvest <- subset(harvest , length != 0)
  for (i in 1:(Amax)) {
    each_h_age <- data.frame(age=i, length = all_harvest[[(i+1)]][,(i+1)])
    #drop dead fish
    each_h_age <- subset(each_h_age , length != 0)
    #save
    harvest <- rbind(harvest, each_h_age)
  }
  #  we have a harvest
  
  # add a warning message if L0_sd was large compared to L0 such that we have negative length fish
  if (min(population$length)<0) { print("Warning: negative length fish, L0_sd too large compared to L0") }
  
  # make a quick average M at age object
	A0 = log(1-L0/Linf)/k
	Avg_age <- data.frame(Ages = seq(0,Amax,1), L_age=9999, M_age = 9999, Selex = 9999)
			# grow the fish
				for (i in 1:(Amax+1)) {
				Avg_age[i,2] = Linf*(1-exp(-k*(Avg_age[i,1]-A0)))
				}
			
	if (Lorenzen == FALSE) {
		Avg_age$M_age = M
		} else {
		Avg_age$M_age = M1 / Avg_age$L_age
		}

	Avg_age$Selex <- pmin((1-pnorm(mincat,Avg_age$L_age,catsd)),pnorm(maxcat,Avg_age$L_age,maxcatsd))

	if (Lorenzen == TRUE) {
				M1 = M1
				} else {
				M1 = 'NA'
				}
  
	# calculate CV length at age

	all_big_fish_pop <- subset(population, age >= age_max)
	all_big_fish_harv <- subset(harvest, age >= age_max)
	all_big_fish <- rbind(all_big_fish_pop, all_big_fish_harv)
	CV_L_age_max <- sd(all_big_fish$length)/mean(all_big_fish$length)

	all_0_fish_pop <- subset(population, age == 0)
	all_0_fish_harv <- subset(harvest, age == 0)
	all_0_fish <- rbind(all_0_fish_pop, all_0_fish_harv)
	CV_L_0 <- sd(all_0_fish$length)/mean(all_0_fish$length)		# in theory should be very close to L0_sd / L0

  params <- list(Linf = Linf,
				Linf_sd = Linf_sd,
				Lorenzen = Lorenzen,
				M = M,
				M1 = M1,
				F = F, 
				mincat = mincat , 
				catsd = catsd , 
				maxcat = maxcat , 
				maxcatsd = maxcatsd , 
				L0 = L0 , 
				L0_sd= L0_sd, 
				k = k,
				k_sd = k_sd, 
				Amax = Amax,
				age_max = age_max,
				CV_L_age_max =  CV_L_age_max,
				CV_L_0 =  CV_L_0,
				N = N)

  return(list(population = population, harvest = harvest, Avg_age = Avg_age, parameters = params))

}	#  --------------------------------  end function


# --------------------------------------------------------------------------------------------------------------------------------------




# --------------------------------------------------------------------------------------------------------------------------------------
#	SIMULATE SOME POPS
##	Scenario 1 Onaga  (F = 0.3, no small in catch)
		
#		pop_harv <-simulate_population_harvest(Linf=100,Linf_sd=2.5, M=0.17, F=0.3, Lorenzen=TRUE, mincat=30, catsd=2.5,maxcat = 200, 
#                                       maxcatsd = 0, L0=10, L0_sd=2.5, k=0.14, k_sd=0, Amax=40, age_max = 20, N=100000)

#  save
# S1_OnagaF3Nosmall_simulated_object <- pop_harv





# --------------------------------------------------------------------------------------------------------------------------------------



# --------------------------------------------------------------------------------------
#  LH sampling / BOOTSTRAP SAMPLING ROUTINE
#	output is list objects of population params, input args, all bootstrap samples (optional), output parms, best fit models.

#	Updates Sep9+
#	includes:
#	  improved handling of errors from nls()
#	  options for FOS or POS
#	  options for supplement samples in large and/or small length bins, specify n per bin
#	  options for constrained 
#	Update Sep29
#	  corrected minimization error handling.
#	Update Oct 5
#	  corrected negative CV_L0 values
# -------------------------------------------------------------------------------

#  -----------------  arguments, typical values
#	sim_output <- pop_harv		# output of the simulate_population_harvest function
#	n_boots <- 100			# number of bootstraps
#   	samp_size <- 300			# sample size for each bootstrap
#	sample_type <- 'POS'		# define as 'POS' or 'FOS'
 
#   	supp_large <- FALSE		# do we want to supplement with large fish?
#   	supp_large_n_per_bin <- 3	# number supplemental samples per large bin
#   	supp_small <- FALSE		# do we want to supplement with small fish?
#   	supp_small_n_per_bin <- 3	# number supplemental samples per small bin
#   	supp_min_length <- 10		# for supplemental small sampling, what is the smallest fish (cm) we could get?

#   	constrained <- FALSE		# is t0 constrained?
#   	t0 <- 0				# if constrainted, define t0

#   	save_bootstraps <- TRUE		# should all bootstrap sample draws be saved? if n_boots or samp_size are large, this should be false
#   	Amax <- 40				# maximum theoretical age (prop population input parameter)
#   	age_max <- 20			# choose age_max (the plus group for pop dynamics) a priori
#   	Lbin_width <- 5			# length bin width

# options(warn=0)

#  -----------------  BEGIN READ IN FUNCTION

LH_sample <- function(sim_output, n_boots, samp_size, sample_type, supp_large = FALSE, supp_large_n_per_bin = 3, 
					  supp_small = FALSE, supp_small_n_per_bin = 3, supp_min_length = 2, constrained = FALSE, 
					  t0 = 0, SD_L_const = TRUE, save_bootstraps = FALSE, Amax, age_max, Lbin_width = 2) {

#  Preliminaries:
#  Get system time
	time_start <- Sys.time()
	print(time_start)

#  define population_true and population_harvest
   	population_true <- sim_output$population	
   	population_harv <- sim_output$harvest			

#  make empty lists to hold each of the things we are interested in:
   	list_boot_mods <- list()
   	list_boot_preds <- list()
   	list_boot_CVs <- list()
	list_boot_SDs <- list()
   	list_boot_samps <- list()
	list_some_boot_samps <- list()

# which boots will we save for diagnostics
	if(n_boots > 9) {
		save_9boots <- sample(1:n_boots,9,replace=FALSE)
		}
	if(n_boots <= 9) {
		save_9boots <- seq(1,n_boots,1)
		}

#  bin harvest and population lengths		#head(population_harv)
   	brks <- seq(0,1.1*max(population_true$length),Lbin_width) 
   	age <- seq(0,Amax,1)
   	population_harv$binL <- cut(population_harv[,"length"], breaks=brks, labels=brks[1:(length(brks)-1)],right=F)

    population_harv <- population_harv %>% 		
     group_by(binL) %>%
     mutate(prop=n()) %>%
     group_by() %>%
     mutate(n=sum(n())) %>%
     mutate(propL=prop/n)					#str(population_harv)	head(population_harv) 

   population_harv <- mutate(population_harv, binL_num = as.character(binL))
   population_harv$binL_num <- as.numeric(population_harv$binL_num)		#population_harv[1:500,]

   population_harv_df <- as.data.frame(population_harv)


   # update 30Mar: new POS approach. Only do if sample_type==POS:

  if(sample_type == 'POS') {
   basic_POS_props <- unique(population_harv_df[c("binL","propL")])		#sum(basic_POS_props$prop1)
   basic_POS_props <- basic_POS_props[order(basic_POS_props$binL),]		#str(basic_POS_samps)
   basic_POS_samps <- mutate(basic_POS_props, basic_POS_n = round((samp_size*propL),0), 
				binL_num = as.numeric(as.character(binL)))	#basic_POS_samps[9,3] <- 40
   # 
	if (sum(basic_POS_samps$basic_POS_n) > samp_size) {
		while(sum(basic_POS_samps$basic_POS_n) > samp_size) {
		most_index <- which(basic_POS_samps$basic_POS_n == max(basic_POS_samps$basic_POS_n))[1]
		basic_POS_samps[most_index,3] <- basic_POS_samps[most_index,3] - 1
			}
		}
	if (sum(basic_POS_samps$basic_POS_n) < samp_size) {
		while(sum(basic_POS_samps$basic_POS_n) < samp_size) {
		most_index <- which(basic_POS_samps$basic_POS_n == max(basic_POS_samps$basic_POS_n))[1]
		basic_POS_samps[most_index,3] <- basic_POS_samps[most_index,3] + 1
			}
		}
   }

#  add size bins for the population
    	population_true$binL <- cut(population_true[,"length"], breaks=brks, labels=brks[1:(length(brks)-1)],right=F)
   	population_true$binL <- as.character(population_true$binL)
   	population_true$binL <- as.numeric(population_true$binL)		# head(population_true)	#str(population_true)

#  reset error counter
    count_error <- 0 

# ---- prepare some FOS error warnings outside of the bootstrap loop
#		this sample won't actually get used in the bootstrap sampling, it is just here so errors don't come out
#		nsamp times
	if(sample_type == 'FOS') {
	  # determine how many fish in each bin in our harvest
	  count_harv_samps_per_bin <- summary(population_harv$binL)
	  count_harv_samps_per_bin_df <- data.frame(bin_lower = as.numeric(names(count_harv_samps_per_bin)),
								bin_counts = as.numeric(count_harv_samps_per_bin))
	  bins_w_samps_harv <- nrow(subset(count_harv_samps_per_bin_df, bin_counts != 0))
	  min_samps_per_bin <- ceiling(samp_size/bins_w_samps_harv)

	  # try to take that many samples per bin
	  samps_per_bin <- min_samps_per_bin
	  FOS <- population_harv %>% dplyr::group_by(binL) %>% sample_n(pmin(n(),samps_per_bin ))
	    while(nrow(FOS) < samp_size) {
	    samps_per_bin <- samps_per_bin + 1
	    FOS <- population_harv %>% dplyr::group_by(binL) %>% sample_n(pmin(n(),samps_per_bin ))
	    }
  	  FOS <- FOS %>% 
    	  group_by(binL) %>%
    	  mutate(prop=n()) %>%
	  select(age, length, binL, prop, binL_num)
	  # coerce to dataframe
	  FOS <- as.data.frame(FOS)

	# return warnings to alert user if they are asking for a large number of supplemental samples relative to the
	#		number of samples per bin in the unsupplemented design
	    if(supp_small == TRUE) {
		if(supp_small_n_per_bin > max(FOS$prop)) {
			warn_message <- paste('warning: max unsupplemented FOS samps per Lbin', max(FOS$prop),
				'user requested', supp_small_n_per_bin, 'additional samps per Lbin', sep=" ")
			print(warn_message)
			}
		}
	    if(supp_large == TRUE) {
		if(supp_large_n_per_bin > max(FOS$prop)) {
			warn_message <- paste('warning: max unsupplemented FOS samps per Lbin', max(FOS$prop),
				'user requested', supp_large_n_per_bin, 'additional samps per Lbin', sep=" ")
			print(warn_message)
			}
		}
	}


for(i in 1:n_boots) {

# print(paste("begin boot",i,sep=" "))

   if(sample_type == 'POS') {	# ---------------------  BEGIN POS SAMPLING


   #head(population_harv_df)		#str(population_harv_df)
   #	head(population_harv)		# str(basic_POS_samps)

   POS_build <- population_harv_df[1,]
	for (pp in 1:nrow(basic_POS_samps)) {
		if(basic_POS_samps$basic_POS_n[pp]>0){
			sample_me_binL <- basic_POS_samps$binL_num[pp]
			sample_me_n	<- basic_POS_samps$basic_POS_n[pp]
			sample_me_pop_harv <- subset(population_harv_df, binL_num == sample_me_binL)		#str(sample_me_pop_harv)	
			sample_me <- sample_n(sample_me_pop_harv, sample_me_n, replace=FALSE)
		POS_build <- rbind(POS_build, sample_me)
		}
	    }
   #drop row 1 of POS_build
	POS_build <- POS_build[-1,]		#str(POS_build)

   #rearrange to make compatible with remainder of sampling...		#str(POS)
	POS <- POS_build %>% select(age, length, binL_num, n)
	colnames(POS) <- c('age','length','binL','n' )

   # re-count samps per bin
	      POS <- POS %>% 		
     	      group_by(binL) %>%
     	      mutate(nbin=n()) %>%
     	      group_by()
	   # coerce back to df
	      POS <- as.data.frame(POS)
	   # sort by length ascending
	      POS <- POS[order(POS$length),]	#nrow(POS)

	# IF supplemental sampling, collect some info for later...
	if(supp_small == TRUE) {
		  # we will need to know how many samples from which small bins to collect
		POS_samps_per_bin <- unique(POS[c("binL","nbin")])
		sm_need_bins_df <- data.frame('binL' = seq(supp_min_length, max(population_true$binL), Lbin_width))
		sm_need_bins_df <- merge(sm_need_bins_df, POS_samps_per_bin, by='binL', all.x = TRUE)		
			sm_need_bins_df[is.na(sm_need_bins_df)] <- 0 
		sm_need_bins_df <- mutate(sm_need_bins_df, get_samps = pmax(0,(supp_small_n_per_bin - nbin)))
		middle_sampled_bin <- subset(sm_need_bins_df, nbin == max(sm_need_bins_df$nbin))  
		middle_sampled_bin <- as.numeric(middle_sampled_bin$binL)[1]
		sm_get_bins_df <- subset(sm_need_bins_df, binL < middle_sampled_bin & get_samps > 0)
		sm_get_bins_df <- data.frame('binL' = sm_get_bins_df$binL, 'get_samps' = sm_get_bins_df$get_samps)
		  # note: if the user asks for sup small but there are already > supp_small_n_per_bin,
		  #	then sm_get_bins_df will be empty
		}

	if(supp_large == TRUE) {
		  # we will need to know how many samples from which large bins to collect
		POS_samps_per_bin <- unique(POS[c("binL","nbin")])
		lg_need_bins_df <- data.frame('binL' = seq(supp_min_length, max(population_true$binL), Lbin_width))
		lg_need_bins_df <- merge(lg_need_bins_df, POS_samps_per_bin, by='binL', all.x = TRUE)		
			lg_need_bins_df[is.na(lg_need_bins_df)] <- 0 
		lg_need_bins_df <- mutate(lg_need_bins_df, get_samps = pmax(0,(supp_large_n_per_bin - nbin)))
		middle_sampled_bin <- subset(lg_need_bins_df, nbin == max(lg_need_bins_df$nbin))  
		middle_sampled_bin <- as.numeric(middle_sampled_bin$binL)[1]
		lg_get_bins_df <- subset(lg_need_bins_df, binL > middle_sampled_bin & get_samps > 0)
		lg_get_bins_df <- data.frame('binL' = lg_get_bins_df$binL, 'get_samps' = lg_get_bins_df$get_samps)
		  # note: if the user asks for sup small but there are already > supp_small_n_per_bin,
		  #	then lg_get_bins_df will be empty	test with  nrow(lg_get_bins_df)
		}

	#trim POS	head(POS)
	POS <- POS[,c(1:3)]


    # ------------------  SUPPLEMENTAL SAMPLING
    # LARGE
	 if(supp_large == TRUE) {
	  		population_addition <- merge(population_true, lg_get_bins_df, by='binL',all.x=TRUE)
	  		population_addition$get_samps[is.na(population_addition$get_samps)] <- 0
        	 	s_samples <- population_addition %>%dplyr::group_by(binL)%>% dplyr::sample_n(pmin(n(), get_samps))
        	 	s_samples <- as.data.frame(s_samples)		# str(s_samples)
		 	s_samples <- s_samples[,c(2,3,1)]
        	 	POS <- rbind(POS, s_samples) #join additional samples to FOS samples	#str(POS)	#str(s_samples)
	    }

    # SMALL		#head(population_addition)
      if(supp_small == TRUE) {
 	  population_addition <- merge(population_true, sm_get_bins_df, by='binL',all.x=TRUE)
	  population_addition$get_samps[is.na(population_addition$get_samps)] <- 0
        	 s_samples <- population_addition %>%dplyr::group_by(binL)%>% dplyr::sample_n(pmin(n(), get_samps))
        	 s_samples <- as.data.frame(s_samples)		# str(s_samples)
		 s_samples <- s_samples[,c(2,3,1)]
        	 POS <- rbind(POS, s_samples)
		}

    #	if we just built onto POS using either large or small supplemental sampling, 
    #		we must reduce total N back down to target, but don't through out any of the supplemental samples
    # repeat as long as necessary		head(POS)

    # watch out: old rows of POS are ordered by length. We want lengths to be random within binL
    # complete rand...
	POS <- sample_n(POS, nrow(POS), replace=FALSE)
    # sort by BinL
	POS <- POS[order(POS$binL),]

	while(nrow(POS) > samp_size) {
	# print(paste("remove extra samples, n=", nrow(POS)-samp_size, sep=" "))
	   # re-count samps per bin
	      POS <- POS %>% 		
     	      group_by(binL) %>%
     	      mutate(nbin=n()) %>%
     	      group_by()
	   # coerce back to df
	      POS <- as.data.frame(POS)
 	   # add pkeep = high for all samples
	      POS <- mutate(POS, pkeep = 1e10)
	   # ensure we don't throw out more than 1 samp per bin
	   # determine the L bins that were not supp sampled
		sm_index <- which(POS$nbin > supp_small_n_per_bin)[1]
		min_non_supp <- POS$binL[sm_index]
		lg_index <- tail(which(POS$nbin > supp_large_n_per_bin),1)
		max_non_supp <- POS$binL[lg_index]

		for (ii in 2:nrow(POS)) {
		if(POS$binL[ii] >= min_non_supp) {
		   if(POS$binL[ii] <= max_non_supp) {
			if(POS$binL[ii] != POS$binL[ii-1]) {
		  	POS$pkeep[ii] <- 0.5
			}
		     }
	   	    }
		   }

	   #  how many samples do we want to possibly throw out?
		ok_throw_out <- nrow(subset(POS, pkeep==0.5))
		not_ok_throw_out <- nrow(POS) - ok_throw_out
			if (not_ok_throw_out >= samp_size) {
			   POS_keep <- sample_n(POS, not_ok_throw_out, replace=FALSE, weight = pkeep)	
				}
			if (not_ok_throw_out < samp_size) {
			   POS_keep <- sample_n(POS, samp_size, replace=FALSE, weight = pkeep)	
				}
		POS <- POS_keep[,1:3]
          }
	# at the conclusion of this do while loop, we should have correct samp_size
		use_sample <- POS			#head(use_sample)		#str(use_sample)
    }	# ----------------------  END draw sample if POS


   if(sample_type == 'FOS') {	# ---------------------  BEGIN FOS SAMPLING.

	# determine how many fish in each bin in our harvest
	count_harv_samps_per_bin <- summary(population_harv$binL)
	count_harv_samps_per_bin_df <- data.frame(bin_lower = as.numeric(names(count_harv_samps_per_bin)),
								bin_counts = as.numeric(count_harv_samps_per_bin))
	bins_w_samps_harv <- nrow(subset(count_harv_samps_per_bin_df, bin_counts != 0))
	min_samps_per_bin <- ceiling(samp_size/bins_w_samps_harv)

	# try to take that many samples per bin
	samps_per_bin <- min_samps_per_bin

	FOS <- population_harv %>% dplyr::group_by(binL) %>% sample_n(pmin(n(),samps_per_bin ))

	while(nrow(FOS) < samp_size) {
	samps_per_bin <- samps_per_bin + 1
	FOS <- population_harv %>% dplyr::group_by(binL) %>% sample_n(pmin(n(),samps_per_bin ))
	}

 	FOS <- FOS %>% 
    	group_by(binL) %>%
    	mutate(prop=n()) %>%
	select(age, length, binL, prop, binL_num)

	# coerce to dataframe
	FOS <- as.data.frame(FOS)

	#	for any bins with less than the max number of samples, assign a probability of being kept that is very high, i.e. 1e10
	#		so all of those samples are equally likely (approaching 1) to be sampled.
	#	for bins that have the max number of samples, assign a small number, e.g., 0.5.
	# in the event that nrow(FOS) = samp_size, then the following step doesn't actually change anything,
	#	i.e., FOS and FOS_keep are the same, just a different order since we sampled without replacement.
	#  this is done iteratively, shaving down nrow(FOS) so that we don't exclude more samples from some bins than others

	FOS <- mutate(FOS, pkeep = ifelse(prop < samps_per_bin, 1e10, 0.5))		#View(FOS)
	#   this is a little silly... now, we want to assign pkeep = 0.5 to only 1 row for rows where prop = samps_per_bin
	for (ii in 2:nrow(FOS)) {
		if(FOS$prop[ii] == samps_per_bin) {
			if(FOS$binL_num[ii] == FOS$binL_num[ii-1]) {
		  	FOS$pkeep[ii] <- 1e10
			}
		}
	   }

	FOS_keep <- sample_n(FOS, samp_size, replace=FALSE, weight = pkeep)		# str(FOS_keep)  #View(FOS_keep) #head(FOS)

	# IF supplemental sampling, collect some info for later...
	if(supp_small == TRUE) {
		  # we will need to know how many samples from which small bins to collect
		FOS_keep_samps_per_bin <- unique(FOS_keep[c("binL_num","prop")])
		sm_need_bins_df <- data.frame('binL_num' = seq(supp_min_length, max(population_true$binL), Lbin_width))
		sm_need_bins_df <- merge(sm_need_bins_df, FOS_keep_samps_per_bin, by='binL_num', all.x = TRUE)		
			sm_need_bins_df[is.na(sm_need_bins_df)] <- 0 
		sm_need_bins_df <- mutate(sm_need_bins_df, get_samps = pmax(0,(supp_small_n_per_bin - prop)))
		sm_get_bins_df <- subset(sm_need_bins_df, binL_num < median(sm_need_bins_df$binL_num) & get_samps > 0)
		sm_get_bins_df <- data.frame('binL' = sm_get_bins_df$binL_num, 'get_samps' = sm_get_bins_df$get_samps)
		  # note: if the user asks for sup small but there are already > supp_small_n_per_bin,
		  #	then sm_get_bins_df will be empty
		}

	if(supp_large == TRUE) {
		  # we will need to know how many samples from which small bins to collect
		FOS_keep_samps_per_bin <- unique(FOS_keep[c("binL_num","prop")])
		lg_need_bins_df <- data.frame('binL_num' = seq(supp_min_length, max(population_true$binL), Lbin_width))
		lg_need_bins_df <- merge(lg_need_bins_df, FOS_keep_samps_per_bin, by='binL_num', all.x = TRUE)		
			lg_need_bins_df[is.na(lg_need_bins_df)] <- 0 
		lg_need_bins_df <- mutate(lg_need_bins_df, get_samps = pmax(0,(supp_large_n_per_bin - prop)))	
		lg_get_bins_df <- subset(lg_need_bins_df, binL_num > median(lg_need_bins_df$binL_num) & get_samps > 0)
		lg_get_bins_df <- data.frame('binL' = lg_get_bins_df$binL_num, 'get_samps' = lg_get_bins_df$get_samps)
		  # note: if the user asks for sup small but there are already > supp_small_n_per_bin,
		  #	then sm_get_bins_df will be empty
		}

  #	FOS <- FOS_keep[,1:3]	
	FOS <- FOS_keep[,1:4]								# str(FOS)
	FOS$binL <- as.character(FOS$binL)
	FOS$binL <- as.numeric(FOS$binL)

    # ------------------  SUPPLEMENTAL SAMPLING
    # LARGE
	 if(supp_large == TRUE) {
	  population_addition <- merge(population_true, lg_get_bins_df, by='binL',all.x=TRUE)
	  population_addition$get_samps[is.na(population_addition$get_samps)] <- 0 	#head(subset(population_addition, binL == 10))
        	 s_samples <- population_addition %>%dplyr::group_by(binL)%>% dplyr::sample_n(pmin(n(), get_samps))
        	 s_samples <- as.data.frame(s_samples)		# str(s_samples)
		 colnames(s_samples)[4] <- 'prop'
		  # match columns to FOS so we can rbind
		 s_samples <- s_samples[,c(2,3,1,4)]
        	 FOS <- rbind(FOS, s_samples) #join additional samples to FOS samples	#str(FOS2)
		}

    # SMALL		#head(population_addition)
      if(supp_small == TRUE) {
 	population_addition <- merge(population_true, sm_get_bins_df, by='binL',all.x=TRUE)
	  population_addition$get_samps[is.na(population_addition$get_samps)] <- 0 	#head(subset(population_addition, binL == 10))
        	 s_samples <- population_addition %>%dplyr::group_by(binL)%>% dplyr::sample_n(pmin(n(), get_samps))
        	 s_samples <- as.data.frame(s_samples)		# str(s_samples)
		 colnames(s_samples)[4] <- 'prop'
		  # match columns to FOS so we can rbind
		 s_samples <- s_samples[,c(2,3,1,4)]
        	 FOS <- rbind(FOS, s_samples) #join additional samples to FOS samples	#str(FOS2)
		}

    # drop old prop column since we don't need it now
	   FOS <- FOS[,-4]		# nrow(FOS)

    #	if we just built onto FOS using either large or small supplemental sampling, 
    #		we must reduce total N back down to target, but don't throw out any of the supplemental samples
    # repeat as long as necessary

    # make sure to randominze within binL
	FOS <- sample_n(FOS, nrow(FOS), replace=FALSE)
	FOS <- FOS[order(FOS$binL),]

	while(nrow(FOS) > samp_size) {
	#  print(paste("remove extra samples, n=", nrow(FOS)-samp_size, sep=" "))
	  # re-count samps per bin
	      FOS <- FOS %>% 		
     	      group_by(binL) %>%
     	      mutate(nbin=n()) %>%
     	      group_by()
	   # coerce back to df
	      FOS <- as.data.frame(FOS)
 	   # add pkeep
	      FOS <- mutate(FOS, pkeep = ifelse(nbin < (max(FOS$nbin)), 1e10, 0.5))
	   # ensure we don't throw out more than 1 samp per bin
		for (ii in 2:nrow(FOS)) {
		if(FOS$nbin[ii] == max(FOS$nbin)) {
			if(FOS$binL[ii] == FOS$binL[ii-1]) {
		  	FOS$pkeep[ii] <- 1e10
			}
		     }
	   	    }
	   #  how many samples do we want to possibly throw out?
		ok_throw_out <- nrow(subset(FOS, pkeep==0.5))
		not_ok_throw_out <- nrow(FOS) - ok_throw_out
			if (not_ok_throw_out >= samp_size) {
			   FOS_keep <- sample_n(FOS, not_ok_throw_out, replace=FALSE, weight = pkeep)	
				}
			if (not_ok_throw_out < samp_size) {
			   FOS_keep <- sample_n(FOS, samp_size, replace=FALSE, weight = pkeep)	
				}
		FOS <- FOS_keep[,1:3]
          }
	# at the conclusion of this do while loop, we should have correct samp_size
   use_sample <- FOS			#head(use_sample)		#str(use_sample)
   }	# ---------------------  END IF FOS

	
   # ----  save this sample if randomly chosen
	if(i %in% save_9boots) {
		save_index <- which(i == save_9boots)
		list_some_boot_samps[[save_index]] <- hist(use_sample$length, breaks = seq(0,(max(population_true$length)+Lbin_width),Lbin_width),
								include.lowest=TRUE, right=FALSE,plot=FALSE)
		}
	
    # ------------------   now we have the sample. Solve growth equation.		# use_sample <- list_boot_samps[[i]]

  # if t0 constrained to 0
	if (constrained == TRUE) {
	
    # check to see if the model will minimize
       trynls <- try(nls(length ~ Linf* (1-exp(-(K1*((age+1)-t0)))),
                      data=use_sample, start = list (Linf = 100, K1 = 0.14)), silent=T)
    
    # if the model will not minimize (use is.error from assertthat), increase the counter, restart loop 
    #  ---- ERIN MODIFY this step LATER so the loop will automatically do additional i's
    #		to make up for each sample that didn't minimize.		 
    # if the model does minimize, refit the model and proceed with rest of loop
    
    if (is.error(trynls)) { 	
      count_error <- count_error + 1 
 	 } else
      {
        list_boot_mods[[i]] <- nls(length ~ Linf* (1-exp(-(K1*((age+1)-t0)))),
                                   data=use_sample, start = list (Linf = 100, K1 = 0.14))
        
        pred.length_use_sample <- coef(list_boot_mods[[i]])[1]*(1 - exp(-coef(list_boot_mods[[i]])[2]*((age + 1) - t0)))
        pred_lengths <- data.frame(age=age, length=pred.length_use_sample)
        list_boot_preds[[i]] <- pred_lengths
        CV_boot <- data.frame(age=age, CV_age = as.numeric(NA))
        for (a in 1 :Amax+1) {
          select_fish <- subset(use_sample, age == (a-1))
          CV_a <- sd(select_fish$length)/mean(select_fish$length)
          CV_boot[a,2] <- CV_a
        }
        list_boot_CVs[[i]] <- CV_boot

        SD_boot <- data.frame(age=age, SD_age = as.numeric(NA))
        for (a in 1 :Amax+1) {
          select_fish <- subset(use_sample, age == (a-1))
          SD_a <- sd(select_fish$length)
          SD_boot[a,2] <- SD_a
        }
        list_boot_SDs[[i]] <- SD_boot


      }  # END predict length and calc CV / SD

    # save each bootstrap sample, watch out, this could get big.
  	if (save_bootstraps == TRUE) {
	  list_boot_samps[[i]] <- use_sample
		}

	}	# --- END if constrained

  # --- if we estimate L0 freely
	if (constrained == FALSE) {

    # check to see if the model will minimize
       trynls <- try(nls(length ~ Linf* (1-exp(-(K1*(age-a0)))),
                      data=use_sample, start = list (Linf = 100, K1 = 0.14, a0=0.1)), silent=T)
    
    # if the model will not minimize (use is.error from assertthat), increase the counter, restart loop 
    #  ---- ERIN MODIFY this step LATER so the loop will automatically do additional i's
    #		to make up for each sample that didn't minimize.		 
    # if the model does minimize, refit the model and proceed with rest of loop
    
    if (is.error(trynls)) { 	
      count_error <- count_error + 1 
	} else
      {
        list_boot_mods[[i]] <- nls(length ~ Linf* (1-exp(-(K1*(age-a0)))),
                                   data=use_sample, start = list (Linf = 100, K1 = 0.14, a0=0.1))
        # t0 was zero here, include estimate t0
        pred.length_use_sample <- coef(list_boot_mods[[i]])[1]*(1 - exp(-coef(list_boot_mods[[i]])[2]*(age - coef(list_boot_mods[[i]])[3])))
        pred_lengths <- data.frame(age=age, length=pred.length_use_sample)
        list_boot_preds[[i]] <- pred_lengths
        CV_boot <- data.frame(age=age, CV_age = as.numeric(NA))
        for (a in 1 :Amax+1) {
          select_fish <- subset(use_sample, age == (a-1))
          CV_a <- sd(select_fish$length)/mean(select_fish$length)
          CV_boot[a,2] <- CV_a
        }
        list_boot_CVs[[i]] <- CV_boot
      
        SD_boot <- data.frame(age=age, SD_age = as.numeric(NA))
        for (a in 1 :Amax+1) {
          select_fish <- subset(use_sample, age == (a-1))
          SD_a <- sd(select_fish$length)
          SD_boot[a,2] <- SD_a
        }
        list_boot_SDs[[i]] <- SD_boot

		}  # END if nls error check

    # save each bootstrap sample, watch out, this could get big.
  	if (save_bootstraps == TRUE) {
	  list_boot_samps[[i]] <- use_sample
		}
	}  # --- END if not constrained

# print(i)

   }	# ------ END boot i


#  make a warning message
	if (count_error > 0) {
	warn_message <- paste('warning: bootstraps convergence fail ', count_error, sep="")
	print(warn_message)
	}

  parameter_outputs <- data.frame(Linf = numeric(), K1 = numeric(), a0 = numeric(), CV_L_a0 = numeric(), CV_L_age_max = numeric())

  for (i in 1: length(list_boot_mods)) {
	if (is.null(list_boot_mods[[i]])) {
	# print('von bert nls error')
	} else {
	parameter_outputs[i,1] <- as.numeric(coef(list_boot_mods[[i]])[1])
	parameter_outputs[i,2] <- as.numeric(coef(list_boot_mods[[i]])[2])
	
	if (constrained == FALSE) {
	parameter_outputs[i,3] <- as.numeric(coef(list_boot_mods[[i]])[3])
	}

	if (constrained == TRUE) {
	parameter_outputs[i,3] <- -1
	}
	
	CV_age_boot <- list_boot_CVs[[i]]
	SD_age_boot <- list_boot_SDs[[i]]

#  	 calculate CV_L_age0 and CV_L_ageMax
#		UPDATE Nov 2 for the manuscript, we assumed SD Length was constant (linear) over ages, so assuming CV is linear here 
#			is setting us up for poor CV_L0 estimates. Erin modify so SD is assumed linear with length.
#	 if the slope is 0, nls will throw an error, deal with that, use intercept (avg).

	if (SD_L_const == FALSE) {
		trynls <- try(nls(CV_age ~ b1*age + b0 ,
                      data=CV_age_boot, start = list (b1=-0.001, b0=0.05)), silent=T)
		  if (is.error(trynls)) { 
		  parameter_outputs[i,4] <- mean(CV_age_boot$CV_age, na.rm = TRUE)
		  parameter_outputs[i,5] <- mean(CV_age_boot$CV_age, na.rm = TRUE)
			} else {
 		  fit_line_cv <- nls(CV_age ~ b1*age + b0 ,
                                   data=CV_age_boot, start = list (b1=-0.001, b0=0.05))
		  parameter_outputs[i,4] <- max(0.001,as.numeric(coef(fit_line_cv)[2]))
		  parameter_outputs[i,5] <- max(0.001,as.numeric(coef(fit_line_cv)[2]) + age_max*as.numeric(coef(fit_line_cv)[1]))
			} 

		    } else {
		
		  trynls <- try(nls(SD_age ~ b1*age + b0 ,
                      data=SD_age_boot, start = list (b1=-0.001, b0=0.05)), silent=T)
		  if (is.error(trynls)) { 
			mean_sd <- mean(SD_age_boot$SD_age, na.rm = TRUE)
			est_L0 <- parameter_outputs[i,1]*(1-exp(parameter_outputs[i,3]*parameter_outputs[i,2]))
		  	parameter_outputs[i,4] <- mean_sd/est_L0
			parameter_outputs[i,5] <- mean_sd/parameter_outputs[i,1]
			
		    } else {

 		  fit_line_sd <- nls(SD_age ~ b1*age + b0 ,
                                   data=SD_age_boot, start = list (b1=-0.001, b0=0.05))
			pred_sd_L0 <- max(0.001,as.numeric(coef(fit_line_sd)[2]))
			  est_L0 <- parameter_outputs[i,1]*(1-exp(parameter_outputs[i,3]*parameter_outputs[i,2]))
		  	parameter_outputs[i,4] <- pred_sd_L0/est_L0

			pred_sd_Lmax <- max(0.001,as.numeric(coef(fit_line_sd)[2]) + age_max*as.numeric(coef(fit_line_sd)[1]))
			parameter_outputs[i,5] <- pred_sd_Lmax/parameter_outputs[i,1]

			} 
		    } 
 	}
   }	#  --- END create parameter_outputs

 	# back-calculate L0 for easy comparison
	parameter_outputs <- mutate(parameter_outputs, L0 = Linf*(1-exp(a0*K1)))

#    Extract out summaries of all bootstrap runs, put in a df
	
	extract_summary <- data.frame(parm_name = character(), lower95 = numeric(), lower50 = numeric(), 
				avg = numeric(), upper50 = numeric(), upper95 = numeric(), stringsAsFactors=FALSE)

     for (j in 1: ncol(parameter_outputs)) {
	  extract_summary[j,1] <- colnames(parameter_outputs)[j]
	  extract_summary[j,2] <- as.numeric(quantile(parameter_outputs[,j],probs = 0.025, na.rm = TRUE))
	  extract_summary[j,3] <- as.numeric(quantile(parameter_outputs[,j],probs = 0.25, na.rm = TRUE))
	  extract_summary[j,4] <- mean(parameter_outputs[,j],na.rm=TRUE)
	  extract_summary[j,5] <- as.numeric(quantile(parameter_outputs[,j],probs = 0.75, na.rm = TRUE))
	  extract_summary[j,6] <- as.numeric(quantile(parameter_outputs[,j],probs = 0.975, na.rm = TRUE))
	  }


    	params_input_output <- list(sim_output_name = substitute(sim_output), 
					n_boots = n_boots, 
					samp_size = samp_size, 
					sample_type = sample_type, 
					supp_large = supp_large, 
					supp_large_n_per_bin = supp_large_n_per_bin, 
					supp_small = supp_small, 
					supp_small_n_per_bin = supp_small_n_per_bin,
					supp_min_length = supp_min_length, 
					constrained = constrained, 
					t0 = t0,
					SD_L_const = SD_L_const,
					save_bootstraps = save_bootstraps, 
					Amax = Amax , 
					age_max = age_max, 
					Lbin_width = Lbin_width,
					boots_nls_fail = count_error)

    	if (save_bootstraps == TRUE) {

		return(list(list_boot_samps = list_boot_samps,
			list_boot_preds = list_boot_preds,
			list_boot_mods = list_boot_mods, 
			list_boot_CVs = list_boot_CVs,
			list_boot_SDs = list_boot_SDs,
			parameter_outputs = parameter_outputs,
			params_input_output = params_input_output,
			simulation_params = sim_output$parameters,
			parameter_summary_all_boots = extract_summary,
			list_some_boot_samps = list_some_boot_samps))
		}

	if (save_bootstraps == FALSE) {

		return(list(list_boot_samps = 'NA',
			list_boot_preds = list_boot_preds,
			list_boot_mods = list_boot_mods, 
			list_boot_CVs = list_boot_CVs,
			list_boot_SDs = list_boot_SDs,
			parameter_outputs = parameter_outputs,
			params_input_output = params_input_output,
			simulation_params = sim_output$parameters,
			parameter_summary_all_boots = extract_summary,
			list_some_boot_samps=list_some_boot_samps))
		}

#  print out runtime
	run_time_calc <- Sys.time() - time_start
	print(run_time_calc)

  } 		# ---------- END FUNCTION





# --------------------------------------------------------------------------------------------------------------------------------------
# ------- PLOT FUNCTIONS TO SHOW RESULTS  -----------------------
#	
#
#
#

LH_plot <- function(sample_output, all_plots = TRUE) {

 look_at_me <- sample_output 

 dev.new()

#  ---------- plot #: selectivity
 if(all_plots == TRUE) {
  par(omi=c(0,0,0,0)) #set outer margins
  par(mai=c(1,0.8, 0.8, 0.4)) #set inner margins

  main_title <- paste(look_at_me$params_input_output$sim_output_name, "Selectivity at Length", "Min=",
				look_at_me$simulation_params$mincat, "Max=",look_at_me$simulation_params$maxcat,
				sep=" ")

  xmax = round((look_at_me$simulation_params$Linf + 0.20*look_at_me$simulation_params$Linf),0)
  plot(seq(0,xmax ,1),(pmin((1-pnorm(look_at_me$simulation_params$mincat,seq(0,xmax ,1),look_at_me$simulation_params$catsd)),
				pnorm(look_at_me$simulation_params$maxcat,seq(0,xmax ,1),look_at_me$simulation_params$maxcatsd))),
       type="l",xlab="Fish Length",ylab="Fishery Selectivity", main= main_title)
	abline(v=look_at_me$simulation_params$mincat, lty=2, col="blue")
	abline(v=look_at_me$simulation_params$maxcat, lty=2, col="blue")

	readline(prompt="Press [enter] in R console for next figure")
 }


#  ---------- plot #: natural mortality at age
 if(all_plots == TRUE) {
  main_title <- paste("Natural Mortality at Age, Moverall =",
				look_at_me$simulation_params$M, "Solve M1=", round(look_at_me$simulation_params$M1,2), 
				sep=" ")

  par(omi=c(0,0,0,0)) #set outer margins
  par(mai=c(1,0.8, 0.8, 0.4)) #set inner margins

  plot(get(as.character(look_at_me$params_input_output$sim_output_name))$Avg_age$Ages,
	get(as.character(look_at_me$params_input_output$sim_output_name))$Avg_age$M_age,
	xlab="Age", ylab="M at Age", main=main_title)

  readline(prompt="Press [enter] in R console for next figure")
}

#  ---------- plot #: pop age vs. length
 if(all_plots == TRUE) {
  par(omi=c(0,0,0,0)) #set outer margins
  par(mai=c(1,0.8, 0.8, 0.4)) #set inner margins

  input_plot_ages <- seq(0,look_at_me$params_input_output$Amax,0.1)
  Linf <- get(as.character(look_at_me$params_input_output$sim_output_name))$parameters$Linf
  Linf_sd <- get(as.character(look_at_me$params_input_output$sim_output_name))$parameters$Linf_sd
  L0 <- get(as.character(look_at_me$params_input_output$sim_output_name))$parameters$L0
  L0_sd <- get(as.character(look_at_me$params_input_output$sim_output_name))$parameters$L0_sd
  k <- get(as.character(look_at_me$params_input_output$sim_output_name))$parameters$k
  a0 = (log(1-(L0/Linf)))/k
  pred_lengths = Linf *(1-exp(-k*(input_plot_ages-a0)))

  plot(get(as.character(look_at_me$params_input_output$sim_output_name))$population$age,
	get(as.character(look_at_me$params_input_output$sim_output_name))$population$length,
	xlab="Age", ylab="Length", main="Population Age vs. Length w/ simulation input growth")
  lines(input_plot_ages, pred_lengths, lty=1, col="blue", lwd=2)

  readline(prompt="Press [enter] in R console for next figure")
}

# ---------- plot #: population prop at age and harvest histograms
 if(all_plots == TRUE) {
  age_breaks = seq(0,look_at_me$params_input_output$Amax,1)
  maxL_pop <- max(get(as.character(look_at_me$params_input_output$sim_output_name))$population$length)
  length_breaks = seq(0,maxL_pop+look_at_me$params_input_output$Lbin_width,look_at_me$params_input_output$Lbin_width)

  par(mfcol=c(2,2))
  par(omi=c(0.3,0.3,0.3,0.1)) #set outer margins
  par(mai=c(0.4,0.4, 0.3, 0)) #set inner margins

  hist(get(as.character(look_at_me$params_input_output$sim_output_name))$population$length,
	breaks=length_breaks, include.lowest=TRUE, right=FALSE,plot=TRUE,
	main="Population Lengths",xlab="Length",ylab="Frequency")

  hist(get(as.character(look_at_me$params_input_output$sim_output_name))$harvest$length,
	breaks=length_breaks, include.lowest=TRUE, right=FALSE,plot=TRUE,
	main="Harvest Lengths",xlab="Length",ylab="Frequency")
  mtext(side=1, "Length", line=2.5,outer=FALSE)

  hist(get(as.character(look_at_me$params_input_output$sim_output_name))$population$age,
	breaks=age_breaks, include.lowest=TRUE, right=FALSE,plot=TRUE,
	main="Population Ages",xlab="Length",ylab="Frequency")

  hist(get(as.character(look_at_me$params_input_output$sim_output_name))$harvest$age,
	breaks=age_breaks, include.lowest=TRUE, right=FALSE,plot=TRUE,
	main="Harvest Ages",xlab="Length",ylab="Frequency")
  mtext(side=1, "Age", line=2.5,outer=FALSE)

  mtext(side=2, "Frequency",line=0,outer=TRUE)
  main_title <- paste(look_at_me$params_input_output$sim_output_name,
				"population and harvest histograms", sep=" ")
  mtext(side=3, main_title, line=0,outer=TRUE)

  readline(prompt="Press [enter] in R console for next figure")
}

#  ---------- plot #: A few bootstraps

  par(mfrow=c(3,3))
  par(omi=c(0.3,0.3,0.3,0.1)) #set outer margins
  par(mai=c(0.2,0.2, 0.3, 0)) #set inner margins

  for (j in 1:9) {
	sample_size <- sum(look_at_me$list_some_boot_samps[[j]]$counts)
	title_temp <- paste("samp boot",j,"n=",sample_size,sep=" ")
	plot(look_at_me$list_some_boot_samps[[j]], main = title_temp)
   }
	mtext(side=1,"Length",line=1,outer=TRUE)
	mtext(side=2, "Frequency",line=1,outer=TRUE)
	main_title <- paste(look_at_me$params_input_output$sim_output_name,
				look_at_me$params_input_output$sample_type,
				"supp sm", look_at_me$params_input_output$supp_small,
				"supp lg", look_at_me$params_input_output$supp_large,
				":   A Few Bootstraps", sep=" ")
	mtext(side=3, main_title, line=0,outer=TRUE)


  }	#----------- end plot function


# make a function workspace
# rm(list=ls()[3:11])

# save.image("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_Functions\\MUS_Functions_updated_29Mar.RData")
# load("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_Functions\\MUS_Functions.RData")















#  ------------------------------------------------------------------------------------------------------