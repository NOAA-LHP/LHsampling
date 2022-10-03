#'@title LH_sample
#
#'@description A bootstrap sampling routine to estimate life history parameters from fishery catches simulated by simulate_population_harvest(). This function will take n_boots samples (without replacement) from the harvested individuals following either a fixed otolith sampling (FOS) or proportional otolith sampling (POS) strategy. The function then parameterizes the von Bertalanffy growth function and estimates the population coefficient of variation of length at age for each bootstrap sample.

#'@usage LH_sample <- function(sim_output, n_boots, samp_size, sample_type, supp_large = FALSE, supp_large_n_per_bin = 3, supp_small = FALSE, supp_small_n_per_bin = 3, supp_min_length = 2, constrained = FALSE, t0 = 0, SD_L_const = TRUE, save_bootstraps = FALSE, Amax = NULL, age_max = NULL, Lbin_width = 2)
#'@keywords function
#'@details This function will take n_boots samples (without replacement) from the harvested individuals following either a fixed otolith sampling (FOS) or proportional otolith sampling (POS) strategy. The function then parameterizes the von Bertalanffy growth function and estimates the population coefficient of variation of length at age for each bootstrap sample.

#To produce each FOS bootstrap, n individuals (calculated as total sample size, samp_size, divided by the number of length bins represented in the harvest, rounded up to the next integer) are randomly sampled, without replacement, from the harvest for each length bin. If less than n individuals are available in a harvest length bin, then additional samples are randomly drawn from the remaining length bins in the harvest until the total prescribed number of samples (n × number of length bins represented in the harvest) are attained. If greater than samp_size individuals have been collected, individuals within fully filled length bins will be randomly discarded from the sample until samp_size is attained. If supplemental sampling is being implemented (supp_large or supp_small = TRUE) then the specified number of samples per large (supp_large_n_per_bin) or small (supp_small_n_per_bin) are taken randomly (without replacement) from the population. For this step, ‘small’ and ‘large’ length bins are defined as any bins containing individuals in the population where fewer than supp_large_n_per_bin or supp_small_n_per_bin individuals in the FOS sample. After supplemental samples are added, individuals from the most populous length bins will be randomly discarded from the sample until samp_size is attained.
#Each POS bootstrap is produced in much the same way as for FOS. The primary difference is the ‘target’ number n of individuals per length bin L (nL) is calculated based on the proportional distribution of lengths within the entire harvest, where nL is rounded to the nearest integer. If ∑▒〖n_(L )≠samp_size〗 then nL will be updated by adding or subtracting from the most populous length bin. Making adjustments to the number of samples targeted in the most populous length bin minimizes effects on the POS scheme from adjusting for the desired samp_size. For each POS bootstrap, nL individuals are randomly sampled, without replacement, from the harvest for each length bin. If supplemental sampling is being implemented (supp_large or supp_small = TRUE) then the specified number of samples per large (supp_large_n_per_bin) or small (supp_small_n_per_bin) are taken randomly (without replacement) from the population. For this step, ‘small’ and ‘large’ length bins are defined as any bins containing individuals in the population with fewer than  supp_large_n_per_bin or supp_small_n_per_bin in the POS sample. After supplemental samples are added, individuals from the non-supplemental length bins will be randomly discarded from the sample until samp_size is attained.
#For each bootstrap sample, the coefficient of variation of length at age (CVa) is calculated (given ≥ 2 individuals per age). The coefficient of variation of length at age 0 (CVa0) and age_max (CVage_max) are extrapolated assuming a linear relationship between age and CVa (SD_L_const = FALSE) or standard deviation of length at age (SD_L_const = TRUE). The slope and intercept are parameterized using nls(). If nls() fails to converge, then the average variance in length at age, over all ages, is used.
#For each bootstrap sample, the von Bertalanffy growth function is parameterized using nls(), with t0 (where t0 = a0 + 1) either estimated or fixed at the user specified value.

#L_a=L_∞ (1-e^(-k(a-a_0 ) ))
#and
#a_0=(Ln(1-L_0/L_∞))/k

#The total number of bootstrap samples for which nls() did not converge is reported out to the R console.



#Paramaters for LH_sample()
#'@param sim_output output from simulate_population_harvest()
#'@param n_boots number of bootstraps for von Bertalanffy growth function
#'@param sample_type The sampling strategy to be used, either proportional otolith sampling (‘POS’) or fixed otolith sampling (‘FOS’)
#'@param supp_large TRUE / FALSE specifying whether supplemental samples will be collected from large length bins
#'@param supp_large_n_per_bin The number of samples per length bin to be collected from large bins (ignored if supp_large = FALSE)
#'@param supp_small TRUE / FALSE specifying whether supplemental samples will be collected from small length bins
#'@param supp_small_n_per_bin The number of samples per length bin to be collected from small bins (ignored if supp_small = FALSE)
#'@param supp_min_length The minimum length fish that could be collected from the wild fish population
#'@param constrained TRUE / FALSE specifying whether theoretical time at length zero (t0) should be estimated
#'@param t0 If constrained = TRUE, the fixed value for t0 (typically 0)
#'@param SD_L_const TRUE / FALSE describing assumptions of population variance in length at age. If TRUE, then standard deviation (√𝜎𝜎2) of length at age is assumed a linear function of age. If FALSE, then the coefficient of variation of length at age is assumed a linear function of age.
#'@param save_bootstraps TRUE / FALSE specifying whether all bootstrap samples will be included in the function output
#'@param Amax Maximum longevity (years). If not specified, this value is taken from sim_output.
#'@param age_max An arbitrary age selected to represent “old” fish (years). If not specified, this value is taken from sim_output.
#'@param Lbin_width The width of each length bin (cm).

#'@example S1_A9 <- LH_sample(sim_output = S1_Auric_lowF, n_boots = 1000, samp_size = 300, sample_type = 'POS', supp_large = FALSE, supp_small = FALSE, constrained = FALSE, save_bootstraps = TRUE,  Lbin_width = 2)



#'@export


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
#	Update June10 2021
#	  If user does not specify Amax or age_max arguments, they will be taken from the sim_output object
#	  If user specifies Amax or age_max, the specified value will be used with a message and a reminder of the value
#		used in the sim_output object to notify the user whether the values match.
# -------------------------------------------------------------------------------

#  -----------------  arguments, typical values
#	sim_output <- S6_Auric_highF		# output of the simulate_population_harvest function
#	    n_boots <- 100			# number of bootstraps
#   	samp_size <- 305			# sample size for each bootstrap
#	    sample_type <- 'POS'		# define as 'POS' or 'FOS'

#   	supp_large <- FALSE		# do we want to supplement with large fish?
#   	supp_large_n_per_bin <- 3	# number supplemental samples per large bin
#   	supp_small <- FALSE		# do we want to supplement with small fish?
#   	supp_small_n_per_bin <- 3	# number supplemental samples per small bin
#   	supp_min_length <- 10		# for supplemental small sampling, what is the smallest fish (cm) we could get?

#   	constrained <- FALSE		# is t0 constrained?
#   	t0 <- 0				# if constrainted, define t0

#   	save_bootstraps <- FALSE	# should all bootstrap sample draws be saved? if n_boots or samp_size are large, this should be false
#   	Amax <- NULL			# maximum theoretical age (prop population input parameter)
#   	age_max <- NULL			# choose age_max (the plus group for pop dynamics) a priori
#   	Lbin_width <- 2			# length bin width

# options(warn=0)

#  -----------------  BEGIN READ IN FUNCTION

LH_sample <- function(sim_output, n_boots, samp_size, sample_type, supp_large = FALSE, supp_large_n_per_bin = 3,
                      supp_small = FALSE, supp_small_n_per_bin = 3, supp_min_length = 2, constrained = FALSE,
                      t0 = 0, SD_L_const = TRUE, save_bootstraps = FALSE, Amax = NULL, age_max = NULL, Lbin_width = 2) {

  #  Preliminaries:
  #  Get system time
  time_start <- Sys.time()
  print(time_start)

  #  define population_true and population_harvest
  population_true <- sim_output$population
  population_harv <- sim_output$harvest

  #  extract Amax and age_max from the sim_output
  #  if Amax and age_max are defined in LH_sample function arguments, use those but issue a message
  if(!is.null(Amax)) {
    print(paste("Amax is user specified =", Amax, sep=" "))
    print(paste("Amax used in population simulation =", sim_output$parameters$Amax, sep=" "))
  } else
  {
    Amax <- sim_output$parameters$Amax
  }

  if(!is.null(age_max)) {
    print(paste("age_max is user specified =", age_max, sep=" "))
    print(paste("age_max used in population simulation =", sim_output$parameters$age_max, sep=" "))
  } else
  {
    age_max <- sim_output$parameters$age_max
  }

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
      use_sample <- POS			#head(use_sample)		#str(use_sample)	use_sample <- POS[,1:3]
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

      CV_age_boot <- list_boot_CVs[[i]]			# CV_age_boot <- CV_boot
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

