#'@title LH_plot
#
#'@description A function that produces plots from the output of LH_sample().

#'@usage LH_plot <- function(sample_output, output_type = 'none')

#Paramaters for LH_sample()
#'@param sample_output Output from LH_sample()
#'@param output_type How plots are written and saved: ‘none’ displays in R graphics device only, ’pdf’ produces a single .pdf with all plots, and ‘png' produces a separate .png for each plot.

#'@keywords plot
#'@details This function outputs plots to the R graphics device, requiring the user press [enter] in the R console to advance to the next plot. Users are encouraged to examine each plot to confirm the characteristics of the simulated population, fishing fleet, and bootstrap samples match expectations.

#Plots:
#Selectivity at length as specified in the initial population simulation
#Natural mortality at age
#Scatterplot of population length at age, overlaid with average length at age as expected from the von Bertalanffy growth parameters passed from simulate_population_harvest(). The scatterplot is limited to a random sample of 50,000 individuals from the population.
#Histograms of frequency distributions for population length, harvest length, population age, and harvest age.
#Histograms of number of individuals per length bin for 9 randomly chosen bootstrap samples.


#'@example LH_plot(S1_A9, output_type = ‘pdf’)
#'@export



# --------------------------------------------------------------------------------------------------------------------------------------
# ------- PLOT FUNCTIONS TO SHOW RESULTS  -----------------------
#
#
#	June 10: Adding some more plots and options
#


#  -----------------  arguments, typical values
#	sample_output <- S4_A2		# output of the LH_sample function
#	output_type <- 'pdf'		# if = 'none' then plots are produced in the R graphics device only.
#					 	# 	if = 'png' or 'pdf' then the corresponding file will also be produced and saved in the
#						#	current working directory


#  ----- BEGIN FUNCTION ------------

LH_plot <- function(sample_output, output_type = 'none') {

  look_at_me <- sample_output

  dev.new()

  #  ---------- plot #1: selectivity

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

  readline(prompt="Press [enter] in R console for next figure ")

  #  ---------- plot #2: natural mortality at age

  main_title <- paste("Natural Mortality at Age, Moverall =",
                      look_at_me$simulation_params$M, "Solve M1=", round(look_at_me$simulation_params$M1,2),
                      sep=" ")

  par(omi=c(0,0,0,0)) #set outer margins
  par(mai=c(1,0.8, 0.8, 0.4)) #set inner margins

  plot(get(as.character(look_at_me$params_input_output$sim_output_name))$Avg_age$Ages,
       get(as.character(look_at_me$params_input_output$sim_output_name))$Avg_age$M_age,
       xlab="Age", ylab="M at Age", main=main_title)

  readline(prompt="Press [enter] in R console for next figure ")

  #  ---------- plot #3: pop age vs. length (max N = 50,000)

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

  sim_output_pop_age_length <- data.frame(age = get(as.character(look_at_me$params_input_output$sim_output_name))$population$age,
                                          length = get(as.character(look_at_me$params_input_output$sim_output_name))$population$length)
  if (nrow(sim_output_pop_age_length)>50000) {
    plot_me <- sample_n(sim_output_pop_age_length, size = 50000,replace=FALSE)
  } else {
    plot_me <- sim_output_pop_age_length
  }

  plot(plot_me$age,
       plot_me$length,
       xlab="Age", ylab="Length", main="Population Age vs. Length w/ simulation input growth")
  lines(input_plot_ages, pred_lengths, lty=1, col="blue", lwd=2)

  readline(prompt="Press [enter] in R console for next figure ")


  # ---------- plot #4: population prop at age and harvest histograms

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

  readline(prompt="Press [enter] in R console for next figure ")


  #  ---------- plot #5: A few bootstraps

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


  #----------- if user wants to save outputs:

  # --- write pdf outputs
  if(output_type == 'pdf') {
    get_time <- Sys.time()
    timestamp <- paste0(substr(get_time,1,4),substr(get_time,6,7),substr(get_time,9,10),
                        substr(get_time,12,13),substr(get_time,15,16),substr(get_time,18,19))
    pdf(paste0("LH_plot_",timestamp,".pdf"))
    # repeat all graphs
    #  ---------- plot #1: selectivity
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
    #  ---------- plot #2: natural mortality at age
    main_title <- paste("Natural Mortality at Age, Moverall =",
                        look_at_me$simulation_params$M, "Solve M1=", round(look_at_me$simulation_params$M1,2),
                        sep=" ")
    par(omi=c(0,0,0,0)) #set outer margins
    par(mai=c(1,0.8, 0.8, 0.4)) #set inner margins
    plot(get(as.character(look_at_me$params_input_output$sim_output_name))$Avg_age$Ages,
         get(as.character(look_at_me$params_input_output$sim_output_name))$Avg_age$M_age,
         xlab="Age", ylab="M at Age", main=main_title)
    #  ---------- plot #3: pop age vs. length (max N = 50,000)
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
    sim_output_pop_age_length <- data.frame(age = get(as.character(look_at_me$params_input_output$sim_output_name))$population$age,
                                            length = get(as.character(look_at_me$params_input_output$sim_output_name))$population$length)
    if (nrow(sim_output_pop_age_length)>50000) {
      plot_me <- sample_n(sim_output_pop_age_length, size = 50000,replace=FALSE)
    } else {
      plot_me <- sim_output_pop_age_length
    }
    plot(plot_me$age,
         plot_me$length,
         xlab="Age", ylab="Length", main="Population Age vs. Length w/ simulation input growth")
    lines(input_plot_ages, pred_lengths, lty=1, col="blue", lwd=2)
    # ---------- plot #4: population prop at age and harvest histograms
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
    #  ---------- plot #5: A few bootstraps
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

    dev.off()
  }

  # --- write png outputs
  if(output_type == 'png') {
    get_time <- Sys.time()
    timestamp <- paste0(substr(get_time,1,4),substr(get_time,6,7),substr(get_time,9,10),
                        substr(get_time,12,13),substr(get_time,15,16),substr(get_time,18,19))

    # repeat all graphs, define seperate png before each

    #  ---------- plot #1: selectivity
    png(paste0("1_selex_",timestamp,".png"))
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
    dev.off()

    #  ---------- plot #2: natural mortality at age
    png(paste0("2_MatAge_",timestamp,".png"))
    main_title <- paste("Natural Mortality at Age, Moverall =",
                        look_at_me$simulation_params$M, "Solve M1=", round(look_at_me$simulation_params$M1,2),
                        sep=" ")

    par(omi=c(0,0,0,0)) #set outer margins
    par(mai=c(1,0.8, 0.8, 0.4)) #set inner margins

    plot(get(as.character(look_at_me$params_input_output$sim_output_name))$Avg_age$Ages,
         get(as.character(look_at_me$params_input_output$sim_output_name))$Avg_age$M_age,
         xlab="Age", ylab="M at Age", main=main_title)
    dev.off()

    #  ---------- plot #3: pop age vs. length (max N = 50,000)
    png(paste0("3_Pop_AgeLength_",timestamp,".png"))
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

    sim_output_pop_age_length <- data.frame(age = get(as.character(look_at_me$params_input_output$sim_output_name))$population$age,
                                            length = get(as.character(look_at_me$params_input_output$sim_output_name))$population$length)
    if (nrow(sim_output_pop_age_length)>50000) {
      plot_me <- sample_n(sim_output_pop_age_length, size = 50000,replace=FALSE)
    } else {
      plot_me <- sim_output_pop_age_length
    }

    plot(plot_me$age,
         plot_me$length,
         xlab="Age", ylab="Length", main="Population Age vs. Length w/ simulation input growth")
    lines(input_plot_ages, pred_lengths, lty=1, col="blue", lwd=2)
    dev.off()

    # ---------- plot #4: population prop at age and harvest histograms
    png(paste0("4_Pop_Harv_Histograms_",timestamp,".png"))
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
    dev.off()

    #  ---------- plot #5: A few bootstraps
    png(paste0("5_A_Few_Bootstraps_",timestamp,".png"))
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
    dev.off()

  }


}	#----------- end plot function



# make a function workspace
# rm(list=ls()[3:11])

# save.image("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_Functions\\MUS_Functions_updated_29Mar.RData")
# load("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\MUS_Functions\\MUS_Functions.RData")





