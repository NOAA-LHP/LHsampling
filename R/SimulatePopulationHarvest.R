#'@title Simulate Population Harvest
#
#'@description This is a IBM to generate a population and catch from the population
# to test sampling approaches of POS or FOS, sample size, supplemental sampling,
# and constraining or not constraining t0. It using a von Bertalanffy growth function.
#  Latest update Jun10, 2021
#'@Usage simulate_population_harvest(Linf, Linf_sd, M, Lorenzen, F, mincat,catsd, maxcat, maxcatsd, L0, L0_sd, k, k_sd, Amax, age_max, N)
#
#'@keywords function
#Paramaters for simulate_population_harvest()
#'@param Linf Von Bertalanffy theoretical asymptotic length (cm)
#'@param Linf_sd Population standard deviation of asymptotic length (cm)
#'@param M Instantaneous natural mortality rate (yr-1)
#'@param Lorenzen TRUE / FALSE specifying whether natural mortality is a function of individual length following Lorenzen (Lorenzen, 2000; Lorenzen, 2005)
#'@param F Apical (fully selected) instantaneous fishing mortality rate (yr-1)
#'@param mincat Minimum length at 50% fishery selectivity (cm)
#'@param catsd Slope of the ascending region of selectivity at length (cm), see details
#'@param maxcat Maximum length at 50% fishery selectivity (cm)
#'@param maxcatsd Slope of the descending region of selectivity at length (cm), see details
#'@param L0 Von Bertalanffy length at age 0 (cm)
#'@param L0_sd Population standard deviation of length at age 0 (cm)
#'@param k Von Bertalanffy growth coefficient
#'@param k_sd Population standard deviation of Von Bertalanffy growth coefficient
#'@param Amax Maximum longevity (years)
#'@param age_max An arbitrary age selected to represent “old” fish (years)
#'@param N The number of age 0 fish in each simulated cohort, typical value =100,000

#'@example S1_Auric_lowF <- simulate_population_harvest(Linf = 32.5, Linf_sd = 2.5, M = 0.18, F = 0.09, Lorenzen = TRUE, mincat = 10, catsd = 2.5, maxcat = 200, maxcatsd = 0, L0 = 10, L0_sd = 2.5, k = 0.6, k_sd = 0, Amax = 32, age_max = 15, N = 100000)
#'@details The individual-based simulation model begins with a cohort of N age 0 fish which progress through ages 0 to Amax following the Von Bertalanffy growth function for length at age (La) with individual-specific L∞ and L0 (length at age 0) parameters drawn from a normal distribution of mean = µ and standard deviation = σ [~N(µ, σ)].
#L_(a,i)=L_(∞,i) (1-e^(-K_i (a-a_(0,i) ) ))
#and
#a_(0,i)=(Ln(1-L_(0,i)/L_(∞,i)))/K_i

#L_(a,i) = predicted length (cm) of individual i at the end of age a (years)
#L∞,i = asymptotic length (cm) of individual i
#Ki = growth coefficient for individual i
#a = age in years
#a_0 = the theoretical age at which the fish would have zero length

#It is important to note that fish enter the population simulation as age 0 fish, meaning they are at the end of their first year of life, about to turn 1 year old. At each age, individual fish are subject to death by natural mortality followed by death from harvest (fishing mortality) in a Bernoulli random process with probability pM,i or pF,i, for natural death or harvest, respectively. According to the Baranov catch equation (Quinn and Deriso, 1999), the probability of natural mortality at age a for fish i is
#p_(M_(a,i) )=  M_(a,i)/(F_(a,i)+M_(a,i) ) (1-e^(-F_(a,i)-M_(a,i) ) )

#where Ma,i is natural mortality at age a for fish i and is a function of individual length at age (La,i, calculated from the VBGF, below), following Lorenzen (Lorenzen, 2000; Lorenzen, 2005):

#  M_(a,i)=M1/L_(a,i)

#Parameter M1 describes the relationship between length and natural mortality in the overall population. M1 is estimated within the model by simultaneously solving the following set of equations:

#  {■(M_a=M1/L_a @M_overall=〖-Ln(Survivorship)〗_Amax/A_max @〖Survivorship〗_Amax=N_Amax/N_0 =∏_(a=0)^(A_max)▒e^(-M_a ) =f(M_a,A_max,L_∞,L_0,k))}

#where:
#Ma = population expected natural mortality at age
#La = population mean length at age
#Moverall = population overall natural mortality
#NAmax and N0 = expected number of fish in the population at ages Amax and 0.
#SurvivorshipAmax = average probability of an individual surviving natural mortality to reach Amax.

#The probability of fishing mortality (being harvested) at age a for fish i is

#p_(F_(a,i) )=F_(a,i)/(F_(a,i)+M_(a,i) ) (1-e^(-F_(a,i)-M_(a,i) ) )

#Where Fa,i is the fishing mortality at age a for fish i and is the product of apical (fully selected) fishing mortality (Fʹ) and selectivity at age conditioned on length (SelexL):

#  F_(a,i) 〖=F*Selex〗_L

#SelexL is modeled as the cumulative normal probability density of mean = mincat and standard deviation = mincatsd.

#At each time step, individual fish that experience death by natural mortality are removed from the simulated cohort. Of the fish that survived natural mortality, the individuals that die by fishing mortality are removed from the simulated cohort and set aside as harvested fish. The fish that survived both natural and fishing mortality undergo Von Bertalanffy growth and advance to the next age. Amax + 1 cohorts are created and then 1 age (from a = 0 to Amax) is taken from the survivors and harvest of each cohort to form the simulated population and catch.

#'@export

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

