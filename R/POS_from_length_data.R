#  load packages
# rm(list=ls())
# library(reshape)
# library(dplyr)
# library(ggplot2)
# library(magrittr)
# #library(assertthat)


##population data - fishery independent data
cam_length<-read.csv("BFISH 2016-2019 Camera Lengths.csv",header = T)
head(cam_length)


paka_dat<-cam_length[cam_length$SPECIES_CD=="PRFI",]
paka_dat$length<-paka_dat$LENGTH_CM
population_true=paka_dat

population_true<-as.data.frame(population_true)

Lbin_width <- 2.5
samp_size <- 450 ##play with sample size run 470, 400, 350, 300
brks<-seq(1.5,80, Lbin_width ) ###need to make breaks same as FOS breaks
#brks <- seq(0,1.1*max(population_true$length),Lbin_width)
#age <- seq(0,Amax,1)


population_true$binL <- cut(population_true[,"length"], breaks=brks, labels=brks[1:(length(brks)-1)],right=F)

population_true <- population_true %>%
  group_by(binL) %>%
  mutate(prop=n()) %>%
  group_by() %>%
  mutate(n=sum(n())) %>%
  mutate(propL=prop/n)					#str(population_true)	head(population_true)

population_true <- mutate(population_true, binL_num = as.character(binL))
population_true$binL_num <- as.numeric(population_true$binL_num)		#population_true[1:500,]

population_true_df <- as.data.frame(population_true)

##available otolith data
otos<-read.csv("MHI PRFI data.csv")
head(otos)
otos$length=otos$FL
population_harv= otos

#create sample length bins
population_harv$binL <- cut(population_harv[,"length"], breaks=brks, labels=brks[1:(length(brks)-1)],right=F)
#join propL from population to sample data

head(population_true_df)
population_true_df<-select(population_true_df, binL, propL)
population_true_df <- unique(population_true_df[c("binL","propL")])
str(population_true_df)
str(population_harv)

population_harv<-select(population_harv, Fish, length, binL )

population_harv$binL<-as.character(population_harv$binL)
population_harv$binL<-as.numeric(population_harv$binL)
population_true_df$binL<-as.character(population_true_df$binL)
population_true_df$binL<-as.numeric(population_true_df$binL)

population_harv<-dplyr::left_join(population_harv,population_true_df, by="binL")
str(population_harv)
population_harv$binL_num=population_harv$binL

basic_POS_props <- unique(population_harv[c("binL","propL")])		#sum(basic_POS_props$prop1)
basic_POS_props <- basic_POS_props[order(basic_POS_props$binL),]		#str(basic_POS_samps)
basic_POS_samps <- dplyr::mutate(basic_POS_props, basic_POS_n = round((samp_size*propL),0),
                                 binL_num = as.numeric(as.character(binL)))

sum(basic_POS_samps$basic_POS_n)

#corrects to get your exact sample size
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


sum(basic_POS_samps$basic_POS_n) #check to make sure = sample size
#basic_POS_samps gives the numbers per length bin for POS


# ***************************************
#	sample_n is causing the problem:
#  		samples are drawn 1 at a time, independent of previous samples. Since the length composition of the camera survey
#		is kind of flat, most bins have a nearly equal probability of being sampled, so there will be a lot of variability
#		each time you run sample_n.
#   Use the updated POS sample routine (using the POS_build df) from the March30 bug fix,
#		modified to account for instances when there aren't enough samples in a bin
#			(take all samples in that bin, do not make up elsewhere so total N will be < target).
#   Note also that the histogram still looked a little off because must use hist argument right = FALSE
#		otherwise the breaks will be included in the lower bin (i.e. L = 16 would be in the 14-16 bin.)


#head(population_harv)


POS_build <- population_harv[1,]
for (pp in 1:nrow(basic_POS_samps)) {
  if(basic_POS_samps$basic_POS_n[pp]>0){
    sample_me_binL <- basic_POS_samps$binL_num[pp]
    sample_me_n	<- basic_POS_samps$basic_POS_n[pp]
    sample_me_pop_harv <- subset(population_harv, binL_num == sample_me_binL)
    if(sample_me_n <= nrow(sample_me_pop_harv)) {
      sample_me <- sample_n(sample_me_pop_harv, sample_me_n, replace=FALSE)
    } else
    {
      sample_me <- sample_me_pop_harv
    }
    POS_build <- rbind(POS_build, sample_me)
  }
}
#drop row 1 of POS_build
POS <- POS_build[-1,]		#str(POS_build)

# how many samples did we get??
nrow(POS)		  #384 with 2.5 cm bins for N=400, 431 for N=450

# make a hist to check
brks
#compare POS sample to population distribution
par(mfrow=c(2,1))
population_true$binL<-as.numeric(population_true$binL)
population_true<-population_true %>%
  subset(binL>=4)
population_true$binL<-as.factor(population_true$binL)
hist_cam<-hist(population_true$length,breaks=seq(4,80,2.5), right = FALSE, main="Camera & POS Length Frequency", xlab="Fork Length (cm)")
hist_POS<-hist(POS$length, breaks=seq(4,80,2.5), right = FALSE, add=TRUE, col="lightblue")

all_otos<-population_harv %>%
  dplyr::group_by(binL) %>%
  dplyr::mutate(available_nbin=n())

hist_otos<-hist(all_otos$length, breaks=seq(4,80,2.5), right = FALSE, main="Available Otoliths", xlab="Fork Length (cm)" ,col="orange")

hist_cam <- hist(population_true$length, breaks=seq(4,80,2.5), right = FALSE)
hist_POS <- hist(POS$length, breaks=seq(4,80,2.5), right = FALSE)
hist_otos <- hist(all_otos$length, breaks=seq(4,80,2.5), right = FALSE)

length(hist_cam$counts)
length(hist_cam$counts)
length(hist_otos$counts)
compare_hists <- data.frame(binL = hist_cam$breaks[1:30], camera_counts = hist_cam$counts,
                            POS_camera_counts = hist_POS$counts, otolith_counts = hist_otos$counts, sample_size=samp_size)
#compare_hists_300<- compare_hists
#compare_hists_350<- compare_hists
#compare_hists_400<- compare_hists
#compare_hists_450<- compare_hists

POS_target <- select(basic_POS_samps, binL, basic_POS_n)
#compare_hists <- merge(x=compare_hists, y=POS_target, by= 'binL', all.x = TRUE)


##if already aged sum of the otoliths and need to transition to POS- uses propL (proportion per length bin) from "population" i.e. length data
aged<-read.csv("PRFI MHI ages.csv")			#head(aged)
# aged<-read.csv("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\PRFI_POS\\PRFI_POS\\PRFI MHI ages.csv")
aged$length=aged$FL
aged$binL <- cut(aged[,"length"], breaks=brks, labels=brks[1:(length(brks)-1)],right=F)
aged$binL<-as.factor(aged$binL)
population_true_df$binL<-as.factor(population_true_df$binL)
aged_POS<-dplyr::left_join(aged,population_true_df, by="binL") ##add proportion from population to subsample

aged_summ<-aged %>%
  group_by(binL)%>%
  summarise(tot_bin=n())

aged_POS<-aged_POS %>%
  dplyr::group_by(binL) %>%
  dplyr::mutate(POS_n=round(propL*samp_size), sample_size=samp_size) #number per length bin under POS for a set sample size

aged_POS<-aged_POS %>%
  dplyr::group_by(binL) %>%
  dplyr::mutate(nbin=n())

summary<-unique(aged_POS[c("binL","propL", "nbin", "POS_n", "sample_size")])

summary_temp<-summary %>%
  select(binL, sample_size, aged_otos=nbin)

#str(all_sample_sizes)
#compare_hists_300$binL<-as.factor(compare_hists_300$binL)
#compare_hists_300<-left_join(compare_hists_300, summary_temp)

#str(all_sample_sizes)
#compare_hists_350$binL<-as.factor(compare_hists_350$binL)
#compare_hists_350<-left_join(compare_hists_350, summary_temp)

#str(all_sample_sizes)
#compare_hists_400$binL<-as.factor(compare_hists_400$binL)
#compare_hists_400<-left_join(compare_hists_400, summary_temp)

str(all_sample_sizes)
compare_hists_450$binL<-as.factor(compare_hists_450$binL)
compare_hists_450<-left_join(compare_hists_450, summary_temp)

all_sample_sizes<-rbind(compare_hists_300,compare_hists_350, compare_hists_400, compare_hists_450)
write.csv(all_sample_sizes, "POS_sample_sizes.csv")


all_sample_sizes_temp<-all_sample_sizes%>%
  mutate(needed_per_binL=POS_camera_counts-aged_otos)

write.csv(all_sample_sizes_temp, "POS_sample_sizes_needed.csv")

str(all_sample_sizes_temp)
total_otos<-subset(all_sample_sizes_temp, needed_per_binL>0)

total_otos<- total_otos%>%
  group_by(sample_size) %>%
  summarise(additional_otos=sum(needed_per_binL))
total_otos_all_years<-total_otos
total_otos_all_years$years<-"all"
write.csv(total_otos_all_years, "total_otos_all_years.csv")

summary_available<-unique(all_otos[c("binL","available_nbin")])
summary_available$binL<-as.factor(summary_available$binL)
summary<-left_join(summary, summary_available, by="binL")
summary<-summary%>%
  dplyr::mutate(needed=POS_n-nbin, sample_size=samp_size)


##how many more otoliths need to be sectioned?
total_n=summary %>%
  subset(needed>0)
total_n<-sum(total_n$needed)
#2.5cm bins 46 for N=300, 66 for N=350, 99 for N=400, 114 for N=450 (431)


##what size bins are there not enough available samples
temp<-summary %>%
  dplyr::group_by(binL)%>%
  subset(needed>available_nbin) #note for sample size of 400 your total sample size will be 393 because missing 7 samples from 9cm length bin



###


save.image("PRFI_POS_camera.RData")



#########population BFISH research fishing lengths########
##population data - fishery independent data
bfish<-read.csv("BFISH 2016-2019 Research Fishing Lengths.csv",header = T)
head(bfish)


paka_dat<-bfish[bfish$SPECIES_CD=="PRFI",]
paka_dat$length<-paka_dat$LENGTH_CM
population_true=paka_dat
population_harv=paka_dat

Lbin_width <- 2
samp_size <- 400

population_true <- population_true[complete.cases(population_true$length),]
brks <- seq(0,1.1*max(population_true$length),Lbin_width)
#age <- seq(0,Amax,1)
population_true$binL <- cut(population_true[,"length"], breaks=brks, labels=brks[1:(length(brks)-1)],right=F)

population_true <- population_true %>%
  group_by(binL) %>%
  mutate(prop=n()) %>%
  group_by() %>%
  mutate(n=sum(n())) %>%
  mutate(propL=prop/n)					#str(population_true)	head(population_true)

population_true <- mutate(population_true, binL_num = as.character(binL))
population_true$binL_num <- as.numeric(population_true$binL_num)		#population_true[1:500,]

population_true_df <- as.data.frame(population_true)

##available otolith data
otos<-read.csv("MHI PRFI data.csv")
head(otos)
otos$length=otos$FL
population_harv= otos

#create sample length bins
population_harv$binL <- cut(population_harv[,"length"], breaks=brks, labels=brks[1:(length(brks)-1)],right=F)
#join propL from population to sample data

head(population_true_df)
population_true_df<-select(population_true_df, binL, propL)
population_true_df <- unique(population_true_df[c("binL","propL")])
str(population_true_df)
str(population_harv)

population_harv<-select(population_harv, Fish, length, binL )

population_harv$binL<-as.character(population_harv$binL)
population_harv$binL<-as.numeric(population_harv$binL)
population_true_df$binL<-as.character(population_true_df$binL)
population_true_df$binL<-as.numeric(population_true_df$binL)

population_harv<-dplyr::left_join(population_harv,population_true_df, by="binL")
str(population_harv)
population_harv$binL_num=population_harv$binL

basic_POS_props <- unique(population_harv[c("binL","propL")])		#sum(basic_POS_props$prop1)
basic_POS_props <- basic_POS_props[order(basic_POS_props$binL),]		#str(basic_POS_samps)
basic_POS_samps <- dplyr::mutate(basic_POS_props, basic_POS_n = round((samp_size*propL),0),
                                 binL_num = as.numeric(as.character(binL)))

sum(basic_POS_samps$basic_POS_n)
basic_POS_samps<-basic_POS_samps[complete.cases(basic_POS_samps$propL),]

#corrects to get your exact sample size
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


sum(basic_POS_samps$basic_POS_n) #check to make sure = sample size
#basic_POS_samps gives the numbers per length bin for POS


# ***************************************
#	sample_n is causing the problem:
#  		samples are drawn 1 at a time, independent of previous samples. Since the length composition of the camera survey
#		is kind of flat, most bins have a nearly equal probability of being sampled, so there will be a lot of variability
#		each time you run sample_n.
#   Use the updated POS sample routine (using the POS_build df) from the March30 bug fix,
#		modified to account for instances when there aren't enough samples in a bin
#			(take all samples in that bin, do not make up elsewhere so total N will be < target).
#   Note also that the histogram still looked a little off because must use hist argument right = FALSE
#		otherwise the breaks will be included in the lower bin (i.e. L = 16 would be in the 14-16 bin.)


#head(population_harv)


POS_build <- population_harv[1,]
for (pp in 1:nrow(basic_POS_samps)) {
  if(basic_POS_samps$basic_POS_n[pp]>0){
    sample_me_binL <- basic_POS_samps$binL_num[pp]
    sample_me_n	<- basic_POS_samps$basic_POS_n[pp]
    sample_me_pop_harv <- subset(population_harv, binL_num == sample_me_binL)
    if(sample_me_n <= nrow(sample_me_pop_harv)) {
      sample_me <- sample_n(sample_me_pop_harv, sample_me_n, replace=FALSE)
    } else
    {
      sample_me <- sample_me_pop_harv
    }
    POS_build <- rbind(POS_build, sample_me)
  }
}
#drop row 1 of POS_build
POS <- POS_build[-1,]		#str(POS_build)

# how many samples did we get??
nrow(POS)		#391		View(POS)

# make a histo to check

#compare POS sample to population distribution
par(mfrow=c(2,1))
hist(population_true$length, breaks=seq(0,80,2), right = FALSE, main="Camera & POS Length Frequency", xlab="Fork Length (cm)")
hist(POS$length, breaks=seq(0,80,2), right = FALSE, add=TRUE, col="lightblue")

all_otos<-population_harv %>%
  dplyr::group_by(binL) %>%
  dplyr::mutate(available_nbin=n())

hist(all_otos$length, breaks=seq(0,80,2), right = FALSE, main="Available Otoliths", xlab="Fork Length (cm)" ,col="orange")

hist_bfish <- hist(population_true$length, breaks=seq(0,80,2), right = FALSE)
hist_POS <- hist(POS$length, breaks=seq(0,80,2), right = FALSE)
hist_otos <- hist(all_otos$length, breaks=seq(0,80,2), right = FALSE)

compare_hists_bfish <- data.frame(binL = hist_cam$breaks[1:40], camera_counts = hist_cam$counts,
                                  POS_counts = hist_POS$counts, otolith_counts = hist_otos$counts)

POS_target <- select(basic_POS_samps, binL, basic_POS_n)
compare_hists <- merge(x=compare_hists, y=POS_target, by= 'binL', all.x = TRUE)




##if already aged sum of the otoliths and need to transition to POS- uses propL (proportion per length bin) from "population" i.e. length data
aged<-read.csv("PRFI MHI ages.csv")			#head(aged)
# aged<-read.csv("C:\\Users\\Erin.Bohaboy\\Documents\\MUS_Sample_Design\\PRFI_POS\\PRFI_POS\\PRFI MHI ages.csv")
aged$length=aged$FL
aged$binL <- cut(aged[,"length"], breaks=brks, labels=brks[1:(length(brks)-1)],right=F)
aged$binL<-as.factor(aged$binL)
population_true_df$binL<-as.factor(population_true_df$binL)
aged_POS<-dplyr::left_join(aged,population_true_df, by="binL") ##add proportion from population to subsample

aged_summ<-aged %>%
  group_by(binL)%>%
  summarise(tot_bin=n())

aged_POS<-aged_POS %>%
  dplyr::group_by(binL) %>%
  dplyr::mutate(POS_n=round(propL*samp_size), sample_size=samp_size) #number per length bin under POS for a set sample size

aged_POS<-aged_POS %>%
  dplyr::group_by(binL) %>%
  dplyr::mutate(nbin=n())

summary<-unique(aged_POS[c("binL","propL", "nbin", "POS_n", "sample_size")])



summary_available<-unique(all_otos[c("binL","available_nbin")])
summary_available$binL<-as.factor(summary_available$binL)
summary<-left_join(summary, summary_available, by="binL")
summary<-summary%>%
  dplyr::mutate(needed=POS_n-nbin)


##how many more otoliths need to be sectioned?
total_n=summary %>%
  subset(needed>0)
total_n<-sum(total_n$needed) #95 for a POS sample size of 400,72 for sample size of 350, 48 for a sample size of 300

##what size bins are there not enough available samples
temp<-summary %>%
  dplyr::group_by(binL)%>%
  subset(needed>available_nbin) #note for sample size of 400 your total sample size will be 393 because missing 7 samples from 9cm length bin



###



