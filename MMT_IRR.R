##############################################
##                                          ##
##               MMT Pre-IRR                ##
##                                          ##
##############################################

# Written for the EvoLearn project reliability analyses - specifically for the marshmallow
# task behavioral coding

# Contributors: Lydia Chen, Oskar Burger

## Goal is to have a script which arranges the exported coding from BORIS to allow pre-IRR 
## to be calculated for the marshmallow behavioral and quality checking video coding 

## Identify steps in this script:
## 1) Calculate kappa for categorical codes
## 2) Calculate ICC for duration behaviors - using behavioral category totals for invidual ppts
## 3) Calculate weighted Kappa for point behaviors - using total identified instances of behaviors

## libraries
library(tidyverse)
library(irr)
library(readxl)
library(here)

## clear environment
rm(list =ls())

## Set working directory to location of current script 

dirpath <- here()
setwd(dirpath)
getwd()
## read in datasets
# Read in two separate BORIS aggregate files, one for each coder
TC_irr <-read.csv("TA_MMT_preIRR_TC.csv")
LC_irr <-read.csv("TA_MMT_preIRR_LC.csv")

## Combine datasets into one file 
site_preirr <- bind_rows(TC_irr, LC_irr) %>%
  mutate(behaviour_mod = paste(Behavior, Modifiers,sep="_")) %>% # create behavior modifier pasted variable name
  rename('Duration_s' = 'Duration..s.') %>%
  rename('Start_s' = 'Start..s.') %>%
  rename('Stop_s' = 'Stop..s.')
  ## ^ we may not need to do that, but keeping for now. 


## Look at data
head(site_preirr)

# check column names
names(site_preirr)

view(table(site_preirr$behaviour_mod, useNA = 'always'))

################################
### 1) Categorical codes IRR ###
################################

# Select categorical columns for Kappa analysis and pivot to longer format
site_preirr_eaten = site_preirr %>% 
  select(PID, coder_id, reward_eaten) %>% 
  distinct(.) %>% # the reward_eaten code is repeated for each behavior, so can remove duplicates
  pivot_wider(.,id_cols = c(PID), # previously there was a pivot_longer then a pivot_wider
        names_from = "coder_id", values_from = "reward_eaten") # make longer
head(site_preirr_eaten)

# run kappa on categorical variable `eaten`
irr::kappa2(site_preirr_eaten %>% select(Taylor, Lydia), weight = "unweighted")

# Coding is considered reliable here if Kappa is above 80%, though it should 
# really should be 100% because this code is extremely clear. If not 100%, please check discrepancies.

#################################
### 2) Duration Behaviors ICC ###
#################################

## ICC For sum of duration behaviors ----
# see this doc: https://www.datanovia.com/en/lessons/intraclass-correlation-coefficient-in-r/#how-to-choose-the-correct-icc-forms

# Select duration columns for ICC analysis ----
site_preirr_sums = site_preirr %>%
  select(PID, coder_id, `Behavioral.category`, `Behavior.type`, `Duration_s`, `Start_s`, `Stop_s`) %>% # using behavioural category to make this comparison
  filter(`Behavior.type` == "STATE") %>% #filter out any point behaviors (these will be in the 'mistakes' category)
  mutate(Duration_s = as.numeric(Duration_s)) %>% # convert duration to numeric from character
  group_by(coder_id, PID, `Behavioral.category`) %>% # group by to the following summation is for each coder, for each video, for each behavior cat.
  summarize(timesum = sum(Duration_s, na.rm = T)) %>% # calculate the sum within that breakdown of groups.
  ungroup()  
# This does give a warning message but it is just introducing NAs for the mutate 
# step as some of the behaviors don't have a duration (start and stop time are the same)

site_preirr_sums_wide = full_join( # this join takes the previous table and 'breaks it in half' and places the j data next to the m data 
  site_preirr_sums %>% filter(coder_id == 'Taylor'),  
  site_preirr_sums %>% filter(coder_id == 'Lydia'), 
  by = c('PID','Behavioral.category'),
  suffix = c("_t","_l")) %>%
  mutate(timesum_t = replace_na(timesum_t, 0), # make sure sums of '0' were counted, not marked as NA
         timesum_l = replace_na(timesum_l, 0),
         timediff = abs(timesum_t - timesum_l)) %>%
  select(-coder_id_t,-coder_id_l, PID, `Behavioral.category`, timesum_t, timesum_l) %>%
  arrange(desc(timediff)) #see biggest differences at top
# view(site_preirr_sums_wide) # uncomment to visually check

# run the ICC on the data: 
iccmmt = irr::icc(site_preirr_sums_wide[,c('timesum_t','timesum_l')], model='oneway', type='consistency',unit='single')
iccmmt # this gives the fulll ICC output. 
iccmmt$value %>% round(.,2) # this gives us the summary output only for the ICC

# Even if this is sufficiently high, lets check the largest differences between the coders.
# Let's start by viewing the largest differences in time summed for each behavior: 
site_preirr_sums_wide %>% filter(timediff >= 20) %>% # differences of 20 seconds or more. this threshold could be changed as needed. 
  view()

# Let's also consider which behavior categories are coming up and how often in these differences of 20 seconds or longer. 
site_preirr_sums_wide %>% filter(timediff >= 20) %>% pull(`Behavioral.category`) %>% table() %>% view()

##############################
### 3) Point Behaviors ICC ###
##############################

## Check reliability of coding of point behaviours by summing them up and comparing between coders
## select the behaviors and start times for each coder and PID

# POINT behaviors
site_preirr_point = site_preirr %>%
  filter(`Behavior.type` == "POINT") %>% # filter out the point behaviors
  select(PID, coder_id, Behavior, `Start_s`) %>%
  mutate(Behavior = as.factor(Behavior)) # added this to make the point behaviors factors 
 
## Identify differences across videos
table(site_preirr_point$PID, site_preirr_point$coder_id)
## Identify differences across error types
table(site_preirr_point$Behavior, site_preirr_point$coder_id)


# .drop = false prevents factors from being dropped
# which means you get the zeros that you need in the counts. 
site_preirr_point_wide = site_preirr_point %>%
  group_by(PID, coder_id, .drop = FALSE) %>%
  count(Behavior) %>% ungroup() %>%
# pivot wider for counts side by side. 
  pivot_wider(.,id_cols = c(PID, Behavior), 
                        names_from = "coder_id", 
                        values_from = "n") %>% 
  mutate(across(Taylor:Lydia, ~ replace_na(.x, replace = 0))) # replace NAs for unobserved cases with 0 

# Calculate weighted kappa (equal weight)
irr::kappa2(site_preirr_point_wide %>% select(Taylor, Lydia), weight = 'squared', sort.levels = TRUE) # calculate kappa 

# To pass - we want this kappa to be 80% or higher

# also, check % agreement 
site_preirr_point_wide %>% 
  mutate(pointagree = (Taylor == Lydia)) %>%
  summarize(propagree = mean(pointagree))

# Number of agree vs disagree
site_preirr_point_wide %>% 
  mutate(pointagree = (Taylor == Lydia)) %>%
  select(pointagree) %>% table() 

site_preirr_point_wide %>% select(Taylor, Lydia) %>% table() 

# We also consider that a weighted Kappa is not always suggested for ordinal variables, and can lead to lower kappas for unbalanced data,
# so we'll also include a couple of other measures here too - and the results of all of these can be evaluated
# in conjunction to determine acceptable reliability

# trying this other technique...
irrCAC::bp.coeff.raw(site_preirr_point_wide %>% select(Taylor,Lydia))

irrCAC::gwet.ac1.raw(site_preirr_point_wide %>% select(Taylor,Lydia), 
                     weights = 'ordinal')


# write.csv(pointdata, 'TA_MMT_Pre-IRR_PointBehaviours_06AUG2021.csv')



