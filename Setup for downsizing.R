                                                            ##### Setup for downsizing #####

## 1.  Basic setup: ----
setwd("~/Pensioner downsizing")
.libPaths("/Users/Lucille/Documents/Data work/RPackages")


# Installing packages:
#install.packages("plyr", lib = "/Users/Lucille/Documents/Data work/RPackages")
#install.packages("dplyr", lib = "/Users/Lucille/Documents/Data work/RPackages")
#install.packages("tidyr", lib = "/Users/Lucille/Documents/Data work/RPackages")
#install.packages("data.table", lib = "/Users/Lucille/Documents/Data work/RPackages")
#install.packages( "magrittr", lib = "/Users/Lucille/Documents/Data work/RPackages")
#install.packages("lazyeval", lib = "/Users/Lucille/Documents/Data work/RPackages")
#install.packages("gmodels", lib = "/Users/Lucille/Documents/Data work/RPackages") #For CrossTable
#install.packages("xts", lib = "/Users/Lucille/Documents/Data work/RPackages") #For time series manipulations
#install.packages("Hmisc", lib = "/Users/Lucille/Documents/Data work/RPackages") # For describe
#install.packages("xlsx")
#install.packages("rJava")
#install.packages("zoo")
#install.packages("/Users/Lucille/Documents/Data work/HILDA/hildaData_0.2.0.tar.gz", repos=NULL, type="source")
#install.packages("/Users/Lucille/Documents/Data work/HILDA/hildaExtra_0.1.0.tar.gz", repos=NULL, type="source")
library(plyr) 
library(dplyr)
library(tidyr)
library(data.table)
library(magrittr)
library(lazyeval)
library(hildaData)
library(hildaExtra)
library(gmodels)
library(Hmisc)
#Sys.setenv(JAVA_HOME='C:\\Applications\\Stata\\utilities\\java\\macosx-x64\\jre1.8.0_31')
#library(rJava) # Here is the advice I found that finally made it all work: https://stackoverflow.com/questions/30738974/rjava-load-error-in-rstudio-r-after-upgrading-to-osx-yosemite; https://github.com/snowflakedb/dplyr-snowflakedb/wiki/Configuring-R-rJava-RJDBC-on-Mac-OS-X
library(xlsx)
library(zoo)

## 2.  Loading the data ----

wave1<-hildaData::Combined_a150c
wave2<-hildaData::Combined_b150c
wave3<-hildaData::Combined_c150c
wave4<-hildaData::Combined_d150c
wave5<-hildaData::Combined_e150c
wave6<-hildaData::Combined_f150c
wave7<-hildaData::Combined_g150c
wave8<-hildaData::Combined_h150c
wave9<-hildaData::Combined_i150c
wave10<-hildaData::Combined_j150c
wave11<-hildaData::Combined_k150c
wave12<-hildaData::Combined_l150c
wave13<-hildaData::Combined_m150c
wave14<-hildaData::Combined_n150c
wave15<-hildaData::Combined_o150c

# Strip the first letter from each variable name so that it's comparable across samples
names(wave1)<-substring(names(as.data.frame(wave1)),2)
names(wave2)<-substring(names(as.data.frame(wave2)),2)
names(wave3)<-substring(names(as.data.frame(wave3)),2)
names(wave4)<-substring(names(as.data.frame(wave4)),2)
names(wave5)<-substring(names(as.data.frame(wave5)),2)
names(wave6)<-substring(names(as.data.frame(wave6)),2)
names(wave7)<-substring(names(as.data.frame(wave7)),2)
names(wave8)<-substring(names(as.data.frame(wave8)),2)
names(wave9)<-substring(names(as.data.frame(wave9)),2)
names(wave10)<-substring(names(as.data.frame(wave10)),2)
names(wave11)<-substring(names(as.data.frame(wave11)),2)
names(wave12)<-substring(names(as.data.frame(wave12)),2)
names(wave13)<-substring(names(as.data.frame(wave13)),2)
names(wave14)<-substring(names(as.data.frame(wave14)),2)
names(wave15)<-substring(names(as.data.frame(wave15)),2)

# Add a wave variable:
      
      # PREP: For some reason, R is certain that I've got two vars called hhraid in wave1. I've dropped it for now. 
      wave1_v1 <- subset(wave1, select = -c(hhraid) )
      wave1<-wave1_v1
      rm(wave1_v1)
      
wave1 %<>% mutate(wave = 1)
wave2 %<>% mutate(wave = 2)
wave3 %<>% mutate(wave = 3)
wave4 %<>% mutate(wave = 4)
wave5 %<>% mutate(wave = 5)
wave6 %<>% mutate(wave = 6)
wave7 %<>% mutate(wave = 7)
wave8 %<>% mutate(wave = 8)
wave9 %<>% mutate(wave = 9)
wave10 %<>% mutate(wave = 10)
wave11 %<>% mutate(wave = 11)
wave12 %<>% mutate(wave = 12)
wave13 %<>% mutate(wave = 13)
wave14 %<>% mutate(wave = 14)
wave15 %<>% mutate(wave = 15)

# Merge the data files together:
hilda_panel<-rbind.fill(wave1, wave2, wave3, wave4, wave5, wave6, wave7, wave8, wave9, wave10, wave11, wave12, wave13, wave14, wave15)
names(hilda_panel)[1] <- "xwaveid" #I accidentally stripped this variable of its "x" when merging the data. 
hilda_panel %<>% mutate(wealth_wave=ifelse(wave==2 | wave==6 | wave==10 | wave==14, 1, 0))
rm(wave1, wave2, wave3, wave4, wave5, wave6, wave7, wave8, wave9, wave10, wave11, wave12, wave13, wave14, wave15)

## 2.  ID variables: ----

id_vars_person1 <-
  hilda_panel %>%
  filter(hgxid1!="-------") %>%
  mutate(xwaveid_e = as.numeric(hgxid1)+(as.numeric(xwaveid))*10000000,  # "g" stands for generalised. 
  xwaveid_by_wave = as.numeric(xwaveid)+wave/100, 
  original_person_n = 1) %>%
  select(xwaveid_e, wave, xwaveid, hhrhid, xwaveid_by_wave, original_person_n)

id_vars_person2 <-
  hilda_panel %>%
  filter(hgxid2!="-------") %>%
  mutate(xwaveid_e = as.numeric(hgxid2)+(as.numeric(xwaveid))*10000000,
  xwaveid_by_wave = as.numeric(xwaveid)+wave/100, 
  original_person_n = 2) %>%
  select(xwaveid_e, wave, xwaveid, hhrhid, xwaveid_by_wave, original_person_n)

id_vars_person3 <-
  hilda_panel %>%
  filter(hgxid3!="-------") %>%
  mutate(xwaveid_e = as.numeric(hgxid3)+(as.numeric(xwaveid))*10000000, 
         xwaveid_by_wave = as.numeric(xwaveid)+wave/100, 
         original_person_n = 3) %>%
  select(xwaveid_e, wave, xwaveid, hhrhid, xwaveid_by_wave, original_person_n)

id_vars_person4 <-
  hilda_panel %>%
  filter(hgxid4!="-------") %>%
  mutate(xwaveid_e = as.numeric(hgxid4)+(as.numeric(xwaveid))*10000000, 
         xwaveid_by_wave = as.numeric(xwaveid)+wave/100, 
         original_person_n = 4) %>%
  select(xwaveid_e, wave, xwaveid, hhrhid, xwaveid_by_wave, original_person_n)

id_vars_person5 <-
  hilda_panel %>%
  filter(hgxid5!="-------") %>%
  mutate(xwaveid_e = as.numeric(hgxid5)+(as.numeric(xwaveid))*10000000, 
         xwaveid_by_wave = as.numeric(xwaveid)+wave/100, 
         original_person_n = 5)%>%
  select(xwaveid_e, wave, xwaveid, hhrhid, xwaveid_by_wave, original_person_n)

id_vars_person6 <-
  hilda_panel %>%
  filter(hgxid6!="-------") %>%
  mutate(xwaveid_e = as.numeric(hgxid6)+(as.numeric(xwaveid))*10000000, 
         xwaveid_by_wave = as.numeric(xwaveid)+wave/100, 
         original_person_n = 6)%>%
  select(xwaveid_e, wave, xwaveid, hhrhid, xwaveid_by_wave, original_person_n)

id_vars_person7 <-
  hilda_panel %>%
  filter(hgxid7!="-------") %>%
  mutate(xwaveid_e = as.numeric(hgxid7)+(as.numeric(xwaveid))*10000000, 
         xwaveid_by_wave = as.numeric(xwaveid)+wave/100, 
         original_person_n = 7)%>%
  select(xwaveid_e, wave, xwaveid, hhrhid, xwaveid_by_wave, original_person_n)

id_vars_person8 <-
  hilda_panel %>%
  filter(hgxid8!="-------") %>%
  mutate(xwaveid_e = as.numeric(hgxid8)+(as.numeric(xwaveid))*10000000, 
         xwaveid_by_wave = as.numeric(xwaveid)+wave/100, 
         original_person_n = 8)%>%
  select(xwaveid_e, wave, xwaveid, hhrhid, xwaveid_by_wave, original_person_n)

id_vars=rbind(id_vars_person1, id_vars_person2, id_vars_person3, id_vars_person4, id_vars_person5, id_vars_person6, id_vars_person7, id_vars_person8)
rm(id_vars_person1, id_vars_person2, id_vars_person3, id_vars_person4, id_vars_person5, id_vars_person6, id_vars_person7, id_vars_person8)


# Creating xhhrhid:
xwaveid_xhhrhid <-
  id_vars %>%
  group_by(xwaveid_e)%>%
  arrange(wave) %>%
  slice(1) %>%
  mutate(xhhrhid = as.numeric(hhrhid)) %>%
  ungroup %>%
  select(xwaveid_e, xhhrhid)
id_vars <- merge(xwaveid_xhhrhid, id_vars, key = ("xwaveid_e")) # This cross-wave h_id variable is defined at the individual level. It will be shared across periods for all household members who entered at the same time (this makes it half-way between xwaveid and hhrhid)
rm(xwaveid_xhhrhid)

# Adding entry, exit and duration variables
id_vars %<>%
  dplyr::group_by(xwaveid_e) %>%
  dplyr::mutate(min_wave_g = min(wave), 
                max_wave_g = max(wave), 
                n_waves_g = max_wave_g - min_wave_g) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(xwaveid) %>%
  dplyr::mutate(min_wave = min(wave), 
                max_wave = max(wave), 
                n_waves = max_wave - min_wave, 
                n_different_min_wave_g_by_household = length(unique(min_wave_g)), 
                n_different_max_wave_g_by_household = length(unique(max_wave_g))) %>%
  dplyr::ungroup() 

str(id_vars)
# Investigating stability of household membership: 50% of houses have the same members the whole time. 
id_vars %>%
  filter(n_different_max_wave_g_by_household==1) %>%
  group_by(xwaveid) %>%
  slice(1) %>%
  group_by(n_different_min_wave_g_by_household) %>%
  tally()

## 3.  Create base dataset by individual (mostly contains housing variables): -----

      #### SECTION 4 INDEX:
          # 3.1   Generating the sample selection variables that can be derrived from variables collected annually
          # 3.2   Generating simple housing panel
          # 3.3   Generating extended housing panel

# 3.1  Generating the sample selection variables that can be derrived from variables collected annually:
hilda_panel %<>%
  mutate(xwaveid_by_wave = as.numeric(xwaveid)+wave/100, 
         year = substr(hhhqivw, 7,10),
         home_ownership_status = if_else(hstenr=="[1] Own/currently paying off mortgage", "Owner", 
                                         if_else(hstenr=="[2] Rent (or pay board)" | hstenr=="[3] Involved in a rent-buy scheme", "Renter", 
                                                 if_else(hstenr=="[4] Live here rent free/Life Tenure", "Other", "NA"))),
         n_people = hhadult + hh0_4 + hh5_9 + hh10_14, 
         n_adults = hhadult, 
         household_type = if_else(hhtype=="[24] Lone person", "Living alone", 
                                  if_else(hhtype=="[13] Lone parent with children < 15 wo others" | hhtype=="[14] Lone parent with children < 15 w other related" | hhtype=="[15] Lone parent with children < 15 w other not related" | hhtype=="[16] Lone parent with depst wo others" | hhtype=="[17] Lone parent with depst w other related" | hhtype=="[18] Lone parent with depst w other not related" | hhtype=="[19] Lone parent with ndepchild wo others" | hhtype=="[20] Lone parent with ndepchild w other related" | hhtype=="[21] Lone parent with ndepchild w other not related" | hhtype=="[22] Other related family wo children < 15 or others" | hhtype=="[23] Other related family wo children < 15 w others" | hhtype=="[25] Group household" | hhtype=="[26] Multi family household", "Other", 
                                          if_else(hhtype=="[-10] Non-responding person" | hhtype=="[-9] Non-responding household" | hhtype=="[-8] No SCQ" | hhtype=="[-7] Not able to be determined" | hhtype=="[-6] Implausible value" | hhtype=="[-5] Multiple response SCQ" | hhtype=="[-4] Refused/Not stated" | hhtype=="[-3] Dont know" | hhtype=="[-2] Not applicable" | hhtype=="[-1] Not asked", "NA", 
                                                  if_else(hhtype=="[1] Couple family wo children or others","Couple living alone","Couple living with others")))),
         single_or_simple_couple = if_else(hhtype== "[24] Lone person", "Single", if_else(hhtype=="[1] Couple family wo children or others", "Couple", "Other")))                          

# 3.2   Generating simple housing panel:
hilda_housing_panel <-
  hilda_panel %>%
  select(xwaveid, xwaveid_by_wave, hhrhid, wave, year, wealth_wave, home_ownership_status, n_people, n_adults, household_type, single_or_simple_couple, hsoid1, hsoid2, hsoid3, hsoid4, hsoid5, hsoid6, hsoid7, hsoid8, hgage1, hgage2, hgage3, hgage4, hgage5, hgage6, hgage7, hgage8)

# 3.3   Generating extended housing panel:
        # Drop any vars that will cause duplication when merged with hilda_panel: 
        id_vars_for_merging <-
          id_vars %>% 
          select(xwaveid_by_wave, xhhrhid, xwaveid_e, min_wave, max_wave, n_waves, n_different_min_wave_g_by_household, n_different_max_wave_g_by_household, original_person_n)
        
        # Merge: 
        hilda_housing_panel_e <- # "e" stands for extended.
          merge(hilda_housing_panel, id_vars_for_merging, by = ("xwaveid_by_wave"))
        
        # Drop unnecssary vars:
        rm(id_vars_for_merging)

        # Transform individual-specific vars:
        zero <- as.integer(0)
        hilda_housing_panel_e %<>%
          mutate(hsoid = ifelse(original_person_n==1, as.numeric(hsoid1),
                                if_else(original_person_n==2, as.numeric(hsoid2), 
                                        if_else(original_person_n==3, as.numeric(hsoid3), 
                                                if_else(original_person_n==4, as.numeric(hsoid4), 
                                                        if_else(original_person_n==5, as.numeric(hsoid5), 
                                                                if_else(original_person_n==6, as.numeric(hsoid6), 
                                                                        if_else(original_person_n==7, as.numeric(hsoid7), 
                                                                                if_else(original_person_n==8, as.numeric(hsoid8),  0)))))))),
                 hsoid = if_else(hsoid==10, "NA", if_else(hsoid==11, "[0] No", if_else(hsoid==12, "[1] Yes", "Missing"))),#Don't know how to fix the error message regarding this specification of NA, but it doesn't matter.
                 age = if_else(original_person_n==1, hgage1,
                                 if_else(original_person_n==2, hgage2, 
                                         if_else(original_person_n==3, hgage3, 
                                                 if_else(original_person_n==4, hgage4, 
                                                         if_else(original_person_n==5, hgage5, 
                                                                 if_else(original_person_n==6, hgage6, 
                                                                         if_else(original_person_n==7, hgage7, 
                                                                                 if_else(original_person_n==8, hgage8, as.integer(0))))))))))

        rm(zero)

### RELOADING THE DATA FROM THIS POINT ### ------
#load("~/Pensioner downsizing/Product_of_setup_for_downsizing_script.RData")