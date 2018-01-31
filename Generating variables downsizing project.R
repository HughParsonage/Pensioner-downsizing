                                                ##### Generating variables for downsizing ######
 

####### Set-up: ----
# load("~/Pensioner downsizing/Product_of_setup_for_downsizing_script.RData")
                                                
## 1. Housing variables: =====

#### SECTION 1 INDEX 
# 1.2: Cleaning variables: 
        # 1.1    Cleaning the individual level home ownership variable
        # 1.2    Cleaning the household level home ownership variable
        # 1.3    Reconciling the individual-level and household-level home ownership variable:

    # 1.1   Cleaning the individual level home ownership variable:
          
          # If individual ownership is the same on either side of a missing wealth wave, we fill it: 
          # By doing this, we recover data on an additional 4217 rows (xwaveid_e for a particular wave))
          hilda_housing_panel_e %<>%
            group_by(xwaveid_e) %>%
            mutate(wealth_wave_n = if_else(wave==2, 1, if_else(wave==6, 2, if_else(wave==10, 3, if_else(wave==14, 4, 0)))), # This coding is slightly lazy: all waves other than 2, 6, 10 and 14 have been allocated "0". This means that the wealth_wave_n lag of wave2 observations will be na(), and the lead of wave14 will also be na().
                   last_hsoid = lag(hsoid, 1, order_by = wealth_wave_n),
                   next_hsoid = lead(hsoid, 1, order_by = wealth_wave_n), 
                   last_home_ownership_status = lag(home_ownership_status, 1, order_by = wave),
                   next_home_ownership_status = lead(home_ownership_status, 1, order_by = wave)) %>%
            # Central waves
            mutate(hsoid = if_else(wave==6 | wave==10, if_else(hsoid=="[0] No" | hsoid=="[1] Yes", hsoid, if_else(last_hsoid==next_hsoid, last_hsoid, hsoid)), hsoid), # surrounding waves could = NA, but that wouldn't be any worse than the status quo. 
                   hsoid = if_else(wave==2, # Wave 2 (no prior info to draw on) 
                                   if_else(hsoid=="[0] No" | hsoid=="[1] Yes", hsoid, if_else(last_home_ownership_status==next_home_ownership_status, next_hsoid, hsoid)),
                                   # Wave 14 (no later info to draw on)
                                   if_else(hsoid=="[0] No" | hsoid=="[1] Yes", hsoid, if_else(last_home_ownership_status ==next_home_ownership_status, last_hsoid, hsoid))))
          
          
        
    # 1.2 Cleaning the household level home ownership variable:
    #table(hilda_housing_panel_e$home_ownership_status, exclude = NULL) # 1,758 are formal NA's, another 67,608 are missing

          # All of the missing observations are from wave 1: 
          #hilda_panel %>%
            #filter(is.na(home_ownership_status)) %>%
            #filter(wave>1) %>%
            #summarise(n())
          
          # Is there any variation within households? No, by construction. 

    # 1.3 Reconciling the individual-level and household-level home ownership variable:
    #hilda_housing_panel_e %>% 
      #filter(wealth_wave==1) %>%
     # group_by(hsoid, home_ownership_status) %>%
      #tally ()

            # Looking only at the wealth waves:
            # None of the hsoid NA's are home owners (mostly renters, some others, v few which also have NA for home_ownership_status)
            # A)  98 % of the hsoids that are home owners have the corresponding home_ownership_status. Almost all who don't are down as renters: maybe they don't own the house they live in?
            # B) 97 % of the hsoids that are NOT home owners have the home_ownership_status of owner. This MUST be within house renting, right? 
            # C) 91 % of missing hsoids completely are renters (remainin 10% are owners)
            # The overall split of owners / renters from the home_ownership_status var is 28% renters, 70% home owners, 2% other, remaining NA.
            
            # This leads me to think that hsoid is only collected within households that are owned by a resident:
            #hilda_housing_panel_e %>%
              #group_by(xwaveid_by_wave) %>%
              #mutate(max_hsoid = max(hsoid)) %>%
              #ungroup()
            # This interpretation of the numbers stacks up: hsoid indicates 70% = house owners, 6% not, 23% missing data, 5% NA (looking only at the wealth waves).
            
            # Let's check my hypothesis for group B) most people with hsoid=No live with a homeowner:
            #hilda_housing_panel_e %>%
              #group_by(xwaveid_by_wave) %>%          
              #mutate(max_hsoid = max(hsoid)) %>%
              #ungroup() %>%
              #filter(hsoid=="[0] No") %>%
              #group_by(max_hsoid)%>%
              #select(max_hsoid) %>%
              #tally()
            # 97% are. The remaining are split almost equally between No and NA. 
            
            # Forward-filling hsoid:
            hilda_housing_panel_e %<>%
              mutate(last_hsoid_ww = if_else(wave==3, lag(hsoid, 1, order_by = wave),
                                             if_else(wave==4, lag(hsoid, 2, order_by = wave), 
                                                     if_else(wave==5, lag(hsoid, 3, order_by = wave), 
                                                             if_else(wave==6, lag(hsoid, 4, order_by = wave), 
                                                                     if_else(wave==7, lag(hsoid, 1, order_by = wave),
                                                                             if_else(wave==8, lag(hsoid, 2, order_by = wave), 
                                                                                     if_else(wave==9, lag(hsoid, 3, order_by = wave), 
                                                                                             if_else(wave==10, lag(hsoid, 4, order_by = wave), 
                                                                                                     if_else(wave==11, lag(hsoid, 1, order_by = wave),
                                                                                                             if_else(wave==12, lag(hsoid, 2, order_by = wave), 
                                                                                                                     if_else(wave==13, lag(hsoid, 3, order_by = wave), 
                                                                                                                             # Wave 14 (no later info to draw on)                                            
                                                                                                                             if_else(wave==14, lag(hsoid, 4, order_by = wave), hsoid))))))))))))) %>%
              mutate(next_hsoid_ww = if_else(wave==2, lead(hsoid, 4, order_by = wave), 
                                             if_else(wave==3, lead(hsoid, 3, order_by = wave), 
                                                     if_else(wave==4, lead(hsoid, 2, order_by = wave), 
                                                             if_else(wave==5, lead(hsoid, 1, order_by = wave), 
                                                                     if_else(wave==6, lead(hsoid, 4, order_by = wave), 
                                                                             if_else(wave==7, lead(hsoid, 3, order_by = wave), 
                                                                                     if_else(wave==8, lead(hsoid, 2, order_by = wave), 
                                                                                             if_else(wave==9, lead(hsoid, 1, order_by = wave), 
                                                                                                     if_else(wave==10, lead(hsoid, 4, order_by = wave), 
                                                                                                             if_else(wave==11, lead(hsoid, 3, order_by = wave), 
                                                                                                                     if_else(wave==12, lead(hsoid, 2, order_by = wave), 
                                                                                                                             if_else(wave==13, lead(hsoid, 1, order_by = wave), hsoid))))))))))))) %>%
              mutate(hsoid_fill = if_else(wave==3, lead(hsoid, 1, order_by = wave),
                                          if_else(wave==4, lead(hsoid, 2, order_by = wave),
                                                  if_else(wave==5, lead(hsoid, 5, order_by = wave), 
                                                          if_else(wave==7, lead(hsoid, 1, order_by = wave),
                                                                  if_else(wave==8, lead(hsoid, 2, order_by = wave),
                                                                          if_else(wave==9, lead(hsoid, 5, order_by = wave), 
                                                                                  if_else(wave==11, lead(hsoid, 1, order_by = wave),
                                                                                          if_else(wave==12, lead(hsoid, 2, order_by = wave),
                                                                                                  if_else(wave==13, lead(hsoid, 5, order_by = wave), hsoid)))))))))) %>%
              mutate(hsoid_filled = if_else(wave!=1 & is.na(hsoid) & last_hsoid_ww == next_hsoid_ww & last_home_ownership_status == next_home_ownership_status, hsoid_fill, hsoid))
            
            ### Investigating whether there's any more information we could be capturing:
            # All of the hsoid_filled that are = officially NA have max_hsoid of NA too (ie: there's no hsoid data in the observation)
            #hilda_housing_panel_e %>%
              #group_by(xwaveid_by_wave) %>%          
              #mutate(max_hsoid = max(hsoid)) %>%
              #ungroup() %>%
              #filter(is.na(hsoid_filled)) %>%
              #group_by(max_hsoid, wave) %>%
              #tally ()
            # But most of them have some home_ownership_status data
            #hilda_housing_panel_e %>%
              #filter(is.na(hsoid_filled)) %>%
              #group_by(home_ownership_status) %>%
              #tally ()
            
            # The same is true for obs with explicit NAs:
            #hilda_housing_panel_e %>%
              #group_by(xwaveid_by_wave) %>%          
              #mutate(max_hsoid = max(hsoid)) %>%
              #ungroup() %>%
              #filter(hsoid_filled!="[0] No" & hsoid_filled != "[1] Yes") %>%
              #group_by(max_hsoid, wave) %>%
              #tally ()
            
            #hilda_housing_panel_e %>%
              #filter(hsoid_filled!="[0] No" & hsoid_filled != "[1] Yes") %>%
              #group_by(home_ownership_status) %>%
              #tally ()      
            
            
            # Reconciling home_ownership_status (about whether someone at the property owns the house) and hsoid (whether particular household members own houses)
            
            #### If hsoid = YES:
            hilda_housing_panel_e %<>%
              
              mutate(individual_home_ownership_details = "Other",
                     individual_home_ownership_details = if_else(hsoid_filled=="[1] Yes" & home_ownership_status=="Owner", "Owns the house they live in",
                                                                 if_else(hsoid_filled=="[1] Yes" & home_ownership_status=="Renter", "Owns a house, but not the one they live in", 
                                                                         if_else(hsoid_filled=="[1] Yes" & (home_ownership_status=="NA" | home_ownership_status=="Other"), "Owns a house, but not the one they live in", individual_home_ownership_details)))) %>% # This classification is a (conservative) assumption, but it doesn't matter too much because there's only 340 of them. 
              ### If hsoid = NO: 
              mutate(individual_home_ownership_details = if_else(hsoid_filled=="[0] No" & home_ownership_status=="Owner", "Lives with home owner",
                                                                 if_else(hsoid_filled=="[0] No" & (home_ownership_status!="Owner"), "Not a home owner", individual_home_ownership_details))) %>%
              ### If hsoid = NA: 
              mutate(individual_home_ownership_details = if_else((hsoid_filled!="[0] No" & hsoid_filled!="[1] Yes") & home_ownership_status=="Owner", "Lives with home owner",
                                                                 if_else((hsoid_filled!="[0] No" & hsoid_filled!="[1] Yes") & home_ownership_status!="Owner", "Not a home owner", individual_home_ownership_details))) %>%
              mutate(individual_home_ownership_details = if_else(is.na(hsoid_filled) & home_ownership_status=="Owner", "Lives with home owner",
                                                                 if_else(is.na(hsoid_filled) & home_ownership_status!="Owner", "Not a home owner", individual_home_ownership_details)))
            
            # Order individual_home_ownership_details:
            hilda_housing_panel_e$individual_home_ownership_details <- factor(hilda_housing_panel_e$individual_home_ownership_details, 
                                                                             levels = c("Owns the house they live in", "Owns a house, but not the one they live in", "Lives with home owner", "Not a home owner"), 
                                                                             ordered = TRUE)
            hilda_housing_panel_e$home_ownership_status <- factor(hilda_housing_panel_e$home_ownership_status, levels=c("NA","Other","Renter","Owner"), ordered = TRUE)
            
            # Looks great!
            #hilda_housing_panel_e %>% 
            #  group_by(home_ownership_status, individual_home_ownership_details, hsoid_filled, hsoid) %>%
            #  tally () %>%
            #  View()


## 2. Demographics: ======

###### Employment variables:
      employment_status<- 
        hilda_panel %>%
        select(xwaveid_by_wave, hges1, hges2, hges3, hges4, hges5, hges6, hges7, hges8)
      
      hilda_housing_panel_e<-
        merge(hilda_housing_panel_e, employment_status, by = "xwaveid_by_wave", all = TRUE)
      
      hilda_housing_panel_e %<>%
        mutate(i_employment_status = if_else(original_person_n==1, hges1,
                                             if_else(original_person_n==2, hges2, 
                                                     if_else(original_person_n==3, hges3, 
                                                             if_else(original_person_n==4, hges4, 
                                                                     if_else(original_person_n==5, hges5,
                                                                             if_else(original_person_n==6, hges6, 
                                                                                     if_else(original_person_n==7, hges7, hges8)))))))) 
      
      rm(employment_status)
      
###### Relationship to other household members:
      relationships <-
        hilda_panel %>%
        select(xwaveid_by_wave, hhrih01, hhrih02, hhrih03, hhrih04, hhrih05, hhrih06, hhrih07, hhrih08)
      
      hilda_housing_panel_e<-
        merge(hilda_housing_panel_e, relationships, by = "xwaveid_by_wave", all = TRUE)
      
      hilda_housing_panel_e %<>%
        mutate(i_relationship = if_else(original_person_n==1, hhrih01,
                                             if_else(original_person_n==2, hhrih02, 
                                                     if_else(original_person_n==3, hhrih03, 
                                                             if_else(original_person_n==4, hhrih04, 
                                                                     if_else(original_person_n==5, hhrih05,
                                                                             if_else(original_person_n==6, hhrih06, 
                                                                                     if_else(original_person_n==7, hhrih07, hhrih08))))))), 
               i_child = if_else(i_relationship=="Dependent student" | i_relationship=="Non-dependent child" | i_relationship=="Child < 15", 1, 0))
      
rm(relationships)

###### Owner_type:
#table(hilda_housing_panel_e$home_ownership_status)
#table(hilda_housing_panel_e$individual_home_ownership_details)
#table(hilda_housing_panel_e$hsoid)

##### Household type:
#table(hilda_housing_panel_e$household_type)
  

###### Age:

# Cleaning the age variable: 
#table(hilda_housing_panel_e$age) # 58 NA's (negative ages)

hilda_housing_panel_e %<>%
  group_by(xwaveid_e) %>%
  mutate(last_age = lag(age, 1, order_by = wave),
         next_age = lead(age, 1, order_by = wave)) %>%
  mutate(age = if_else(age>=0, as.numeric(age), 
                         if_else(last_age>0, as.numeric(last_age) + 1, 
                                 if_else(next_age>0, as.numeric(next_age) - 1, as.numeric(age)))), age = age) %>%
  subset(select = -c(last_age, next_age)) %>%
  ungroup() %>%
  mutate(over_65s = if_else(age>=65, 1, 0), 
         age_decade = round_any(age - 4,10)) %>% # We subtract 5 so that 0-10 = 0, 10-19 = 10, 20 - 29 = 20, etc...
  group_by(xwaveid_by_wave) %>%
  mutate(n_over_65s_in_household = sum(over_65s), 
         prep_for_count = 1,
         n_in_household = sum(prep_for_count), 
         prop_over_65s_in_household = (n_over_65s_in_household/n_in_household)) %>%
  ungroup() %>% 
  group_by(xwaveid) %>%
  mutate(max_over_65s_status=max(over_65s)) %>%
  ungroup()

# Creating the household_age_decade variable:


# Average the ages of the house_owners:
hilda_housing_panel_e %<>%
  mutate(individual_home_owner = if_else(individual_home_ownership_details=="Owns the house they live in", 1, 0),
         individual_home_owner = if_else(is.na(individual_home_owner), 0, individual_home_owner),
         not_retired_and_not_child = if_else(i_child==0 & age>=15 & i_employment_status!="[4] Retired", 1, 0), 
         not_retired_and_not_child = if_else(is.na(not_retired_and_not_child), 0, not_retired_and_not_child), 
         not_child = if_else(i_child==0 & age>=15, 1, 0), 
         not_child = if_else(is.na(not_child), 0, not_child)) 

  # Average age of home owners:
average_age_decade_of_household1 <-
    hilda_housing_panel_e %>%
    filter(individual_home_owner==1) %>%
    group_by(xwaveid_by_wave) %>%
    mutate(average_age_decade_of_household1 = round_any(mean((age-4)),10)) %>%
    slice(1) %>%
    ungroup() %>%
    select(xwaveid_by_wave, average_age_decade_of_household1)
  
average_age_decade_of_household2 <-
  hilda_housing_panel_e %>%
  filter(not_retired_and_not_child==1)%>%
  group_by(xwaveid_by_wave) %>%
  mutate(average_age_decade_of_household2 = round_any(mean((age-4)),10)) %>%
  slice(1) %>%
  ungroup() %>%
  select(xwaveid_by_wave, average_age_decade_of_household2)

average_age_decade_of_household3 <-
  hilda_housing_panel_e %>%
  filter(not_child==1)%>%
  group_by(xwaveid_by_wave) %>%
  mutate(average_age_decade_of_household3 = round_any(mean((age-4)),10)) %>%
  slice(1) %>%
  ungroup() %>%
  select(xwaveid_by_wave, average_age_decade_of_household3)

average_age_decade_of_household4 <-
  hilda_housing_panel_e %>%
  group_by(xwaveid_by_wave) %>%
  mutate(average_age_decade_of_household4 = round_any(mean((age-4)),10)) %>%
  slice(1) %>%
  ungroup() %>%
  select(xwaveid_by_wave, average_age_decade_of_household4)

hilda_housing_panel_e<-
  merge(hilda_housing_panel_e, average_age_decade_of_household1, by = "xwaveid_by_wave", all = TRUE)

hilda_housing_panel_e<-
  merge(hilda_housing_panel_e, average_age_decade_of_household2, by = "xwaveid_by_wave", all = TRUE)

hilda_housing_panel_e<-
  merge(hilda_housing_panel_e, average_age_decade_of_household3, by = "xwaveid_by_wave", all = TRUE)

hilda_housing_panel_e<-
  merge(hilda_housing_panel_e, average_age_decade_of_household4, by = "xwaveid_by_wave", all = TRUE)

rm(average_age_decade_of_household1, average_age_decade_of_household2, average_age_decade_of_household3, average_age_decade_of_household4)

hilda_housing_panel_e %<>%
  group_by(xwaveid_by_wave) %>%
  mutate(max_individual_home_owner = max(individual_home_owner), 
         max_not_retired_and_not_child = max(not_retired_and_not_child), 
         max_not_child = max(not_child), 
         average_age_decade_of_household1 = if_else(is.na(average_age_decade_of_household1), 0, average_age_decade_of_household1),
         average_age_decade_of_household2 = if_else(is.na(average_age_decade_of_household2), 0, average_age_decade_of_household2),
         average_age_decade_of_household3 = if_else(is.na(average_age_decade_of_household3), 0, average_age_decade_of_household3),
         average_age_decade_of_household4 = if_else(is.na(average_age_decade_of_household4), 0, average_age_decade_of_household4)) %>%
  mutate(average_age_decade_of_household = if_else(max_individual_home_owner==1, average_age_decade_of_household1, 
                                                   if_else(max_not_retired_and_not_child==1, average_age_decade_of_household2, 
                                                           if_else(max_not_child==1, average_age_decade_of_household3, average_age_decade_of_household4)))) %>%
  ungroup() %>%
  #subset(select = -c(average_age_decade_of_household1, average_age_decade_of_household2, average_age_decade_of_household3, average_age_decade_of_household4, max_individual_home_owner, max_not_retired_and_not_child, max_not_child)) %>%
  group_by(xwaveid) %>%
  mutate(max_average_age_decade_of_household = max(average_age_decade_of_household)) %>%
  ungroup()  %>%
  subset(select = -c(average_age_decade_of_household1, average_age_decade_of_household2, average_age_decade_of_household3, average_age_decade_of_household4, max_individual_home_owner, max_not_retired_and_not_child, max_not_child))

  
## 3. Finance (wealth, moving costs, financial stress): ======

####### Pension opportunity cost:

####### Value of personal assets (ie: moving cost): 

        # Mergining in variables:  
        asset_variables <-
            hilda_panel %>%
            select(xwaveid_by_wave, hwnfii, mvcval)
          
        hilda_housing_panel_e<-
          merge(hilda_housing_panel_e, asset_variables, by = "xwaveid_by_wave", all = TRUE)
        
        rm(asset_variables)  

        # Allocating inituitive names:  
        hilda_housing_panel_e %<>%
            mutate(non_financial_assets = hwnfii, # Household Non-Financial Assets [imputed] ($) [weighted topcode]; W2, W6, W10, W14
                   value_of_cars = mvcval, # Current worth of vehicles [weighted topcode]
                   non_financial_assets_less_cars = non_financial_assets - value_of_cars) %>%
            subset(select = -c(hwnfii, mvcval)) # Now that you've renamed, remove the old vars. 
        
        # This table shows that the variable is only collected in wealth waves:
        #hilda_housing_panel_e %>% mutate(NA_non_financial_assets_less_cars = is.na(non_financial_assets_less_cars)) %>% select(NA_non_financial_assets_less_cars, wave) %>% table()
        
        # Filling in W3-5, 7-9, 11-13:
        hilda_housing_panel_e %<>%
          group_by(xwaveid_e) %>%
          mutate(last_nonfin_assets_less_cars_ww = if_else(wave==3, lag(non_financial_assets_less_cars, 1, order_by = wave), 
                                                         if_else(wave==4, lag(non_financial_assets_less_cars, 2, order_by = wave),
                                                                 if_else(wave==5, lag(non_financial_assets_less_cars, 3, order_by = wave),
                                                                         if_else(wave==7, lag(non_financial_assets_less_cars, 1, order_by = wave),
                                                                                 if_else(wave==8, lag(non_financial_assets_less_cars, 2, order_by = wave),
                                                                                         if_else(wave==9, lag(non_financial_assets_less_cars, 3, order_by = wave),
                                                                                                 if_else(wave==11, lag(non_financial_assets_less_cars, 1, order_by = wave),
                                                                                                         if_else(wave==12, lag(non_financial_assets_less_cars, 2, order_by = wave),
                                                                                                                 if_else(wave==13, lag(non_financial_assets_less_cars, 3, order_by = wave), 
                                                                                                                         if_else(wave==2 | wave==6 | wave==10 | wave==14, non_financial_assets_less_cars, as.integer(0)))))))))))) %>%
          ungroup()
        
        # These NA's match the waves of data that are missing all together, which makes me confident that the command is working as I expect:
        #hilda_housing_panel_e %>% mutate(NA_last_nonfin_assets_less_cars_ww = is.na(last_nonfin_assets_less_cars_ww)) %>% select(NA_last_nonfin_assets_less_cars_ww, wave) %>% table()
        

####### Wealth: ====== These need to be scaled by age:

        ## Net worth:
        
            # Merge the variables in:
            net_worth_vars <-
              hilda_panel %>%
              mutate(net_worth = hwnwip - hwnwin) %>% # DV: Household Net Worth [positive values] [imputed] ($) [weighted topcode] *less* DV: Household Net Worth [positive values] [imputed] ($) [weighted topcode]; W2, W6, W10, W14
              select(net_worth, xwaveid_by_wave)
    
            hilda_housing_panel_e<-
              merge(hilda_housing_panel_e, net_worth_vars, by = "xwaveid_by_wave", all = TRUE)
            
            rm(net_worth_vars)
            
            # Fill the gaps:
            hilda_housing_panel_e %<>%
              group_by(xwaveid_e) %>%
              mutate(last_net_worth_ww = if_else(wave==3, lag(net_worth, 1, order_by = wave), 
                                                               if_else(wave==4, lag(net_worth, 2, order_by = wave),
                                                                       if_else(wave==5, lag(net_worth, 3, order_by = wave),
                                                                               if_else(wave==7, lag(net_worth, 1, order_by = wave),
                                                                                       if_else(wave==8, lag(net_worth, 2, order_by = wave),
                                                                                               if_else(wave==9, lag(net_worth, 3, order_by = wave),
                                                                                                       if_else(wave==11, lag(net_worth, 1, order_by = wave),
                                                                                                               if_else(wave==12, lag(net_worth, 2, order_by = wave),
                                                                                                                       if_else(wave==13, lag(net_worth, 3, order_by = wave), 
                                                                                                                               if_else(wave==2 | wave==6 | wave==10 | wave==14, net_worth, 0))))))))))) %>%
              ungroup()
            
        
        ## Mega costs - eg extreme health expenditure 
                # hxyhlpi	Expenditure	DV: SCQ Household annual expenditure - Fees paid to health practitioners ($) [imputed]
                # hxyphmi	Expenditure	DV: SCQ Household annual expenditure - Medicines, prescriptions, pharmaceuticals, alternative medicines ($) [imputed]
            
##### Insurance expenditure: 
            insurance_variables <-
              hilda_panel %>%
              mutate(insurance_expenditure = hxyphii + hxyoii) %>% #DV: SCQ Household annual expenditure - Private health insurance ($) [imputed] - W5 onwards; # DV: SCQ Household annual expenditure - Other insurance (home/contents/motor vehicle) ($) [imputed] - W5 onwards
              select(insurance_expenditure, xwaveid_by_wave)
                
            hilda_housing_panel_e<-
              merge(hilda_housing_panel_e, insurance_variables, by = "xwaveid_by_wave", all = TRUE)
            
            rm(insurance_variables)
            
             
### Expenses: 
            
            # The challenge with these is that they are collected over different waves...
            
            ## Expenditure variables: 
            #xpgroci	x	Expenditure	DV: Household weekly expenditure on all groceries [imputed]
            #xpfoodi	x	Expenditure	DV: Household weekly expenditure on groceries for food and drink [imputed]
            # INCLUDED IN FINANCIAL STRESS CATEGORY ALREADY: #xposmli	x	Expenditure	DV: Household weekly expenditure on meals outside the home [imputed]
            #hxyalci	h	Expenditure	DV: SCQ Household annual expenditure - Alcohol ($) [imputed]
            #hxycigi	h	Expenditure	DV: SCQ Household annual expenditure - Cigarettes and tobacco ($) [imputed]
            #hxymvfi	h	Expenditure	DV: SCQ Household annual expenditure - Motor vehicle fuel ($) [imputed]
            #hxymcfi	h	Expenditure	DV: SCQ Household annual expenditure - Mens clothing and footwear ($) [imputed]
            #hxywcfi	h	Expenditure	DV: SCQ Household annual expenditure - Womens clothing and footwear ($) [imputed]
            #hxyccfi	h	Expenditure	DV: SCQ Household annual expenditure - Childrens clothing and footwear ($) [imputed]
            #hxyphii	h	Expenditure	DV: SCQ Household annual expenditure - Private health insurance ($) [imputed]
            #hxyoii	h	Expenditure	DV: SCQ Household annual expenditure - Other insurance (home/contents/motor vehicle) ($) [imputed]
            #hxymvri	h	Expenditure	DV: SCQ Household annual expenditure - Motor vehicle repairs/maintenance ($) [imputed]
            #hxygrci	h	Expenditure	DV: SCQ Household annual expenditure - Groceries ($) [imputed]
            #hxypbti	h	Expenditure	DV: SCQ Household annual expenditure - Public transport and taxis ($) [imputed]
            #hxymli	h	Expenditure	DV: SCQ Household annual expenditure - Meals eaten out ($) [imputed]
            #hxyhmri	h	Expenditure	DV: SCQ Household annual expenditure - Home repairs/renovations/maintenance ($) [imputed]
            #hxyedci	h	Expenditure	DV: SCQ Household annual expenditure - Education fees ($) [imputed]
            #hxyutli	h	Expenditure	DV: SCQ Household annual expenditure - Electricity bills, gas bills and other heating fuel ($) [imputed]
            #hxytlii	h	Expenditure	DV: SCQ Household annual expenditure - Telephone rent, calls and internet charges ($) [imputed]
            # INCLUDED IN HEALTH CATEGORY ALREADY: #hxyhlpi	h	Expenditure	DV: SCQ Household annual expenditure - Fees paid to health practitioners ($) [imputed]
            # INCLUDED IN HEALTH CATEGORY ALREADY: #hxyphmi	h	Expenditure	DV: SCQ Household annual expenditure - Medicines, prescriptions, pharmaceuticals, alternative medicines ($) [imputed]

            
### Financial stress: 
            
        ## Overdue bills: 
            
            # Merge the variable in:
            financial_stress_vars <-
              hilda_panel %>%
              mutate(overdue_bills = hwobdti, # DV: Household Net Worth [positive values] [imputed] ($) [weighted topcode] *less* DV: Household Net Worth [positive values] [imputed] ($) [weighted topcode]; W2, W6, W10, W14
                     eating_out = xposmli, # Expenditure	DV: Household weekly expenditure on meals outside the home [imputed], W1, W3-5, W11+
                     major_worsening_of_finances_this_yr = if_else(lefnw=="[2] Yes", 1, 0)) %>% #Life events in past year: Major worsening in finances; collected in all waves.
              select(overdue_bills, eating_out, major_worsening_of_finances_this_yr, xwaveid_by_wave)
            
            hilda_housing_panel_e<-
              merge(hilda_housing_panel_e, financial_stress_vars, by = "xwaveid_by_wave", all = TRUE)
            
            rm(financial_stress_vars)
            
            # Fill the gaps: 
            hilda_housing_panel_e %<>%
              group_by(xwaveid_e) %>%
              mutate(last_overdue_bills_ww = if_else(wave==3, lag(overdue_bills, 1, order_by = wave), 
                                                 if_else(wave==4, lag(overdue_bills, 2, order_by = wave),
                                                         if_else(wave==5, lag(overdue_bills, 3, order_by = wave),
                                                                 if_else(wave==7, lag(overdue_bills, 1, order_by = wave),
                                                                         if_else(wave==8, lag(overdue_bills, 2, order_by = wave),
                                                                                 if_else(wave==9, lag(overdue_bills, 3, order_by = wave),
                                                                                         if_else(wave==11, lag(overdue_bills, 1, order_by = wave),
                                                                                                 if_else(wave==12, lag(overdue_bills, 2, order_by = wave),
                                                                                                         if_else(wave==13, lag(overdue_bills, 3, order_by = wave), 
                                                                                                                 if_else(wave==2 | wave==6 | wave==10 | wave==14, overdue_bills, as.integer(0)))))))))))) %>%
              ungroup()
            
            hilda_housing_panel_e %<>%
              group_by(xwaveid_e) %>%
              mutate(last_eating_out_ww = if_else(wave==2, lag(eating_out, 1, order_by = wave), 
                                                  if_else(wave==6, lag(eating_out, 1, order_by = wave), 
                                                            if_else(wave==7, lag(eating_out, 2, order_by = wave),
                                                                       if_else(wave==8, lag(eating_out, 3, order_by = wave),
                                                                               if_else(wave==9, lag(eating_out, 4, order_by = wave),
                                                                                       if_else(wave==10, lag(eating_out, 5, order_by = wave),
                                                                                               if_else(wave==1 | wave==3 | wave==4 | wave==5 | wave==11 | wave==12 | wave==13 | wave==14 | wave==15, eating_out, as.integer(0))))))))) %>%
              ungroup()
            
            
                                                
                                              
        # The material deprivation variables are only collected in W14. Frustrating!    
          
## ?. Life expectancy:
           # psfagd  Age of respondent when father died # collected in w8, w12, w15
           # psmagd	 Age of respondent when mother died # collected in w8, w12, w15
           # ghsf6d  Health state classification # all waves
           # hedifma Health condition causes morbidity issues #w4, w9, w13
            
## 4. Potential non-financial reasons for moving: HEALTH ======
            
    # Codify health variables:
            #NOTE: total_health_expenditure excludes insurance spending, and is only collected from wave 6.
            
         health_variables <-  
            hilda_panel %>%
              mutate(hglth1_1 = if_else(hglth1=="[1] Yes", 1, 0), # Long term health condition, disability or impairment on Household Form, answered by one person in household. Collected every wave!
                     hglth2_1 = if_else(hglth2=="[1] Yes", 1, 0),
                     hglth3_1 = if_else(hglth3=="[1] Yes", 1, 0),
                     hglth4_1 = if_else(hglth4=="[1] Yes", 1, 0),
                     hglth5_1 = if_else(hglth5=="[1] Yes", 1, 0),
                     hglth6_1 = if_else(hglth6=="[1] Yes", 1, 0),
                     hglth7_1 = if_else(hglth7=="[1] Yes", 1, 0), 
                     hglth8_1 = if_else(hglth8=="[1] Yes", 1, 0),
                     n_ppl_w_health_condition = hglth1_1 + hglth2_1 + hglth3_1 + hglth4_1 + hglth5_1 + hglth6_1 + hglth7_1 + hglth8_1, 
                     total_health_expenditure =  hxyhlpi + hxyphmi, #Expenditure	DV: SCQ Household annual expenditure - Fees paid to health practitioners ($) [imputed]; Expenditure	DV: SCQ Household annual expenditure - Medicines, prescriptions, pharmaceuticals, alternative medicines ($) [imputed]
                     top_quintile_health_expenditure = if_else(total_health_expenditure>=2000, 1, 0)) %>% # $2000 is the 80th percentile value.
              group_by(xwaveid) %>%
              mutate(D_n_ppl_w_health_condition = lag(n_ppl_w_health_condition, 1, order_by = wave),
                     D_total_health_expenditure = lag(total_health_expenditure, 1, order_by = wave),
                     D_n_ppl_w_health_condition_3yrs = lag(n_ppl_w_health_condition, 3, order_by = wave),
                     D_total_health_expenditure_3yrs = lag(total_health_expenditure, 3, order_by = wave)) %>%
              ungroup() %>%
              select(n_ppl_w_health_condition, total_health_expenditure, top_quintile_health_expenditure, D_n_ppl_w_health_condition, D_n_ppl_w_health_condition_3yrs, D_total_health_expenditure, D_total_health_expenditure_3yrs, xwaveid_by_wave)
              
            hilda_housing_panel_e<-merge(hilda_housing_panel_e, health_variables, by = "xwaveid_by_wave", all = TRUE)
            
            rm(health_variables)  
   
          
          
        # Exploring other health variables: 
            # gh1   Self-assessed health (all waves)
            # gh2   Health compared to 1 year ago (all years)
            

        # ROUGH WORKING: Deciding on the cut-off for high health expenditure: 
        #    quantile(testing$total_health_expenditure, probs = c(0.5, 0.75, 0.8, 0.9, 0.95, 0.99), type = 1, na.rm = TRUE)

## 5. Potential non-financial reasons for moving: CHANGE IN FAMILY STRUCTURE ======            

## 7. Potential non-financial reasons for moving: PROPENSITY TO MOVE ======     
      other_reasons_for_moving <- 
            hilda_panel %>%
            mutate(years_since_divorce_year = if_else(mrpdivn>0, wave+2000 - mrpdivn, var(1)), 
                   death_in_household_year = if_else(ledsc=="[2] Yes", wave, as.double(0)), 
                   number_of_moves_in_10ys = if_else(mhn10yr=="[1] Lived in one home in the last 10 years", 1, # Number of houses lived in in the last 10 years, collected every wave. 
                                                     if_else(mhn10yr=="[2] Lived in two homes in the last 10 years", 2,
                                                             if_else(mhn10yr=="[3] Lived in three homes in the last 10 years", 3, 
                                                                     if_else(mhn10yr=="[4] Lived in four homes in the last 10 years", 4,
                                                                             if_else(mhn10yr=="[5] 5 to 9", 7, 
                                                                                     if_else(mhn10yr=="[6] 10 to 14", 12, 
                                                                                             if_else(mhn10yr=="[7] 15+", 17, var(0)))))))),  # Just using var(1) to induce NA.) %>%
                   satisfaction_w_neighbourhood = if_else(losatnl>=0, losatnl, as.integer(7.874279))) %>% # The neighbourhood in which you live, collected every wave. We're replacing satisfaction with the mean if not observed directly. 
            select(years_since_divorce_year, death_in_household_year, number_of_moves_in_10ys, satisfaction_w_neighbourhood, xwaveid_by_wave)

      # Finding the mean of losatnl for the above script: 
          # hilda_panel %>%  group_by(xwaveid_by_wave) %>% slice(1) %>% ungroup() %>% filter(losatnl>=0) %>% summarise(mean(losatnl))
            
          hilda_housing_panel_e<-
              merge(hilda_housing_panel_e, other_reasons_for_moving, by = "xwaveid_by_wave", all = TRUE)
          
          rm(other_reasons_for_moving)                   
          
          hilda_housing_panel_e %<>%
            group_by(xwaveid) %>%
            arrange(wave) %>%
            mutate(change_in_household_type = if_else(household_type == lag(household_type, 1, order_by = wave), 1, 0), 
                   max_death_in_household_year = max(death_in_household_year)) %>%
            ungroup() %>%
            mutate(years_since_death_in_household_year = if_else(max_death_in_household_year>=0, wave - max_death_in_household_year, var(1)))
          
          
            
## 8. Moving house: ======
      
            
            
      # Various definitions of value of home:      
            
            #hsvalui	h	Property	DV: Home value ($) [imputed] [weighted topcode]
            #hsvalue	h	Property	Approximate value of home today [weighted topcode]
            #hsprice	h	Property	Price of home when purchased
            #hwhmvai	h	Property	DV: Household wealth: Home: Apportioned value [imputed] ($) [weighted topcode]
            
            #hilda_panel %>%
              #select(hwhmvai) %>%
              #describe()
            
            # hsvalui: missing = 0, zero = 92,287, mean = $351,986
            # hsvalue: missing = 0,                mean = $340,775
            # hsprice: missing = 217,524,               mean = $169,807
            # hwhmvai: Missing = 217,524, zero = 24,183, mean = 343,289
            
            # hwhmvai only adds 13 obs to hsvalui, but hsvalui adds 149,433 to hwhmvai. We'll use hsvalui. 
            
            #hilda_panel %>%
            #  mutate(missing_hsvalui = if_else(hsvalui==0, 1, 0), 
            #         waves = if_else(wave>=1, 1, 0)) %>%
            #  group_by(xwaveid_by_wave) %>%
            #  slice(1) %>%
            #  ungroup() %>%
            #  group_by(xwaveid) %>%
            #  mutate(n_missing_hsvalui = sum(missing_hsvalui), 
            #         n_waves = sum(waves)) %>%
            #  slice(1) %>%
            #  ungroup() %>%
            #  select(n_missing_hsvalui, n_waves) %>%
            #  table()
            
            # I need the 0's to be authentic NA's:
            hsvalui<- hilda_panel %>% select(hsvalui)
            hsvalui %<>%
              mutate(hsvalui = if_else(hsvalui==0, var(1), as.double(hsvalui))) # I'm using this to replace 0's with authentic NA's, as var(1)=NA.
            hilda_panel$hsvalui <- hsvalui$hsvalui 
            rm(hsvalui)
            
            # This function fills missing data with the closest non-NA:
            fill_with_closest_wave <- function(dat) {
              N <- length(dat)
              na.pos <- which(is.na(dat))
              if (length(na.pos) %in% c(0, N)) {
                return(dat)
              }
              non.na.pos <- which(!is.na(dat))
              intervals  <- findInterval(na.pos, non.na.pos,
                                         all.inside = TRUE)
              left.pos   <- non.na.pos[pmax(1, intervals)]
              right.pos  <- non.na.pos[pmin(N, intervals+1)]
              left.dist  <- na.pos - left.pos
              right.dist <- right.pos - na.pos
              
              dat[na.pos] <- ifelse(left.dist <= right.dist,
                                    dat[left.pos], dat[right.pos])
              return(dat)
            }

            
    
  # Did they move? 
    moving_variables <-
              hilda_panel %>%
              mutate(moved_house = if_else(hhmove=="[1] Moved" | mhli=="[1] Yes", 1, 0), # collected in every wave except wave 1
                     moved_more_than_100k = if_else(hhmovek>100, 1, 0), # collected in every wave except wave 1
                     years_at_current_address_rounded = if_else(hsyrcad>15, 15, if_else(hsyrcad>=0,round_any(hsyrcad, 1), 100)), # In 77k cases, hsyrcad is missing, but coded aas -10 or -7. 100 is my new NA number. 
                     year_moved_to_current_address = mhyr,
                     value_of_home = hsvalui) %>%
              group_by(xwaveid) %>%
              # Value of home: 
              arrange(wave) %>%
              mutate(value_of_home = if_else(!is.na(value_of_home), value_of_home, fill_with_closest_wave(value_of_home)),
              value_of_home = as.numeric(value_of_home), 
              change_in_home_value = if_else(wave==1, 0, value_of_home - lag(value_of_home, 1, order_by = wave))) %>% 
              # moved house, implied:
              mutate(moved_house_during_hilda = max(moved_house),
                     moved_house_implied = if_else(moved_house==1 | years_at_current_address_rounded==0 | lead(years_at_current_address_rounded, 1, order_by = wave)==1 | year_moved_to_current_address==year | lead(year_moved_to_current_address, 1, order_by = wave)==year ,1,0), 
                     moved_house_during_hilda_implied=if_else(max(moved_house)==1 | max(moved_house_implied)==1, 1, 0)) %>%
              ungroup() %>%
              select(xwaveid_by_wave, moved_house, moved_house_implied, moved_more_than_100k, years_at_current_address_rounded, moved_house_during_hilda, moved_house_during_hilda_implied, value_of_home, change_in_home_value)
    
    hilda_housing_panel_e %<>%
      group_by(xwaveid) %>%
      arrange(wave) %>%
      mutate(change_in_home_value = if_else(wave==1, 0, value_of_home - lag(value_of_home, 1, order_by = wave))) %>% 
      ungroup()
    

    
            # Investigating NA's in value_of_home:
            # This table shows that 11072/11824 come from obs with no value_of_home data
            # Only 772 NA's come from observations with some value_of_home data
            # These NA's precede the first piece of value_of_home data
                    #moving_variables %>% 
                    #  mutate(value_of_home_IS_NA = if_else(is.na(value_of_home), 1, 0), 
                    #         value_of_home_IS_ZERO = if_else(value_of_home==0, 1, 0)) %>%
                    #  group_by(xwaveid) %>%
                    #  mutate(prop_value_of_home_IS_NA = mean(value_of_home_IS_NA),
                    #         prop_value_of_home_IS_NA_eq_1 = if_else(prop_value_of_home_IS_NA==1, 1, 0)) %>%
                    #  slice(1) %>%
                    #  ungroup() %>%
                    #  select(value_of_home_IS_NA, prop_value_of_home_IS_NA_eq_1) %>%
                    #  table()
          
        hilda_housing_panel_e <-
            merge(hilda_housing_panel_e, moving_variables, by = "xwaveid_by_wave", all = TRUE)

        rm(moving_variables)
    

## 9. Wealth waves:  -----   
      # Number of wealth changes observed:   
        # We have 19,357 observations where owners move house (and enough info to see D(wealth))  
        hilda_housing_panel_e %<>%
          mutate(w2=if_else(wave==2, 1, 0), w6=if_else(wave==6, 1, 0), w10=if_else(wave==10, 1, 0), w14=if_else(wave==14, 1, 0)) %>%
          group_by(xwaveid) %>%
          mutate(w2_data = max(w2), w6_data = max(w6), w10_data = max(w10), w14_data = max(w14), first_wealth_change = if_else(w2_data==1 & w6_data==1, 1, 0), second_wealth_change = if_else(w6_data==1 & w10_data==1, 1, 0), third_wealth_change = if_else(w10_data==1 & w14_data==1, 1, 0), n_wealth_changes = first_wealth_change + second_wealth_change + third_wealth_change) %>%
          ungroup() %>%
          subset(select = -c(w2, w6, w10, w14, w2_data, w6_data, w10_data, w14_data, first_wealth_change, second_wealth_change, third_wealth_change))
          
## 10. Late additions:  -----
        
        raw_home_value_data<-
          hilda_panel %>%
          mutate(value_of_home_raw = hsvalui) %>%
          group_by(xwaveid) %>%
          arrange(wave) %>%
          mutate(change_in_home_value_raw = if_else(wave==1, 0, value_of_home_raw - lag(value_of_home_raw, 1, order_by = wave))) %>% 
          ungroup() %>%
          select(xwaveid_by_wave, change_in_home_value_raw)
        
        testing<-
          merge(hilda_housing_panel_e, raw_home_value_data, by = "xwaveid_by_wave", all = TRUE)
          
          
                                                  
        
        
###### Reloading this data: -----
#        load("~/Pensioner downsizing/Product_of_generating_variables_script.RData")
          