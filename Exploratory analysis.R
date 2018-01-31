###################### Exploratory analysis:

###### Basic sample size numbers:  -----
    # Hilda_panel: 
        tally(hilda_panel) # This is the same: tally(hilda_housing_panel)
        hilda_housing_panel %>% group_by(xwaveid) %>% slice(1) %>% ungroup() %>% tally()
        hilda_panel %>% select(wave) %>% table()
    
    # Extended hilda panel:
        tally(hilda_housing_panel_e)
        hilda_housing_panel_e %>% group_by(xwaveid_e) %>% slice(1) %>% ungroup() %>% tally()
        
    # Entry and exit into the dataset:
    hilda_housing_panel_e %>%
      group_by(xwaveid) %>%
      mutate(first_wave_in_dataset = min(wave), 
             last_wave_in_dataset = max(wave)) %>%
      slice(1) %>%
      ungroup() %>%
      select(first_wave_in_dataset, last_wave_in_dataset) %>%
      table()
    
###### Basic age numbers: -----
        
    # Sample size by age: 
        # N obs of 65+'s
        hilda_housing_panel_e %>% filter(over_65s==1) %>% tally()
        hilda_housing_panel_e %>% filter(over_65s==1) %>% group_by(xwaveid_by_wave) %>% slice(1)%>% ungroup()%>% tally()
        hilda_housing_panel_e %>% filter(over_65s==1) %>% group_by(xwaveid) %>% slice(1)%>% ungroup()%>% tally()
        # N obs of 65+'s by wave
        hilda_housing_panel_e %>% filter(over_65s==1) %>% select(wave) %>% table()
        # Obs by age decade by wave
        hilda_housing_panel_e %>% select(age_decade, wave) %>% table()
        # Number of people who are ever over 65:
        hilda_housing_panel_e %>% group_by(xwaveid) %>% slice(1) %>% ungroup() %>% filter(max_over_65s_status==1) %>% tally()
        hilda_housing_panel_e %>% filter(!is.na(over_65s))%>% dplyr::group_by(xwaveid_e) %>% dplyr::summarise(ever_over_65=max(over_65s)) %>% ungroup() %>% select(ever_over_65) %>% table()
            # If over 65 once, Number of waves for which they are over 65: 
            hilda_housing_panel_e %>%  filter(over_65s==1) %>% dplyr::group_by(xwaveid_e) %>% dplyr::mutate(number_of_waves_over_65 = n()) %>% slice(1) %>% dplyr::ungroup() %>% select(number_of_waves_over_65) %>% table()
    
##### How many wealth waves do we observe? ------   
            
    # Number of wealth changes observed:
            hilda_housing_panel_e %>%
              mutate(w2=if_else(wave==2, 1, 0), w6=if_else(wave==6, 1, 0), w10=if_else(wave==10, 1, 0), w14=if_else(wave==14, 1, 0)) %>%
              group_by(xwaveid) %>%
              mutate(w2_data = max(w2), w6_data = max(w6), w10_data = max(w10), w14_data = max(w14), first_wealth_change = if_else(w2_data==1 & w6_data==1, 1, 0), second_wealth_change = if_else(w6_data==1 & w10_data==1, 1, 0), third_wealth_change = if_else(w10_data==1 & w14_data==1, 1, 0), n_wealth_changes = first_wealth_change + second_wealth_change + third_wealth_change) %>%
              slice(1) %>%
              ungroup() %>%
              select(n_wealth_changes) %>%
              table()
            
            # Under 65's
            hilda_housing_panel_e %>%
              filter(max_over_65s_status==0) %>%
              mutate(w2=if_else(wave==2, 1, 0), w6=if_else(wave==6, 1, 0), w10=if_else(wave==10, 1, 0), w14=if_else(wave==14, 1, 0)) %>%
              group_by(xwaveid) %>%
              mutate(w2_data = max(w2), w6_data = max(w6), w10_data = max(w10), w14_data = max(w14), first_wealth_change = if_else(w2_data==1 & w6_data==1, 1, 0), second_wealth_change = if_else(w6_data==1 & w10_data==1, 1, 0), third_wealth_change = if_else(w10_data==1 & w14_data==1, 1, 0), n_wealth_changes = first_wealth_change + second_wealth_change + third_wealth_change) %>%
              slice(1) %>%
              ungroup() %>%
              select(n_wealth_changes) %>%
              table()
            
            # Over 65s:
            hilda_housing_panel_e %>%
              filter(max_over_65s_status==1) %>%
              mutate(w2=if_else(wave==2, 1, 0), w6=if_else(wave==6, 1, 0), w10=if_else(wave==10, 1, 0), w14=if_else(wave==14, 1, 0)) %>%
              group_by(xwaveid) %>%
              mutate(w2_data = max(w2), w6_data = max(w6), w10_data = max(w10), w14_data = max(w14), first_wealth_change = if_else(w2_data==1 & w6_data==1, 1, 0), second_wealth_change = if_else(w6_data==1 & w10_data==1, 1, 0), third_wealth_change = if_else(w10_data==1 & w14_data==1, 1, 0), n_wealth_changes = first_wealth_change + second_wealth_change + third_wealth_change) %>%
              slice(1) %>%
              ungroup() %>%
              select(n_wealth_changes) %>%
              table()
            
##### Home owners, movers, and downsizers: -----            
        
            # We have 187,230 (155,352) observations of households who own house 
            hilda_housing_panel_e %>%
              filter(home_ownership_status=="Owner") %>%
              group_by(xwaveid_by_wave) %>%
              slice(1) %>%
              ungroup() %>%
              tally()
           
            # This information relates to 27,161 (15,845) households
            hilda_housing_panel_e %>%
              filter(home_ownership_status=="Owner") %>%
              group_by(xwaveid) %>%
              slice(1) %>%
              ungroup() %>%
              tally()
            
            # We have 23,473 (17,984) observations where households move house 
            hilda_housing_panel_e %>%
              filter(home_ownership_status=="Owner" & moved_house_implied==1) %>%
              group_by(xwaveid_by_wave) %>%
              slice(1) %>%
              ungroup() %>%
              tally()
            
            # Involving 14,541 (9,803) households:
            hilda_housing_panel_e %>%
              filter(home_ownership_status=="Owner" & moved_house_implied==1) %>%
              group_by(xwaveid) %>%
              slice(1) %>%
              ungroup() %>%
              tally()
            
            # This tells me that this cohort of people moves 1.6 times on average:
            hilda_housing_panel_e %>%
              filter(home_ownership_status=="Owner" & moved_house_implied==1) %>%
              group_by(xwaveid_by_wave) %>%
              slice(1) %>%
              ungroup() %>%
              group_by(xwaveid) %>%
              group_size() %>%
              describe()
              
           # In 4,402 of the 23,473 moves, people downsized: (5,746 had no change in value; 9,128 increased home value; 4197 NA's)
           hilda_housing_panel_e %>%
              filter(home_ownership_status=="Owner" & moved_house_implied==1) %>%
              mutate(downsizing_status = if_else(moved_house_implied==1 & change_in_home_value<0, 1, 
                                                 if_else(moved_house_implied==1 & change_in_home_value==0, 0.5, 
                                                         if_else(moved_house_implied==1 & change_in_home_value>0, 0, var(1))))) %>%
              group_by(xwaveid_by_wave) %>%
              slice(1) %>%
              ungroup() %>%
              select(downsizing_status) %>%
              #is.na() %>%
              table()
           
           # These 4,402 downsizes were completed by 1850 households. (4418 had no change in value; 4376 increased home value. 4197 NA's)
           hilda_housing_panel_e %>%
             filter(home_ownership_status=="Owner" & moved_house_implied==1) %>%
             mutate(downsizing_status = if_else(moved_house_implied==1 & change_in_home_value<0, 1, 
                                                if_else(moved_house_implied==1 & change_in_home_value==0, 0.5, 
                                                        if_else(moved_house_implied==1 & change_in_home_value>0, 0, var(1))))) %>%
             group_by(xwaveid) %>%
             slice(1) %>%
             ungroup() %>%
             select(downsizing_status) %>%
             #is.na() %>%
             table()
           
##### Is there any more information we can extract about downsizing? -----           
           
           
                       ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
                       ## ## ## ## #### EXPLORING DOWNSIZES THAT DO NOT COINCIDE WITH MOVES ### ## ## ## ## ## ## ## # 
                       ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
                      
                        # Number of moves vs number of downsizes by household: 
                       hilda_housing_panel_e %>%
                         mutate(owner_move = if_else(home_ownership_status=="Owner" & moved_house_implied==1, 1, 0),
                                owner_move = if_else(is.na(owner_move), 0, owner_move),
                                downsized = if_else(is.na(change_in_home_value),0, if_else(change_in_home_value<0, 1, 0))) %>%
                         group_by(xwaveid_by_wave) %>%
                         slice(1) %>%
                         ungroup() %>%
                         group_by(xwaveid) %>%
                         mutate(owner_move_max = max(owner_move), 
                                downsized_max = max(downsized), 
                                n_owner_move = sum(owner_move), 
                                n_downsizes = sum(downsized)) %>%
                         slice(1) %>%
                         ungroup() %>%
                         filter(owner_move_max ==1) %>%
                         select(n_owner_move, n_downsizes) %>%
                         table()
                       
                       # Number of downsizes alongside move vs in neighbouring periods: 
                       
                       # No lags: 4402 downsizes
                       hilda_housing_panel_e %>%
                         mutate(owner_move = if_else(home_ownership_status=="Owner" & moved_house_implied==1, 1, 0), 
                                downsized = if_else(is.na(change_in_home_value),-1, if_else(change_in_home_value<0, 1, 0))) %>%
                         filter(owner_move==1) %>%
                         group_by(xwaveid_by_wave) %>%
                         slice(1) %>%
                         ungroup() %>%
                         select(downsized) %>%
                         table()
            
                       # lag(1): 4549 downsizes
                       hilda_housing_panel_e %>%
                         mutate(owner_move = if_else(home_ownership_status=="Owner" & moved_house_implied==1, 1, 0), 
                                downsized = if_else(is.na(change_in_home_value),-1, if_else(change_in_home_value<0, 1, 0))) %>%
                         filter(owner_move==1) %>%
                         group_by(xwaveid) %>%
                         mutate(downsized = if_else(!is.na(lag(downsized,1,order_by=wave)), if_else(lag(downsized,1,order_by=wave)==1, 1, downsized), downsized)) %>%
                         ungroup() %>%
                         group_by(xwaveid_by_wave) %>%
                         slice(1) %>%
                         ungroup() %>%
                         select(downsized) %>%
                         table()
                       
                       # lead(1): 4546 downsizes
                       hilda_housing_panel_e %>%
                         mutate(owner_move = if_else(home_ownership_status=="Owner" & moved_house_implied==1, 1, 0), 
                                downsized = if_else(is.na(change_in_home_value),-1, if_else(change_in_home_value<0, 1, 0))) %>%
                         filter(owner_move==1) %>%
                         group_by(xwaveid) %>%
                         mutate(downsized = if_else(!is.na(lead(downsized,1,order_by=wave)), if_else(lead(downsized,1,order_by=wave)==1, 1, downsized), downsized)) %>%
                         ungroup() %>%
                         group_by(xwaveid_by_wave) %>%
                         slice(1) %>%
                         ungroup() %>%
                         select(downsized) %>%
                         table()
                       
                       # lag(1) and lead(1): 4680 downsizes
                       hilda_housing_panel_e %>%
                         mutate(owner_move = if_else(home_ownership_status=="Owner" & moved_house_implied==1, 1, 0), 
                                downsized = if_else(is.na(change_in_home_value),-1, if_else(change_in_home_value<0, 1, 0))) %>%
                         filter(owner_move==1) %>%
                         group_by(xwaveid) %>%
                         mutate(downsized = if_else(!is.na(lag(downsized,1,order_by=wave)), if_else(lag(downsized,1,order_by=wave)==1, 1, downsized), downsized), 
                                downsized = if_else(!is.na(lead(downsized,1,order_by=wave)), if_else(lead(downsized,1,order_by=wave)==1, 1, downsized), downsized)) %>%
                         ungroup() %>%
                         group_by(xwaveid_by_wave) %>%
                         slice(1) %>%
                         ungroup() %>%
                         select(downsized) %>%
                         table()
                       
                       # lag(1, 2): 5293 downsizes (744 more than 1 lag)
                       hilda_housing_panel_e %>%
                         mutate(owner_move = if_else(home_ownership_status=="Owner" & moved_house_implied==1, 1, 0), 
                                downsized = if_else(is.na(change_in_home_value),-1, if_else(change_in_home_value<0, 1, 0))) %>%
                         filter(owner_move==1) %>%
                         group_by(xwaveid) %>%
                         mutate(downsized = if_else(!is.na(lag(downsized,1,order_by=wave)), if_else(lag(downsized,1,order_by=wave)==1, 1, downsized), downsized), 
                                downsized = if_else(!is.na(lag(downsized,2,order_by=wave)), if_else(lag(downsized,2,order_by=wave)==1, 1, downsized), downsized)) %>%
                         ungroup() %>%
                         group_by(xwaveid_by_wave) %>%
                         slice(1) %>%
                         ungroup() %>%
                         select(downsized) %>%
                         table()
                       
                       # lead(1, 2): 5622 downsizes (1076 more than 1 lead)
                       hilda_housing_panel_e %>%
                         mutate(owner_move = if_else(home_ownership_status=="Owner" & moved_house_implied==1, 1, 0), 
                                downsized = if_else(is.na(change_in_home_value),-1, if_else(change_in_home_value<0, 1, 0))) %>%
                         filter(owner_move==1) %>%
                         group_by(xwaveid) %>%
                         mutate(downsized = if_else(!is.na(lead(downsized,1,order_by=wave)), if_else(lead(downsized,1,order_by=wave)==1, 1, downsized), downsized), 
                                downsized = if_else(!is.na(lead(downsized,2,order_by=wave)), if_else(lead(downsized,2,order_by=wave)==1, 1, downsized), downsized)) %>%
                         ungroup() %>%
                         group_by(xwaveid_by_wave) %>%
                         slice(1) %>%
                         ungroup() %>%
                         select(downsized) %>%
                         table()
                       
                       # lag(1,2) and lead(1,2): 6416 downsizes
                       hilda_housing_panel_e %>%
                         mutate(owner_move = if_else(home_ownership_status=="Owner" & moved_house_implied==1, 1, 0), 
                                downsized = if_else(is.na(change_in_home_value),-1, if_else(change_in_home_value<0, 1, 0))) %>%
                         filter(owner_move==1) %>%
                         group_by(xwaveid) %>%
                         mutate(downsized = if_else(!is.na(lag(downsized,1,order_by=wave)), if_else(lag(downsized,1,order_by=wave)==1, 1, downsized), downsized), 
                                downsized = if_else(!is.na(lag(downsized,2,order_by=wave)), if_else(lag(downsized,2,order_by=wave)==1, 1, downsized), downsized), 
                                downsized = if_else(!is.na(lead(downsized,1,order_by=wave)), if_else(lead(downsized,1,order_by=wave)==1, 1, downsized), downsized), 
                                downsized = if_else(!is.na(lead(downsized,2,order_by=wave)), if_else(lead(downsized,2,order_by=wave)==1, 1, downsized), downsized)) %>%
                         ungroup() %>%
                         group_by(xwaveid_by_wave) %>%
                         slice(1) %>%
                         ungroup() %>%
                         select(downsized) %>%
                         table()
                       
                       # lag(1, 2, 3): 6032 downsizes (739 more than 2 lags)
                       hilda_housing_panel_e %>%
                         mutate(owner_move = if_else(home_ownership_status=="Owner" & moved_house_implied==1, 1, 0), 
                                downsized = if_else(is.na(change_in_home_value),-1, if_else(change_in_home_value<0, 1, 0))) %>%
                         filter(owner_move==1) %>%
                         group_by(xwaveid) %>%
                         mutate(downsized = if_else(!is.na(lag(downsized,1,order_by=wave)), if_else(lag(downsized,1,order_by=wave)==1, 1, downsized), downsized), 
                                downsized = if_else(!is.na(lag(downsized,2,order_by=wave)), if_else(lag(downsized,2,order_by=wave)==1, 1, downsized), downsized),
                                downsized = if_else(!is.na(lag(downsized,3,order_by=wave)), if_else(lag(downsized,3,order_by=wave)==1, 1, downsized), downsized)) %>%
                         ungroup() %>%
                         group_by(xwaveid_by_wave) %>%
                         slice(1) %>%
                         ungroup() %>%
                         select(downsized) %>%
                         table()
                       
                       # lead(1, 2, 3): 5622 downsizes (1127 more than 2 leads)
                       hilda_housing_panel_e %>%
                         mutate(owner_move = if_else(home_ownership_status=="Owner" & moved_house_implied==1, 1, 0), 
                                downsized = if_else(is.na(change_in_home_value),-1, if_else(change_in_home_value<0, 1, 0))) %>%
                         filter(owner_move==1) %>%
                         group_by(xwaveid) %>%
                         mutate(downsized = if_else(!is.na(lead(downsized,1,order_by=wave)), if_else(lead(downsized,1,order_by=wave)==1, 1, downsized), downsized), 
                                downsized = if_else(!is.na(lead(downsized,2,order_by=wave)), if_else(lead(downsized,2,order_by=wave)==1, 1, downsized), downsized),
                                downsized = if_else(!is.na(lead(downsized,3,order_by=wave)), if_else(lead(downsized,3,order_by=wave)==1, 1, downsized), downsized)) %>%
                         ungroup() %>%
                         group_by(xwaveid_by_wave) %>%
                         slice(1) %>%
                         ungroup() %>%
                         select(downsized) %>%
                         table()
                       
                       # lag(1,2,3) and lead(1,2,3): 8131 downsizes
                       hilda_housing_panel_e %>%
                         mutate(owner_move = if_else(home_ownership_status=="Owner" & moved_house_implied==1, 1, 0), 
                                downsized = if_else(is.na(change_in_home_value),-1, if_else(change_in_home_value<0, 1, 0))) %>%
                         filter(owner_move==1) %>%
                         group_by(xwaveid) %>%
                         mutate(downsized = if_else(!is.na(lag(downsized,1,order_by=wave)), if_else(lag(downsized,1,order_by=wave)==1, 1, downsized), downsized), 
                                downsized = if_else(!is.na(lag(downsized,2,order_by=wave)), if_else(lag(downsized,2,order_by=wave)==1, 1, downsized), downsized),
                                downsized = if_else(!is.na(lag(downsized,3,order_by=wave)), if_else(lag(downsized,3,order_by=wave)==1, 1, downsized), downsized),
                                downsized = if_else(!is.na(lead(downsized,1,order_by=wave)), if_else(lead(downsized,1,order_by=wave)==1, 1, downsized), downsized), 
                                downsized = if_else(!is.na(lead(downsized,2,order_by=wave)), if_else(lead(downsized,2,order_by=wave)==1, 1, downsized), downsized),
                                downsized = if_else(!is.na(lead(downsized,3,order_by=wave)), if_else(lead(downsized,3,order_by=wave)==1, 1, downsized), downsized)) %>%
                         ungroup() %>%
                         group_by(xwaveid_by_wave) %>%
                         slice(1) %>%
                         ungroup() %>%
                         select(downsized) %>%
                         table()
           
##### Home owners, movers and downsizers by age -----
                       
                    
                       
          # DEFINITIONS: Over 65+ home owners: 
          hilda_housing_panel_e %>%
              mutate(over_65_home_owner = if_else(individual_home_ownership_details=="Owns the house they live in" & age>=65), 
                     old_def = if_else(home_ownership_status=="Owner" & over_65s, 1, 0)) %>%
              select(over_65_home_owner, old_def) %>%
              table()
                       
          # Full sample by age (# households by wave)
         hilda_housing_panel_e %>%
           group_by(xwaveid_by_wave) %>%
           slice(1) %>%
           ungroup() %>%
           select(average_age_decade_of_household) %>%
           table()
         
           
          # Home owners by age (# households by wave):
           hilda_housing_panel_e %>%
             filter(home_ownership_status=="Owner") %>%
             group_by(xwaveid_by_wave) %>%
             slice(1) %>%
             ungroup() %>%
             select(average_age_decade_of_household) %>%
             table()
           
          # Home owners by age (# households):
           hilda_housing_panel_e %>%
             filter(home_ownership_status=="Owner") %>%
             group_by(xwaveid) %>%
             slice(1) %>%
             ungroup() %>%
             select(average_age_decade_of_household) %>%
             table()
           
           # Home owners who move by age (# households by wave):
           hilda_housing_panel_e %>%
             filter(home_ownership_status=="Owner" & (moved_house_implied==1 | moved_house==1)) %>%
             group_by(xwaveid_by_wave) %>%
             slice(1) %>%
             ungroup() %>%
             select(average_age_decade_of_household) %>%
             table()
           
           # Home owners who move by age (# households):
           hilda_housing_panel_e %>%
             filter(home_ownership_status=="Owner" & moved_house_implied==1) %>%
             group_by(xwaveid) %>%
             slice(1) %>%
             ungroup() %>%
             select(average_age_decade_of_household) %>%
             table()
           
           
           # Downsize status by age for each of the 23,473 moves (# households by wave):
           hilda_housing_panel_e %>%
             filter(home_ownership_status=="Owner" & moved_house_implied==1) %>%
             mutate(downsizing_status = if_else(moved_house_implied==1 & change_in_home_value<0, 1, 
                                                if_else(moved_house_implied==1 & change_in_home_value==0, 0.5, 
                                                        if_else(moved_house_implied==1 & change_in_home_value>0, 0, var(1))))) %>%
             group_by(xwaveid_by_wave) %>%
             slice(1) %>%
             ungroup() %>%
             select(downsizing_status, average_age_decade_of_household) %>%
             table()
           
           # Downsize status by age for each of the 14541 households who moved (# households):
           hilda_housing_panel_e %>%
             filter(home_ownership_status=="Owner" & moved_house_implied==1) %>%
             mutate(downsizing_status = if_else(moved_house_implied==1 & change_in_home_value<0, 1, 
                                                if_else(moved_house_implied==1 & change_in_home_value==0, 0.5, 
                                                        if_else(moved_house_implied==1 & change_in_home_value>0, 0, var(1))))) %>%
             group_by(xwaveid) %>%
             slice(1) %>%
             ungroup() %>%
             select(downsizing_status) %>%
             table()   
           
           hilda_housing_panel_e %>%
             filter(average_age_decade_of_household==100) %>%
             select(age, xwaveid) %>%
             table()
           
           # Downsizing status among under and over 65s:
           hilda_housing_panel_e %>%
             group_by(xwaveid_by_wave) %>%
             slice(1) %>%
             ungroup() %>%
             mutate(downsizing_status = if_else(moved_house_implied==1 & change_in_home_value<0, 1, 
                                                if_else(moved_house_implied==1 & change_in_home_value==0, 0.5, 
                                                        if_else(moved_house_implied==1 & change_in_home_value>0, 0, var(1))))) %>%
             select(downsizing_status, over_65s) %>%
             table()

           hilda_housing_panel_e %>%
             group_by(xwaveid_by_wave) %>%
             slice(1) %>%
             ungroup() %>%
             mutate(downsizing_status = if_else(moved_house_implied==1 & change_in_home_value_raw<0, 1, 
                                                if_else(moved_house_implied==1 & is.na(change_in_home_value_raw), 0.4, 
                                                if_else(moved_house_implied==1 & change_in_home_value_raw==0, 0.5, 
                                                        if_else(moved_house_implied==1 & change_in_home_value_raw>0, 0, var(1)))))) %>%
             select(downsizing_status, over_65s) %>%
             table()
           
           # NOTE: Move is actively recorded for 73% of upsizes, 77% of downsizes and 90% of those with no change in home value. 
            
           ## Key numbers for the 65+ age group: 
           
           # Sample:
           hilda_housing_panel_e %>%
             filter(over_65s==1) %>%
             group_by(xwaveid_by_wave) %>%
             slice(1) %>%
             ungroup() %>%
             tally()
           
           # Home Owners: 
           hilda_housing_panel_e %>%
             filter(over_65s==1) %>%
             filter(home_ownership_status=="Owner") %>%
             group_by(xwaveid_by_wave) %>%
             slice(1) %>%
             ungroup() %>%
             tally()
           
           # Movers: 
           hilda_housing_panel_e %>%
             filter(over_65s==1) %>%
             filter(home_ownership_status=="Owner" & (moved_house_implied==1 | moved_house==1)) %>%
             group_by(xwaveid_by_wave) %>%
             slice(1) %>%
             ungroup() %>%
             tally()
           
           # Downsizers:
           hilda_housing_panel_e %>%
             filter(home_ownership_status=="Owner" & (moved_house_implied==1 | moved_house==1)) %>%
             mutate(downsizing_status = if_else(moved_house_implied==1 & change_in_home_value<0, 1, 
                                                if_else(moved_house_implied==1 & change_in_home_value==0, 0.5, 
                                                        if_else(moved_house_implied==1 & change_in_home_value>0, 0, var(1))))) %>%
             filter(downsizing_status==0)%>%
             filter(over_65s==1) %>%
             filter(wealth_wave_n>=1) %>%
             group_by(xwaveid_by_wave) %>%
             slice(1) %>%
             ungroup() %>%
             tally()

           
           
#### Some miscellaneous deep dives: ------           
           
           ## Deep dive into move vs move implied: 
          hilda_panel %>%
             mutate(moved_house = if_else(hhmove=="[1] Moved" | mhli=="[1] Yes", 1, 0), # collected in every wave except wave 1
                    years_at_current_address_rounded = if_else(hsyrcad>15, 15, if_else(hsyrcad>=0,round_any(hsyrcad, 1), 100)),
                    year_moved_to_current_address = mhyr) %>%
             group_by(xwaveid) %>%
             # Value of home: 
             arrange(wave) %>%
             # moved house, implied:
             mutate(moved_house_implied = if_else(years_at_current_address_rounded==0 | lead(years_at_current_address_rounded, 1, order_by = wave)==1 | year_moved_to_current_address==year | lead(year_moved_to_current_address, 1, order_by = wave)==year ,1,0)) %>%
             ungroup() %>%
             group_by(xwaveid_by_wave)%>%
             slice(1)%>%
             ungroup() %>%
             select(moved_house, moved_house_implied) %>%
             table()
           
          # Does the imputed data pick up neighbouring changes in house value?
         testing<-
           hilda_housing_panel_e %>%
            mutate(downsize_event = if_else(change_in_home_value<0, 1, 0), 
                   upsize_event = if_else(change_in_home_value>0, 1, 0), 
                   moved_house = if_else(is.na(moved_house),0, moved_house),
                   moved_house_implied = if_else(is.na(moved_house_implied),0, moved_house_implied))  %>%
            mutate(downsizing_status = if_else(moved_house_implied==1 & change_in_home_value<0, 1,
                                                       if_else(moved_house_implied==1 & change_in_home_value==0, 0.5, 
                                                               if_else(moved_house_implied==1 & change_in_home_value>0, 0, var(1))))) %>%
            group_by(xwaveid_by_wave)%>%
            slice(1) %>%
            ungroup() %>%
            group_by(xwaveid)%>%
            mutate(n_downsize_events = sum(downsize_event), 
                   n_upsize_events = sum(upsize_event), 
                   n_downsize_or_upsize_events = n_downsize_events + n_upsize_events, 
                   n_moves = sum(moved_house),
                   n_moves_implied = sum(moved_house_implied), 
                   n_downsize_or_upsize_events_V_n_of_moves = if_else(n_downsize_or_upsize_events>n_moves, 1, 
                                                                      if_else(n_downsize_or_upsize_events<n_moves, -1,  0)), 
                   n_downsize_or_upsize_events_V_n_of_moves_implied = if_else(n_downsize_or_upsize_events>moved_house_implied, 1, 
                                                                      if_else(n_downsize_or_upsize_events<moved_house_implied, -1,  0))) %>%
            ungroup() 
         
         testing %>%
            select(n_downsize_or_upsize_events_V_n_of_moves_implied, downsizing_status) %>%
            table()
         
         ####
         # Normally there's many more changes in value than moves. 
         # That's not necessarily the case with those where we D(value) is imputed: 
         # it's equally common to observe *less* changes in value than moves. 
         # That suggests some of these moves didn't happen. 
         ####
         # When implied moves are also considered, 
         # we *only* observe more moves than changes in home values among those that have no change in value during the move.... 
         
         
         testing<-
           hilda_housing_panel_e %>%
           mutate(downsize_event = if_else(change_in_home_value_raw<0, 1, 0), 
                  upsize_event = if_else(change_in_home_value_raw>0, 1, 0), 
                  moved_house = if_else(is.na(moved_house),0, moved_house),
                  moved_house_implied = if_else(is.na(moved_house_implied),0, moved_house_implied))  %>%
           mutate(downsizing_status = if_else(moved_house_implied==1 & change_in_home_value_raw<0, 1,
                                              if_else(moved_house_implied==1 & change_in_home_value_raw==0, 0.5, 
                                                      if_else(moved_house_implied==1 & change_in_home_value_raw>0, 0, var(1))))) %>%
           group_by(xwaveid_by_wave)%>%
           slice(1) %>%
           ungroup() %>%
           group_by(xwaveid)%>%
           mutate(n_downsize_events = sum(downsize_event), 
                  n_upsize_events = sum(upsize_event), 
                  n_downsize_or_upsize_events = n_downsize_events + n_upsize_events, 
                  n_moves = sum(moved_house),
                  n_moves_implied = sum(moved_house_implied), 
                  n_downsize_or_upsize_events_V_n_of_moves = if_else(n_downsize_or_upsize_events>n_moves, 1, 
                                                                     if_else(n_downsize_or_upsize_events<n_moves, -1,  0)), 
                  n_downsize_or_upsize_events_V_n_of_moves_implied = if_else(n_downsize_or_upsize_events>moved_house_implied, 1, 
                                                                             if_else(n_downsize_or_upsize_events<moved_house_implied, -1,  0))) %>%
           ungroup() 
         
         testing %>%
           select(n_downsize_or_upsize_events_V_n_of_moves_implied, downsizing_status) %>%
           table()
         
         #### 
         # Normally there's many more changes in value than moves. 
         # That's not necessarily the case with those where we D(value) is imputed: 
         # it's equally common to observe *less* changes in value than moves. 
         # That suggests some of these moves didn't happen. 
         ####
         
         
            testing<-
            hilda_housing_panel_e %>%
              mutate(xwaveid_by_wave = as.numeric(xwaveid)+wave/100)
            
            str(hilda_housing_panel_e)
            
            
              
            
          
                       
#### Summary stats - 65+ vs others.... NET WORTH: ----
 
    hilda_housing_panel_e %>%
      filter(over_65s==1) %>%
      group_by(xwaveid_by_wave) %>%
      slice(1) %>%
      ungroup() %>%
      select(net_worth) %>%
      is.na() %>%
      table() # 73% NAs.
    
    hilda_housing_panel_e %>%
      filter(over_65s==1) %>%
      group_by(xwaveid_by_wave) %>%
      slice(1) %>%
      ungroup() %>%
      group_by(xwaveid) %>%
      mutate(net_worth_filled = if_else(!is.na(net_worth), net_worth, fill_with_closest_wave(net_worth))) %>%
      ungroup() %>%
      select(net_worth_filled) %>%
      is.na() %>%
      table() #15% NAs
      
    # Net worth: under 65s vs over 65s:
    hilda_housing_panel_e %>%
      group_by(xwaveid_by_wave) %>%
      slice(1) %>%
      ungroup() %>%
      filter(!is.na(net_worth)) %>% # Here we remove NA's rather than using the filled variable so that we don't doubly weight info. 
      group_by(xwaveid) %>%
      mutate(average_net_worth = mean(net_worth), 
             rounded_average_net_worth = round_any(average_net_worth, 100000)) %>%
      slice(1) %>%
      ungroup() %>%
      select(rounded_average_net_worth, max_over_65s_status) %>%
      table()

                  # AVERAGE net worth - o65s (not):   818732.1  (530769.2)   
                  hilda_housing_panel_e %>%
                    group_by(xwaveid_by_wave) %>% slice(1) %>%  ungroup() %>%
                    filter(!is.na(net_worth)) %>% # Here we remove NA's rather than using the filled variable so that we don't doubly weight info. 
                    group_by(xwaveid) %>% mutate(average_net_worth = mean(net_worth), rounded_average_net_worth = round_any(average_net_worth, 100000)) %>% slice(1) %>% ungroup() %>% 
                    group_by(max_over_65s_status) %>% 
                    summarise(mean(rounded_average_net_worth))
                  
    # Net worth of over 65s: home owners vs not: 
    hilda_housing_panel_e %>%
      filter(max_over_65s_status==1)%>%
      mutate(home_ownership_numeric = if_else(home_ownership_status=="Owner", 1, 0)) %>%
      group_by(xwaveid_by_wave) %>%
      slice(1) %>%
      ungroup() %>%
      filter(!is.na(net_worth)) %>% # Here we remove NA's rather than using the filled variable so that we don't doubly weight info. 
      group_by(xwaveid) %>%
      mutate(max_home_ownership_status_numeric = max(home_ownership_numeric)) %>%
      mutate(average_net_worth = mean(net_worth), 
             rounded_average_net_worth = round_any(average_net_worth, 100000)) %>%
      slice(1) %>%
      ungroup() %>%
      select(rounded_average_net_worth, max_home_ownership_status_numeric) %>%
      table()
    
              # AVERAGE net worth - owners (not):  956315.7 (90410.96)
              hilda_housing_panel_e %>%
                filter(max_over_65s_status==1)%>% mutate(home_ownership_numeric = if_else(home_ownership_status=="Owner", 1, 0)) %>% 
                group_by(xwaveid_by_wave) %>% slice(1) %>%  ungroup() %>% 
                filter(!is.na(net_worth)) %>% # Here we remove NA's rather than using the filled variable so that we don't doubly weight info. 
                group_by(xwaveid) %>% mutate(max_home_ownership_status_numeric = max(home_ownership_numeric)) %>% mutate(average_net_worth = mean(net_worth), rounded_average_net_worth = round_any(average_net_worth, 100000)) %>% slice(1) %>% ungroup() %>%
                group_by(max_home_ownership_status_numeric) %>%#==1) %>%
                summarise(mean(rounded_average_net_worth))
                
      
    # Net worth of over 65 home owner - movers: downsizers vs not: 
    hilda_housing_panel_e %>%
      filter(max_over_65s_status==1)%>%
      mutate(home_ownership_numeric = if_else(home_ownership_status=="Owner", 1, 0)) %>%
      group_by(xwaveid_by_wave) %>%
      slice(1) %>%
      ungroup() %>%
      filter(!is.na(net_worth)) %>% # Here we remove NA's rather than using the filled variable so that we don't doubly weight info. 
      group_by(xwaveid) %>%
      mutate(max_home_ownership_status_numeric = max(home_ownership_numeric), 
             max_moved_house_implied = max(moved_house_implied)) %>%
      mutate(average_net_worth = mean(net_worth), 
             rounded_average_net_worth = round_any(average_net_worth, 100000)) %>%
      mutate(downsizing_status = if_else(moved_house_implied==1 & change_in_home_value<0, 1, 
                                         if_else(moved_house_implied==1 & change_in_home_value==0, 0.5, 
                                                 if_else(moved_house_implied==1 & change_in_home_value>0, 0, 
                                                         if_else(max_moved_house_implied==0, -1, var(1)))))) %>%
      slice(1) %>%
      ungroup() %>%
      filter(max_home_ownership_status_numeric==1) %>%
      select(rounded_average_net_worth, downsizing_status) %>%
      table()
    
    # AVERAGE net worth - downsizers [upsizers] (non-movers): 754782.6 [1059534.9] (1009764.0)
    hilda_housing_panel_e %>%
    filter(max_over_65s_status==1)%>%
      mutate(home_ownership_numeric = if_else(home_ownership_status=="Owner", 1, 0)) %>%
      group_by(xwaveid_by_wave) %>%
      slice(1) %>%
      ungroup() %>%
      filter(!is.na(net_worth)) %>% # Here we remove NA's rather than using the filled variable so that we don't doubly weight info. 
      group_by(xwaveid) %>%
      mutate(max_home_ownership_status_numeric = max(home_ownership_numeric), 
             max_moved_house_implied = max(moved_house_implied)) %>%
      mutate(average_net_worth = mean(net_worth), 
             rounded_average_net_worth = round_any(average_net_worth, 100000)) %>%
      mutate(downsizing_status = if_else(moved_house_implied==1 & change_in_home_value<0, 1, 
                                         if_else(moved_house_implied==1 & change_in_home_value==0, 0.5, 
                                                 if_else(moved_house_implied==1 & change_in_home_value>0, 0, 
                                                         if_else(max_moved_house_implied==0, -1, var(1)))))) %>%
      slice(1) %>%
      ungroup() %>%
      filter(max_home_ownership_status_numeric==1) %>%
      group_by(downsizing_status) %>%
      summarise(mean(rounded_average_net_worth))
            
            
#### Summary stats - misc variables: 
    
    #!!!!!!! non_financial_assets_less_cars
    
    # Defining the subsamples
    full_sample <- hilda_housing_panel_e %>% group_by(xwaveid_by_wave) %>%  slice(1) %>%  ungroup()
    
    full_sample_o65s <- hilda_housing_panel_e %>% filter(over_65s==1) %>% group_by(xwaveid_by_wave) %>% slice(1) %>%  ungroup()
    
    full_sample_o65s_home_owners <- hilda_housing_panel_e %>% filter(over_65s==1 & home_ownership_status=="Owner") %>% group_by(xwaveid_by_wave) %>% slice(1) %>% ungroup()
    
    full_sample_o65s_home_owner_movers <- hilda_housing_panel_e %>% filter(home_ownership_status=="Owner" & moved_house_implied==1 & max_over_65s_status==1) %>% group_by(xwaveid_by_wave) %>% slice(1) %>% ungroup()
    
    full_sample_o65s_home_owner_downsizer <-hilda_housing_panel_e %>% mutate(downsizing_status = if_else(moved_house_implied==1 & change_in_home_value<0, 1,  
                                                                                                         if_else(moved_house_implied==1 & change_in_home_value==0, 0.5,  
                                                                                                                 if_else(moved_house_implied==1 & change_in_home_value>0, 0, var(1))))) %>% filter(home_ownership_status=="Owner" & moved_house_implied==1 & max_over_65s_status==1 & downsizing_status==1) %>% group_by(xwaveid_by_wave) %>% slice(1) %>% ungroup()
    # Sample sizes: 
    full_sample %>% tally()
    full_sample_o65s %>% tally()
    full_sample_o65s_home_owners %>% tally()
    full_sample_o65s_home_owner_movers %>% tally()
    full_sample_o65s_home_owner_downsizer %>% tally()
    
    # Descriptive stats, financial stress: 
    
    full_sample %>% select(n_ppl_w_health_condition, total_health_expenditure, insurance_expenditure,  last_overdue_bills_ww,    last_eating_out_ww,   major_worsening_of_finances_this_yr) %>% describe()
    
    full_sample_o65s %>% select(n_ppl_w_health_condition, total_health_expenditure, insurance_expenditure,  last_overdue_bills_ww,    last_eating_out_ww,   major_worsening_of_finances_this_yr) %>% describe()
    
    full_sample_o65s_home_owners %>% select(n_ppl_w_health_condition, total_health_expenditure, insurance_expenditure,  last_overdue_bills_ww,    last_eating_out_ww,   major_worsening_of_finances_this_yr) %>% describe()
    
    full_sample_o65s_home_owner_movers %>% select(n_ppl_w_health_condition, total_health_expenditure, insurance_expenditure,  last_overdue_bills_ww,    last_eating_out_ww,   major_worsening_of_finances_this_yr) %>% describe()
    
    full_sample_o65s_home_owner_downsizer %>% select(n_ppl_w_health_condition, total_health_expenditure, insurance_expenditure,  last_overdue_bills_ww,    last_eating_out_ww,   major_worsening_of_finances_this_yr) %>% describe()
    
    # Descriptive stats, other reasons for moving: 
    full_sample %>% select(years_since_divorce_year, years_since_death_in_household_year, number_of_moves_in_10ys, satisfaction_w_neighbourhood, change_in_household_type) %>% describe()
    
    full_sample_o65s %>% select(years_since_divorce_year, years_since_death_in_household_year, number_of_moves_in_10ys, satisfaction_w_neighbourhood, change_in_household_type) %>% describe()
    
    full_sample_o65s_home_owners %>% select(years_since_divorce_year, years_since_death_in_household_year, number_of_moves_in_10ys, satisfaction_w_neighbourhood, change_in_household_type) %>% describe()
    
    full_sample_o65s_home_owner_movers %>% select(years_since_divorce_year, years_since_death_in_household_year, number_of_moves_in_10ys, satisfaction_w_neighbourhood, change_in_household_type) %>% describe()
    
    full_sample_o65s_home_owner_downsizer %>% select(years_since_divorce_year, years_since_death_in_household_year, number_of_moves_in_10ys, satisfaction_w_neighbourhood, change_in_household_type) %>% describe()
    
    # Descriptive stats, moving cost:
    full_sample %>% select(non_financial_assets_less_cars) %>% describe()
    full_sample_o65s %>% select(non_financial_assets_less_cars) %>% describe()
    full_sample_o65s_home_owners %>% select(non_financial_assets_less_cars) %>% describe()
    full_sample_o65s_home_owner_movers %>% select(non_financial_assets_less_cars) %>% describe()
    full_sample_o65s_home_owner_downsizer %>% select(non_financial_assets_less_cars) %>% describe()
    
    # Descriptive stats, household type: 
    full_sample %>%  select(n_over_65s_in_household, household_type) %>% table()
    full_sample_o65s_home_owners %>%  select(n_over_65s_in_household, household_type) %>% table()
    full_sample_o65s_home_owner_movers %>%  select(n_over_65s_in_household, household_type) %>% table()
    full_sample_o65s_home_owner_downsizer %>%  select(n_over_65s_in_household, household_type) %>% table()
    
    full_sample %>%  select(n_over_65s_in_household, n_in_household) %>% table()
    # CHECK:  full_sample_o65s %>%  select(n_in_household) %>% table() # PASS: This line is consistent with the one above.
    full_sample_o65s_home_owners %>%  select(n_in_household) %>% table()
    full_sample_o65s_home_owner_movers %>%  select(n_in_household) %>% table()
    full_sample_o65s_home_owner_downsizer %>%  select(n_in_household) %>% table()
    
    rm(full_sample, full_sample_o65s_home_owners, full_sample_o65s_home_owner_movers, full_sample_o65s_home_owner_downsizer)
      
    
##### Things I haven't folded up yet: -----            
            
            #  ~~~~~ Deep dive ~~~~~ # 
            
            # What's the profile of change_in_home_value for people who don't move?
            
            hilda_housing_panel_e %>%
              mutate(change_in_home_value_rounded = round_any(change_in_home_value, 100000), 
                     moved_house_combined = if_else(moved_house==1 | moved_house_implied==1, 1, 0)) %>%
              filter(home_ownership_status=="Owner" & change_in_home_value<0 & moved_house_combined==0) %>%
              select(moved_house, wave) %>%
              table()
            
            hilda_housing_panel_e %>%
              select(moved_house, wave) %>%
              table()
            
            hilda_housing_panel_e %>%
              filter(change_in_home_value<0) %>%
              select(wave,moved_house) %>%
              table()
            
            hilda_housing_panel_e %>% #3, 4, 5; 7, 8, 9;  
              filter(wave==15) %>%
              #mutate(value_of_home_IS_NA=if_else(is.na(value_of_home),1,0)) %>%
              select(value_of_home) %>%
              table()
              
              
            
            
              
              
              
              
              
              describe(hilda_housing_panel_e$ change_in_home_value)
            
              
              
              
            
            
            
            
              select(wave, moved_house_during_hilda_implied) %>%
              table()
              
              
              
            
            
                
    # Home ownership status by wave:
       hilda_housing_panel_e %>% mutate(home_ownership_status = if_else(is.na(home_ownership_status), "NA", home_ownership_status)) %>% filter(over_65s==1) %>% group_by(xwaveid_by_wave) %>% slice(1) %>% ungroup()%>% select(home_ownership_status, wave) %>% table()
    # Home ownership status by age_decade:
       hilda_housing_panel_e %>% mutate(home_ownership_status = if_else(is.na(home_ownership_status), "NA", home_ownership_status)) %>% group_by(xwaveid_by_wave) %>% mutate(max_houseehold_age_decade= round_any(max(age), 10)) %>% slice(1) %>% ungroup()%>% select(home_ownership_status, max_houseehold_age_decade) %>% table()
    # Moving (among home owners) by age_decade: 
       hilda_housing_panel_e %>% group_by(xwaveid_by_wave) %>% slice(1) %>% ungroup() %>% filter(home_ownership_status=="Owner") %>% mutate(moved_house_combined =max(moved_house, moved_house_implied)) %>% select(moved_house_combined, wave) %>% table() 
       
       
     
      
       
       
       # Changes to home ownership status:
          
            # DEFINING THE VARIABLE:
            hilda_housing_panel_e %<>%
              mutate(home_ownership_status = if_else(is.na(home_ownership_status), "NA", as.character(home_ownership_status)),
                     home_owner = if_else(home_ownership_status=="Owner", 1, 0)) %>%
              group_by(xwaveid_e) %>%
              arrange(wave) %>%
              mutate(home_owner_lag = lag(home_owner, order_by=wave), 
                     home_owner_change = home_owner - home_owner_lag) %>%
              ungroup() #%>%
              #select(xwaveid, xwaveid_e, wave, home_owner, home_owner_lag, home_owner_change) %>%
              #View()
              #select(home_owner_change) %>%
              #table()
              
            hilda_housing_panel_e %>%
              filter(over_65s==1) %>%
              group_by(xwaveid) %>%
              mutate(net_home_ownership_change = sum(home_owner_change), 
                     gross_home_ownership_changes = sum(abs(home_owner_change))) %>%
              ungroup() %>%
              select(net_home_ownership_change, gross_home_ownership_changes) %>%
              table()
              
            # Survival curve: 
            hilda_panel %>%
              select(wave, decades_at_current_address) %>%
              table()
            
            # Home owners moved house 16,873 times
            hilda_housing_panel_e %>%
              mutate(home_ownership_status = if_else(is.na(home_ownership_status), "NA", as.character(home_ownership_status)),
                     home_owner = if_else(home_ownership_status=="Owner", 1, 0)) %>%
              mutate(moved_house = if_else(moved_house==1, 1, if_else(years_at_current_address==0, 1, 0))) %>% 
              group_by(xwaveid) %>%
              mutate(moved_house_lead = lead(moved_house, order_by = wave)) %>%
              ungroup() %>%
              filter(home_owner==1) %>%
              group_by(xwaveid_by_wave) %>%
              slice(1) %>%
              ungroup() %>%
              select(moved_house_lead) %>%
              table()
 
            # 6778 Home owners were responsible for these moves:
             hilda_housing_panel_e %>%
              mutate(home_ownership_status = if_else(is.na(home_ownership_status), "NA", as.character(home_ownership_status)),
                     home_owner = if_else(home_ownership_status=="Owner", 1, 0)) %>%
              mutate(moved_house = if_else(moved_house==1, 1, if_else(years_at_current_address==0, 1, 0))) %>% 
              group_by(xwaveid) %>%
              mutate(moved_house_lead = lead(moved_house, order_by = wave)) %>%
              ungroup() %>%
              filter(home_owner==1) %>%
              group_by(xwaveid) %>%
              mutate(ever_moved_house = max(moved_house_lead)) %>%
              slice(1) %>%
              ungroup() %>%
              select(moved_house_lead) %>%
              table()
             
             # Change in house value profiles for different move types:            
             hilda_housing_panel_e %>%
               filter(moved_house_implied ==1) %>%
               mutate(change_in_home_value_rounded = round_any(change_in_home_value, 100000)) %>%
               select(change_in_home_value_rounded) %>%
               table()
             
             hilda_housing_panel_e %>%
               filter(moved_house ==1) %>%
               filter(value_of_home!=0) %>%
               mutate(change_in_home_value_rounded = round_any(change_in_home_value, 100000)) %>%
               select(change_in_home_value_rounded) %>%
               hist()
             
             hilda_housing_panel_e %>%
               filter(moved_house_implied ==1 | moved_house==1) %>%
               filter(value_of_home!=0) %>%
               mutate(change_in_home_value_rounded = round_any(change_in_home_value, 100000)) %>%
               select(change_in_home_value_rounded) %>%
               hist()
             

              # Proportion of home owners who move and downsize: (Looking at all households, categorising age by first two members)
                     
                     # Number of people in this mini sample: 
                     hilda_housing_panel_e %>%
                       filter(home_ownership_status=="Owner") %>%
                       group_by(xwaveid_by_wave) %>%
                       mutate(household_age_halfdecade = round_any(if_else(n_in_household==1, hgage1, as.integer((hgage1+hgage2)/2)),5)) %>%
                       ungroup() %>%
                       select(household_age_halfdecade) %>%
                       table()
                     
                     # Number of people who move: 
                     hilda_housing_panel_e %>%
                       filter((moved_house==1 | moved_house_implied==1) & home_ownership_status=="Owner") %>%
                       group_by(xwaveid_by_wave) %>%
                       mutate(household_age_halfdecade = round_any(if_else(n_in_household==1, hgage1, as.integer((hgage1+hgage2)/2)),5)) %>%
                       ungroup() %>%
                       select(household_age_halfdecade) %>%
                       table()
                     
                     # Number of people who downsize:
                     hilda_housing_panel_e %>%
                       filter((moved_house==1 | moved_house_implied==1) & change_in_home_value<0 & home_ownership_status=="Owner") %>%
                       group_by(xwaveid_by_wave) %>%
                       mutate(household_age_halfdecade = round_any(if_else(n_in_household==1, hgage1, as.integer((hgage1+hgage2)/2)),5)) %>%
                       ungroup() %>%
                       select(household_age_halfdecade) %>%
                       table()
                     
                     
                     
            ### Is the change_in_home_value variable working properly as a downsizing indicator?        
                     
                     table(hilda_housing_panel_e  %>% select(over_65s))
                     table(hilda_housing_panel_e %>% filter(over_65s==1) %>% select(home_ownership_status))
                     table(hilda_housing_panel_e %>% filter(over_65s==1 & home_ownership_status=="Owner") %>% select(moved_house, moved_house_implied))
                     hist(hilda_housing_panel_e %>% filter(over_65s==1 & home_ownership_status=="Owner" & (moved_house==1 | moved_house_implied==1)) %>% select(change_in_home_value))
                     table(hilda_housing_panel_e %>% filter(over_65s==1 & home_ownership_status=="Owner" & (moved_house==1 | moved_house_implied==1)) %>% mutate(downsizer = if_else(change_in_home_value==0, 1, 0)) %>% select(downsizer))
                     hist(hilda_housing_panel_e %>% filter(home_ownership_status=="Owner" & (moved_house==1 | moved_house_implied==1) & change_in_home_value==0) %>% select(value_of_home))
                     # Change in home value = NA in most instances:
                     table(is.na(hilda_housing_panel_e %>% filter(home_ownership_status=="Owner" & (moved_house==1 | moved_house_implied==1)) %>% select(change_in_home_value)))
                     table(is.na(hilda_housing_panel_e %>% filter(home_ownership_status=="Owner" & (moved_house==1 | moved_house_implied==1)) %>% select(value_of_home)))
                     
                     
             ### What's the average change in house value per move, per age group?
                     
                     hilda_housing_panel_e %>%
                       filter(home_ownership_status=="Owner" & (moved_house==1 | moved_house_implied==1)) %>%
                       group_by(xwaveid_by_wave) %>%
                       mutate(household_age_halfdecade = round_any(if_else(n_in_household==1, hgage1, as.integer((hgage1+hgage2)/2)),5)) %>%
                       ungroup() %>%
                       group_by(household_age_halfdecade) %>%
                       summarise(mean_change_in_value = mean(change_in_home_value))
                     
             

               table(hilda_housing_panel_e$value_of_home)
             


###### Matters relating to sample selection: ======

## Household type by ownership status:
hilda_housing_panel_g %>%
  mutate(over_65s = if_else(hgage>=65, 1, 0)) %>%
  group_by(xwaveid, wave) %>%
  mutate(n_over_65s = sum(over_65s)) %>%
  slice(1) %>%
  ungroup() %>%
  filter(n_over_65s>=1) %>%
  group_by(household_type, home_ownership_status) %>%
  tally() %>%
  View()



###### Demographics: ======

# Age profile of sample over time:
write.xlsx(hilda_housing_panel_g %>%
             group_by(wave, hgage) %>%
             tally() %>%
             View(), 
           "Users/Lucille/Documents/Data work/HILDA/Hilda_housing_panel_g_AGE_x_WAVE", sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)

# Age split of households with elderly members:
hilda_housing_panel_g %>%
  group_by(xwaveid_by_wave) %>%
  slice(1) %>%
  ungroup() %>%
  filter(prop_over_65s_in_household>0) %>%
  group_by(prop_over_65s_in_household) %>%
  tally()


###### Costs: ======

## Value of non-financial assets:
describe(hilda_housing_panel_g$non_financial_assets)
describe(hilda_housing_panel_g$value_of_cars)
describe(hilda_housing_panel_g$non_financial_assets_less_cars)


####### Pension opportunity cost:

####### Value of personal assets: 
hist(round_any(as.numeric(unlist(hilda_panel %>% filter(wealth_wave==1) %>% select(non_financial_assets))), 100000), exclude=NULL)
table(round_any(as.numeric(unlist(hilda_panel %>% filter(wealth_wave==1) %>% select(non_financial_assets))), 100000), exclude=NULL)
table(round_any(as.numeric(unlist(hilda_panel %>% filter(wealth_wave==1) %>% select(non_financial_assets_less_cars))), 100000), exclude=NULL)

###### Wealth: ======

###### Net worth:
table(round_any(as.numeric(unlist(hilda_panel %>% filter(wealth_wave==1) %>% select(net_worth))),100000))




###### Funding retirement: expectations vs reality: ----
    managing_financially_in_retirement_table<-
      managing_financially_in_retirement %>%
      group_by(hgage1) %>% # It's okay to use this age variable because we're just using info that relates to the first member of the household. 
      slice(1) %>%
      ungroup() %>%
      select(hgage1, prop_cut_big_ticket_spends_by_age, prop_cut_normal_spending_by_age, prop_downsize_by_age, prop_none_of_the_above_by_age, prop_paid_work_by_age, prop_partners_paid_work_by_age, prop_share_housing_by_age, prop_all_NA_or_no_by_age)
    
    intend_to_do_as_result_of_retired_income_table <-
      intend_to_do_as_result_of_retired_income %>%
      group_by(hgage1) %>% 
      slice(1) %>%
      ungroup() %>%
      select(hgage1, prop_cut_big_ticket_spends_by_age, prop_cut_normal_spending_by_age, prop_downsize_by_age, prop_none_of_the_above_by_age, prop_paid_work_by_age, prop_partners_paid_work_by_age, prop_share_housing_by_age, prop_all_NA_or_no_by_age)
    
    did_as_result_of_retired_income_table <- 
      did_as_result_of_retired_income %>%
      group_by(hgage1) %>% 
      slice(1) %>%
      ungroup() %>%
      select(hgage1, prop_cut_big_ticket_spends_by_age, prop_cut_normal_spending_by_age, prop_downsize_by_age, prop_none_of_the_above_by_age, prop_paid_work_by_age, prop_partners_paid_work_by_age, prop_share_housing_by_age, prop_all_NA_or_no_by_age)

    reasons_for_moving_table <- 
      reasons_for_moving %>%
      group_by(hgage1) %>% 
      slice(1) %>%
      ungroup() %>%
      select(hgage1, prop_location_related_move_by_age, prop_lifestyle_related_move_by_age, prop_work_study_related_move_by_age, prop_upsized_house_by_age, prop_downsized_house_by_age, prop_all_NA_or_no_by_age)
    
write.xlsx(managing_financially_in_retirement_table, "Hilda_panel_retirement_finance_intentions.xlsx", sheetName="expect_to_do_to_manage", col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)
write.xlsx(intend_to_do_as_result_of_retired_income_table, "Hilda_panel_retirement_finance_intentions.xlsx", sheetName="intend_to_do", col.names=TRUE, row.names=TRUE, append=TRUE, showNA=TRUE)
write.xlsx(did_as_result_of_retired_income_table, "Hilda_panel_retirement_finance_intentions.xlsx", sheetName="did_do", col.names=TRUE, row.names=TRUE, append=TRUE, showNA=TRUE)
write.xlsx(reasons_for_moving_table, "Hilda_panel_retirement_finance_intentions.xlsx", sheetName="reason_for_move", col.names=TRUE, row.names=TRUE, append=TRUE, showNA=TRUE)


##### Value of home by age: 
age_home_value_table<-
hilda_housing_panel_e %>%
  filter(original_person_n==1 & home_ownership_status=="Owner") %>%
  group_by(xwaveid_by_wave) %>%
  slice() %>%
  ungroup() %>%
  group_by(age) %>%
  mutate(mean_value_of_home_by_age = mean(value_of_home)) %>%
  slice(1) %>%
  ungroup() %>%
  select(age, mean_value_of_home_by_age)
write.xlsx(age_home_value_table, "Hilda_panel_retirement_finance_intentions.xlsx", sheetName="Home_value_x_age", col.names=TRUE, row.names=TRUE, append=TRUE, showNA=TRUE)

