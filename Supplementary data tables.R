# Reason for moving:
reasons_for_moving<-
  hilda_panel %>%
  mutate(location_related_move= if_else(mhreaas=="[1] Yes" | mhreabn=="[1] Yes" | mhreaff=="[1] Yes" | mhreahn=="[1] Yes" | mhreafm=="[1] Yes" | mhrearo=="[1] Yes", 1, 0), 
         lifestyle_related_move= if_else(mhreals=="[1] Yes" | mhreamb=="[1] Yes" | mhreapf=="[1] Yes" | mhreapo=="[1] Yes", 1, 0), 
         work_study_related_move= if_else(mhreanj=="[1] Yes" | mhreaob=="[1] Yes" | mhrearb=="[1] Yes" | mhreast=="[1] Yes" | mhreawp=="[1] Yes" | mhreawr=="[1] Yes" | mhreawt=="[1] Yes", 1, 0), 
         upsized_house= if_else(mhrealb=="[1] Yes", 1, 0), 
         downsized_house= if_else(mhreasm=="[1] Yes", 1, 0),
         all_NA_or_no= if_else(location_related_move==1 | lifestyle_related_move==1 | work_study_related_move==1 | upsized_house==1 | downsized_house==1, 0, 1)) %>%
  mutate(prop_location_related_move = mean(location_related_move), 
         prop_lifestyle_related_move = mean(lifestyle_related_move), 
         prop_work_study_related_move = mean(work_study_related_move), 
         prop_upsized_house = mean(upsized_house), 
         prop_downsized_house = mean(downsized_house), 
         prop_all_NA_or_no = mean(all_NA_or_no)) %>%
  group_by(hgage1) %>% # This might not be the most appropriate age variable
  mutate(prop_location_related_move_by_age = mean(location_related_move), 
         prop_lifestyle_related_move_by_age = mean(lifestyle_related_move), 
         prop_work_study_related_move_by_age = mean(work_study_related_move), 
         prop_upsized_house_by_age = mean(upsized_house), 
         prop_downsized_house_by_age = mean(downsized_house), 
         prop_all_NA_or_no_by_age = mean(all_NA_or_no)) %>%
  ungroup() %>%
  select(xwaveid, wave, hgage1, location_related_move, lifestyle_related_move, work_study_related_move, upsized_house, downsized_house, all_NA_or_no, prop_location_related_move, prop_lifestyle_related_move, prop_work_study_related_move, prop_upsized_house, prop_downsized_house, prop_all_NA_or_no, prop_location_related_move_by_age, prop_lifestyle_related_move_by_age, prop_work_study_related_move_by_age, prop_upsized_house_by_age, prop_downsized_house_by_age, prop_all_NA_or_no_by_age)    

# So this is the overall split of reasons:    
View(reasons_for_moving  %>% slice(1) %>% select(prop_location_related_move, prop_lifestyle_related_move, prop_work_study_related_move, prop_upsized_house, prop_downsized_house, prop_all_NA_or_no)) 
# Why doesn't this table work ?!?
View(reasons_for_moving %>% group_by(hgage1) %>% slice(1) %>% ungroup() %>% select(hgage1, prop_location_related_move_by_age, prop_lifestyle_related_move_by_age, prop_work_study_related_move_by_age, prop_upsized_house_by_age, prop_downsized_house_by_age, prop_all_NA_or_no_by_age))              
# Especially when the same command works on my other dataframe:
View(managing_financially_in_retirement %>% group_by(hgage1) %>% slice(1) %>% ungroup() %>% select(hgage1, prop_cut_big_ticket_spends_by_age, prop_cut_normal_spending_by_age, prop_downsize_by_age, prop_none_of_the_above_by_age, prop_paid_work_by_age, prop_partners_paid_work_by_age, prop_share_housing_by_age, prop_all_NA_or_no_by_age))              
# And I can see that there's variation by age: 
reasons_for_moving %>% filter(hgage1<=50) %>% summarise(mean(location_related_move))

############ Questions:
###### Do I need to do anything special with the wealth var? 
###### Any quirks to how we should use the material deprivation vars?

## 6. Reason for moving:
hilda_panel$mhrea

## 6. Moving: -----

## 7. Managing financially in retirement: ----

managing_financially_in_retirement<-
  hilda_panel %>%
  filter(wave==3 | wave==7 | wave==11 | wave==15) %>%
  mutate(cut_big_ticket_spends = if_else(rtmfclf=="[1] Yes", 1, 0), # Defining the variables in this way means there's no NA's to worry about.
         cut_normal_spending = if_else(rtmfcns=="[1] Yes", 1, 0), 
         downsize = if_else(rtmfmv=="[1] Yes", 1, 0), 
         none_of_the_above = if_else(rtmfno=="[1] Yes", 1, 0), 
         paid_work = if_else(rtmfpw=="[1] Yes", 1, 0), 
         partners_paid_work = if_else(rtmfrpw=="[1] Yes", 1, 0), 
         share_housing = if_else(rtmfso=="[1] Yes", 1, 0), 
         all_NA_or_no = if_else(cut_big_ticket_spends==1 | cut_normal_spending==1 | downsize==1 | none_of_the_above==1 | 
                                  paid_work==1 | partners_paid_work==1 | share_housing ==1, 0, 1)) %>%
  mutate(prop_cut_big_ticket_spends = mean(cut_big_ticket_spends), 
         prop_cut_normal_spending = mean(cut_normal_spending), 
         prop_downsize = mean(downsize), 
         prop_none_of_the_above = mean(none_of_the_above), 
         prop_paid_work = mean(paid_work), 
         prop_partners_paid_work = mean(partners_paid_work), 
         prop_share_housing = mean(share_housing), 
         prop_all_NA_or_no = mean(all_NA_or_no)) %>%
  group_by(hgage1) %>% # This might not be the right age variable. 
  mutate(prop_cut_big_ticket_spends_by_age = mean(cut_big_ticket_spends), 
         prop_cut_normal_spending_by_age = mean(cut_normal_spending), 
         prop_downsize_by_age = mean(downsize), 
         prop_none_of_the_above_by_age = mean(none_of_the_above), 
         prop_paid_work_by_age = mean(paid_work), 
         prop_partners_paid_work_by_age = mean(partners_paid_work), 
         prop_share_housing_by_age = mean(share_housing), 
         prop_all_NA_or_no_by_age = mean(all_NA_or_no)) %>%
  ungroup() %>%
  select(xwaveid, wave, hgage1, xwaveid_by_wave , cut_big_ticket_spends, cut_normal_spending, downsize, none_of_the_above, paid_work, partners_paid_work, share_housing, all_NA_or_no, prop_cut_big_ticket_spends, prop_cut_normal_spending, prop_downsize, prop_none_of_the_above, prop_paid_work, prop_partners_paid_work, prop_share_housing, prop_all_NA_or_no, prop_cut_big_ticket_spends_by_age, prop_cut_normal_spending_by_age, prop_downsize_by_age, prop_none_of_the_above_by_age, prop_paid_work_by_age, prop_partners_paid_work_by_age, prop_share_housing_by_age, prop_all_NA_or_no_by_age)


intend_to_do_as_result_of_retired_income<-
  hilda_panel %>%
  filter(wave==3 | wave==7 | wave==11 | wave==15) %>%
  mutate(cut_big_ticket_spends = if_else(rtnwicl=="[1] Yes", 1, 0), # Defining the variables in this way means there's no NA's to worry about.
         cut_normal_spending = if_else(rtnwics=="[1] Yes", 1, 0), 
         downsize = if_else(rtnwimv=="[1] Yes", 1, 0), 
         none_of_the_above = if_else(rtnwino=="[1] Yes", 1, 0), 
         paid_work = if_else(rtnwipw=="[1] Yes", 1, 0), 
         partners_paid_work = if_else(rtnwisw=="[1] Yes", 1, 0), 
         share_housing = if_else(rtnwiso=="[1] Yes", 1, 0), 
         all_NA_or_no = if_else(cut_big_ticket_spends==1 | cut_normal_spending==1 | downsize==1 | none_of_the_above==1 | 
                                  paid_work==1 | partners_paid_work==1 | share_housing ==1, 0, 1)) %>%
  mutate(prop_cut_big_ticket_spends = mean(cut_big_ticket_spends), 
         prop_cut_normal_spending = mean(cut_normal_spending), 
         prop_downsize = mean(downsize), 
         prop_none_of_the_above = mean(none_of_the_above), 
         prop_paid_work = mean(paid_work), 
         prop_partners_paid_work = mean(partners_paid_work), 
         prop_share_housing = mean(share_housing), 
         prop_all_NA_or_no = mean(all_NA_or_no)) %>%
  group_by(hgage1) %>% # This might not be the right age variable. 
  mutate(prop_cut_big_ticket_spends_by_age = mean(cut_big_ticket_spends), 
         prop_cut_normal_spending_by_age = mean(cut_normal_spending), 
         prop_downsize_by_age = mean(downsize), 
         prop_none_of_the_above_by_age = mean(none_of_the_above), 
         prop_paid_work_by_age = mean(paid_work), 
         prop_partners_paid_work_by_age = mean(partners_paid_work), 
         prop_share_housing_by_age = mean(share_housing), 
         prop_all_NA_or_no_by_age = mean(all_NA_or_no)) %>%
  ungroup() %>%
  select(xwaveid, wave, hgage1, xwaveid_by_wave , cut_big_ticket_spends, cut_normal_spending, downsize, none_of_the_above, paid_work, partners_paid_work, share_housing, all_NA_or_no, prop_cut_big_ticket_spends, prop_cut_normal_spending, prop_downsize, prop_none_of_the_above, prop_paid_work, prop_partners_paid_work, prop_share_housing, prop_all_NA_or_no, prop_cut_big_ticket_spends_by_age, prop_cut_normal_spending_by_age, prop_downsize_by_age, prop_none_of_the_above_by_age, prop_paid_work_by_age, prop_partners_paid_work_by_age, prop_share_housing_by_age, prop_all_NA_or_no_by_age)

did_as_result_of_retired_income<-
  hilda_panel %>%
  filter(wave==3 | wave==7 | wave==11 | wave==15) %>%
  mutate(cut_big_ticket_spends = if_else(rtnwdcl=="[1] Yes", 1, 0), # Defining the variables in this way means there's no NA's to worry about.
         cut_normal_spending = if_else(rtnwdcs=="[1] Yes", 1, 0), 
         downsize = if_else(rtnwdmv=="[1] Yes", 1, 0), 
         none_of_the_above = if_else(rtnwdno=="[1] Yes", 1, 0), 
         paid_work = if_else(rtnwdpw=="[1] Yes", 1, 0), 
         partners_paid_work = if_else(rtnwdsw=="[1] Yes", 1, 0), 
         share_housing = if_else(rtnwdso=="[1] Yes", 1, 0), 
         all_NA_or_no = if_else(cut_big_ticket_spends==1 | cut_normal_spending==1 | downsize==1 | none_of_the_above==1 | 
                                  paid_work==1 | partners_paid_work==1 | share_housing ==1, 0, 1)) %>%
  mutate(prop_cut_big_ticket_spends = mean(cut_big_ticket_spends), 
         prop_cut_normal_spending = mean(cut_normal_spending), 
         prop_downsize = mean(downsize), 
         prop_none_of_the_above = mean(none_of_the_above), 
         prop_paid_work = mean(paid_work), 
         prop_partners_paid_work = mean(partners_paid_work), 
         prop_share_housing = mean(share_housing), 
         prop_all_NA_or_no = mean(all_NA_or_no)) %>%
  group_by(hgage1) %>% # This might not be the right age variable. 
  mutate(prop_cut_big_ticket_spends_by_age = mean(cut_big_ticket_spends), 
         prop_cut_normal_spending_by_age = mean(cut_normal_spending), 
         prop_downsize_by_age = mean(downsize), 
         prop_none_of_the_above_by_age = mean(none_of_the_above), 
         prop_paid_work_by_age = mean(paid_work), 
         prop_partners_paid_work_by_age = mean(partners_paid_work), 
         prop_share_housing_by_age = mean(share_housing), 
         prop_all_NA_or_no_by_age = mean(all_NA_or_no)) %>%
  ungroup() %>%
  select(xwaveid, wave, hgage1, xwaveid_by_wave, cut_big_ticket_spends, cut_normal_spending, downsize, none_of_the_above, paid_work, partners_paid_work, share_housing, all_NA_or_no, prop_cut_big_ticket_spends, prop_cut_normal_spending, prop_downsize, prop_none_of_the_above, prop_paid_work, prop_partners_paid_work, prop_share_housing, prop_all_NA_or_no, prop_cut_big_ticket_spends_by_age, prop_cut_normal_spending_by_age, prop_downsize_by_age, prop_none_of_the_above_by_age, prop_paid_work_by_age, prop_partners_paid_work_by_age, prop_share_housing_by_age, prop_all_NA_or_no_by_age)




