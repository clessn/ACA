# --- Dataframe
df <- read.csv("data/NAME.csv") 

# --- Binary DV
# Priority
budget_health_priority_bin
budget_education_priority_bin
budget_pensions_priority_bin
budget_taxes_priority_bin
budget_debt_priority_bin

# CHANGE Spending (priority if majority of 100 points to one issue)
budget_spend_prio_health_bin        
budget_spend_prio_seniors_bin       
budget_spend_prio_childcare_bin     
budget_spend_prio_growth_bin    
budget_spend_prio_climate_bin

# CHANGE Trade-off childcare (priority if majority of 100 points to one issue)
tradeoff_childcare_taxes_bin
tradeoff_childcare_cut_bin 
tradeoff_childcare_debt_bin 
tradeoff_childcare_noincrease_bin 

# CHANGE Trade-off green economy (priority if majority of 100 points to one issue)
tradeoff_ge_taxes_bin
tradeoff_ge_cut_bin 
tradeoff_ge_debt_bin 
tradeoff_ge_noincrease_bin 

# CHANGE Trade-off new taxation (priority if majority of 100 points to one issue)
tradeoff_no_taxes_bin           
tradeoff_taxes_sales_bin            
tradeoff_taxes_high_income_bin      
tradeoff_taxes_wealthy_bin

# CHANGE Policy trade-offs (priority if majority of 100 points to one issue)
# Child care
budget_tradeoff_cc2_all_bin
budget_tradeoff_cc2_low_1_bin
budget_tradeoff_cc2_low_2_bin

#  CHANGE Senior policy
budget_tradeoff_hc_all_bin
budget_tradeoff_hc_low_1_bin
budget_tradeoff_hc_educ_bin
budget_tradeoff_hc_low_2_bin

# Redistribution questions
redis_opportunity_bin     # redis_fei_can2          
redis_reasons_rich_bin    # redis_pnvo_rich        
redis_reasons_poor_bin    # redis_pnvo_poor          
redis_effort_bin          # redis_fei_can1          
redis_social_benefits_bin  # redis_prev_freerider        
redis_welfare_bin          # redis_fid_can         
redis_deserve              # redis_iden_freerider
names(df)

# --- IV
gender_bin                  # ses_gender                           
age                         # ses_year_born
region                      # ses_region
lang                        # ses_language
education                   # ses_education                    
income                      # ses_income
citizen_bin                 # ses_citizen_status
children_bin                # ses_children
young_children_bin          # ses_household_compo
employ_fulltime_bin         # ses_employ_status
ideo_right_num              # ideo_left_right
ideo_vote_fed               # ideo_vote_fed/PROV (à regrouper selon les partis R-L et selon province)
ideo_vote_prov              # ideo_vote_prov/PROV (à regrouper selon les partis R-L et selon province)
pol_int_num                 # ideo_interest
moreno # avant nommé : ideo_country_bin  # ideo_define/PROV
trust_social_bin            # trust_social    
trust_pol_parties_bin       # trust_institution_1        
trust_fed_gov_bin           # trust_institution_2                   
trust_prov_gov_bin          # trust_institution_3               
trust_media_bin             # trust_institution_4

# SES coded binary, don't think I need this format (to discuss)
# userFR_bin                          
# ses_male_bin                        
# ses_age                             
# age34                              
# age35_54  
# educ_group                         
# educBHS                           