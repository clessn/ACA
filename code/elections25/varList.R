# --- Dataframe
df <- read.csv("data/ACA_weighted.csv") 

# --- Weight vector
weightvec

# --- Binary DV
# Priority
budget_health_priority_bin
budget_education_priority_bin
budget_pensions_priority_bin
budget_taxes_priority_bin
budget_debt_priority_bin

# Spending
budget_spend_prio_health_bin        
budget_spend_prio_seniors_bin       
budget_spend_prio_childcare_bin     
budget_spend_prio_costLiving_bin    
budget_spend_prio_climateChange_bin

# Trade-off childcare (first = control, three others = treatments)
tradeoff_childcare_num_bin    
tradeoff_childcare_higher_taxes_bin
tradeoff_childcare_by_cutting_bin 
tradeoff_childcare_debt_bin 

# Trade-off new taxation (first = control, three others = treatments)
tradeoff_no_taxes_bin           
tradeoff_taxes_sales_bin            
tradeoff_taxes_high_income_bin      
tradeoff_taxes_wealthy_bin

# Policy trade-offs (question wording & choice)
# Child care
tradeoff_childcare_lowincome_bin  
tradeoff_childcare_benefits_bin

# Senior policy
tradeoff_senior_benefits_bin        
tradeoff_senior_income_bin

# Redistribution questions
redis_opportunity_bin               
redis_reasons_rich_bin              
redis_reasons_poor_bin              
redis_effort_bin                    
redis_social_benefits_bin           
redis_welfare_bin                   
redis_no_cheat_system_bin  
names(df)

# --- IV
gender                              
age                                 
education                          
income
home_owned_bin
children_bin
employ_fulltime_bin
ideo_right_num.
ideo_country_bin
trust_your_prov_gov_bin
trust_social_bin                    
trust_pol_parties_bin               
trust_fed_gov_bin                   
trust_prov_gov_bin                  
trust_media_bin

# SES coded binary, don't think I need this format
# userFR_bin                          
# ses_male_bin                        
# ses_age                             
# age34                              
# age35_54  
# educ_group                         
# educBHS                           