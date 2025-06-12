library(psych)
library(mice)

# Recode where necessary 
 df$indic_rich_num <- 1 - df$redis_reasons_rich_num        
 df$indic_poor_num <- 1 - df$redis_reasons_poor_num  
 df$indic_welfare_num <- df$redis_welfare_num

# Missing due to split sample, predictive mean matching
 prop_items <- df[, c("redis_intelligence_num", "redis_opportunity_num", 
                      "indic_rich_num", "indic_poor_num", "redis_effort_num")]
 
 # Run imputation
 imp <- mice(prop_items, m = 5, method = "pmm", seed = 2025)
 
 # Extract first completed dataset
 prop_items_imputed <- complete(imp, 1)
 
 # Check reliability
 psych::alpha(prop_items_imputed) # ⚠️⚠️⚠️ DANGER ⚠️⚠️⚠️ Too low, use only redis_effort_num
 

 # Missing due to split sample, predictive mean matching
 recip_items <- df[, c("redis_social_benefits_num",            
                       # "indic_welfare_num",                    
                        "redis_no_cheat_system_num")]
 
 # Run imputation
 imp <- mice(recip_items, m = 5, method = "pmm", seed = 2025)
 
 # Extract first completed dataset
prop_items_imputed <- complete(imp, 1)

psych::alpha(prop_items_imputed) # ⚠️⚠️ Low reliability across reciprocity items. Use only redis_no_cheat_system_num if needed.
