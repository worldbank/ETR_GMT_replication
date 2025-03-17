
print("Begin Regression")

########################| 1. PREPARE DATASET |################################# 

# CALL ETR FUNCTION:

source(paste0(aux_source, "function_ETR.R"))


# + Cross section and Panel: Dummies for different exemptions: ####

df.ETR.reg.panel <- list()
df.ETR.reg.cross <- list()

list_regressors <- c('exempt_income', 'non_deduc_inp', 
                     'tot_deduc_taxbase', 'depreciation', 'investment_taxbase', 'capital_allowance',
                     'loss_carryforward', 'tot_noelse_taxliab', 'cred_foreign_tax', 'special_taxliab', 'trade_taxliab',
                     'tot_deduc_taxliab',  'investment_taxliab',  'other_cred_taxliab') 

list_control <- c('section', 'FEZ', 'capital_city', 'first_year', 'firm_age', 'foreign_ownership', 
                  "asset_tax", "min_tax", "special_regime",  'region', 'tax_center')


for (i in 1: length(files)){
  
  # + Effective Tax Rate constructed here:
  df <- ETR_FUN(data.panel[[i]], "net_profit") 
  
  # Dummy if tax provision is claimed
  df <- df %>% mutate(across(all_of(list_regressors), ~if_else(!is.na(.) & .!=0, 1, 0)))
  
  # Other dummy:
  df <- df %>% mutate(loss = if_else(net_profit<0, 1, 0),
                      ftc_else_cred = if_else(cred_foreign_tax ==1 | other_cred_taxliab == 1, 1, 0))
  
  df <- df %>% group_by(country) %>% mutate(max_STR = if_else(country[1] == "ECU" | country[1] == "HND", 0.25, max(STR)),
                                            reduced_rate = if_else(STR < max(STR), 1, 0))
  
  df <- df %>% select(all_of(list_regressors), one_of(list_control), 
                      ETR_keep_neg, ETR_drop_neg, country, STR, percentile_99.9, percentile_rob, decile, 
                      loss, ftc_else_cred, reduced_rate,  year, tax_ID) 
  # Panel
  df.ETR.reg.panel[[i]] <- df
  
  # Cross-section: last year of the panel--except if covid year
  df.ETR.reg.cross[[i]] <- df %>% filter(year == max(data.cross[[i]]$year)) 
  
  print(country_code[[i]])
} #END 
 


rm(df)

########################| 2. REGRESSION FUNCTION |################################# 

# Requires tidyverse, stargazer, broom and fixest packages
# Choose the following when calling the function:
# Dataset, explanatory variable, regression type, regression name


regFUN = function(data, explvar, reg_type, reg_name){  
  
  # + Sample and Variables : ####
  #..................
  
  df_all <- data.frame(df) %>% filter(!is.na(ETR_keep_neg) & !is.na(explvar)) # data is at the country level
  df_profitable <- data.frame(df) %>% filter(!is.na(ETR_drop_neg) & !is.na(explvar)) # data is at the country level
  
  var_agg  <- "STR + exempt_income + tot_deduc_taxbase + loss_carryforward + tot_deduc_taxliab "
  var_agg_c  <- c( "STR", "exempt_income", "tot_deduc_taxbase", "loss_carryforward","tot_deduc_taxliab") # reduced_rate",
  
  var_disagg  <- "reduced_rate + exempt_income + depreciation + investment_taxbase + capital_allowance + 
                  loss_carryforward + investment_taxliab + special_taxliab + trade_taxliab +  
                  cred_foreign_tax + other_cred_taxliab"
  
  char <- c("asset_tax", "min_tax", "foreign_ownership", "capital_city", "firm_age", "first_year", "FEZ",
            'region', 'tax_center')
  
  # BASE 
  base <- paste0("ETR_keep_neg ~ ", explvar)
  
  ###
  country <- df_profitable$country[1]
  
  
  # + Base model 0 ####
  #..................
  
  # Profitable firms only
  fit0 <- feols(as.formula(paste(base)), data =df_profitable) 
  # All
  fit0_all <- feols(as.formula(paste(base)), data =df_all) 
  
  
  # + Base with characteristics ####
  #..................

  if (reg_type == "OLS" ){
    # All Countries have sectors
    base <- paste0("ETR_keep_neg ~ ", explvar, " + factor(section)")
    
    # Add characteristics individually for each country
    characteristic_FUN = function(var){
      t <- c()
      if (all(!is.na(df_profitable[[var]])) == TRUE & any(names(df_profitable) == var) ){
        t <- append(t, var)  
      }}
    
    #Cannot use across() so we will use a loop:
    control <- lapply(char, characteristic_FUN) %>% unlist()
    ctr_list <- paste(control, collapse = " + ")
    if (is.null(control) == FALSE){
      base_char <- paste0(base, " + ", ctr_list)
    } else {
      base_char <- paste0(base)
    }
    base <- paste0("ETR_keep_neg ~ ", explvar)
  }
  
  
  # + Add tax covariates  ####
  #..................
  if (reg_type == "OLS"){
    
    f_char <- feols(as.formula(paste(base_char)), data =df_profitable) 
    # --Controls
    f_rate <- feols(as.formula(paste(base, "STR", sep = "+")), data =df_profitable)
    f_exem <- feols(as.formula(paste(base, "exempt_income", sep = "+")), data =df_profitable)
    f_ince <- feols(as.formula(paste(base, "tot_deduc_taxbase", sep = "+")), data =df_profitable)
    f_loss <- feols(as.formula(paste(base, "loss_carryforward", sep = "+")), data =df_profitable)
    f_cred <- feols(as.formula(paste(base, "tot_deduc_taxliab", sep = "+")), data =df_profitable)
    # --All variables
    f_all <- feols(as.formula(paste(base_char, var_agg, sep = "+")), data =df_profitable)
    
    # Print regression table
    sink("NUL") # don't show R console output
    # .tex output
    etable(fit0, f_char, f_rate,  f_exem, f_ince, f_loss, f_cred, f_all,
           tex = TRUE, file = paste0(country, "_reg_", reg_name, ".tex"), replace = TRUE,
           drop = "section", title = paste0(country, "_reg_", reg_name), digits = 2, digits.stats = 2)
    sink()
    print("Regression table saved.")
    
  }
  
  
  sum <- summary(fit0)
  # Save coeff for country average
  tidy0 <- fit0 %>% tidy %>% mutate(model = 0)
  tidy1 <- f_char %>% tidy %>% mutate(model = 1)
  tidy2 <- f_rate %>% tidy %>% mutate(model = 2)
  tidy3 <- f_exem %>% tidy %>% mutate(model = 3)
  tidy4 <- f_ince %>% tidy %>% mutate(model = 4)
  tidy5 <- f_loss %>% tidy %>% mutate(model = 5)
  tidy6<- f_cred %>% tidy %>% mutate(model = 6)
  tidy7 <- f_all %>% tidy %>% mutate(model = 7)
  tidy8 <- fit0_all %>% tidy %>% mutate(model = 8)
  
  tidyfit <- rbind(tidy0, #, tidy8
                   tidy1,  tidy2, tidy3, tidy4, tidy5, tidy6, tidy7) %>% 
    mutate(country = df$country[[1]], to_drop = NA_real_) %>% 
    filter( term %in% var_agg_c | term %in% char |
              term == "top99" |  term == paste0(explvar) | term == "(Intercept)" ) 
  
  
} #END 


########################| 3. RUN SPECIFICATIONS  |################################# 

setwd(regressions)
reg_name <- c()

# a. Main regressions ####
#...............

# OLS, P90-P99 (dummy top 1 %):
temp <- list()
for (i in 1:length(files)){
  print(paste0(country_code[[i]], ", OLS model, top 1%"))
  df <- df.ETR.reg.cross[[i]] %>% 
    filter(decile == 10) %>% mutate(top99 = if_else(percentile_99.9 >= 99 , 1, 0))
  temp[[i]] <- regFUN(df, "top99", "OLS", "T10")
}
df.ETR.fit.T10 <- do.call("rbind", temp)
reg_name <- append(reg_name, "df.ETR.fit.T10")


# OLS, P1-P89 (percentiles):
temp <- list()
for (i in 1:length(files)){
  print(paste0(country_code[[i]], ", OLS model, up to P90"))
  df <- df.ETR.reg.cross[[i]] %>% filter( percentile_99.9 <=90)
  temp[[i]] <- regFUN(df, "percentile_99.9", "OLS", "B90")
}
df.ETR.fit.B90 <- do.call("rbind", temp)
reg_name <- append(reg_name, "df.ETR.fit.B90")


# b. Robustness regressions ####
#...............

#### i. sample is top 10 ####

# + Dummy is top 5% ####
# OLS, P90-P99 (dummy top 5 %):
temp <- list()
for (i in 1:length(files)){
  print(paste0(country_code[[i]], ", OLS model, top 5%"))
  df <- df.ETR.reg.cross[[i]] %>%  
    filter(decile == 10) %>% mutate(top5 = if_else(percentile_99.9 >= 95, 1, 0))
  temp[[i]] <- regFUN(df, "top5", "OLS", "T10.D5")
}
df.ETR.fit.T10.D5 <- do.call("rbind", temp)
reg_name <- append(reg_name, "df.ETR.fit.T10.D5")

# + Dummy is top 3% ####
# OLS, P90-P99 (dummy top 3 %):
temp <- list()
for (i in 1:length(files)){
  print(paste0(country_code[[i]], ", OLS model, top 3%"))
  df <- df.ETR.reg.cross[[i]] %>% 
    filter(decile == 10) %>% mutate(top3 = if_else(percentile_99.9 >= 97, 1, 0))
  temp[[i]] <- regFUN(df, "top3", "OLS", "T10.D3")
}
df.ETR.fit.T10.D3 <- do.call("rbind", temp)
reg_name <- append(reg_name, "df.ETR.fit.T10.D3")

# + Dummy is top 2% ####
# OLS, P90-P99 (dummy top 2 %):
temp <- list()
for (i in 1:length(files)){
  print(paste0(country_code[[i]], ", OLS model, top 2%"))
  df <- df.ETR.reg.cross[[i]] %>% 
    filter(decile == 10) %>% mutate(top2 = if_else(percentile_99.9 >= 98, 1, 0))
  temp[[i]] <- regFUN(df, "top2", "OLS", "T10.D2")
}
df.ETR.fit.T10.D2 <- do.call("rbind", temp)
reg_name <- append(reg_name, "df.ETR.fit.T10.D2")

# + Dummy is top 0.1% ####
# OLS, P90-P99 (dummy top 0.1 %):
temp <- list()
for (i in 1:length(files)){
  print(paste0(country_code[[i]], ", OLS model, top 0.1%"))
  df <- df.ETR.reg.cross[[i]] %>% 
    filter(decile == 10) %>% mutate(top999 = if_else(percentile_rob == 99.9 , 1, 0))
  temp[[i]] <- regFUN(df, "top999", "OLS", "T10.D01")
}
df.ETR.fit.T10.D01 <- do.call("rbind", temp)
reg_name <- append(reg_name, "df.ETR.fit.T10.D01")


# + Percentile continuous at the top 10####
# OLS, P90-P99 (percentiles):
temp <- list()
for (i in 1:length(files)){
  print(paste0(country_code[[i]], ", OLS model, continuous, top10"))
  df <- df.ETR.reg.cross[[i]] %>%
    filter(decile == 10) %>% mutate(percentiles = if_else(percentile_99.9 >= 99 , 99, percentile_99.9))
  temp[[i]] <- regFUN(df, "percentiles", "OLS", "T10.C")
}
df.ETR.fit.T10.C <- do.call("rbind", temp)
reg_name <- append(reg_name, "df.ETR.fit.T10.C")


#### ii. sample is top 20 ####

# + dummy top 1 % ####
# OLS, P80-P99 (dummy top 1 %):
temp <- list()
for (i in 1:length(files)){
  print(paste0(country_code[[i]], ", OLS model, top 1%"))
  df <- df.ETR.reg.cross[[i]] %>%
    filter(decile >= 9) %>% mutate(top99 = if_else(percentile_99.9 >= 99, 1, 0))
  temp[[i]] <- regFUN(df, "top99", "OLS", "T20")
}
df.ETR.fit.T20 <- do.call("rbind", temp)
reg_name <- append(reg_name, "df.ETR.fit.T20")


# + Dummy is top 2% ####
# OLS, P90-P99 (dummy top 5 %):
temp <- list()
for (i in 1:length(files)){
  print(paste0(country_code[[i]], ", OLS model, top 2%"))
  df <- df.ETR.reg.cross[[i]] %>%
    filter(decile >= 9) %>% mutate(top2 = if_else(percentile_99.9 >= 98, 1, 0))
  temp[[i]] <- regFUN(df, "top2", "OLS", "T20.D2")
}
df.ETR.fit.T20.D2 <- do.call("rbind", temp)
reg_name <- append(reg_name, "df.ETR.fit.T20.D2")

# + Dummy is top 3% ####
# OLS, P90-P99 (dummy top 3 %):
temp <- list()
for (i in 1:length(files)){
  print(paste0(country_code[[i]], ", OLS model, top 3%"))
  df <- df.ETR.reg.cross[[i]] %>%
    filter(decile >= 9) %>% mutate(top3 = if_else(percentile_99.9 >= 97, 1, 0))
  temp[[i]] <- regFUN(df, "top3", "OLS", "T20.D3")
}
df.ETR.fit.T20.D3 <- do.call("rbind", temp)
reg_name <- append(reg_name, "df.ETR.fit.T20.D3")

# + Dummy is top 5% ####
# OLS, P90-P99 (dummy top 5 %):
temp <- list()
for (i in 1:length(files)){
  print(paste0(country_code[[i]], ", OLS model, top 5%"))
  df <- df.ETR.reg.cross[[i]] %>% 
    filter(decile >= 9) %>% mutate(top5 = if_else(percentile_99.9 >= 95, 1, 0))
  temp[[i]] <- regFUN(df, "top5", "OLS", "T20.D5")
}
df.ETR.fit.T20.D5 <- do.call("rbind", temp)
reg_name <- append(reg_name, "df.ETR.fit.T20.D5")


# + Dummy is top 0.1% ####
# OLS, P90-P99 (dummy top 0.1 %):
temp <- list()
for (i in 1:length(files)){
  print(paste0(country_code[[i]], ", OLS model, top 0.1%"))
  df <- df.ETR.reg.cross[[i]] %>%
    filter(decile >= 9) %>% mutate(top999 = if_else(percentile_rob == 99.9 , 1, 0))
  temp[[i]] <- regFUN(df, "top999", "OLS", "T20.D01")
}
df.ETR.fit.T20.D01 <- do.call("rbind", temp)
reg_name <- append(reg_name, "df.ETR.fit.T20.D01")

# + Percentile continuous at the top 20 ####
# OLS, P80-P99 (percentiles):
temp <- list()
for (i in 1:length(files)){
  print(paste0(country_code[[i]], ", OLS model, continuous, top 20"))
  df <- df.ETR.reg.cross[[i]] %>% 
    filter(decile >= 9) %>% mutate(percentiles = if_else(percentile_99.9 >= 99 , 99, percentile_99.9))
  temp[[i]] <- regFUN(df, "percentiles", "OLS", "T20.C")
}
df.ETR.fit.T20.C <- do.call("rbind", temp)
reg_name <- append(reg_name, "df.ETR.fit.T20.C")



print("Regressions done")

########################| 4. EXTRACT META-DATA  |################################# 


# Export some metadata in csv format
setwd(metadata)

# --List all graphs to append
list.name <- reg_name 

# and store them as objects in one list
list.df <- list() 
for (i in 1:length(list.name)){
  list.df[[i]] <- get(list.name[[i]])
}

# Save in RDS format
for (i in 1:length(list.df)){
  saveRDS(list.df[[i]], file = paste0(pre, list.name[[i]], ".RDS", sep = "" ))
  print(list.name[[i]])
}



print("All dataframes stored")
