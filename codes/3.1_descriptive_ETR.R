
print("Begin Descriptive Analysis")

########################| 1. Effective Tax Rate |################################# 

# In environment, we should have:
# data.cross[[i]]
# data.panel[[i]]

# CALL ETR FUNCTION:
source(paste0(aux_source, "/function_ETR.R"))


# Construct ETR at the firm-year level: 
data.cross.ETR <- list()
data.panel.ETR <- list()
for (i in 1: length(files)){
  data.cross.ETR[[i]] <- ETR_FUN(data.cross[[i]], "net_profit") %>% 
    mutate(ETR_minus_STR =  ETR_drop_neg - (STR))
  data.panel.ETR[[i]] <- ETR_FUN(data.panel[[i]], "net_profit") %>% 
    mutate(ETR_minus_STR =  ETR_drop_neg - (STR))
} 
  

########################| 2. Aggregate Descriptive |################################# 

df_name <- c() ## store df names, will be used to store metadata


#### a. Statistics country  ####
temp <- list()
for (i in 1: length(files)){
  
  min_year <- min(data.panel[[i]]$year)
  df <- data.panel[[i]] %>% filter(year == max(year))
  
  df <- df %>% 
    summarize(year = mean(year),
              count = n(),
              count_scope = sum(if_else(orbis_match == 1, 1, 0)),
              count_sez = sum(if_else(sez == 1, 1, 0)),
              count_sez_orbis = sum(if_else(sez == 1 & orbis_match == 1, 1, 0)),
              sum_turn =  sum(turnover, na.rm = T),
              pos_turn =  length(tax_ID[turnover>0]),
              sum_taxliab =  sum(pos_taxliab, na.rm = T),
              pos_taxliab = length(tax_ID[pos_taxliab>0]),
              pos_income = length(tax_ID[pos_income>0]),
              sum_profit =  sum(net_profit, na.rm = T),
              sum_profit_scope =  sum(if_else(orbis_match == 1, net_profit, 0), na.rm = T),
              sum_profit_scope_alt =  sum(net_profit[orbis_match == 1], na.rm = T),
              sum_profitpos =  sum(pos_profit, na.rm = T),
              sum_grossprofit =  sum(gross_profit, na.rm = T),
              sum_grossprofitpos =  sum(gross_profit[gross_profit>0], na.rm = T),
              pos_profit = length(tax_ID[pos_profit>0]),
              profitable = sum(profitable, na.rm = T),
              turnover_avg =  mean(turnover_usd_real, na.rm = T), 
              log_GDP_pc = mean(log_GDP_pc, na.rm = T),
              GDP_pc_USD = mean(GDP_pc_const2015, na.rm = T),
              STR = max(STR)) %>% 
    mutate("Turnover>0" = 100*pos_turn/count,
           "Total Income>0" = 100*pos_income/count,
           "Tax liability>0" = 100*pos_taxliab/count,
           "Net profit>0" = 100*pos_profit/count,
           "profitable" = 100*profitable/count)
  
  df$country <- country_code[[i]]
  df$country_name <- country_name[[i]]
  df$min_year <- min_year
  temp[[i]] <- df
}
df.ETR.descr <- do.call("rbind", temp)
df_name <- append(df_name, "df.ETR.descr")

# a.2 Descriptive for GMT countries
temp <- list()
for (i in 1: length(files)){
  
  min_year <- min(data.panel[[i]]$year)
  df <- data.panel.ETR[[i]] %>% filter(year == max(year))
  
  df <- df %>% 
    summarize(year = mean(year),
              count = n(),
              count_scope = sum(if_else(orbis_match == 1, 1, 0)),
              sum_profit =  sum(net_profit, na.rm = T),
              sum_profit_scope =  sum(if_else(orbis_match == 1, net_profit, 0), na.rm = T),
              sum_profit_scope_alt =  sum(net_profit[orbis_match == 1], na.rm = T),
              mean_ETR_scope = mean(ETR_keep_neg[orbis_match == 1], na.rm = T),
              mean_ETR_scope_prof = mean(ETR_drop_neg[orbis_match == 1], na.rm = T))
  
  df$country <- country_code[[i]]
  df$country_name <- country_name[[i]]
  df$min_year <- min_year
  temp[[i]] <- df
}
df.ETR.gmt <- do.call("rbind", temp)
df_name <- append(df_name, "df.ETR.gmt")


#### b. Stats on Percentile Distribution: shares ####

temp <- list()
for (i in 1: length(files)){
  
  df <- data.cross[[i]] 
  
  df <- df %>% select(year, percentile, pos_profit, total_income, pos_taxliab, labor_inp) %>%
    group_by(year, percentile) %>% 
    fsum() 
  
  df <- df %>% mutate(pos_profit =  100*pos_profit/sum(pos_profit),
                      total_income =  100*total_income/sum(total_income),
                      pos_taxliab =  100*pos_taxliab/sum(pos_taxliab),
                      labor_inp = 100*labor_inp/sum(labor_inp))
  
  df$country <- country_name[[i]]
  temp[[i]] <- df
}
df.perc.distribution <- do.call("rbind", temp)
df_name <- append(df_name, "df.perc.distribution")


#### c. Stats on the 90th Percentiles ####

temp <- list()
for (i in 1: length(files)){
  #Employee
  if ("nb_employee" %in% colnames(data.cross[[i]]) == "TRUE") {
    df.e <- data.cross[[i]] %>% filter(percentile == 90) %>% arrange(total_income) 
    df.e <- df.e[1,] 
    df.e <- df.e %>% select(year, country, nb_employee, percentile) 
  } else {
    df.e <- data.frame(country = c(data.cross[[i]]$country[1]),
                       nb_employee = c(NA_real_), 
                       year = c(data.cross[[i]]$year[1]),
                       percentile = c(90))
  }
  
  df.t <- data.cross[[i]] %>% filter(percentile == 90) %>% arrange(turnover_usd_real) 
  df.t <- df.t[1,] 
  df.t <- df.t %>% select(year, country, turnover_usd_real, percentile) 
  
  df <- merge(df.t, df.e, by = c("country", "year", "percentile"), all = T)
  temp[[i]] <- df
}
df.p90.stat <- do.call("rbind", temp)
df_name <- append(df_name, "df.p90.stat")


#### d. Stats by deciles in USD (mean, median) ####
temp <- list()
for (i in 1: length(files)){
  df <- data.cross.ETR[[i]]  %>%
    select(ETR_drop_neg, ETR_keep_neg, total_income, turnover, net_profit, net_tax_liability, decile, index, official_exchange) %>%
    mutate(factor = index/official_exchange,
           total_income = total_income*factor,
           net_profit = net_profit*factor,
           net_tax_liability = net_tax_liability*factor) %>% select(-factor, -official_exchange, -index) 
  
  # Custom mean and median functions to handle NA values
  fmean <- function(x) mean(x, na.rm = TRUE)
  fmedian <- function(x) median(x, na.rm = TRUE)
  
  # Group by decile and calculate mean
  df_mean <- df %>%
    group_by(decile) %>%
    summarise(across(everything(), fmean)) %>%
    mutate(Measure = "mean")
  
  # Group by decile and calculate median
  df_median <- df %>%
    group_by(decile) %>%
    summarise(across(everything(), fmedian)) %>%
    mutate(Measure = "median")
  
  df <- rbind(df_mean, df_median)
  df$country <- country_name[[i]]
  temp[[i]] <- df
}
df.stat.decile.usd <- do.call("rbind", temp)
df_name <- append(df_name, "df.stat.decile.usd")

#### e. Profitability and cost as function of turnover ####
temp <- list()
for (i in 1:length(files)){
  fmean <- function(x) mean(x, na.rm = TRUE)
  # List of variables to divide by turnover
  vars_to_divide <- c("pos_profit", "taxable_profit", "gross_tax_base",
                      "non_deduc_inp", "loss_carryforward", "exempt_income",
                      "dividend_income", "tot_deduc_taxliab", "tot_deduc_taxbase",
                      "year", "percentile_99.9")
  
  # Divide the variables by turnover
  df <- data.cross.ETR[[1]]  %>%
    mutate(across(all_of(vars_to_divide), ~ pmax(pmin(. / turnover, 1), -1), .names = "div_{.col}")) %>%
    select(starts_with("div_"), "percentile_99.9", "year") %>% 
    group_by(percentile_99.9, year) %>% 
    summarise(across(starts_with("div_"), fmean, .names = "mean_{.col}")) %>% 
    ungroup()
  
  # Convert to long format
  temp[[i]] <- df %>%
    pivot_longer(cols = starts_with("mean_"),
                 names_to = "variable",
                 values_to = "value") %>%
    mutate(variable = sub("mean_", "", variable))
  df$country <- country_name[[i]]
  
  print(country_code[[i]])
}
df.perc.turnover <- do.call("rbind", temp)
df_name <- append(df_name, "df.perc.turnover")


#### f. Correlation between total assets and total total income ####
temp <- list()
for (i in 1: length(files)){
  df <- data.panel.ETR[[i]] 
  if ("total_assets" %in% colnames(df) == "TRUE") {
    temp[[i]] <- df %>% 
      mutate(ratio_asset = net_profit/total_assets,
             ratio_asset = if_else(ratio_asset< -1, -1, ratio_asset),
             ratio_asset = if_else(ratio_asset>1, 1, ratio_asset), 
             ratio_income = net_profit/total_income,
             ratio_income = if_else(ratio_income< -1, -1, ratio_income),
             ratio_income = if_else(ratio_income>1, 1, ratio_income)) %>% 
      group_by(year, country, percentile, decile) %>% 
        summarize(ratio_asset = 100*mean(ratio_asset, na.rm = T),
                  ratio_income = 100*mean(ratio_income, na.rm = T),
                  na_asset = 100*length(total_assets[is.na(total_assets) | total_assets==0]) / n()) %>% 
      ungroup()
  }
  print(country_code[[i]])
}
df.ETR.corr.asset <- do.call("rbind", temp)
df_name <- append(df_name, "df.ETR.corr.asset")


#### g. Consistency of precentiles over time ####

# Firms in the percentile X in t, vs firms in percentile X in t+1
temp <- list()
for (i in 1: length(files)){
  df <- data.panel.ETR[[i]] %>% select(country, year, tax_ID, percentile_99.9)
  df <- df %>% 
    arrange(tax_ID, year) %>% group_by(tax_ID) %>% 
    mutate(lag_percentile = dplyr::lag(percentile_99.9)) %>% 
    filter(year != min(year)) %>% group_by(country, year, lag_percentile) %>% 
    summarize(percentile_99.9 = mean(percentile_99.9, na.rm = T))
  temp[[i]] <- df
}
df.percentile.overtime <- bind_rows(temp) 
df_name <- append(df_name, "df.percentile.overtime")

#### h. Macro data: Turnover and counts across years: ####
# would theoretically only need sum_turnover and n from df.ETR.descr

######################## | 3. ETR Descriptive | ################################# 


## 0. Distribution Functions #### 

FUN_ETR_quantile_df = function(source_df, quantile){
  df <- source_df %>% group_by(.data[[quantile]], country, ETR_denominator, year) %>% #log is taken out
    summarize(n_keep = n(),
              n_etr_below15 = sum(if_else(ETR_drop_neg < 15, 1, 0), na.rm =T),
              n_etr_below15_1 = sum(if_else(ETR_keep_neg < 15 & net_profit>0, 1, 0), na.rm =T),
              n_etr_below15_2 = length(ETR_drop_neg[ETR_drop_neg<15]),
              n_foreign = sum(if_else(foreign_ownership == 1, 1, 0), na.rm =T),
              n_scope = sum(if_else(orbis_match == 1, 1, 0), na.rm =T),
              n_liable_cbcr = sum(if_else(liable_cbcrc_y1 == 1, 1, 0), na.rm =T),
              n_keep_sez = sum(if_else(sez == 1, 1, 0), na.rm =T),
              n_keep_sez_orbis = sum(if_else(sez == 1 & orbis_match==1, 1, 0), na.rm =T),
              n_drop = length(ETR_drop_neg[!is.na(ETR_drop_neg)]),
              ETR_drop_avg= mean(ETR_drop_neg, na.rm =T),
              ETR_keep_avg= mean(ETR_keep_neg, na.rm =T),
              ETR_drop_avg_sez= mean(if_else(sez == 1, ETR_drop_neg, NA_real_), na.rm = T),
              ETR_keep_avg_sez= mean(if_else(sez == 1, ETR_keep_neg, NA_real_), na.rm = T),
              ETR_drop_med= median(ETR_drop_neg, na.rm =T),
              ETR_keep_med= median(ETR_keep_neg, na.rm =T),
              ETR_minus_STR = mean(ETR_minus_STR, na.rm = T),
              STR = mean(STR, na.rm = T),
              log_GDP_pc = mean(log_GDP_pc, na.rm = T)) 
}


FUN_ETR_sector = function(source_df, quantile){
  df <- source_df %>% 
 mutate(sector = case_when(
   section == "C" ~ "Manufacturing",
   section == "G" ~ "Retail",
   section %in% c("A", "B", "D", "E", "F") ~ "Primary/Secondary",
   section %in% c("H", "I", "N", "O", "Q", "R", "S") ~ "Services",
   section %in% c("J", "M", "P") ~ "Knowledge-Based",
   section %in% c("K", "L") ~ "Financial & Estate",
   TRUE ~ "Other"),
   sector = factor(sector, levels= c("Primary/Secondary", "Manufacturing", "Retail", "Services", "Knowledge-Based", "Financial & Estate", "Other")),
   
   largesector = case_when(
      section == "G" ~ "Retail",
      section %in% c("A", "B") ~ "Primary",
      section %in% c("C", "D", "E", "F") ~ "Secondary",
      sector %in% c("Services", "Knowledge-Based", "Financial & Estate") ~ "Services",
      TRUE ~ "Other"),
      largesector = factor(largesector, levels= c("Primary", "Secondary", "Retail", "Services"))) %>% 
    group_by(country, log_GDP_pc, largesector, ETR_denominator, .data[[quantile]], year) %>%
    summarize(n_keep = n(),
              n_drop = length(ETR_drop_neg[!is.na(ETR_drop_neg)]),
              ETR_drop_avg= mean(ETR_drop_neg, na.rm =T),
              ETR_keep_avg= mean(ETR_keep_neg, na.rm =T),
              ETR_drop_med= median(ETR_drop_neg, na.rm =T),
              ETR_keep_med= median(ETR_keep_neg, na.rm =T),
              STR = mean(STR, na.rm = T)) %>% 
    ungroup()
}



#### a.  Average Effective tax rate (weighted by turnover) ####
temp <- list()
for (i in 1: length(files)){
  temp[[i]] <-  data.cross.ETR[[i]] %>%  group_by(country, ETR_denominator) %>% 
    mutate(ETR_drop_neg_sez = if_else(sez == 1, ETR_drop_neg, NA_real_)) %>% 
    mutate(ETR_keep_neg_sez = if_else(sez == 1, ETR_keep_neg, NA_real_)) %>% 
    summarise(ETR_drop_wgt = sum(ETR_drop_neg*turnover, na.rm = T)/sum(turnover), 
              ETR_keep_wgt = sum(ETR_keep_neg*turnover, na.rm =T)/sum(turnover),
              ETR_drop_neg= mean(ETR_drop_neg, na.rm =T), 
              ETR_keep_neg= mean(ETR_keep_neg, na.rm =T),
              ETR_drop_neg_sez= mean(ETR_drop_neg_sez, na.rm = T),
              ETR_keep_neg_sez= mean(ETR_keep_neg_sez, na.rm = T)) %>% 
    rename("Drop" = "ETR_drop_neg", "Keep" = "ETR_keep_neg", "Drop (weighted)" = "ETR_drop_wgt", "Keep (weighted)" = "ETR_keep_wgt", "Keep (SEZ)" = "ETR_keep_neg_sez", "Drop (SEZ)" = "ETR_drop_neg_sez") %>% 
    gather("treatment neg", value, "Drop (weighted)":"Keep (SEZ)") %>% 
    ungroup() %>% spread(ETR_denominator, value) %>% 
    mutate_if(is.numeric, round, digits = 1)   
  print(country_code[[i]])
}
df.ETR.avg.table <- do.call("rbind", temp)
df_name <- append(df_name, "df.ETR.avg.table")


#### b.i. ETR by firm size (percentile) ####
temp <- list()
for (i in 1: length(files)){
  temp[[i]] <- data.cross.ETR[[i]] %>% FUN_ETR_quantile_df(., "percentile_99.9") 
  print(country_code[[i]])
}
df.ETR.size.p <- do.call("rbind", temp)
df_name <- append(df_name, "df.ETR.size.p")

#### b.ii. ETR by firm size (exclude dividend income percentile) ####
temp <- list()
for (i in 1: length(files)){
  temp[[i]] <-  ETR_FUN(data.cross[[i]], "net_profit_div") %>% 
    mutate(ETR_minus_STR =  ETR_drop_neg - (STR)) %>% 
    FUN_ETR_quantile_df(., "percentile_99.9") 
  print(country_code[[i]])
}
df.ETR.size.p.div <- do.call("rbind", temp)
df_name <- append(df_name, "df.ETR.size.p.div")


#### c. ETR by firm size Panel (percentile) ####
temp <- list()
for (i in 1: length(files)){
  temp[[i]] <- data.panel.ETR[[i]] %>% FUN_ETR_quantile_df(., "percentile_99.9") 
  print(country_code[[i]])
}
df.ETR.panel.cross <- do.call("rbind", temp)
df_name <- append(df_name, "df.ETR.panel.cross")


#### d. ETR by firm size (percentile robust (10 bins)) ####
temp <- list()
for (i in 1: length(files)){
  temp[[i]] <- data.cross.ETR[[i]] %>%
    select(-percentile_99.9) %>% 
    rename(percentile_99.9 = percentile_rob) %>% 
    FUN_ETR_quantile_df(., "percentile_99.9") 
  print(country_code[[i]])
}
df.ETR.size.p.rob <- do.call("rbind", temp)
df_name <- append(df_name, "df.ETR.size.p.rob")


#### e. ETR Robustness: numerator ####
temp <- list()
for (i in 1: length(files)){
  
  df <- data.cross[[i]] %>% 
  mutate(
  ## Foreign tax credits
  foreign_credit = if_else(is.na(cred_foreign_tax) | cred_foreign_tax<0, 0, cred_foreign_tax),  
  ntl_noftc = net_tax_liability + foreign_credit, 
  ## All credits
  other_credit = if_else(is.na( other_cred_taxliab) |  other_cred_taxliab<0, 0,  other_cred_taxliab),
  ntl_noelse	= net_tax_liability + foreign_credit + other_credit,
  ## losses
  loss_carryforward = as.numeric(loss_carryforward),
  loss_carryforward = ifelse(loss_carryforward<0 | is.na(loss_carryforward), 0, loss_carryforward),
  tot_deduc_taxliab = ifelse(tot_deduc_taxliab<0 | is.na(tot_deduc_taxliab), 0, tot_deduc_taxliab),
  new_taxbase = net_tax_base+loss_carryforward,
  ntl_nolcf = new_taxbase*(STR/100)-tot_deduc_taxliab,
  # Because we reconstruct the tax liab based on the STR, we make sure that:
  ntl_nolcf = if_else(ntl_nolcf<net_tax_liability, net_tax_liability, ntl_nolcf)) 

    df.0 <- ETR_num_FUN(df, "net_tax_liability") %>% 
    mutate(ETR_minus_STR =  ETR_drop_neg - (STR)) %>% FUN_ETR_quantile_df(., "percentile_99.9") 
  df.1 <- ETR_num_FUN(df, "ntl_noftc") %>% 
    mutate(ETR_minus_STR =  ETR_drop_neg - (STR)) %>% FUN_ETR_quantile_df(., "percentile_99.9") 
  df.2 <- ETR_num_FUN(df, "ntl_noelse") %>%  
    mutate(ETR_minus_STR =  ETR_drop_neg - (STR)) %>% FUN_ETR_quantile_df(., "percentile_99.9") 
  df.3 <- ETR_num_FUN(df, "ntl_nolcf") %>%  
    mutate(ETR_minus_STR =  ETR_drop_neg - (STR)) %>% FUN_ETR_quantile_df(., "percentile_99.9") 
  
  temp[[i]] <- rbind(df.0, df.1, df.2, df.3) 

  print(country_code[[i]])
}
df.ETR.size.num.p <- do.call("rbind", temp)
df_name <- append(df_name, "df.ETR.size.num.p")



#### f. ETR Panel Effective tax rate (one measure of ETR for all years), by firm size (percentile) ####
temp <- list()
for (i in 1: length(files)){
  df.1 <- ETR_balpanel_FUN(data.panel.ETR[[i]], 1) ## Decide number of years a firm stays in the panel
  df.2 <- ETR_balpanel_FUN(data.panel.ETR[[i]], 2) ## Decide number of years a firm stays in the panel
  df.3 <- ETR_balpanel_FUN(data.panel.ETR[[i]], 3) ## Decide number of years a firm stays in the panel
  df.4 <- ETR_balpanel_FUN(data.panel.ETR[[i]], 4)
  df.5 <- ETR_balpanel_FUN(data.panel.ETR[[i]], 5) 
  df <- rbind(df.1, df.2, df.3, df.4, df.5)
  temp[[i]] <- df
  print(country_code[[i]])
}
df.ETR.panel.bal <- do.call("rbind", temp)
df_name <- append(df_name, "df.ETR.panel.bal")


#### d. ETR by Asset/Employees/Payroll percentile (different than turnover) ####

FUN_new_perc = function(source_df, var){
if (all(is.na(c(source_df[[var]]))) == "FALSE") { 
  name <- var
  df <- source_df %>% mutate(perc_new = as.numeric(.data[[var]]),
                      perc_new =  ntile(perc_new, 100), 
                      # We want percentile to go from 0 to 99
                      perc_new = perc_new-1)
  # Decompose percentile 99 in 10 bins
  top99 <- df %>% filter(perc_new == 99) %>%  mutate(p_99 = ntile(.data[[var]], 10)) %>%
    select(tax_ID, p_99)  %>% ungroup()
  
  df <- merge(df, top99, by =c("tax_ID"), all = TRUE) %>%
    mutate(p_99 = as.numeric(p_99),
           perc_new_99.9 = if_else(!is.na(p_99), perc_new+(p_99-1)/10, perc_new)) %>% ungroup()
  
  # Get correlation coefficient between two measures of percentiles
  corr.test <- cor.test(df$percentile_99.9, df$perc_new_99, method = c("pearson"))
  
  
  df <- df %>%  group_by(perc_new_99.9, country) %>%
    summarize(n_keep = n(),
              n_drop = length(ETR_drop_neg[!is.na(ETR_drop_neg)]),
              ETR_drop_avg= mean(ETR_drop_neg, na.rm =T),
              ETR_keep_avg= mean(ETR_keep_neg, na.rm =T),
              temp = mean(.data[[var]], na.rm = T),
              perc_99.9_avg = mean(percentile_99.9, na.rm = T),
              perc_99.9_med = median(percentile_99.9, na.rm = T)) %>% 
     rename(!!name := temp)
  df$correlation <- corr.test$estimate
  return(df)
}}



temp_empl <- list()
temp_asset <- list()
temp_payroll <- list()

for (i in 1: length(files)){
  temp_empl[[i]] <- FUN_new_perc(data.cross.ETR[[i]], "nb_employee")
  temp_payroll[[i]] <- FUN_new_perc(data.cross.ETR[[i]], "labor_inp")
  temp_asset[[i]] <- FUN_new_perc(data.cross.ETR[[i]], "total_assets")
  
  print(country_code[[i]])
}
df.ETR.nbemployee <- do.call("rbind", temp_empl)
df.ETR.payroll <- do.call("rbind", temp_payroll)
df.ETR.assets <- do.call("rbind", temp_asset)
df_name <- append(df_name, c("df.ETR.nbemployee", "df.ETR.payroll", "df.ETR.assets"))  



#### e. ETR by sector ####
temp_sec <- list()
temp_largesec <- list()
temp_largesec.d <- list()

for (i in 1:length(files)){
  temp_largesec[[i]] <- FUN_ETR_sector(data.cross.ETR[[i]],  "percentile_99.9")
  temp_largesec.d[[i]] <- FUN_ETR_sector(data.cross.ETR[[i]],  "decile")
  
  print(country_code[[i]])
}
df.ETR.largesector.p <- do.call("rbind", temp_largesec)
df.ETR.largesector.dec <- do.call("rbind", temp_largesec.d)
df_name <- append(df_name, c("df.ETR.largesector.p", "df.ETR.largesector.dec"))


#### g. Tax gap - ETR: revenue forgone (all years) ####
temp <- list()
for (i in 1: length(files)){
  df <- data.panel.ETR[[i]] %>% filter(!is.na(ETR_drop_neg) & !is.na(net_profit)) %>%
    mutate(STR = STR/100,
           ETR = ETR_drop_neg/100, #Here we take into account the capped ETR
           STR_ETR = STR-ETR,
           gap = pos_profit*STR_ETR,
           ntl_built = pos_profit*ETR,
           np_str = pos_profit*STR) %>%
    group_by(country, year, log_GDP_pc) %>%
    summarise(sum_ntl_built = sum(ntl_built, na.rm = T),
              sum_ntl = sum(pos_taxliab, na.rm = T),
              sum_gap = sum(gap, na.rm = T),
              sum_np_str = sum(np_str, na.rm = T)) %>%
    mutate(rev_forgone = 100*(sum_gap/(sum_ntl+sum_gap)),
           rev_forgone_built = 100*(sum_gap/(sum_ntl_built+sum_gap)),
           rev_forgone_str = 100*(sum_gap/(sum_np_str)))
    temp[[i]] <- df 
  print(country_code[[i]])
}
df.taxgap.cap <- do.call("rbind", temp)
df_name <- append(df_name, "df.taxgap.cap")


#### h. Share of ETR below 15% across firm distribution ####
temp <- list()
for (i in 1: length(files)){
  temp[[i]] <- data.cross.ETR[[i]] %>% 
    mutate(below15_keep = if_else(ETR_keep_neg < 15, 1, 0),
           below15_drop = if_else(ETR_drop_neg < 15, 1, 0)) %>% 
    group_by(percentile_99.9, country, ETR_denominator, year) %>% #log is taken out
    summarize(n_keep = n(),
              n_drop = length(ETR_drop_neg[!is.na(ETR_drop_neg)]),
              below15_drop = sum(below15_drop, na.rm =T),
              below15_keep= sum(below15_keep, na.rm =T),
              STR = mean(STR, na.rm = T),
              log_GDP_pc = mean(log_GDP_pc, na.rm = T)) %>% ungroup() %>% 
    mutate(share_below15_drop = 100*below15_drop/n_drop,
           share_below15_keep = 100*below15_keep/n_keep)
  print(country_code[[i]])
}
df.ETR.share15.p <- do.call("rbind", temp)
df_name <- append(df_name, "df.ETR.share15.p")



print("All dataframes created")



######################## | 4. EXTRACT METADATA | ################################# 


# Export some metadata in csv format
setwd(metadata)

# --List all graphs to append
list.name <- df_name 

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




print("End Analysis")



