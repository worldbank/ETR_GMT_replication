
### 1. Prepare data 

print("Begin cleaning")


#### 1. Country specific cleaning: Special regimes ####
#...............................................................................
# Some firms have to be excluded because data includes non-general CIT regimes
# This is done on a case by case basis, as some firms can face both a special regime or the CIT, in that case we must include them
# Minimum tax or reduced rate are also included; depending on the country
data.panel <- list()

for (i in 1: length(files)){
  
  temp <- data_raw[[i]]
  
  if (country_code[[i]]=="ISO3"){
    temp <- temp %>% mutate(liable_cbcrc_y1 = 0)
  }
  
  if ("special_regime" %in% colnames(temp) == "FALSE") {
    temp <- temp %>% mutate(special_regime = NA_integer_)
  }
  
  if ("foreign_ownership" %in% colnames(temp) == "FALSE") {
    temp <- temp %>% mutate(foreign_ownership = 0)
  }
  
  if ("orbis_match" %in% colnames(temp) == "FALSE") {
    temp <- temp %>% mutate(orbis_match = 0)
  }
  
  if ("sez" %in% colnames(temp) == "FALSE") {
    temp <- temp %>% mutate(sez = 0)
  }

## Specific cleaning for the ETR analysis: sources part of the results we got from GMT: 

if (prepare_data == "ETR_cleaning"){
  if (country_code[[i]] %in% country_GMT){
    df_cbcr <-  read_excel(paste0(gitHub, "ETR/output_GMT/", country_code[[i]], "_Groups_cbcr_TA9.xlsx"))
    temp <- left_join(temp, df_cbcr, by = c("group_m")) 
    temp <- temp %>% mutate(liable_cbcrc_y1 = as.numeric(liable_cbcrc_y1))
    
  }}

  data.panel[[i]] <- temp
  
} #end loop



#### 2. General cleaning #####
#...............................................................................

# + WDI variables: ####

for (i in 1:length(files)) {
  print(country_code[[i]])

  WDI_data <- read.csv(paste0(gitHub, project, "/input/WDI/WDI_vars_", country_code[[i]], ".csv"))
  
  # Merge
  data.panel[[i]] <- left_join(data.panel[[i]], WDI_data, by = c("country", "year")) %>% 
    mutate(log_GDP_pc = log(GDP_pc_const2015))


    # For Greek exchange rate, we use the OECD rate:
    if (country_code[[i]]=="GRC"){ 
      
      oecd <- read.csv(paste0(gitHub, project, "/input/prep/OECD_GRC_EXCHANGE_RATES_2024.csv")) %>% mutate(Time = as.numeric(TIME_PERIOD), ObsValue = as.numeric(OBS_VALUE)) %>% 
        select(Time, ObsValue)
      
      data.panel[[i]]  <- left_join(data.panel[[i]], oecd, by =c("year"= "Time")) %>% 
        mutate(official_exchange = if_else(is.na(official_exchange), ObsValue, official_exchange)) %>% select(-ObsValue)
    }
    }


# + Adjust variables: ####

# Get inflation rates for USD, base year is 2019: 
 index_2019 <- read.csv(paste0(gitHub, project, "/input/prep/", "/index_2019.csv")) %>% select(-X)
 
 
 var_to_numeric <- c("material_inp", "labor_inp", "operating_inp", "capital_inp", "financial_inp", "depreciation", "other_inp",
                     "net_profit", "net_tax_liability", "ntl_noelse", "cred_foreign_tax", "other_cred_taxliab",
                     "trade_taxliab", "dividend_income", "dividend_exempt")
 

for (i in 1: length(files)){
  temp <- data.panel[[i]]
  
  temp <- temp %>% 
    mutate_at(var_to_numeric, as.numeric)
  
  # USD and inflation:
  temp <- merge(temp, index_2019, by=c("year")) %>% 
          mutate(turnover_usd = turnover/official_exchange,
                 turnover_usd_real = if_else(turnover_usd>0, turnover_usd/index, NA_real_)) #,
                 # log_turn_usd = if_else(total_income>0, log(turnover_usdadj), NA_real_),
                 # log_GDP_pc = log(GDP_pc_const2015))
  
  
  # Only keep active firms: ie turnover > 1. 
  # Some firms report 0.1 turnover. Limiting to 0 would still produce outlier ratios.
  temp <- temp %>% filter(turnover > 1 & !is.na(turnover))
  

  # Define total income where we exclude incomes from dividends:
  # Redefine net profit without dividend income as well:
  temp <- temp %>%
    mutate(
      total_income_no_div = case_when(
        !is.na(dividend_exempt) ~ total_income - dividend_exempt,
        is.na(dividend_exempt) &
          !is.na(dividend_income) ~ total_income - dividend_income,
        TRUE ~ total_income
      ),
      net_profit_div = net_profit,
      net_profit_no_div = case_when(
        !is.na(dividend_exempt) ~ net_profit - dividend_exempt,
        is.na(dividend_exempt) &
          !is.na(dividend_income) ~ net_profit - dividend_income,
        TRUE ~ net_profit
      ),
      net_profit = net_profit_no_div
    )
  
  
  # Create variables we will use regularly:
  temp <- temp %>% mutate(STR = STR*100,
                        pos_income = if_else(total_income > 0 & !is.na(total_income), total_income, 0),
                        pos_taxliab = if_else(net_tax_liability > 0 & !is.na(net_tax_liability), net_tax_liability, 0),
                        pos_profit = if_else(net_profit > 0 & !is.na(net_profit), net_profit, 0),
                        profitable = if_else(net_profit>0 | (net_profit<=0 & net_tax_base>0), 1, 0)) 



# + Define the distribution of firms: ####
      temp <- temp %>% group_by(year) %>%
        mutate(decile = ntile(total_income, 10),
               # We want percentile to go from 0 to 99
               percentile = ntile(total_income, 100) - 1) %>% ungroup()

      # Split into sub-bins
      # Deciles
      sub <- temp %>% group_by(year) %>% filter(decile == max(decile)) %>%
        mutate(sub_bin5 = ntile(total_income, 5),
               sub_bin10 = ntile(total_income, 10)) %>% select(year, tax_ID, sub_bin5, sub_bin10) %>% ungroup()

      temp <- left_join(temp, sub, by = c("year", "tax_ID")) %>%
        group_by(year) %>%
        mutate(decile5 = if_else(!is.na(sub_bin5), as.numeric(decile) + (sub_bin5-1)/10, as.numeric(decile)),
               decile10 = if_else(!is.na(sub_bin10), as.numeric(decile) + (sub_bin10-1)/10, as.numeric(decile))) %>% ungroup() %>%
        select(-sub_bin5, -sub_bin10)

      # Percentile
      sub <- temp %>% group_by(year) %>% filter(percentile == max(percentile)) %>%
        mutate(sub_bin5 = ntile(total_income, 5),
               sub_bin10 = ntile(total_income, 10)) %>% select(year, tax_ID, sub_bin5, sub_bin10) %>% ungroup()

      temp <- left_join(temp, sub, by = c("year", "tax_ID")) %>%
        group_by(year) %>%
        mutate(percentile_99.9 = if_else(!is.na(sub_bin5), as.numeric(percentile) + (sub_bin5-1)/10, as.numeric(percentile)),
               percentile_rob = if_else(!is.na(sub_bin10), as.numeric(percentile) + (sub_bin10-1)/10, as.numeric(percentile))) %>% ungroup() %>%
        select(-sub_bin5, -sub_bin10)

      #only if split top 1 percent in 5
      temp <- temp %>% mutate(percentile_99.9 = case_when(percentile_99.9 == 99.4 ~ 99.9,
                                                          percentile_99.9 == 99.3 ~ 99.7,
                                                          percentile_99.9 == 99.2 ~ 99.5,
                                                          percentile_99.9 == 99.1 ~ 99.3,
                                                          percentile_99.9 == 99.0 ~ 99.1,
                                                          TRUE ~ percentile_99.9))


      
  data.panel[[i]]  <- temp 
  print(country_code[[i]])
}

print("Additional Cleaning: harmonized some variables")

 
#### 3. Sample Restriction: ####
#...............................................................................

# + Sample restriction for all: 
#  keep last year available for each year. 

data.cross <- list()

for (i in 1: length(files)){
  data.cross[[i]] <- data.panel[[i]] %>% 
    filter(year < 2020) %>%  ### Do not include COVID years
    filter(year == max(year))
}

for (i in 1: length(files)){
  # No restriction of year at this point for the panel data
  data.panel[[i]]  <- data.panel[[i]] %>%
filter(year < 2020)
 }


# Remove list/object for memory purposes
rm(temp, df, WDI_data, index_2019, sub)


print("End cleaning")

