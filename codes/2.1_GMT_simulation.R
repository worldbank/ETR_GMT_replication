
print("Begin GTM Analysis")

#########################################################.
# REQUIREMENTS:

# variable "group" needs to exist
# orbis_match == 1 also need to exist
# STR, in the form of decimal and not percentage (so far)
# foreign_ownership
# Need to add the n everywhere for South Africa.
# nb_employee 

# Notes: this file uses loops instead of functions, which might increase 
# running time.

# In environment, we should have:
# data.cross[[i]]
# data.panel[[i]]

########################| A. SET UP |################################# 

# CALL ETR FUNCTION:
source(paste0(aux_source, "/function_ETR.R"))


# Load Raw Aggregate CbCR Data 
cbcr_oecd <- read.csv(paste0(gitHub, project, "/input/prep/CBCR_TABLEI_06032024193031092.csv"))
cbcr <- cbcr_oecd %>% filter(Grouping == "All Sub-Groups" &  
                             (CBC == "ENTITIES_COUNT" | #The number of entities present in the tax jurisdiction
                                CBC == "SUBGROUPS_COUNT" |  #Refer to confidentiality issues
                                CBC == "CBCR_COUNT" |  #Number of MNE groups that reported operations in the partner jurisdiction
                                CBC == "TOT_REV" | # Total revenue of all entities of MNE groups
                                CBC == "EMPLOYEES" |
                                CBC == "PROFIT" |
                                CBC == "TAX_PAID" |
                                CBC == "TAX_ACCRUED" ) 
) %>%
  select(-YEA, -PAN,-Grouping, -Variable, -Flags, - Flag.Codes) %>% 
  spread(CBC, Value)

# Restrict sample to ETR paper countries
cbcr <- cbcr %>% filter(JUR %in% country_code) %>% ungroup()

cbcr_country_level <- cbcr %>%
  # some entities count are NA, we assume they are at least the number of CbCR count (might be larger)
  mutate(ENTITIES_COUNT = if_else(is.na(ENTITIES_COUNT), CBCR_COUNT, ENTITIES_COUNT)) %>%
  group_by(JUR, Year) %>%
  summarize(
    n_entities = sum(ENTITIES_COUNT, na.rm = T),
    n_groups = sum(CBCR_COUNT, na.rm = T),
    sum_revenue = sum(TOT_REV, na.rm = T),
    sum_employee = sum(EMPLOYEES, na.rm = T),
    sum_tax = sum(TAX_PAID, na.rm = T),
    sum_profit = sum(PROFIT, na.rm = T) 
  ) %>%
  rename(year = Year) %>%
  mutate(sample = "CbCr") %>% ungroup()


# Select only data that we need based on country_GMT list: 
data.GMT <- list()
for (i in 1: length(data.panel)){
 df <- data.panel[[i]]
 if (df$country[[1]] %in% country_GMT){
   print(paste0(country_code[[i]], " is used for GMT analysis"))
   data.GMT <- append(data.GMT, list(df))
 }}
  

################| B. MAIN COUNTRY LOOP STARTS HERE: |########################### 

######| 1. Country specific parameters |###### 


for (c in 1: length(data.GMT)){
  
  data <- data.GMT[[c]] 
# Define years and CPI index (from WDI data) for simulation: 

if (country_GMT[[c]]=="ISO3"){
  index <- 0.8580305 ## REPLACE HERE
  year_t <- 2018 ## REPLACE HERE
  year_1_t <- 2017 ## REPLACE HERE
}


iso3 <- country_GMT[[c]]
print(iso3)


desc.cbcr <- cbcr_country_level %>% filter(JUR == iso3 & year == 2018) %>%
  select(-JUR, -year) %>% mutate(threshold_percentile = NA_real_)



######| 2. START BUILT IN TAX RATE LOOP HERE |###### 

### FOR 15% and 20%: 

tau <- c(0.15, 0.2) 

for (r in 1: length(tau)){
  print(tau[[r]])
  path <- paste0("GMT_", tau[[r]]*100, "/")
  setwd(paste0(metadata, path))


# + Adjustment based on panel dimension of the data:
#................................

# Adjusted covered taxes take into account five preceding years, we will restrict it to a 2-year window
  
# + Income and profits are net of dividend incomes: 
#................................

df <- data %>% mutate(net_profit = net_profit_no_div,
                      group = if_else(is.na(group) | group == "", as.character(tax_ID), as.character(group)))

# Compute deferred tax asset at the entity level:
df <- df %>% filter(year == year_1_t | year == year_t) %>% 
  mutate(loss_tax_base = if_else(net_profit < 0, abs(net_profit), 0),
         tax_asset = loss_tax_base * tau[[r]]) %>%
  group_by(tax_ID) %>%
  arrange(tax_ID, year) %>%
  mutate(
    deferred_tax_asset = lag(tax_asset),
    deferred_tax_asset = if_else(is.na(deferred_tax_asset), 0, deferred_tax_asset),
    # Assume no change in the group across year t-1 and year t:
    lead_id = lead(group),
    group = if_else(year == year_1_t, lead_id, group)
  ) %>% ungroup()


# Compute ETR, deferred tax asset and new ETR, at the group level:
deferred_tax_groupFUN = function(source_df){
  STR <- max(source_df$STR)
  source_df %>% 
  mutate(ntl_pos = if_else(net_tax_liability<0, 0, net_tax_liability),
         neg_profit = if_else(net_profit< 0, abs(net_profit), 0)) %>% 
  group_by(group, year) %>% 
  summarize(sum_net_profit = sum(net_profit, na.rm = T),
            sum_neg_profit = sum(neg_profit, na.rm = T),
            sum_ntl_pos = sum(ntl_pos, na.rm = T)) %>% 
  mutate(ETR_group = 100*sum_ntl_pos/sum_net_profit,
         ## Make sure that those above don't fall into the GMT
         ETR_group = if_else(sum_net_profit <= 0 & sum_ntl_pos >0 , STR, ETR_group),
         ETR_group = if_else(sum_net_profit <= 0 & sum_ntl_pos ==0, 0, ETR_group),
         ETR_group = if_else(ETR_group>STR, STR, ETR_group)) %>% 
        # If losses, deferred tax asset = 15%*sum_net_profit
  mutate(missing_tax_to15 = if_else(sum_net_profit>0 & ETR_group<(tau[[r]]*100), sum_net_profit*tau[[r]]-sum_ntl_pos, NA_real_),
         deferred_tax_asset_group = case_when(sum_net_profit<=0 ~ sum_neg_profit*tau[[r]],
                                               sum_net_profit>0 & ETR_group>=(tau[[r]]*100) ~ sum_neg_profit*tau[[r]],
                                               sum_net_profit>0 & ETR_group<(tau[[r]]*100) ~ sum_neg_profit*tau[[r]]- missing_tax_to15),
         deferred_tax_asset_group = case_when(sum_net_profit>0 & ETR_group<(tau[[r]]*100) & (sum_neg_profit*tau[[r]]< missing_tax_to15) ~ 0,
                                               TRUE ~ deferred_tax_asset_group),
         
         # check:
         deferred_tax_asset_group = if_else(deferred_tax_asset_group <0, 0, deferred_tax_asset_group),
         new_ETR_group = case_when(sum_net_profit>0 & ETR_group<(tau[[r]]*100) & (sum_neg_profit*tau[[r]]>= missing_tax_to15) ~ (tau[[r]]*100), 
                                   sum_net_profit>0 & ETR_group<(tau[[r]]*100) & (sum_neg_profit*tau[[r]]< missing_tax_to15) ~ 100*(sum_neg_profit*tau[[r]])/sum_net_profit,
                                    TRUE ~ ETR_group),
         new_ETR_group = if_else(new_ETR_group<ETR_group, ETR_group, new_ETR_group),
         new_ntl_group = case_when(sum_net_profit>0 ~ (new_ETR_group/100)*sum_net_profit,
                                   sum_net_profit<=0 ~ sum_ntl_pos),
         # With tax asset, tax liab should be higher, if not, set the other one. 
         new_ntl_group = if_else(new_ntl_group<sum_ntl_pos, sum_ntl_pos, new_ntl_group)
         )
  }

## Apply to year t-1 so that we can offset deferred tax asset in year t: 
df.group <- deferred_tax_groupFUN(df) %>% 
  arrange(group, year) %>%
  mutate(
    deferred_tax_asset_group = lag(deferred_tax_asset_group),
    deferred_tax_asset_group = if_else(is.na(deferred_tax_asset_group), 0, deferred_tax_asset_group)) %>% ungroup() %>% 
  select(deferred_tax_asset_group, year, group, new_ETR_group, ETR_group, new_ntl_group)


df <- left_join(df, df.group, by = c("group", "year"))


# + Restrict data to year t:
#................................

df.iso3 <- ETR_FUN(df %>% filter(year == year_t), "net_profit") %>% 
  #exchange rate, convert to USD
  mutate(net_profit_pos = if_else(net_profit<0, 0, net_profit),
         ntl_pos = if_else(net_tax_liability<0, 0, net_tax_liability),
         ntl_pos_built = if_else(net_profit>0, round(net_profit * (ETR_keep_neg / 100), digits = 0), 0),
         ntl_pos_usd = ntl_pos/official_exchange,
         ntl_pos_built_usd = ntl_pos_built/official_exchange,
         turnover_usd = turnover/official_exchange,
         net_profit_usd = net_profit/official_exchange,
         net_profit_pos_usd = net_profit_pos/official_exchange,
         tax_to_remit_usd = tax_to_remit/official_exchange,
         group = if_else(is.na(group), as.character(tax_ID), as.character(group))
         
  ) %>% 
  # total taxpayer liability
  mutate(total_taxpayers_liab = sum(ntl_pos, na.rm = T),
         total_taxpayers_liab_built = sum(ntl_pos_built, na.rm = T),
         total_taxpayers_profit = sum(net_profit_pos, na.rm = T),
         type = "All")

# Sample is reduced to profitable firms only
df.iso3.prof <- df.iso3 %>% 
  filter(!is.na(ETR_drop_neg)) %>% 
  # total taxpayer liability
  mutate(total_taxpayers_liab = sum(ntl_pos, na.rm = T),
         total_taxpayers_liab_built = sum(ntl_pos_built, na.rm = T),
         total_taxpayers_profit = sum(net_profit_pos, na.rm = T),
         type = "Profitable")


# + General aggregates
#................................


# Define descriptive stat function
FUN_desc = function(source_df){
  source_df %>% group_by(sample) %>%  
    summarize(
      n_entities = n(),
      sum_revenue = sum(turnover_usd, na.rm = T),
      sum_employee = sum(nb_employee, na.rm = T),
      sum_profit = sum(net_profit_usd, na.rm = T),
      sum_profit_pos = sum(net_profit_pos_usd, na.rm = T),
      sum_tax = sum(ntl_pos_usd, na.rm = T),
      sum_tax_built = sum(ntl_pos_built_usd, na.rm = T),
      threshold_percentile = min(percentile_99.9, na.rm = T),
      n_groups = n_distinct(group)) 
}

FUN_agg = function(source_df){
  source_df %>% group_by(year, type) %>%  
    summarize(
      n_entities = n(),
      sum_revenue_usd = sum(turnover_usd, na.rm = T),
      sum_revenue_lcu = sum(turnover, na.rm = T),
      sum_employee = sum(nb_employee, na.rm = T),
      sum_profit_usd = sum(net_profit_usd, na.rm = T),
      sum_profit_pos_usd = sum(net_profit_pos_usd, na.rm = T),
      sum_profit_lcu = sum(net_profit, na.rm = T),
      sum_profit_pos_lcu = sum(net_profit_pos, na.rm = T),
      sum_tax = sum(ntl_pos, na.rm = T),
      sum_tax_built = sum(ntl_pos_built, na.rm = T),
      sum_tax_usd = sum(ntl_pos_usd, na.rm = T),
      sum_tax_usd_built = sum(ntl_pos_built_usd, na.rm = T),
      n_groups = n_distinct(group),
      average_ETR = mean(ETR_keep_neg, na.rm = T),
      average_STR = mean(STR, na.rm = T)) %>% 
    ungroup()
}


#### STEP 1: Firms in Scope ####
#................................................................................

# We have several ways of identifying firms in scope, let's proceed by samples

# Profitable
df.p <- FUN_agg(df.iso3.prof)
# All firms
df.a <- FUN_agg(df.iso3)
df <- rbind(df.p, df.a)

write.csv(df, paste0(iso3, tau[[r]]*100, "_aggregate_statistics.csv"))


# + 0. Method: take the top 1 percent:
#................................

FUN_temp = function(source_df){
  source_df %>% filter(percentile_99.9 >= 99) %>%
    mutate(sample = "Top 1%",
           group = tax_ID)
}

# Sample:
df.top1 <- FUN_temp(df.iso3)
df.top1.prof <- FUN_temp(df.iso3.prof)

# Some descriptive:
desc.top1 <- df.top1 %>% FUN_desc() %>% 
  mutate(restriction = "All",
         n_groups = NA_real_)

desc.top1.prof <- df.top1.prof %>% FUN_desc() %>% 
  mutate(restriction = "Profitable",
         n_groups = NA_real_)


# TOP 1% simulation:

FUN_top1 = function(source_df, restr, dummy_deferred_asset, ETR_var){ 
  
  # Original tax liability
  tax_liab <- source_df %>% group_by(percentile_99.9) %>% 
    summarize(
  old_taxrev = sum(ntl_pos, na.rm = T),
  old_taxrev_built = sum(ntl_pos_built, na.rm = T)) %>% ungroup()
  
  temp <- source_df %>% 
    mutate(
      # Assumption is that negative net profit ensure no taxes paid
      ntl_pos_built = if_else(net_profit>0, round(net_profit * (ETR_keep_neg / 100), digits = 0), 0),
      ntl_pos = round(ntl_pos, digits = 0))
      
      ## DIFFERENT HERE
      # Compute ETR with deferred asset
      if (dummy_deferred_asset == 1 ){
        temp <- temp %>% 
          mutate(
      missing_tax_to15 = if_else(net_profit>0 & ETR_keep_neg<(tau[[r]]*100), net_profit*tau[[r]]-ntl_pos, NA_real_),
      ETR_entity = case_when(net_profit>0 & ETR_keep_neg<(tau[[r]]*100) & (deferred_tax_asset >= missing_tax_to15) ~ (tau[[r]]*100), 
                             net_profit>0 & ETR_keep_neg<(tau[[r]]*100) & (deferred_tax_asset < missing_tax_to15) ~ 100*(deferred_tax_asset)/net_profit,
                             TRUE ~ ETR_keep_neg),
      ETR_entity = if_else(ETR_entity<ETR_keep_neg, ETR_keep_neg, ETR_entity),
      new_ntl= case_when(net_profit>0 ~ round(net_profit * (ETR_entity / 100), digits = 0),
                         net_profit<=0 ~ ntl_pos),
      # With tax asset, tax liab should be higher, if not, set the other one. 
      new_ntl = if_else(new_ntl<ntl_pos, ntl_pos, new_ntl),
      #flag firms with etr<15 (replace by ETR_keep_neg if tax asset))
      less_15 = if_else( .data[[ETR_var]] < (tau[[r]]*100), 1, 0),
      tag_built = if_else(ntl_pos != ntl_pos_built, 1, 0),
      # Compute new tax liability:
      top_up = if_else(less_15==1, (net_profit_pos * tau[[r]]) - new_ntl, 0),
      top_up = if_else(less_15==1 & top_up<0, 0, top_up))
        
      } 
  
      if (dummy_deferred_asset == 0 ){
        temp <- temp %>% 
          mutate(
         #flag firms with etr<15
        less_15 = if_else(ETR_keep_neg < (tau[[r]]*100), 1, 0),
        tag_built = if_else(ntl_pos != ntl_pos_built, 1, 0),
        # Compute new tax liability:
        top_up = if_else(less_15==1, (net_profit_pos * tau[[r]]) - ntl_pos_built, 0) 
        )
      }
       ### if statements end here 
      
     
  df <- temp %>% 
    ## do it for every percentile
    group_by(percentile_99.9) %>%
    summarize(
      n = n(),  # Nb firms per percentile
      n_less15 = sum(less_15, na.rm = T), # Nb firms per percentile with ETR below 15
      top_up = sum(top_up, na.rm = T),
      # Turnover
      turnover_usd = sum(turnover_usd, na.rm = T),
      net_profit_pos = sum(net_profit_pos, na.rm = T)) %>% ungroup()
  
  df <- left_join(df, tax_liab, by = c("percentile_99.9"))
  
  # 1. Share of Firms with ETR below 15% within the top 1% 
  share_below15 <- df %>%  filter(percentile_99.9 > 98) %>% 
    summarize(share_less15_sample = round(100 * sum(n_less15) / sum(n), digits = 1)) %>% pull() 
  share_below15_all <- df %>%  filter(percentile_99.9 > 98) %>% 
    summarize(share_less15_all = round(100 * sum(n_less15) / sum(n), digits = 1)) %>% pull() 
  
  n_below15 <- df %>%  filter(percentile_99.9 > 98) %>% 
    summarize(sum(n_less15)) %>% pull() 
  
  # 2. Conditional on being below 15%, average ETR
  ETR_below15 <- temp %>%  filter(percentile_99.9 > 98 & less_15 == 1) %>% 
    summarize(mean_ETR = mean(.data[[ETR_var]], na.rm =T)) %>% pull() 
  
  # 3. Turnover share 
  turnover_less15 <- temp %>%  filter(percentile_99.9 > 98 & less_15 == 1) %>% 
    summarize(turnover_usd = sum(turnover_usd, na.rm = T)) %>% pull() 
  
  turnover_1pct <- temp %>%  filter(percentile_99.9 > 98) %>% 
    summarize(turnover_usd = sum(turnover_usd, na.rm = T)) %>% pull() 
  
  # 4. Net profit share 
  profit_less15 <- temp %>%  filter(percentile_99.9 > 98 & less_15 == 1) %>% 
    summarize(net_profit_pos = sum(net_profit_pos, na.rm = T)) %>% pull() 
  
  
  # Original tax base: based on the restriction: sum across percentile
  df_old <- df %>% 
    summarise(n = sum(n),
              old_taxrev = sum(old_taxrev),
              old_taxrev_built = sum(old_taxrev_built),
              turnover_usd = sum(turnover_usd, na.rm = T),
              net_profit_pos = sum(net_profit_pos, na.rm = T))
  
  turnover_share15_all <- 100*turnover_less15/df_old$turnover_usd[1]
  turnover_share15_1pct <- 100*turnover_less15/turnover_1pct
  
  profit_share15_all <- 100*profit_less15/df_old$net_profit_pos[1]

  # 4. Tax revenue increase 
  
  # For those in the top 1%, apply the new tax revenue if ETR is minimum 15%, 
  # And compute the revenue change
  top_up <- df %>% filter(percentile_99.9 > 98) %>% 
    summarise(top_up = sum(top_up)) %>% pull()
  
  # Merge both
  df_taxbase_1pct <- df_old %>% 
    mutate(
      sample = "Top 1 percent",
      restriction = restr,
      top_up  = top_up,
      change_built = round( 100 *top_up / old_taxrev_built, digits = 0),
      change = round( 100 *top_up / old_taxrev, digits = 0),
      n_below15 = n_below15,
      turnover_less15 = turnover_less15,
      profit_less15 =profit_less15,
      share_less15 = share_below15,
      share_less15_all = share_below15_all,
      turnover_share15_all = turnover_share15_all,
      profit_share15_all = profit_share15_all,
      turnover_share15_1pct = turnover_share15_1pct,
      ETR_below15 = ETR_below15) 
}

## WITH deferred tax asset
# Profitable 
df.p <- FUN_top1(df.iso3.prof, "Profitable", 1, "ETR_entity")
# All firms 
df.a <- FUN_top1(df.iso3, "All", 1, "ETR_entity")
df <- rbind(df.p, df.a)

write.csv(df, paste0(iso3, tau[[r]]*100, "_1pct_revenue_simul.csv"))

## WITHOUT deferred tax asset
# Profitable 
df.p <- FUN_top1(df.iso3.prof, "Profitable", 0, "ETR_keep_neg")
# All firms  
df.a <- FUN_top1(df.iso3, "All", 0, "ETR_keep_neg")
df <- rbind(df.p, df.a)

write.csv(df, paste0(iso3, tau[[r]]*100, "_1pct_revenue_simul_nodat.csv"))

rm(df.a, df.p, df)  



# + CbcR macro--top up:  
# Calculate top-up tax on all MNEs (accordingly to globe rules without carve outs)
# then rank top-up tax and take a number that is smaller than or equal to the N in CbCr data
#................................

FUN_temp = function(source_df){
  df <- source_df %>% 
    filter(foreign_ownership == 1) %>% 
    mutate(sample = "Largest MNEs (top up)") 

   ## First step: compute ETR with deferred tax assets (group level): 
  df.group <- df %>% 
    group_by(group, year) %>% 
    mutate(sum_net_profit = sum(net_profit, na.rm = T),
           missing_tax_to15 = if_else(sum_net_profit>0 & new_ETR_group<(tau[[r]]*100), sum_net_profit*tau[[r]]-new_ntl_group, NA_real_),
           new_ETR_group_final = case_when(sum_net_profit>0 & new_ETR_group<(tau[[r]]*100) & (deferred_tax_asset_group >= missing_tax_to15) ~ (tau[[r]]*100), 
                                           sum_net_profit>0 & new_ETR_group<(tau[[r]]*100) & (deferred_tax_asset_group < missing_tax_to15) ~ 100*deferred_tax_asset_group/sum_net_profit,
                                           TRUE ~ new_ETR_group),
           new_ETR_group_final = if_else(new_ETR_group_final<new_ETR_group, new_ETR_group, new_ETR_group_final),
           group_below15 = if_else(new_ETR_group_final<(tau[[r]]*100) & sum_net_profit>0, 1, 0),
           adjusted_covered_tax = case_when(sum_net_profit>0 & new_ETR_group<(tau[[r]]*100) ~ (new_ETR_group_final/100)*sum_net_profit,
                                            TRUE ~ new_ntl_group),
           ETR_group_loss = if_else(sum_net_profit<=0, 1, 0)) %>% ungroup()
  
  ## Second step: top up (group level): 
  df.group <- df.group %>% group_by(group, year) %>% 
      mutate(
        top_up = if_else(group_below15==1, (sum_net_profit* tau[[r]]) - adjusted_covered_tax, 0), 
        top_up = if_else(group_below15==1 & top_up<0, 0, top_up)) %>% ungroup()
  
  ## Select largest top up: 
  df.group <- df.group %>% 
  arrange(desc(top_up)) %>% 
    mutate(flag_cbcr = if_else(row_number() <= desc.cbcr$n_entities[[1]], 1, NA_real_)) %>% 
    ## Make sure we include group in full:
    group_by(group) %>% 
    fill(flag_cbcr, .direction = "downup") %>% 
    mutate(flag_cbcr = if_else(is.na(flag_cbcr), 0, flag_cbcr)) %>% 
    ungroup() %>%
    filter(flag_cbcr == 1) %>% 
    select(-c(group_below15, top_up, ETR_group_loss, adjusted_covered_tax, 
              new_ETR_group_final, missing_tax_to15, sum_net_profit))
  } 

# Sample:
df.cbcr.topup <- FUN_temp(df.iso3)
df.cbcr.topup.prof <- FUN_temp(df.iso3.prof)

# Some descriptive:
desc.cbcr.topup <- df.cbcr.topup %>% FUN_desc() %>% 
  mutate(restriction = "All")
#n_groups = NA_real_)

desc.cbcr.topup.prof <- df.cbcr.topup.prof %>% FUN_desc() %>% 
  mutate(restriction = "Profitable")
# n_groups = NA_real_)



# + Match with Orbis data 
#................................

FUN_temp = function(source_df){
  source_df %>% filter(orbis_match == 1 ) %>% 
    mutate(sample = "Orbis match")
}

# Sample:
df.orbis <- FUN_temp(df.iso3)
df.orbis.prof <- FUN_temp(df.iso3.prof)

write.csv(df.orbis, paste0(iso3, tau[[r]]*100, "_data_orbis", ".csv"))

# Some descriptive:
desc.orbis <- df.orbis %>% FUN_desc() %>%   
  mutate(restriction = "All")

desc.orbis.prof <- df.orbis.prof %>% FUN_desc() %>% 
  mutate(restriction = "Profitable")

# + Match with Orbis data, group = tax_ID 
#................................

FUN_temp = function(source_df){
  source_df %>% filter(orbis_match == 1) %>% 
    mutate(sample = "Orbis match", 
           group = tax_ID) 
}

# Sample:
df.orbis.nogroup <- FUN_temp(df.iso3)
df.orbis.nogroup.prof <- FUN_temp(df.iso3.prof)



#### STEP 2: CREATE SIMULATIONS FUNCTION #### 
#................................................................................

#We define a function so that the simulation can be applied to the different
#samples defined above: 

# Objects that will need to be defined: 

#    - "sample" is one of the samples defined above: {df.cbcr.turn, df.rev, orbis}
#    - "name" is the name of the sample: {"n_largest", "revenue", "orbis"}
#    - "extension" is the extension name for output : {"_n_largest.tex", "_revenue.tex", "_orbis.tex"}
#    - "X" the carveouts share : {0.05, 0.01}
#    - "ext" the carveouts share : {"5", "10"}



#### GMT function ####
#................................

FUN_GMT = function(source_df, name, Q, ext, X, Y){
# 
   # source_df <- df.cbcr.topup
   # name <- "cbcr_topup"
   # Q <- ""
   # ext <- 5
   # Y <- 0.05
   # X <- 0.05


  #### Step 2.1 Define functions ####
  #................................
  
  # Define the top up tax: 
  # what changes is the taxable profit taxed : we call it var_profit
  
  FUN_topup = function(df.in, var_profit){
    df.out <- df.in %>% 
      mutate(
        top_up = if_else(group_below15==1, ({{var_profit}} * tau[[r]]) - adjusted_covered_tax, 0), 
        top_up = if_else(group_below15==1 & top_up<0, 0, top_up), 
        new_liability = if_else(group_below15 == 1, adjusted_covered_tax + top_up, adjusted_covered_tax)
      )
  } ## END FUNCTION
  
  
  FUN_topup_agg = function(df.in){
    df.out <- df.in %>% 
      group_by(group_below15) %>% 
      summarize(
        n = n(),
        N_group_topup = length(group[top_up>0]),
        N_entity_topup = sum(n_entities_count[top_up>0]),
        mean_ETR = mean(ETR_group[top_up>0], na.rm = T),
        sum_new_liability = sum(new_liability, na.rm = T),
        sum_original_tax_above15 = sum(ntl_pos_built, na.rm = T),
        sum_top_up = sum(top_up, na.rm = T),
        sum_profit = sum(net_profit_pos, na.rm = T)
      ) %>% 
      mutate(
        # Express in USD
        sum_top_up_usd = sum_top_up/df.iso3$official_exchange[1],
        sum_profit_usd = sum_profit/df.iso3$official_exchange[1],
        sum_original_tax_built = sum_original_tax_builtvec,
        sum_original_tax = sum_original_taxvec,
        sum_original_profit = sum_original_profitvec,
        #
        pct_change  = 100*(sum_top_up)/sum_original_tax,
        pct_change_built = 100*(sum_top_up)/sum_original_tax_built,
        pct_change_built_above15 = 100*(sum_top_up)/sum_original_tax_above15,
        
      ) %>% mutate(sample = name) %>% ungroup() 
    
  } ## END FUNCTION
  
  FUN_format = function(df.in){
    df.out <- df.in %>% mutate(group_below15 = as.character(group_below15),
                               group_below15 = if_else(group_below15 == "1", "Yes", "No")) %>% 
      relocate(sample) %>% 
      rename(
        `Group ETR<15%` = group_below15,
        `N groups (all)` = n,
        `N groups (taxed)` = N_group_topup,
        `N entity (taxed)` = N_entity_topup,
        `mean(ETR group)` =  mean_ETR,
        `Top up (LCU)` = sum_top_up,
        `Top up (USD)` = sum_top_up_usd,
        `Profits (LCU)` = sum_profit,
        `Profits (USD)` = sum_profit_usd,
        `Original Tax Base (LCU)` = sum_original_tax,
        `Original Tax Base (LCU, built)` = sum_original_tax_built,
        `Original Aggregate Profit (LCU)` = sum_original_profit,
        `Revenue Change built (%)`  = pct_change_built,
        `Revenue Change (%)`  = pct_change,
        `Revenue Change (above 15%)`  = pct_change_built_above15) %>% 
      select(-sum_new_liability, -sum_original_tax_above15)
  } ## END FUNCTION
  
  #### Step 2.2 : Compute the ETRs with Adjusted Covered Taxes ####
  #................................
  
  #--- Sum(taxes)/sum(net profit)
  orig <- source_df 

  # Change group definition depending on the sample:
  if (name == "top1" | name == "top1_prof" | name == "orbis_nogroup" | name == "orbis_nogroup_prof" ){ 
    orig <- orig %>% mutate(group = tax_ID,
                        deferred_tax_asset_group = deferred_tax_asset)
    print("Group ID is not available")
    
  } else {
    print("Group ID is available")
  }
  
  ### TOTAL tax liability and profit prior to GMT:
  # We don't take into account credit or anything else here. Little bias re the built method,
  # as we use the ETR to construct it.
  sum_original_tax_builtvec <- orig$total_taxpayers_liab_built[1]
  sum_original_taxvec <-  orig$total_taxpayers_liab[1]
  sum_original_profitvec <-  orig$total_taxpayers_profit[1]
  
  # Simulation of QDTC: we need to redefine ETR, profit and tax liability: 
  STR <- max(orig$STR) 
  
  if (Q == "_QRTC"){ 
    orig <- orig %>% mutate(net_profit = net_profit+tot_deduc_taxliab,
                            net_tax_liability = net_tax_liability+tot_deduc_taxliab,
                            net_profit_pos = if_else(net_profit<1, 0, net_profit),
                            ntl_pos = if_else(net_tax_liability<1, 0, net_tax_liability)) %>% 
      select(-ETR_group)
    
   # Recompute original ETR:
    orig <- orig %>% mutate(ETR = 100*ntl_pos/net_profit_pos,
                                           ETR = if_else(net_profit_pos == 0 & ntl_pos >0 , STR, ETR),
                                           ETR = if_else(net_profit_pos == 0 & ntl_pos ==0, 0, ETR),
                                           ETR = if_else(is.nan(ETR), 0, ETR),
                            ETR_winz =  if_else(ETR<0, 0, ETR),
                            ETR_winz =  if_else(ETR>max(orig$STR), max(orig$STR), ETR_winz),
                            ETR_keep_neg = if_else(net_profit_pos>0, ETR_winz, 0))
    orig <- orig %>% mutate(ntl_pos_built = if_else(net_profit>0, round(net_profit * (ETR_keep_neg / 100), digits = 0), 0))
    
    # Recompute group ETR:
    df.groupETR <- orig %>%
    group_by(group, year) %>% 
      summarize(sum_net_profit = sum(net_profit, na.rm = T),
                sum_ntl_pos = sum(ntl_pos, na.rm = T)) %>% 
      mutate(ETR_group = 100*sum_ntl_pos/sum_net_profit,
             ## Make sure that those above don't fall into the GMT
             ETR_group = if_else(sum_net_profit <= 0 & sum_ntl_pos >0 , STR, ETR_group),
             ETR_group = if_else(sum_net_profit <= 0 & sum_ntl_pos ==0, 0, ETR_group),
             ETR_group = if_else(ETR_group>STR, STR, ETR_group)) %>% ungroup()
    orig <- left_join(orig, df.groupETR, by = c("group", "year") )
    print("QRTC")
  } else {
    print("Non-QRTC")
  }


  
  # Entity level
  #................................
  
  ### What is the deferred tax asset needed to get to 15%?
  df.adj <- orig %>% mutate(
    ETR_loss = if_else(ETR_keep_neg == 0 & net_profit<=0, 1, 0),
    expected_additional_tax = case_when(ETR_keep_neg>=(tau[[r]]*100) | ETR_loss == 1 ~ NA_real_,
                                        ### HOW TO DEAL WITH LOSSES IN CURRENT YEAR?
                                        ETR_keep_neg<(tau[[r]]*100) & ETR_loss == 0 ~ (tau[[r]]- (ETR_keep_neg/100))*net_profit),
    remaining_tax_asset = case_when(expected_additional_tax >= deferred_tax_asset ~ 0,
                                    expected_additional_tax < deferred_tax_asset ~ deferred_tax_asset - expected_additional_tax),
    adjusted_covered_tax = case_when(ETR_keep_neg>=(tau[[r]]*100) | ETR_loss == 1 ~ ntl_pos_built,
                                     ETR_keep_neg<(tau[[r]]*100) & ETR_loss == 0 & remaining_tax_asset == 0 ~ deferred_tax_asset+ntl_pos_built,
                                     ETR_keep_neg<(tau[[r]]*100) & ETR_loss == 0 & remaining_tax_asset != 0 ~ (deferred_tax_asset-remaining_tax_asset)+ntl_pos_built),
    new_ETR = case_when(ETR_keep_neg>=(tau[[r]]*100) | ETR_loss == 1 ~ ETR_keep_neg,
                        ETR_keep_neg<(tau[[r]]*100) & ETR_loss == 0 ~ round(100*adjusted_covered_tax / net_profit, 1)),
    new_ETR = if_else(new_ETR<ETR_keep_neg, ETR_keep_neg, new_ETR)
    ) %>% 
    # There are some cases where the ETR is 0 because firms are making losses, those are not subject to the GMT that year (?), 
    # So we separate them:
      mutate(ETR_below15 = if_else(new_ETR<(tau[[r]]*100) & ETR_loss == 0, 1, 0),
           ntl_pos_built = if_else(net_profit>0, round(net_profit * (new_ETR / 100), digits = 0), 0),
           ntl_pos = round(ntl_pos, digits = 0))
  

  # Group level
  #................................
  
  #--- Sum(adjusted taxes)/sum(net profit)
  
  df.group <- df.adj  %>% 
    group_by(group, year) %>% 
    summarise(n_entities_count = n(),
              n_entities_below_15 = sum(ETR_below15, na.rm = T),
              n_entities_loss = sum(ETR_loss, na.rm = T),
              sum_ntl_pos = sum(ntl_pos , na.rm = T),
              sum_net_profit_pos = sum(net_profit_pos, na.rm = T),  
              sum_ntl_pos_built = sum(ntl_pos_built, na.rm = T), ##  capped at 0. 
              sum_labor_inp = sum(labor_inp, na.rm = T),
              sum_tangible_assets = sum(tangible_assets, na.rm = T),
              sum_empl = sum(nb_employee, na.rm = T),
              sum_turnover = sum(turnover, na.rm = T),
              sum_taxcredit = sum(tot_deduc_taxliab, na.rm = T),
              deferred_tax_asset_year_1_t = mean(deferred_tax_asset_group, na.rm = T)) %>% ungroup()

   # First adjustment:             
    df.def <- deferred_tax_groupFUN(df.adj) %>% select(group, ETR_group, new_ETR_group, new_ntl_group, sum_net_profit)
    df.group <- left_join(df.group, df.def, by = c("group"))
    
   # Second adjustment: Apply last year deferred tax 
    df.group <- df.group %>% group_by(group, year) %>% 
      mutate(missing_tax_to15 = if_else(sum_net_profit>0 & new_ETR_group<(tau[[r]]*100), sum_net_profit*tau[[r]]-new_ntl_group, NA_real_),
             new_ETR_group_final = case_when(sum_net_profit>0 & new_ETR_group<(tau[[r]]*100) & (deferred_tax_asset_year_1_t >= missing_tax_to15) ~ (tau[[r]]*100), 
                                             sum_net_profit>0 & new_ETR_group<(tau[[r]]*100) & (deferred_tax_asset_year_1_t < missing_tax_to15) ~ 100*deferred_tax_asset_year_1_t/sum_net_profit,
                                             TRUE ~ new_ETR_group),
             new_ETR_group_final = if_else(new_ETR_group_final<new_ETR_group, new_ETR_group, new_ETR_group_final),
             group_below15 = if_else(new_ETR_group_final<(tau[[r]]*100) & sum_net_profit>0, 1, 0),
             adjusted_covered_tax = case_when(sum_net_profit>0 & new_ETR_group<(tau[[r]]*100) ~ (new_ETR_group_final/100)*sum_net_profit,
                                              TRUE ~ new_ntl_group),
             ETR_group_loss = if_else(sum_net_profit<=0, 1, 0)) %>% ungroup()
    

# Output: 
#................................
    
  desc_group_sum <- df.group %>%
    summarize(n_group = n(),
              n_group_below15 = sum(group_below15, na.rm = T),
              n_group_loss = sum(ETR_group_loss, na.rm = T),
              n_entity = sum(n_entities_count, na.rm = T),
              n_entity_below15 = sum(n_entities_below_15, na.rm = T),
              n_entities_loss = sum(n_entities_loss, na.rm = T),
              avg_sub_count = mean(n_entities_count, na.rm = T),
              avg_ETR_group = mean(new_ETR_group_final, na.rm = T),
              avg_ETR_below = mean(new_ETR_group_final[group_below15==1], na.rm = T)) %>% 
    mutate(country = iso3, year = max(source_df$year))
  
  # Second stats: 
  df <- left_join(df.adj, df.group, by = c("year", "group")) %>% 
    mutate(ETR_below15 = if_else(new_ETR <(tau[[r]]*100) & ETR_loss == 0, 1, 0),
           scope_entity_group = if_else(ETR_below15 == 1 & group_below15==1, 1, 0),
           scope_noentity_group = if_else(ETR_below15 == 0 & group_below15==1, 1, 0),
           scope_entity_nogroup = if_else(ETR_below15 == 1 & group_below15==0, 1, 0),
           scope_noentity_nogroup = if_else(ETR_below15 == 0 & group_below15==0, 1, 0)) %>% 
    summarize(scope_entity_group = sum(scope_entity_group, na.rm = T),
              scope_noentity_group =  sum(scope_noentity_group, na.rm = T),
              scope_entity_nogroup =  sum(scope_entity_nogroup, na.rm = T),
              scope_noentity_nogroup =  sum(scope_noentity_nogroup, na.rm = T)) 
  
  
  df.tex <- cbind(desc_group_sum, df) %>% mutate(sample = name) %>% 
    select(n_group, n_group_below15, n_group_loss, n_entity_below15, n_entity, n_entities_loss, avg_sub_count, avg_ETR_below, scope_entity_group, scope_noentity_group, scope_entity_nogroup,
           scope_noentity_nogroup) %>% 
    relocate(n_group, n_group_below15, n_group_loss, n_entity, n_entity_below15, n_entities_loss, avg_sub_count, avg_ETR_below, scope_entity_group, scope_noentity_group, scope_entity_nogroup,
             scope_noentity_nogroup) %>% 
  rename(
    `N entities` = n_entity,
    `N groups` = n_group,
    `N entities<15%` = n_entity_below15,
    `N groups<15%` = n_group_below15,
    `N loss` = n_entities_loss,
    `N groups loss` = n_group_loss,
    `mean(subsidiaries)` = avg_sub_count,
    `mean(ETR if <15)` = avg_ETR_below,
    `Scope: Group+Entity` = scope_entity_group,
    `Scope: Group` = scope_noentity_group,
    `Scope: Entity` = scope_entity_nogroup,
    `Scope: none` = scope_noentity_nogroup)
  

  write.csv(df.tex, paste0(iso3, tau[[r]]*100, "_step2_group_entity_ETR15", name, Q, ".csv"))  
  
  
  #### Data ready: deferred tax asset, no de minimis, no carve-outs ####
  #................................
  
  df.2 <-  df.group %>% rename(net_profit = sum_net_profit,
                             net_profit_pos=sum_net_profit_pos,
                             ntl_pos = sum_ntl_pos,
                             ntl_pos_built = sum_ntl_pos_built,
                             turnover = sum_turnover, 
                             labor_inp = sum_labor_inp,
                             tangible_assets = sum_tangible_assets,
                             sum_empl = sum_empl) %>% 
    mutate(turnover_usd = turnover/df.iso3$official_exchange[1],
           net_profit_usd = net_profit/df.iso3$official_exchange[1],
           net_profit_pos_usd = net_profit_pos/df.iso3$official_exchange[1],
           taxable_profit = if_else(net_profit<0, 0, net_profit))
  
  
  #### Top up: deferred tax asset, no de minimis, no carve-outs ####
  #................................
  df.2 <- FUN_topup(df.2, taxable_profit) 
  topup <- FUN_topup_agg(df.2) 
  topup <- topup %>% FUN_format()
  write.csv(topup, paste0(iso3, tau[[r]]*100, "_step2_topup_", name, Q, ".csv"))  

  write.csv(df.2, paste0(iso3, tau[[r]]*100, "_List_No_Deminimis_", name, Q, ".csv"))  
  
  #### Step 2.3: DE MINIMIS ####
  #................................
  
  
  # De minimis exclusion at the group level:
  # Easiest it to use USD value (controlled for inflation) and then apply that threshold. Should not have to be country specific
  # Convert income and turnover to USD, inflation adj
  # Convert de minimis threshold from EURO to USD; Average exchange rate in 2018: 1euro = 1.1811 USD. It has to be adjusted for 2018 inflation.
  
  # -- Turnover is lower than 10M
  # --- net profit < 1 M
  
  df.2 <- df.2 %>% mutate(turnover_below_10m = if_else((turnover_usd) < 10000000*1.1811*index, 1, 0),
                      income_below_1m = if_else((net_profit_usd) < 1000000*1.1811*index, 1, 0),
                      de_minimis = if_else(turnover_below_10m == 1 & income_below_1m == 1, 1, 0)) 
  
  # Output: 
  #................................
  
  desc.deminimis <- df.2 %>% group_by(group_below15) %>% 
    summarise(n = n(),
              turnover_below_10m = sum(turnover_below_10m, na.rm = T),
              income_below_1m = sum(income_below_1m, na.rm = T), 
              de_minimis = sum(de_minimis, na.rm = T)) %>% mutate(sample = name)
  
  write.csv(desc.deminimis, paste0(iso3, tau[[r]]*100, "_step3_deminimis_", name, Q, ".csv"))
 
  # Excess profits to be computed with the carve outs
  # -- 5% of payroll costs and 5% of the carrying value of tangible assets.
  # --- How to deal with negative profit?? So far put the carve outs at 0
  
  
  #### Data ready: deferred tax asset, de minimis, no carve-outs ####
  #................................
  
  df.3 <- df.2 %>% filter(de_minimis != 1)
  
  #### Top up: deferred tax asset, de minimis, no carve-outs ####
  #................................
  # Top up:
  
  df.3 <- FUN_topup(df.3, taxable_profit) 
  topup_deminimis <-  FUN_topup_agg(df.3) %>% FUN_format()
  
  write.csv(topup_deminimis, paste0(iso3, tau[[r]]*100, "_step3_topup_nocarv_", name, Q, ".csv"))  

  write.csv(df.3, paste0(iso3, tau[[r]]*100, "_List_With_Deminimis_", name, Q, ".csv"))
  
  #### Step 2.4: CARVEOUTS  ####
  #................................
  
  temp <- df.3 %>% filter(group_below15 == 1) %>% select(sum_empl)
  
  #### Data ready: deferred tax asset, de minimis, carve-outs ####
  #................................
  
  df.4 <- df.3 %>% 
    mutate(carveout_payroll = if_else(!is.na(labor_inp), X*abs(labor_inp), 0),
           carveout_tangible = if_else(!is.na(tangible_assets), Y*abs(tangible_assets), 0),
           carveout_total = carveout_payroll + carveout_tangible, 
           
           #Expressed as share of net profit:
           payroll_share = if_else(net_profit>0, carveout_payroll/net_profit, 0),
           payroll_share = if_else(net_profit>0 & (carveout_payroll>net_profit), 1, payroll_share),  # bind at 1 if larger than net profit
           
           tangible_share = if_else(net_profit>0, carveout_tangible/net_profit, 0),
           tangible_share = if_else(net_profit>0 & (carveout_tangible>net_profit), 1, tangible_share),  # bind at 1 if larger than net profit
           
           carveout_share = if_else(net_profit>0, carveout_total/net_profit, 0),
           carveout_share = if_else(net_profit>0 & (carveout_total>net_profit), 1, carveout_share),  # bind at 1 if larger than net profit
           
           #Excess profit:
           excess_profit = if_else(net_profit>0, net_profit - carveout_total, 0),
           excess_profit = if_else(net_profit>0 & (carveout_total>net_profit), 0, excess_profit),
           taxable_profit = if_else(net_profit>0, net_profit, 0))
  
  

  df.agg.carveout <- df.4 %>% group_by(group_below15) %>% 
    summarize(
      n = n(),
      mean_payroll_share = mean(payroll_share, na.rm = T),
      mean_tangible_share = mean(tangible_share, na.rm = T),
      mean_carveout_share = mean(carveout_share, na.rm = T),
      min_carveout_share = min(carveout_share, na.rm = TRUE),
      max_carveout_share = max(carveout_share, na.rm = TRUE),
    ) %>% mutate(sample = name)

  write.csv(df.4, paste0(iso3, tau[[r]]*100, "_List_Carveouts_", name, Q, ext, ".csv"))
  
  #### Step 2.5: TOP UP: deferred tax asset, de minimis, carve-outs ####
  #................................

  #### Data ready: deferred tax asset, de minimis, carve-outs ####
  #................................
  
  # Top up:
  df.5 <- FUN_topup(df.4, excess_profit)
  
  write.csv(df.5, paste0(iso3, tau[[r]]*100, "_List_Carveouts_Final_", name, Q, ext, ".csv"))
  
  topup_carveouts <- FUN_topup_agg(df.5) %>% FUN_format() %>%
    mutate(Carveouts = paste0(ext, "%")) %>% 
    relocate(sample, Carveouts)
  
  write.csv(topup_carveouts, paste0(iso3, tau[[r]]*100, "_step5_topup_carv_", name, Q, ext, ".csv"))  
  
  
} ## FUNCTION END HERE 


#### STEP 3: APPLY FUNCTION ACROSS SAMPLES #### 

FUN_GMT(df.cbcr.topup, "cbcr_topup", "", "5", 0.05, 0.05)
FUN_GMT(df.cbcr.topup, "cbcr_topup", "", "10", 0.1, 0.08)
FUN_GMT(df.cbcr.topup, "cbcr_topup", "_QRTC", "5", 0.05, 0.05)
FUN_GMT(df.cbcr.topup, "cbcr_topup", "_QRTC", "10", 0.1, 0.08)


FUN_GMT(df.orbis, "orbis", "", "5", 0.05, 0.05)
FUN_GMT(df.orbis, "orbis", "", "10", 0.1, 0.08)
FUN_GMT(df.orbis, "orbis", "_QRTC", "5", 0.05, 0.05)
FUN_GMT(df.orbis, "orbis", "_QRTC", "10", 0.1, 0.08)


FUN_GMT(df.orbis.nogroup, "orbis_nogroup", "", "5", 0.05, 0.05)
FUN_GMT(df.orbis.nogroup, "orbis_nogroup", "", "10", 0.1, 0.08)


## TOGETHER
list.name <- c("cbcr_topup", "cbcr_topup_QRTC",
               'orbis', 'orbis_QRTC',
               "orbis_nogroup")

steps <- c( "_step5_topup_carv_")


# Gather in one table carveouts 5% and carveouts 10%
for (i in 1: length(list.name)){
  print(list.name[i])
  for (y in 1: length(steps)){
    print(steps[y])
    carv5 <- read.csv(paste0(iso3, tau[[r]]*100, steps[y], list.name[i], "5.csv"), check.names=FALSE)
    carv10 <- read.csv(paste0(iso3, tau[[r]]*100, steps[y], list.name[i], "10.csv"), check.names=FALSE)
    df <- rbind(carv5, carv10) %>% select(-1)
    if (nrow(df) == 0) {
      # Create a row with NA values for each column
      new_row <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(df)))
      colnames(new_row) <- colnames(df)  # Ensure column names match
      # Add the new row to the dataframe
      df <- rbind(df, new_row)
    }
    write.csv(df, paste0(iso3, tau[[r]]*100, steps[y], list.name[i], ".csv"))
    
    # Delete the unnecessary files
    file.remove(paste0(iso3, tau[[r]]*100, steps[y], list.name[i], "5.csv"), 
                paste0(iso3, tau[[r]]*100, steps[y], list.name[i], "10.csv"))
    }
}


} ## END loop tax rate 

print(paste0("End GTM Analysis for", country_GMT[[c]]))

}

print("End GTM Analysis")
