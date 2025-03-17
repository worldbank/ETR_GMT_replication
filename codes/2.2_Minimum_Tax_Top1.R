
#########################################################.
# REQUIREMENTS:

# variable "group" needs to exist
# orbis_match == 1 also need to exist
# STR, in the form of decimal and not percentage (so far)
# foreign_ownership
# Need to add the n everywhere for South Africa.
# nb_employee 

# CALL ETR FUNCTION:
source(paste0(aux_source, "/function_ETR.R"))

# Select only data that are not part of the detailed GMT list: 
data.GMT <- list()
name.GMT <- c()

for (i in 1: length(data.panel)){
  df <- data.panel[[i]]
  if (df$country[[1]] %in% country_GMT){
    print(paste0(country_code[[i]], " is used for Top 1% GMT analysis"))
    data.GMT <- append(data.GMT, list(df))
    name.GMT <- append(name.GMT, country_code[[i]])
  }}


#### Graph simulation top 1 pct : ####
if (length(data.GMT) > 0) {
for (i in 1: length(data.GMT)){
  
  #########################################################.
  
  #### 1. COUNTRY SPECIFIC DATA: Create the simulation : ####
  
  # + Income and profits are net of dividend incomes: 
  #................................
  
  df <- data.GMT[[i]] %>% mutate(net_profit = net_profit_no_div,
                             group = tax_ID)
 
  # + Adjustment based on panel dimension of the data:
  #................................
  
  ## Adjusted covered taxes take into account five preceding years, we will restrict it to a 2-year window:
  
  # let's take 2018
  year_t <- max(df$year)
  year_1_t <- year_t - 1 
  
  
  # Compute deferred tax asset at the entity level:
  df <- df %>% filter(year == year_1_t | year == year_t) %>% 
    mutate(loss_tax_base = if_else(net_profit < 0, abs(net_profit), 0),
           tax_asset = loss_tax_base * 0.15) %>%
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
      mutate(missing_tax_to15 = if_else(sum_net_profit>0 & ETR_group<15, sum_net_profit*0.15-sum_ntl_pos, NA_real_),
             deferred_tax_asset_group = case_when(sum_net_profit<=0 ~ sum_neg_profit*0.15,
                                                  sum_net_profit>0 & ETR_group>=15 ~ sum_neg_profit*0.15,
                                                  sum_net_profit>0 & ETR_group<15 ~ sum_neg_profit*0.15 - missing_tax_to15),
             deferred_tax_asset_group = case_when(sum_net_profit>0 & ETR_group<15 & (sum_neg_profit*0.15 < missing_tax_to15) ~ 0,
                                                  TRUE ~ deferred_tax_asset_group),
             new_ETR_group = case_when(sum_net_profit>0 & ETR_group<15 & (sum_neg_profit*0.15 >= missing_tax_to15) ~ 15, 
                                       sum_net_profit>0 & ETR_group<15 & (sum_neg_profit*0.15 < missing_tax_to15) ~ 100*(sum_neg_profit*0.15)/sum_net_profit,
                                       TRUE ~ ETR_group),
             new_ntl_group = case_when(sum_net_profit>0 ~ (new_ETR_group / 100)*sum_net_profit,
                                       sum_net_profit<=0 ~ sum_ntl_pos))
  }
  
  ## Apply to year t-1 so that we can offset deferred tax asset in year t: 
  df.group <- deferred_tax_groupFUN(df) %>% 
    arrange(group, year) %>%
    mutate(
      deferred_tax_asset_group = lag(deferred_tax_asset_group),
      deferred_tax_asset_group = if_else(is.na(deferred_tax_asset_group), 0, deferred_tax_asset_group)) %>% ungroup() %>% 
    select(deferred_tax_asset_group, year, group, ETR_group)
  
  
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
           tax_to_remit_usd = tax_to_remit/official_exchange
           
    ) %>% 
    # total taxpayer liability
    mutate(total_taxpayers_liab = sum(ntl_pos, na.rm = T),
           total_taxpayers_liab_built = sum(ntl_pos_built, na.rm = T),
           type = "All")
  
  # Sample is reduced to profitable firms only
  df.iso3.prof <- df.iso3 %>% 
    filter(!is.na(ETR_drop_neg)) %>% 
    # total taxpayer liability
    mutate(total_taxpayers_liab = sum(ntl_pos, na.rm = T),
           total_taxpayers_liab_built = sum(ntl_pos_built, na.rm = T),
           type = "Profitable")
  
  
  # + General aggregates
  #................................
  
  
  # Define descriptive stat function
  FUN_desc = function(source_df){
    source_df %>% group_by(sample) %>%  
      summarize(
        n_entities = n(),
        sum_revenue = sum(turnover_usd, na.rm = T),
        #sum_employee = sum(nb_employee, na.rm = T),
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
        #sum_employee = sum(nb_employee, na.rm = T),
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
  # We have several ways of identifying firms in scope, let's proceed by samples
  
  # Profitable
  df.p <- FUN_agg(df.iso3.prof)
  # All firms
  df.a <- FUN_agg(df.iso3)
  df <- rbind(df.p, df.a)
  
  setwd(metadata)
  write.csv(df, paste0(name.GMT[[i]], "_aggregate_statistics.csv"))
  
  
  # + 0. Earlier method: take the top 1 percent:
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
  
  #... Initial Simulation:
  
  FUN_top1 = function(source_df, restr){ 
    
    # Original tax liability
    tax_liab <- source_df %>% group_by(percentile_99.9) %>% 
      summarize(
        old_taxrev = sum(ntl_pos, na.rm = T),
        old_taxrev_built = sum(ntl_pos_built, na.rm = T)) %>% ungroup()
    
    temp <- source_df %>% 
      mutate(
        # Assumption is that negative net profit ensure no taxes paid
        ntl_pos_built = if_else(net_profit>0, round(net_profit * (ETR_keep_neg / 100), digits = 0), 0),
        ntl_pos = round(ntl_pos, digits = 0),
        # Compute ETR with deferred asset
        missing_tax_to15 = if_else(net_profit>0 & ETR_keep_neg<(0.15*100), net_profit*0.15-ntl_pos, NA_real_),
        ETR_entity = case_when(net_profit>0 & ETR_keep_neg<(0.15*100) & (deferred_tax_asset >= missing_tax_to15) ~ (0.15*100), 
                               net_profit>0 & ETR_keep_neg<(0.15*100) & (deferred_tax_asset < missing_tax_to15) ~ 100*(deferred_tax_asset)/net_profit,
                               TRUE ~ ETR_keep_neg),
        ETR_entity = if_else(ETR_entity<ETR_keep_neg, ETR_keep_neg, ETR_entity),
        new_ntl= case_when(net_profit>0 ~ (ETR_entity/100)*net_profit,
                           net_profit<=0 ~ ntl_pos),
        #flag firms with etr<15
        less_15 = if_else(ETR_entity < (0.15*100), 1, 0),
        tag_built = if_else(ntl_pos != ntl_pos_built, 1, 0),
        # Compute new tax liability:
        top_up = if_else(less_15==1, (net_profit_pos * 0.15) - new_ntl, 0),
        top_up = if_else(less_15==1 & top_up<0, 0, top_up))
        
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
      summarize(share_less15_sample = round(100 * sum(n_less15) / sum(n), digits = 2)) %>% pull() 
    share_below15_all <- df %>%  filter(percentile_99.9 > 98) %>% 
      summarize(share_less15_all = round(100 * sum(n_less15) / sum(n), digits = 2)) %>% pull() 
    
    n_below15 <- df %>%  filter(percentile_99.9 > 98) %>% 
      summarize(sum(n_less15)) %>% pull() 
    
    # 2. Conditional on being below 15%, average ETR
    ETR_below15 <- temp %>%  filter(percentile_99.9 > 98 & less_15 == 1) %>% 
      summarize(mean_ETR = mean(ETR_entity, na.rm =T)) %>% pull() 
    
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
        change_built = round( 100 *top_up / old_taxrev_built, digits = 2),
        change = round( 100 *top_up / old_taxrev, digits = 2),
        n_below15 = n_below15,
        turnover_less15 = turnover_less15,
        profit_less15 =profit_less15,
        share_less15 = share_below15,
        share_less15_all = share_below15_all,
        turnover_share15_all = round( turnover_share15_all, digits = 2), 
        profit_share15_all = round( profit_share15_all, digits = 2), 
        
        turnover_share15_1pct = round( turnover_share15_1pct, digits = 2),
        ETR_below15 = round(ETR_below15, digits = 2)) 
  }
  
  # Profitable
  df.p <- FUN_top1(df.iso3.prof, "Profitable")
  # All firms
  df.a <- FUN_top1(df.iso3, "All")
  df <- rbind(df.p, df.a) 
  
  setwd(metadata)
  write.csv(df, paste0(name.GMT[[i]], "_1pct_revenue_simul.csv"))
  







FUN_top1_nodat = function(source_df, restr){ 
  
  # Original tax liability
  tax_liab <- source_df %>% group_by(percentile_99.9) %>% 
    summarize(
      old_taxrev = sum(ntl_pos, na.rm = T),
      old_taxrev_built = sum(ntl_pos_built, na.rm = T)) %>% ungroup()
  
  temp <- source_df %>% 
    mutate(
      # Assumption is that negative net profit ensure no taxes paid
      ntl_pos_built = if_else(net_profit>0, round(net_profit * (ETR_keep_neg / 100), digits = 0), 0),
      ntl_pos = round(ntl_pos, digits = 0),
      
      #flag firms with etr<15
      less_15 = if_else(ETR_keep_neg < (0.15*100), 1, 0),
      tag_built = if_else(ntl_pos != ntl_pos_built, 1, 0),
      # Compute new tax liability:
      top_up = if_else(less_15==1, (net_profit_pos * 0.15) - ntl_pos_built, 0),
      top_up = if_else(less_15==1 & top_up<0, 0, top_up),
      top_up = if_else(less_15==0, 0, top_up)
      
    )  
  
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
    summarize(share_less15_sample = round(100 * sum(n_less15) / sum(n), digits = 2)) %>% pull() 
  share_below15_all <- df %>%  filter(percentile_99.9 > 98) %>% 
    summarize(share_less15_all = round(100 * sum(n_less15) / sum(n), digits = 2)) %>% pull() 
  
  n_below15 <- df %>%  filter(percentile_99.9 > 98) %>% 
    summarize(sum(n_less15)) %>% pull() 
  
  # 2. Conditional on being below 15%, average ETR
  ETR_below15 <- temp %>%  filter(percentile_99.9 > 98 & less_15 == 1) %>% 
    summarize(mean_ETR = mean(ETR_keep_neg, na.rm =T)) %>% pull() 
  
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
              old_taxrev = sum(old_taxrev, na.rm = T),
              old_taxrev_built = sum(old_taxrev_built, na.rm = T),
              turnover_usd = sum(turnover_usd, na.rm = T),
              net_profit_pos = sum(net_profit_pos, na.rm = T))
  
  turnover_share15_all <- 100*turnover_less15/df_old$turnover_usd[1]
  turnover_share15_1pct <- 100*turnover_less15/turnover_1pct
  profit_share15_all <- 100*profit_less15/df_old$net_profit_pos[1]
  
 
  # 4. Tax revenue increase 
  
  # For those in the top 1%, apply the new tax revenue if ETR is minimum 15%, 
  # And compute the revenue change
  top_up <- df %>% filter(percentile_99.9 > 98) %>% 
    summarise(top_up = sum(top_up, na.rm = T)) %>% pull()
  
  # Merge both
  df_taxbase_1pct <- df_old %>% 
    mutate(
      sample = "Top 1 percent",
      restriction = restr,
      top_up  = top_up,
      change_built = round( 100 *top_up / old_taxrev_built, digits = 2),
      change = round( 100 *top_up / old_taxrev, digits = 2),
      n_below15 = n_below15,
      turnover_less15 = turnover_less15,
      profit_less15 = profit_less15,
      share_less15 = share_below15,
      share_less15_all = share_below15_all,
      profit_share15_all = round( profit_share15_all, digits = 2), 
      turnover_share15_all = round( turnover_share15_all, digits = 2), 
      turnover_share15_1pct = round( turnover_share15_1pct, digits = 2),
      ETR_below15 = round(ETR_below15, digits = 2)) 
}

# Profitable
df.p <- FUN_top1_nodat(df.iso3.prof, "Profitable")
# All firms
df.a <- FUN_top1_nodat(df.iso3, "All")
df <- rbind(df.p, df.a) 

setwd(metadata)
write.csv(df, paste0(name.GMT[[i]], "_1pct_revenue_simul_nodat.csv"))

}
}
print("2.2_Minimum_Tax_Top1.R done")

