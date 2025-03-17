
# Effective tax rate functions


# 1. Construct the Effective Tax Rates, choose the denominator: ####
ETR_FUN= function(source_df, denominator){
  max <-  max(source_df$STR)
  df <- source_df %>% 
    # Make sure that profit and taxes are at least < 1 
    # If profit variable is NA, then ETR will also be NA
    mutate(x = if_else(source_df[[denominator]] < 1, 0, source_df[[denominator]]),
           ntl = if_else(pos_taxliab < 1 | is.na(pos_taxliab), 0, pos_taxliab),
           ETR = (ntl / x) * 100,
           # Several cases:        
           ETR = case_when(x == 0 & pos_taxliab > 0 ~ max,
                           x == 0 & pos_taxliab ==0 ~ 0,
                           is.nan(ETR) ~ 0,
                           TRUE ~ ETR),
           ETR_denominator = denominator)
  
  # Winzorization:
 
    df <- df  %>% mutate(ETR_winz = Winsorize(ETR, val=c(0, max)))
  
  # Profit or loss making firms:
  df <- df  %>% mutate(ETR_drop_neg = if_else(net_profit <= 0, NA_real_, ETR_winz), 
                       ETR_keep_neg = if_else(net_profit <= 0 , 0, ETR_winz))
}



# 2. Construct the Effective Tax Rates for each firm, choose the numerator:: ####
ETR_num_FUN= function(source_df, numerator){
  max <-  max(source_df$STR)
  
  df <- source_df %>% 
    # Make sure that profit and taxes are at least < 1 
    # If profit variable is NA, then ETR will also be NA
    mutate(net_profit = if_else(net_profit < 1, 0, net_profit),
           x = if_else(.data[[numerator]] < 1 | is.na(.data[[numerator]]), 0, .data[[numerator]]),
           ETR = (x / net_profit) * 100,
           # Several cases:        
           ETR = case_when(net_profit == 0 & x > 0 ~ max,
                           net_profit == 0 & x ==0 ~ 0,
                           is.nan(ETR) ~ 0,
                           TRUE ~ ETR),
           ETR_denominator = numerator)
  
  # Winsorization:
     df <- df  %>% mutate(ETR_winz = Winsorize(ETR, val=c(0, max)))
   # Profit or loss making firms:
  df <- df  %>% mutate(ETR_drop_neg = if_else(net_profit <= 0, NA_real_, ETR_winz), 
                       ETR_keep_neg = if_else(net_profit <= 0 , 0, ETR_winz))
     return(df)
}



# 3. ETR using multiple years for panel data: ####

ETR_balpanel_FUN = function(source_df, nb_year){
  
  max <- max(source_df$STR)
  first_year <- max(source_df$year) - nb_year + 1 
  
  # Keep only panel appearing in last cross-section obs
  df.id <- source_df %>% filter(year == max(year)) %>% pull(tax_ID) 
  
  df <- source_df %>% mutate(sample = if_else(tax_ID %in% df.id, 1, 0)) %>%
    filter(sample == 1)
  
  # Keep if firms appear (nb_year), need to be balanced panel, and no change in STR
  df <- df %>% filter(year >= first_year)   %>% 
    arrange(tax_ID, year) %>% 
    group_by(tax_ID) %>% 
    mutate(str_change = STR - dplyr::lag(STR),
           sum_year = n()) %>% 
    ungroup() %>% 
    filter(sum_year == nb_year)
  
  
  # Construct ETRs lifetime:
  
  # Panel (assumption is that LCF will impact ETR, so we don't need to account for previous losses):
  df.life <- df %>% 
    group_by(tax_ID, country) %>% 
    summarize(sum_pos_taxliab = sum(pos_taxliab, na.rm = T),
              sum_net_profit = sum(net_profit, na.rm = T),
              ETR_all_panel = 100*sum_pos_taxliab/sum_net_profit,
              ETR_all_panel = if_else(is.nan(ETR_all_panel), NA_real_, ETR_all_panel),
              count_year = n())  %>% ungroup() 
  
  # No separation between profitable and unprofitable firms here
    df.life <- df.life  %>% mutate(ETR_all_panel = Winsorize(ETR_all_panel, val=c(0, max)))
 
  df.p <- df %>% filter(year == max(year)) %>% select(tax_ID, percentile_99.9)
  df <- left_join(df.life, df.p, by = c("tax_ID"))
  # Percentile level
  df <- df %>% group_by(country, percentile_99.9) %>% 
    mutate(ETR_prof_panel = if_else(sum_net_profit > 0, ETR_all_panel, NA_real_)) %>%
    summarize(n_all = length(ETR_all_panel),
              n_prof = length(ETR_all_panel[!is.na(ETR_all_panel)]),
              ETR_all_panel = mean(ETR_all_panel, na.rm = T),  
              ETR_prof_panel = mean(ETR_prof_panel, na.rm = T)) %>%
    mutate(year = nb_year)
  return(df)
}


