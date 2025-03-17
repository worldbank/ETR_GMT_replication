

# + ggplot theme: ####
theme_niwot <- function(){
  theme_bw() +
    theme(
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 18, face = "plain"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
      plot.title = element_text(size = 18, vjust = 1, hjust = 0),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      legend.position = "top",
      legend.key = element_blank())
}

# + Percentile function: ####
gg_guidesFUN = function(df){
 df +
    guides(shape = guide_legend(""), color = guide_legend(""),
           linetype = guide_legend(""), label = guide_legend("")) +
    theme(axis.text = element_text(size = 9),  
          legend.text = element_text(size=12), 
          panel.grid.major.y = element_blank(),
          axis.title =  element_text(size = 11, margin = margin(r = 10)),
          plot.subtitle = element_text(hjust = 0.5, size = 12 ),
          strip.background = element_blank()) 
}


# + Parameters: ####
line_size <- 0.9
dot_size <- 0.9


# + Rename function: ####
renameFUN <- function(df){
  df <- df %>% mutate(ETR_denominator= recode(ETR_denominator, net_profit="Net Profit"),
                      Measure = recode(Measure, ETR_drop_avg = "Profitable firms", ETR_keep_avg="All firms", 
                                       ETR_drop_med = "Profitable firms", ETR_keep_med="All firms" ),
                      Measure = factor(Measure, levels = c("Profitable firms", "All firms")))
}

# + Percentile function: ####
percentileFUN = function(df){
  df1 <- df  %>% 
    mutate(percentile_99.9 = round(percentile_99.9, digits = 1),
           p_chr = as.character(percentile_99.9),
           p = percentile_99.9,
           p = if_else(p_chr=="99", 101, p),
           p = if_else(p_chr=="99.1", 103, p),
           p = if_else(p_chr=="99.2", 105, p),
           p = if_else(p_chr=="99.3", 107, p),
           p = if_else(p_chr=="99.4", 109, p),
           p = if_else(p_chr=="99.5", 111, p),
           p = if_else(p_chr=="99.6", 113, p),
           p = if_else(p_chr=="99.7", 115, p),
           p = if_else(p_chr=="99.8", 117, p), 
           p = if_else(p_chr=="99.9", 119, p)) 
  
}




