
##### 3_ output

rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) != "character"])

print("Begin Output")

#######################| 1. LOAD AND MERGE ALL METADATA |################################ 


setwd(metadata)

list.name <- c("df.ETR.descr", "df.ETR.gmt", "df.perc.distribution", "df.p90.stat", "df.stat.decile.usd", "df.perc.turnover", "df.ETR.corr.asset", 
               "df.percentile.overtime", "df.ETR.avg.table", "df.ETR.size.p", "df.ETR.size.p.div", "df.ETR.panel.cross", "df.ETR.size.p.rob", "df.ETR.size.num.p", 
                "df.ETR.panel.bal", "df.ETR.nbemployee", "df.ETR.payroll", "df.ETR.assets", "df.ETR.largesector.p", "df.ETR.largesector.dec", 
                 "df.taxgap.cap", "df.ETR.share15.p", "df.ETR.fit.B90",  
               "df.ETR.fit.T10", "df.ETR.fit.T10.D5", "df.ETR.fit.T10.D3", "df.ETR.fit.T10.D2", "df.ETR.fit.T10.D01", "df.ETR.fit.T10.C", 
               "df.ETR.fit.T20", "df.ETR.fit.T20.D5", "df.ETR.fit.T20.D3", "df.ETR.fit.T20.D2", "df.ETR.fit.T20.D01", "df.ETR.fit.T20.C"
)  

## Call source merge file 
for (i in 1: length(list.name)){
  print(list.name[i])
  df <- readRDS(paste0(pre, list.name[[i]], ".RDS", sep = ""))
  assign(paste(list.name[i], sep = ""), df.)   
}


# WDI data:
setwd(paste0(gitHub, project, "/input/WDI/"))
WDI <- list()
for (i in 1: length(country_name)){
  WDI[[i]] <- read.csv(paste0("WDI_vars_", country_code[[i]], ".csv", sep = ""))
  print(country_name[[i]])
}
WDI_all <- do.call("rbind", WDI)
  
  
#######################| 2. Settings for graphs aesthetics |################################ 

# Need to order countries by their STR rates: 
df_STR_name <- df.ETR.descr %>% group_by(country, country_name) %>%
  summarize(max_STR = max(STR)) 

# Graph themes: 
# Include theme, percentile function to add visual space for the top 1%, and rename groups of firms
setwd(aux_source)
source("ggtheme.R")
theme_set(theme_niwot())


#######################| 3. DESCRIPTIVE TABLES  |################################ 

# Export tax gap metadata in csv format, used with GTED data
setwd(metadata)
write.csv(df.taxgap.cap, file = "metadata_taxgap.csv")

# Location table output
setwd(paste0(output, "/tables"))

# + Table 1: Descriptive Statistics on Countries and Data ####

df <- df.ETR.avg.table %>% filter(`treatment neg`=="Keep" | `treatment neg`=="Drop") %>% 
  select(country, net_profit, `treatment neg`) %>%
  pivot_wider(names_from = "treatment neg", values_from = net_profit) %>% rename(ETR_avg_all = Keep, ETR_avg_profitable = Drop) %>% 
  mutate_if(is.factor, as.character)

table_sumstat <- df.ETR.descr %>% 
  select(country_name, country, year, GDP_pc_USD, count, 
         turnover_avg, `Net profit>0`, STR, min_year)  %>% 
  left_join(df, by = c("country"))

table_sumstat <- table_sumstat %>% 
  arrange(country_name) %>% 
  mutate(turnover_avg = turnover_avg/1000) %>%
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate(STR = as.character(STR),
         country_name = paste0(country_name, " (", country, ")"),
         year = paste0(min_year, "-", year),
         `ETR average All (Profitable)` = paste0(ETR_avg_all , "/", ETR_avg_profitable)) %>%
  select(-country, -min_year, -ETR_avg_profitable, -ETR_avg_all)

stargazer(table_sumstat, rownames = FALSE, summary = FALSE,  out = "T1_desc_stats.tex", digits = 1)
latex_code <- readLines("T1_desc_stats.tex")
latex_code <- latex_code[!grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\caption\\{\\}|\\\\label\\{\\}", latex_code)]
writeLines(latex_code, "T1_desc_stats.tex")



# + Table A.2: Coverage of Tax Data by Country: ####

# Open Entrepreneurship Database - Source WBG
Total_LLCs <- read_excel(paste0(gitHub, project, "/input/", "Total LLCs.xlsx")) %>%
  select("country_name" = Economy,
         "year" = Year,
         "count_LLC" = `Total Number of\r\nLimited Liability Companies`) 
Total_LLCs$country <- countrycode(Total_LLCs$country_name, origin = 'country.name', destination = 'iso3c') 
Total_LLCs <- Total_LLCs %>% select(-country_name)

# Load WDI variables - GDP and Population 
WDI_data <- WDI_all %>% 
  select(GDP_currentLCU, GDP_pc_const2015, pop_tot, year, iso3c ) %>% 
  rename(country = "iso3c") 

# Sample: Country and year 
table <- df.ETR.descr %>% select(country, year, country_name, count, sum_turn)

table <- table %>% 
  left_join(WDI_data, by = c("country", "year"))

table <- table %>% 
  left_join(Total_LLCs, by = c("country", "year"))

# Now we join the Surplus data from WID
WID_Data <- read_excel(paste0(gitHub, project, "/input/WID_Data_12092024-181642.xlsx"),
             col_names = FALSE)

colnames(WID_Data) <- c("country_name", "indicator", "pall", "year", "value")
WID_Data$country <- countrycode(WID_Data$country_name, origin = 'country.name', destination = 'iso3c') 

WID_Data <- WID_Data %>% 
  tidyr::separate(indicator, into = c("A", "indicator", "c", "d"), sep = "\\r") %>%
  select(country, indicator, year, value) %>%
  pivot_wider(names_from = indicator, values_from = value)

table <- table %>% 
  left_join(WID_Data, by = c("country", "year"))

# Creating the data frame with country names and years
total_production <- 
  read_excel(paste0(gitHub, project, "/input/TableA1_ETR.xlsx"), sheet = "Sheet1")

colnames(total_production) <- c("country_name", "Total Production")
total_production$country <- countrycode(total_production$country_name, origin = 'country.name', destination = 'iso3c') 
total_production <- total_production  %>% select(-country_name)

table <- table %>% 
  left_join(total_production, by = c("country"))
 

#Create table
tab <- table %>%
  arrange(GDP_pc_const2015)  %>% 
  mutate(Count = count/count_LLC,
         `GDP per capita (USD)` = GDP_pc_const2015,
         `Ratio Turnover to GDP` = sum_turn/GDP_currentLCU,
         `Ratio Turnover to total prod` = sum_turn/Tot_prod,
         year = as.character(year)) %>%
  select(country_name, year, `GDP per capita (USD)`, Count, `Ratio Turnover to GDP`, `Ratio Turnover to total prod`, `pop_tot`) %>% 
  rename(Population = pop_tot,
         Country = country_name,
        `Share of firms vs. Entrepreneurship` = Count) %>% 
  mutate_if(is.numeric, round, digits = 2) %>% as.data.frame()

tab %>% mutate(across(where(is.numeric), ~ formatC(., format = "f", big.mark = ",", digits = 0)))

 
#Save
setwd(paste0(output, "/tables"))
 
stargazer(tab, rownames = FALSE, summary = FALSE,  out = "TA2_coverage_tax_data.tex", digits = 1)
latex_code <- readLines("TA2_coverage_tax_data.tex")
# Remove \begin{table}, \end{table}, \caption{}, and \label{} lines
latex_code <- latex_code[!grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\caption\\{\\}|\\\\label\\{\\}", latex_code)]
# Write the modified LaTeX back to a new file
writeLines(latex_code, "TA2_coverage_tax_data.tex")



# + Table A.3: Number of firms in each bin, for each country ####

df_avg <- df.ETR.size.p %>% gather(Measure, value, "ETR_drop_avg":"ETR_keep_avg")  
df99 <- df_avg %>% filter(ETR_denominator == "net_profit" & Measure == "ETR_keep_avg") %>%  
  select(country, n_keep, n_drop, percentile_99.9) %>% 
  mutate(percentile_99.9 = if_else(percentile_99.9 >= 99, 99, percentile_99.9)) %>% 
  group_by(country, percentile_99.9) %>% 
  summarize(n_keep = sum(n_keep, na.rm = T), 
            n_drop = sum(n_drop, na.rm = T)) %>% ungroup()


# Panel A: Number for sample of all firms
N.all <- df99 %>% select(country, n_keep, percentile_99.9) %>%
  filter(percentile_99.9 == "90" | percentile_99.9 == "98" | percentile_99.9 == "99") %>%
  pivot_wider(names_from = "country", values_from = n_keep) %>% 
  rename( "Percentiles" = percentile_99.9)

# Panel B: Number for sample of profitable firms
N.prof <- df99 %>% select(country, n_drop, percentile_99.9) %>%  
  filter(percentile_99.9 == "90" | percentile_99.9 == "98" | percentile_99.9 == "99") %>%
  pivot_wider(names_from = "country", values_from = n_drop) %>% 
  rename( "Percentiles" = percentile_99.9)

# export
stargazer(N.all, summary = FALSE, rownames = FALSE, out = "TA3_panelA_bins_all.tex")
latex_code <- readLines("TA3_panelA_bins_all.tex")
# Remove \begin{table}, \end{table}, \caption{}, and \label{} lines
latex_code <- latex_code[!grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\caption\\{\\}|\\\\label\\{\\}", latex_code)]
# Write the modified LaTeX back to a new file
writeLines(latex_code, "TA3_panelA_bins_all.tex")


stargazer(N.prof, summary = FALSE,  rownames = FALSE, out = "TA3_panelB_bins_profitable.tex")
latex_code <- readLines("TA3_panelB_bins_profitable.tex")
# Remove \begin{table}, \end{table}, \caption{}, and \label{} lines
latex_code <- latex_code[!grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\caption\\{\\}|\\\\label\\{\\}", latex_code)]
# Write the modified LaTeX back to a new file
writeLines(latex_code, "TA3_panelB_bins_profitable.tex")


# + Table A.4: Top 1 percent Revenue, Profit, CIT and Payroll as Share of total Distribution ####

df <- df.perc.distribution %>% filter(percentile == 99) %>% 
  select(pos_profit, total_income, pos_taxliab, labor_inp, country, year)

df.avg <- df %>%  summarize(pos_profit =  mean(pos_profit),
                            total_income =  mean(total_income),
                            pos_taxliab =  mean(pos_taxliab),
                            labor_inp = mean(labor_inp, na.rm = T)) %>%
  mutate(country = "Average",
         year = "")

df <- rbind(df.avg, df) 
df <- df[, c(5, 6, 2, 1, 3, 4)]

df <- df %>% arrange(country) %>% 
  rename( "Revenue (%)" = total_income ,
                     "CIT (%)" = pos_taxliab , 
                     "Profit (%)"= pos_profit,
                     "Payroll (%)" = labor_inp,
                     " " = country,
                     "Year" = year) %>%
  mutate_if(is.numeric, round, digits = 1)


stargazer(df, rownames = FALSE, summary = FALSE,  out = "TA4_top1_stats.tex", digits = 1)

# Read the generated LaTeX file
latex_code <- readLines("TA4_top1_stats.tex")


# Remove \begin{table}, \end{table}, \caption{}, and \label{} lines
latex_code <- latex_code[!grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\caption\\{\\}|\\\\label\\{\\}", latex_code)]

# Find the "Average" line
average_line_index <- grep("^Average", latex_code)

# Split the line into components
average_line_parts <- unlist(strsplit(latex_code[average_line_index], "&"))

# Remove any trailing \\ from parts and wrap each part in \textbf{}
average_line_parts <- trimws(average_line_parts) # Remove leading/trailing spaces
average_line_parts <- gsub("\\\\$", "", average_line_parts)  # Remove trailing \\

# Manually format each part to ensure proper placement of \textbf{} and \\
average_line_parts <- sapply(average_line_parts, function(part) {
  if (grepl("\\\\$", part)) {
    # If part ends with \\, wrap without trailing space and add \\ after
    part <- gsub("\\\\$", "", part)
    return(paste0("\\textbf{", part, "} \\\\"))
  } else {
    return(paste0("\\textbf{", part, "}"))
  }
})

# Combine the parts back into a single line
average_line <- paste(average_line_parts, collapse = " & ")

# Add extra space after the "Average" line
average_line <- paste0(average_line, " \ [1.5ex]")

# Replace the original "Average" line with the modified one
latex_code[average_line_index] <- average_line

# Find the index of the last \hline before \end{tabular}
last_hline_index <- tail(grep("\\\\hline", latex_code), 1)

# Replace the last \hline with \hline \hline \\[-1.8ex]
latex_code[last_hline_index] <- "\\hline \\hline"

# Write the modified LaTeX back to a new file
writeLines(latex_code, "TA4_top1_stats.tex")

#########################| 4. REGRESSION TABLES |#################################  

setwd(paste0(output, "/tables"))

# + Table 2: Explaining the Relationship Between Effective Tax Rates and Firm Size Within the Top Decile of Firm Size ####
# + Table A.5: Explaining the Relationship Between Effective Tax Rates & Firm Size: Deciles 1 to 9 ####

# List of regressions used
df.fit <- list(df.ETR.fit.B90, df.ETR.fit.T10)
regression.nb <- c("TA5_regression_ETR_B90", "T2_regression_ETR_Top1")
regression.nb.all <- c("TA7_regression_B_B90_All", "TA7_regression_A_Top1_All")

## Average across countries of coefficient Beta (percentile)
# - sign of the coefficient
# - one-sided t-test
# - number of countries 

for (i in 1:length(df.fit)){
  # Store explanatory var
  explvar <- df.fit[[i]]$term[2]
  df.fit[[i]]$reg <- regression.nb[[i]]
  df.ETR.fit <- df.fit[[i]]  %>% mutate(# - sign of the coefficient
    pos_coeff = if_else(estimate > 0, 1, 0),
    neg_coeff = if_else(estimate < 0, 1, 0),
    # - one-sided t-test
    one_sided_pos = if_else(pos_coeff == 1 & statistic >= 1.645, 1, 0),
    one_sided_neg = if_else(neg_coeff == 1 & statistic <= -1.645, 1, 0)) %>% # direction of the estimate Panel A
    group_by(model, term) %>%
    summarise(nbr_country= length(estimate), estimate=mean(estimate), std.error = mean(std.error),
              pos_coeff = sum(pos_coeff, na.rm = T), 
              neg_coeff = sum(neg_coeff, na.rm = T),
              ttest_pos = sum(one_sided_pos),
              ttest_neg = sum(one_sided_neg),
              tstat= mean(statistic)) %>% ungroup() 
  
  # Save results with all coefficients 
  #write.csv(df.ETR.fit, file = paste0("all_coeff_", regression.nb[[i]], ".csv")) # Raw
  
  # Select only the coefficient of interest Beta 
  table.fit <- df.ETR.fit %>% filter(term == explvar) %>%
    select(estimate,  pos_coeff, neg_coeff, ttest_pos, ttest_neg, tstat,nbr_country) %>%
    t() %>% as.data.frame() ## transpose 
  # Rename 
  colnames(table.fit) <-c("Baseline", "Characteristics", "Reduced rate", "Exempt income", "Special deductions", "Re-timing", "Credits", "All")
  # colnames(table.fit) <-c("Base", "Characteristics", "Reduced rate", "Exempt income", "Special deductions", "Re-timing", "Credits",
  #                         "All", "Base (all)")
  # Subset columns
  if (regression.nb[[i]] =="B90") {
    rownames(table.fit) <- c("Percentile (1-89)",  'N positive coeff.', 'N negative coeff.', 'Upper one-sided t-test', 'Lower one-sided t-test', "tstat","N country") 
    table.fit <- subset(table.fit, rownames(table.fit) %in% c("Percentile (1-89)",  'N positive coeff.', 'Upper one-sided t-test',"N country")) 
  } else {
    rownames(table.fit) <- c("Dummy Top 1%",  'N positive coeff.', 'N negative coeff.', 'Upper one-sided t-test', 'Lower one-sided t-test', "tstat","N country") 
    table.fit <- subset(table.fit, rownames(table.fit) %in% c("Dummy Top 1%",  'N negative coeff.', 'Lower one-sided t-test',"N country")) 
  }
  
  # -- Extract 
  sink("NUL") 
  stargazer(table.fit, summary = FALSE, out = paste0(regression.nb[[i]], ".tex"), digits = 2 )
  sink()
  print(regression.nb[[i]])
}


# + Table A.7: Explaining the Effective Tax Rates & Firm Size Relation at the Top: ####
#              Regression Table with Country-Specific Coefficients 

# For each regression:
# Add stars
startup <- function(x, out=NULL, ...){
  undo <- gsub("\\\\textasteriskcentered", "*", stargazer(x, ...))
  restar <- gsub("* * *", "$^{***}$", undo, fixed = TRUE)
  restar <- gsub("* *", "$^{**}$", restar, fixed = TRUE)
  restar <- gsub("* ", "$^{*}$", restar, fixed = TRUE)
  if(!is.null(out)) cat(restar, file = out, sep="\n")
  restar
}

for (i in 1:length(df.fit)){
  # Store explanatory var
  explvar <- df.fit[[i]]$term[2]
  
  df.ETR.fit <- df.fit[[i]]  %>%
    group_by(model, term) %>%
    summarise(nbr_country= length(estimate), estimate=mean(estimate, na.rm = T), std.error = mean(std.error),
              p.value = length(p.value[p.value<0.05])) %>% ungroup()
  
  # --merge average and countries separately
  table.fit <- df.ETR.fit %>% filter(term==explvar) %>% select(estimate) %>% mutate(estimate = as.character(round(estimate, 2))) %>%
    t() %>% as.data.frame() %>% mutate(country = "Average") 
  colnames(table.fit) <- c("0", "1", "2", "3", "4", "5", "6", "7", "country")
  
  temp <- df.fit[[i]] %>% filter(term==explvar) %>% mutate(stars = if_else(p.value<0.1, "*", ""),
                                                           stars = if_else(p.value<0.05, "**", stars),
                                                           stars = if_else(p.value<0.01, "***", stars),
                                                           estimate.star = paste(round(estimate, 2), stars)) %>% 
    group_by(country, model) %>% select(estimate.star) %>% spread(model, estimate.star)
  rownames(temp) <- temp$country
  
  # Country name:
  df.name <- df.ETR.descr %>% select(country, country_name) %>% unique()
  temp <- left_join(temp, df.name, by = c("country")) %>% 
    mutate(country = country_name) %>% select(-country)
  
  df <- bind_rows(table.fit, temp)
  colnames(df) <- c( "Baseline", "Characteristics", "Reduced rate", "Exempt income", "Special deductions", "Re-timing", "Credits",
                     "All", "country")
  df <- df[, c(9, 1,2,3,4,5,6,7, 8)]
  
  # -- Extract
  sink("NUL") 
  startup(df, 
          summary = FALSE, 
          rownames = FALSE, 
          out = paste0(regression.nb.all[[i]], ".tex"),
          type = "latex",
          colnames = TRUE, 
          notes = "Levels: ${}^{***} p < .01$, ${}^{**} p < .05$, ${}^{*} p < .1$")
  sink()
  print(regression.nb[[i]])
}



# + Table A.6: Explaining the Effective Tax Rates & Firm Size Relation at the Top: Robustness ####

df.fit <- list(df.ETR.fit.T10, df.ETR.fit.T10.D2, df.ETR.fit.T10.D3, df.ETR.fit.T10.D01, df.ETR.fit.T10.C,
               df.ETR.fit.T20, df.ETR.fit.T20.D2, df.ETR.fit.T20.D3, df.ETR.fit.T20.D01, df.ETR.fit.T20.C)
regression.nb <- c("T10", "T10.D2", "T10.D3", "T10.D01", "T10.C", 
                   "T20", "T20.D2", "T20.D3", "T20.D01", "T20.C")

## Average across countries of coefficient Beta (percentile)
# - sign of the coefficient
# - one-sided t-test
# - number of countries 
temp <- list()

for (i in 1:length(df.fit)){
  # Store explanatory var
  explvar <- df.fit[[i]]$term[2]
  df.fit[[i]]$reg <- regression.nb[[i]]
  df.ETR.fit <- df.fit[[i]]  %>% mutate(
    pos_coeff = if_else(estimate > 0, 1, 0),
    neg_coeff = if_else(estimate < 0, 1, 0),
    # - one-sided t-test
    one_sided_pos = if_else(pos_coeff == 1 & statistic >= 1.645, 1, 0),
    one_sided_neg = if_else(neg_coeff == 1 & statistic <= -1.645, 1, 0)) %>% #
    group_by(model, term) %>%
    summarise(nbr_country= length(estimate), estimate=mean(estimate), std.error = mean(std.error),
              pos_coeff = sum(pos_coeff, na.rm = T), 
              neg_coeff = sum(neg_coeff, na.rm = T),
              ttest_pos = sum(one_sided_pos),
              ttest_neg = sum(one_sided_neg),
              tstat= mean(statistic)) %>% ungroup() %>% filter(model == 0)
  
  # Select only the coefficient of interest Beta 
  table.fit <- df.ETR.fit %>% filter(term == explvar) %>%
    select(estimate,  pos_coeff, neg_coeff, ttest_pos, ttest_neg, tstat,nbr_country) %>%
    t() %>% as.data.frame() ## transpose 
  # Rename 
  colnames(table.fit) <-c(regression.nb[[i]])
  # Subset columns
  rownames(table.fit) <- c(regression.nb[[i]],  'N positive coeff.', 'N negative coeff.', 'Upper one-sided t-test', 'Lower one-sided t-test', "tstat","N country") 
  table.fit <- subset(table.fit, rownames(table.fit) %in% c(regression.nb[[i]],  'N negative coeff.', 'Lower one-sided t-test',"N country")) 
  
  temp[[i]] <- table.fit
}

table.fit <- do.call("cbind", temp)

# -- Extract 
sink("NUL") 
stargazer(table.fit, summary = FALSE, out = paste0("TA6_regression_robustness.tex"), digits = 2 )
sink()
print("Robustness table")




#########################| 5. FIGURES |################################# 


#### + Figure 2: ETR and Firm size, All firms #### 
#### + Figure A3: ETR and Firm size, Profitable firms #### 

# Data:
df_avg <- df.ETR.size.p %>% gather(Measure, value, "ETR_drop_avg":"ETR_keep_avg") #%>% select(-country_name)
# Add full country names 
df_avg <- merge(df_avg, df_STR_name, by = c("country"))


# Graph (control version spline):
ETR_panel_group_GRAPH <- function(dataset, colour, knot){
  dataset$country_name <- reorder(dataset$country_name, dataset$max_STR) 
  dSTR <- dataset %>% group_by(country_name) %>% summarise(value = max(STR), Measure = "Top STR", p = p) %>% ungroup()
  p <- ggplot(data = dataset, aes(x = p, 
                                  y = value, colour=Measure, linetype=Measure))
  p <- p  +  scale_x_continuous(
    breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
    labels =  c("","20","","40","","60","","80","","99","",  "99.9")) + 
    geom_rect(aes (xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf, fill = "Top 1%"), colour = NA, alpha = 0.05) +
    geom_point( shape = c(3), size = 0.5, color = 'grey60') +
    geom_spline(aes( x = p, y = value), size= 0.9, nknots = knot) +
    geom_line(data = dSTR, aes(x = p, y = value), size = 0.8) +
    scale_colour_manual(values = c( colour, "grey40")) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    facet_wrap(~country_name, drop = T, ncol = 4) 
  
  #Aesthetic
  p <- p + 
    scale_fill_manual(
      values = c( "lightgrey"),  # Add a second (invisible) color
      labels = c("Top 1%"),  # Labels for both entries
      guide = guide_legend(override.aes = list(alpha = c(1)))
      ) 
  p <- p +  
    labs(subtitle = "Countries ordered by Statutory Tax Rate", 
    y = "", x = "\nFirm Size Quantiles") 
   
  p <- gg_guidesFUN(p)
}


#### + Figure 2: ETR and Firm size, All firms ### 
df.all <- df_avg  %>% renameFUN() %>% percentileFUN() %>% 
  filter(Measure == 'All firms' & ETR_denominator == "Net Profit" ) %>% 
  select(country_name, country, value, Measure, p, ETR_denominator, STR, max_STR) %>%
  mutate(Measure = "ETR (All firms, incl. loss-making)") 

# Export
setwd(paste0(output, "/figures"))
write.csv(df.all, paste0("F2_metadata.csv"))
pdf("F2_ETR_size_all.pdf")
print(ETR_panel_group_GRAPH(df.all, "#00BFC4", 6)) 
dev.off()


#### + Figure A3: ETR and Firm size, Profitable firms ### 
df.prof <- df_avg  %>% renameFUN() %>% percentileFUN() %>% 
  filter(Measure == 'Profitable firms' & ETR_denominator == "Net Profit") %>% 
  select(country_name, country, value, Measure, p, ETR_denominator, STR, max_STR) %>%
  mutate(Measure = "ETR (Profitable firms only)") 

# Export
setwd(paste0(output, "/figures"))
write.csv(df.prof, paste0("FA3_metadata.csv"))
pdf("FA3_ETR_size_profitable.pdf")
print(ETR_panel_group_GRAPH(df.prof, "#8073ac", 6)) 
dev.off()



#### + Figure 3: ETR Minus STR, Profitable firms #### 

# Data:
df_str_etr <- df.ETR.size.p %>% 
  filter(ETR_denominator == "net_profit") %>%
  select(STR, ETR_minus_STR, ETR_drop_avg, percentile_99.9, country) %>%
  percentileFUN() %>%  
  mutate(y = ETR_drop_avg-STR) 
# Add full country names 
df_str_etr <- merge(df_str_etr, df_STR_name, by = c("country"))

 

# Graph:
ETR_STR_p_GRAPH <- function(dataset, yvar){
  dataset$yvar<- f_eval(~uq(yvar), data = dataset)
  dataset <- dataset %>% mutate(Measure = "ETR-STR (Profitable firms only)")
  dataset$country_name <- reorder(dataset$country_name, dataset$max_STR) 
  
  p <- ggplot(data = dataset, aes(x = p, 
                                  y = yvar, colour=Measure)) 
  p <- p  + 
    scale_x_continuous( breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
                        labels =  c("","20","","40","","60","","80","","99", "", "99.9")) +
    geom_rect(aes (xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf, fill = "Top 1%"), colour = NA, alpha = 0.05) +
    geom_point( shape = c(3), size = 0.5, color = 'grey60') +
    geom_hline( yintercept = 0, size  = 0.2, color = "grey40", linetype ="longdash") + 
    geom_spline(aes(x = p, y = yvar), size= 0.9, nknots = 6) +
    scale_fill_manual("Top 1%", values = "lightgrey", 
                      guide = guide_legend(override.aes = list(alpha = 1))) +
    scale_colour_manual(values = c("#F8766D")) +
    facet_wrap(~country_name, drop = T, ncol = 4)  
  p <- p +  
    labs(subtitle = "Countries ordered by Statutory Tax Rate", 
         y = "", x = "\nFirm Size Quantiles")
  
  p <- gg_guidesFUN(p)
  
}


# Export:
adj <- df_str_etr  
write.csv(adj, paste0("F3_metadata.csv"))
pdf("F3_ETR_minus_STR.pdf")
print(ETR_STR_p_GRAPH(adj, ~ETR_minus_STR))
dev.off()


#### + Figure 4: ETR and Firm size: Robustness  ####
#................................

#-- PANEL a. Cross country Average ETR

# Data:
df_avg_net_profit <- df.ETR.size.p  %>% filter(ETR_denominator == "net_profit") %>% 
  group_by(percentile_99.9, ETR_denominator) %>%
  summarize(ETR_drop_avg=mean(ETR_drop_avg, na.rm = T), ETR_keep_avg=mean(ETR_keep_avg, na.rm = T),
            n_keep =mean(n_keep), n_drop = mean(n_drop)) %>% 
  gather(Measure, value, "ETR_drop_avg":"ETR_keep_avg")  %>% ungroup()  %>%
  mutate(country_name = 1) 

# Graph: 
ETR_size_avg_GRAPH <- function(dataset, ETR_denom, ytitle, metric){
  dataset <- dataset %>% filter(ETR_denominator == ETR_denom )  %>% renameFUN() %>% percentileFUN() %>%
    mutate(Measure = factor(Measure, levels = c("All firms", "Profitable firms"))) %>%
    filter(Measure ==  "Profitable firms") %>% mutate(Measure = "Average ETR (Profitable firms only)")
  sub <- dataset
  p <- ggplot(data = dataset, aes(x = p, 
                                  y = value, group = Measure, color=factor(Measure)))  
  p <- p  +  
    geom_rect(aes (xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf), colour = NA, alpha = 0.1, fill = "lightgrey") +
    geom_spline(aes(x = p, y = value), size= 0.9, nknots = 6) +
    geom_point(shape = c(3), size = 0.9, color = "grey60") +
    scale_x_continuous( breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
                        labels =  c("10","20","30","40","50","60","70","80","90","99.0", "", "99.9")) +
    scale_color_manual(values = c("#F8766D")) 
  
  p  <- p + labs(subtitle = "Countries ordered by Statutory Tax Rate", 
    x = "\nFirm Size Quantiles", y = "")  +
    scale_y_continuous(limits = c(15,25))
  
   gg_guidesFUN(p)
}

# Export:
setwd(paste0(output, "/figures"))
pdf("F4_a_ETR_country_avg.pdf")
ETR_size_avg_GRAPH(df_avg_net_profit, "net_profit", "Effective Tax Rate (%)\n", "Average")
dev.off()

write.csv(df_avg_net_profit, file = "F4_a_metadata.csv")



#-- PANEL b. ETR by Sector

df <- df.ETR.largesector.p %>% percentileFUN() %>% group_by(p, largesector, ETR_denominator) %>% 
  summarize(value = mean(ETR_drop_avg, na.rm = T),
            nb_country = n()) %>% 
  filter(ETR_denominator=="net_profit")


# Graph: 
ETR_largesectordec_GRAPH <- function(dataset, ETR_denom){
  dataset <- dataset  %>% filter(ETR_denominator == ETR_denom) %>% filter(!is.na(largesector)) %>%
    mutate(largesector = factor(largesector, levels = c("Primary", "Secondary", "Retail", "Services")))
  p <- ggplot(data = dataset, aes(x = p, 
                                  y = value, group = largesector, color=factor(largesector), linetype = factor(largesector)))  
  p <- p  +   geom_rect(aes (xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf), colour = NA, alpha = 0.1, fill = "lightgrey") +
    geom_spline(aes(x = p, y = value), size= 0.9, nknots = 6) +
    scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
                       labels =  c("10","20","30","40","50","60","70","80","90","99.0", "", "99.9")) +
    scale_color_manual(values = c("#008ECE" ,"#990000" ,"#588300","#FAAB18"  )) + 
    scale_linetype_manual(values = c("dashed", "dotted", "longdash", "dotdash"))

  p  <- p +  labs(
    x = "\nFirm Size Quantiles", y = "")  +
    scale_y_continuous(limits = c(15,25))
   gg_guidesFUN(p)
}

# Export:
setwd(paste0(output, "/figures"))
pdf("F4_b_ETR_sector_avg.pdf")
ETR_largesectordec_GRAPH(df, "net_profit")
dev.off()

write.csv(df, file = "F4_b_metadata.csv")

#-- PANEL c. Average lifetime ETR: balanced panel (1 to 5 years)

# Data: 
df <- df.ETR.panel.bal %>% gather(Measure, value, "ETR_all_panel":"ETR_prof_panel") %>% 
  group_by(percentile_99.9, Measure, year) %>%
  summarize(value = mean(value, na.rm = T),
            n_country = length(country)) %>% mutate(country = "") %>% ungroup() %>%
  select(Measure, percentile_99.9, value, year, n_country)   

df_avg_panel_prof <- df %>% filter(Measure == "ETR_prof_panel") %>% filter(year>1)
df_avg_panel_all <- df %>% filter(Measure == "ETR_all_panel")   %>% filter(year>1)



# Graph:
ETR_balpanel_GRAPH = function(dataset){
  dataset <- dataset %>% percentileFUN() %>%
    mutate(year = paste0(year, "-year"))
  
  p <- ggplot(data = dataset, aes(x = p, 
                                  y = value, group = year, color=factor(year), linetype = factor(year)))  
  p <- p  +   geom_rect(aes (xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf), colour = NA, alpha = 0.1, fill = "lightgrey") +
    geom_spline(aes(x = p, y = value), size= 0.9, nknots = 6) +
    scale_x_continuous( breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
                        labels =  c("10","20","30","40","50","60","70","80","90","99.0", "", "99.9")) #+

  p <- p + labs(  
    x = "\nFirm Size Quantiles", y = "")  +     scale_y_continuous(limits = c(15,25))
  p <- gg_guidesFUN(p)
  print(p)
}


# Export: 
setwd(paste0(output, "/figures"))
pdf("F4_c_ETR_lifetime_avg.pdf")
print(ETR_balpanel_GRAPH(df_avg_panel_prof)) 
dev.off()

write.csv(df_avg_panel_prof, file = "F4_c_metadata.csv")

#### + Figure A4: Share of unprofitable firms ####

df.ETR.noprof <- df.ETR.size.p %>% 
  filter(ETR_denominator == "net_profit") %>% 
  mutate( n_nonprof = n_keep - n_drop,
          share_nonprof = 100*n_nonprof/n_keep,
          Measure= "Share of unprofitable firms") %>% 
  select(country, Measure, percentile_99.9, share_nonprof, n_nonprof, n_keep) 
df.ETR.noprof <- merge(df.ETR.noprof, df_STR_name, by = c("country")) %>% percentileFUN()


FUN_below15 = function(source_df, variable){ 
  source_df$country_name <- reorder(source_df$country_name, source_df$max_STR) 
  p <- ggplot(data = source_df , aes(x = p, y = .data[[variable]], color = Measure)) 
  p <- p  + 
    scale_x_continuous( breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
                        labels =  c("","20","","40","","60","","80","","99", "", "99.9")) +
    geom_rect(aes (xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf, fill = "Top 1%"), colour = NA, alpha = 0.05) +
    geom_point( shape = c(3), size = 0.5, color = 'grey60') +
    geom_spline(aes(x = p, y = .data[[variable]]), size= 0.9, nknots = 6) +
    scale_colour_manual(values = c("#69b3a2")) +
    scale_linetype_manual(values = c("solid")) +
    scale_fill_manual("Top 1%", values = "lightgrey", 
                      guide = guide_legend(override.aes = list(alpha = 1))) +
    facet_wrap(~country_name, drop = F, ncol = 4)  
  
  p  <- p +  labs(subtitle = "Countries ordered by Statutory Tax Rate", 
                  y = "", x = "\nFirm Size Quantiles") 
  p <- gg_guidesFUN(p)
  print(p)
}

setwd(paste0(output, "/figures"))
pdf("FA4_unprofitable.pdf")
FUN_below15(df.ETR.noprof, "share_nonprof")
dev.off() 

write.csv(df.ETR.noprof, file = "FA4_metadata.csv")

#### + Figure A5: Cross country correlations between tax variables and macro variables ####

# List of regressions used
fit.B90.prof <- df.ETR.fit.B90 %>%
  filter(term == "percentile_99.9" & model == 0) %>% select(country, estimate) %>%
  rename(coeff_p90 = "estimate")

# For dummy 1% coeff, we scale by intercept to get a percentage drop
fit.T10.prof <- df.ETR.fit.T10 %>% select(-to_drop)

fit.T10.prof <- fit.T10.prof %>%
  filter((term == "top99" | term == "(Intercept)") & model == 0) %>%
  select(term, estimate, country) %>%  group_by(country) %>%
  spread(term, estimate) %>%
  mutate(drop_pct_dummy1 = top99/`(Intercept)`) %>%
  select(country, drop_pct_dummy1)

# Extract ETR avg and ETR-STR: have to deal with the top being split up first
fit.ETR <- df.ETR.size.p %>% filter(ETR_denominator == "net_profit")
p99 <- fit.ETR %>% filter(percentile_99.9>=99) %>% group_by(country, ETR_denominator) %>%
  fmean(na.rm = T) %>% mutate(percentile_99.9 = 99) %>% ungroup()
fit.ETR <- fit.ETR %>% filter(percentile_99.9<99)
fit.ETR <- rbind(fit.ETR, p99) %>%
  group_by(country) %>%
  summarize(avg_ETR_all = mean(ETR_keep_avg, na.rm = T),
            avg_ETR_prof = mean(ETR_drop_avg, na.rm = T),
            avg_ETR_STR = mean(ETR_minus_STR, na.rm = T),
            mean_STR = mean(STR, na.rm = T),
            max_STR = max(STR, na.rm = T))

# Divide by max_STR
fit.ETR.STR <- fit.ETR %>%
  mutate(avg_ETR_all = avg_ETR_all/max_STR,
         avg_ETR_prof = avg_ETR_prof/max_STR,
         avg_ETR_STR = avg_ETR_STR/max_STR)

## Merge all together:
coeff_correlation <- merge(fit.B90.prof, fit.T10.prof, by = "country")

coeff_correlation.STR <- merge(coeff_correlation, fit.ETR.STR, by = "country")
coeff_correlation <- merge(coeff_correlation, fit.ETR, by = "country")

### Country coefficient to interact with:
wdi_country <- df.ETR.descr %>% select(country) %>% 
  mutate(country = if_else(country == "ESW", "SWZ", country)) %>% unique() %>%  pull()
wdi_year <- df.ETR.descr %>% select(year) %>% unique() %>%  pull()
temp <- df.ETR.descr %>%  mutate(country = if_else(country == "ESW", "SWZ", country)) %>% 
  group_by(country, year) %>%  summarize()

## Country coefficient to interact with:
WDI_data <- read.csv(
  paste0(gitHub, project, "/input/WDI_all_correlation.csv")) %>% select(-X)

WDI_data <- WDI_data %>%
  mutate(log_GDP_pc = log(GDP_pc_const2015)) %>%
  select(-c(country, iso2c)) %>% rename(country = iso3c)

df.wdi <-  left_join(temp, WDI_data, by = c('country', "year"))
write.csv(df.wdi,
          paste0(gitHub, project, "/input/wdi_appendix.csv"))


df.corr <- merge(coeff_correlation, df.wdi, by = c("country"))
df.corr.STR <- merge(coeff_correlation.STR, df.wdi, by = c("country"))


### CREATE z-score of macro measures to be able to compare then:
df.wdi.z <- df.wdi %>% mutate(z_log_GDP_pc = (log_GDP_pc - mean(df.corr$log_GDP_pc)) /
                                sd(df.corr$log_GDP_pc),
                              z_pop = (pop - mean(df.corr$pop)) / sd(df.corr$pop),
                              z_FDI_netinflow = (FDI_netinflow_share - mean(df.corr$FDI_netinflow_share)) /
                                sd(df.corr$FDI_netinflow_share),
                              z_trade_pct_GDP = (trade_pct_GDP - mean(df.corr$trade_pct_GDP)) /
                                sd(df.corr$trade_pct_GDP),
                              z_income_tax = (income_tax_pct_all - mean(df.corr$income_tax_pct_all, na.rm = T)) /
                                sd(df.corr$income_tax_pct_all, na.rm = T))

df.corr <- merge(coeff_correlation, df.wdi.z, by = c("country"))
df.corr.STR <- merge(coeff_correlation.STR, df.wdi.z, by = c("country"))


# + Regressions:
#................................

# Get correlation coefficient between two measures of percentiles: #
reg_FUN = function(var, label, source_df){
  df <- source_df %>% filter(!is.na(var))
  f.p90 <- feols(as.formula(paste0("coeff_p90 ~ ", var)), data =df) %>% tidy() %>% mutate(measure = "Coeff <P90", Label = label)
  f.t10 <- feols(as.formula(paste0("drop_pct_dummy1 ~ ", var)), data =df) %>% tidy() %>% mutate(measure = "Coeff Top1%", Label = label)
  f.ETR_a <- feols(as.formula(paste0("avg_ETR_all ~ ", var)), data =df) %>% tidy() %>% mutate(measure = "Avg ETR (All)", Label = label)
  f.ETR_p <- feols(as.formula(paste0("avg_ETR_prof ~ ", var)), data =df) %>% tidy() %>% mutate(measure = "Avg ETR (profitable)", Label = label)
  f.ETR_STR <- feols(as.formula(paste0("avg_ETR_STR ~ ", var)), data =df) %>% tidy() %>% mutate(measure = "Avg STR-ETR (tax gap)", Label = label)
  #f.max_STR <- feols(as.formula(paste0("max_STR ~ ", var)), data =df) %>% tidy() %>% mutate(measure = "Max STR", Label = label)
  
  fit <- rbind(f.p90, f.t10, f.ETR_a, f.ETR_p, f.ETR_STR)  %>% #, f.max_STR
    filter(term != "(Intercept)")
  
}

# GRAPH
graph_FUN = function(source_df){
  
  ggplot(source_df, aes(color = measure, shape = measure)) +
    geom_hline(yintercept = 0, colour = gray(1/2), lty = 2 ) +
    # geom_linerange(aes( x = Label, ymin = estimate-1.64*std.error, ymax = estimate+1.64*std.error), lwd = 0.6, position = position_dodge(width = 1/2)) +
    geom_pointrange(aes( x = Label, y = estimate,  ymin = estimate-1.64*std.error, ymax = estimate+1.64*std.error), lwd = 1/2, position = position_dodge(width = 1/2), fill = "white") +
    scale_shape_manual(values = c(21, 22, 23, 24, 25),
                       breaks = c("Avg STR-ETR (tax gap)", "Avg ETR (All)", "Avg ETR (profitable)", "Coeff <P90", "Coeff Top1%")) +
    scale_color_manual(values = c("orange", "yellow4", "seagreen3", "steelblue2", "hotpink2"),
                       breaks = c("Avg STR-ETR (tax gap)", "Avg ETR (All)", "Avg ETR (profitable)", "Coeff <P90", "Coeff Top1%")) +
    coord_flip() +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
    labs(title = "",
         y = "",
         x = "") + 
    theme(legend.position ="right")  + 
    guides(shape = guide_legend(""), colour = guide_legend("")) ##, reverse = TRUE))
}

# Here we divide ETR, ETR-STR by maximum STR in the country
df.corr.STR <- df.corr.STR %>% mutate(max_STR = (max_STR - mean(df.corr.STR$max_STR)) / sd(df.corr.STR$max_STR))
df.corr.STR <- df.corr.STR %>% mutate(avg_ETR_STR = -avg_ETR_STR)

df.STR <- reg_FUN("z_log_GDP_pc", "log(GDP per capita)", df.corr.STR)
df.STR <- rbind(df.STR, reg_FUN("z_pop", "Population", df.corr.STR))
df.STR <- rbind(df.STR, reg_FUN("z_FDI_netinflow", "FDI, net inflows (% GDP)", df.corr.STR))
df.STR <- rbind(df.STR, reg_FUN("z_trade_pct_GDP", "Trade (% GDP)", df.corr.STR))
df.STR <- rbind(df.STR, reg_FUN("z_income_tax", "Income & Profit Tax (% Total Tax)", df.corr.STR))
df.STR <- rbind(df.STR, reg_FUN("max_STR", "Max. STR", df.corr.STR))


df.STR$measure <- factor(df.STR$measure, levels=c('Coeff Top1%', 'Coeff <P90', 'Avg ETR (profitable)', 'Avg ETR (All)', 'Avg STR-ETR (tax gap)'))
df.STR$Label <- factor(df.STR$Label, levels=c('FDI, net inflows (% GDP)', 'Trade (% GDP)', 'Income & Profit Tax (% Total Tax)', 'Max. STR', 'Population', 'log(GDP per capita)'))

setwd(paste0(output, "/figures"))
pdf("FA5_regressions_robustness_STR.pdf", width = 14, height = 9)
graph_FUN(df.STR)
dev.off()
write.csv(df.STR, file = "FA5_metadata.csv")

#### + Figure A6: Alternative Firm Size Measures: ETR by Percentiles of Payroll and Total Assets ####

# Data:
df <- df.ETR.size.p %>% filter(ETR_denominator == "net_profit") %>% ungroup() %>% select(country, percentile_99.9)
df.STR <- right_join(df, df_STR_name, by = c("country")) %>% 
  select(percentile_99.9, country, country_name, max_STR) %>% mutate(`Top STR` = max_STR) %>% 
  gather(Measure, value, c(`Top STR`))

# Data Payroll: 
df.payroll <- df.ETR.payroll %>% gather(Measure, value, c("ETR_drop_avg", "ETR_keep_avg")) %>%
  filter(Measure!="ETR_keep_avg") %>% rename(percentile_99.9 = perc_new_99.9)

# Data Assets: 
df.assets <- df.ETR.assets %>% gather(Measure, value, c("ETR_drop_avg", "ETR_keep_avg")) %>%
  filter(Measure!="ETR_keep_avg") %>% rename(percentile_99.9 = perc_new_99.9)

# Data:merge asset and payroll
df.payroll <- df.payroll %>% mutate(Measure = "Payroll") %>% select(-labor_inp)
df.assets <- df.assets %>% mutate(Measure = "Total assets") %>% select(-total_assets)
df.merge.a.p <- rbind(df.payroll, df.assets) 
df.merge.a.p <- right_join(df.merge.a.p, df_STR_name, by = c("country"))
df.merge.a.p <- rbind(df.merge.a.p, df.STR)  %>% percentileFUN() 


# Graph (control version spline):
ETR_panel_group_GRAPH <- function(dataset){
  dataset$country_name <- reorder(dataset$country_name, dataset$max_STR) 
  # Define the desired order for Measure levels
  measure_levels <- c("Payroll", "Total assets", "Top STR")
  
  # Apply factor levels to Measure in dataset
  dataset$Measure <- factor(dataset$Measure, levels = measure_levels)
  
  p <- ggplot(data = dataset, aes(x = p, 
                                  y = value, colour=Measure, linetype=Measure))
  p <- p  +  scale_x_continuous(
    breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
    labels =  c("","20","","40","","60","","80","","99","",  "99.9")) + 
    geom_rect(aes (xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf, fill = "Top 1%"), inherit.aes = FALSE, colour = NA, alpha = 0.05) +
    geom_spline(aes( x = p, y = value), size= 0.9, nknots = 7) +
    scale_colour_manual(values = c(  "#61D04F", "#1B9E77","grey40" )) +
    scale_linetype_manual(values = c( "solid", "longdash",  "dashed")) +
    facet_wrap(~country_name, drop = T, ncol = 4) 
  
  #Aesthetic
  p <- p + 
    scale_fill_manual(
      values = c( "lightgrey"),  # Add a second (invisible) color
      labels = c("Top 1%"),  # Labels for both entries
      guide = guide_legend(override.aes = list(alpha = c(1)))
    ) 
  p <- p +  
    labs(subtitle = "Countries ordered by Statutory Tax Rate", 
         y = "", x = "\nFirm Size Quantiles (Payroll or Total assets)") +
    guides(
      colour = guide_legend(title = "Measure"),
      linetype = guide_legend(title = "Measure"),
      fill = guide_legend(title = "Top 1%", override.aes = list(alpha = 1))
    ) 
  p <- gg_guidesFUN(p)
  print(p)
  
}

# Export
setwd(paste0(output, "/figures"))
pdf("FA6_ETR_size_robust.pdf")
ETR_panel_group_GRAPH(df.merge.a.p) 
dev.off()
write.csv(df.merge.a.p, file = "FA6_metadata.csv")

# extract correlation coefficients between percentile of Measures and Revenue
corr <- df.merge.a.p %>% group_by(Measure, country) %>% select(correlation) %>% fmean()
setwd(paste0(output, "/figures"))
write.csv(corr, "FA6_metadata_correlation_size.csv")


#### + Figure A7: Correlations of Assets and Profit at the top: ####

# Panel A: Correlation of Assets and Profit

# Redefine decile without splitting the top 1%
df <- df.ETR.corr.asset %>% 
  mutate(decile = if_else(decile != 1 & percentile < 10, 1, decile),
         decile = if_else(decile != 2 & percentile >= 10 & percentile < 20, 2, decile),
         decile = if_else(decile != 3 & percentile >= 20 & percentile < 30, 3, decile),
         decile = if_else(decile != 4 & percentile >= 30 & percentile < 40, 5, decile),
         decile = if_else(decile != 5 & percentile >= 40 & percentile < 50, 5, decile),
         decile = if_else(decile != 6 & percentile >= 50 & percentile < 60, 6, decile),
         decile = if_else(decile != 7 & percentile >= 60 & percentile < 70, 7, decile),
         decile = if_else(decile != 8 & percentile >= 70 & percentile < 80, 8, decile),
         decile = if_else(decile != 9 & percentile >= 80 & percentile < 90, 9, decile),
         decile = if_else(decile != 10 & percentile >= 90, 10, decile)) %>% 
  # Average within countries
  group_by(country, percentile, decile) %>% 
  summarize(mean = mean(ratio_asset, na.rm = T)) %>% filter(decile>=8) %>% 
  # Average across countries
  group_by(percentile, decile) %>% 
  summarize(mean = mean(mean, na.rm = T)) %>% 
  # Rename Deciles: 
  mutate(decile = as.character(decile),
         decile = fct_recode(decile, "D8" = "8", "D9" = "9", "D10" = "10"))

setwd(paste0(output, "/figures"))
pdf("FA7_panelA_correlation.pdf", width = 9, height = 5 )
p <- ggplot(df
            , aes(percentile, mean)) +
  geom_point() + geom_smooth(formula = y~ x, method = "lm", color = "cornflowerblue") + 
  scale_x_continuous(breaks = seq(70, 100, by = 2)) +
  scale_y_continuous(limits = c(4, 20)) +
  facet_wrap(~decile, scales = "free_x") +
  labs(y = "100*(Profit/Total assets)\n",  x = "\nFirm Size Quantiles") 
p <- gg_guidesFUN(p)
print(p)
dev.off()

write.csv(df, file = "FA7_a_metadata.csv")


# Panel B: Share of Missing or 0 total asset:  
df <- df.ETR.corr.asset %>% 
  mutate(decile = if_else(decile != 1 & percentile < 10, 1, decile),
         decile = if_else(decile != 2 & percentile >= 10 & percentile < 20, 2, decile),
         decile = if_else(decile != 3 & percentile >= 20 & percentile < 30, 3, decile),
         decile = if_else(decile != 4 & percentile >= 30 & percentile < 40, 5, decile),
         decile = if_else(decile != 5 & percentile >= 40 & percentile < 50, 5, decile),
         decile = if_else(decile != 6 & percentile >= 50 & percentile < 60, 6, decile),
         decile = if_else(decile != 7 & percentile >= 60 & percentile < 70, 7, decile),
         decile = if_else(decile != 8 & percentile >= 70 & percentile < 80, 8, decile),
         decile = if_else(decile != 9 & percentile >= 80 & percentile < 90, 9, decile),
         decile = if_else(decile != 10 & percentile >= 90, 10, decile)) %>% 
  filter(decile>=8) %>% 
  # Average across countries
  group_by(percentile, decile) %>% 
  summarize(mean = mean(na_asset, na.rm = T)) %>% 
  # Rename Deciles: 
  mutate(decile = as.character(decile),
         decile = fct_recode(decile, "D8" = "8", "D9" = "9", "D10" = "10"))


pdf("FA7_panelB_missing.pdf", width = 9, height = 5 )
p <- ggplot( df, 
             aes(percentile, mean)) +
  geom_point() + 
  geom_smooth(formula = y~x, method = "lm", color = "#69b3a2") + 
  scale_x_continuous(breaks = seq(70, 100, by = 2)) +
  scale_y_continuous(limits = c(0, 50)) +
  facet_wrap(~decile, scales = "free_x") +
  labs(y = "Total assets is missing or null (%)\n",  x = "\nFirm Size Quantiles")  
p <- gg_guidesFUN(p)
print(p)
dev.off()

write.csv(df, file = "FA7_b_metadata.csv")

#### + Figure A8: Alternative ETR Measure in Which Foreign Tax Credits Are Not Deducted from Net Tax Liability  ####

# Data:
df_avg <- df.ETR.size.num.p %>% gather(Measure, value, "ETR_drop_avg":"ETR_keep_avg") 
df_add <- df.ETR.size.p %>% gather(Measure, value, "ETR_drop_avg":"ETR_keep_avg") 

# Add max STR by country, and groups
df_avg <- rbind(df_avg, df_add)

df_avg <- right_join(df_avg, df_STR_name, by = c("country"))


# Graph (control version spline):
ETR_numerator_GRAPH <- function(dataset, colour, knot, numerator, name){
  dataset$numerator <- f_eval(~uq(numerator), data = dataset)
  dataset$country_name <- reorder(dataset$country_name, dataset$max_STR) 
  dSTR <- dataset %>% group_by(country_name) %>% summarise(value = max_STR, Measure = "Top STR", p = p) 
  p <- ggplot(data = dataset, aes(x = p, 
                                  y = value, colour = Measure, linetype = Measure))
  p <- p  +  scale_x_continuous( breaks = c(10,20,30,40,50,60,70,80,90,101, 111, 121),
                                 labels =  c("","20","","40","","60","","80","","99", "", "99.9")) +
    geom_rect(aes (xmin =  101, xmax=Inf, ymin = -Inf, ymax = Inf, fill = "Top 1%"), colour = NA, alpha = 0.05) +
    geom_spline(aes( x = p, y = value), size= 0.9, nknots = knot) +
    geom_line(data = dSTR, aes(x = p, y = value), size = 0.8) +
    facet_wrap(~country_name, drop = F, ncol = 4) 
  
  #Aesthetic
  p <- p + scale_colour_manual(values = c(colour,  "#DF536B", "grey40")) +
    scale_linetype_manual(values = c( "solid", "dashed", "dashed")) +
    scale_fill_manual("Top 1%", values = "lightgrey", 
                      guide = guide_legend(override.aes = list(alpha = 1))) +
  scale_y_continuous(limits = c(0,36)) 
  p <- p +  labs(subtitle = "Countries ordered by Statutory Tax Rate", 
    y = "", x = "\nFirm Size Quantiles") 
  p <- gg_guidesFUN(p)
  print(p)
}


df.prof <- df_avg  %>% percentileFUN() %>% filter(Measure == "ETR_drop_avg") %>% 
  select(country_name, country, value, p, Measure, ETR_denominator, STR, max_STR) %>%
  filter(ETR_denominator == "ntl_noelse" | ETR_denominator == "net_tax_liability") %>%
  mutate(Measure = if_else(ETR_denominator == "ntl_noelse", "Alternative ETR Measure", Measure),
         Measure = if_else(ETR_denominator == "net_tax_liability", "Main ETR Measure", Measure),
         Measure = factor(Measure, levels=c("Main ETR Measure", "Alternative ETR Measure")))

# Export
setwd(paste0(output, "/figures"))
pdf("FA8_ETR_foreigncredit.pdf")
print(ETR_numerator_GRAPH(df.prof, "#92a1cf", 7, "ntl_noelse", "Foreign & Other Tax Credits are excluded"))  #
dev.off()

write.csv(df.prof, file = "FA8_metadata.csv")



print("Output generation is finished")

#########################| 6. TEXT INSERTS |################################# 


## In the median country by number of CIT returns, 23,000 firms file the CIT, and thus the top 1\% corresponds to 230 firms 
print(df99 %>% filter(percentile_99.9 == "99") %>% summarise(median = median(n_keep)))

# Difference in ETR across deciles ###
df <- df.stat.decile.usd  %>% filter(Measure =="mean") %>% group_by(country) %>%
  summarize(diff_1_9th = ETR_keep_neg[decile ==9] - ETR_keep_neg[decile == 1])
df

# Across all firms, the average of the country-specific correlation coefficients between revenue and payroll (asset)
corr %>% group_by(Measure) %>% summarise(mean = mean(correlation, na.rm = T))

