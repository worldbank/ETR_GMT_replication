

#######################| 1. LOAD AND MERGE ALL METADATA |################################ 


temp <- list()
temp_nodat <- list()

# Countries where we don't run detailed GMT simulations
ctry_name <- country_name
ctry_code <- country_code


for (i in 1: length(ctry_code)){
  file_path <- paste0(metadata, ctry_code[[i]], "_1pct_revenue_simul.csv")
  if (file.exists(file_path)) {
  df <- read.csv(file_path) %>%
    mutate(country = ctry_code[[i]],
           country_name = ctry_name[[i]])  %>% select(-X)
  temp[[i]] <- df 
 
  df_nodat <- read.csv(paste0(metadata, ctry_code[[i]], "_1pct_revenue_simul_nodat.csv")) %>%
    mutate(country = ctry_code[[i]],
           country_name = ctry_name[[i]]) %>% select(-X)
  temp_nodat[[i]] <- df_nodat 
  } else {
    message(paste("File not found:", file_path))
  }
}


df_all <- c()
df_all_nodat <- c()

if (length(temp) > 0) {
  df_all <- do.call("rbind", temp) %>% 
  select(country, country_name, share_less15, change_built, ETR_below15, profit_share15_all, restriction, n_below15) #%>% 
}

if (length(temp_nodat) > 0) {
  df_all_nodat <- do.call("rbind", temp_nodat) %>% 
  select(country, country_name, share_less15, change_built, ETR_below15, profit_share15_all, restriction, n_below15) 
}

df_all_nodat_prof <- df_all_nodat  %>%  filter(restriction == "Profitable") %>% select(country, n_below15)

# Export data
write.csv(df_all_nodat_prof, paste0(metadata, "F5_Data_Below15.csv"))


### Import STR 
setwd(metadata)
df.STR <- readRDS("df.ETR.size.p.RDS") %>% 
  group_by(country) %>%
  summarize(max_STR = max(STR, na.rm = T))

df.STR$country <- reorder(df.STR$country, df.STR$max_STR) 

df_all <- left_join(df_all, df.STR, by = c("country")) 

df_all_nodat <- left_join(df_all_nodat, df.STR, by = c("country"))


# Graph themes: 
# Include theme, percentile function to add visual space for the top 1%, and rename groups of firms
setwd(aux_source)
source("ggtheme.R")
theme_set(theme_niwot())



#########################| 2. FIGURE 5  |################################# 
# Scope and Tax Revenue Potential of a 15% Domestic Minimum Tax on the 1% Largest Firms

# - panel a. Number of firms in top 1% that pay less than 15% in Effective Corporate Income Tax
# - panel b. conditional on being below 15%, average ETR 
# - panel c. conditional on being below 15%, share of profit
# - panel d. simulation top 1 pct

setwd(paste0(output, "/figures"))


# Profitable firms:
df <- df_all %>% filter(restriction == "Profitable") %>% 
  select(country, country_name, share_less15, change_built, ETR_below15, profit_share15_all, max_STR) %>% 
  mutate(across(where(is.numeric), round, 1))

# Profitable firms:
df_nodat <- df_all_nodat %>% filter(restriction == "Profitable") %>% 
  select(country, country_name, share_less15, change_built, ETR_below15, profit_share15_all, max_STR) %>% 
  mutate(across(where(is.numeric), round, 1))



#### + Figure 5 PANEL a: Share of firms in top 1% with ETR below 15% #### 

# Panel a.
pdf("F5_a_share_less15_nodat.pdf")
dataset <- df_nodat
dataset$country_name <- reorder(dataset$country_name, dataset$max_STR) 
dataset <- dataset %>% mutate(label = as.character(share_less15),
                              max_STR = paste0("STR=", max_STR, "%"))
p <- ggplot(data = dataset, aes(x = share_less15, 
                                y = country_name, group = max_STR, label = max_STR)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#00AFBB") +
  geom_text(aes(label = label), hjust = -.2, size = 5.5)
p <- p + labs( 
  x = "\nShare of top 1% firms with ETR<15%", y = "")  
p <- p + scale_x_continuous(limits = c(0, 70)) +
  theme(axis.text = element_text(size = 16),   panel.grid.major.y = element_blank(),
        axis.title =  element_text(size = 16, margin = margin(r = 10))) 
p <- p + guides(shape = guide_legend(""), color = guide_legend(""), fill = guide_legend(""),
                linetype = guide_legend(""), label = guide_legend("")) 

p <- p +   stat_brace(rotate = 90,  width = 3, outerstart = 53) +
  stat_bracetext(rotate = 90,  width = 0.5, outerstart = 58, size = 4)

print(p)
dev.off()


#### + Figure 5 PANEL b: conditional on being below 15%, average ETR  #### 

ETR_lower15_GRAPH_b <- function(dataset, x, x_title, color, upperbound){
  dataset$x <- f_eval(~uq(x), data = dataset)
  dataset$country_name <- reorder(dataset$country_name, dataset$max_STR) 
  dataset <- dataset %>% mutate(label = as.character(x))
  p <- ggplot(data = dataset, aes(x = x, 
                                  y = country_name)) +
    geom_bar(stat = "identity", position = "dodge", fill = color) +
    geom_text(aes(label = label), hjust = -.2, size = 5.5)
  p <- p + labs( 
    x = x_title, y = "")  
  p <- p + scale_x_continuous(limits = c(0, upperbound)) +
    theme(axis.text = element_text(size = 16),   panel.grid.major.y = element_blank(),
          axis.title =  element_text(size = 16, margin = margin(r = 10))) 
  p <- p + guides(shape = guide_legend(""), color = guide_legend(""), fill = guide_legend(""),
                  linetype = guide_legend(""), label = guide_legend("")) 
  p <- p  + 
    geom_vline(xintercept = 15, linetype = "dashed", color = "grey", size = 1) 
  print(p)
  
}

pdf("F5_b_ETR_below15_nodat.pdf")
ETR_lower15_GRAPH_b(df_nodat, ~ETR_below15, "\nMean(ETR) conditional on ETR<15%", "#E7B800", 15) 
dev.off()

#### + Figure 5 PANEL c: conditional on being below 15%, share of profit  #### 

# Graph: 
ETR_lower15_GRAPH_c <- function(dataset, x, x_title, color, upperbound){
  dataset$x <- f_eval(~uq(x), data = dataset)
  dataset$country_name <- reorder(dataset$country_name, dataset$max_STR) 
  dataset <- dataset %>% mutate(label = as.character(x))
  p <- ggplot(data = dataset, aes(x = x, 
                                  y = country_name)) +
    geom_bar(stat = "identity", position = "dodge", fill = color) +
    geom_text(aes(label = label), hjust = -.2, size = 5.5)
  p <- p + labs( 
    x = x_title, y = "")  
  p <- p + scale_x_continuous(limits = c(0, upperbound)) +
    theme(axis.text = element_text(size = 16),   panel.grid.major.y = element_blank(),
          axis.title =  element_text(size = 16, margin = margin(r = 10))) 
  p <- p + guides(shape = guide_legend(""), color = guide_legend(""), fill = guide_legend(""),
                  linetype = guide_legend(""), label = guide_legend("")) 
  p <- p  
  print(p)
  
}

pdf("F5_c_profit_less15_nodat.pdf")
ETR_lower15_GRAPH_c(df_nodat, ~profit_share15_all, "\nProfit cond. ETR<15%, as share of total ", "#FC4E07", 75)
dev.off()



#### + Figure 5 PANEL d: Tax revenue simulation of a 15% minimum tax  #### 

# Graph: 
ETR_lower15_GRAPH_d <- function(dataset, x, x_title, color, upperbound){
  dataset$x <- f_eval(~uq(x), data = dataset)
  dataset$country_name <- reorder(dataset$country_name, dataset$max_STR) 
  dataset <- dataset %>% mutate(label = as.character(x))
  p <- ggplot(data = dataset, aes(x = x, 
                                  y = country_name)) +
    geom_bar(stat = "identity", position = "dodge", fill = color) +
    geom_text(aes(label = label), hjust = -.2, size = 5.5)
  p <- p + labs( 
    x = x_title, y = "")  
  p <- p + scale_x_continuous(limits = c(0, upperbound)) +
    theme(axis.text = element_text(size = 16),   panel.grid.major.y = element_blank(),
          axis.title.y =  element_text(size = 16, margin = margin(r = 10)),
          axis.title.x =  element_text(size = 15, margin = margin(r = 10))) 
  p <- p + guides(shape = guide_legend(""), color = guide_legend(""), fill = guide_legend(""),
                  linetype = guide_legend(""), label = guide_legend("")) +
    
    theme(legend.position = "none")
  print(p)
  
}


# Panel d.
pdf("F5_d_revchange_nodat.pdf")
ETR_lower15_GRAPH_d(df_nodat, ~change_built, "\nRevenue change with domestic min. tax, share of baseline", "#52854C", 47)
dev.off()


#########################| 3. FIGURE A9  |################################# 
# Scope and Tax Revenue Potential of a 15% Domestic Minimum Tax on the 1%
# Largest Firms with Deferred Tax Assets Taken Into Account


#### + Figure A9 PANEL a: Share of firms in top 1% with ETR below 15% #### 

pdf("FA9_a_share_less15.pdf")
dataset <- df
dataset$country_name <- reorder(dataset$country_name, dataset$max_STR) 
dataset <- dataset %>% mutate(label = as.character(share_less15),
                         max_STR = paste0("STR=", max_STR, "%"))
p <- ggplot(data = dataset, aes(x = share_less15, 
                                y = country_name, group = max_STR, label = max_STR)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#00AFBB") +
  geom_text(aes(label = label), hjust = -.2, size = 5.5)
p <- p + labs( 
  x = "\nShare of top 1% firms with ETR<15%", y = "")  
p <- p + scale_x_continuous(limits = c(0, 70)) +
  theme(axis.text = element_text(size = 16),   panel.grid.major.y = element_blank(),
        axis.title =  element_text(size = 16, margin = margin(r = 10))) 
p <- p + guides(shape = guide_legend(""), color = guide_legend(""), fill = guide_legend(""),
                linetype = guide_legend(""), label = guide_legend("")) 

p <- p +   stat_brace(rotate = 90,  width = 3, outerstart = 54) +
  stat_bracetext(rotate = 90,  width = 0.5, outerstart = 59, size = 4)
print(p)
dev.off()


#### + Figure A9 PANEL b: conditional on being below 15%, average ETR  #### 

pdf("FA9_b_ETR_below15.pdf")
ETR_lower15_GRAPH_b(df, ~ETR_below15, "\nMean(ETR) conditional on ETR<15%", "#E7B800", 15) 
dev.off()


#### + Figure A9 PANEL c: conditional on being below 15%, share of profit  #### 

# Panel c.
pdf("FA9_c_profit_less15.pdf")
ETR_lower15_GRAPH_c(df, ~profit_share15_all, "\nProfit cond. ETR<15%, as share of total ", "#FC4E07", 75)
dev.off()


#### + Figure A9 PANEL d: Tax revenue simulation of a 15% minimum tax  #### 
pdf("FA9_d_revenue_change.pdf")
ETR_lower15_GRAPH_d(df, ~change_built, "\nRevenue change with domestic min. tax, share of baseline", "#52854C", 47) 
dev.off()




 #########################| 3. TEXT INSERTS |################################# 
 

 # Introduction:
 
 # Across the 16 countries, the mean(median) share of top 1% firms paying an 
 # ETR below 15% is XX%(YY%).
df_all_nodat %>% summarise(mean = mean(share_less15),
                              median = median(share_less15))
 
 # Conditional on paying an ETR below 15%, the mean(median) ETR is only WW%(ZZ%).
df_all_nodat %>% summarise(mean = mean(ETR_below15),
                              median = median(ETR_below15))
 
 # absent behavioral responses, CIT revenue could rise by XX(YY)% in the average (median) country
df_all_nodat %>% summarise(mean = mean(change_built),
                              median = median(change_built))
