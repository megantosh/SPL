## summaries, filters,  graphs etc

source("01preprocessing.R")


# visualizations ---------------------------
# http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
# https://www.r-graph-gallery.com/map/


# map graph showing size of candidates in each country
summary(meps_df$mep_country)
str(meps_df)

# bar graph showing number of countries in each eurpoarty



summary(meps_df %>%
  select(mep_country, mep_europarty, mep_natparty))
  
#   # group_by(mep_country) %>%
#   summarize(ctry_sum = count(mep_country), eurpoarty_sum = count(mep_europarty))

  # select(mep_country, mep_europarty, mep_natparty) %>%


    
by_polling_firm_nat <- static_data %>%
  filter(Scope == "National") %>%
  group_by(`Polling Firm`) %>%
  summarise()
    
    
    # distribution of sample size
# TODO: how to show NAs
    boxplot(static_data$`Sample Size`, log = "y", col = "bisque", xlab = "Sample size - log(10)")
    boxplot(as.numeric(static_data$duration), col = "bisque", xlab = "Duration")
    
    
    # sample size can use a histogram
    
  #TODO: show all countries  
  barplot(nos_by_country, xlab = "country", ylab = "number of polls", las=2, main = "Polling participation per country")

  # ctry_sample_size <- list()
  # for(i in length(static_data)){
  #   for(country_suffix in eu_country_codes){
  #     if(static_data$country[i] == country_suffix){
  #       print(static_data$`Sample Size`[i])
  #       ctry_sample_size[[country_suffix]] <- c(ctry_sample_size[[country_suffix]], static_data$`Sample Size`[i]) 
  #       ctry_sample_size[country_suffix]
  #     }
  #   }
  # }
    
  

  ctry_sample_size_tot <-   static_data %>%
    group_by(country) %>%
    summarise(sum_sample_size = sum(`Sample Size`, na.rm = TRUE))
  plot(ctry_sample_size_tot, las=2, )
  
  
    ctry_sample_size_nat <-   static_data %>%
      filter(Scope == "National") %>%
      group_by(country) %>%
      summarise(sum_sample_size = sum(`Sample Size`, na.rm = TRUE))
    plot(ctry_sample_size_nat, las = 2)
    
      
    ctry_sample_size_eur <-   static_data %>%
      filter(Scope == "European") %>%
      group_by(country) %>%
      summarise(sum_sample_size = sum(`Sample Size`, na.rm = TRUE))
    plot(ctry_sample_size_eur, las=2)
    
  #TODO map nat and eur into both
    
    
    ggplot(polls_by_ctry_list[['bg']], aes(x = `Fieldwork Start`, y = `Sample Size`)) + geom_point()
    
    test <- polls_by_ctry_list[['bg']] %>%
      # filter(Scope =="National") %>%
      select(`Fieldwork Start`, `Fieldwork End`, -(1:9))
    
    plot(polls_by_ctry_list[[1]][1])
    
  
  
  
  
  
  
  

install.packages('devtools')
devtools::install_github("UrbanInstitute/urbnmapr")


library(tidyverse)
library(urbnmapr)

ggplot() + 
  geom_polygon(data = urbnmapr::, mapping = aes(x = long, y = lat, group = group),
               fill = "grey", color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)
