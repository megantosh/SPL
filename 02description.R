## summaries, filters,  graphs etc

source("01preprocessing.R")
dir.create('figs/')

# stats -----------------------------------------------------------------------

# TODO subsetting not working
for(country_suffix in eu_country_codes) {
  if(!country_suffix %in% skipped_country_codes) {
    europolls_by_ctry_list <- subset(polls_by_ctry_list, polls_by_ctry_list[[country_suffix]]$Scope != "National")
  }
}

class(polls_by_ctry_list)
str(polls_by_ctry_list)
summary(polls_by_ctry_list)
head(polls_by_ctry_list)
tail(polls_by_ctry_list)
View(polls_by_ctry_list)


str(eopaod_static_data)
summary(eopaod_static_data)

#average duration of poll
summary(as.numeric(eopaod_static_data$duration))

# TODO variables to use in graphs later

# to find out which polling firm had the most/least/average polling jobs
polling_firms_summary <- summary(eopaod_static_data$`Polling Firm`)
max(polling_firms_summary)
min(polling_firms_summary)
median(polling_firms_summary)

commissioners_summary <- summary(eopaod_static_data$Commissioners)


min(eopaod_static_data$`Sample Size`, na.rm = TRUE)
max(eopaod_static_data$`Sample Size`, na.rm = TRUE)
median(eopaod_static_data$`Sample Size`, na.rm = TRUE)
mean(eopaod_static_data$`Sample Size`, na.rm = TRUE)

nos_by_country <-summary(eopaod_static_data$country)

# most recent start date
max(eopaod_static_data$`Fieldwork Start`)
eopaod_static_data %>%
  arrange(desc(`Fieldwork Start`))

#oldest start date
min(eopaod_static_data$`Fieldwork Start`)
eopaod_static_data %>%
  arrange((`Fieldwork Start`))#2017 not 2018!

eopaod_static_data %>%
  arrange(desc(duration))

eopaod_static_data %>%
  arrange((duration))




# -----------------------------------------------------------------------------
# Parliament Seats occupation

parliament_seats_by_europarty <- meps_df %>%
  group_by(mep_europarty) %>%
  summarise(
    totalNatparties = sum(mep_weight))

parliament_seats_by_country <- meps_df %>%
  group_by(mep_country) %>%
  summarise(
    totalNatparties = sum(mep_weight))

parliament_seats_by_country_and_europarty <- meps_df %>%
  group_by(mep_europarty, mep_country) %>%
  summarise(
    totalNatparties = sum(mep_weight))






# Summaries and Histograms --------------------------------------------------
# levels of histogram intervals
# breaks_samplesize <- seq(0, max(eopaod_static_data$`Sample Size`, na.rm = TRUE), by = 100)
breaks_samplesize <- seq(0, 1200, by = 100)
breaks_duration <- seq(0, 190, by = 10)
# sample sizes less than this are too inacurate
hist_range_samplesize <- 1000
# polls less than this time are shorter/longer in period than median
hist_range_duration <- 4
# based on country and party-independent variable
summaries_and_hists <- function(country_suffix, hist_var, breaks, hist_range){
  # to Summarize e.g. Sample Size (contained in the polls data frame), hist_var <- "Sample Size"
  print(paste(names(eu_country_codes[eu_country_codes == country_suffix]),"-", hist_var))
  print(summary(polls_by_ctry_list[[country_suffix]][[hist_var]]))
  # Create a histogram showing the distribution of polls 
  # e.g. to discover a sample size below 1000.
  # and save it as image
  png(paste('figs/', country_suffix, "/", country_suffix, "-", hist_var, '.png', sep = ""), width = 642, height = 400, units = "px", res = 105)
  hist(polls_by_ctry_list[[country_suffix]][[hist_var]][polls_by_ctry_list[[country_suffix]][[hist_var]] < hist_range], 
       breaks_samplesize,
       main = paste(hist_var, "in", names(eu_country_codes[eu_country_codes ==country_suffix])),
       xlab = paste (hist_var))
  dev.off()
}






for(country_suffix in eu_country_codes) {
  if(!country_suffix %in% skipped_country_codes) {
    dir.create(paste('figs/', country_suffix, sep = ""))
    # generate a summary and create a histogram file, save it into 'figs'
    summaries_and_hists(country_suffix = country_suffix, hist_var = "Sample Size", breaks = breaks_samplesize, hist_range_samplesize)
    # polls_by_ctry_list[[country_suffix]]$duration <- as.numeric(polls_by_ctry_list[[country_suffix]]$duration)
    # summaries_and_hists(country_suffix = country_suffix, hist_var =  "duration", breaks = breaks_duration, hist_range_duration)
  }
}
# Create a new data frame that contains only polls with a sample size greater than 400: polls2
polls2 <- polls[polls$samplesize > 400, ]




# TODO visualize on map

# TODO visualize on kreis with seats

# Polls Visualizations ---------------------------

plot_party <- function(country_suffix, party, election_result){
  ggplot(polls_by_ctry_list[[country_suffix]], 
         aes_string(x = polls_by_ctry_list[[country_suffix]]$`Fieldwork Start`, 
                    y = party
                    )
         ) + 
    # geom_hline(yintercept=election_result, linetype = "dashed", size=1.5, colour=y) +
    ## trend line
    # geom_smooth(se=FALSE, method="loess", colour=y) +
    geom_point(aes(colour=polls_by_ctry_list[[country_suffix]]$`Polling Firm`, 
                   shape=polls_by_ctry_list[[country_suffix]]$`Polling Firm`), size=2.5) +
    xlab("") +
    ylab("% Votes") # +
    # scale_colour_brewer(palette="Paired") +
    # scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
    # {if(with(polls.100, min(get(party), na.rm=TRUE)) < 4) geom_hline(yintercept=2, linetype = "dashed") }+
    # theme(
    #   plot.margin = unit(rep(0.5, 4), "cm"), plot.background = element_blank(), panel.background = element_blank(),
    #   panel.border = element_blank(), legend.background = element_rect(fill = "transparent", colour = NA),
    #   legend.key = element_rect(fill = "transparent", colour = NA), legend.title = element_blank(),
    #   strip.background = element_rect(fill = "transparent", colour = NA),
    #   panel.grid.major = element_line(linetype = "dotted", colour = "#757575", size = 0.3), panel.grid.minor = element_blank(),
    #   axis.ticks = element_blank(), axis.line = element_line(color = "#FFFFFF", size = 0.3),
    #   plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 15)),
    #   plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 5)),
    #   plot.caption = element_text(size = 10, colour = "#212121", margin = margin(t = 15)),
    #   axis.title = element_text(size = 11, face = "plain"), axis.text = element_text(size = 10, face = "plain"),
    #   legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain")
    # ) 
}




plot( y = polls_by_ctry_list$bg$`United Patriots`,
            x = polls_by_ctry_list$bg$`Fieldwork Start`) 


ggplot( data = polls_by_ctry_list$bg, 
       aes(y = polls_by_ctry_list$bg$`United Patriots`,
           x = polls_by_ctry_list$bg$`Fieldwork Start`, colour = "smooth")) + 
  geom_point(aes(colour=polls_by_ctry_list[[country_suffix]]$Scope #, 
                 # shape=polls_by_ctry_list[[country_suffix]]$`Polling Firm`
                 ), size=2.5) + 
## trend line
  geom_smooth(se=FALSE, method="loess", span = 0.5) 

  

plot_party(country_suffix = "bg", 
           party = polls_by_ctry_list$bg$`Bulgarian Socialist Party`,
           election_result = 5
           )

plot(polls_by_ctry_list$bg$`Fieldwork Start`, polls_by_ctry_list$bg$`Citizens for European Development of Bulgaria`)









boxplot(polling_firms_summary)
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


    
by_polling_firm_nat <- eopaod_static_data %>%
  filter(Scope == "National") %>%
  group_by(`Polling Firm`) %>%
  summarise()
    
    
    # distribution of sample size
# TODO: how to show NAs
    boxplot(eopaod_static_data$`Sample Size`, log = "y", col = "bisque", xlab = "Sample size - log(10)")
    boxplot(as.numeric(eopaod_static_data$duration), col = "bisque", xlab = "Duration")
    
    
    # sample size can use a histogram
    
  #TODO: show all countries  
  barplot(nos_by_country, xlab = "country", ylab = "number of polls", las=2, main = "Polling participation per country")

  # ctry_sample_size <- list()
  # for(i in length(eopaod_static_data)){
  #   for(country_suffix in eu_country_codes){
  #     if(eopaod_static_data$country[i] == country_suffix){
  #       print(eopaod_static_data$`Sample Size`[i])
  #       ctry_sample_size[[country_suffix]] <- c(ctry_sample_size[[country_suffix]], eopaod_static_data$`Sample Size`[i]) 
  #       ctry_sample_size[country_suffix]
  #     }
  #   }
  # }
    
  

  ctry_sample_size_tot <-   eopaod_static_data %>%
    group_by(country) %>%
    summarise(sum_sample_size = sum(`Sample Size`, na.rm = TRUE))
  plot(ctry_sample_size_tot, las=2, )
  
  
    ctry_sample_size_nat <-   eopaod_static_data %>%
      filter(Scope == "National") %>%
      group_by(country) %>%
      summarise(sum_sample_size = sum(`Sample Size`, na.rm = TRUE))
    plot(ctry_sample_size_nat, las = 2)
    
      
    ctry_sample_size_eur <-   eopaod_static_data %>%
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
