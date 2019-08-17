# Preprocessing.R - Polls' behaviour for the EU Parliament Elections 2019
# Author: Mohamed Megahed - Matr.: 589375
# Statistical Programming Languages - SoSe 2019


# Imports ---------------------------------------------------------------------

## Unload packages to reset - do not perform if package versions are conflicting
# detach("package:dplyr")  
# detach("package:stats")  

library(tidyverse)
# library(dplyr) # incl. in tidyverse - used for pipes to manipulate/inspect data
# library(tibble) # incl. in tidyverse - based on data.frame, easier representation
# library(readr) # incl. in tidyverse - to import datasets
library(xml2) # to import datasets from EU website
library(stats) # for statistical calculations

# Global Variables ------------------------------------------------------------

# Sorted as adopted from http://publications.europa.eu/code/pdf/370000en.htm
# Ambiguities:
#             Greece is 'gr' and not 'el' (Hellenic Republic)
#             U.K. is 'gb-gbn' and not 'uk' for Great Britain + Northern Ireland
eu_country_codes <- c("be", "bg", "cz", "dk", "de", "ee", "ie", "gr", "es",
                      "fr", "hr", "it","cy", "lv", "lt", "lu", "hu", "mt", "nl",
                      "at", "pl", "pt", "ro", "si","sk", "fi", "se", "gb-gbn")
eu_country_names <- c("Belgium", "Bulgaria", "Czechia", "Denmark", "Germany",
                      "Estonia", "Ireland", "Greece", "Spain", "France", 
                      "Croatia", "Italy", "Cyprus", "Latvia", "Lithuania", 
                      "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",
                      "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", 
                      "Finland", "Sweden", "United Kingdom") 
# Europarties: EU Parliament Political Groups sorted by affiliation 
#              (extreme left to extreme right)
# source: https://en.wikipedia.org/wiki/European_political_party and 
#   https://election-results.eu/seats-political-group-country/2019-2024/
# 2014 data can be used to compare with new parties, details discussed in paper
europarties_2014 <- c("GUE/NGL", "G/EFA", "S&D", "ALDE", "EPP", "ECR", 
                      "EFDD", "ENF/EAPN", "UnAff", "NI")
# 2019-2024 parliament grouping - updated Aug 2019
europarties_2019 <- c("GUE/NGL", "G/EFA", "S&D", "RE", "EPP", "ECR", "ID", "NI", "UnAff")
# shortened common names for readability
europarties_2014_names <-c ("Eur. United Left - Nordic Green Left", 
                            "Greens / Eur. Free All.", "Socialists & Democrats", 
                            "All. of Liberals and Democrats", 
                            "Eur. People's Party (Chris. Democrats)", 
                            "Eur. Conservatives & Reformists", 
                            "Euro. Freedom & Direct Democracy", 
                            "Nations & Freedom / Eur. All. of People's & Nations", 
                            "Unaffiliated", "Non-Inscrits")
europarties_2019_names <- c("Eur. United Left - Nordic Green Left",
                            "Greens / Eur. Free All.", "Socialists & Democrats",
                            "Renew Europe",
                            "Eur. People's Party (Chris. Democrats)", 
                            "Eur. Conservatives & Reformists", 
                            "Identity and Democracy",
                            "Unaffiliated", "Non-Inscrits")
# positions on political spectrum, https://en.wikipedia.org/wiki/Political_spectrum
# adopted from Table in https://en.wikipedia.org/wiki/European_political_party
# a first attempt to quantify orientations of 
#                 eurofederalists, ECR's big tent, eurosceptic-populist + brexit party
# analysis of political orientations of EU Parties on http://europeelects.eu
pol_positions_2014 <- c("left-wing", "green", "centre-left", "liberal", 
                        "centre-right", "national-conservative", "eurosceptic-populist",
                        "right-wing", "unaffiliated", "non-inscrit")
pol_positions_2019 <- c("far-left/left-wing", "centre-left/left-wing", "centre-left", 
                        "centre", "centre-right", "centre-right/right-wing", 
                        "right-wing/far right")
# number of seats each country occupies in the EU Parliament 
# sorted (as above) according to table in http://publications.europa.eu/code/pdf/370000en.htm
# source: https://en.wikipedia.org/wiki/Apportionment_in_the_European_Parliament
mep_seats_by_country <- c(21, 17, 21, 13, 96, 6, 11, 21, 54, 74, 11, 73, 6, 8, 
                          11, 6, 21, 6, 26, 18, 51, 21, 32, 8, 13, 13, 20, 73)

# labeling vectors ------------------------------------------------------------
names(eu_country_codes) <- eu_country_names
names(europarties_2014) <- europarties_2014_names
names(europarties_2019) <- europarties_2019_names
names(mep_seats_by_country) <- eu_country_codes

# global stats  ---------------------------------------------------------------
count_mep_seats <- sum(mep_seats_by_country)
count_eu_countries <- as.numeric(length(eu_country_codes))
count_europarties_2014 <-as.numeric(length(europarties_2014)-1) # excl UnAff
count_europarties_2019 <-as.numeric(length(europarties_2019)-1) # excl UnAff

#due to inconsistencies in Dataset, these countries will not be considered
skipped_country_codes <- c("be", "el", "cr", "gr", "uk", "gb","gbe","gb-gbe", "gb-gbn")

# -----------------------------------------------------------------------------
# Importing dataset  ----------------------------------------------------------
# run from here every time you manipulate code to avoid tibble duplicates

# depending on where project is run, type your own path.
# As files are packed into one project, this step was not necessary
## setwd("path") 
print(paste("Project Working Directory: ", getwd()))

# import all country CSVs from EOPaOD into one big list of tibbles (data frames)
import_path_prefix <- "datasets/"
# initializing lists to store data frames of poll data and iterate over it by country
# init multiple vars at once instead of an extra line for each variable
# explaining variable functionality further below with "VARNAME: <var>"
# VARNAME: polls_by_ctry_list is a list of lists with poll data for each country
# VARNAME: results_by_ctry_list has official results from election-results.eu
# VARNAME: summary_by_country has the common part accross all CSVs (first part)
# VARNAME: parties_by_country has parties-by-country data of the tibble (second part)
# VARNAME: parties is a list of parties in each country
polls_by_ctry_list <- results_by_ctry_list <-
  summary_by_country <- parties_by_country <- parties <- list() 
# VARNAME: eopaod_static_data is a tibble with all countries' static data
# it is more logical to split data here for preprocessing then add it by country 
# in '02description.R'. Otherwise, we would have to put all parties from each 
# country altogether in one huge table rendering it less descriptive 
# since e.g. bulgarian voters cannot vote in the Netherlands etc.
eopaod_static_data <- data.frame()
count <- 1 
for(country_suffix in eu_country_codes){
  # skip data frames in EOPaOD folder not present in proper format
  if(!country_suffix %in% skipped_country_codes){
    count <- count + 1
    # file path for EOPaOD polls
    # add "-N" or "-E" before ".csv" to load national or european results only
    import_path_polls <- paste(import_path_prefix, "eopaod-master/docs/", 
                               country_suffix, ".csv", sep = "")
    # file path for Election Results
    import_path_results <- paste(import_path_prefix, "eu_results/election_results/",
                                 country_suffix, ".csv", sep = "")
    
    # using readr::read_csv as a better version of read.csv ()
    # alt. compact way to define column types: c, i, n, d, l, f, D, T, t, ?, _/-
    # .default eliminates '%' in 'Precision', 'Participation' and parties  '%'
    polls_by_ctry_list <- c(
      polls_by_ctry_list, lapply(import_path_polls, read_csv, 
                                 na = c("", "NA", "N.A.", "NaN", "Not Available"),
                                 col_types = cols(
                                   `Polling Firm` = col_factor(),
                                   `Commissioners` = col_factor(),
                                   `Fieldwork Start` = col_date(),
                                   `Fieldwork End` = col_date(),
                                   `Scope` = col_factor(),
                                   `Sample Size Qualification` = col_factor(),
                                   `Participation` = col_number(),
                                   .default = col_number()
                                   )
                                 )
      )
    # alternatively use read.csv(..., row.names = NULL) in order not to
    #               collide with some files like 'cy.csv'
    print(paste(
      "imported", names(eu_country_codes[count]), "from", import_path_polls)) 
    
    results_by_ctry_list <- c(
      results_by_ctry_list, lapply(import_path_results, read_delim,
                                   delim = ";",
                                   col_types = cols(
                                     `GROUP_ID` = col_factor(),
                                     `SEATS_TOTAL` = col_number(),
                                     `SEATS_PERCENT_EU` = col_number(),
                                     `UPDATE_STATUS` = col_factor(),
                                     `UPDATE_TIME` = col_datetime()
                                   )
      )
    )
  }
}
# each tibble gets the label of the respective country
names(polls_by_ctry_list) <- 
  eu_country_codes[!eu_country_codes %in% skipped_country_codes]
names(results_by_ctry_list) <-
  eu_country_codes[!eu_country_codes %in% skipped_country_codes]
#reset counter to avoid errors in reruns
count <- NULL

# initialize parties_by_ctry_list and eopaod_static_data via polls_by_ctry_list
# add country code in first row in both structures
for(country_suffix in eu_country_codes) {
  if(!country_suffix %in% skipped_country_codes) {
    print(paste(
      "split tables for", names(eu_country_codes[eu_country_codes==country_suffix])))
    
    summary_by_country[[country_suffix]] <- polls_by_ctry_list[[country_suffix]][1:9]
    summary_by_country[[country_suffix]] <- 
      add_column(summary_by_country[[country_suffix]], 
                 country = country_suffix, 
                 .before = 1)
    summary_by_country[[country_suffix]] <- 
      add_column(summary_by_country[[country_suffix]],
                 duration = summary_by_country[[country_suffix]]$`Fieldwork End` 
                 - summary_by_country[[country_suffix]]$`Fieldwork Start`, 
                 .before = 6)
    # Toggle show/hide
    ## print(str(summary_by_country[[country_suffix]]))
    
    # consolidate "first part" to generate one big data frame of all countries
    eopaod_static_data <- rbind(
      eopaod_static_data, summary_by_country[[country_suffix]])
    
    # initialize parties_by_country
    parties_by_country[[country_suffix]] <- 
      polls_by_ctry_list[[country_suffix]][-(1:9)]
    parties_by_country[[country_suffix]] <- 
      add_column(parties_by_country[[country_suffix]],
                 country = country_suffix, 
                 .before = 1)
    # remove "%" then convert all but first row to numbers to match a data format R can use
    parties_by_country[[country_suffix]] <- lapply(
      parties_by_country[[country_suffix]], gsub, pattern = "%", replacement='' )
    parties_by_country[[country_suffix]] <- lapply(
      parties_by_country[[country_suffix]][-1], as.numeric)
    # Toggle show/hide
    # writeLines(paste("\n", str(parties_by_country[[country_suffix]]), "\n"))
    # initialize parties
    parties[[country_suffix]] <- names(parties_by_country[[country_suffix]])
    
    #add duration to polls_by_ctry_list
    polls_by_ctry_list[[country_suffix]] <- 
      add_column(polls_by_ctry_list[[country_suffix]],
                 duration = polls_by_ctry_list[[country_suffix]]$`Fieldwork End` 
                 - polls_by_ctry_list[[country_suffix]]$`Fieldwork Start`, 
                 .before = 6)
    
  }
}







# ---- polls by country heya el mosta2bal..hanboss f eopaod_static_data w nesalla7 fel etnein

# check eopaod_static_data for anomalies / errors then correct them in both ------------------------------------
## eopaod_static_data contains the non-party-dependent statistics of all entries from all Member States

eopaod_static_data$country <- factor(eopaod_static_data$country)

# correct $duration------------------------------------------------------------
min(eopaod_static_data$duration)
median(eopaod_static_data$duration)
max(eopaod_static_data$duration)
# examine duration anomalies, assuming these are errors
eopaod_static_data[eopaod_static_data$duration < 0, ]
# correct negative differences as these are not possible. 
# We conclude this happened because of data entry typos 
eopaod_static_data[eopaod_static_data$duration < 0, ][1,]$`Fieldwork End` <- 
  as.Date("2019-05-08") # %M typo
eopaod_static_data[eopaod_static_data$duration < 0, ][2,]$`Fieldwork End` <- 
  as.Date("2019-04-01") # since 2019 is not a leap year, we assume the typo is %M
eopaod_static_data[eopaod_static_data$duration < 0, ][3,]$`Fieldwork End` <- 
  as.Date("2019-05-17") # %Y typo
eopaod_static_data[eopaod_static_data$duration < 0, ][4,]$`Fieldwork End` <- 
  as.Date("2019-05-02") # %Y typo
# correct durations 
# Warning: corrections happen only in this table and not in the polls_by_ctry!
eopaod_static_data$duration <- 
  eopaod_static_data$`Fieldwork End` - eopaod_static_data$`Fieldwork Start`



# examine commisioners
# TODO check for synonyms if necessary - details in paper






# stats -------------------------------------------------------------

str(eopaod_static_data)
summary(eopaod_static_data)

# variables to use in graphs later
polling_firms_summary <- summary(eopaod_static_data$`Polling Firm`)
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



#TODO reveal the NAs and find a reason / workaround ------------------------------------------------

## Germany
#TODO flag if CDU/CSU is joined?
for(i in 1 : length(parties_by_country$de$Union)){
  if(is.na(parties_by_country$de$Union[i]) && ! is.na(parties_by_country$de$CDU[i])){
    parties_by_country$de$Union[i] <- parties_by_country$de$CDU[i]
  } else if(is.na(parties_by_country$de$Union[i]) && ! is.na(parties_by_country$de$CSU[i])){
    parties_by_country$de$Union[i] <- parties_by_country$de$CSU[i]
  }
}
 



# assign europarties and political orientation ------------------------------------------------------
# <div class="flourish-embed flourish-survey" data-src="visualisation/143818"></div><script src="https://public.flourish.studio/resources/embed.js"></script>
# using a data.frame that contains lists for each cell
# party_repr_in_eup <- list of vectors( , nrow = count_eu_countries)

# europarties_2014_members <- (europarties_2014)
# europarties_2014_members["GUE/NGL"] <-  c( NA, "NddA", NA, NA, NA, NA, NA, NA, NA, NA)
# europarties_2014_members["bg"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)






# party_political_aff <- left, right, etc -----------------------------------------------------------


class(polls_by_ctry_list)
str(polls_by_ctry_list)
summary(polls_by_ctry_list)
head(polls_by_ctry_list)
tail(polls_by_ctry_list)
View(polls_by_ctry_list)



# Read XML into tables -------------
# data adopted from: https://data.europa.eu/euodp/data/dataset/members-of-the-european-parliament - see: http://www.europarl.europa.eu/meps/en/full-list.html
# purpose of this table is similar to http://www.europarl.europa.eu/meps/en/search/table or http://www.europarl.europa.eu/meps/en/search/chamber

meps_df <- data.frame("mep_name"=character(), "mep_country"=factor(), "mep_europarty"=factor(), "mep_natparty"=factor(), stringsAsFactors = FALSE)# initialize the variable to store the MEPs
meps_xml <- read_xml("datasets/vars/meps.xml")
for ( i in 1:length(xml_children(meps_xml))) {
  mep_added <- data.frame( 
    mep_name = xml_text(xml_child(xml_child(meps_xml, i), 1 )),
    mep_country = xml_text(xml_child(xml_child(meps_xml, i), 2 )), # needs to match a code in names(eu_country_Code)
    mep_europarty = xml_text(xml_child(xml_child(meps_xml, i), 3 )), # needs to match a europarty
    mep_natparty = xml_text(xml_child(xml_child(meps_xml, i), 5 )), # needs to match a national party / or be other!
    stringsAsFactors = FALSE)
  meps_df <- rbind(meps_df, mep_added)
  mep_added <- NULL
}
meps_df[] <- lapply(meps_df[], function(x) type.convert(as.factor(x)))
meps_df$mep_name <- as.character(meps_df$mep_name)

# TODO: Performance - data frame is not as good as matrix, for loop is worse than sapply. altogether taking here about 16 sec!

## TODO: rbind does not read into matrix. look here instead: 
# http://www.win-vector.com/blog/2015/07/efficient-accumulation-in-r/
# https://www.c-sharpcorner.com/article/matrix-in-r-adding-rows47columns-and-accessing-elements-by-name-in-matrix-i/
# http://www.r-tutor.com/r-introduction/matrix/matrix-construction
#
# meps_matrix <- matrix(c("mep_name", "mep_country", "mep_europarty", "mep_natparty"), ncol = 4)
# meps_xml <- read_xml("datasets/meps.xml")
# 
# for ( i in 1:length(xml_children(meps_xml))) {
#   mep_vec <- c(xml_text(xml_child(xml_child(meps, i), 1 )),
#              xml_text(xml_child(xml_child(meps, i), 2 )),
#              xml_text(xml_child(xml_child(meps, i), 3 )),
#              xml_text(xml_child(xml_child(meps, i), 5 ))
#              )
#   # sapply(meps_matrix, rbind, mep_vec)
#   rbind(meps_matrix, mep_vec)
# }
#
#
# meps_df <- function(nRow,nCol) {
#   d <- lapply(seq_len(nRow),function(i) {
#     ri <- mkRow(nCol)
#     data.frame(ri, stringsAsFactors=FALSE)
#   })
#   do.call(rbind, d)
# }



# we want to model a structure that contains all Europarties
#  in each europarty you want to see the national parties and which country they come from









# TODO create a 'europarty_data' that maps each country's nat party to a europarty such that the new table can join 1:1 with eopaod_static_data

# for that we need a list of each nat party's europarty grouping
# this we have indirectly in meps_df










# TODO use levels() in pol_positions

# summarize kol el polls by sherka, by country -------------------------------------------------------
# shouf yougov 3malet kam wa7da w fih patterns/trends fiha?
# this is no longer preprocessing
# subgroup by bigger countries


# DONE zabbat-hom el awwel marra wa7da ba3dein efselhom

# how many seats with respect to how many countries - and the way this is changing with brexit seats redistributed
# apply on 2014 poll if possible

# fel parties_by_country e3mel vector esmo EPP, ..., we include fih [parties_by_country][index of party]



#TODO: special treatment for exception countries (e.g. Belgium)



##TODO: access data.frame tables via: data_list[['eu']]$...


## TODO: put common part into data table with one big list incl. country
## TODO: then take out the party names [9:-1] and put them into a political affiliation

# TODO: filter scope to european/national
#       replace NAs by means/medians
#       type conversions         - as list of data frames
#       variable transformations - remove percent





# TODO: derive party names from col names
# 
# at_parties <- c("GRÜNE", "PILZ", "SPÖ", "NEOS", "ÖVP", "FPÖ")
# names(at_parties) <- c("Die Grüne Alternative", "Liste Peter Pilz", "Sozialdemokratische Partei Österreichs", "Das Neue Österreich", "Österreichische Volkspartei", "Freiheitliche Partei Österreichs")
# 
 
# CDU/CSU -> Union - needs to be summarized in table - die Partei is left wing despite being NI
## their real name, Partei für Arbeit, Rechtsstaat, Tierschutz, Elitenförderung und basisdemokratische Initiative, has been shortened for convenience
# de_parties <- c("Linke", "PARTEI", "Grüne", "ÖDP", "Piraten", "SPD", "FDP", "CDU/CSU",  "LKR", "AfD", "B", "NPD")
# names(de_parties) <- c("Die Linke", "Die PARTEI", "Bündnis90/Die Grünen", 
#                        "Ökologisch-Demokratische Partei", "Piratenpartei Deutschlands", "Sozialdemoratische Partei Deutschlands",
#                        "Freie Demokraten", "Christlich Demokratische Union", "Liberal-Konservative Reformer", "Alternative für Deutschland", 
#                        "Die Blaue Partei", "Nationaldemokratische Partei Deutschlands" )

# Macron is making a new party that was not included in the polls
# fr_parties <- c("FI.fr", "PCF", "PS", "EELV", "LEM", "MoDem", "LR", "DLF", "RN")
# names(fr_parties) <- c("La France Insoumise", "Parti Communiste Français",  "Parti Socialiste", "Europe Écologie - Les Verts",
#                        "La République En Marche", "Mouvement Democrate", "Les Républicains", "Debout La France", "Rassemblement National" )

#FI not to be confused with the French France Insoumise
## SI not to confuse with french
# it_parties <- c("SI", "PaP", "PD", "+E", "MDP", "FI.it", "FdI", "M5S", "LEGA" )
# names(it_parties) <- c("Sinistra Italiana", "Potere al Popolo", "Partito Democratico", "Più Europa", "Articolo 1", "Forza Italia", "Fratelli d'Italia",
#                        "Movimento 5 Stelle", "Lega")

#spain

#greece

#netherlands


# nach bundesland!!!


# TODO: how to add this check without resulting in too much code?
# if (length(data_list) > count_eu_countries){
#   # reset list to avoid conflicts in reruns
#   data_list <- NULL ; count <- 1
# }





comparison_countries <- c("at", "de", "fr", "it")
comparison_df <- data.frame()







# Function: ------------


# if(DE_data$Sample.Size == "NA" | "Not Available" | "N/A"){
#   DE_data$Sample.Size <- median(DE_data$Sample.Size, na.rm = TRUE)
# }

# 
# DE_data_readable <- DE_data %>%
#   Fieldwork.Start = as.Date(as.character(Fieldwork.Start))
# 
# DE_data_readable




# Election results

