# Preprocessing.R - Polls' behaviour for the EU Parliament Elections 2019
# Author: Mohamed Megahed - Matr.: 589375
# Statistical Programming Languages - SoSe 2019


# Imports ---------------------------------------------------------------------

## TODO: remove if not required
## Unload packages to reset
# detach("package:dplyr")  
# detach("package:stats")  

library(tidyverse)
# library(dplyr) # incl. in tidyverse - used for pipes to manipulate/inspect data
# library(tibble) # incl. in tidyverse - based on data.frame, easier representation
# library(readr) # incl. in tidyverse - to import datasets
library(xml2) # to import datasets from EU website
library(stats) # for statistical calculations

# Global Variables -------------------------------------------------------------

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
#         https://election-results.eu/seats-political-group-country/2019-2024/
# 2014 data can be used to compare with new parties
europarties_2014 <- c("GUE/NGL", "G/EFA", "S&D", "ALDE", "EPP", "ECR", "EFDD", 
                      "ENF/EAPN", "NI", "UnAff")
# updated Aug 2019
europarties_2019 <- c("GUE/NGL", "G/EFA", "S&D", "RE","EPP", "ECR", "ID", "NI")
# shortened common names for readability
europarties_2014_names <-c ("Eur. United Left - Nordic Green Left", 
                            "Greens / Eur. Free All.", "Socialists & Democrats", 
                            "All. of Liberals and Democrats", 
                            "Eur. People's Party (Chris. Democrats)", 
                            "Eur. Conservatives & Reformists", 
                            "Euro. Freedom & Direct Democracy", 
                            "Nations & Freedom / Eur. All. of People's & Nations", 
                            "Non-Inscrits", "Unaffiliated")
europarties_2019_names <- c("Eur. United Left - Nordic Green Left",
                            "Greens / Eur. Free All.", "Socialists & Democrats",
                            "Renew Europe",
                            "Eur. People's Party (Chris. Democrats)", 
                            "Eur. Conservatives & Reformists", 
                            "Identity and Democracy",
                            "Non-Inscrits", "Unaffiliated")
# extended names for formaility [use only as alternative representation]
europarties_2014_names_ext <- c("Confederal Group of the European United Left - Nordic Green Left", "Group of the Greens/European Free Alliance",
                                "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament",
                                "Group of the Alliance of Liberals and Democrats for Europe", "Group of the European People's Party (Christian Democrats)",
                                "European Conservatives and Reformists Group", "Europe of Freedom and Direct Democracy Group", 
                                "Europe of Nations and Freedom Group (- European Alliance of Peoples and Nations)", 
                                "Non-Inscrits (Independents, Nazis, Satirists, Communists)", "Unaffiliated")
# positions on the political spectrum - https://en.wikipedia.org/wiki/Political_spectrum
# derrived from analysis of political orientations of EU Parties (EUP) on http://europeelects.eu
pol_positions_2014 <- c("left-wing", "green", "center-left", "liberal", 
                        "centre-right", "national-conservative", "eurosceptic-populist", "right-wing", "unaffiliated", 
                        "non-inscrit")
pol_positions_2019 <- c("left-wing", "green", "center-left", "UnAff1: eurofederalist", "liberal", 
                        "centre-right", "national-conservative", "eurosceptic-populist", "right-wing", "unaff2: M5S", "UnAff3: BREXIT",
                        "non-inscrit")
# number of seats each country occupies in the EU Parliament - sorted (as above) according to table in http://publications.europa.eu/code/pdf/370000en.htm
# source: https://en.wikipedia.org/wiki/Apportionment_in_the_European_Parliament
mep_seats_by_country <- c(21, 17, 21, 13, 96, 6, 11, 21, 54, 74, 11, 73, 6, 8, 11, 6, 21, 6, 26, 18, 51, 21, 32, 8, 13, 13, 20, 73)

# labeling vectors --------------------------------------------------------------
names(eu_country_codes) <- eu_country_names
names(europarties_2014) <- europarties_2014_names
names(europarties_2019) <- europarties_2019_names
names(mep_seats_by_country) <- eu_country_codes

# global stats  -----------------------------------------------------------------
count_mep_seats <- sum(mep_seats_by_country)
count_eu_countries <- as.numeric(length(eu_country_codes))
count_europarties_2014 <-as.numeric(length(europarties_2014))

skipped_eu_country_codes <-c("be", "el", "cr", "gr", "uk", "gb","gbe","gb-gbe", "gb-gbn")


# Importing dataset  -------------------------------------------------------------
print(paste("Project Working Directory: ", getwd()))

# import all country CSVs into one big list of tibbles (data frames)
import_path_prefix <- "datasets/"
# init list to store data frames of poll data and iterate over it by country - warning: run these lines every time you manipulate code to avoid duplicates
### initializing multiple vars at once instead of an extra line for each variable. explaining variable functionality further below
polls_by_ctry_list <- results_by_ctry_list <- summary_by_country <- parties_by_country <- parties <- list() 
count <- 1 
for(country_suffix in eu_country_codes){
  # skips in case you want to exclude a certain data frame not currently present (in proper format) in folder 
  if(!country_suffix %in% skipped_eu_country_codes){
    count <- count + 1
    # add "-N" or "-E" before ".csv" for national or european subtables
    import_path_polls <- paste(import_path_prefix, "eopaod-master/docs/", country_suffix, ".csv", sep = "")
      # VARNAME: polls_by_ctry_list # A list of lists containng poll data for each country
    # using readr::read_csv as a better version of read.csv () - caution: outputs tibble, which inherit from data.frame
    # alt. compact way to define column types: c = char, i = int, n = num, d = dbl, l = bool, f = factor, D = date, T = date/time, t = time, ? = guess, _/- = skip col.
    polls_by_ctry_list <- c(polls_by_ctry_list, lapply(import_path_polls, read_csv,  na = c("", "NA", "N.A.", "NaN", "Not Available"),
                                     col_types = cols(`Polling Firm` = col_factor(),
                                                      `Commissioners` = col_factor(),
                                                      `Scope` = col_factor(),
                                                      `Sample Size Qualification` = col_factor(),
                                                      `Precision` = col_number(), # converts to char bec of '%' in the number
                                                      `Participation` = col_number()
                                       )))
    # alternatively use the line below: read.csv(..., row.names = NULL) in order not to collide with some files like 'cy.csv'
    ## data_list <- c(data_list,lapply(import_path, read.csv, row.names = NULL))
    # TODO maybe add here as.data.frame([count])
    # each data.frame gets the label of the respective country
    # names(data_list[[count]]) <- eu_country_codes[count+1] #+1 for ignoring Belgium
    print(paste("imported", names(eu_country_codes[count]), "from", import_path_polls)) #,"into", names(data_list[[count]])))
    
    
    import_path_results <-  paste(import_path_prefix, "eu_results/election_results/", country_suffix, ".csv", sep = "")
    # VARNAME: results_by_ctry_list # official results from election-results.eu
    results_by_ctry_list <- c(results_by_ctry_list, lapply(import_path_results, read_csv2,
                                                       col_types = cols(`GROUP_ID` = col_factor(),
                                                                        `SEATS_TOTAL` = col_number(),
                                                                        `SEATS_PERCENT_EU` = col_number(),
                                                                        `UPDATE_STATUS` = col_factor(),
                                                                        `UPDATE_TIME` = col_datetime()  
                                                                        )))
  
  }
}
names(polls_by_ctry_list) <- eu_country_codes[!eu_country_codes %in% skipped_eu_country_codes]
count <- NULL #reset counter to avoid errors in reruns
static_data <- data.frame()

# extract first part of the tibble (common part accross all CSVs) into summary_by_country
# extract second part of the tibble (parties-dependent part) into parties_by_country
# add country code in first row in both structures
for(country_suffix in eu_country_codes){
  if(!country_suffix %in% skipped_eu_country_codes){
    print(paste("split tables for", toupper(names(eu_country_codes[eu_country_codes==country_suffix]))))
    summary_by_country[[country_suffix]] <- polls_by_ctry_list[[country_suffix]][1:9]
    summary_by_country[[country_suffix]] <- add_column(summary_by_country[[country_suffix]], country = country_suffix, .before = 1)
    summary_by_country[[country_suffix]] <- add_column(summary_by_country[[country_suffix]], duration =
                                                         summary_by_country[[country_suffix]]$`Fieldwork End` 
                                                       - summary_by_country[[country_suffix]]$`Fieldwork Start`, .before = 6)
    # Toggle show/hide
    # print(str(summary_by_country[[country_suffix]]))
    static_data <- rbind(static_data, summary_by_country[[country_suffix]]) # consolidate static part to generate one big data.frame
    

    parties_by_country[[country_suffix]] <- polls_by_ctry_list[[country_suffix]][-(1:9)] # TODO:ASK as.data.frame is overriden. why?
    parties_by_country[[country_suffix]] <- add_column(parties_by_country[[country_suffix]], country = country_suffix, .before = 1)
    #TODO:ASK: use instead: lapply(data_list[[country_suffix]], add_column, parties_by_country[[country_suffix]], country = country_suffix)
    parties_by_country[[country_suffix]] <- lapply(parties_by_country[[country_suffix]], gsub, pattern = "%", replacement='' ) # remove "%"
    #TODO: Produces warning inside file: NAs introduced by coercion - outside it produces error?
    parties_by_country[[country_suffix]] <- lapply(parties_by_country[[country_suffix]][-1], as.numeric) # convert all but first row to numbers
    # Toggle show/hide
    # writeLines(paste("\n", str(parties_by_country[[country_suffix]]), "\n"))
    parties[[country_suffix]] <- names(parties_by_country[[country_suffix]])
    
  }
}


# check static_data for anomalies / errors -------------------------------------------------
## static_data contains the non-party-dependent statistics of all entries from all Member States

static_data$country <- factor(static_data$country)

  # correct $duration----------------
min(static_data$duration)
median(static_data$duration)
max(static_data$duration)

static_data[static_data$duration < 0, ]
#negative differences are not possible. We conclude this happened because of data entry typos 
    # static_data %>%
    #   mutate(static_data[static_data$duration < 0, ][1,]$`Fieldwork End` <- as.Date("2019-05-08")) %>% # %M typo
    #   static_data
static_data[static_data$duration < 0, ][1,]$`Fieldwork End` <- as.Date("2019-05-08") # %M typo
static_data[static_data$duration < 0, ][2,]$`Fieldwork End` <- as.Date("2019-04-01") # since 2019 is not a leap year, we assume the typo is %M
static_data[static_data$duration < 0, ][3,]$`Fieldwork End` <- as.Date("2019-05-17") # %Y typo
static_data[static_data$duration < 0, ][4,]$`Fieldwork End` <- as.Date("2019-05-02") # %Y typo
static_data$duration <- static_data$`Fieldwork End` - static_data$`Fieldwork Start`









# stats -------------------------------------------------------------

str(static_data)
summary(static_data)

# variables to use in graphs later
polling_firms_summary <- summary(static_data$`Polling Firm`)
commissioners_summary <- summary(static_data$Commissioners)


min(static_data$`Sample Size`, na.rm = TRUE)
max(static_data$`Sample Size`, na.rm = TRUE)
median(static_data$`Sample Size`, na.rm = TRUE)
mean(static_data$`Sample Size`, na.rm = TRUE)

nos_by_country <-summary(static_data$country)

# most recent start date
max(static_data$`Fieldwork Start`)
static_data %>%
  arrange(desc(`Fieldwork Start`))

  #oldest start date
min(static_data$`Fieldwork Start`)
static_data %>%
  arrange((`Fieldwork Start`))#2017 not 2018!

static_data %>%
  arrange(desc(duration))

static_data %>%
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









# TODO create a 'europarty_data' that maps each country's nat party to a europarty such that the new table can join 1:1 with static_data

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

