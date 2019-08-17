# Preprocessing.R - Polls' behaviour for the EU Parliament Elections 2019
# Author: Mohamed Megahed - Matr.: 589375
# Statistical Programming Languages - SoSe 2019


# Imports ---------------------------------------------------------------------

## TODO: remove if not required
## Unload packages to reset
# detach("package:dplyr")  
# detach("package:stats")  

# library(tidyverse)
library(dplyr)
library(tibble)
library(readr)
library(stats)

# Global Variables -------------------------------------------------------------

# Sorted as adopted from http://publications.europa.eu/code/pdf/370000en.htm
## Greece is 'gr' and not 'el' (Hellenic Republic)   - U.K. is 'gb-gbn' and not 'uk' for Great Britain + Northern Ireland
eu_country_codes <- c("be", "bg", "cz", "dk", "de", "ee", "ie", "gr", "es", "fr", "hr", "it","cy", "lv", "lt", "lu", "hu",
                     "mt", "nl", "at", "pl", "pt", "ro", "si","sk", "fi", "se", "gb-gbn")
eu_country_names <- c("Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia", "Ireland", "Greece", "Spain",
                      "France", "Croatia", "Italy", "Cyprus", "Latvia", "Lithuania", "Luxembourg", "Hungary", "Malta", 
                      "Netherlands", "Austria", "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", 
                      "Sweden", "United Kingdom") 
# Europarties: EU Parliament Political Groups sorted by affiliation (extreme left to extreme right)
# source: https://en.wikipedia.org/wiki/European_political_party
europarties_2014 <- c("GUE/NGL", "G/EFA", "S&D", "ALDE", "EEP", "ECR", "EFDD", "ENF/EAPN", "NI", "UnAff")
# xTmp: Ex tempore (at time of research)   --   ALDE includes french Macron's Renaissance
europarties_2019 <- c("GUE/NGL", "G/EFA", "S&D", "UnAff1 - Volt", "ro_PSD (xTmp S&D)", "ALDE", "ro_ALDE (xTmp)","EEP", 
                      "hu_Fidesz (xTmp EPP)", "ECR","UnAff2 - it_M5S + all.", "EFDD", "UnAff3 - uk_BRExit", "ENF/EAPN", "NI")
# shortened common names for readability
europarties_2014_names <-c ("Eur. United Left - Nordic Green Left", "Greens / Eur. Free All.", "Socialists & Democrats",
                            "All. of Liberals and Democrats", "Eur. People's Party (Chris. Democrats)", "Eur. Conservatives & Reformists", 
                            "Euro. Freedom & Direct Democracy", "Nations & Freedom / Eur. All. of People's & Nations", "Non-Inscrits", "Unaffiliated")
europarties_2019_names <- c("Eur. United Left - Nordic Green Left", "Greens / Eur. Free All.", "Socialists & Democrats",
                            "All. of Liberals and Democrats", "Eur. People's Party (Chris. Democrats)", "Eur. Conservatives & Reformists", 
                            "Euro. Freedom & Direct Democracy", "Nations & Freedom / Eur. All. of People's & Nations", "Non-Inscrits", "Unaffiliated")
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

skipped_eu_country_codes <-c("be", "uk", "el", "cr")


# Importing dataset  -------------------------------------------------------------
print(paste("Project Working Directory: ", getwd()))

# import all country CSVs into one big list of tibbles (data frames)
import_path_prefix <- "datasets/eopaod-master/docs/"
# init list to store data frames and iterate over - warning: use these lines when rerunning code to avoid duplicates
data_list <- summary_by_country <- parties_by_country <- parties <- list() # TODO:ASK is okay to init multiple vars like this or prefer extra line for each to explain var name / manipilate separately etc?
count <- 1 
for(country_suffix in eu_country_codes){
  # skips in case you want to exclude a certain data frame not currently present (in proper format) in folder 
  if(!country_suffix %in% skipped_eu_country_codes){
    count <- count + 1
    # add "-N" or "-E" before ".csv" for national or european subtables
    import_path <- paste(import_path_prefix, country_suffix, ".csv", sep = "")
    # using readr::read_csv as a better version of read.csv () - caution: outputs tibble, which inherit from data.frame
    # compact: c = char, i = int, n = num, d = dbl, l = bool, f = factor, D = date, T = date/time, t = time, ? = guess, _/- = skip col.
    data_list <- c(data_list, lapply(import_path, read_csv, na = c("", "NA", "N.A.", "NaN", "Not Available"),
                                     col_types = cols(`Polling Firm` = col_factor(),
                                       `Commissioners` = col_factor(),
                                       `Scope` = col_factor(),
                                       `Sample Size Qualification` = col_factor(),
                                       `Precision` = col_number(), # converts to char bec of '%'
                                       `Participation` = col_number()
                                       )))
    # alternatively use the line below: read.csv(..., row.names = NULL) in order not to collide with 'cy.csv'
    # data_list <- c(data_list,lapply(import_path, read.csv, row.names = NULL))
    # TODO maybe add here as.data.frame([count])
    #each data.frame gets the label of the respective country
    # names(data_list[[count]]) <- eu_country_codes[count+1] #+1 for ignoring Belgium
    print(paste("imported", names(eu_country_codes[count]), "from", import_path)) #,"into", names(data_list[[count]])))
  }
}
names(data_list) <- eu_country_codes[!eu_country_codes %in% skipped_eu_country_codes]
count <- NULL #reset counter to avoid errors in reruns
static_data <- data.frame()

# extract first part of the tibble (common part accross all CSVs) into summary_by_country
# extract second part of the tibble (parties-dependent part) into parties_by_country
# add country code in first row in both structures
for(country_suffix in eu_country_codes){
  if(!country_suffix %in% skipped_eu_country_codes){
    print(paste("split tables for", toupper(names(eu_country_codes[eu_country_codes==country_suffix]))))
    summary_by_country[[country_suffix]] <- data_list[[country_suffix]][1:9]
    summary_by_country[[country_suffix]] <- add_column(summary_by_country[[country_suffix]], country = country_suffix, .before = 1)
    summary_by_country[[country_suffix]] <- add_column(summary_by_country[[country_suffix]], duration =
                                                         summary_by_country[[country_suffix]]$`Fieldwork End` 
                                                       - summary_by_country[[country_suffix]]$`Fieldwork Start`, .before = 6)
    print(str(summary_by_country[[country_suffix]]))
    static_data <- rbind(static_data, summary_by_country[[country_suffix]]) # consolidate static part to generate one big data.frame
    

    parties_by_country[[country_suffix]] <- data_list[[country_suffix]][-(1:9)] # TODO:ASK as.data.frame is overriden. why?
    parties_by_country[[country_suffix]] <- add_column(parties_by_country[[country_suffix]], country = country_suffix, .before = 1)
    #TODO:ASK: use instead: lapply(data_list[[country_suffix]], add_column, parties_by_country[[country_suffix]], country = country_suffix)
    parties_by_country[[country_suffix]] <- lapply(parties_by_country[[country_suffix]], gsub, pattern = "%", replacement='' ) # remove "%"
    parties_by_country[[country_suffix]] <- lapply(parties_by_country[[country_suffix]][-1], as.numeric) # convert all but first row to numbers
    writeLines(paste("\n", str(parties_by_country[[country_suffix]]), "\n"))
    parties[[country_suffix]] <- names(parties_by_country[[country_suffix]])
    
  }
}

# assign europarties and political orientation ------------------------------------------------------
# <div class="flourish-embed flourish-survey" data-src="visualisation/143818"></div><script src="https://public.flourish.studio/resources/embed.js"></script>
# using a data.frame that contains lists for each cell
# party_repr_in_eup <- list of vectors( , nrow = count_eu_countries)

europarties_2014_members <- (europarties_2014)
europarties_2014_members["GUE/NGL"] <-  c( NA, "NddA", NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["bg"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["cz"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["dk"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["de"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["ee"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["ie"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["gr"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["es"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["fr"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["hr"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["it"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["cy"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["lv"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["lt"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["lu"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["hu"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["mt"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["nl"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["at"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["pl"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["pt"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["ro"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["si"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["sk"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["fi"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["se"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
europarties_2014_members["gb-gbn"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)




parties[['bg']][1], parties[['dk']][2])
# names(europarties_2014_members[2]) <- "Hej"


# party_political_aff <- left, right, etc -----------------------------------------------------------


class(data_list)
str(data_list)
summary(data_list)
head(data_list)
tail(data_list)
View(data_list)


# TODO use levels() in pol_positions

# summarize kol el polls by sherka, by country -------------------------------------------------------
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

# TODO: derive party names from col names

at_parties <- c("GRÜNE", "PILZ", "SPÖ", "NEOS", "ÖVP", "FPÖ")
names(at_parties) <- c("Die Grüne Alternative", "Liste Peter Pilz", "Sozialdemokratische Partei Österreichs", "Das Neue Österreich", "Österreichische Volkspartei", "Freiheitliche Partei Österreichs")


# CDU/CSU -> Union - needs to be summarized in table - die Partei is left wing despite being NI
## their real name, Partei für Arbeit, Rechtsstaat, Tierschutz, Elitenförderung und basisdemokratische Initiative, has been shortened for convenience
de_parties <- c("Linke", "PARTEI", "Grüne", "ÖDP", "Piraten", "SPD", "FDP", "CDU/CSU",  "LKR", "AfD", "B", "NPD")
names(de_parties) <- c("Die Linke", "Die PARTEI", "Bündnis90/Die Grünen", 
                       "Ökologisch-Demokratische Partei", "Piratenpartei Deutschlands", "Sozialdemoratische Partei Deutschlands",
                       "Freie Demokraten", "Christlich Demokratische Union", "Liberal-Konservative Reformer", "Alternative für Deutschland", 
                       "Die Blaue Partei", "Nationaldemokratische Partei Deutschlands" )


# Macron is making a new party that was not included in the polls
fr_parties <- c("FI.fr", "PCF", "PS", "EELV", "LEM", "MoDem", "LR", "DLF", "RN")
names(fr_parties) <- c("La France Insoumise", "Parti Communiste Français",  "Parti Socialiste", "Europe Écologie - Les Verts",
                       "La République En Marche", "Mouvement Democrate", "Les Républicains", "Debout La France", "Rassemblement National" )

#FI not to be confused with the French France Insoumise
## SI not to confuse with french
it_parties <- c("SI", "PaP", "PD", "+E", "MDP", "FI.it", "FdI", "M5S", "LEGA" )
names(it_parties) <- c("Sinistra Italiana", "Potere al Popolo", "Partito Democratico", "Più Europa", "Articolo 1", "Forza Italia", "Fratelli d'Italia",
                       "Movimento 5 Stelle", "Lega")

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


if(DE_data$Sample.Size == "NA" | "Not Available" | "N/A"){
  DE_data$Sample.Size <- median(DE_data$Sample.Size, na.rm = TRUE)
}

# 
# DE_data_readable <- DE_data %>%
#   Fieldwork.Start = as.Date(as.character(Fieldwork.Start))
# 
# DE_data_readable
