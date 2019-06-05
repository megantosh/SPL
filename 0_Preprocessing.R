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
count_mep_seats <- sum(seats_by_country)
count_eu_countries <- as.numeric(length(eu_country_codes))
count_europarties_2014 <-as.numeric(length(europarties_2014))

skipped_eu_country_codes <-c("be", "uk", "el", "cr")


# Importing dataset  -------------------------------------------------------------
print(paste("Project Working Directory: ", getwd()))

# import all country CSVs into one big list of tibbles (data frames)
import_path_prefix <- "datasets/eopaod-master/docs/"
# init list to store data frames and iterate over - warning: use these lines when rerunning code to avoid duplicates
data_list <- summary_by_country <- parties_by_country <- list() # TODO:ASK is okay to init multiple vars like this or prefer extra line for each to explain var name / manipilate separately etc?
count <- 1 
for(country_suffix in eu_country_codes){
  # skips in case you want to exclude a certain data frame not currently present (in proper format) in folder 
  if(country_suffix %in% skipped_eu_country_codes){
    next; # TODO:ASK does that need to go onto one line?
  } else {
    count <- count + 1
    # add "-N" or "-E" before ".csv" for national or european subtables
    import_path <- paste(import_path_prefix, country_suffix, ".csv", sep = "")
    # using readr::read_csv as a better version of read.csv () - caution: outputs tibble, which inherit from data.frame
    data_list <- c(data_list, lapply(import_path, read_csv))
    # alternatively use the line below: read.csv(..., row.names = NULL) in order not to collide with 'cy.csv'
    # data_list <- c(data_list,lapply(import_path, read.csv, row.names = NULL))
    # TODO maybe add here as.data.frame([count])
    #each data.frame gets the label of the respective country
    # names(data_list[[count]]) <- eu_country_codes[count+1] #+1 for ignoring Belgium
    print(paste("imported", names(eu_country_codes[count]), "from", import_path)) #,"into", names(data_list[[count]])))
  }
}
count <- NULL #reset counter to avoid errors in reruns
# TODO: change index to exclude skippable countries
names(data_list) <- eu_country_codes[2:28] #starts at 2 ignoring Belgium 


# extract first part of the table (common part)
for(country_suffix in eu_country_codes){
  if(country_suffix %in% skipped_eu_country_codes){
    next;
  } else {
    print(country_suffix)
  summary_by_country[[country_suffix]] <- data_list[[country_suffix]][1:9]
  parties_by_country[[country_suffix]] <- data_list[[country_suffix]][-(1:9)]
  # add_column(parties_by)
  }
}

class(data_list)
str(data_list)
head(data_list)
tail(data_list)
View(data_list)


# party_repr_in_eup <- list of vectors( , nrow = count_eu_countries)
# party_political_aff <- left, right, etc
# TODO use levels() in pol_positions





# zabbat-hom el awwel marra wa7da ba3dein efselhom
# summarize kol el polls by sherka, by country ...
# subgroup by bigger countries
# how many seats with respect to how many countries - and the way this is changing with brexit seats redistributed
# apply on 2014 poll if possible

# fel parties_by_country e3mel vector esmo EPP, ..., we include fih [parties_by_country][index of party]



#TODO: special treatment for exception countries (e.g. Belgium)



##TODO: access data.frame tables via: data_list[['eu']]$...


## TODO: put common part into data table with one big list incl. country
## TODO: then take out the party names [9:-1] and put them into a political affiliation



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









transform_country_csv <- function(input_data_frame) {
  input_data_frame <- lapply(input_data_frame, gsub, pattern = "Not Available|NA|N.A.|N/A", ignore.case = TRUE , replacement= NA)
  input_data_frame <- lapply(input_data_frame, gsub, pattern = "%", replacement='' )
  
  input_data_frame$Fieldwork.Start <- as.Date(as.character(input_data_frame$Fieldwork.Start))
  input_data_frame$Fieldwork.End <- as.Date(as.character(input_data_frame$Fieldwork.End))
  input_data_frame$duration <- as.numeric(input_data_frame$Fieldwork.End - input_data_frame$Fieldwork.Start)
  input_data_frame$Sample.Size <- as.character(input_data_frame$Sample.Size)
  

  str(input_data_frame)
  head(input_data_frame)
  return(input_data_frame)
}


fr_trans <- as.data.frame(transform_country_csva(FR_data))


class(fr_trans)
str(fr_trans)


FR_data

fr_trans <- as.data.frame(transform_country_csva())


class(fr_trans)
str(fr_trans)
str(fr_trans)





if(DE_data$Sample.Size == "NA" | "Not Available" | "N/A"){
  DE_data$Sample.Size <- median(DE_data$Sample.Size, na.rm = TRUE)
}

# 
# DE_data_readable <- DE_data %>%
#   Fieldwork.Start = as.Date(as.character(Fieldwork.Start))
# 
# DE_data_readable
