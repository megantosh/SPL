# Preprocessing.R - Polls' behaviour for the EU Parliament Elections 2019
# Author: Mohamed Megahed - Matr.: 589375
# Statistical Programming Languages - SoSe 2019


# # # # # Imports # # # # #


# library(tidyverse)
library(dplyr)
library(readr)
library(stats)


# # # # # Variables # # # # #

# Sorted as adopted from http://publications.europa.eu/code/pdf/370000en.htm
## Greece is 'gr' and not 'el (Hellenic Republic)
eu_country_code <-c("be", "bg", "cz", "dk", "de", "ee",
                     "ie", "gr", "es", "fr", "hr", "it", "cy", "lv", "lt", "lu", "hu",
                     "mt", "nl", "at", "pl", "pt", "ro", "si", "sk", "fi", "se", "gb-gbn")
names(eu_country_code) <- c("Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia",
                            "Ireland", "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", "Latvia", "Lithuania", "Luxembourg", "Hungary",
                            "Malta", "Netherlands", "Austria", "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden", "United Kingdom (GB+NIr)") 

# EU Parliament Political Groups sorted by affiliation (extreme left to extreme right)
eup_pol_group_2014 <- c("GUE/NGL", "G/EFA", "S&D", "ALDE", "EEP", "ECR", "EFDD", "ENF/EAPN", "NI", "UnAff")
# Using shortened common names for readability
names(eup_pol_group_2014) <-c ("Eur. United Left - Nordic Green Left", "Greens / Eur. Free All.", "Socialists & Democrats",
                               "All. of Liberals and Democrats", "Eur. People's Party (Chris. Democrats)", "Eur. Conservatives & Reformists", 
                               "Euro. Freedom & Direct Democracy", "Nations & Freedom / Eur. All. of People's & Nations", "Non-Inscrits", "Unaffiliated")

# Using extended names for formaility
names(eup_pol_group_2014) <- c("Confederal Group of the European United Left - Nordic Green Left",
"Group of the Greens/European Free Alliance", "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament",
"Group of the Alliance of Liberals and Democrats for Europe", "Group of the European People's Party (Christian Democrats)","European Conservatives and Reformists Group",
"Europe of Freedom and Direct Democracy Group", "Europe of Nations and Freedom Group (- European Alliance of Peoples and Nations)",
"Non-Inscrits (Independents, Nazis, Satirists, Communists)", "Unaffiliated")

# xTmp: Ex tempore (at time of research)   --   ALDE includes french Macron's Renaissance
eup_pol_group_2019 <- c("GUE/NGL", "G/EFA", "S&D", "UnAff1 - Volt", "ro_PSD (xTmp S&D)", "ALDE", "ro_ALDE (xTmp)",
                       "EEP", "hu_Fidesz (xTmp EPP)", "ECR","UnAff2 - it_M5S + all.", "EFDD", "UnAff3 - uk_BRExit", "ENF/EAPN", "NI")
names(eup_pol_group_2019) <- c("Eur. United Left - Nordic Green Left", "Greens / Eur. Free All.", "Socialists & Democrats",
                               "All. of Liberals and Democrats", "Eur. People's Party (Chris. Democrats)", "Eur. Conservatives & Reformists", 
                               "Euro. Freedom & Direct Democracy", "Nations & Freedom / Eur. All. of People's & Nations", "Non-Inscrits", "Unaffiliated")


# derrived from analysis of political orientations of EU Parties (EUP)
orientation_scale_2014 <- c("left-wing", "green", "center-left", "liberal", "centre-right", "national-conservative", "eurosceptic-populist", "right-wing", "unaffiliated", "non-inscrit")
orientation_scale_2019 <- c("left-wing", "green", "center-left", "UnAff1: eurofederalist", "liberal", "centre-right", "national-conservative", "eurosceptic-populist", "right-wing", "unaff2: M5S", "UnAff3: BREXIT", "non-inscrit")



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





#levels()

count_mep <- 751
count_eu_countries <- length(eu_country_code)
count_eup_pol_group_2014 <-length(eup_pol_group_2014)

# nat_eu_matrix <- matrix( , nrow = count_eu_countries)















# step 1: import dataset
print("Project Working Directory: "); getwd()      

import_path_prefix <- "datasets/eopaod-master/docs/"
for(country_suffix in eu_country_code){
  if(country_suffix == "be"){
    next;
  } else {
  import_path <- paste(import_path_prefix, country_suffix, ".csv", sep = "")
  read.csv(import_path, row.names = NULL)
  print(paste("imported ", country_suffix , " from ", import_path))
  }
}



import_path_at <- paste(import_path_prefix, "at-N.csv", sep = "")
read.csv(import_path_de)

comparison_countries <- c("at", "de", "fr", "it")
comparison_df <- data.frame()

for (i in 1:length(comparison_countries)) {
  comparison_df <- read.csv(paste(import_path_prefix, comparison_countries[i], "-n.csv", sep =""))
}



detach("package:dplyr")  # Unload dplyr
detach("package:stats")  






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


DE_data_readable <- DE_data %>%
  Fieldwork.Start = as.Date(as.character(Fieldwork.Start))

DE_data_readable
