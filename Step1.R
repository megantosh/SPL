#Dependencies
if(!require("RCurl")) install.packages("RCurl"); library(RCurl) # Curl from HTTPS

getwd ()

#dataset files
 
FR_data <-read.csv(text = getURL("https://filipvanlaenen.github.io/eopaod/fr.csv"))
FR_E_data <- read.csv(text = getURL("https://filipvanlaenen.github.io/eopaod/fr-E.csv"))
FR_N_data <- read.csv(text = getURL("https://filipvanlaenen.github.io/eopaod/fr-E.csv"))

DE_data <-read.csv(text = getURL("https://filipvanlaenen.github.io/eopaod/de.csv"))
DE_E_data <- read.csv(text = getURL("https://filipvanlaenen.github.io/eopaod/de-E.csv"))
DE_N_data <- read.csv(text = getURL("https://filipvanlaenen.github.io/eopaod/de-E.csv"))

IT_data <-read.csv(text = getURL("https://filipvanlaenen.github.io/eopaod/it.csv"))
IT_E_data <- read.csv(text = getURL("https://filipvanlaenen.github.io/eopaod/it-E.csv"))
IT_N_data <- read.csv(text = getURL("https://filipvanlaenen.github.io/eopaod/it-E.csv"))

ES_data <-read.csv(text = getURL("https://filipvanlaenen.github.io/eopaod/es.csv"))
ES_E_data <- read.csv(text = getURL("https://filipvanlaenen.github.io/eopaod/es-E.csv"))
ES_N_data <- read.csv(text = getURL("https://filipvanlaenen.github.io/eopaod/es-E.csv"))


summary(FR_data)
summary(FR_E_data)
summary(FR_N_data)

summary(DE_data)
summary(DE_E_data)
summary(DE_N_data)

summary(IT_data)
summary(IT_E_data)
summary(IT_N_data)

summary(ES_data)
summary(ES_E_data)
summary(ES_N_data)


# variable Declarations


# EP Groups
EU_parties <- c("GUE/NGL", "G/EFA", "S&D", "ALDE", "EEP", "ECR", "EFDD", "ENF/EAPN", "NI", "Unaffiliated")
# 
GUE_NGL_members <- c("","")
# 
G_EFA_members <- c("","")
# 
SandD_members <- c(,"")
# 
ALDE_members <- c("","")
# 
EPP_members <- c(DE_parties['CDU'],"")
# 
ECR_members <- c("","")
# 
EFDD_members <- c("","")
# 
ENF_EAPN_members <- c("","")
# 
NI_members <- c("","")
# 
unaffiliated_members <- c("","")


political_groups <- c(EU_parties, GUE_NGL_members, G_EFA_members, SandD_members, ALDE_members, EPP_members, ECR_members, EFDD_members, ENF_EAPN_members, NI_members, unaffiliated_members)


# National Parties by country
AT_parties <- c("ÖVP", "SPÖ", "FPÖ", "NEOS", "PILZ", "GRÜNE")
BE_parties <- c("N-VA", "CD&V", "Open Vld", "sp.a", "Groen", "Vlaams Belang")
BG_parties <- c("GERB","BSP", "DPS", "OP","-")
HR_parties <- c("","")
CY_parties <- c("","")
CZ_parties <- c("","")
DK_parties <- c("","")
EE_parties <- c("","")
FI_parties <- c("","")
FR_parties <- c("LREM","LR","RN","FI","PS","EELV","PCF","DF","MoDem")
DE_parties <- c("CDU","SPD","AfD","FDP","LINKE","GRÜNE","CSU","B","LKR","FREIE WÄHLER", "ÖDP", "Die PARTEI", "PIRATEN", "NPD")
GR_parties <- c("","")
HU_parties <- c("","")
IE_parties <- c("","")
IT_parties <- c("M5S","LEGA","PD","FI","FdI","+E","MDP","Nci/UDP","SI","PaP")
LV_parties <- c("","")
LT_parties <- c("","")
LU_parties <- c("","")
MT_parties <- c("","")
NL_parties <- c("","")
PL_parties <- c("","")
PT_parties <- c("","")
RO_parties <- c("","")
SK_parties <- c("","")
SI_parties <- c("","")
ES_parties <- c("PP","PSOE","UP-ECM-EM","Cs","ERC-CatSi","PDeCAT","PNV","PACMA","EHB","CC-PNC")
SE_parties <- c("","")
GB_GBN_parties <- c("","")
GB_NIR_parties <- c("","")



