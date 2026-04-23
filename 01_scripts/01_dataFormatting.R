# Chargement et nettoyage des tables
# Auteur : Alexandre Poncin



# 1. Chargement des librairies, sources et création des paramètres ----
## A. Library ----
pacman::p_load(
  data.table,
  here,
  packageOejqs,
  snakecase,
  lubridate, 
  textclean
)
## B. Sources internes ----

# 2. Chargement des données ----
dtTrajectoires <- fread(here("02_inputFiles", "11_XYZTrajectoires.csv"))
dtProfiles <- fread(here("02_inputFiles", "11_XYZProfiles.csv"))
dtStructures <- fread(here("02_inputFiles", "20260401_tableProvenancesDestinations_def.csv"), header = TRUE)
# 3. Nettoyage et formattage ----
## A. dtStructures
dtStructures[, c("V4", "V5", "V6") := NULL]
dtStructures <- dtStructures[!provenancesEtDestinations %in% c("CSEE_X", "CSEE_Y", "CSEE_Z")]
dtStructures[, ID := .I] #Ajout d'un ID (num de ligne)

## A. dtTrajectoires ----
dtTrajectoires <- cleanTextCols(dtTrajectoires)
names(dtTrajectoires) <- to_lower_camel_case(names(dtTrajectoires))
names(dtTrajectoires) <- replace_non_ascii(names(dtTrajectoires))
dtTrajectoires[, episodeID := .I]

## B. dtProfiles ----
dtProfiles <- cleanTextCols(dtProfiles)
names(dtProfiles) <- to_lower_camel_case(names(dtProfiles))
names(dtProfiles) <- replace_non_ascii(names(dtProfiles))

## C. Conversion dates Excel (format numérique) vers Date ----
excelToDate <- function(x) {
  as.Date(as.numeric(x), origin = "1899-12-30")
}

dtTrajectoires[, dateMajorite := excelToDate(dtTrajectoires[["dateMajorite"]])]

## D. Conversion des colonnes de dates (format dd.mm.yyyy) ----
parseDMY <- function(x) as.Date(x, format = "%d.%m.%Y")

dtTrajectoires[, dateDebut := parseDMY(`dateDeDebut`)]
dtTrajectoires[, dateFin := parseDMY(`dateDeFin`)]
dtTrajectoires[, duree := as.numeric(dateFin - dateDebut)]

## E. Suppression des lignes fantômes ----
dtTrajectoires <- dtTrajectoires[!is.na(cas) & !is.na(dateDebut)]

# 4. Jointure des tables ----









# ============================================================
# Classification des lignes par type d'événement
# ============================================================

# # Règle de priorité : Admission > Séjour > Congé > Fugue > Suivi
# dtTrajectoires[, typeEvenement := fcase(
#   !is.na(admission) & admission != "", "admission",
#   !is.na(sejourConge) & grepl("Sejour", sejourConge), "sejour",
#   !is.na(sejourConge) & grepl("Conge", sejourConge), "conge",
#   !is.na(fugue) & fugue != "", "fugue",
#   !is.na(admission) & grepl("Suivi", admission), "suivi",
#   default = NA_character_
# )]

# Extraction du numéro de séquence dans l'événement
# dtTrajectoires[, numEvenement := as.integer(
#   gsub(
#     "[^0-9]",
#     "",
#     fcoalesce(
#       fifelse(
#         admission == "",
#         NA_character_,
#         admission
#       ),
#       fifelse(
#         sejourConge == "",
#         NA_character_,
#         sejourConge
#       ),
#       fifelse(
#         fugue == "",
#         NA_character_,
#         fugue
#       )
#     )
#   )
# )]




# # ============================================================
# # Recodage des 22 provenances/destinations en 6 catégories
# # ============================================================
# 
# recoderOrigine <- function(x) {
#   fcase(
#     grepl("Familie|Eltern|famille",       x, ignore.case = TRUE), "famille",
#     grepl("Psychiatr|Hôpital|Krankenhaus", x, ignore.case = TRUE), "hopitalPsy",
#     grepl("CSEE|Wohngruppe|Unisec",       x, ignore.case = TRUE), "csee",
#     grepl("fermé|Geschlossen|Centre fermé", x, ignore.case = TRUE), "centreFerme",
#     grepl("Heimstruktur|AEF|foyer",       x, ignore.case = TRUE), "autreStructure",
#     !is.na(x) & x != "",                                           "autre",
#     default = NA_character_
#   )
# }
# 
# dtTrajectoire[, provenanceCateg   := recoderOrigine(Provenance)]
# dtTrajectoire[, destinationCateg  := recoderOrigine(Destination)]