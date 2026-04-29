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
## C. Fonctions helpers ----
### Fct conversion date excel
excelToDate <- function(x) {
  as.Date(as.numeric(x), origin = "1899-12-30")
}
### Fct parsing date dmy
parseDMY <- function(x) as.Date(x, format = "%d.%m.%Y")


# 2. Chargement des données ----
dtTrajectoiresBrute <- fread(here("02_inputFiles", "11_XYZTrajectoires.csv"))
dtProfilesBrute <- fread(here("02_inputFiles", "11_XYZProfiles.csv"))
dtStructuresBrute <- fread(here("02_inputFiles", "20260401_tableProvenancesDestinations_def.csv"), header = TRUE)

dtTrajectoires <- copy(dtTrajectoiresBrute)
dtProfiles <- copy(dtProfilesBrute)
dtStructures <- copy(dtStructuresBrute)

# 3. Nettoyage et formattage ----
## A. dtStructures
### Clean text
dtStructures <- cleanTextCols(dtStructures)
names(dtStructures) <- to_lower_camel_case(names(dtStructures))
names(dtStructures) <- replace_non_ascii(names(dtStructures))
setnames(
  x = dtStructures,
  old = "provenancesEtDestinations",
  new = "nomStructure"
)

### Suppression lignes et colonnes fantômes
dtStructures[, c("v4", "v5", "v6") := NULL]
dtStructures <- dtStructures[!nomStructure %in% c("CSEE_X", "CSEE_Y", "CSEE_Z")]
### Ajout d'un ID (num de ligne)
dtStructures[, idStructure := .I] 

## B. dtTrajectoires ----
### Clean text
dtTrajectoires <- cleanTextCols(dtTrajectoires)
names(dtTrajectoires) <- to_lower_camel_case(names(dtTrajectoires))
names(dtTrajectoires)  <- replace_non_ascii(names(dtTrajectoires))
### Conversion des dates (format dd.mm.yyyy) ----
dtTrajectoires[, dateMajorite := excelToDate(dtTrajectoires[["dateMajorite"]])] # Excel (format numérique) vers Date
dtTrajectoires[, dateDebut := parseDMY(`dateDeDebut`)]
dtTrajectoires[, dateFin := parseDMY(`dateDeFin`)]
dtTrajectoires[, duree := as.numeric(dateFin - dateDebut)]
### Suppression des lignes fantômes ----
dtTrajectoires <- dtTrajectoires[!is.na(cas) & !is.na(dateDebut)]
### Ajout ID (num de ligne) ----
dtTrajectoires[, idEpisode := .I]

## C. dtProfiles ----
### Clean text ----
dtProfiles <- cleanTextCols(dtProfiles)
names(dtProfiles) <- replace_non_ascii(names(dtProfiles))
names(dtProfiles) <- to_lower_camel_case(names(dtProfiles))
### Ajout ID (num de ligne) ----
dtProfiles[, idProfile := .I]

# 4. Renommage des tables pour une future mise en DB ----
dimStructure <- copy(dtStructures)
dimJeune <- copy(dtProfiles)
factEpisode <- copy(dtTrajectoires)

# 5. Jointure des tables ----

factEpisode <- merge(
  factEpisode,
  dimStructure[, .(nomStructure, idStructure)],
  by.x = "sejour",
  by.y = "nomStructure",
  all.x = TRUE
)


# Vérification que les 3 tables sont bien construites
cat("dim_jeune    :", nrow(dimJeune),    "jeunes\n")
cat("dim_structure:", nrow(dimStructure),"structures\n")
cat("fact_episode :", nrow(factEpisode), "épisodes\n")






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