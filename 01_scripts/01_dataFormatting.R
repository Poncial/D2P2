# ============================================================
# Chargement et nettoyage des tables
# Auteur : Jean-Eudes Méthodologiste
# ============================================================

library(data.table)
library(lubridate)
library(here)
library(packageOejqs)
library(snakecase)
library(textclean)

# --- Chargement ---
dtTrajectoires <- fread(here("02_inputFiles", "11_XYZTrajectoires.csv"))
dtProfiles <- fread(here("02_inputFiles", "11_XYZProfiles.csv"))

# --- Nettoyage des strings ---
## A. Trajectoires
dtTrajectoires <- cleanTextCols(dtTrajectoires)
names(dtTrajectoires) <- to_lower_camel_case(names(dtTrajectoires))
names(dtTrajectoires) <- replace_non_ascii(names(dtTrajectoires))

## B. Profiles
dtProfiles <- cleanTextCols(dtProfiles)
names(dtProfiles) <- to_lower_camel_case(names(dtProfiles))
names(dtProfiles) <- replace_non_ascii(names(dtProfiles))

# --- Conversion dates Excel (format numérique) vers Date ---
excelToDate <- function(x) {
  as.Date(as.numeric(x), origin = "1899-12-30")
}

dtTrajectoires[, dateMajorite := excelToDate(dtTrajectoires[["dateMajorite"]])]

# --- Conversion des colonnes de dates (format dd.mm.yyyy) ---
parseDMY <- function(x) as.Date(x, format = "%d.%m.%Y")

dtTrajectoires[, dateDebut := parseDMY(`dateDeDebut`)]
dtTrajectoires[, dateFin := parseDMY(`dateDeFin`)]
dtTrajectoires[, duree := as.numeric(dateFin - dateDebut)]

# --- Suppression des lignes fantômes ---
dtTrajectoires <- dtTrajectoires[!is.na(cas) & !is.na(dateDebut)]

# ============================================================
# Classification des lignes par type d'événement
# ============================================================

# Règle de priorité : Admission > Séjour > Congé > Fugue > Suivi
dtTrajectoires[, typeEvenement := fcase(
  !is.na(admission) & admission != "", "admission",
  !is.na(sejourConge) & grepl("Sejour", sejourConge), "sejour",
  !is.na(sejourConge) & grepl("Conge", sejourConge), "conge",
  !is.na(fugue) & fugue != "", "fugue",
  !is.na(admission) & grepl("Suivi", admission), "suivi",
  default = NA_character_
)]

# Extraction du numéro de séquence dans l'événement
dtTrajectoires[, numEvenement := as.integer(
  gsub(
    "[^0-9]",
    "",
    fcoalesce(
      fifelse(
        admission == "",
        NA_character_,
        admission
      ),
      fifelse(
        sejourConge == "",
        NA_character_,
        sejourConge
      ),
      fifelse(
        fugue == "",
        NA_character_,
        fugue
      )
    )
  )
)]

# ============================================================
# Recodage des 22 provenances/destinations en 6 catégories
# ============================================================

recoderOrigine <- function(x) {
  fcase(
    grepl("Familie|Eltern|famille",       x, ignore.case = TRUE), "famille",
    grepl("Psychiatr|Hôpital|Krankenhaus", x, ignore.case = TRUE), "hopitalPsy",
    grepl("CSEE|Wohngruppe|Unisec",       x, ignore.case = TRUE), "csee",
    grepl("fermé|Geschlossen|Centre fermé", x, ignore.case = TRUE), "centreFerme",
    grepl("Heimstruktur|AEF|foyer",       x, ignore.case = TRUE), "autreStructure",
    !is.na(x) & x != "",                                           "autre",
    default = NA_character_
  )
}

dtTrajectoire[, provenanceCateg   := recoderOrigine(Provenance)]
dtTrajectoire[, destinationCateg  := recoderOrigine(Destination)]