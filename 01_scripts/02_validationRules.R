# ============================================================
# VALIDATION — ARCHITECTURE REGISTRE DE RÈGLES
# Auteur  : Alexandre Poncin
# Date : Avril 2026
# ============================================================
# Architecture :
#   1. PARAM        — paramètres globaux
#   2. REGISTRE     — liste déclarative de règles
#   3. FONCTIONS    — une fn pure par règle (retourne dt ou NULL)
#   4. RUNNER       — applique toutes les règles via purrr::map
#   5. RAPPORT      — agrège et présente les résultats
# ============================================================

# ============================================================
# 0. Package et fonctions
# ============================================================
pacman::p_load(
  data.table, 
  purrr,
  pointblank
)

# ============================================================
# 1. PARAMÈTRES GLOBAUX
# ============================================================

PARAM <- list(
  ageMinAdmission   = 10L,
  ageMaxAdmission   = 17L,
  dureeMaxJours     = 365L * 3L,
  toleranceMajorite = 30L,
  seuilNaCritique   = 0.30,
  typesValides      = c("admission", "sejour", "conge", "fugue", "suivi"),
  structuresValides = c(
    "CSEE_X Intensivpadagogische Wohngruppe",
    "CSEE Unisec",
    "CSEE_Y Aufnahmegruppe",
    "CSEE_Z Intensivpadagogische Wohngruppe",
    "CSEE_Y Studio",
    "CSEE_Y Intensivpadagogische Wohngruppe",
    "CSEE_Y semi-autonome Wohngruppe",
    "CSEE Intensivpadagogische Wohngruppe"
  )
)

# ============================================================
# 2. FONCTIONS DE RÈGLES PURES
# Signature uniforme : function(tables, param) → data.table | NULL
# Retourne : les lignes problématiques avec colonnes
#            id, episodeId, valeurObs, details
#            OU NULL si aucun problème
# ============================================================

# --- Helpers internes ---

# Normalise une colonne en character, remplace "" par NA
normChar <- function(x) {
  x <- as.character(x)
  fifelse(is.na(x) | trimws(x) == "", NA_character_, x)
}

# Construit une data.table de cas vides (règle OK)
aucunCas <- function() NULL

# Vérifie qu'une data.table retournée a bien les bonnes colonnes
# (programmation défensive)
verifierColonnes <- function(dt) {
  stopifnot(is.data.table(dt) || is.null(dt))
  if (!is.null(dt) && nrow(dt) > 0L) {
    colsAttendues <- c("id", "episodeId", "valeurObs", "details")
    colsManquantes <- setdiff(colsAttendues, names(dt))
    if (length(colsManquantes) > 0L) {
      stop(paste("Colonnes manquantes dans le retour :",
                 paste(colsManquantes, collapse = ", ")))
    }
  }
  dt
}

# ---- Section 1-2 : dim_jeune --------------------------------

# R01 — Unicité des ID jeunes
fnR01 <- function(tables, param) {
  dt <- tables$dimJeune
  res <- dt[duplicated(id), .(
    id        = id,
    episodeId = NA_character_,
    valeurObs = id,
    details   = "Identifiant dupliqué dans dim_jeune"
  )]
  verifierColonnes(res)
}

# R02 — Naissance renseignée
fnR02 <- function(tables, param) {
  dt <- tables$dimJeune
  res <- dt[
    is.na(anneeDeNaissance) | is.na(moisDeNaissance), .(
      id,
      episodeId = NA_character_,
      valeurObs = NA_character_,
      details   = "Année ou mois de naissance manquant"
    )
  ]
  verifierColonnes(res)
}

# R03 — Cohérence date majorité / naissance (18 ans ± 6 mois)
fnR03 <- function(tables, param) {
  dt <- copy(tables$dimJeune)
  dt[, dateNaissCalc := as.Date(paste0(
    anneeDeNaissance, "-",
    formatC(moisDeNaissance, width = 2L, flag = "0"),
    "-01"
  ))]
  dt[, ageMaj := as.numeric(
    difftime(dateMajorite, dateNaissCalc, units = "days")
  ) / 365.25]
  
  res <- dt[
    !is.na(ageMaj) & (ageMaj < 17.5 | ageMaj > 18.5), .(
      id,
      episodeId = NA_character_,
      valeurObs = as.character(round(ageMaj, 2L)),
      details   = paste0(
        "Âge à la majorité = ", round(ageMaj, 2L),
        " ans (attendu 18 ± 0.5)"
      )
    )
  ]
  verifierColonnes(res)
}

# R04 — Âge à l'admission plausible
fnR04 <- function(tables, param) {
  dt <- tables$dimJeune
  if (!"ageAnnees" %in% names(dt)) return(aucunCas())
  res <- dt[
    !is.na(ageAnnees) & (
      ageAnnees < param$ageMinAdmission |
        ageAnnees > param$ageMaxAdmission
    ), .(
      id,
      episodeId = NA_character_,
      valeurObs = as.character(ageAnnees),
      details   = paste0(
        "Âge = ", ageAnnees, " ans (attendu ",
        param$ageMinAdmission, "–", param$ageMaxAdmission, ")"
      )
    )
  ]
  verifierColonnes(res)
}

# R05 — Sexe renseigné
fnR05 <- function(tables, param) {
  dt <- tables$dimJeune
  if (!"sexe" %in% names(dt)) return(aucunCas())
  res <- dt[is.na(normChar(sexe)), .(
    id,
    episodeId = NA_character_,
    valeurObs = NA_character_,
    details   = "Sexe manquant"
  )]
  verifierColonnes(res)
}

# ---- Section 3-5 : fact_episode -----------------------------

# R06 — Types d'épisodes valides
fnR06 <- function(tables, param) {
  dt <- tables$factEpisode
  res <- dt[!typeEvenement %in% param$typesValides, .(
    id,
    episodeId = as.character(episodeId),
    valeurObs = typeEvenement,
    details   = paste0(
      "Type '", typeEvenement, "' invalide. ",
      "Valides : ", paste(param$typesValides, collapse = ", ")
    )
  )]
  verifierColonnes(res)
}

# R07 — Date de début renseignée
fnR07 <- function(tables, param) {
  dt <- tables$factEpisode
  res <- dt[is.na(dateDebut), .(
    id,
    episodeId = as.character(episodeId),
    valeurObs = NA_character_,
    details   = paste0(
      "Date de début manquante (type : ", typeEvenement, ")"
    )
  )]
  verifierColonnes(res)
}

# R08 — Date fin >= date début
fnR08 <- function(tables, param) {
  dt <- tables$factEpisode[!is.na(dateFin)]
  res <- dt[dateFin < dateDebut, .(
    id,
    episodeId = as.character(episodeId),
    valeurObs = paste0(dateDebut, " → ", dateFin),
    details   = paste0(
      "Date fin (", dateFin,
      ") antérieure à date début (", dateDebut, ")"
    )
  )]
  verifierColonnes(res)
}

# R09 — Cohérence durée calculée vs stockée
fnR09 <- function(tables, param) {
  dt <- copy(tables$factEpisode[!is.na(dateFin) & !is.na(dateDebut)])
  dt[, dureeCalc := as.integer(dateFin - dateDebut)]
  res <- dt[
    !is.na(duree) & abs(dureeCalc - duree) > 1L, .(
      id,
      episodeId = as.character(episodeId),
      valeurObs = paste0("stockée=", duree, " | calculée=", dureeCalc),
      details   = paste0(
        "Écart de ", abs(dureeCalc - duree),
        " j entre durée stockée et calculée"
      )
    )
  ]
  verifierColonnes(res)
}

# R10 — Durées négatives
fnR10 <- function(tables, param) {
  dt <- tables$factEpisode
  res <- dt[!is.na(duree) & duree < 0L, .(
    id,
    episodeId = as.character(episodeId),
    valeurObs = as.character(duree),
    details   = paste0("Durée négative : ", duree, " jours")
  )]
  verifierColonnes(res)
}

# R11 — Durées excessivement longues
fnR11 <- function(tables, param) {
  dt <- tables$factEpisode
  res <- dt[
    !is.na(duree) & duree > param$dureeMaxJours, .(
      id,
      episodeId = as.character(episodeId),
      valeurObs = as.character(duree),
      details   = paste0(
        "Durée ", duree, "j > seuil ", param$dureeMaxJours, "j"
      )
    )
  ]
  verifierColonnes(res)
}

# R12 — Ordre chronologique par jeune
fnR12 <- function(tables, param) {
  dt <- copy(tables$factEpisode)
  setorder(dt, id, dateDebut)
  dt[, debutPrec := shift(dateDebut, 1L), by = id]
  dt[, idPrec    := shift(id, 1L)]
  res <- dt[
    !is.na(debutPrec) & id == idPrec & dateDebut < debutPrec, .(
      id,
      episodeId = as.character(episodeId),
      valeurObs = as.character(dateDebut),
      details   = paste0(
        "Début (", dateDebut,
        ") < épisode précédent (", debutPrec, ")"
      )
    )
  ]
  verifierColonnes(res)
}

# ---- Section 4 : relations ----------------------------------

# R13 — FK : ID épisode → jeune existant
fnR13 <- function(tables, param) {
  idsJeune <- tables$dimJeune$id
  res <- tables$factEpisode[!id %in% idsJeune, .(
    id,
    episodeId = as.character(episodeId),
    valeurObs = id,
    details   = paste0(
      "ID '", id, "' absent de dim_jeune"
    )
  )]
  verifierColonnes(res)
}

# R14 — FK : structure → référentiel
fnR14 <- function(tables, param) {
  dt <- tables$factEpisode
  res <- dt[
    typeEvenement == "sejour" &
      !is.na(normChar(sejour)) &
      !sejour %in% param$structuresValides, .(
        id,
        episodeId = as.character(episodeId),
        valeurObs = sejour,
        details   = paste0("Structure '", sejour, "' hors référentiel")
      )
  ]
  verifierColonnes(res)
}

# R15 — Jeunes sans aucun épisode
fnR15 <- function(tables, param) {
  idsAvecEp <- unique(tables$factEpisode$id)
  idsSans   <- setdiff(tables$dimJeune$id, idsAvecEp)
  if (length(idsSans) == 0L) return(aucunCas())
  res <- data.table(
    id        = idsSans,
    episodeId = NA_character_,
    valeurObs = NA_character_,
    details   = "Jeune sans aucun épisode associé"
  )
  verifierColonnes(res)
}

# ---- Section 5 : temporel -----------------------------------

# R16 — Épisode après majorité
fnR16 <- function(tables, param) {
  dt <- merge(
    tables$factEpisode[!is.na(dateDebut)],
    tables$dimJeune[, .(id, dateMajorite)],
    by = "id", all.x = TRUE
  )
  res <- dt[
    !is.na(dateMajorite) &
      dateDebut > dateMajorite + param$toleranceMajorite, .(
        id,
        episodeId = as.character(episodeId),
        valeurObs = as.character(dateDebut),
        details   = paste0(
          "Épisode le ", dateDebut,
          " après majorité le ", dateMajorite,
          " (tolérance ", param$toleranceMajorite, "j)"
        )
      )
  ]
  verifierColonnes(res)
}

# R17 — Chevauchement entre séjours
fnR17 <- function(tables, param) {
  dt <- copy(tables$factEpisode[
    typeEvenement == "sejour" & !is.na(dateDebut) & !is.na(dateFin)
  ])
  setorder(dt, id, dateDebut)
  dt[, finPrec := shift(dateFin, 1L), by = id]
  dt[, idPrec  := shift(id, 1L)]
  res <- dt[
    !is.na(finPrec) & id == idPrec & dateDebut < finPrec, .(
      id,
      episodeId = as.character(episodeId),
      valeurObs = paste0(dateDebut, " < fin préc. ", finPrec),
      details   = paste0(
        "Séjour débute le ", dateDebut,
        " avant fin du séjour précédent (", finPrec, ")"
      )
    )
  ]
  verifierColonnes(res)
}

# ---- Section 6 : métier -------------------------------------

# R18 — Séjour sans structure
fnR18 <- function(tables, param) {
  dt <- tables$factEpisode
  res <- dt[
    typeEvenement == "sejour" & is.na(normChar(sejour)), .(
      id,
      episodeId = as.character(episodeId),
      valeurObs = NA_character_,
      details   = "Séjour sans structure associée"
    )
  ]
  verifierColonnes(res)
}

# R19 — Fugue pendant séjour actif
fnR19 <- function(tables, param) {
  fugues  <- tables$factEpisode[
    typeEvenement == "fugue" & !is.na(dateDebut)
  ]
  sejours <- tables$factEpisode[
    typeEvenement == "sejour" & !is.na(dateDebut) & !is.na(dateFin)
  ]
  
  res <- purrr::map(unique(fugues$id), function(jeuneId) {
    fuguesJ  <- fugues[id == jeuneId]
    sejoursJ <- sejours[id == jeuneId]
    
    if (nrow(sejoursJ) == 0L) {
      return(fuguesJ[, .(
        id,
        episodeId = as.character(episodeId),
        valeurObs = as.character(dateDebut),
        details   = "Fugue sans aucun séjour pour ce jeune"
      )])
    }
    
    couverte <- purrr::map_lgl(
      seq_len(nrow(fuguesJ)),
      function(i) {
        any(
          sejoursJ$dateDebut <= fuguesJ$dateDebut[i] &
            sejoursJ$dateFin   >= fuguesJ$dateDebut[i]
        )
      }
    )
    
    fuguesJ[!couverte, .(
      id,
      episodeId = as.character(episodeId),
      valeurObs = as.character(dateDebut),
      details   = paste0(
        "Fugue le ", dateDebut, " sans séjour actif correspondant"
      )
    )]
  }) |>
    purrr::keep(~ !is.null(.x) && nrow(.x) > 0L) |>
    rbindlist(use.names = TRUE)
  
  if (nrow(res) == 0L) return(aucunCas())
  verifierColonnes(res)
}

# R20 — Cohérence sortie majorité / date majorité
fnR20 <- function(tables, param) {
  dt <- tables$factEpisode[
    grepl("majorit", destination, ignore.case = TRUE) & !is.na(dateFin)
  ]
  if (nrow(dt) == 0L) return(aucunCas())
  
  dt <- merge(
    dt[, .(id, episodeId, dateFin)],
    tables$dimJeune[, .(id, dateMajorite)],
    by = "id"
  )
  dt[, ecart := abs(as.integer(dateFin - dateMajorite))]
  
  res <- dt[ecart > param$toleranceMajorite, .(
    id,
    episodeId = as.character(episodeId),
    valeurObs = paste0("sortie=", dateFin, " | maj=", dateMajorite),
    details   = paste0(
      "Écart de ", ecart,
      "j entre sortie et majorité (tolérance ", param$toleranceMajorite, "j)"
    )
  )]
  verifierColonnes(res)
}

# R21 — Provenance première admission manquante
fnR21 <- function(tables, param) {
  dt <- tables$factEpisode[typeEvenement == "admission"]
  premiers <- dt[order(dateDebut), .SD[1L], by = id]
  res <- premiers[is.na(normChar(provenance)), .(
    id,
    episodeId = as.character(episodeId),
    valeurObs = NA_character_,
    details   = "Provenance manquante pour la 1ère admission"
  )]
  verifierColonnes(res)
}

# R22 — Destination dernière admission manquante
fnR22 <- function(tables, param) {
  dt <- tables$factEpisode[typeEvenement == "admission"]
  derniers <- dt[order(dateDebut), .SD[.N], by = id]
  res <- derniers[is.na(normChar(destination)), .(
    id,
    episodeId = as.character(episodeId),
    valeurObs = NA_character_,
    details   = "Destination manquante pour la dernière admission"
  )]
  verifierColonnes(res)
}

# ---- Section 9 : complétude ---------------------------------

# R23 — Taux NA motifs de placement
fnR23 <- function(tables, param) {
  dt <- tables$factEpisode[typeEvenement == "admission"]
  if (!"motifsDePlacement" %in% names(dt)) return(aucunCas())
  
  tauxNa <- mean(is.na(normChar(dt$motifsDePlacement)))
  if (tauxNa <= param$seuilNaCritique) return(aucunCas())
  
  data.table(
    id        = NA_character_,
    episodeId = NA_character_,
    valeurObs = sprintf("%.1f%%", tauxNa * 100),
    details   = paste0(
      "Taux NA motifs = ", sprintf("%.1f%%", tauxNa * 100),
      " > seuil ", param$seuilNaCritique * 100, "%"
    )
  ) |> verifierColonnes()
}

# R24 — Épisodes en cours sans date fin
fnR24 <- function(tables, param) {
  dt <- tables$factEpisode
  res <- dt[is.na(dateFin), .(
    id,
    episodeId = as.character(episodeId),
    valeurObs = typeEvenement,
    details   = paste0(
      "Épisode '", typeEvenement, "' sans date fin (en cours/censure)"
    )
  )]
  verifierColonnes(res)
}

# R25 — Jeune sans séjour (trajectoire non reconstituable)
fnR25 <- function(tables, param) {
  idsAvecSej <- unique(tables$factEpisode[typeEvenement == "sejour", id])
  idsSans    <- setdiff(tables$dimJeune$id, idsAvecSej)
  if (length(idsSans) == 0L) return(aucunCas())
  data.table(
    id        = idsSans,
    episodeId = NA_character_,
    valeurObs = NA_character_,
    details   = "Jeune sans séjour — trajectoire non reconstituable"
  ) |> verifierColonnes()
}

# ============================================================
# 3. REGISTRE DES RÈGLES
# Structure déclarative : chaque règle est une ligne du registre
# Ajouter une règle = ajouter une entrée ici + écrire sa fn
# ============================================================

registreRegles <- list(
  
  # --- dim_jeune ---
  list(id="R01", label="Unicité ID jeune",
       section="S1-Général",   severite="ERREUR", table="dim_jeune",   fn=fnR01),
  list(id="R02", label="Naissance renseignée",
       section="S2-Individus", severite="ERREUR", table="dim_jeune",   fn=fnR02),
  list(id="R03", label="Cohérence majorité/naissance",
       section="S2-Individus", severite="ALERTE", table="dim_jeune",   fn=fnR03),
  list(id="R04", label="Âge admission plausible",
       section="S2-Individus", severite="ALERTE", table="dim_jeune",   fn=fnR04),
  list(id="R05", label="Sexe renseigné",
       section="S9-Complétude",severite="ALERTE", table="dim_jeune",   fn=fnR05),
  
  # --- fact_episode ---
  list(id="R06", label="Type épisode valide",
       section="S3-Épisodes",  severite="ERREUR", table="fact_episode",fn=fnR06),
  list(id="R07", label="Date début renseignée",
       section="S3-Épisodes",  severite="ERREUR", table="fact_episode",fn=fnR07),
  list(id="R08", label="Date fin >= date début",
       section="S3-Épisodes",  severite="ERREUR", table="fact_episode",fn=fnR08),
  list(id="R09", label="Cohérence durée calculée/stockée",
       section="S8-Durées",    severite="ALERTE", table="fact_episode",fn=fnR09),
  list(id="R10", label="Durée non négative",
       section="S8-Durées",    severite="ERREUR", table="fact_episode",fn=fnR10),
  list(id="R11", label="Durée non excessive",
       section="S8-Durées",    severite="ALERTE", table="fact_episode",fn=fnR11),
  list(id="R12", label="Ordre chronologique",
       section="S3-Épisodes",  severite="ERREUR", table="fact_episode",fn=fnR12),
  
  # --- relations ---
  list(id="R13", label="FK jeune valide",
       section="S4-Relations", severite="ERREUR", table="fact_episode",fn=fnR13),
  list(id="R14", label="Structure dans référentiel",
       section="S4-Relations", severite="ALERTE", table="fact_episode",fn=fnR14),
  list(id="R15", label="Jeune avec au moins un épisode",
       section="S4-Relations", severite="ALERTE", table="dim_jeune",   fn=fnR15),
  
  # --- temporel ---
  list(id="R16", label="Épisode avant majorité",
       section="S5-Temporel",  severite="ALERTE", table="fact_episode",fn=fnR16),
  list(id="R17", label="Pas de chevauchement séjours",
       section="S5-Temporel",  severite="ERREUR", table="fact_episode",fn=fnR17),
  
  # --- métier ---
  list(id="R18", label="Séjour avec structure",
       section="S6-Métier",    severite="ERREUR", table="fact_episode",fn=fnR18),
  list(id="R19", label="Fugue pendant séjour actif",
       section="S6-Métier",    severite="ALERTE", table="fact_episode",fn=fnR19),
  list(id="R20", label="Cohérence sortie majorité",
       section="S6-Métier",    severite="ALERTE", table="fact_episode",fn=fnR20),
  list(id="R21", label="Provenance 1ère admission",
       section="S6-Métier",    severite="ALERTE", table="fact_episode",fn=fnR21),
  list(id="R22", label="Destination dernière admission",
       section="S6-Métier",    severite="ALERTE", table="fact_episode",fn=fnR22),
  
  # --- complétude ---
  list(id="R23", label="Complétude motifs placement",
       section="S9-Complétude",severite="ALERTE", table="fact_episode",fn=fnR23),
  list(id="R24", label="Épisodes en cours identifiés",
       section="S9-Complétude",severite="INFO",   table="fact_episode",fn=fnR24),
  list(id="R25", label="Trajectoire reconstituable",
       section="S10-Trajectoires",severite="ALERTE",table="dim_jeune", fn=fnR25)
)

# ============================================================
# 4. RUNNER — applique toutes les règles via purrr::map
# ============================================================

lancerValidation <- function(tables, registre, param) {
  
  cat(strrep("=", 60), "\n")
  cat("  VALIDATION EN COURS...\n")
  cat(strrep("=", 60), "\n")
  
  # Application de chaque règle
  resultats <- purrr::map(registre, function(regle) {
    
    cat(sprintf("  [%s] %s\n", regle$id, regle$label))
    
    # Exécution défensive : capture les erreurs sans tout arrêter
    cas <- tryCatch(
      regle$fn(tables, param),
      error = function(e) {
        warning(sprintf("[%s] Erreur : %s", regle$id, conditionMessage(e)))
        data.table(
          id        = NA_character_,
          episodeId = NA_character_,
          valeurObs = NA_character_,
          details   = paste0("ERREUR D'EXÉCUTION : ", conditionMessage(e))
        )
      }
    )
    
    # Si aucun problème : retourner NULL
    if (is.null(cas) || nrow(cas) == 0L) return(NULL)
    
    # Enrichissement avec les métadonnées de la règle
    cas[, `:=`(
      regleId    = regle$id,
      regleLabel = regle$label,
      section    = regle$section,
      severite   = regle$severite,
      table      = regle$table
    )]
    cas
  })
  
  # Agrégation : supprimer les NULL et empiler
  casProblematiques <- resultats |>
    purrr::keep(~ !is.null(.x)) |>
    rbindlist(use.names = TRUE, fill = TRUE)
  
  # Réordonnancement des colonnes pour lisibilité
  setcolorder(casProblematiques, c(
    "regleId", "regleLabel", "section", "severite", "table",
    "id", "episodeId", "valeurObs", "details"
  ))
  setorder(casProblematiques, regleId, id)
  
  cat(strrep("=", 60), "\n")
  cat(sprintf(
    "  Terminé : %d erreurs | %d alertes | %d total\n",
    sum(casProblematiques$severite == "ERREUR"),
    sum(casProblematiques$severite == "ALERTE"),
    nrow(casProblematiques)
  ))
  cat(strrep("=", 60), "\n\n")
  
  casProblematiques
}

# ============================================================
# 5. RAPPORT ET EXPORT
# ============================================================

afficherRapport <- function(casProblematiques) {
  
  # Résumé par règle
  resumeParRegle <- casProblematiques[, .(
    nCas     = .N,
    severite = severite[1L],
    section  = section[1L],
    table    = table[1L]
  ), by = .(regleId, regleLabel)][order(regleId)]
  
  cat("\n  RÉSUMÉ PAR RÈGLE\n")
  cat(strrep("-", 60), "\n")
  print(resumeParRegle)
  
  invisible(list(
    resume = resumeParRegle,
    detail = casProblematiques
  ))
}

exporterRapport <- function(casProblematiques,
                            dossier = "output",
                            prefixe = "validation") {
  if (!dir.exists(dossier)) dir.create(dossier, recursive = TRUE)
  
  resumeParRegle <- casProblematiques[, .(
    nCas     = .N,
    severite = severite[1L],
    section  = section[1L],
    table    = table[1L]
  ), by = .(regleId, regleLabel)][order(regleId)]
  
  fwrite(resumeParRegle,    file.path(dossier,
                                      paste0(prefixe, "_resume_", Sys.Date(), ".csv")), bom = TRUE)
  fwrite(casProblematiques, file.path(dossier,
                                      paste0(prefixe, "_detail_", Sys.Date(), ".csv")), bom = TRUE)
  
  cat("✅ Exports réalisés dans :", dossier, "\n")
  invisible(list(resume = resumeParRegle, detail = casProblematiques))
}

# ============================================================
# 6. POINT D'ENTRÉE
# ============================================================

# Regroupement des tables dans une liste (interface unique du runner)
tables <- list(
  dimJeune     = dimJeune,
  factEpisode  = factEpisode
)

# Lancement
casProblematiques <- lancerValidation(tables, registreRegles, PARAM)
afficherRapport(casProblematiques)
exporterRapport(casProblematiques, dossier = "output")
