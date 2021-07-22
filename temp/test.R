library(BilanROE)

CheminExportROE2020 <- "temp/geobs_interne2020_09_22.zip"
CheminExportROE2021 <- "temp/geobs_interne2021_03_13_sansRadars.zip"
Bassins <- NULL
Departements <- c("75", "77", "78", "91", "92", "93", "94", "95")
CheminExportBDOE <- "temp/20210505_143734873_MONDY_export_bdoe_total.zip"

DonneesBrutes2020 <- importer_donnees(
    chemin_archive = CheminExportROE2020,
    bassins = Bassins,
    departements = Departements
)

DonneesBrutes2021 <- importer_donnees(
    chemin_archive = CheminExportROE2021,
    bassins = Bassins,
    departements = Departements
)

Listes1et2 <- importer_listes_bdoe(CheminExportBDOE)

OuvragesPrioritaires <- readxl::read_xlsx(
    path = "temp/200525_ouvrages_prioritaires.xlsx"
    ) %>%
    dplyr::select(identifiant_roe = `Code ROE`) %>%
    dplyr::mutate(prioritaire = TRUE)

DonneesBilan2020 <- preparer_donnees_bilan(
    donnees_brutes = DonneesBrutes2020,
    # ATTENTION les ouvrages non validés ou gelés ne sont pas intégrés dans
    # l'export de la BDOE, il faut donc vérifier, en particulier pour les
    # ouvrages non validés s'ils sont localisés sur des cours d'eau classés ou
    # non
    listes = Listes1et2,
    ouvrages_prioritaires = OuvragesPrioritaires
) %>%
    dplyr::mutate(dept_nom = factor(
        dept_nom,
        levels = c(
            "PARIS", "HAUTS-DE-SEINE", "SEINE-SAINT-DENIS", "VAL-DE-MARNE",
            "ESSONNE", "YVELINES", "VAL-D'OISE", "SEINE-ET-MARNE"
            )
        ))

DonneesBilan2021 <- preparer_donnees_bilan(
    donnees_brutes = DonneesBrutes2021,
    # ATTENTION les ouvrages non validés ou gelés ne sont pas intégrés dans
    # l'export de la BDOE, il faut donc vérifier, en particulier pour les
    # ouvrages non validés s'ils sont localisés sur des cours d'eau classés ou
    # non
    listes = Listes1et2,
    ouvrages_prioritaires = OuvragesPrioritaires
) %>%
    dplyr::mutate(dept_nom = factor(
        dept_nom,
        levels = c(
            "PARIS", "HAUTS-DE-SEINE", "SEINE-SAINT-DENIS", "VAL-DE-MARNE",
            "ESSONNE", "YVELINES", "VAL-D'OISE", "SEINE-ET-MARNE"
        )
    ))

