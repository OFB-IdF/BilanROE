library(BilanROE)
library(templatesOFB)


CheminExportROE2020 <- "temp/geobs_interne2020_09_22.zip"
CheminExportROE202103 <- "temp/geobs_interne2021_03_13_sansRadars.zip"
CheminExportROE202106 <- "temp/geobs_interne2021_06_20_sansRadars.zip"
Bassins <- NULL
Departements <- c("75", "77", "78", "91", "92", "93", "94", "95")
# Departements <- c("78", "95")
# CheminExportBDOE <- "temp/20210505_143734873_MONDY_export_bdoe_total.zip"
CheminBdTopo <- file.path(
    Sys.getenv("GEOFB_data_path_local"),
    "VECTEUR/hydrographie/BDTOPO_IdF_2019-12-16/COURS_D_EAU_IDF.shp"
    )
CheminListes2 <- file.path(
    Sys.getenv("GEOFB_data_path_local"),
    "VECTEUR/classements/COUCHES_SIG_CLASSEMENTS_L1_et_2_2012-2/liste_2_gpkg.gpkg"
)
CheminMasqueIDF <- file.path(
    Sys.getenv("GEOFB_data_path_local"),
    "VECTEUR/masques/IdF/MASQUE_IdF.shp"
)

DonneesBrutes2020 <- importer_donnees(
    chemin_archive = CheminExportROE2020,
    bassins = Bassins,
    departements = Departements
)

DonneesBrutes202103 <- importer_donnees(
    chemin_archive = CheminExportROE202103,
    bassins = Bassins,
    departements = Departements
)

DonneesBrutes202106 <- importer_donnees(
    chemin_archive = CheminExportROE202106,
    bassins = Bassins,
    departements = Departements
)

MasqueIDF <- sf::st_read(CheminMasqueIDF)
ReseauHydro <- sf::st_read(CheminBdTopo)
Listes2 <- sf::st_read(CheminListes2, layer = "liste_2_gpkg") %>%
    sf::st_intersection(MasqueIDF)

# Listes1et2 <- importer_listes_bdoe(CheminExportBDOE)

OuvragesPrioritaires <- readxl::read_xlsx(
    path = "temp/200525_ouvrages_prioritaires.xlsx"
    ) %>%
    dplyr::select(identifiant_roe = `Code ROE`) %>%
    dplyr::mutate(prioritaire = TRUE)

DonneesBilan2020 <- preparer_donnees_bilan(
    donnees_brutes = DonneesBrutes2020,
    ouvrages_prioritaires = OuvragesPrioritaires
) %>%
    dplyr::mutate(dept_nom = factor(
        dept_nom,
        levels = c(
            "PARIS", "HAUTS-DE-SEINE", "SEINE-SAINT-DENIS", "VAL-DE-MARNE",
            "ESSONNE", "YVELINES", "VAL-D'OISE", "SEINE-ET-MARNE"
            )
        ))

DonneesBilan202103 <- preparer_donnees_bilan(
    donnees_brutes = DonneesBrutes202103,
    ouvrages_prioritaires = OuvragesPrioritaires
) %>%
    dplyr::mutate(dept_nom = factor(
        dept_nom,
        levels = c(
            "PARIS", "HAUTS-DE-SEINE", "SEINE-SAINT-DENIS", "VAL-DE-MARNE",
            "ESSONNE", "YVELINES", "VAL-D'OISE", "SEINE-ET-MARNE"
        )
    ))

DonneesBilan202106 <- preparer_donnees_bilan(
    donnees_brutes = DonneesBrutes202106,
    # ATTENTION les ouvrages non validés ou gelés ne sont pas intégrés dans
    # l'export de la BDOE, il faut donc vérifier, en particulier pour les
    # ouvrages non validés s'ils sont localisés sur des cours d'eau classés ou
    # non
    # listes = Listes1et2,
    ouvrages_prioritaires = OuvragesPrioritaires
) %>%
    dplyr::mutate(dept_nom = factor(
        dept_nom,
        levels = c(
            "PARIS", "HAUTS-DE-SEINE", "SEINE-SAINT-DENIS", "VAL-DE-MARNE",
            "ESSONNE", "YVELINES", "VAL-D'OISE", "SEINE-ET-MARNE"
        )
    ))

DonneesBilan2020 %>%
    synthetiser_validation(dept_nom)
DonneesBilan202106 %>%
    synthetiser_validation(dept_nom)

visualiser_evolution_validations(
    list(
        "Septembre 2020" = DonneesBilan2020,
        "Mars 2021" = DonneesBilan202103,
        "Juin 2021" = DonneesBilan202106
    ),
    log_y = TRUE,
    dept_nom
) +
    templatesOFB::theme_ofb() +
    ggplot2::theme(
        # strip.text = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank()
    )

DonneesBilan202106 %>%
    visualiser_validation(dept_nom)

DonneesBilan202106 %>%
    visualiser_evolution_roe(groupe = dept_nom, log_y = TRUE) +
    ggplot2::scale_colour_manual(
        values = c(
            "PARIS" = templatesOFB::ofb_cols("rouge")[[1]],
            "SEINE-SAINT-DENIS" = templatesOFB::ofb_cols("rouge")[[1]],
            "HAUTS-DE-SEINE" = templatesOFB::ofb_cols("rouge")[[1]],
            "VAL-DE-MARNE" = templatesOFB::ofb_cols("rouge")[[1]],
            "ESSONNE" = templatesOFB::ofb_cols("bleu1")[[1]],
            "SEINE-ET-MARNE" = templatesOFB::ofb_cols("vert1")[[1]],
            "YVELINES" = templatesOFB::ofb_cols("orange2")[[1]],
            "VAL-D'OISE" = templatesOFB::ofb_cols("orange2")[[1]]
        )
    ) +
    templatesOFB::theme_ofb() +
    ggplot2::theme(
        legend.position = "none"
    )

DonneesBilan202106 %>%
    synthetiser_completude(dept_nom, prioritaire)


DonneesBilan202106  %>%
    visualiser_completude(
        visualiser_prioritaires = TRUE,
        groupe = dept_nom
    )



(
    DonneesBilan202106 %>%
        cartographier_completude(
            reseau_hydro = ReseauHydro,
            listes2 = Listes2
        ) +
        templatesOFB::theme_ofb() +
        ggplot2::theme(
            axis.line = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            axis.text = ggplot2::element_blank()
        ) +
        templatesOFB::scale_fill_ofb(
            name = "information manquante",
            palette = "principale"
        )
) %>%
    templatesOFB::compose_ofb(src = "ROE")

