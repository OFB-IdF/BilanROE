library(BilanROE)

Bassins <- NULL
Departements <- c("75", "77", "78", "91", "92", "93", "94", "95")

DonneesBrutes_1 <- importer_donnees(
    chemin_archive = "dev/geobs_interne2020_09_22.zip",
    bassins = Bassins,
    departements = Departements
)

DonneesBrutes_2 <- importer_donnees(
    chemin_archive = "dev/geobs_interne2021_06_20_sansRadars.zip",
    bassins = Bassins,
    departements = Departements
)

DonneesBrutes_3 <- importer_donnees(
    chemin_archive = "dev/geobs_interne2022_07_04_SansRadars.zip",
    bassins = Bassins,
    departements = Departements
)

OuvragesPrioritaires <- readxl::read_xlsx(
    path = "dev/200525_ouvrages_prioritaires.xlsx") %>%
    dplyr::select(identifiant_roe = `Code ROE`) %>%
    dplyr::mutate(prioritaire = TRUE)

TypologieOuvrages <- vroom::vroom(
    file = "dev/typologie_ouvrages.csv",
    col_types = list(
        .default = vroom::col_character()
    )
)

LimitesZone <- telecharger_limites_region(codes_region = "11")

ReseauHydro <- telecharger_ce_topage(bassin = "03", limites_zone = LimitesZone)

NomsCeTopo <-  importer_donnees(
    chemin_archive = "dev/geobs_interne2022_07_04_SansRadars.zip"
) %>%
    dplyr::distinct(id_troncon_topo, nom_topo)

MassesEau <- telecharger_masses_eau(limites_zone = LimitesZone)

Listes2 <- telecharger_listes2(limites_zone = LimitesZone)

usethis::use_data(
    DonneesBrutes_1, DonneesBrutes_2, DonneesBrutes_3,
    OuvragesPrioritaires,
    TypologieOuvrages,
    LimitesZone,
    ReseauHydro, NomsCeTopo,
    MassesEau,
    Listes2,
    internal = TRUE,
    overwrite = TRUE
)

