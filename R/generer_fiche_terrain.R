
#' Générer des fiches terrain ROE
#'
#' Cette fonction permet de générer des fiches terrain pré-remplies avec les
#' données issues d'un export du ROE. Ces fiches pourront alors servir lors de
#' visites sur site pour la mise en qualité des données du ROE.
#'
#' @param donnees données issus d'un export ROE
#' @param codes_roe vecteur de codes ROE des obstacles pour lesquels on veut
#'   générer des fiches terrain.
#' @param date_export date à laquelle l'export du ROE utilisé a été réalisée
#' @param dossier_sortie chemin du dossier dans lequel seront enregistrées les
#'   fiches terrain générées.
#' @param sous_dossiers vecteur contenant les noms de colonnes du tableau
#'   `donnees` qui serviront à hiérarchiser les enregistrements des fiches (e.g.
#'   par département en spécifiant "dept_nom")
#' @param fond_carte fond de carte, objet raster de classe stars
#'
#' @export
#'
#' @importFrom dplyr mutate filter distinct group_by summarise pull rename
#'   bind_cols case_when select starts_with all_of
#' @importFrom openxlsx loadWorkbook writeData openxlsx_setOp saveWorkbook
#' @importFrom progress progress_bar
#' @importFrom purrr walk map
#' @importFrom sf st_as_sf st_transform st_coordinates st_drop_geometry
#' @importFrom stringr str_split str_detect
#' @importFrom tidyr pivot_longer
generer_fiches_terrain <- function(donnees, codes_roe = NULL, date_export, dossier_sortie = getwd(), sous_dossiers = NULL, fond_carte) {

    formater_cellule <- function(nom_cellule) {
        nom_cellule %>%
            stringr::str_split(pattern = "-") %>%
            '[['(1)
    }

    donnees <- donnees %>%
        dplyr::mutate(
            statut = ifelse(
                test = statut_nom %in% c("Validé par échantillonnage", "Validé point par point"),
                yes = iconv(x = "Validé", to = "latin1"),
                no = statut_nom
            )
        )

    FichiersSortie <- donnees %>%
        dplyr::mutate(dossier = dossier_sortie) %>%
        dplyr::select(dossier, dplyr::all_of(sous_dossiers), identifiant_roe) %>%
        tidyr::pivot_longer(cols = -identifiant_roe) %>%
        dplyr::group_by(identifiant_roe) %>%
        dplyr::summarise(chemin = paste(value, collapse = "/")) %>%
        dplyr::mutate(fichier_sortie = paste0(chemin, "/FicheTerrain_", identifiant_roe, ".xlsx"))

    donnees <- donnees %>%
        dplyr::left_join(FichiersSortie, by = "identifiant_roe")

    if (is.null(codes_roe))
        codes_roe <- unique(donnees$identifiant_roe)

    donnees <- donnees %>%
        dplyr::filter(identifiant_roe %in% codes_roe) %>%
        dplyr::arrange(fichier_sortie)

    if (!is.null(sous_dossiers)) {
        donnees %>%
            dplyr::distinct(across(all_of(sous_dossiers))) %>%
            dplyr::mutate(id = seq(dplyr::n())) %>%
            tidyr::pivot_longer(
                cols = -id,
                names_to = "niveau",
                values_to = "valeur"
                ) %>%
            dplyr::group_by(id) %>%
            dplyr::summarise(
                chemin = paste(c(dossier_sortie, valeur), collapse = "/")
                ) %>%
            dplyr::pull(chemin) %>%
            purrr::walk(
                .f = function(x) {
                    if (!dir.exists(x))
                        dir.create(path = x, recursive = TRUE)
                }
            )

    }

    pb <- progress::progress_bar$new(
        total = length(codes_roe),
        format = "[:bar] (:eta)"
    )

    generer_carte <- function(code_roe) {
        pb$tick()

        donnees_sf <- donnees %>%
            sf::st_as_sf(
                coords = c("x_l93", "y_l93"),
                crs = 2154,
                remove = FALSE
            )

        donnees_ouvrage <- donnees_sf %>%
            dplyr::filter(identifiant_roe %in% code_roe)

        ouvrage_bbox <- donnees_ouvrage %>%
            sf::st_buffer(625) %>%
            sf::st_bbox()

        donnees_ouvrage_prox <- donnees_sf %>%
            dplyr::filter(
                x_l93 >= ouvrage_bbox["xmin"] & x_l93 <= ouvrage_bbox["xmax"] &
                    y_l93 >= ouvrage_bbox["ymin"] & y_l93 <= ouvrage_bbox["ymax"] &
                    identifiant_roe != code_roe
            )

        fond_ouvrage <- fond_carte %>%
            sf::st_crop(ouvrage_bbox) %>%
            stars::st_as_stars() %>%
            stars::st_rgb()

        carte_ouvrage <- ggplot2::ggplot() +
            stars::geom_stars(data = fond_ouvrage) +
            ggplot2::geom_sf(data = donnees_ouvrage, size = 5.75, colour = "black") +
            ggplot2::geom_sf(data = donnees_ouvrage, size = 5, colour = "darkgrey") +
            ggplot2::geom_sf(data = donnees_ouvrage_prox,
                             colour = "white",
                             size = 3.25
            ) +
            ggplot2::geom_sf(data = donnees_ouvrage_prox,
                             colour = "black",
                             size = 2.5
            ) +
            ggplot2::coord_sf(
                xlim = ouvrage_bbox[c("xmin", "xmax")],
                ylim = ouvrage_bbox[c("ymin", "ymax")], clip = "off"
            ) +
            ggplot2::theme_void()

        carte_ouvrage
        # ggplot2::ggsave(
        #     plot = carte_ouvrage,
        #     filename = stringr::str_replace(unique(donnees_ouvrage$fichier_sortie), pattern = ".xlsx", replacement = ".png"),
        #     width = 15, height = 15, units = "cm",
        #     dpi = 300
        # )
    }

    generer_fiche_terrain <- function(code_roe) {
        pb$tick()

        FicheTerrain <- openxlsx::loadWorkbook(
            file = system.file(
                "extdata",
                "FicheTerrain.xlsx",
                package = "BilanROE"
                )
        )

        ecrire_donnee <- function(cellule, valeur) {
            if (all(!is.na(cellule)))
                openxlsx::writeData(
                    wb = FicheTerrain,
                    sheet = 1,
                    x = valeur,
                    xy = cellule
                )
        }

        donnees_ouvrage <- donnees %>%
            dplyr::filter(identifiant_roe == code_roe) %>%
            sf::st_as_sf(
                coords = c("x_l93", "y_l93"),
                crs = 2154,
                remove = FALSE
                ) %>%
            (function(df) {
                coords_wgs84 <- df %>%
                    sf::st_transform(crs = 4326) %>%
                    sf::st_coordinates() %>%
                    as.data.frame() %>%
                    dplyr::rename(
                        x_wgs84 = X, y_wgs84 = Y
                    )

                dplyr::bind_cols(
                    df, coords_wgs84
                )

            }) %>%
            sf::st_drop_geometry()

        openxlsx::openxlsx_setOp("keepNA", FALSE)

        # Identification et implantation de l'ouvrage----
        if (is.na(donnees_ouvrage$nom_principal))
            ecrire_donnee(
                valeur = "Nom de l'ouvrage: ",
                cellule = c("D", 9)
            )
        ecrire_donnee(
            valeur = donnees_ouvrage$nom_principal,
            cellule = c("H", 9)
        )
        ecrire_donnee(
            valeur = donnees_ouvrage$identifiant_roe,
            cellule = c("Q", 9)
        )
        ecrire_donnee(
            valeur = donnees_ouvrage$nom_topo,
            cellule = c("H", 10)
        )
        if (any(is.na(c(donnees_ouvrage$x_wgs84, donnees_ouvrage$y_wgs84))))
            ecrire_donnee(
                valeur = iconv("Coordonnées GPS: ", to = "latin1"),
                cellule = c("D", 11)
            )
        ecrire_donnee(
            valeur = donnees_ouvrage$x_wgs84,
            cellule = c("J", 11)
        )
        ecrire_donnee(
            valeur = donnees_ouvrage$y_wgs84,
            cellule = c("O", 11)
        )
        ecrire_donnee(
            valeur = donnees_ouvrage$date_construction_ouvrage,
            cellule = c("H", 14)
        )
        ecrire_donnee(
            valeur = donnees_ouvrage$statut,
            cellule = c("Q", 14)
        )

        # Etat----
        if (is.na(donnees_ouvrage$etat_nom))
            ecrire_donnee(
                valeur = "Etat: ",
                cellule = c("D", 17)
            )

        cellule_etat <- dplyr::case_when(
            donnees_ouvrage$etat_nom == "En projet" ~ "F-17",
            donnees_ouvrage$etat_nom == "En construction" ~ "H-17",
            donnees_ouvrage$etat_nom == "Existant" ~ "M-17",
            donnees_ouvrage$etat_nom == "Détruit partiellement" ~ "F-18",
            donnees_ouvrage$etat_nom == "Détruit entièrement" ~ "H-18"
        ) %>%
            formater_cellule()
        ecrire_donnee(
            valeur = "X",
            cellule = cellule_etat
        )

        # Typologie-----
        if (is.na(donnees_ouvrage$type_nom))
            ecrire_donnee(
                valeur = iconv("Eléments fixes ", to = "latin1"),
                cellule = c("B", 21)
            )

        cellule_type <- dplyr::case_when(
            donnees_ouvrage$type_nom == "Barrage" ~ "B-22",
            donnees_ouvrage$type_nom == "Seuil en rivière" ~ "B-34",
            donnees_ouvrage$type_nom == "Obstacle induit par un pont" ~ "H-22",
            donnees_ouvrage$type_nom == "Digue" ~ "H-30",
            donnees_ouvrage$type_nom == "Grille de pisciculture" ~ "H-36",
            donnees_ouvrage$type_nom == "Epis en rivière" ~ "H-38"
        ) %>%
            formater_cellule()
        ecrire_donnee(
            valeur = "X",
            cellule = cellule_type
        )

        cellule_sous_type <- dplyr::case_when(
            ## Barrage----
            donnees_ouvrage$stype_nom == "Barrage-poids" ~ "D-23",
            donnees_ouvrage$stype_nom == "Barrage-voûte" ~ "D-24",
            donnees_ouvrage$stype_nom == "Barrage-poids-voûte" ~ "D-25",
            donnees_ouvrage$stype_nom == "Barrage à contreforts" ~ "D-26",
            donnees_ouvrage$stype_nom == "Barrage à voûtes multiples" ~ "D-27",
            donnees_ouvrage$stype_nom == "Barrage mobile" ~ "D-29",
            donnees_ouvrage$stype_nom == "Barrage en remblais" ~ "D-30",
            donnees_ouvrage$stype_nom == "Sous-type de barrage inconnu" ~ "D-31",
            donnees_ouvrage$stype_nom == "Autre sous-type de barrage (préciser)" ~ "D-32",
            ## Seuil en rivière----
            donnees_ouvrage$stype_nom == "Déversoir" ~ "D-35",
            donnees_ouvrage$stype_nom == "Radier" ~ "D-36",
            donnees_ouvrage$stype_nom == "Enrochements" ~ "D-37",
            donnees_ouvrage$stype_nom == "Sous-type de seuil en rivière inconnu" ~ "D-38",
            donnees_ouvrage$stype_nom == "Autre sous-type de seuil en rivière (préciser)" ~ "D-39",
            ## Obstacle induit par un pont----
            donnees_ouvrage$stype_nom == "Radier de pont" ~ "J-23",
            donnees_ouvrage$stype_nom == "Buse(s)" ~ "J-24",
            donnees_ouvrage$stype_nom == "Passage à gué" ~ "J-25",
            donnees_ouvrage$stype_nom == "Sous-type inconnu d'obstacle induit par un pont" ~ "J-26",
            donnees_ouvrage$stype_nom == "Autre sous-type d'obstacle induit par un pont (préciser)" ~ "J-27",
            ## Digue----
            donnees_ouvrage$stype_nom == "Digue (longitudinale) de canaux" ~ "J-31",
            donnees_ouvrage$stype_nom == "Digue (longitudinale) de protection contre les inondations" ~ "J-32",
            donnees_ouvrage$stype_nom == "Digue (longitudinale) mixte (canaux + protection inondation)" ~ "J-34"
        ) %>%
            formater_cellule()
        ecrire_donnee(
            valeur = "X",
            cellule = cellule_sous_type
        )

        if (!is.na(donnees_ouvrage$type_nom) & !is.na(donnees_ouvrage$stype_nom)) {
            if (
                donnees_ouvrage$type_nom == "Barrage" &
                donnees_ouvrage$stype_nom == "Autre sous-type de barrage (préciser)"
            )
                ecrire_donnee(
                    valeur = "... ",
                    cellule = c("E", 33)
                )

            if (
                donnees_ouvrage$type_nom == "Seuil en rivière" &
                donnees_ouvrage$stype_nom == "Autre sous-type de seuil en rivière (préciser)"
            )
                ecrire_donnee(
                    valeur = "... ",
                    cellule = c("E", 40)
                )

            if (
                donnees_ouvrage$type_nom == "Obstacle induit par un pont" &
                donnees_ouvrage$stype_nom == "Autre sous-type d'obstacle induit par un pont (préciser)"
            )
                ecrire_donnee(
                    valeur = "... ",
                    cellule = c("K", 28)
                )
        }


        elements_mobiles <- donnees_ouvrage %>%
            dplyr::select(
                identifiant_roe,
                dplyr::starts_with("emo_nom")
            ) %>%
            tidyr::pivot_longer(
                cols = dplyr::starts_with("emo"),
                names_to = "colonne",
                values_to = "element_mobile"
            ) %>%
            dplyr::filter(
                !(stringr::str_detect(colonne, "emo_nom") & colonne != "emo_nom1" & is.na(element_mobile))
            ) %>%
            dplyr::pull(element_mobile)

        cellule_element_mobile <- elements_mobiles %>%
            purrr::map(
                function(x) {
                    dplyr::case_when(
                        x == "Absence d'élément mobile" ~ "T-22",
                        x == "Clapet basculant" ~ "T-23",
                        x == "Vannes levantes" ~ "T-24",
                        x == "Autre type de vannes" ~ "T-25",
                        x == "Aiguilles" ~ "T-26",
                        x == "Hausses" ~ "T-27",
                        x == "Batardeau" ~ "T-28",
                        x == "Portes à flots" ~ "T-29",
                        x == "Clapets à marée" ~ "T-30",
                        x == "Type d'élément mobile inconnu" ~ "T-31",
                        x == "Autre type d'élément mobile (préciser)" ~ "T-32"
                    ) %>%
                        formater_cellule()
                }
            )

        purrr::walk(
            cellule_element_mobile,
            ecrire_donnee, valeur = "X"
            )

        if (
            any(!is.na(elements_mobiles)) &
            any(elements_mobiles == "Autre sous-type de barrage (préciser)")
        )
            ecrire_donnee(
                valeur = "... ",
                cellule = c("V", 33)
            )


        # Hauteur de chute à l'étiage----
        if (!is.na(donnees_ouvrage$hauteur_chute_etiage)) {
            ecrire_donnee(
                valeur = donnees_ouvrage$hauteur_chute_etiage,
                cellule = c("B", 45)
            )
        }

        cellule_classe_hauteur <- dplyr::case_when(
            donnees_ouvrage$hauteur_chute_etiage_classe == "Indéterminée" ~ "H-44",
            donnees_ouvrage$hauteur_chute_etiage_classe == "Inférieure à 0.5m" ~ "F-45",
            donnees_ouvrage$hauteur_chute_etiage_classe == "De 0.5m à inférieure à 1m" ~ "F-46",
            donnees_ouvrage$hauteur_chute_etiage_classe == "De 1m à inférieure à 1.5m" ~ "H-45",
            donnees_ouvrage$hauteur_chute_etiage_classe == "De 1.5m à inférieure à 2m" ~ "H-46",
            donnees_ouvrage$hauteur_chute_etiage_classe == "De 2m à inférieure à 3m" ~ "M-45",
            donnees_ouvrage$hauteur_chute_etiage_classe == "De 3m à inférieure à 5m" ~ "M-46",
            donnees_ouvrage$hauteur_chute_etiage_classe == "De 5m à inférieure à 10m" ~ "T-45",
            donnees_ouvrage$hauteur_chute_etiage_classe == "Supérieure ou égale à 10m" ~ "T-46"
        ) %>%
            formater_cellule()
        ecrire_donnee(
            valeur = "X",
            cellule = cellule_classe_hauteur
        )

        # Usages----
        usages <- donnees_ouvrage %>%
            dplyr::select(
                identifiant_roe,
                dplyr::starts_with("usage_nom")
            ) %>%
            tidyr::pivot_longer(
                cols = dplyr::starts_with("usage"),
                names_to = "colonne",
                values_to = "usage"
            ) %>%
            dplyr::filter(
                !(stringr::str_detect(colonne, "usage_nom") & colonne != "usage_nom1" & is.na(usage))
            ) %>%
            dplyr::pull(usage)

        cellule_usage <- usages %>%
            purrr::map(
                function(x) {
                    dplyr::case_when(
                        x == "Type d'usage inconnu" ~ "B-50",
                        x == "Alimentation en eau potable" ~ "B-51",
                        x == "Industrie" ~ "B-52",
                        x == "Agriculture (irrigation, abreuvage)" ~ "B-54",
                        x == "Loisirs et sports aquatiques" ~ "B-55",
                        x == "Energie et hydroélectricité" ~ "B-57",
                        x == "Activités aquacoles" ~ "H-50",
                        x == "Transports et soutien de navigation" ~ "H-53",
                        x == "Sécurité des biens et des personnes" ~ "H-54",
                        x == "Stabilisation du profil en long du lit, lutte contre l'érosion" ~ "Q-50",
                        x == "Suivi technique et scientifique (débit, température)" ~ "Q-52",
                        x == "Aucun" ~ "Q-54",
                        x == "Obsolète" ~ "Q-55",
                        x == "Autre usage (préciser)" ~ "Q-56"
                    ) %>%
                        formater_cellule()
                }
            )

        purrr::walk(
            cellule_usage,
            ecrire_donnee, valeur = "X"
            )

        if ("Industrie" %in% usages)
            ecrire_donnee(valeur = "Extraction granulats ", cellule = c("E", 53))
        if ("Loisirs et sports aquatiques" %in% usages)
            ecrire_donnee(valeur = "Baignade ", cellule = c("E", 56))
        if ("Activités aquacoles" %in% usages){
            ecrire_donnee(valeur = "Pisciculture ", cellule = c("K", 51))
            ecrire_donnee(
                valeur = iconv("Pêche professionnelle ", to = "latin1"),
                cellule = c("K", 52)
            )
        }
        if ("Sécurité des biens et des personnes" %in% usages){
            ecrire_donnee(
                valeur = iconv("Défense contre les crues ", to = "latin1"),
                cellule = c("K", 55)
            )
            ecrire_donnee(
                valeur = iconv("Soutien d'étiage ", to = "latin1"),
                cellule = c("K", 56)
            )
            ecrire_donnee(
                valeur = "Stockage de l'eau pour l'incendie ",
                cellule = c("K", 57)
            )
        }
        if ("Autre usage (préciser)" %in% usages)
            ecrire_donnee(valeur = "... ", cellule = c("S", 57))

        # Dispositif de franchissement piscicole----
        passes <- donnees_ouvrage %>%
            dplyr::select(
                identifiant_roe,
                dplyr::starts_with("fpi_nom")
            ) %>%
            tidyr::pivot_longer(
                cols = dplyr::starts_with("fpi"),
                names_to = "colonne",
                values_to = "passe"
            ) %>%
            dplyr::filter(
                !(stringr::str_detect(colonne, "fpi_nom") & colonne != "fpi_nom1" & is.na(passe))
            ) %>%
            dplyr::pull(passe)

        if ("Absence de passe" %in% passes) {
            ecrire_donnee(valeur = "X", cellule = c("B", 61))
        }
        if ("Passe à ralentisseurs" %in% passes) {
            ecrire_donnee(valeur = "X", cellule = c("B", 62))
            ecrire_donnee(valeur = " ", cellule = c("H", 62))
        }
        if ("Passe à bassins successifs" %in% passes) {
            ecrire_donnee(valeur = "X", cellule = c("B", 63))
            ecrire_donnee(valeur = " ", cellule = c("H", 63))
        }
        if ("Ecluse à poisson" %in% passes) {
            ecrire_donnee(valeur = "X", cellule = c("B", 64))
            ecrire_donnee(valeur = " ", cellule = c("H", 64))
        }
        if ("Passe à Anguille" %in% passes) {
            ecrire_donnee(valeur = "X", cellule = c("B", 65))
            ecrire_donnee(valeur = " ", cellule = c("H", 65))
            ecrire_donnee(valeur = "tapis brosse ", cellule = c("D", 66))
            ecrire_donnee(valeur = " ", cellule = c("H", 66))
            ecrire_donnee(valeur = "substrat rugueux ", cellule = c("D", 67))
            ecrire_donnee(valeur = " ", cellule = c("H", 67))
            ecrire_donnee(
                valeur = iconv("passe piège ", to = "latin1"),
                cellule = c("D", 68)
            )
            ecrire_donnee(valeur = " ", cellule = c("H", 68))
        }
        if ("Ascenseur à poisson" %in% passes) {
            ecrire_donnee(valeur = "X", cellule = c("B", 69))
            ecrire_donnee(valeur = " ", cellule = c("H", 69))
        }
        if ("Pré-barrage" %in% passes) {
            ecrire_donnee(valeur = "X", cellule = c("M", 62))
            ecrire_donnee(valeur = " ", cellule = c("T", 62))
        }
        if ("Rampe" %in% passes) {
            ecrire_donnee(valeur = "X", cellule = c("M", 63))
            ecrire_donnee(valeur = " ", cellule = c("T", 63))
            ecrire_donnee(
                valeur = "sur partie de la largeur ",
                cellule = c("O", 64)
            )
            ecrire_donnee(valeur = " ", cellule = c("T", 64))
            ecrire_donnee(
                valeur = iconv("sur totalité de la largeur ", to = "latin1"),
                cellule = c("O", 65)
            )
            ecrire_donnee(valeur = " ", cellule = c("T", 65))
        }
        if ("Rivière de contournement" %in% passes) {
            ecrire_donnee(valeur = "X", cellule = c("M", 66))
            ecrire_donnee(valeur = " ", cellule = c("T", 66))
        }
        if ("Exutoire de dévalaison" %in% passes) {
            ecrire_donnee(valeur = "X", cellule = c("M", 67))
            ecrire_donnee(valeur = " ", cellule = c("T", 67))
        }
        if ("Type de dispositif (piscicole) inconnu" %in% passes) {
            ecrire_donnee(valeur = "X", cellule = c("M", 68))
            ecrire_donnee(valeur = " ", cellule = c("T", 68))
        }
        if ("Autre type de passe" %in% passes) {
            ecrire_donnee(valeur = "X", cellule = c("M", 69))
            ecrire_donnee(valeur = " ", cellule = c("T", 69))
            ecrire_donnee(valeur = "... ", cellule = c("O", 70))
        }

        # Dispositif de franchissement pour la navigation----
        cellule_navigation <- donnees_ouvrage %>%
            dplyr::select(
                identifiant_roe,
                dplyr::starts_with("fnt_nom")
            ) %>%
            tidyr::pivot_longer(
                cols = dplyr::starts_with("fnt"),
                names_to = "colonne",
                values_to = "navigation"
            ) %>%
            dplyr::filter(
                !(stringr::str_detect(colonne, "fnt_nom") & colonne != "fnt_nom1" & is.na(navigation))
            ) %>%
            dplyr::pull(navigation) %>%
            purrr::map(
                function(x) {
                    dplyr::case_when(
                        x == "Absence de dispositif de franchissement (navigation)" ~ "B-73",
                        x == "Type de dispositif (navigation) inconnu" ~ "B-74",
                        x == "Ecluse" ~ "H-74",
                        x == "Ascenseur" ~ "M-74",
                        x == "Passe à canoë" ~ "Q-74"
                    ) %>%
                        formater_cellule()
                }
            )

        purrr::walk(
            cellule_navigation,
            ecrire_donnee, valeur = "X"
        )

        ecrire_donnee(
            valeur = iconv(paste0("Export Géobs: ", date_export), to = "latin1"),
            cellule = c("B", 81)
        )

        print(generer_carte(code_roe))

        openxlsx::insertPlot(
            wb = FicheTerrain,
            sheet = 1,
            width = 15,
            height = 15,
            units = "cm",
            dpi = 300,
            fileType = "png",
            xy = c("Z", 2)
        )

        openxlsx::saveWorkbook(
            wb = FicheTerrain,
            file = donnees_ouvrage$fichier_sortie,
            overwrite = TRUE
        )

    }

    purrr::walk(
        codes_roe,
        generer_fiche_terrain
    )

}
