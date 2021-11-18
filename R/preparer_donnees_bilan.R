#' Sélectionne les champs de l'export qui seront utilisés pour réaliser le bilan
#'
#'
#' @param donnees_brutes tableau de données obtenu avec la fonction
#'   [importer_donnees()]
#'
#' @return un tableau de données ne contenant que la sélection de champs de
#'   l'export qui sont utilisés par les fonctions de {BilanROE}
#'
#' @importFrom dplyr select starts_with
selectionner_donnees_bilan <- function(donnees_brutes) {
    donnees_brutes %>%
        dplyr::select(
            identifiant_roe,
            nom_principal,
            dplyr::starts_with("dept"),
            commune_code,
            x_l93, y_l93,
            bassin_administratif,
            id_troncon_carthage, nom_carthage,
            id_troncon_topo, nom_topo,
            dplyr::starts_with("statut_"),
            date_construction_ouvrage,
            date_modification_ouvrage,
            date_validation_ouvrage,
            dplyr::starts_with("etat_"),
            dplyr::starts_with("type_"),
            dplyr::starts_with("stype_"),
            dplyr::starts_with("emo_"),
            dplyr::starts_with("hauteur_chute_etiage"),
            dplyr::starts_with("fpi_"),
            dplyr::starts_with("fnt_"),
            dplyr::starts_with("usage_")
        )
}

#' Ajouter l'information sur les ouvrages prioritaires
#'
#' Cette fonction permet d'ajouter une information sur le fait que les ouvrages
#' sont considérés ou non comme prioritaires au titre de la politique de
#' restauration de la continuité écologique.
#'
#' Dans le tableau ouvrages_prioritaires, le champ identifiant_roe correspond
#' aux identifiants utilisés dans le tableau donnees_bilan. Le tableau peut ne
#' contenir que les ouvrages prioritaires, dans ce cas les ouvrages présents
#' dans donnees_bilan et absents du tableau ouvrages_prioritaires seront alors
#' automatiquement considérés comme non prioritaires.
#'
#' @param donnees_bilan tableau de données préparées obtenu avec la fonction
#'   [selectionner_donnees_bilan()]
#' @param ouvrages_prioritaires tableau de données contenant au moins les deux
#'   champs suivants: identifiant_roe (texte) et prioritaire (logique:
#'   TRUE/FALSE).
#'
#' @return un tableau de données contenant les informations de l'export du ROE
#'   complété par le statut prioritaire ou non des ouvrages.
#'
#' @importFrom dplyr left_join select mutate
#' @importFrom stringr str_replace_na
ajouter_prioritaires <- function(donnees_bilan, ouvrages_prioritaires) {
    donnees_bilan %>%
        dplyr::left_join(
            ouvrages_prioritaires %>%
                dplyr::select(identifiant_roe, prioritaire),
            by = "identifiant_roe"
        ) %>%
        dplyr::mutate(
            prioritaire = stringr::str_replace_na(
                string = prioritaire,
                replacement = "FALSE"
            ) %>%
                as.logical()
        )
}


#' Préparer les données pour faire le bilan
#'
#' Cette fonction est juste un 'wrapper' d'autres fonctions du package pour
#' réaliser en une étape la sélection des champs de l'export brut et l'ajout de
#' informations sur le caractère prioritaire ou non des ouvrages dans le cadre
#' des politiques de restauration de la continuité écologique.
#'
#' @inheritParams selectionner_donnees_bilan
#' @inheritParams ajouter_prioritaires
#'
#' @return un tableau de données contenant les informations de l'export du ROE
#'   préparées et complétées par le statut prioritaire ou non des ouvrages.
#' @export
#'
#' @importFrom sf st_as_sf st_join st_drop_geometry
preparer_donnees_bilan <- function(donnees_brutes,
                                   ouvrages_prioritaires = NULL,
                                   masses_eau = NULL) {
    DonneesBilan <- donnees_brutes %>%
        selectionner_donnees_bilan()

    if (!is.null(ouvrages_prioritaires))
        DonneesBilan <- DonneesBilan %>%
            ajouter_prioritaires(ouvrages_prioritaires)

    if (!is.null(masses_eau))
        DonneesBilan <- DonneesBilan %>%
            sf::st_as_sf(
                coords = c("x_l93", "y_l93"),
                crs = 2154,
                remove = FALSE
            ) %>%
            sf::st_join(masses_eau) %>%
            sf::st_drop_geometry()

    DonneesBilan
}
