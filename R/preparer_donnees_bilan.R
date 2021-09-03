#' Sélectionne les champs de l'export qui seront utilisés pour réaliser le bilan
#'
#' Sur les 107 champs compris dans l'export, 40 sont conservés pour réaliser le bilan.
#' Les champs conservés concernent:
#' 1. l'identité de l'ouvrage: nom et code ROE
#' 2. la localisation de l'ouvrage: département, commune et coordonnées
#' 3. la position de l'ouvrage dans le réseau hydrographique: bassin, tronçons des BD Carthage et Topo
#' 4. des informations sur l'historique de saisie de l'ouvrage: statut de la fiche, dates de modification et de validation
#' 5. l'état de l'ouvrage
#' 6. le type principal de l'ouvrage
#' 7. la hauteur de chute
#' 8. la présence de dispositifs de franchissement piscicole
#' 9. l'usage de l'ouvrage
#'
#' @param donnees_brutes tableau de données obtenu avec la fonction [importer_donnees()]
#'
#' @return
#' @export
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
            date_modification_ouvrage,
            date_validation_ouvrage,
            dplyr::starts_with("etat_"),
            dplyr::starts_with("type_"),
            dplyr::starts_with("hauteur_chute_etiage"),
            dplyr::starts_with("fpi_"),
            dplyr::starts_with("usage_")
        )
}

#' Ajouter l'information sur les ouvrages prioritaires
#'
#' Cette fonction permet d'ajouter une information sur le fait que les ouvrages sont considérés ou non comme prioritaires au titre de la politique de restauration de la continuité écologique.
#'
#' Dans le tableau ouvrages_prioritaires, le champ identifiant_roe est de type texte et correspond aux identifiants utilisés dans le tableau donnees_bilan. Le champ prioritaire est de type logique (TRUE/FALSE). Le tableau peut ne contenir que les ouvrages prioritaires (TRUE), les ouvrages présents dans donnees_bilan et pas dans ouvrages_prioritaires seront alors automatiquement considérés comme non prioritaires (FALSE)
#'
#' @inheritParams ajouter_listes
#' @param ouvrages_prioritaires tableau de données contenant au moins les deux champs suivants: identifiant_roe (texte) et prioritaire (logique: TRUE/FALSE).
#'
#'
#'
#' @return
#' @export
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
#' Cette fonction est juste un 'wrapper' d'autres fonctions du package pour réaliser en une étape la sélection des champs de l'export brut et l'ajout de informations sur les classements en liste 1 et 2 ainsi que sur le caractère prioritaire ou non des ouvrages dans le cadre des politiques de restauration de la continuité écologique.
#'
#' @inheritParams selectionner_donnees_bilan
#' @inheritParams ajouter_listes
#' @inheritParams ajouter_prioritaires
#'
#' @seealso [selectionner_donnees_bilan()] [ajouter_listes()] [ajouter_prioritaires()]
#'
#' @return
#' @export
#'
preparer_donnees_bilan <- function(donnees_brutes,
                                   ouvrages_prioritaires) {
    donnees_brutes %>%
        selectionner_donnees_bilan() %>%
        ajouter_prioritaires(ouvrages_prioritaires)
}
