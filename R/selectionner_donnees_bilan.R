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
