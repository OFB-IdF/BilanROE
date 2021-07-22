#' Ajouter l'information sur les listes aux données du bilan
#'
#'Cette fonction permet d'ajouter une information sur le fait que les ouvrages sont localisés ou non sur des cours d'eau classés en liste 1 et/ou en liste 2.
#'
#' Le tableau listes peut ne pas être importé avec la fonction [importer_listes_bdoe()], auquel cas il doit contenir quatre colonnes: identifiant_roe, liste1, liste2 et date_liste. L'identifiant_roe commence par 'ROE', les colonnes liste1 et liste2 sont renseignées par 'TRUE' (l'ouvrage est sur un cours d'eau classé) ou 'FALSE' (l'ouvrage n'est pas sur un cours d'eau classé). La colonne date_liste doit être présente mais peut être vide.
#'
#' @param donnees_bilan tableau de données tel que obtenu avec la fonction [selectionner_donnees_bilan()]
#' @param listes tableau de données tel que obtenu avec la fonction [importer_listes_bdoe()].
#'
#' @return
#' @export
#'
#' @importFrom dplyr left_join select
ajouter_listes <- function(donnees_bilan, listes) {
    donnees_bilan %>%
        dplyr::left_join(
            listes %>%
                dplyr::select(
                    identifiant_roe,
                    liste1,
                    liste2,
                    date_liste
                ),
            by = "identifiant_roe"
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
