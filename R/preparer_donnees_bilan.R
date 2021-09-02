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
                                   # listes,
                                   ouvrages_prioritaires) {
    donnees_brutes %>%
        selectionner_donnees_bilan() %>%
        # ajouter_listes(listes) %>%
        ajouter_prioritaires(ouvrages_prioritaires)
}
