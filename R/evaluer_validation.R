#' Evaluer la validation des fiches ouvrages
#'
#' Cette fonction permet d'évaluer simplement si un ouvrage est validé (sans faire la distinction entre point par point et par échantillonnage), gelé ou non validé.
#'
#' @inheritParams ajouter_listes
#'
#' @return
#' @export
#'
#' @seealso synthetiser_validation visualiser_validation
#' @importFrom dplyr mutate case_when
evaluer_validation <- function(donnees_bilan) {
    donnees_bilan %>%
        dplyr::mutate(
            validation = dplyr::case_when(
                statut_code %in% c(0, 4) ~ "Validé",
                statut_code %in% c(2, 3) ~ statut_nom
            )
        )
}

#' Synthétiser l'information sur la validation des ouvrages
#'
#' L'évaluation de la validation de chaque ouvrage réalisée avec la fonction [evaluer_validation()] est synthétisée en comptant le nombre d'ouvrages validés, non validés ou gelés.
#'
#' Le décompte des ouvrages peut être réalisé au niveau du jeu de données entier (par défaut), ou au niveau de sous-ensembles en indiquant comme paramètres supplémentaires (`...`), les noms de champs pour lesquels on veut regarder en détail (e.g. dept_nom pour un détail par département, prioritaire pour un détail pour les ouvrages prioritaires ou non). Fournir plusieurs noms de champs à la place de `...` permet de considérer des groupes et sous-groupes définis par les combinaisons de ces champs.
#'
#' @inheritParams synthetiser_completude
#'
#' @return
#' @export
#'
#' @seealso evaluer_validation visualiser_validation
#'
#' @importFrom dplyr group_by mutate n_distinct summarise
synthetiser_validation <- function(donnees_bilan, ...) {
    donnees_bilan %>%
        evaluer_validation() %>%
        dplyr::group_by(...) %>%
        dplyr::mutate(total = dplyr::n_distinct(identifiant_roe)) %>%
        dplyr::group_by(..., total, validation) %>%
        dplyr::summarise(
            nombre = dplyr::n_distinct(identifiant_roe),
            .groups = "drop"
        )
}

#' Visualisation de la synthèse des validations
#'
#' Représente sous la forme d'une treemap, la répartition des nombres d'ouvrages en fonction de leur statut de validation (Validé, Non validé ou Gelé). Cette répartition peut être déclinée par une variable de regroupement des ouvrages (e.g. par département)
#'
#' @inheritParams synthetiser_validation
#' @param groupe nom de la colonne pour laquelle on veut voir le détail des nombres d'ouvrages gelés, validés ou non validés
#'
#' @return
#' @export
#'
#' @seealso evaluer_validation synthetiser_validation
#'
#' @importFrom dplyr select mutate
#' @importFrom ggplot2 ggplot aes scale_fill_manual theme
#' @importFrom stringr str_replace_na
#' @importFrom treemapify geom_treemap geom_treemap_subgroup_border geom_treemap_subgroup_text geom_treemap_text
visualiser_validation <- function(donnees_bilan, groupe = NULL) {
    ValidGroup <- try(
        ncol(dplyr::select(donnees_bilan, {{ groupe }}))
    )

    DonneesTreemap <- donnees_bilan %>%
        synthetiser_validation({{groupe}}) %>%
        dplyr::mutate(
            validation = factor(
                validation,
                levels = c("Validé", "Non validé", "Gelé")
            )
        )

    if (class(ValidGroup) != "try-error" & ValidGroup > 0) {
        DonneesTreemap <- DonneesTreemap %>%
            dplyr::mutate(
                group = stringr::str_replace_na({{groupe}}, "NA")
                )
    } else {
        DonneesTreemap <- DonneesTreemap %>%
            dplyr::mutate(
                group = ""
            )
    }


    treemap <- DonneesTreemap %>%
        ggplot2::ggplot(ggplot2::aes(
            area = nombre,
            fill = validation,
            label = nombre,
            subgroup = group
        )) +
        treemapify::geom_treemap()

    if (class(ValidGroup) != "try-error" & ValidGroup > 0) {
        treemap <- treemap +
            treemapify::geom_treemap_subgroup_border(
                colour = "white"
                ) +
            treemapify::geom_treemap_subgroup_text()
    }

    treemap +
        treemapify::geom_treemap_text(
            place = "centre",
            colour = "white"
        ) +
        ggplot2::scale_fill_manual(
            name = "",
            values = c(
                `Validé` = "#4F94CD",
                `Non validé` = "#CD3333",
                `Gelé` = "#8B8B83"
                )
            ) +
        ggplot2::theme(legend.position = "bottom")
}
