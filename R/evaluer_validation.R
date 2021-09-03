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

#' Visualiser l'évolution du nombre d'obstacles validés, non validés ou gelés
#'
#' Cette fonction utilise des bilans réalisés à différentes dates pour suivre l'évolution du statut de validation des obstacles.
#'
#' @param liste_bilans une liste de tableaux obtenus avec la fonction [preparer_donnees_bilan()]. Cette liste doit être nommée (e.g. avec les dates des exports correspondants aux différents tableaux de bilan) et les éléments de cette liste classés dans l'ordre dans lequel on veut les afficher.
#' @inheritParams synthetiser_completude
#' @inheritParams visualiser_evolution_roe
#'
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom withr with_package
#' @importFrom dplyr mutate left_join group_by summarise n_distinct
#' @importFrom ggalluvial geom_flow geom_stratum StatStratum
#' @importFrom ggplot2 ggplot aes scale_x_discrete geom_text facet_wrap vars scale_fill_manual labs theme element_blank scale_y_log10
#' @importFrom purrr map2_df
visualiser_evolution_validations <- function(liste_bilans, ..., log_y = FALSE) {
    withr::with_package(
        package = "ggalluvial",
        code = {
            EvolutionValidations <- purrr::map2_df(
                .x = liste_bilans,
                .y = names(liste_bilans),
                .f = function(x = .x, y = .y) {
                    dplyr::mutate(x, index = y)
                }
            ) %>%
                dplyr::mutate(
                    index = factor(index, levels = names(liste_bilans))
                ) %>%
                evaluer_validation() %>%
                (function(df) {
                    dplyr::left_join(
                        df,
                        df %>%
                            dplyr::group_by(identifiant_roe) %>%
                            dplyr::summarise(sequence = paste0(validation, collapse = "->")),
                        by = "identifiant_roe"
                    )
                }) %>%
                dplyr::group_by(
                    index,
                    sequence,
                    validation,
                    ...
                ) %>%
                dplyr::summarise(
                    nombre_obstacles = dplyr::n_distinct(identifiant_roe),
                    .groups = "drop"
                ) %>%
                dplyr::group_by(
                    index,
                    validation,
                    ...
                ) %>%
                dplyr::mutate(
                    label = sum(nombre_obstacles)
                ) %>%
                ggplot2::ggplot(
                    ggplot2::aes(x = index,
                                 stratum = validation,
                                 alluvium = sequence,
                                 y = nombre_obstacles,
                                 fill = validation,
                                 label = label)) +
                ggplot2::scale_x_discrete(expand = c(.1, .1)) +
                ggalluvial::geom_flow() +
                ggalluvial::geom_stratum(alpha = .5, colour = NA) +
                ggplot2::geom_text(stat = "stratum", size = 3) +
                ggplot2::facet_wrap(facets = ggplot2::vars(...),
                                    scales=  "free_y") +
                ggplot2::scale_fill_manual(
                    name = "",
                    values = c(
                        `Validé` = "#6495ED",
                        `Non validé` = "#F08080",
                        `Gelé` = "#C1CDCD"
                    )
                ) +
                ggplot2::labs(x = "", y = "") +
                ggplot2::theme(
                    # legend.position = "none",
                    panel.grid = ggplot2::element_blank(),
                    panel.background = ggplot2::element_blank(),
                    axis.ticks.x = ggplot2::element_blank(),
                    axis.text.y = ggplot2::element_blank(),
                    axis.ticks.y = ggplot2::element_blank()
                )

            if (log_y) {
                EvolutionValidations +
                    ggplot2::scale_y_log10()
            } else {
                EvolutionValidations
            }
        }
    )

}
