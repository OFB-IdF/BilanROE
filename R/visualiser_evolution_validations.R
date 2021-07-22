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
        package = "ggplot2",
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
                ggalluvial::geom_stratum(alpha = .5) +
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
