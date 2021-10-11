#' Visualiser l'évolution du nombre d'ouvrages validés dans le ROE
#'
#' Cette fonction permet de visualiser simplement l'évolution du nombre cumulé
#' d'ouvrages dans le ROE. Cette évolution peut être déclinée par une variable
#' de regroupement des ouvrages (e.g. par département).
#'
#' @inheritParams preparer_donnees_bilan
#' @param log_y valeur logique (TRUE/FALSE) pour exprimer ou non l'axe des y
#'   (nombre d'obstacles validés) avec une échelle logarithmique
#' @param ajuster_xmax valeur numérique par laquelle le maximum de l'axe x est
#'   ajusté pour laisser de la place aux annotations avec le nombre d'ouvrages.
#'
#' @return un objet ggplot
#' @export
#'
#' @importFrom dplyr select mutate case_when filter group_by arrange n ungroup
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_text labs unit theme
#'   layer_scales scale_x_date scale_y_log10
#' @importFrom ggrepel geom_text_repel
#' @importFrom lubridate as_date
visualiser_evolution_roe <- function(donnees_bilan, groupe = NULL, log_y = FALSE, ajuster_xmax = 1.015) {
    ValidGroup <- try(
        ncol(dplyr::select(donnees_bilan, {{ groupe }}))
    )

    DonneesTendances <- donnees_bilan %>%
        dplyr::mutate(
            date = dplyr::case_when(
                !is.na(date_validation_ouvrage) ~ lubridate::as_date(date_validation_ouvrage),
                is.na(date_validation_ouvrage) & !is.na(date_modification_ouvrage) ~ lubridate::as_date(date_modification_ouvrage)
            )
        ) %>%
        dplyr::filter(!is.na(date)) %>%
        dplyr::select({{ groupe }}, date) %>%
        dplyr::group_by({{ groupe }}) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(n = seq(dplyr::n())) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(n_tot = seq(dplyr::n()))

    GraphTendances <- ggplot2::ggplot(
        data = DonneesTendances,
        mapping = ggplot2::aes(x = date)
    ) +
        ggplot2::geom_line(
            mapping = ggplot2::aes(y = n_tot),
            colour = "black",
            size = 1
        ) +
        ggplot2::geom_point(
            data = DonneesTendances %>%
                dplyr::filter(date == max(date)),
            mapping = ggplot2::aes(y = n_tot),
            colour = "black",
            size = 3
        ) +
        ggplot2::geom_text(
            data = DonneesTendances %>%
                dplyr::filter(n_tot == max(n_tot)),
            mapping = ggplot2::aes(
                y = n_tot,
                label = paste0("total: ", n_tot)
                ),
            hjust = 0,
            nudge_x = 60,
            colour = "black"
        ) +
        ggplot2::labs(
            x = "",
            y = "nombre d'obstacles référencés"
        )

    if (class(ValidGroup) != "try-error" & ValidGroup == 1) {
        GraphTendances <- GraphTendances +
            ggplot2::geom_line(
                mapping = ggplot2::aes(
                    y = n,
                    colour = {{ groupe }}
                    ),
                size = .5
            ) +
            ggplot2::geom_point(
                data = DonneesTendances %>%
                    dplyr::group_by( {{ groupe }}) %>%
                    dplyr::filter(n == max(n)),
                mapping = ggplot2::aes(
                    y = n,
                    colour = {{ groupe }}
                ),
                size = 2
            ) +
            ggrepel::geom_text_repel(
                data = DonneesTendances %>%
                    dplyr::group_by( {{ groupe }}) %>%
                    dplyr::filter(n == max(n)),
                mapping = ggplot2::aes(
                    y = n,
                    label = paste0({{ groupe }}, ": ", n),
                    colour = {{ groupe }}
                ),
                hjust = 0,
                nudge_x = 60,
                min.segment.length = ggplot2::unit(100, "pt")
            ) +
            ggplot2::theme(legend.position = "none")
    }

    xLims <- ggplot2::layer_scales(GraphTendances)$x$range$range

    GraphTendances <- GraphTendances +
        ggplot2::scale_x_date(limits = lubridate::as_date(c(xLims[1], ajuster_xmax * xLims[2])))

    if (log_y) {
        GraphTendances +
            ggplot2::scale_y_log10()
    } else {
        GraphTendances
    }
}
