#' Evaluer la validation et la complétude des fiches ouvrages
#'
#' Cette fonction permet d'évaluer simplement si un ouvrage est gelé, non
#' validé, validé avec ou sans information manquante (obligatoire ou
#' complémentaire parmi la hauteur de chute et la présence de passe à poisson).
#'
#' @inheritParams evaluer_completude
#'
#' @export
#'
#' @importFrom dplyr mutate case_when
evaluer_validation <- function(donnees_bilan) {
    donnees_bilan %>%
        dplyr::left_join(
            dplyr::bind_cols(
                donnees_bilan %>%
                    evaluer_completude(),
                donnees_bilan %>%
                    dplyr::select(etape)
            ),
             by = c("identifiant_roe", "etape")
        ) %>%
        dplyr::mutate(
            validation = dplyr::case_when(
                statut_code %in% c(2, 3) ~ statut_nom,
                obligatoire < 4 ~ "Obligatoire manquant",
                complementaire < 2 ~ "Complémentaire manquant",
                TRUE ~ "Ok"
            )
        )
}

#' Synthétiser l'information sur la validation des ouvrages
#'
#' L'évaluation de la validation de chaque ouvrage réalisée avec la fonction
#' [evaluer_validation()] est synthétisée en comptant le nombre d'ouvrages
#' validés (avec ou sans information manquante), non validés ou gelés.
#'
#' Le décompte des ouvrages peut être réalisé au niveau du jeu de données entier
#' (par défaut), ou au niveau de sous-ensembles en indiquant comme paramètres
#' supplémentaires (`...`), les noms de champs pour lesquels on veut regarder en
#' détail (e.g. `dept_nom` pour un détail par département, `prioritaire` pour un
#' détail pour les ouvrages prioritaires ou non). Fournir plusieurs noms de
#' champs à la place de `...` permet de considérer des groupes et sous-groupes
#' définis par les combinaisons de ces champs.
#'
#' @inheritParams synthetiser_completude
#' @param format format du tableau retourné (long ou large)
#'
#' @export
#'
#' @importFrom dplyr group_by mutate n_distinct summarise
#' @importFrom tidyr pivot_wider
synthetiser_validation <- function(donnees_bilan, ..., format = "long") {
    BilanValidation <- donnees_bilan %>%
        evaluer_validation() %>%
        dplyr::group_by(...) %>%
        dplyr::mutate(total = dplyr::n_distinct(identifiant_roe)) %>%
        dplyr::group_by(..., total, validation) %>%
        dplyr::summarise(
            nombre = dplyr::n_distinct(identifiant_roe),
            .groups = "drop"
        )

    if (format == "large") {
        BilanValidation %>%
            tidyr::pivot_wider(
                names_from = validation,
                values_from = nombre,
                values_fill = 0
            )
    } else {
        BilanValidation
    }
}

#' Visualisation de la synthèse des validations
#'
#' Représente sous la forme d'une treemap, la répartition des nombres d'ouvrages
#' en fonction de leur statut de validation. Cette
#' répartition peut être déclinée par une variable de regroupement des ouvrages
#' (e.g. par département)
#'
#' @inheritParams visualiser_completude
#'
#' @export
#'
#' @importFrom dplyr select mutate
#' @importFrom ggplot2 ggplot aes scale_fill_manual theme
#' @importFrom stringr str_replace_na
#' @importFrom treemapify geom_treemap geom_treemap_subgroup_border
#'   geom_treemap_subgroup_text geom_treemap_text
visualiser_validation <- function(donnees_bilan, groupe = NULL) {
    ValidGroup <- try(
        ncol(dplyr::select(donnees_bilan, {{ groupe }}))
    )

    DonneesTreemap <- donnees_bilan %>%
        synthetiser_validation({{groupe}}) %>%
        dplyr::mutate(
            validation = factor(
                validation,
                levels = c(
                    "Ok",
                    "Complémentaire manquant", "Obligatoire manquant",
                    "Non validé", "Gelé"
                    )
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

#' Mettre un tableau de données dans un format adapté à la production de
#' diagramme de Sankey avec {ggalluvial}
#'
#' Le format de données requis pour construire un diagramme de Sankey est
#' particulier puisqu'il doit permettre de suivre l'appartenance d'individus à
#' des groupes entre plusieurs étapes. Cette fonction permet de gérer le
#' formatage d'un tableau de données au format 'tidy' pour pouvoir être utilisé
#' pour produire un diagramme de Sankey.
#'
#' @param donnees tableau à mettre en forme
#' @param etape nom de la colonne correspondant à la variable selon laquelle on
#'   veut suivre les évolutions (dates, ...)
#' @param individu nom de la colonne correspondant aux identifiants uniques des
#'   'individus'
#' @param groupe nom de la colonne correspondant aux groupes auxquels les
#'   individus appartiennent. L'appartenance d'un individu à un groupe peut
#'   changer en fonction de `x`
#' @param ... noms des colonnes pour lesquels on veut détailler les flux
#'   (sous-ensembles des données)
#'
#' @export
#'
#' @importFrom dplyr mutate left_join group_by summarise n_distinct ungroup
#' @importFrom rlang expr_name enquo
#' @importFrom sf st_drop_geometry
#' @importFrom stringr str_remove
preparer_donnees_sankey <- function(donnees, etape, individu, groupe, ...) {
    donnees %>%
        (function(df) {
            if ("sf" %in% class(df)) {
                sf::st_drop_geometry(df)
            } else {
                df
            }
        }) %>%
        dplyr::mutate(
            etape = factor(
                as.character({{etape}}),
                levels = unique(as.character({{etape}}))
            )
        ) %>%
        (function(df) {
            dplyr::left_join(
                df,
                df %>%
                    dplyr::group_by({{individu}}) %>%
                    dplyr::summarise(sequence = paste0({{groupe}}, collapse = "->")),
                by = rlang::expr_name(rlang::enquo(individu)) %>%
                    stringr::str_remove(pattern = "~")
            )
        }) %>%
        dplyr::group_by(
            etape,
            sequence,
            {{groupe}},
            ...
        ) %>%
        dplyr::summarise(
            nombre = dplyr::n_distinct({{individu}}),
            .groups = "drop"
        ) %>%
        dplyr::group_by(
            etape,
            {{groupe}},
            ...
        ) %>%
        dplyr::mutate(
            label = sum(nombre)
        ) %>%
        dplyr::ungroup()
}

#' Visualiser les changements de groupes d'individus en fonction d'étapes à
#' l'aide d'un diagramme de Sankey
#'
#' @param df tableau mis en forme à l'aide de la fonction
#'   [preparer_donnees_sankey()]
#' @inheritParams preparer_donnees_sankey
#' @param text_size taille du texte affichant les nombres d'individu par groupe
#' @param log_y valeur logique (TRUE/FALSE): les comptes d'individus doivent-ils
#'   être exprimés en log ou non. Peut être utile pour suivre l'évolution des
#'   groupes à faibles effectifs
#'
#' @export
#'
#' @importFrom ggalluvial geom_flow geom_stratum
#' @importFrom ggplot2 ggplot aes scale_x_discrete geom_text facet_wrap vars
#'   labs theme element_blank scale_y_log10
#' @importFrom withr with_package
visualiser_sankey <- function(df, etape, groupe, text_size = 3, log_y = FALSE, ...) {
    withr::with_package(
        package = "ggalluvial",
        code = {
            gg <- df %>%
                ggplot2::ggplot(
                    ggplot2::aes(x = {{etape}},
                                 stratum = {{groupe}},
                                 alluvium = sequence,
                                 y = nombre,
                                 fill = {{groupe}},
                                 label = label)) +
                ggplot2::scale_x_discrete(expand = c(.1, .1)) +
                ggalluvial::geom_flow() +
                ggalluvial::geom_stratum(alpha = .5, colour = NA) +
                ggplot2::geom_text(stat = "stratum", size = text_size) +
                ggplot2::facet_wrap(facets = ggplot2::vars(...),
                                    scales=  "free_y", drop = TRUE) +
                ggplot2::labs(x = "", y = "") +
                ggplot2::theme(
                    panel.grid = ggplot2::element_blank(),
                    panel.background = ggplot2::element_blank(),
                    axis.ticks.x = ggplot2::element_blank(),
                    axis.text.y = ggplot2::element_blank(),
                    axis.ticks.y = ggplot2::element_blank()
                )

            if (log_y) {
                gg +
                    ggplot2::scale_y_log10()
            } else {
                gg
            }
        })

}

#' Visualiser l'évolution du nombre d'obstacles en fonction de leur statut de
#' validation et de la complétude des informations
#'
#' Cette fonction utilise des bilans réalisés à différentes dates pour suivre
#' l'évolution du statut de validation et de complétude de l'information des
#' obstacles.
#'
#' @param liste_bilans une liste de tableaux obtenus avec la fonction
#'   [preparer_donnees_bilan()]. Cette liste doit être nommée (e.g. avec les
#'   dates des exports correspondants aux différents tableaux de bilan) et les
#'   éléments de cette liste classés dans l'ordre dans lequel on veut les
#'   afficher.
#' @inheritParams visualiser_sankey
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom purrr map2_df
visualiser_evolution_validations <- function(liste_bilans, ..., log_y = FALSE, text_size = 3) {

    if (length(liste_bilans) < 2)
        stop("Au moins deux bilans doivent être fournis")

    gg <- purrr::map2_df(
        .x = liste_bilans,
        .y = names(liste_bilans),
        .f = function(x = .x, y = .y) {
            dplyr::mutate(x, etape = y)
        }
    ) %>%
        evaluer_validation() %>%
        dplyr::mutate(
            validation = factor(
                validation,
                levels = c(
                    "Non validé", "Obligatoire manquant","Complémentaire manquant",
                    "Ok", "Gelé"
                )
            )
        ) %>%
        preparer_donnees_sankey(
            etape = etape,
            individu = identifiant_roe,
            groupe = validation,
            ...
        ) %>%
        visualiser_sankey(
            etape = etape,
            groupe = validation,
            text_size = text_size,
            log_y = log_y,
            ...
        ) +
        ggplot2::scale_fill_manual(
            name = "",
            values = c(
                `Non validé` = "#C2133E",#unname(templatesOFB::ofb_cols("rouge")),
                `Obligatoire manquant` = "#ED6A53",#unname(templatesOFB::ofb_cols("orange1")),
                `Complémentaire manquant` = "#FFD744",#unname(templatesOFB::ofb_cols("jaune")),
                `Ok` = "#003A76",#unname(templatesOFB::ofb_cols("bleu1"))
                `Gelé` = "#564949"#unname(templatesOFB::ofb_cols("marron1")),
            )
        ) +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            axis.line = ggplot2::element_blank()
        )

    if (length(ggplot2::vars(...)) == 0)
        gg <- gg +
            ggplot2::theme(
                strip.text = ggplot2::element_blank()
            )

    gg
}
