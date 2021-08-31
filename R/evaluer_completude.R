#' Evaluer la complétude des données obligatoires et facultatives
#'
#' Cette fonction permet d'évaluer pour chaque ouvrage la complétion de l'information pour les champs obligatoires (nom, type, état et coordonnées) et trois champs complémentaires (classe de hauteur de chute, usage et équipement en dispositif de franchissement piscicole) du ROE.
#'
#' Il est vérifié qu'une information est fournie pour les champs nom_principal, type_code, etat_code, pour les deux champs de coordonnées x_l93 et y_l93, pour le champ hauteur_chute_etiage_classe ainsi que pour au moins un des champs correspondant aux dispositifs de franchissement piscicole (fpi_code1 à fpi_code5) et à l'usage (usage_code1 à usage_code4). Le nombre d'information obligatoire (les coordonnées sont considérées comme une information unique) et complémentaire renseignées sont également calculés.
#'
#' @inheritParams ajouter_listes
#'
#' @return
#' @export
#'
#' @seealso synthetiser_completude visualiser_completude
#'
#' @importFrom dplyr select starts_with filter group_by summarise mutate left_join transmute across
#' @importFrom stringr str_replace_all
#' @importFrom tidyr pivot_longer
#' @importFrom janitor make_clean_names
evaluer_completude <- function(donnees_bilan) {

    PassesPoisson <- donnees_bilan %>%
        dplyr::select(
            identifiant_roe,
            dplyr::starts_with("fpi_code")
            ) %>%
        tidyr::pivot_longer(
            cols = dplyr::starts_with("fpi_code"),
            names_to = "name",
            values_to = "passe_poisson"
        ) %>%
        dplyr::filter(!is.na(passe_poisson)) %>%
        dplyr::select(-name) %>%
        dplyr::group_by(identifiant_roe) %>%
        dplyr::summarise(
            passe_poisson = paste(
                passe_poisson,
                collapse = ", "
                )
            )

    Usages <- donnees_bilan %>%
        dplyr::select(
            identifiant_roe,
            dplyr::starts_with("usage_code")
        ) %>%
        tidyr::pivot_longer(
            cols = dplyr::starts_with("usage_code"),
            names_to = "name",
            values_to = "usage"
        ) %>%
        dplyr::filter(!is.na(usage)) %>%
        dplyr::select(-name) %>%
        dplyr::group_by(identifiant_roe) %>%
        dplyr::summarise(
            usage = paste(
                usage,
                collapse = ", "
                )
            )

    donnees_bilan %>%
        dplyr::select(
            -dplyr::starts_with("fpi_"),
            -dplyr::starts_with("usage_")
        ) %>%
        dplyr::mutate(
            coordonnees = ifelse(
                any(is.na(c(x_l93, y_l93))),
                NA_character_,
                paste0(x_l93, ", ", y_l93)
            )
        ) %>%
        dplyr::left_join(
            PassesPoisson,
            by = "identifiant_roe"
        ) %>%
        dplyr::left_join(
            Usages,
            by = "identifiant_roe"
        ) %>%
        dplyr::mutate(
            hauteur_chute_etiage_classe = hauteur_chute_etiage_classe %>%
                janitor::make_clean_names() %>%
                stringr::str_replace_all(
                    string = .,
                    pattern = "indeterminee",
                    replacement = NA_character_
                    )
            ) %>%
        dplyr::transmute(
            identifiant_roe,
            dplyr::across(
                .cols = c(
                    "nom_principal",
                    "type_code",
                    "etat_code",
                    "coordonnees",
                    "hauteur_chute_etiage_classe",
                    "passe_poisson",
                    "usage"
                ),
                .fns = function(x) {
                    as.numeric(!is.na(x))
                }
            )
        ) %>%
        dplyr::mutate(
            obligatoire = nom_principal + type_code + etat_code + coordonnees,
            complementaire = hauteur_chute_etiage_classe + passe_poisson + usage
        )

}

#' Synthétiser l'information sur la complétude des données obligatoires et complémentaires
#'
#' L'évaluation de la complétude des informations à l'ouvrage est synthétisée pour les ouvrages validés en comptant le nombre d'ouvrages pour lequel il manque au moins une information obligatoire (nom, type, état et coordonnées) et/ou au moins une information facultative parmi les trois considérées (classe de hauteur de chute, usage et équipement en dispositif de franchissement piscicole).
#'
#' Le décompte des ouvrages pour lesquels de l'information est manquante peut être réalisé au niveau du jeu de données entier (par défaut), ou au niveau de sous-ensembles en indiquant comme paramètres supplémentaires (`...`), les noms de champs pour lesquels on veut regarder en détail (e.g. dept_nom pour un détail par département, prioritaire pour un détail pour les ouvrages prioritaires ou non). Fournir plusieurs noms de champs à la place de `...` permet de considérer des groupes et sous-groupes définis par les combinaisons de ces champs.
#'
#' @inheritParams ajouter_listes
#' @param ... noms des champs (sans "") permettant de définir les groupes pour lesquels on veut le détail
#'
#' @return
#' @export
#'
#' @seealso evaluer_completude visualiser_completude
#' @importFrom dplyr filter select left_join group_by summarise n_distinct
synthetiser_completude <- function(donnees_bilan, ...) {
    Completude <- evaluer_completude(donnees_bilan)

    donnees_bilan %>%
        evaluer_validation() %>%
        dplyr::filter(validation == "Validé") %>%
        dplyr::select(identifiant_roe, ...) %>%
        dplyr::left_join(
            Completude,
            by = "identifiant_roe"
            ) %>%
        dplyr::group_by(...) %>%
        dplyr::summarise(
            obligatoire_manquant = dplyr::n_distinct(identifiant_roe[obligatoire < 4]),
            complementaire_manquant = dplyr::n_distinct(identifiant_roe[complementaire < 3]),
            total = dplyr::n_distinct(identifiant_roe),
            .groups = "drop"
        )
}

#' Visualisation de la synthèse de la complétude des données obligatoires et complémentaires
#'
#' Représente sous la forme d'une graphique de type 'upset' la complétude des informations obligatoires (nom, type, état et coordonnées) et de trois informations complémentaires (classe de hauteur de chute, usage et équipement en dispositif de franchissement piscicole). Le nombre d'ouvrage correspondant à chaque combinaison d'information manquante est représenté. Cette répartition peut être déclinée par une variable de regroupement des ouvrages (e.g. par département)
#'
#' @inheritParams visualiser_validation
#' @param visualiser_prioritaires valeur logique (TRUE/FALSE) contrôlant si le détail du nombre d'ouvrage est afficher en différentiant les ouvrages prioritaires ou pas
#' @param nombre_lignes nombre de lignes sur lesquelles répartir les graphiques crées pour chacun des `groupe`s. Calculé automatiquement par défaut
#' @param nombre_colonnes nombre de colonnes sur lesquelles répartir les graphiques crées pour chacun des `groupe`s. Calculé automatiquement par défaut.
#' @param ajuster_ymax valeur numérique par laquelle le maximum de l'axe y est ajusté pour augmenter l'espace entre les barres et le titre. Valeur par défaut de 1.1
#'
#'
#' @export
#' @importFrom dplyr select inner_join filter mutate group_by case_when n_distinct summarise group_split rowwise across count arrange
#' @importFrom ggplot2 ggplot aes geom_bar geom_text after_stat scale_fill_manual labs theme element_blank layer_scales scale_y_continuous
#' @importFrom ggtext element_markdown
#' @importFrom ggupset scale_x_upset
#' @importFrom patchwork wrap_plots
#' @importFrom stringr str_remove_all str_replace_all
#' @importFrom tidyr pivot_longer
visualiser_completude <- function(donnees_bilan, groupe = NULL, visualiser_prioritaires = FALSE, nombre_lignes = NULL, nombre_colonnes = NULL, ajuster_ymax = 1.1) {
    # axe y avec seulement des entiers
    # https://stackoverflow.com/a/57086284
    int_breaks_rounded <- function(x, n = 3) {
        pretty(x, n)[round(pretty(x, n),1) %% 1 == 0]
    }

    ValidGroup <- try(
        ncol(dplyr::select(donnees_bilan, {{ groupe }}))
    )

    DataUpset <- dplyr::inner_join(
        dplyr::select(donnees_bilan, prioritaire, identifiant_roe, {{ groupe }}),
        donnees_bilan %>%
            evaluer_validation() %>%
            dplyr::filter(validation == "Validé") %>%
            evaluer_completude(),
        by = "identifiant_roe"
    ) %>%
        dplyr::select(-obligatoire, -complementaire) %>%
        tidyr::pivot_longer(
            cols = nom_principal:usage,
            names_to = "information",
            values_to = "renseignement"
        ) %>%
        dplyr::mutate(
            type_information = ifelse(information %in% c("nom_principal", "type_code", "etat_code", "coordonnees"), "obligatoire", "complementaire")
        ) %>%
        dplyr::filter(renseignement == 0) %>%
        dplyr::group_by({{ groupe }}, information) %>%
        dplyr::mutate(
            information =
                dplyr::case_when(
                    type_information == "obligatoire" ~
                        stringr::str_remove_all(
                            information,
                            pattern = "_code|_principal") %>%
                        stringr::str_replace_all(
                            pattern = "etat",
                            replacement = "&#233;tat"
                        ) %>%
                        (function(x) {
                            paste0(
                                "**", x, " (",
                                dplyr::n_distinct(identifiant_roe),
                                ")**")
                        }),
                    type_information == "complementaire" ~ information %>%
                        stringr::str_replace_all(
                            pattern = "passe_poisson",
                            replacement = "passe &#224; poisson"
                        ) %>%
                        stringr::str_replace_all(
                            pattern = "hauteur_chute_etiage_classe",
                            replacement = "hauteur de chute"
                        ) %>%
                        paste0(
                            " (",
                            dplyr::n_distinct(identifiant_roe), ")"
                            )
                    )
            ) %>%
        dplyr::group_by({{ groupe }}, prioritaire, identifiant_roe) %>%
        dplyr::summarise(
            information_manquante = list(information),
            .groups = "drop"
        ) %>%
        dplyr::mutate(prioritaire = as.factor(prioritaire))

    if (class(ValidGroup) != "try-error" & ValidGroup == 1) {
        DataUpset <- DataUpset %>%
            dplyr::mutate(
                groupe = {{ groupe }}
            )
    } else {
        DataUpset <- DataUpset %>%
            dplyr::mutate(
                groupe = ""
            )
    }

    DataUpset %>%
        dplyr::group_by(groupe) %>%
        dplyr::group_split() %>%
        lapply(
            function(df) {
                df2 <- df %>%
                    dplyr::rowwise() %>%
                    dplyr::mutate(
                        ID = dplyr::across(groupe,
                                           paste0, collapse = ", ")
                        )

                UpsetPlot <- df2 %>%
                    ggplot2::ggplot(
                        ggplot2::aes(x = information_manquante)
                    )

                if (visualiser_prioritaires) {
                    SummedN <- df2 %>%
                        dplyr::count(
                            information_manquante, prioritaire
                        ) %>%
                        dplyr::group_by(information_manquante) %>%
                        dplyr::arrange(information_manquante, desc(prioritaire)) %>%
                        dplyr::mutate(n_cum = cumsum(n))

                    UpsetPlot <- UpsetPlot +
                        ggplot2::geom_bar(
                            ggplot2::aes(fill = prioritaire)
                        ) +
                        ggplot2::geom_text(
                            data = SummedN,
                            ggplot2::aes(
                                x = information_manquante,
                                y = n_cum,
                                group = prioritaire,
                                label = n
                                ),
                            vjust = -.75
                        )
                } else {
                    UpsetPlot <- UpsetPlot +
                        ggplot2::geom_bar() +
                        ggplot2::geom_text(
                            stat = "count",
                            ggplot2::aes(label = ggplot2::after_stat(count)),
                            vjust = -.75
                            )
                }

                UpsetPlot <- UpsetPlot +
                    ggupset::scale_x_upset() +
                    ggplot2::scale_fill_manual(
                        drop = FALSE,
                        values = c(
                            `FALSE` = "lightgrey",
                            `TRUE` = c("#CD3333")
                            )
                        ) +
                    ggplot2::labs(
                        title = unique(df2$ID),
                        x = "", y = ""
                        ) +
                    ggplot2::theme(
                        panel.background = ggplot2::element_blank(),
                        panel.grid = ggplot2::element_blank(),
                        axis.text.y = ggtext::element_markdown(hjust = 0),
                        legend.position = "none"
                    )

                yLims <- ggplot2::layer_scales(UpsetPlot)$y$range$range

                UpsetPlot +
                    ggplot2::scale_y_continuous(
                        breaks = int_breaks_rounded(yLims),
                        limits = c(yLims[1], yLims[2]*ajuster_ymax)
                    )
                }
            ) %>%
        patchwork::wrap_plots(nrow = nombre_lignes, ncol = nombre_colonnes)
}
