#' Evaluer la complétude des données obligatoires et facultatives
#'
#' Cette fonction permet d'évaluer pour chaque ouvrage la complétion de
#' l'information pour les champs obligatoires (nom, type, état et coordonnées)
#' et deux champs complémentaires (classe de hauteur de chute et équipement en
#' dispositif de franchissement piscicole) du ROE.
#'
#' Il est vérifié qu'une information est fournie pour les champs nom_principal,
#' type_code, etat_code, pour les deux champs de coordonnées x_l93 et y_l93,
#' pour le champ hauteur_chute_etiage_classe ainsi que pour au moins un des
#' champs correspondant aux dispositifs de franchissement piscicole (fpi_code1 à
#' fpi_code5). Le nombre d'information obligatoire (les coordonnées sont
#' considérées comme une information unique) et complémentaire renseignées sont
#' également calculés.
#'
#' @param donnees_bilan sélection des données exportées utilisées pour réaliser
#'   des bilans, obtenue avec la fonction [preparer_donnees_bilan()].
#'
#' @export
#'
#' @importFrom dplyr select starts_with filter group_by summarise mutate
#'   left_join transmute across
#' @importFrom janitor make_clean_names
#' @importFrom stringr str_replace_all
#' @importFrom tidyr pivot_longer
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

    donnees_bilan %>%
        dplyr::select(
            -dplyr::starts_with("fpi_")
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
                    "passe_poisson"
                ),
                .fns = function(x) {
                    as.numeric(!is.na(x))
                }
            )
        ) %>%
        dplyr::mutate(
            obligatoire = nom_principal + type_code + etat_code + coordonnees,
            complementaire = hauteur_chute_etiage_classe + passe_poisson
        )

}

#' Synthétiser l'information sur la complétude des données obligatoires et
#' complémentaires
#'
#' L'évaluation de la complétude des informations à l'ouvrage est synthétisée en
#' comptant le nombre d'ouvrages pour lequel il manque au moins une information
#' obligatoire (nom, type, état et coordonnées) et/ou au moins une information
#' facultative parmi les deux considérées (classe de hauteur de chute et
#' équipement en dispositif de franchissement piscicole).
#'
#' Le décompte des ouvrages non gelés pour lesquels de l'information est
#' manquante peut être réalisé au niveau du jeu de données entier (par défaut),
#' ou au niveau de sous-ensembles en indiquant comme paramètres supplémentaires
#' (`...`), les noms de champs pour lesquels on veut regarder en détail (e.g.
#' `dept_nom` pour un détail par département, `prioritaire` pour un détail pour
#' les ouvrages prioritaires ou non). Fournir plusieurs noms de champs à la
#' place de `...` permet de considérer des groupes et sous-groupes définis par
#' les combinaisons de ces champs.
#'
#' @inheritParams evaluer_completude
#' @param ... noms des champs (sans "") permettant de définir les groupes pour
#'   lesquels on veut le détail
#'
#' @return
#' @export
#'
#' @importFrom dplyr filter select left_join group_by summarise n_distinct
synthetiser_completude <- function(donnees_bilan, ...) {
    Completude <- evaluer_completude(donnees_bilan)

    donnees_bilan %>%
        evaluer_validation() %>%
        dplyr::filter(validation != "Gelé") %>%
        dplyr::select(identifiant_roe, ...) %>%
        dplyr::left_join(
            Completude,
            by = "identifiant_roe"
        ) %>%
        dplyr::group_by(...) %>%
        dplyr::summarise(
            obligatoire_manquant = dplyr::n_distinct(identifiant_roe[obligatoire < 4]),
            complementaire_manquant = dplyr::n_distinct(identifiant_roe[complementaire < 2]),
            total = dplyr::n_distinct(identifiant_roe),
            .groups = "drop"
        )
}

#' Visualisation de la synthèse de la complétude des données obligatoires et
#' complémentaires
#'
#' Représente sous la forme d'une graphique de type 'upset' la complétude des
#' informations obligatoires (nom, type, état et coordonnées) et de deux
#' informations complémentaires (classe de hauteur de chute et équipement en
#' dispositif de franchissement piscicole). Le nombre d'ouvrage non gelés
#' correspondant à chaque combinaison d'information manquante est représenté.
#' Cette répartition peut être déclinée par une variable de regroupement des
#' ouvrages (e.g. par département)
#'
#' @inheritParams evaluer_completude
#' @param groupe nom de la colonne pour laquelle on veut voir le détail
#' @param visualiser_prioritaires valeur logique (TRUE/FALSE) contrôlant si le
#'   détail du nombre d'ouvrage est afficher en différenciant les ouvrages
#'   prioritaires ou pas
#' @param nombre_lignes nombre de lignes sur lesquelles répartir les graphiques
#'   crées pour chacun des `groupe`s. Calculé automatiquement par défaut
#' @param nombre_colonnes nombre de colonnes sur lesquelles répartir les
#'   graphiques crées pour chacun des `groupe`s. Calculé automatiquement par
#'   défaut.
#' @param ajuster_ymax valeur numérique par laquelle le maximum de l'axe y est
#'   ajusté pour augmenter l'espace entre les barres et le titre. Valeur par
#'   défaut de 1.1
#'
#'
#' @export
#' @importFrom dplyr select inner_join filter mutate group_by case_when
#'   n_distinct summarise group_split rowwise across count arrange
#' @importFrom ggplot2 ggplot aes geom_bar geom_text after_stat
#'   scale_fill_manual labs theme element_blank layer_scales scale_y_continuous
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
            # dplyr::mutate(etape = NA_character_) %>%
            # evaluer_validation() %>%
            # dplyr::filter(validation != "Gelé") %>%
            evaluer_completude(),
        by = "identifiant_roe"
    ) %>%
        dplyr::select(-obligatoire, -complementaire) %>%
        tidyr::pivot_longer(
            cols = nom_principal:passe_poisson,
            names_to = "information",
            values_to = "renseignement"
        ) %>%
        dplyr::mutate(
            type_information = ifelse(
                information %in% c(
                    "nom_principal", "type_code",
                    "etat_code", "coordonnees"
                ),
                "obligatoire",
                "complementaire"
            )
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

#' Préparer les données cartographiques résumant la complétude des informations
#'
#' Prépare un tableau spatialisé résumant pour chaque obstacle le nombre
#' d'informations obligatoires et complémentaires manquantes.
#'
#' @inheritParams evaluer_completude
#'
#' @export
#'
preparer_donnees_carte_completude <- function(donnees_bilan) {
    donnees_bilan %>%
        evaluer_validation() %>%
        dplyr::filter(validation != "Gelé") %>%
        evaluer_completude() %>%
        dplyr::filter(coordonnees == 1) %>%
        dplyr::select(
            identifiant_roe,
            obligatoire, complementaire
        ) %>%
        dplyr::mutate(
            obligatoire = 4 - obligatoire,
            complementaire = 2 - complementaire
        ) %>%
        dplyr::mutate(
            total = obligatoire + complementaire,
            info_manquante = dplyr::case_when(
                obligatoire > 0 ~ "obligatoire",
                complementaire > 0 ~ "complémentaire",
                TRUE ~ "aucune"
            )
        ) %>%
        dplyr::left_join(
            donnees_bilan %>%
                dplyr::select(
                    identifiant_roe,
                    ouvrage = prioritaire,
                    x_l93, y_l93
                ),
            by = "identifiant_roe"
        ) %>%
        dplyr::arrange(info_manquante) %>%
        sf::st_as_sf(
            coords = c("x_l93", "y_l93"),
            crs = 2154
        )
}
#' Cartographier la complétude des informations obligatoires et complémentaires
#'
#' Localise sur une carte les obstacles non gelés et pour lesquels on dispose au
#' moins des coordonnées géographiques. La symbologie utilisée permet
#' d'identifier les ouvrages considérés comme prioritaires ainsi que le type
#' d'information manquante parmi les trois informations obligatoires (nom, type
#' et état de l'ouvrage) et deux informations complémentaires (classe de hauteur
#' de chute et équipement en dispositif de franchissement piscicole).
#'
#' @param donnees_carte données obtenues avec la fonction [preparer_donnees_carte_completude()]
#' @param reseau_hydro données spatiales (classe `sf`) du réseau hydrographique.
#'   Par défaut, n'est pas affiché (NULL)
#' @param listes2 données spatiales (classe `sf`) de la partie du réseau.Par
#'   défaut, n'est pas affiché (NULL) hydrographique classé en liste 2
#' @param limites_zone données spatiales (classe `sf`) des limites de la zone
#'   représentée (région, département...). Par défaut, n'est pas affiché (NULL)
#' @param taille_obstacles taille des figurés représentant les obstacles
#'   (défaut: 2.5)
#'
#' @export
#'
#' @importFrom dplyr filter select mutate case_when left_join arrange
#' @importFrom ggplot2 ggplot geom_sf aes theme element_blank scale_shape_manual
#'   scale_colour_manual guides guide_legend
#' @importFrom sf st_as_sf
cartographier_completude <- function(donnees_carte, reseau_hydro = NULL, listes2 = NULL, limites_zone = NULL, taille_obstacles = 2.5) {

    Carte <- ggplot2::ggplot()

    if (!is.null(limites_zone))
        Carte <- Carte +
        ggplot2::geom_sf(
            data = limites_zone,
            fill = NA
        )

    if (!is.null(reseau_hydro)) {
        Carte <- Carte +
            ggplot2::geom_sf(
                data = reseau_hydro,
                size = .5,
                colour = "blue"
                )
    }

    if (!is.null(listes2)) {
        Carte <- Carte +
            ggplot2::geom_sf(
                data = listes2,
                size = 1.5,
                mapping = ggplot2::aes(colour = "liste 2")
            )
    }

    for (i in c("aucune", "complémentaire", "obligatoire")) {
        if (nrow(donnees_carte %>%
                 dplyr::filter(info_manquante == i)) > 0)
            Carte <- Carte +
                ggplot2::geom_sf(
                    data = donnees_carte %>%
                        dplyr::filter(info_manquante == i),
                    mapping = ggplot2::aes(
                        shape = ouvrage,
                        fill = info_manquante
                    ),
                    size = taille_obstacles)
    }

    Carte +
        ggplot2::theme_void() +
        ggplot2::scale_shape_manual(
            name = "Ouvrage",
            labels = c("non prioritaire", "prioritaire"),
            values = c(21, 24)
        ) +
        ggplot2::scale_colour_manual(
            name = "Classement au titre de\nla continuité écologique",
            values = "darkred"
        ) +
        ggplot2::guides(
            size = "none",
            shape = ggplot2::guide_legend(
                override.aes = list(size = 5)
            ),
            fill = ggplot2::guide_legend(
                override.aes = list(size = 5, shape = 21)
            ),
        )
}
