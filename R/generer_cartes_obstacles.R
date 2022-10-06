#' Générer des cartes de localisation des ouvrages ROE
#'
#' @inheritParams generer_fiches_terrain
#' @param fond_carte fond de carte, objet raster de classe stars
#'
#' @return
#' @export
#'
generer_cartes_obstacles <- function(donnees, codes_roe = NULL, dossier_sortie = getwd(), sous_dossiers = NULL, fond_carte) {
    FichiersSortie <- donnees %>%
        dplyr::mutate(dossier = dossier_sortie) %>%
        dplyr::select(dossier, dplyr::all_of(sous_dossiers), identifiant_roe) %>%
        tidyr::pivot_longer(cols = -identifiant_roe) %>%
        dplyr::group_by(identifiant_roe) %>%
        dplyr::summarise(chemin = paste(value, collapse = "/")) %>%
        dplyr::mutate(fichier_sortie = paste0(chemin, "/FicheTerrain_", identifiant_roe, ".jpg"))

    donnees <- donnees %>%
        dplyr::left_join(FichiersSortie, by = "identifiant_roe")

    if (is.null(codes_roe))
        codes_roe <- unique(donnees$identifiant_roe)

    donnees <- donnees %>%
        dplyr::filter(identifiant_roe %in% codes_roe) %>%
        dplyr::arrange(fichier_sortie)

    if (!is.null(sous_dossiers)) {
        donnees %>%
            dplyr::distinct(across(all_of(sous_dossiers))) %>%
            dplyr::mutate(id = seq(dplyr::n())) %>%
            tidyr::pivot_longer(
                cols = -id,
                names_to = "niveau",
                values_to = "valeur"
            ) %>%
            dplyr::group_by(id) %>%
            dplyr::summarise(
                chemin = paste(c(dossier_sortie, valeur), collapse = "/")
            ) %>%
            dplyr::pull(chemin) %>%
            purrr::walk(
                .f = function(x) {
                    if (!dir.exists(x))
                        dir.create(path = x, recursive = TRUE)
                }
            )

    }

    pb <- progress::progress_bar$new(
        total = length(codes_roe),
        format = "[:bar] (:eta)"
    )


    generer_carte <- function(code_roe) {
        pb$tick()

        donnees_ouvrage <- donnees %>%
            dplyr::filter(identifiant_roe %in% code_roe) %>%
            sf::st_as_sf(
                coords = c("x_l93", "y_l93"),
                crs = 2154,
                remove = FALSE
            )

        ouvrage_bbox <- donnees_ouvrage %>%
            sf::st_buffer(625) %>%
            sf::st_bbox()

         fond_ouvrage <- fond_carte %>%
             sf::st_crop(ouvrage_bbox) %>%
             stars::st_as_stars() %>%
             stars::st_rgb()

         carte_ouvrage <- ggplot2::ggplot() +
             stars::geom_stars(data = fond_ouvrage) +
             ggplot2::geom_sf(data = donnees_ouvrage, size = 5.75, colour = "black") +
             ggplot2::geom_sf(data = donnees_ouvrage, size = 5, colour = "darkgrey") +
             ggplot2::theme_void()

         ggplot2::ggsave(
             plot = carte_ouvrage,
             filename = unique(donnees_ouvrage$fichier_sortie),
             width = 15, height = 15, units = "cm",
             dpi = 300
             )
    }

    purrr::walk(
        codes_roe,
        generer_carte
    )

}
