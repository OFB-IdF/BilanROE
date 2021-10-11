#' Importer les données de l'export
#'
#' @param chemin_archive texte. Chemin vers l'archive zip contenant l'export ROE
#' @param bassins vecteur de texte. Noms des bassins hydrographiques (en
#'   majuscules) à importer. Par défaut (NULL), tous les bassins sont importés.
#'   Les obstacles avec un bassin non renseigné (NULL, NA ou '') ne sont pas
#'   importés.
#' @param departements vecteur de texte. Codes INSEE des départements à
#'   importer. Par défaut (NULL), tous les départements sont importés. Les
#'   obstacles avec un département non renseigné (NULL, NA ou '') ne sont pas
#'   importés.
#'
#' @return un tableau contenant l'ensemble de l'export ROE renseigné, filtré le
#'   cas échéant par bassin hydrographique ou département.
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom purrr map_df
#' @importFrom vroom vroom locale
importer_donnees <- function(chemin_archive, bassins = NULL, departements = NULL) {
    DonneesBrutes <-
        read_from_zip(
        zipfile = chemin_archive,
        file = ".{2,3}_temp\\d{8}\\.csv",
        fun = function(files) {
            purrr::map_df(
                files,
                vroom::vroom,
                delim = ";",
                col_types = paste(rep("c", 107), collapse = ""),
                locale = vroom::locale(
                    encoding = "windows-1252",
                    decimal_mark = ","
                    )
            )
        }
    ) %>%
        (function(df) {
            if (all(is.null(bassins)))
                bassins <- get_all_levels(df, bassin_administratif)

            if (all(is.null(departements)))
                departements <- get_all_levels(df, dept_code)

            df %>%
                dplyr::filter(
                    bassin_administratif %in% bassins,
                    dept_code %in% departements
                )
        })
}

#' Télécharger les données cours d'eau de la BD Topage 2019
#'
#' Permet de télécharger soit l'ensemble du référentiel cours d'eau de la BD
#' Topage pour la France continentale et la corse (`bassin = 99`), soit un ou
#' plusieurs bassins hydrographiques en utilisant les services web du Sandre.
#'
#' @param bassin code du ou des bassins hydrographiques dont on veut télécharger
#'   le référentiel: - 01: Artois-Picardie - 02: Rhin-Meuse - 03:
#'   Seine-Normandie - 04: Loire-Bretagne - 05: Adour-Garonne - 06:
#'   Rhône-Méditerranée - 12: Corse - 99: France entière
#'
#' @return un tableau spatial (classe `sf`) contenant le référentiel
#'   hydrographique du bassin hydrographique sélectionné.
#' @export
#'
#' @importFrom purrr map_dfr
#' @importFrom sf st_read
telecharger_ce_topage <- function(bassin = 99) {
    BassinsHydro <- c(
        "01" = "01_Artois-Picardie",
        "02" = "02_Rhin-Meuse",
        "03" = "03_Seine-Normandie",
        "04" = "04-Loire-Bretagne",
        "05" = "05-Adour-Garonne",
        "06" = "06-Rhône-Méditerranée",
        "12" = "12_Corse",
        "99" = "99"
    )


    if (any(!bassin %in% names(BassinsHydro)))
        stop(
            paste0(
                "Les valeurs passées dans l'argument bassin doivent être choisies parmi les suivantes: ",
                paste(names(BassinsHydro), collapse = ", ")
            )
        )

    telecharger_topage_bassin <- function(catchment) {
        temp_file <- tempfile()

        download.file(
            url = paste0("https://services.sandre.eaufrance.fr/telechargement/geo/ETH/BDTopage/2019/CoursEau/Bassin/", BassinsHydro[catchment], "_CoursEau_shp.zip"),
            destfile = temp_file
        )
        TopageBassin <- read_from_zip(
            zipfile = temp_file,
            file = ".shp",
            fun = sf::st_read
        )

        unlink(temp_file)

        TopageBassin
    }

    purrr::map_dfr(
        bassin,
        telecharger_topage_bassin
    )

}
