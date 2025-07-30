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
#' @param encoding texte décrivant l'encodage à utiliser dans vroom::locale
#'
#' @return un tableau contenant l'ensemble de l'export ROE renseigné, filtré le
#'   cas échéant par bassin hydrographique ou département.
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom purrr map_df
#' @importFrom vroom vroom locale
importer_donnees <- function(chemin_archive, bassins = NULL, departements = NULL, encoding = "windows-1252") {
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
                    encoding = encoding,
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

#' Télécharger les limites des régions administratives
#'
#' @param codes_region vecteur contenant les codes Insee des régions pour
#'   lesquelles on veut les limites (par défaut, elles sont toutes retournées)
#'
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom sf st_read
telecharger_limites_region <- function(codes_region = NULL) {
    LimitesRegion <- sf::st_read("https://transcode.geo.data.gouv.fr/links/6072fe3dfa4268bc25ce04b0/downloads/6072fe646a65b64ba3a02074?format=GeoJSON&projection=Lambert93")

    if (!is.null(codes_region))
        LimitesRegion <- LimitesRegion %>%
            dplyr::filter(CdRegion %in% codes_region)

    LimitesRegion
}

#' Télécharger les limites des masses d'eau (France métropolitaine; flux Sandre)
#'
#' Les masses d'eau considérées correspondent aux
#' [bassins versants spécifiques](https://id.eaufrance.fr/ddd/MDO/1.4/BVSpeMasseDEauSurface) tels que définis dans le Référentiel des masses d'eau. Si une
#' zone d'intérêt est fournie avec l'argument `limites_zone` (par exemple les
#' limites des régions administratives obtenues avec la fonction
#' [telecharger_limites_region()]), seule les masses d'eau de cette zone sont
#' retournées, sinon toutes les masses d'eau de France métropolitaine sont
#' retournées.
#'
#' @param limites_zone couche spatiale de type (multi)polygone délimitant l'aire
#'   d'intérêt.
#'
#' @export
#'
#' @importFrom httr parse_url build_url
#' @importFrom rmapshaper ms_clip
#' @importFrom sf st_read st_transform
telecharger_masses_eau <- function(limites_zone = NULL) {
    service_sandre <- "https://services.sandre.eaufrance.fr/geo/sandre"
    url_sandre <- httr::parse_url(service_sandre)
    url_sandre$query <- list(
        service = "wfs",
        version = "2.0.0",
        request = "GetFeature",
        typename = "sa:BVSpeMasseDEauSurface_VEDL2019_FXX"
    )
    requete_sandre <- httr::build_url(url_sandre)

    LimitesMassesEau <- sf::st_read(requete_sandre) %>%
        sf::st_transform(crs = 2154)

    if (!is.null(limites_zone))
        LimitesMassesEau <- LimitesMassesEau %>%
        rmapshaper::ms_clip(limites_zone)

    LimitesMassesEau

}

#' Regrouper les masses d'eau par cours d'eau principaux
#'
#' @param masses_eau
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate group_by summarise
#' @importFrom stringr str_split str_remove
regrouper_masses_eau <- function(masses_eau) {
    masses_eau %>%
        dplyr::mutate(
            nom_me = NomBVSpeMDO %>%
                stringr::str_split(pattern = "de sa source") %>%
                sapply('[[', 1) %>%
                stringr::str_split(pattern = "du confluent") %>%
                sapply('[[', 1) %>%
                stringr::str_remove(pattern = " *$") %>%
                stringr::str_remove(pattern = "^ *")
        ) %>%
        dplyr::group_by(nom_me) %>%
        dplyr::summarise()
}


#' Télécharger les données cours d'eau de la BD Topage 2019 (flux Sandre)
#'
#' Permet de télécharger soit l'ensemble du référentiel cours d'eau de la BD
#' Topage pour la France continentale et la corse (`bassin = 99`), soit un ou
#' plusieurs bassins hydrographiques.
#'
#' @param bassin code du ou des bassins hydrographiques dont on veut télécharger
#'   le référentiel: - 01: Artois-Picardie - 02: Rhin-Meuse - 03:
#'   Seine-Normandie - 04: Loire-Bretagne - 05: Adour-Garonne - 06:
#'   Rhône-Méditerranée - 12: Corse - 99: France entière
#' @inheritParams telecharger_masses_eau
#'
#' @return un tableau spatial (classe `sf`) contenant le référentiel
#'   hydrographique du bassin hydrographique sélectionné.
#' @export
#'
#' @importFrom purrr map_dfr
#' @importFrom rmapshaper ms_clip
#' @importFrom sf st_read
telecharger_ce_topage <- function(bassin = 99, limites_zone = NULL) {
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

    ReseauHydro <- purrr::map_dfr(
        bassin,
        telecharger_topage_bassin
    )

    if (!is.null(limites_zone))
        ReseauHydro <- ReseauHydro %>%
        rmapshaper::ms_clip(limites_zone)

    ReseauHydro

}

#' Télécharger les linéaires de cours d'eau classés en liste 2 au titre de la
#' continuité écologique (flux Sandre)
#'
#' @inheritParams telecharger_masses_eau
#'
#' @return
#' @export
#'
#' @importFrom httr parse_url build_url
#' @importFrom rmapshaper ms_clip
#' @importFrom sf st_read st_transform
telecharger_listes2 <- function(limites_zone = NULL) {

    service_sandre <- "https://services.sandre.eaufrance.fr/geo/zppn"
    url_sandre <- httr::parse_url(service_sandre)
    url_sandre$query <- list(
        service = "wfs",
        version = "2.0.0",
        request = "GetFeature",
        typename = "sa:SegClassContinuiteEcoListe2_FXX"
    )
    requete_sandre <- httr::build_url(url_sandre)

    Listes2 <- sf::st_read(requete_sandre) %>%
        sf::st_transform(crs = 2154)

    if (!is.null(limites_zone))
        Listes2 <- Listes2 %>%
        rmapshaper::ms_clip(limites_zone)

    Listes2
}

#' Télécharger le ROE
#'
#' Cette fonction permet de télécharger le ROE diffusé via un fluw WFS du Sandre (https://www.sandre.eaufrance.fr/atlas/srv/fre/catalog.search#/metadata/59057026-b40c-4cf9-9e3e-7296e0aa1a78).La récupération de ces données peut prendre un peu de temps.
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom httr parse_url build_url
#' @importFrom purrr list_merge
#' @importFrom sf st_read
telecharger_roe_sandre <- function() {
    "https://services.sandre.eaufrance.fr/geo/obs?SERVICE=WFS" %>%
        httr::parse_url() %>%
        purrr::list_merge(
            query = list(
                service = "wfs",
                version = "2.0.0",
                request = "GetFeature",
                typename = "sa:ObstEcoul",
                srsName = "EPSG:4326"
            )
        ) %>%
        httr::build_url() %>%
        sf::st_read()
}
