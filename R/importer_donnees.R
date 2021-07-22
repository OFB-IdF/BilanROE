#' Importer les données de l'export
#'
#' @param chemin_archive texte. Chemin vers l'archive zip contenant l'export ROE
#' @param bassins vecteur de texte. Noms des bassins hydrographiques (en majuscules) à importer. Par défaut (NULL), tous les bassins sont importés. Les obstacles avec un bassin non renseigné (NULL, NA ou '') ne sont pas importés.
#' @param departements vecteur de texte. Codes INSEE des départements à importer. Par défaut (NULL), tous les départements sont importés. Les obstacles avec un département non renseigné (NULL, NA ou '') ne sont pas importés.
#'
#' @return
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
