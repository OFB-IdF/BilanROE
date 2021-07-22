#' Lire le contenu d'une archive
#'
#' @param zipfile texte. Chemin de l'archive
#' @param file texte. Tout ou partie du nom du fichier à extraire
#' @param fun nom de la fonction à utiliser pour lire le fichier
#' @param ...  paramètres supplémentaires à passer à la fonction `fun`
#'
#' @return
#' @export
#'
#' @importFrom archive archive_extract
read_from_zip <- function(zipfile, file, fun, ...) {

    temp <- tempfile()

    archive::archive_extract(zipfile, dir = temp)

    filepath <- list.files(
        path = temp,
        pattern = file,
        full.names = TRUE,
        recursive = TRUE
    )

    obj <- fun(filepath, ...)

    unlink(temp, recursive = TRUE)

    return(obj)
}

#' Title
#'
#' @param data
#' @param column
#'
#' @return
#'
#' @importFrom dplyr filter distinct pull
get_all_levels <- function(data, column) {
    data %>%
        dplyr::filter(
            !is.na({{column}}),
            !is.null({{column}}),
            {{column}} != ""
        ) %>%
        dplyr::distinct({{column}}) %>%
        dplyr::pull({{column}})
}
