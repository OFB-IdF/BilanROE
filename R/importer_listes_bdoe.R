#' Importer les listes 1 et 2 depuis la BDOE
#'
#' Importer, depuis l'export complet de la BDOE, un tableau de correspondance entre les codes ROE des ouvrages et le statut liste 1 et/ou 2 des cours d'eau sur lesquels ils sont positionnés. La date de la liste est également retournée.
#'
#' @param chemin_export_bdoe texte. Chemin vers l'archive zip de l'export complet de la BDOE
#'
#' @return
#' @export
#'
#' @importFrom dplyr select mutate
#' @importFrom stringr str_replace_na str_to_upper
#' @importFrom vroom vroom locale
importer_listes_bdoe <- function(chemin_export_bdoe) {
    replace_list_codes <- function(list) {
        list %>%
            stringr::str_replace_na('FALSE') %>%
            stringr::str_to_upper() %>%
            as.logical()
    }

    read_from_zip(
        zipfile = chemin_export_bdoe,
        file = "^ouvrage_\\d{8}.csv",
        fun = vroom::vroom,
        delim = ";",
        locale = vroom::locale(encoding = "UTF-8"),
        col_types = paste(rep("c", 61), collapse = "")
    ) %>%
        dplyr::select(
            identifiant_roe = ouv_id,
            liste1 = ouv_liste1,
            liste2 = ouv_liste2,
            date_liste = ouv_date_liste
        ) %>%
        dplyr::mutate(
            liste1 = replace_list_codes(liste1),
            liste2 = replace_list_codes(liste2)
        )

}

