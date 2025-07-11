% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluer_validation.R
\name{synthetiser_validation}
\alias{synthetiser_validation}
\title{Synthétiser l'information sur la validation des ouvrages}
\usage{
synthetiser_validation(donnees_bilan, ..., format = "long")
}
\arguments{
\item{donnees_bilan}{sélection des données exportées utilisées pour réaliser
des bilans, obtenue avec la fonction \code{\link[=preparer_donnees_bilan]{preparer_donnees_bilan()}}.}

\item{...}{noms des champs (sans "") permettant de définir les groupes pour
lesquels on veut le détail}

\item{format}{format du tableau retourné (long ou large)}
}
\description{
L'évaluation de la validation de chaque ouvrage réalisée avec la fonction
\code{\link[=evaluer_validation]{evaluer_validation()}} est synthétisée en comptant le nombre d'ouvrages
validés (avec ou sans information manquante), non validés ou gelés.
}
\details{
Le décompte des ouvrages peut être réalisé au niveau du jeu de données entier
(par défaut), ou au niveau de sous-ensembles en indiquant comme paramètres
supplémentaires (\code{...}), les noms de champs pour lesquels on veut regarder en
détail (e.g. \code{dept_nom} pour un détail par département, \code{prioritaire} pour un
détail pour les ouvrages prioritaires ou non). Fournir plusieurs noms de
champs à la place de \code{...} permet de considérer des groupes et sous-groupes
définis par les combinaisons de ces champs.
}
