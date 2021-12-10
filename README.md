BilanROE

Ce pakage permet de réaliser des traitements et visualisations à partir d'exports du ROE pour la production de bilans.

Il peut être installé depuis le Gitlab de l'OFB:

```r
if (!require(remotes))
    install.packages("remotes")

remotes::install_gitlab(
    repo = "cedric.mondy1/bilanroe",
    host = "https://gitlab.ofb.fr", 
    dependencies = TRUE
    )

```
