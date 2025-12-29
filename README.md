# Santé Publique en France

Projet R Shiny Ravancé - Tableau de bord interactif d'analyse de la santé publique en France.

## L'application

### Onglet 1 : Offre de soins et prévention

-   Evolution des effectifs de médecins et infirmiers par département et spécialité (2012-2025).
-   Analyse de l'Accessibilité Potentielle Localisée (APL) par commune.
-   Décalage entre offre et besoin de soins.

### Onglet 2 : Etudes de la mortalité

-   Mortalité périnatale par région.
-   Causes de décès standardisées par sexe.
-   Inégalités de mortalité selon le diplôme et la classe sociale.
-   Evolution des indicateurs (quotient, espérance de vie, survie).

### Onglet 3 : Accessibilité aux médecins généralistes

-   Carte de France par région avec classement national.
-   Carte régionale par commune.
-   Indicateurs clés

## Structure du projet

-   'data/' : données brutes (non modifiées)
-   'donnees_traitees/' : données traitées (.rds)
-   'donnees_shiny/' : données traitées
-   'sante_publique_cck/app.R' : application Shiny
-   'sante_publique_cck/Traitement_donnees.R' : traitement et nettoyage de données

....

## Lancer l'application

### 1. Cloner le projet ou télécharger les fichiers.

### 2. Installer les packages nécessaires

```{r}
install.packages(c("shiny", "shinythemes", "shinycssloaders",
  "dplyr", "ggplot2", "tidyr", "purrr", "stringr",
  "readxl", "sf", "RColorBrewer"))
```

### 3. Lancer l'application

``` r
shiny::runApp("sante_publique_cck/app.R")
```

Ou cliquer sur **"Run App"** dans RStudio.

## Données

Les données proviennent de la plateforme officielle data.gouv.fr dans le cadre du défie "Santé et territoires" : [Plateforme officielle defis.data.gouv.fr - Santé et territoires](https://defis.data.gouv.fr/defis/sante-et-territoires)

## Auteurs

**Karla PEM** \| **Clara GAMBARDELLO** \| **Cindy LANCON**

Master Modélisation Statistique \| R avancé Université de Franche-Comté (2025/2026)
