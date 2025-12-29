# Santé Publique en France

Projet R Shiny Ravancé - Tableau de bord interacif d'analyse de la santé publique en France.

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

....

## Lancer l'application

### 1 : Cloner le projet ou télécharger les fichiers.

### 2 : Installer les packages nécessaires

```{r}
install.packages(c("shiny", "shinythemes", "shinycssloaders",
  "dplyr", "ggplot2", "tidyr", "purrr", "stringr",
  "readxl", "sf", "RColorBrewer"))
```
