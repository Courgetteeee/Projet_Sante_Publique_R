
library(dplyr)
library(purrr)
library(readxl)
library(stringr)
library(tidyr)


## Import et prétraitement des données de Clara --------------------------------

medecin<-read_excel("data/Med_2012_2025.xlsx", sheet=2)


medecin_clean <- medecin %>% filter(!(substr(territoire, 1, 1) %in% c("0", "3")), region != "00-Ensemble", 
                                    sexe=="0-Ensemble", departement!="000-Ensemble", exercice == "0-Ensemble",
                                    tranche_age=="00-Ensemble", specialites != "00-Ensemble")

medecin_long <- medecin_clean %>% pivot_longer(cols=starts_with("effectif_"),
                                               names_to = "annee", values_to = "effectif") %>% 
  mutate(annee=as.integer(sub("effectif_", "", annee)))


saveRDS(medecin_long, "donnees_traitees/medecin_long.rds")



## Import et prétraitement des données de Karla --------------------------------

import_sheet <- function(file, sheet_name) {
  
  # Lire les deux lignes d'en-tête
  headers <- read_excel(file, sheet = sheet_name, n_max = 2, col_names = FALSE)
  
  fill_right <- function(x) {
    for (i in seq_along(x)) {
      if (is.na(x[i]) && i > 1) {
        x[i] <- x[i-1]
      }
    }
    x
  }
  
  header1 <- headers[1, ] %>% unlist() %>% fill_right()
  header2 <- headers[2, ] %>% unlist()
  
  # Fusion des deux en-têtes
  colnames_combined <- paste(header1, header2, sep = "_") %>%
    str_replace_all(" ", "") %>%
    str_replace_all("__", "_") %>%
    str_replace_all("\\n", "") %>%
    str_replace_all("[^A-Za-z0-9_]", "")
  
  # Import des données
  dat <- read_excel(file, sheet = sheet_name, skip = 2, col_names = colnames_combined)
  
  # Extraction du nom de feuille : FM-H-2009_2013
  parts <- unlist(str_split(sheet_name, "-"))
  
  territoire <- parts[1]              # FM
  sexe_code  <- parts[2]              # H ou F
  annees     <- parts[3]              # 2009_2013
  
  deb_fin <- unlist(str_split(annees, "_"))
  
  annee_debut <- as.numeric(deb_fin[1])
  annee_fin   <- as.numeric(deb_fin[2])
  
  dat <- dat %>%
    mutate(
      sexe = case_when(
        sexe_code == "H" ~ "Homme",
        sexe_code == "F" ~ "Femme",
        TRUE ~ sexe_code
      ),
      annee_debut = annee_debut,
      annee_fin = annee_fin
    )
  
  dat
}

import_all_sheets <- function(file) {
  sheet_names <- excel_sheets(file)
  map_dfr(sheet_names, ~import_sheet(file, .x))
}


morta_dip <- import_all_sheets("donnees_shiny/MORTA_DIP.xlsx")
morta_dip <- morta_dip %>% filter(Diplme_INDICATEUR != "Âge") %>% rename(age = Diplme_INDICATEUR)

morta_cs <- import_all_sheets("donnees_shiny/MORTA_CS.xlsx")
morta_cs <- morta_cs %>% filter(Catgoriesocioprofessionnelle_INDICATEUR != "Âge") %>% rename(age = Catgoriesocioprofessionnelle_INDICATEUR)


# Lecture des 2 premières lignes pour récupérer les variables et le sexe
headers <- read_excel("donnees_shiny/FET2021-28.xlsx", n_max = 2, col_names = FALSE)
header1 <- headers[1, ] %>% unlist()  # noms des variables
header1 <- fill(data.frame(header1), header1)$header1

sexe_code <- headers[2, ] %>% unlist() # H ou F

# Lecture des données en sautant les 2 premières lignes
dat <- read_excel("donnees_shiny/FET2021-28.xlsx", skip = 2, col_names = FALSE)

# Création des noms uniques
colnames_unique <- paste0(header1, "_", sexe_code)
colnames_unique<- c("Région", colnames_unique)

#On renomme nos entêtes
colnames(dat) <- colnames_unique

cols_to_pivot <- grep("_(Femmes|Hommes)$", colnames(dat), value = TRUE)

mortalite_cause <- dat %>%
  pivot_longer(
    cols = all_of(cols_to_pivot),
    names_to = "variable_sexe",
    values_to = "valeur"
  ) %>%
  mutate(
    sexe = if_else(str_ends(variable_sexe, "Femmes"), "Femme", "Homme"),
    variable = str_remove(variable_sexe, "_Femmes$|_Hommes$")
  ) %>%
  select(Région, valeur, sexe, variable)


col_tous <- "Mortalité périnatale pour 1 000 naissances_Ensemble"

# Pivot pour mortalite_ensemble avec sexe = "Tous"
mortalite_perinatale_comb <- dat %>%
  select(-all_of(cols_to_pivot)) %>%
  pivot_longer(
    cols = all_of(col_tous),
    names_to = "variable_sexe",
    values_to = "valeur"
  ) %>%
  mutate(sexe = "Tous",
         variable = str_remove(variable_sexe, "_Ensemble")
  )

# Combine les deux
mortalite_combine <- bind_rows(mortalite_cause, mortalite_perinatale_comb) %>% select(-variable_sexe)
#Table a part pour mortalité périnatale
mortalite_perinatale <- mortalite_perinatale_comb %>% select(-variable)



saveRDS(mortalite_combine, "donnees_traitees/mortalite_combine.rds")
saveRDS(mortalite_perinatale, "donnees_traitees/mortalite_perinatale.rds")
saveRDS(mortalite_cause, "donnees_traitees/mortalite_cause.rds")
saveRDS(morta_dip, "donnees_traitees/morta_dip.rds")
saveRDS(morta_cs, "donnees_traitees/morta_cs.rds")


##Import et prétraitement des données de Cindy ---------------------------------