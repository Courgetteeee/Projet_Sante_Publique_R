library(dplyr)
library(purrr)
library(readxl)
library(stringr)
library(sf)
library(tidyr)

source("fonctions.R")

#Repertoires
dir_input <- "../data"
dir_output <- "../donnees_traitees"

#Fichiers entrées
fileinput_medecin <- file.path(dir_input, "Med_2012_2025.xlsx")
fileinput_inf_lib <- file.path(dir_input, "Inf_lib_2012_2023.xlsx")
fileinput_inf_sal <- file.path(dir_input, "Inf_sal_2013_2021.xlsx")
fileinput_apl_med <- file.path(dir_input, "APL_med_gen.xlsx")
fileinput_apl_inf <- file.path(dir_input, "APL_inf.xlsx")
fileinput_morta_dip <- file.path(dir_input, "MORTA_DIP.xlsx")
fileinput_morta_cs <- file.path(dir_input, "MORTA_CS.xlsx")
fileinput_morta_cause <- file.path(dir_input, "FET2021-28.xlsx")
fileinput_apl_gener <- file.path(dir_input, "APL_med_gen.xlsx")

#Fichiers sorties
fileoutput_medecin <- file.path(dir_output, "medecin_long.rds")
fileoutput_inf_lib <- file.path(dir_output, "inf_lib_long.rds")
fileoutput_inf_sal <- file.path(dir_output, "inf_sal_long.rds")
fileoutput_apl_med <- file.path(dir_output, "APL_med_long.rds")
fileoutput_apl_inf <- file.path(dir_output, "APL_inf.rds")
fileoutput_morta_dip <- file.path(dir_output, "morta_dip.rds")
fileoutput_morta_cs <- file.path(dir_output, "morta_cs.rds")
fileoutput_morta_cause <- file.path(dir_output, "mortalite_cause.rds")
fileoutput_morta_combine <- file.path(dir_output, "mortalite_combine.rds")
fileoutput_morta_peri <- file.path(dir_output, "mortalite_perinatale.rds")
fileoutput_apl_gener <- file.path(dir_output, "donnees_propres_apl_generalistes.rds")



## Import et prétraitement des données de Clara --------------------------------

# Medecin

medecin <- read_excel(fileinput_medecin, sheet = 2)

# Suppression des lignes correspondant à des agrégats "Ensemble" afin de ne conserver que les catégories détaillées
medecin_clean <- medecin %>% filter(
  !(substr(territoire, 1, 1) %in% c("0", "3")), region != "00-Ensemble",
  sexe == "0-Ensemble", departement != "000-Ensemble", exercice == "0-Ensemble",
  tranche_age == "00-Ensemble", specialites != "00-Ensemble"
)

# Passage en format long, création de la colonne année et mise en forme des libellés pour les graphiques
medecin_long <- medecin_clean %>%
  pivot_longer(cols = starts_with("effectif_"), names_to = "annee", values_to = "effectif") %>%
  mutate(annee = as.integer(sub("effectif_", "", annee))) %>%
  mutate(
    specialites = str_to_title(str_extract(specialites, "(?<=-).*")),
    departement = str_to_title(str_extract(departement, "(?<=-).*"))
  )

saveRDS(medecin_long, fileoutput_medecin)

# Inf liberal

inf_lib <- read_excel(fileinput_inf_lib, sheet = 2)

# Suppression des lignes agrégées et des colonnes inutiles
inf_lib_clean <- inf_lib %>%
  filter(region != "00 - Ensemble", departement != "00 - Ensemble", sexe == "0 - Ensemble", tranche_age == "00 - Ensemble") %>%
  select(-...17)

# Passage en format long, création de la colonne année et nettoyage des libellés
inf_lib_long <- inf_lib_clean %>%
  pivot_longer(cols = starts_with("effectif_"), names_to = "annee", values_to = "effectif") %>%
  mutate(annee = as.integer(sub("effectif_", "", annee))) %>%
  mutate(departement = str_replace_all(departement, " ", "")) %>%
  mutate(region = str_replace_all(region, " ", "")) %>%
  # Exclusion des années absentes des données infirmiers salariés (inf_sal) pour permettre la comparaison
  filter(!annee %in% c(2012, 2022, 2023)) %>%
  mutate(data_type = "Infirmiers libéraux") %>%
  mutate(departement = str_to_title(str_extract(departement, "(?<=-).*")))

saveRDS(inf_lib_long, fileoutput_inf_lib)

# Inf sal

inf_sal <- read_excel(fileinput_inf_sal, sheet = 2)

inf_sal_clean_names <- inf_sal %>%
  rename(
    secteur_activite = `Secteur d'activité`, specialite = `Spécialité`, Tranche_age = `Tranche d'âge`, region = `Région`,
    departement = `Département`
  )

# Suppression des lignes agrégées pour ne conserver que les données détaillées
inf_sal_clean <- inf_sal_clean_names %>%
  filter(
    region != "00-France entière (hors Mayotte)", departement != "00-Région entière", departement != "00-Région entière",
    secteur_activite == "0-Ensemble", specialite == "0-Ensemble", Sexe == "0-Ensemble", Tranche_age == "00-Ensemble"
  )

# Passage en format long, création de la colonne année et mise en forme des libellés
inf_sal_long <- inf_sal_clean %>%
  pivot_longer(cols = starts_with("effectif_"), names_to = "annee", values_to = "effectif") %>%
  mutate(annee = as.integer(sub("effectif_", "", annee))) %>%
  mutate(data_type = "Infirmiers salariés") %>%
  mutate(departement = str_to_title(str_extract(departement, "(?<=-).*")))

saveRDS(inf_sal_long, fileoutput_inf_sal)

# APL med

APL_med_2022 <- read_excel(fileinput_apl_med, sheet = 2, skip = 8)
APL_med_2023 <- read_excel(fileinput_apl_med, sheet = 3, skip = 8)

# Nettoyage et mise en forme des données
APL_med_2022_clean <- clean_APL_med(APL_med_2022, annee = 2022, annee_pop = 2020)
APL_med_2023_clean <- clean_APL_med(APL_med_2023, annee = 2023, annee_pop = 2021)

APL_med <- bind_rows(APL_med_2022_clean, APL_med_2023_clean)

APL_med_long <- APL_med %>%
  select(Commune, Code_INSEE, Pop_standardisee_med, APL_tous, APL_65, APL_62, APL_60, annee) %>%
  pivot_longer(cols = starts_with("APL"), names_to = "age_medecins", values_to = "APL") %>%
  mutate(
    Pop_standardisee_med = as.numeric(Pop_standardisee_med),
    APL = as.numeric(APL), age_medecins = factor(age_medecins)
  )

saveRDS(APL_med_long, fileoutput_apl_med)

# APL inf

APL_inf_2022 <- read_excel(fileinput_apl_inf, sheet = 2, skip = 8)
APL_inf_2023 <- read_excel(fileinput_apl_inf, sheet = 3, skip = 8)

# Nettoyage et mise en forme des données
APL_inf_2022_clean <- clean_APL_inf(APL_inf_2022, annee = 2022, annee_pop = 2020)
APL_inf_2023_clean <- clean_APL_inf(APL_inf_2023, annee = 2023, annee_pop = 2021)

APL_inf <- bind_rows(APL_inf_2022_clean, APL_inf_2023_clean)

saveRDS(APL_inf, fileoutput_apl_inf)

## Import et prétraitement des données de Karla --------------------------------

# Utilisation des fonctions sur les données
morta_dip <- import_all_sheets(fileinput_morta_dip)
morta_dip <- morta_dip %>%
  filter(Diplme_INDICATEUR != "Âge") %>%
  rename(age = Diplme_INDICATEUR)

morta_cs <- import_all_sheets(fileinput_morta_cs)
morta_cs <- morta_cs %>%
  filter(Catgoriesocioprofessionnelle_INDICATEUR != "Âge") %>%
  rename(age = Catgoriesocioprofessionnelle_INDICATEUR)

# Pivot des deux tables
morta_dip <- morta_dip %>%
  mutate(across(contains("Esprancedevielgex"), as.numeric)) %>%
  pivot_longer(
    cols = -c(int_annee, sexe, age),
    names_to = c("diplome", "indicateur"),
    names_sep = "_",
    values_to = "valeur_dip"
  ) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(indicateur = recode(indicateur,
    "Quotientdemortalitpour100000" = "Quotient de mortalité pour 10000",
    "Survielgex" = "Survie à âge x",
    "Esprancedevielgex" = "Esperance de vie à âge x"
  ))


morta_cs <- morta_cs %>%
  mutate(across(contains("Esprancedevielgex"), as.numeric)) %>%
  pivot_longer(
    cols = -c(int_annee, sexe, age),
    names_to = c("classe_sociale", "indicateur"),
    names_sep = "_",
    values_to = "valeur_cs"
  ) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(indicateur = recode(indicateur,
    "Quotientdemortalitpour100000" = "Quotient de mortalité pour 10000",
    "Survielgex" = "Survie à âge x",
    "Esprancedevielgex" = "Esperance de vie à âge x"
  ))



# Lecture des 2 premières lignes pour récupérer les variables et le sexe
headers <- read_excel(fileinput_morta_cause, n_max = 2, col_names = FALSE)
header1 <- headers[1, ] %>% unlist() # noms des variables
header1 <- fill(data.frame(header1), header1)$header1

sexe_code <- headers[2, ] %>% unlist() # H ou F

dat <- read_excel(fileinput_morta_cause, skip = 2, col_names = FALSE)

# Création des noms uniques
colnames_unique <- paste0(header1, "_", sexe_code)
colnames_unique <- c("Région", colnames_unique)

# On renomme nos entêtes
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
  mutate(
    sexe = "Tous",
    variable = str_remove(variable_sexe, "_Ensemble")
  )

mortalite_combine <- bind_rows(mortalite_cause, mortalite_perinatale_comb) %>% select(-variable_sexe)
# Table a part pour mortalité périnatale
mortalite_perinatale <- mortalite_perinatale_comb %>% select(-variable)

saveRDS(mortalite_combine, fileoutput_morta_combine)
saveRDS(mortalite_perinatale, fileoutput_morta_peri)
saveRDS(mortalite_cause, fileoutput_morta_cause)
saveRDS(morta_dip, fileoutput_morta_dip)
saveRDS(morta_cs, fileoutput_morta_cs)


## Import et prétraitement des données de Cindy ---------------------------------

df_communes_2022 <- charger_apl_medecin_annee(2022)
df_communes_2023 <- charger_apl_medecin_annee(2023)

df_communes <- bind_rows(df_communes_2022, df_communes_2023)


# Chargement données
communes_geo <- st_read(
  "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/georef-france-commune/exports/geojson",
  quiet = TRUE
) %>%
  mutate(code_commune = as.character(com_code))


communes_avec_geo <- communes_geo %>%
  left_join(df_communes, by = "code_commune") %>%
  filter(!substr(code_commune, 1, 2) %in% c("97", "98"))


regions_apl <- communes_avec_geo %>%
  group_by(reg_code, reg_name, annee) %>%
  summarise(
    # apl_moyen par région
    apl_moyen = mean(apl_generalistes, na.rm = TRUE),
    # Population totale par région
    population_totale = sum(pop_totale, na.rm = TRUE), .groups = "drop"
  )


# Sauvegarder les données (format .rds)
saveRDS(list(communes = communes_avec_geo, regions = regions_apl),
  file = fileoutput_apl_gener
)
