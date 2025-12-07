
library(dplyr)
library(purrr)
library(readxl)
library(stringr)
library(tidyr)
library(sf)


## Import et prétraitement des données de Clara --------------------------------

# Medecin

medecin<-read_excel("data/Med_2012_2025.xlsx", sheet=2)


medecin_clean <- medecin %>% filter(!(substr(territoire, 1, 1) %in% c("0", "3")), region != "00-Ensemble", 
                                    sexe=="0-Ensemble", departement!="000-Ensemble", exercice == "0-Ensemble",
                                    tranche_age=="00-Ensemble", specialites != "00-Ensemble")

medecin_long <- medecin_clean %>% pivot_longer(cols=starts_with("effectif_"),
                                               names_to = "annee", values_to = "effectif") %>% 
  mutate(annee=as.integer(sub("effectif_", "", annee)))

saveRDS(medecin_long, "donnees_traitees/medecin_long.rds")

# Inf liberal

inf_lib<-read_excel("data/Inf_lib_2012_2023.xlsx", sheet=2)

inf_lib_clean <- inf_lib %>% 
  filter(region != "00 - Ensemble", departement != "00 - Ensemble", sexe=="0 - Ensemble", tranche_age == "00 - Ensemble") %>% 
  select(-...17)

inf_lib_long <- inf_lib_clean %>% 
  pivot_longer(cols=starts_with("effectif_"), names_to="annee", values_to="effectif") %>% 
  mutate(annee=as.integer(sub("effectif_", "", annee))) %>% 
  mutate(departement=str_replace_all(departement, " ", "")) %>% 
  mutate(region=str_replace_all(region, " ", "")) %>% 
  filter(!annee %in% c(2012, 2022, 2023))

inf_lib_long <- inf_lib_long %>% mutate(data_type = "Libéraux")

saveRDS(inf_lib_long, "donnees_traitees/inf_lib_long.rds")


# Inf sal

inf_sal<-read_excel("data/Inf_sal_2013_2021.xlsx", sheet=2)

inf_sal_clean_names <- inf_sal %>% 
  rename(secteur_activite = `Secteur d'activité`, specialite = `Spécialité`, Tranche_age = `Tranche d'âge`, region = `Région`,
         departement = `Département`)

inf_sal_clean <- inf_sal_clean_names %>% 
  filter(region != "00-France entière (hors Mayotte)", departement != "00-Région entière", departement != "00-Région entière",
         secteur_activite == "0-Ensemble", specialite == "0-Ensemble", Sexe == "0-Ensemble", Tranche_age == "00-Ensemble")

inf_sal_long <- inf_sal_clean %>% 
  pivot_longer(cols=starts_with("effectif_"), names_to="annee", values_to="effectif") %>% 
  mutate(annee=as.integer(sub("effectif_", "", annee)))

inf_sal_long <- inf_sal_long %>% mutate(data_type = "Salariés")

saveRDS(inf_sal_long, "donnees_traitees/inf_sal_long.rds")

# APL med

APL_med_2022 <- read_excel("data/APL_med_gen.xlsx", sheet=2, skip=8)
APL_med_2023 <- read_excel("data/APL_med_gen.xlsx", sheet=3, skip=8)

APL_med_2022_clean <- APL_med_2022 %>% slice(-1) %>% 
  rename(Code_INSEE=`Code commune INSEE`, APL_tous=`APL aux médecins généralistes`, 
         APL_65 =`APL aux médecins généralistes de 65 ans et moins`,
         APL_62=`APL aux médecins généralistes de 62 ans et moins`,
         APL_60=`APL aux médecins généralistes de 60 ans et moins`, 
         Pop_tot=`Population totale 2020`,
         Pop_standardisee_med=`Population standardisée 2020 pour la médecine générale`) %>% mutate(annee=2022)

APL_med_2023_clean <- APL_med_2023 %>% slice(-1) %>% 
  rename(Code_INSEE=`Code commune INSEE`, APL_tous=`APL aux médecins généralistes`, 
         APL_65 =`APL aux médecins généralistes de 65 ans et moins`,
         APL_62=`APL aux médecins généralistes de 62 ans et moins`,
         APL_60=`APL aux médecins généralistes de 60 ans et moins`, 
         Pop_tot=`Population totale 2021`,
         Pop_standardisee_med=`Population standardisée 2021 pour la médecine générale`) %>% mutate(annee=2023)

APL_med <- bind_rows(APL_med_2022_clean, APL_med_2023_clean)

APL_med_long <- APL_med %>%
  select(Commune, Code_INSEE, Pop_standardisee_med, APL_tous, APL_65, APL_62, APL_60, annee) %>%
  pivot_longer(cols=starts_with("APL"), names_to="age_medecins", values_to="APL") %>% 
  mutate(Pop_standardisee_med = as.numeric(Pop_standardisee_med), 
         APL=as.numeric(APL), age_medecins=factor(age_medecins))

saveRDS(APL_med_long, "donnees_traitees/APL_med_long.rds")

# APL inf 

APL_inf_2022 <- read_excel("data/APL_inf.xlsx", sheet=2, skip=8)
APL_inf_2023 <- read_excel("data/APL_inf.xlsx", sheet=3, skip=8)

APL_inf_2022_clean <- APL_inf_2022 %>% slice(-1) %>% 
  rename(Code_INSEE=`Code commune INSEE`, APL_infirmiere=`APL aux infirmières`, 
         Pop_standardisee=`Population standardisée 2020 pour les infirmières`,
         Pop_tot=`Population totale 2020`) %>% mutate(annee=2022) %>% 
  mutate(Pop_standardisee=as.numeric(Pop_standardisee), 
         APL_infirmiere=as.numeric(APL_infirmiere))

APL_inf_2023_clean <- APL_inf_2023 %>% slice(-1) %>% 
  rename(Code_INSEE=`Code commune INSEE`, APL_infirmiere=`APL aux infirmières`, 
         Pop_standardisee=`Population standardisée 2021 pour les infirmières`,
         Pop_tot=`Population totale 2021`) %>% mutate(annee=2023) %>% 
  mutate(Pop_standardisee=as.numeric(Pop_standardisee), 
         APL_infirmiere=as.numeric(APL_infirmiere))

APL_inf <- bind_rows(APL_inf_2022_clean, APL_inf_2023_clean)

saveRDS(APL_inf, "donnees_traitees/APL_inf.rds")

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
  
  dat <- dat %>%
    mutate(
      sexe = case_when(
        sexe_code == "H" ~ "Homme",
        sexe_code == "F" ~ "Femme",
        TRUE ~ sexe_code
      ),
      int_annee = annees

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

morta_dip <- morta_dip %>%
  mutate(across(contains("Esprancedevielgex"), as.numeric)) %>% 
  pivot_longer(cols = -c(int_annee, sexe, age),
               names_to = c("diplome", "indicateur"),
               names_sep = "_",
               values_to = "valeur_dip") %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(indicateur = recode(indicateur,
                             "Quotientdemortalitpour100000"="Quotient de mortalité pour 10000" , 
                             "Survielgex"="Survie à âge x",
                             "Esprancedevielgex"="Esperance de vie à âge x"))


morta_cs <- morta_cs %>%
  mutate(across(contains("Esprancedevielgex"), as.numeric)) %>% 
  pivot_longer(cols = -c(int_annee, sexe, age),
               names_to = c("classe_sociale", "indicateur"),
               names_sep = "_",
               values_to = "valeur_cs") %>%
  mutate(age = as.numeric(age)) %>%   
  mutate(indicateur = recode(indicateur,
                             "Quotientdemortalitpour100000"="Quotient de mortalité pour 10000" , 
                             "Survielgex"="Survie à âge x",
                             "Esprancedevielgex"="Esperance de vie à âge x"))



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

#Lecture et preparation des données
charger_apl_medecin_annee <- function(annee){
  df <- read_excel(
    "data/Indicateur d'accessibilité potentielle localisée (APL) aux médecins généralistes.xlsx",
    sheet= paste0("APL ", annee), skip=8) %>%
    slice(-1) %>%
    #Rename des colonnes
    rename(
      code_commune=`Code commune INSEE`,
      commune=Commune,
      apl_generalistes=`APL aux médecins généralistes`,
      pop_totale=paste0("Population totale ", annee-2)
    ) %>%
    #Modification type colonnes
    mutate(
      code_commune=as.character(code_commune),
      apl_generalistes=as.numeric(apl_generalistes),
      pop_totale=as.numeric(pop_totale),
      annee = annee)
  
  return(df)
}

#chargement données par années
df_communes_2022 <- charger_apl_medecin_annee(2022)
df_communes_2023 <- charger_apl_medecin_annee(2023)
#Combine des deux années
df_communes <- bind_rows(df_communes_2022,df_communes_2023)


#Chargement données
#quiet = TRUE : pas d'affichage
communes_geo <- st_read(
  "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/georef-france-commune/exports/geojson",
  quiet = TRUE)%>%
  mutate(code_commune=as.character(com_code))


# Fusion de commune et nos données par code_commune
communes_avec_geo <- communes_geo %>%
  left_join(df_communes, by="code_commune")%>%
  filter(!substr(code_commune, 1, 2) %in% c("97", "98"))


#Regroupe les communes par région
regions_apl<-communes_avec_geo %>%
  group_by(reg_code, reg_name, annee) %>%
  summarise(
    #apl_moyen par région
    apl_moyen=mean(apl_generalistes, na.rm=TRUE),
    #Population totale par région
    population_totale=sum(pop_totale, na.rm=TRUE), .groups="drop")


#Sauvegarder les données (format .rds)
saveRDS( list( communes = communes_avec_geo, regions = regions_apl),
         file = "donnees_traitees/donnees_propres_apl_generalistes.rds")

