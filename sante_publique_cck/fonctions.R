## Fonctions de Clara -------------------------------------------

#' Title : clean_APL_med
#'
#' Cette fonction structure et renomme les colonnes principales d'une table APL médecin selon l'année,
#' elle ajoute également une colonne correspondant à l'année.
#'
#' @param data dataframe contenant les données
#' @param annee entier correspondant à l'année dont les données sont issues
#' @param annee_pop entier correspondant à l'année utilisée dans la variable Population standardisée
#'
clean_APL_med <- function(data, annee, annee_pop) {
  data %>%
    slice(-1) %>%
    rename(
      Code_INSEE = `Code commune INSEE`,
      APL_tous = `APL aux médecins généralistes`,
      APL_65 = `APL aux médecins généralistes de 65 ans et moins`,
      APL_62 = `APL aux médecins généralistes de 62 ans et moins`,
      APL_60 = `APL aux médecins généralistes de 60 ans et moins`,
      Pop_tot = paste0("Population totale ", annee_pop),
      Pop_standardisee_med = paste0("Population standardisée ", annee_pop, " pour la médecine générale")
    ) %>%
    mutate(annee = annee)
}

#' Title : clean_APL_inf
#'
#' Cette fonction structure et renomme les colonnes principales d'une table APL infirmières selon l'année,
#' elle ajoute également une colonne correspondant à l'année et convertit certaines variable en numérique.
#'
#' @param data dataframe contenant les données
#' @param annee entier correspondant à l'année dont les données sont issues
#' @param annee_pop entier correspondant à l'année utilisée dans la variable Population standardisée
#'
clean_APL_inf <- function(data, annee, annee_pop) {
  data %>%
    slice(-1) %>%
    rename(
      Code_INSEE = `Code commune INSEE`,
      APL_infirmiere = `APL aux infirmières`,
      Pop_standardisee = paste0("Population standardisée ", annee_pop, " pour les infirmières"),
      Pop_tot = paste0("Population totale ", annee_pop)
    ) %>%
    mutate(annee = annee, Pop_standardisee = as.numeric(Pop_standardisee), APL_infirmiere = as.numeric(APL_infirmiere))
}


## Fonctions de Karla -------------------------------------------

#' Title : import_sheet
#'
#' Cette fonction lit une feuille Excel dont les deux premières lignes
#' correspondent aux en-têtes, les fusionne, puis importe les données.
#' Elle extrait également des informations (sexe et période) à partir
#' du nom de la feuille.
#'
#' @param file Chemin vers le fichier Excel.
#' @param sheet_name Nom de la feuille à importer.
#'
#'
import_sheet <- function(file, sheet_name) {
  # Lire les deux lignes d'en-tête
  headers <- read_excel(file, sheet = sheet_name, n_max = 2, col_names = FALSE)

  fill_right <- function(x) {
    for (i in seq_along(x)) {
      if (is.na(x[i]) && i > 1) {
        x[i] <- x[i - 1]
      }
    }
    x
  }

  header1 <- headers[1, ] %>%
    unlist() %>%
    fill_right()
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

  territoire <- parts[1] # FM
  sexe_code <- parts[2] # H ou F
  annees <- parts[3] # 2009_2013

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


#' Title : import_all_sheets
#'
#' Cette fonction importe l'ensemble des feuilles d'un fichier Excel
#' en utilisant la fonction `import_sheet()` et les assemble en un seul
#' data.frame.
#'
#' @param file Chemin vers le fichier Excel.
#'
#'
import_all_sheets <- function(file) {
  sheet_names <- excel_sheets(file)
  map_dfr(sheet_names, ~ import_sheet(file, .x))
}


## Fonctions de Cindy ---------------------------------------

#' Title : charger_apl_medecin_annee
#'
#' Cette fonction lit les données APL depuis un fichier Excel,
#' nettoie les colonnes et ajoute l'année correspondante.
#'
#' @param annee Année d'intérêt (numérique).
#'
charger_apl_medecin_annee <- function(annee) {
  df <- read_excel(
    fileinput_apl_gener,
    sheet = paste0("APL ", annee), skip = 8
  ) %>%
    slice(-1) %>%
    # Rename des colonnes
    rename(
      code_commune = `Code commune INSEE`,
      commune = Commune,
      apl_generalistes = `APL aux médecins généralistes`,
      pop_totale = paste0("Population totale ", annee - 2)
    ) %>%
    # Modification type colonnes
    mutate(
      code_commune = as.character(code_commune),
      apl_generalistes = as.numeric(apl_generalistes),
      pop_totale = as.numeric(pop_totale),
      annee = annee
    )

  return(df)
}

## Fonctions de téléchargement -------------------------

#' Title : download_plot
#'
#' Téchécharge un graphique en pdf, elle gère à la fois un nom de fichier fixe
#' ou dynamique et affiche une notification lors du téléchargement.
#'
#' @param output l'objet output de shiny
#' @param input l'objet input de shiny, seulement si nom dynamique
#' @param output_id caractère : nom de l’élément de sortie shiny
#' @param plot_name fonction retournant un ggplot à télécharger
#' @param name_pdf caractère pour un nom fixe, ou fonction pour un nom dynamique
#'
download_plot <- function(output, input = NULL, output_id, plot_name, name_pdf) {
  output[[output_id]] <- downloadHandler(
    filename = if (is.function(name_pdf)) {
      function() paste0(name_pdf(input), ".pdf")
    } else {
      function() paste0(name_pdf, ".pdf")
    },
    content = function(file) {
      ggsave(file, plot = plot_name(), device = "pdf")

      # Notification si succès de téléchargement
      showNotification("Téléchargement réussi !", type = "message", duration = 5)
    }
  )
}

#' Title : download_map
#'
#' Téchécharge une carte en png et affiche une notification lors du téléchargement.
#'
#' @param output l'objet output de shiny
#' @param input l'objet input de shiny
#' @param output_id caractère : nom de l’élément de sortie shiny
#' @param plot_reactive fonction retournant la carte à télécharger
#' @param prefix caractère : préfixe utilisé pour le nom du fichier
#'
download_map <- function(output, input, output_id, plot_reactive, prefix) {
  output[[output_id]] <- downloadHandler(
    filename = function() {
      paste0(prefix, "_", input$annee_choisie, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = plot_reactive())

      showNotification("Téléchargement réussi !", type = "message", duration = 5)
    }
  )
}
