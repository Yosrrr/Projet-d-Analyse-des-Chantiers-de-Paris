
#PROJET D'ANALYSE DES CHANTIERS À PARIS
#Source des données : OpenData Paris

# OBJECTIF DU PROJET :
# Ce script permet d'analyser les données des chantiers de la Ville de Paris.
# On va télécharger les données depuis internet, les nettoyer, faire des
# statistiques et créer des graphiques pour mieux comprendre la situation
# des chantiers à Paris.

# 1 : PRÉPARATION
# 1.1 - Installation des packages (bibliothèques) nécessaires
# En R, les "packages" sont des bibliothèques qui ajoutent des fonctionnalités.
# Par exemple :
#   - tidyverse : pour manipuler facilement les données (filtrer, trier, etc.)
#   - ggplot2 : pour créer de beaux graphiques (inclus dans tidyverse)
#   - leaflet : pour créer des cartes interactives
#   - httr : pour télécharger des données depuis internet

# Liste de tous les packages dont on a besoin
mes_packages <- c(
  "tidyverse",    
  "lubridate",   
  "janitor",      
  "scales",      
  "leaflet",      
  "plotly",      
  "knitr",       
  "kableExtra",   
  "htmlwidgets",  
  "httr",        
  "jsonlite"      
)

# On vérifie quels packages ne sont pas encore installés
packages_a_installer <- mes_packages[!(mes_packages %in% installed.packages()[,"Package"])]

# Si il y a des packages manquants, on les installe
if(length(packages_a_installer) > 0) {
  print("Installation des packages manquants...")
  install.packages(packages_a_installer)
}

# 1.2 - Chargement des packages
# Maintenant on charge les packages pour pouvoir les utiliser
# La fonction library() permet de charger un package

library(tidyverse)    # Manipulation de données + graphiques
library(lubridate)    # Gestion des dates
library(janitor)      # Nettoyage des données
library(scales)       # Formatage des nombres
library(leaflet)      # Cartes interactives
library(plotly)       # Graphiques interactifs
library(knitr)        # Tableaux
library(kableExtra)   # Amélioration des tableaux
library(htmlwidgets)  # Export HTML
library(httr)         # Requêtes internet
library(jsonlite)     # Lecture JSON
library(stringr)
library(lubridate)
library(tidyr)
library(dplyr)
print("Tous les packages sont chargés !")

# PARTIE 2 : TÉLÉCHARGEMENT DES DONNÉES

print("TÉLÉCHARGEMENT DES DONNÉES DEPUIS PARIS OPENDATA")
  # 2.1 - Configuration de l'URL de l'API
# Voici l'adresse de l'API pour les données des chantiers
api_base_url <- "https://opendata.paris.fr/api/explore/v2.1/catalog/datasets/chantiers-a-paris/records"

# Fonction pour télécharger toutes les données (avec pagination)
telecharger_donnees <- function(limit_par_page = 100) {
  
  tous_les_enregistrements <- list()
  offset <- 0
  total_count <- NULL
  
  cat("Connexion à l'API OpenData Paris...\n")
  
  repeat {
    # Construction de l'URL avec paramètres
    url <- paste0(
      api_base_url,
      "?limit=", limit_par_page,
      "&offset=", offset
    )
    cat("Téléchargement des enregistrements", offset + 1, "à", offset + limit_par_page, "...\n")
    
    # Requête GET
    response <- tryCatch({
      GET(url, timeout(60))
    }, error = function(e) {
      cat("Erreur de connexion:", e$message, "\n")
      return(NULL)
    })
    
    # Vérification de la réponse
    if(is.null(response) || status_code(response) != 200) {
      cat("Erreur HTTP:", status_code(response), "\n")
      break
    }
    
    # Parsing du JSON
    contenu <- content(response, "text", encoding = "UTF-8")
    donnees_json <- fromJSON(contenu, flatten = TRUE)
    
    # Récupération du nombre total si pas encore fait
    if(is.null(total_count)) {
      total_count <- donnees_json$total_count
      cat("Nombre total d'enregistrements disponibles:", total_count, "\n\n")
    }
    
    # Extraction des résultats
    if(length(donnees_json$results) == 0) {
      break
    }
    
    tous_les_enregistrements[[length(tous_les_enregistrements) + 1]] <- donnees_json$results
    
    # Vérification si on a tout récupéré
    offset <- offset + limit_par_page
    if(offset >= total_count) {
      break
    }
  }
  
  # Combinaison de tous les enregistrements
  if(length(tous_les_enregistrements) > 0) {
    df_final <- bind_rows(tous_les_enregistrements)
    cat("\n✓ Téléchargement terminé:", nrow(df_final), "enregistrements récupérés\n\n")
    return(df_final)
  } else {
    cat("Aucune donnée récupérée!\n")
    return(NULL)
  }
}

# Téléchargement des données
chantiers_raw <- telecharger_donnees(limit_par_page = 100)

# Vérification que les données ont été téléchargées
if(is.null(chantiers_raw) || nrow(chantiers_raw) == 0) {
  stop("Impossible de télécharger les données. Vérifiez votre connexion internet.")
}


  # 2.3 - Exécution du téléchargement
  
# On appelle notre fonction pour télécharger les données
donnees_brutes <- telecharger_toutes_les_donnees()

# On vérifie que le téléchargement a fonctionné
if(is.null(donnees_brutes) || nrow(donnees_brutes) == 0) {
  stop("ERREUR : Impossible de télécharger les données. Vérifiez votre connexion internet.")
}

  # 2.4 - Premier aperçu des données
print("APERÇU DES DONNÉES BRUTES")
# Combien de lignes (chantiers) et colonnes (variables) ?
print(paste("Nombre de chantiers:", nrow(donnees_brutes)))
print(paste("Nombre de variables:", ncol(donnees_brutes)))

# Quels sont les noms des colonnes ?
print("Noms des colonnes :")
print(colnames(donnees_brutes))

# Aperçu de la structure des données
print("Structure des données :")
glimpse(donnees_brutes)
#PARTIE 3 : NETTOYAGE DES DONNÉES

# Les données brutes ont souvent besoin d'être "nettoyées" :
# - Renommer les colonnes pour qu'elles soient plus claires
# - Convertir les types de données (texte en date, texte en nombre, etc.)
# - Gérer les valeurs manquantes

print("NETTOYAGE DES DONNÉES")

  # 3.1 - Nettoyage des noms de colonnes
  
# La fonction clean_names() du package janitor nettoie les noms :
# - Met tout en minuscules
# - Remplace les espaces par des underscores
# - Supprime les accents et caractères spéciaux
chantiers <- clean_names(donnees_brutes)

# On affiche les nouveaux noms
print("Noms des colonnes après nettoyage :")
print(colnames(chantiers))

  # 3.2 - Renommage des colonnes pour plus de clarté
  
# On va renommer certaines colonnes pour qu'elles soient plus compréhensibles
# La fonction rename_with permet de renommer plusieurs colonnes d'un coup

chantiers <- chantiers %>%
  rename_with(~ case_when(
    . == "chantier_reference" ~ "reference",
    . == "cp_arrondissement" ~ "code_postal",
    . == "chantier_date_debut" ~ "date_debut",
    . == "chantier_date_fin" ~ "date_fin",
    . == "chantier_categorie" ~ "responsable",
    . == "moa_principal" ~ "maitre_ouvrage",
    . == "chantier_localisation_surface" ~ "surface",
    . == "chantier_synthese" ~ "nature",
    . == "localisation_detail" ~ "encombrement",
    . == "localisation_stationnement" ~ "impact_stat",
    TRUE ~ .  # Les autres colonnes gardent leur nom
  ))

print("Colonnes après renommage :")
print(colnames(chantiers))


  # 3.3 - Transformation des données  
# On va maintenant transformer les données pour les rendre exploitables

chantiers <- chantiers %>%
  mutate(
    # Dates
    date_debut = as.Date(date_debut),
    date_fin   = as.Date(date_fin),

    # Durée du chantier
    duree_jours = as.numeric(date_fin - date_debut),

    # Arrondissement (robuste, sans warning)
    arrondissement = as.integer(str_extract(code_postal, "\\d{2}$")),
    # Variables temporelles
    annee_debut = year(date_debut),
    mois_debut  = month(date_debut, label = TRUE, abbr = FALSE),
    trimestre_debut = quarter(date_debut),

    # Statut du chantier
    statut = case_when(
      date_fin < Sys.Date() ~ "Terminé",
      date_debut > Sys.Date() ~ "À venir",
      TRUE ~ "En cours"
    ),

    # Nettoyage de la surface
    surface = as.numeric(
      str_replace_all(as.character(surface), "[^0-9.]", "")
    ),

    # Sécurisation des types
    encombrement = as.character(encombrement),
    impact_stat  = as.character(impact_stat)
  ) %>%
  mutate(
    # Gestion des NA
    surface = replace_na(surface, 0),
    encombrement = replace_na(encombrement, "Non spécifié"),
    impact_stat  = replace_na(impact_stat, "Aucun")
  ) %>%
  filter(
    !is.na(date_debut),
    !is.na(date_fin),
    duree_jours >= 0
  ) 

  # 3.4 - Extraction des coordonnées géographiques
  
# Les coordonnées GPS servent à placer les chantiers sur une carte
# L'API peut les fournir sous différents formats

# On vérifie si les colonnes de coordonnées existent
if("geo_point_2d_lat" %in% colnames(chantiers) & "geo_point_2d_lon" %in% colnames(chantiers)) {
  # Format 1 : colonnes séparées pour latitude et longitude
  chantiers <- chantiers %>%
    mutate(
      latitude = as.numeric(geo_point_2d_lat),
      longitude = as.numeric(geo_point_2d_lon)
    )
} else if("geo_point_2d" %in% colnames(chantiers)) {
  # Format 2 : coordonnées dans une seule colonne (liste)
  chantiers <- chantiers %>%
    mutate(
      latitude = sapply(geo_point_2d, function(x) if(!is.null(x) && length(x) >= 1) x[[1]] else NA),
      longitude = sapply(geo_point_2d, function(x) if(!is.null(x) && length(x) >= 2) x[[2]] else NA)
    )
} else {
  # Pas de coordonnées disponibles
  print("Attention : Pas de coordonnées géographiques trouvées")
  chantiers$latitude <- NA
  chantiers$longitude <- NA
}
  # 3.5 - Résumé des données nettoyées
  
 
print("RÉSUMÉ DES DONNÉES NETTOYÉES")
 

print(paste("Nombre total de chantiers:", nrow(chantiers)))
print(paste("Période couverte:", min(chantiers$date_debut, na.rm = TRUE), 
            "à", max(chantiers$date_fin, na.rm = TRUE)))

# Aperçu de la structure finale
print("Structure finale des données :")
glimpse(chantiers)

#PARTIE 4 : ANALYSES STATISTIQUES DESCRIPTIVES
 
# L'analyse descriptive permet de comprendre les données avec des chiffres :
# - Compter combien de chantiers par catégorie
# - Calculer des moyennes, médianes, etc.
# - Identifier les tendances

 
print("ANALYSES STATISTIQUES DESCRIPTIVES")
 

  # 4.1 - Répartition des chantiers par arrondissement

print("4.1 - RÉPARTITION PAR ARRONDISSEMENT")

# On compte le nombre de chantiers par arrondissement
# count() compte les occurrences de chaque valeur
# arrange() trie les résultats (desc = ordre décroissant)

stats_arrondissement <- chantiers %>%
  filter(!is.na(arrondissement)) %>%        # On enlève les valeurs manquantes
  count(arrondissement, name = "nb_chantiers") %>%  # On compte par arrondissement
  arrange(desc(nb_chantiers)) %>%           # On trie du plus grand au plus petit
  mutate(
    # On calcule le pourcentage
    pourcentage = round(nb_chantiers / sum(nb_chantiers) * 100, 2),
    # On calcule le cumul des pourcentages
    cumul = cumsum(pourcentage)
  )

# On affiche le résultat
print(stats_arrondissement)

  # 4.2 - Répartition par nature de chantier

print("4.2 - RÉPARTITION PAR NATURE DE CHANTIER")

stats_nature <- chantiers %>%
  filter(!is.na(nature)) %>%
  count(nature, name = "nb_chantiers") %>%
  arrange(desc(nb_chantiers)) %>%
  mutate(pourcentage = round(nb_chantiers / sum(nb_chantiers) * 100, 2))

print(stats_nature)

  # 4.3 - Répartition par responsable (catégorie de chantier)

print("4.3 - RÉPARTITION PAR RESPONSABLE")

stats_responsable <- chantiers %>%
  filter(!is.na(responsable)) %>%
  count(responsable, name = "nb_chantiers") %>%
  arrange(desc(nb_chantiers)) %>%
  mutate(pourcentage = round(nb_chantiers / sum(nb_chantiers) * 100, 2))

print(stats_responsable)

  # 4.4 - Statistiques sur la surface des chantiers
  

print("4.4 - STATISTIQUES SUR LA SURFACE (m²)")


# On calcule plusieurs indicateurs statistiques
# summarise() permet de calculer des résumés

stats_surface <- chantiers %>%
  filter(surface > 0) %>%    # On garde seulement les surfaces positives
  summarise(
    minimum = min(surface, na.rm = TRUE),           # Plus petite valeur
    premier_quartile = quantile(surface, 0.25, na.rm = TRUE),  # 25% des valeurs sont en dessous
    mediane = median(surface, na.rm = TRUE),        # Valeur du milieu
    moyenne = mean(surface, na.rm = TRUE),          # Moyenne arithmétique
    troisieme_quartile = quantile(surface, 0.75, na.rm = TRUE), # 75% des valeurs sont en dessous
    maximum = max(surface, na.rm = TRUE),           # Plus grande valeur
    ecart_type = sd(surface, na.rm = TRUE),         # Mesure de dispersion
    surface_totale = sum(surface, na.rm = TRUE)     # Somme de toutes les surfaces
  )

print(stats_surface)

  # 4.5 - Statistiques sur la durée des chantiers

print("4.5 - STATISTIQUES SUR LA DURÉE (jours)")

stats_duree <- chantiers %>%
  filter(!is.na(duree_jours) & duree_jours > 0) %>%
  summarise(
    minimum = min(duree_jours),
    premier_quartile = quantile(duree_jours, 0.25),
    mediane = median(duree_jours),
    moyenne = mean(duree_jours),
    troisieme_quartile = quantile(duree_jours, 0.75),
    maximum = max(duree_jours),
    ecart_type = sd(duree_jours)
  )

print(stats_duree)

  # 4.6 - Répartition par statut (en cours, terminé, à venir)

print("4.6 - RÉPARTITION PAR STATUT")


stats_statut <- chantiers %>%
  count(statut, name = "nb_chantiers") %>%
  mutate(pourcentage = round(nb_chantiers / sum(nb_chantiers) * 100, 2))

print(stats_statut)

  # 4.7 - Impact sur l'espace public
  

print("4.7 - IMPACT SUR L'ESPACE PUBLIC")


stats_encombrement <- chantiers %>%
  count(encombrement, name = "nb_chantiers") %>%
  arrange(desc(nb_chantiers)) %>%
  mutate(pourcentage = round(nb_chantiers / sum(nb_chantiers) * 100, 2))

print(stats_encombrement)


 
#                   PARTIE 5 : CRÉATION DES GRAPHIQUES
 

# Les graphiques permettent de visualiser les données de façon claire
# On utilise ggplot2 (inclus dans tidyverse) qui est très puissant
# 
# Structure d'un graphique ggplot :
# 1. ggplot(données, aes(x=..., y=...)) : définit les données et les axes
# 2. geom_xxx() : définit le type de graphique (barres, points, lignes...)
# 3. labs() : ajoute les titres et légendes
# 4. theme_xxx() : définit le style visuel

 
print("CRÉATION DES GRAPHIQUES")
  # 5.1 - Graphique en barres : Chantiers par arrondissement
  
print("Création du graphique 1 : Chantiers par arrondissement...")

graphique1 <- ggplot(
  data = stats_arrondissement,  # Données à utiliser
  aes(
    x = reorder(factor(arrondissement), nb_chantiers),  # Axe X = arrondissement (trié)
    y = nb_chantiers                                     # Axe Y = nombre de chantiers
  )
) +
  # geom_col() crée un graphique en barres
  geom_col(fill = "steelblue", alpha = 0.8) +
  # geom_text() ajoute les valeurs sur les barres
  geom_text(aes(label = nb_chantiers), hjust = -0.2, size = 3) +
  # coord_flip() fait des barres horizontales (plus lisible)
  coord_flip() +
  # labs() définit les titres
  labs(
    title = "Nombre de chantiers par arrondissement",
    subtitle = "Source: API OpenData Paris",
    x = "Arrondissement",
    y = "Nombre de chantiers"
  ) +
  # theme_minimal() applique un style épuré
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10)
  ) +
  # scale_y_continuous ajuste l'échelle de l'axe Y
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

# On affiche le graphique
print(graphique1)

# On sauvegarde le graphique en PNG
# ggsave() permet de sauvegarder un graphique ggplot
ggsave("graphique_arrondissements.png", graphique1, width = 10, height = 8, dpi = 300)
print("Graphique sauvegardé : graphique_arrondissements.png")

  # 5.2 - Graphique en barres : Répartition par nature
print("Création du graphique 2 : Répartition par nature...")

graphique2 <- ggplot(
  data = stats_nature,
  aes(x = reorder(nature, nb_chantiers), y = nb_chantiers, fill = nature)
) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  geom_text(aes(label = nb_chantiers), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Répartition par nature de chantier",
    x = "Nature du chantier",
    y = "Nombre de chantiers"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  # scale_fill_brewer utilise une palette de couleurs prédéfinie
  scale_fill_brewer(palette = "Set2")

print(graphique2)
ggsave("graphique_nature_chantiers.png", graphique2, width = 12, height = 8, dpi = 300)
print("Graphique sauvegardé : graphique_nature_chantiers.png")

  # 5.3 - Histogramme : Distribution de la surface

print("Création du graphique 3 : Distribution de la surface...")

# On calcule la médiane pour l'afficher sur le graphique
mediane_surface <- median(chantiers$surface[chantiers$surface > 0], na.rm = TRUE)

graphique3 <- chantiers %>%
  # On filtre pour enlever les valeurs extrêmes (outliers)
  # quantile(surface, 0.95) = valeur en dessous de laquelle se trouvent 95% des données
  filter(surface > 0 & surface < quantile(surface, 0.95, na.rm = TRUE)) %>%
  ggplot(aes(x = surface)) +
  # geom_histogram() crée un histogramme
  geom_histogram(bins = 50, fill = "coral", color = "white", alpha = 0.8) +
  # geom_vline() ajoute une ligne verticale pour la médiane
  geom_vline(xintercept = mediane_surface, color = "darkred", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribution de la surface des chantiers",
    subtitle = paste("Médiane:", round(mediane_surface, 2), "m²"),
    x = "Surface (m²)",
    y = "Nombre de chantiers"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(graphique3)
ggsave("graphique_distribution_surface.png", graphique3, width = 10, height = 6, dpi = 300)
print("Graphique sauvegardé : graphique_distribution_surface.png")

  # 5.4 - Histogramme : Distribution de la durée

print("Création du graphique 4 : Distribution de la durée...")

mediane_duree <- median(chantiers$duree_jours[chantiers$duree_jours > 0], na.rm = TRUE)

graphique4 <- chantiers %>%
  filter(!is.na(duree_jours) & duree_jours > 0 & duree_jours < 365) %>%
  ggplot(aes(x = duree_jours)) +
  geom_histogram(bins = 40, fill = "darkgreen", color = "white", alpha = 0.8) +
  geom_vline(xintercept = mediane_duree, color = "darkred", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribution de la durée des chantiers",
    subtitle = paste("Médiane:", round(mediane_duree, 0), "jours (chantiers < 1 an)"),
    x = "Durée (jours)",
    y = "Nombre de chantiers"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(graphique4)
ggsave("graphique_distribution_duree.png", graphique4, width = 10, height = 6, dpi = 300)
print("Graphique sauvegardé : graphique_distribution_duree.png")

  # 5.5 - Graphique linéaire : Évolution temporelle
print("Création du graphique 5 : Évolution temporelle...")

# On prépare les données : nombre de chantiers par mois
evolution_par_mois <- chantiers %>%
  filter(!is.na(date_debut)) %>%
  mutate(
    # floor_date arrondit la date au premier jour du mois
    mois = floor_date(date_debut, "month")
  ) %>%
  count(mois, name = "nb_chantiers")

graphique5 <- ggplot(evolution_par_mois, aes(x = mois, y = nb_chantiers)) +
  # geom_line() crée une ligne
  geom_line(color = "steelblue", linewidth = 1) +
  # geom_point() ajoute des points
  geom_point(color = "steelblue", size = 2) +
  # geom_smooth() ajoute une courbe de tendance
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2, color = "darkred") +
  labs(
    title = "Évolution mensuelle du nombre de chantiers débutés",
    x = "Date",
    y = "Nombre de chantiers"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  # Formatage des dates sur l'axe X
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months")

print(graphique5)
ggsave("graphique_evolution_temporelle.png", graphique5, width = 12, height = 6, dpi = 300)
print("Graphique sauvegardé : graphique_evolution_temporelle.png")

  # 5.6 - Graphique en barres : Surface moyenne par arrondissement
print("Création du graphique 6 : Surface moyenne par arrondissement...")

# On calcule la surface moyenne par arrondissement
surface_par_arr <- chantiers %>%
  filter(!is.na(arrondissement) & surface > 0) %>%
  group_by(arrondissement) %>%
  summarise(
    surface_moyenne = mean(surface, na.rm = TRUE),
    surface_totale = sum(surface, na.rm = TRUE),
    nb_chantiers = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(surface_moyenne))

graphique6 <- ggplot(
  surface_par_arr, 
  aes(x = reorder(factor(arrondissement), surface_moyenne), y = surface_moyenne)
) +
  geom_col(fill = "purple", alpha = 0.7) +
  geom_text(aes(label = round(surface_moyenne, 1)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Surface moyenne des chantiers par arrondissement",
    x = "Arrondissement",
    y = "Surface moyenne (m²)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(graphique6)
ggsave("graphique_surface_par_arrondissement.png", graphique6, width = 10, height = 8, dpi = 300)
print("Graphique sauvegardé : graphique_surface_par_arrondissement.png")

  # 5.7 - Boxplot : Durée par nature de chantier
print("Création du graphique 7 : Durée par nature (boxplot)...")

# Un boxplot montre la distribution d'une variable :
# - La ligne au milieu = médiane
# - La boîte = 50% des valeurs (entre Q1 et Q3)
# - Les moustaches = valeurs min et max (hors outliers)
# - Les points = outliers (valeurs extrêmes)

graphique7 <- chantiers %>%
  filter(!is.na(duree_jours) & duree_jours > 0 & duree_jours < 500 & !is.na(nature)) %>%
  ggplot(aes(x = reorder(nature, duree_jours, FUN = median), y = duree_jours, fill = nature)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3, show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Durée des chantiers par nature",
    x = "Nature du chantier",
    y = "Durée (jours)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14)) +
  scale_fill_brewer(palette = "Set2")

print(graphique7)
ggsave("graphique_duree_par_nature.png", graphique7, width = 12, height = 8, dpi = 300)
print("Graphique sauvegardé : graphique_duree_par_nature.png")

  # 5.8 - Camembert : Répartition par statut
print("Création du graphique 8 : Répartition par statut (camembert)...")

# Un camembert (pie chart) montre les proportions
# En ggplot2, on crée d'abord un graphique en barres empilées
# puis on le transforme en cercle avec coord_polar()

graphique8 <- stats_statut %>%
  ggplot(aes(x = "", y = nb_chantiers, fill = statut)) +
  geom_col(width = 1, color = "white") +
  # coord_polar transforme en camembert
  coord_polar("y", start = 0) +
  # On ajoute les pourcentages
  geom_text(
    aes(label = paste0(pourcentage, "%")), 
    position = position_stack(vjust = 0.5), 
    size = 4
  ) +
  labs(
    title = "Répartition des chantiers par statut",
    fill = "Statut"
  ) +
  # theme_void() enlève les axes (plus joli pour un camembert)
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5)) +
  # Couleurs personnalisées pour chaque statut
  scale_fill_manual(values = c("En cours" = "orange", "Terminé" = "green", "À venir" = "steelblue"))

print(graphique8)
ggsave("graphique_statut_chantiers.png", graphique8, width = 8, height = 8, dpi = 300)
print("Graphique sauvegardé : graphique_statut_chantiers.png")

#PARTIE 6 : CARTE INTERACTIVE
# La carte interactive permet de visualiser la localisation des chantiers
# On utilise le package leaflet qui crée des cartes web

 
print("CRÉATION DE LA CARTE INTERACTIVE")
# On filtre les chantiers qui ont des coordonnées
chantiers_avec_coords <- chantiers %>%
  filter(!is.na(latitude) & !is.na(longitude))

print(paste("Nombre de chantiers avec coordonnées:", nrow(chantiers_avec_coords)))

# On crée la carte seulement si on a des coordonnées
if(nrow(chantiers_avec_coords) > 0) {
  
  # Si il y a trop de points, on prend un échantillon aléatoire
  # pour éviter de ralentir la carte
  if(nrow(chantiers_avec_coords) > 1000) {
    set.seed(123)  # Pour avoir toujours le même échantillon
    chantiers_carte <- chantiers_avec_coords %>% sample_n(1000)
    print("Échantillon de 1000 chantiers sélectionné pour la carte")
  } else {
    chantiers_carte <- chantiers_avec_coords
  }
  
  # On crée une palette de couleurs selon le statut
  # colorFactor() associe des couleurs aux valeurs
  palette_couleurs <- colorFactor(
    palette = c("green", "orange", "blue"),
    domain = c("Terminé", "En cours", "À venir")
  )
  
  # Création de la carte avec leaflet
  ma_carte <- leaflet(chantiers_carte) %>%
    # addTiles() ajoute le fond de carte OpenStreetMap
    addTiles() %>%
    # setView() définit le centre et le zoom initial
    setView(lng = 2.3522, lat = 48.8566, zoom = 12) %>%
    # addCircleMarkers() ajoute les points
    addCircleMarkers(
      lng = ~longitude,           # Colonne de la longitude
      lat = ~latitude,            # Colonne de la latitude
      radius = ~pmin(sqrt(surface) / 3, 15),  # Taille selon la surface
      color = ~palette_couleurs(statut),       # Couleur selon le statut
      fillOpacity = 0.6,          # Transparence
      stroke = TRUE,
      weight = 1,
      # popup = texte qui s'affiche au clic
      popup = ~paste0(
        "<strong>Référence:</strong> ", reference, "<br>",
        "<strong>Arrondissement:</strong> ", arrondissement, "ème<br>",
        "<strong>Nature:</strong> ", nature, "<br>",
        "<strong>Surface:</strong> ", round(surface, 2), " m²<br>",
        "<strong>Durée:</strong> ", duree_jours, " jours<br>",
        "<strong>Statut:</strong> ", statut, "<br>",
        "<strong>Début:</strong> ", date_debut, "<br>",
        "<strong>Fin:</strong> ", date_fin
      )
    ) %>%
    # addLegend() ajoute une légende
    addLegend(
      position = "bottomright",
      pal = palette_couleurs,
      values = ~statut,
      title = "Statut du chantier"
    )
  
  # Sauvegarde de la carte en fichier HTML
  saveWidget(ma_carte, "carte_chantiers_paris.html", selfcontained = TRUE)
  print("Carte interactive sauvegardée : carte_chantiers_paris.html")
  
} else {
  print("ATTENTION : Pas de coordonnées disponibles pour créer la carte")
}

#PARTIE 7 : ANALYSES AVANCÉES
 

 
print("ANALYSES AVANCÉES")
 

  # 7.1 - Tableau croisé : Arrondissement x Nature

print("7.1 - TABLEAU CROISÉ ARRONDISSEMENT x NATURE")

# Un tableau croisé montre la relation entre deux variables catégorielles
# pivot_wider transforme un tableau long en tableau large

tableau_croise <- chantiers %>%
  filter(!is.na(arrondissement) & !is.na(nature)) %>%
  count(arrondissement, nature) %>%
  pivot_wider(
    names_from = nature,    # Les natures deviennent des colonnes
    values_from = n,        # Les valeurs sont les comptages
    values_fill = 0         # On met 0 là où il n'y a pas de données
  )

print(head(tableau_croise, 10))

  # 7.2 - Test de corrélation : Surface vs Durée

print("7.2 - CORRÉLATION ENTRE SURFACE ET DURÉE")

# La corrélation mesure la relation entre deux variables numériques
# - Corrélation proche de 1 : relation positive forte
# - Corrélation proche de -1 : relation négative forte
# - Corrélation proche de 0 : pas de relation

donnees_corr <- chantiers %>%
  filter(surface > 0 & duree_jours > 0)

if(nrow(donnees_corr) > 10) {
  # cor.test() calcule le coefficient de corrélation et teste sa significativité
  test_correlation <- cor.test(
    donnees_corr$surface, 
    donnees_corr$duree_jours, 
    use = "complete.obs"
  )
  
  print(paste("Coefficient de corrélation:", round(test_correlation$estimate, 4)))
  print(paste("P-value:", format(test_correlation$p.value, scientific = TRUE)))
  
  # Interprétation
  if(test_correlation$p.value < 0.05) {
    print("=> La corrélation est statistiquement significative (p < 0.05)")
  } else {
    print("=> La corrélation n'est pas statistiquement significative (p >= 0.05)")
  }
  
  # Graphique de corrélation (nuage de points)
  graphique9 <- donnees_corr %>%
    filter(surface < quantile(surface, 0.95) & duree_jours < quantile(duree_jours, 0.95)) %>%
    ggplot(aes(x = surface, y = duree_jours)) +
    geom_point(alpha = 0.3, color = "steelblue") +
    # geom_smooth avec method="lm" ajoute une droite de régression
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    labs(
      title = "Relation entre surface et durée des chantiers",
      subtitle = paste("Corrélation:", round(test_correlation$estimate, 3)),
      x = "Surface (m²)",
      y = "Durée (jours)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
  
  print(graphique9)
  ggsave("graphique_correlation_surface_duree.png", graphique9, width = 10, height = 8, dpi = 300)
  print("Graphique sauvegardé : graphique_correlation_surface_duree.png")
}

  # 7.3 - Top 10 des plus gros chantiers
print("7.3 - TOP 10 DES PLUS GROS CHANTIERS (par surface)")

top10_surface <- chantiers %>%
  arrange(desc(surface)) %>%
  select(
    num_emprise,
    arrondissement,
    nature,
    surface,
    duree_jours,
    responsable
  ) %>%
  slice_head(n = 10)

print(top10_surface)

  # 7.4 - Top 10 des chantiers les plus longs
print("7.4 - TOP 10 DES CHANTIERS LES PLUS LONGS")

top10_duree <- chantiers %>%
  filter(!is.na(duree_jours)) %>%
  arrange(desc(duree_jours)) %>%
  select(
    num_emprise,
    arrondissement,
    nature,
    surface,
    duree_jours,
    responsable
  ) %>%
  head(10)

print(top10_duree)
#PARTIE 8 : EXPORT DES RÉSULTATS
print("EXPORT DES RÉSULTATS")
 

# On exporte les données nettoyées et les statistiques en fichiers CSV
# CSV = Comma Separated Values = format lisible par Excel

# Export des données nettoyées
write_csv(chantiers, "chantiers_paris_nettoye.csv")
print("Fichier exporté : chantiers_paris_nettoye.csv")

# Export des statistiques par arrondissement
write_csv(stats_arrondissement, "stats_par_arrondissement.csv")
print("Fichier exporté : stats_par_arrondissement.csv")

# Export des statistiques par nature
write_csv(stats_nature, "stats_par_nature.csv")
print("Fichier exporté : stats_par_nature.csv")

# Export de la surface par arrondissement
write_csv(surface_par_arr, "surface_par_arrondissement.csv")
print("Fichier exporté : surface_par_arrondissement.csv")

#PARTIE 9 : RÉSUMÉ FINAL

print("RÉSUMÉ FINAL DE L'ANALYSE")

print("SOURCE DES DONNÉES :")
print("  - API OpenData Paris")
print("  - Dataset : chantiers-a-paris")
print("  - URL : https://opendata.paris.fr/explore/dataset/chantiers-a-paris/")


print("DONNÉES ANALYSÉES :")
print(paste("  - Nombre total de chantiers :", nrow(chantiers)))
print(paste("  - Période :", min(chantiers$date_debut, na.rm = TRUE), 
            "à", max(chantiers$date_fin, na.rm = TRUE)))
print(paste("  - Surface totale :", format(sum(chantiers$surface, na.rm = TRUE), big.mark = " "), "m²"))

print("TOP 3 DES ARRONDISSEMENTS :")
top3_arr <- head(stats_arrondissement, 3)
for(i in 1:3) {
  print(paste("  -", top3_arr$arrondissement[i], "ème arrondissement :", 
              top3_arr$nb_chantiers[i], "chantiers"))
}


print("TOP 3 DES TYPES DE CHANTIERS :")
top3_nat <- head(stats_nature, 3)
for(i in 1:min(3, nrow(top3_nat))) {
  print(paste("  -", top3_nat$nature[i], ":", top3_nat$nb_chantiers[i], 
              "(", top3_nat$pourcentage[i], "%)"))
}

print("")
print(paste("DURÉE MOYENNE :", round(mean(chantiers$duree_jours, na.rm = TRUE), 1), "jours"))
print(paste("SURFACE MOYENNE :", round(mean(chantiers$surface[chantiers$surface > 0], na.rm = TRUE), 2), "m²"))
print("  FIN DE L'ANALYSE")
 
