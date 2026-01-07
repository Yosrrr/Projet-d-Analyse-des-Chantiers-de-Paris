# 🏗️ Analyse des Chantiers de Paris

> Projet d'analyse de données des chantiers de la Ville de Paris utilisant R et les données OpenData Paris.

---

## 📋 Description du Projet

Ce projet analyse les données des chantiers en cours, terminés et à venir sur le territoire parisien. Il permet de :

- **Télécharger** automatiquement les données depuis l'API OpenData Paris
- **Nettoyer** et préparer les données pour l'analyse
- **Analyser** statistiquement la répartition des chantiers
- **Visualiser** les résultats via des graphiques et une carte interactive
- **Générer** un rapport web interactif avec Quarto

Analyser les données des chantiers de la Ville de Paris pour comprendre leur répartition géographique, leur nature et leur évolution temporelle.
---

## 📁 Structure du Projet

```
Projet_Chantiers_Paris/
│
├── analyse_chantiers.R          # Script principal d'analyse
├── rapport_chantiers.qmd        # Rapport Quarto interactif
├── index.qmd                    # Page d'accueil du site
├── _quarto.yml                  # Configuration Quarto
│
├── 📊 Données générées
│   ├── chantiers_paris_nettoye.csv
│   ├── stats_par_arrondissement.csv
│   ├── stats_par_nature.csv
│   └── surface_par_arrondissement.csv
│
├── 📈 Graphiques générés
│   ├── graphique_arrondissements.png
│   ├── graphique_nature_chantiers.png
│   ├── graphique_distribution_surface.png
│   ├── graphique_distribution_duree.png
│   ├── graphique_evolution_temporelle.png
│   └── ...
│
├── 🗺️ Carte interactive
│   └── carte_chantiers_paris.html
│
└── _site/                       # Site web généré
```

---

## 🛠️ Technologies Utilisées

| Technologie | Utilisation |
|-------------|-------------|
| **R** | Langage de programmation principal |
| **tidyverse** | Manipulation et visualisation de données |
| **ggplot2** | Création de graphiques |
| **leaflet** | Carte interactive |
| **Quarto** | Génération du rapport web |
| **httr / jsonlite** | Connexion à l'API |

---

## 📊 Analyses Réalisées

### 1️⃣ Statistiques Descriptives
- Répartition des chantiers par **arrondissement**
- Répartition par **nature** de chantier (voirie, réseaux, bâtiment...)
- Répartition par **statut** (en cours, terminé, à venir)
- Statistiques sur la **surface** et la **durée**

### 2️⃣ Visualisations
| Type | Description |
|------|-------------|
| Barres | Nombre de chantiers par arrondissement |
| Histogramme | Distribution de la surface et durée |
| Évolution | Tendance temporelle des chantiers |
| Boxplot | Durée par nature de chantier |
| Camembert | Répartition par statut |

### 3️⃣ Carte Interactive
Visualisation géographique des chantiers avec :
- Couleur selon le statut
- Taille selon la surface
- Popup avec détails au clic

### 4️⃣ Analyses Avancées
- Tableau croisé arrondissement × nature
- Corrélation surface / durée
- Top 10 des plus gros chantiers

---

## 🚀 Installation et Exécution

### Prérequis
- R (version 4.0+)
- RStudio (recommandé)

### Installation des packages
```r
install.packages(c(
  "tidyverse", "lubridate", "janitor", "scales",
  "leaflet", "plotly", "knitr", "kableExtra",
  "htmlwidgets", "httr", "jsonlite"
))
```

### Exécution
```r
# Dans RStudio, ouvrir et exécuter :
source("analyse_chantiers.R")
```

### Génération du rapport Quarto
```bash
quarto render rapport_chantiers.qmd
```
# Publier sur GitHub Pages
```bash
quarto publish gh-pages
---
https://yosrrr.github.io/Projet-d-Analyse-des-Chantiers-de-Paris/
```
## 📈 Résultats Principaux

| Indicateur | Valeur |
|------------|--------|
| Nombre total de chantiers | Variable (API live) |
| Arrondissement le plus actif | Consulter `stats_par_arrondissement.csv` |
| Durée moyenne | ~X jours |
| Surface totale impactée | ~X m² |

---

## 📚 Source des Données

- **API** : [OpenData Paris](https://opendata.paris.fr/)
- **Dataset** : [Chantiers à Paris](https://opendata.paris.fr/explore/dataset/chantiers-a-paris/)
- **Format** : JSON via API REST
- **Mise à jour** : Données en temps réel

---

## 🔧 Fonctions Clés du Code

| Fonction | Package | Utilité |
|----------|---------|---------|
| `clean_names()` | janitor | Nettoie les noms de colonnes |
| `mutate()` | dplyr | Crée/modifie des colonnes |
| `filter()` | dplyr | Filtre les lignes |
| `count()` | dplyr | Compte par groupe |
| `summarise()` | dplyr | Calcule des statistiques |
| `ggplot()` | ggplot2 | Crée des graphiques |
| `leaflet()` | leaflet | Crée des cartes |

---

## 👤 Auteur

Projet réalisé dans le cadre d'un cours de R - 5ème année

---

## 📄 Licence

Données sous licence [ODbL](https://opendatacommons.org/licenses/odbl/) (OpenData Paris)
