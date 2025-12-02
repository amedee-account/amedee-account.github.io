library(tidyverse)
library(ggplot2)
library(scales) 

#AFFICHAGE DONNÉES ÉVOLUTION AIDE HUMANITAIRE FRANÇAISE
donnees_aide_raw <- read_delim("/Users/charlotteamedee/data_OCD.csv", delim = ";", show_col_types = FALSE)

# 2. Nettoyage et Transformation
donnees_aide_clean <- donnees_aide_raw %>%
  # Supprimer la ligne vide ou inutile (la ligne 2 qui contient juste "Recipient")
  filter(!is.na(`2002`)) %>% 
  
  # Passer du format LARGE (Années en colonnes) au format LONG (Années en lignes)
  pivot_longer(
    cols = -`Time period`,    # On garde la colonne de noms (Time period), on pivote le reste
    names_to = "Year",        # Les colonnes 2002... deviennent une colonne "Year"
    values_to = "Montant_Brut" # Les valeurs vont dans cette colonne temporaire
  ) %>%
  
  # Nettoyage des chiffres (C'est l'étape cruciale !)
  mutate(
    # 1. On enlève les espaces (y compris les espaces insécables bizarres \u202f)
    Montant_Clean = str_remove_all(Montant_Brut, "\\s|\\u202f"),
    # 2. On remplace la virgule par un point
    Montant_Clean = str_replace(Montant_Clean, ",", "."),
    # 3. On convertit en vrai nombre
    Amount = as.numeric(Montant_Clean),
    # 4. On convertit l'année en nombre
    Year = as.numeric(Year)
  ) %>%
  
  # On renomme la première colonne pour que ce soit plus propre
  rename(Recipient = `Time period`) %>%
  
  # On ne garde que les colonnes propres
  select(Recipient, Year, Amount)

# Vérification du résultat
print(head(donnees_aide_clean))
glimpse(donnees_aide_clean)

#ADDITION DES PED ET PMA
aide_fr_total <- donnees_aide_clean %>%
  # 1. On ne garde que les deux catégories que vous voulez additionner
  filter(Recipient %in% c("Developing countries", "Least developed countries")) %>%
  
  # 2. On groupe par année pour "écraser" les différences entre pays
  group_by(Year) %>%
  
  # 3. L'addition magique
  summarise(
    Montant_Total = sum(Amount, na.rm = TRUE)
  )

# Pour voir le résultat
print(aide_fr_total)


# Création du référentiel politique français
orientation_po_FR <- tibble(Year = 2002:2022) %>% 
  mutate(
    # --- VARIABLE 1 : IDEOLOGIE DU GOUVERNEMENT (Celui qui a le budget) ---
    GOUV_IDEOLOGY = case_when(
      # 2002-2012 : Hégémonie de la Droite (Raffarin, Villepin, Fillon)
      Year >= 2002 & Year <= 2011 ~ 0, # Droite
      
      # 2012-2017 : Retour de la Gauche (Ayrault, Valls, Cazeneuve)
      Year >= 2012 & Year <= 2016 ~ 1, # Gauche
      
      # 2017-Présent : Centre / LREM (Philippe, Castex, Borne)
      Year >= 2017 ~ 2, # Centre
      
      TRUE ~ NA_real_ # Sécurité
    ),
    
    
    # Etiquette pour graphiques 
    Label_Politique = case_when(
      GOUV_IDEOLOGY == 0 ~ "Droite",
      GOUV_IDEOLOGY == 1 ~ "Gauche",
      GOUV_IDEOLOGY == 2 ~ "Centre"
    )
  )

# Vérification visuelle immédiate
print(orientation_po_FR, n = 25)

# Fusion des données économiques et politiques
data_aide_orientation_po <- aide_fr_total %>%
  left_join(orientation_po_FR, by = "Year")

# Aperçu du résultat final
head(data_aide_orientation_po)

ggplot(data_aide_orientation_po, aes(x = Year, y = Montant_Total)) +
  # On colorie les barres selon l'idéologie (Rouge=Gauche, Bleu=Droite, Jaune/Orange=Centre)
  geom_col(aes(fill = Label_Politique), width = 0.8, color = "white", alpha = 0.9) +
  
  # Définition manuelle des couleurs politiques françaises
  scale_fill_manual(values = c("Gauche" = "#eb495d",   # Rouge
                               "Droite" = "#2596be",   # Bleu
                               "Centre" = "#eba64a")) + # Jaune/Orange
  
  # --- 3. LES AXES (Le secret d'un graphe pro) ---
  # Axe Y : On formate les grands nombres avec un espace (big.mark)
  scale_y_continuous(
    labels = scales::number_format(scale = 1, suffix = " Md$", big.mark = " "), # Affiche en Milliards si besoin
    # OU pour rester en millions : labels = scales::number_format(big.mark = " ", suffix = " M$")
    expand = expansion(mult = c(0, 0.1)) # Enlève l'espace vide sous la barre 0
  ) +
  
  # Axe X : On force l'affichage de toutes les 2 années (sinon R en saute parfois)
  scale_x_continuous(breaks = seq(2002, 2022, by = 2)) +
  labs(title = "Évolution de l'Aide Humanitaire Française (2002-2022)",
       subtitle = "Montants cumulés alloués aux Pays en Développement et aux PMA",
       x = "Année",
       y = "Montant (Millions $)",
       fill = "Orientation du gouvernement",
       caption = "Source : Données OCDE (CRS) & Calculs propres.\nNote : Les montants sont exprimés en dollars courants."
       ) +
  theme_minimal(base_size = 14)+

  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 12, color = "gray40", margin = margin(b = 15)),
    
    legend.position = "top",
    legend.justification = "left", # Calée à gauche
    legend.margin = margin(l = -10), # Alignée avec le bord
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    
    plot.caption = element_text(size = 9, color = "gray50", face = "italic", margin = margin(t = 15))
  )

###
#AFFICHAGE DONNÉES PAR SECTEUR DE L'ALLOCATION DE L'AIDE HUMANITAIRE
donnees_secteur_raw <- read_delim("/Users/charlotteamedee/data_aide_FR_secteur.csv", 
                     delim = ";", 
                     col_names = FALSE,       # On ne prend pas la 1ère ligne comme titre tout de suite
                     show_col_types = FALSE,
                     col_types = cols(.default = "c")) # Tout en texte par sécurité

# 2. RÉCUPÉRATION DES ANNÉES (Ligne d'en-tête)
# Les années sont dans la 1ère ligne, à partir de la 3ème colonne
annees <- as.character(donnees_secteur_raw[1, 3:ncol(donnees_secteur_raw)])

# 3. TRANSFORMATION ET NETTOYAGE
donnees_secteur_clean <- donnees_secteur_raw %>%
  # A. CRÉATION DE LA COLONNE SECTEUR
  # Si la ligne commence par "Sector:", on prend le texte. Sinon, on met NA.
  mutate(
    Sector_Raw = if_else(str_detect(X1, "^Sector:"), X1, NA_character_)
  ) %>%
  
  # B. PROPAGATION (FILL DOWN)
  # On remplit les NA vers le bas avec la dernière valeur "Sector" trouvée
  fill(Sector_Raw, .direction = "down") %>%
  
  # C. FILTRAGE DES LIGNES DE DONNÉES
  # On ne garde que les lignes qui nous intéressent (Pays en dév. & PMA)
  filter(X1 %in% c("Developing countries", "Least developed countries")) %>%
  
  # D. NETTOYAGE FINAL DES COLONNES
  mutate(
    Sector = str_remove(Sector_Raw, "Sector:"), # On enlève le préfixe "Sector:"
    Recipient = X1                              # On renomme proprement
  ) %>%
  
  # E. SÉLECTION DES COLONNES UTILES
  # On garde Secteur, Bénéficiaire et les colonnes de données (X3 à la fin)
  select(Sector, Recipient, all_of(3:ncol(donnees_secteur_raw)))

# 4. ATTRIBUTION DES NOMS DE COLONNES (Années)
colnames(donnees_secteur_clean)[3:ncol(donnees_secteur_clean)] <- annees

# 5. PIVOT ET FORMATAGE FINAL (Wide -> Long)
donnees_secteur_final <- donnees_secteur_clean %>%
  pivot_longer(
    cols = -c(Sector, Recipient), # On garde Secteur et Pays fixes
    names_to = "Year", 
    values_to = "Montant_Brut"
  ) %>%
  mutate(
    # Nettoyage des chiffres :
    # 1. On enlève tous les espaces (classiques ou insécables \u00A0 \u202F)
    Montant_Net = str_remove_all(Montant_Brut, "\\s|\\u00A0|\\u202F"),
    # 2. On remplace la virgule par un point (si nécessaire)
    Montant_Net = str_replace(Montant_Net, ",", "."),
    # 3. Conversion en numérique
    Amount = as.numeric(Montant_Net),
    Year = as.numeric(Year)
  ) %>%
  select(Sector, Recipient, Year, Amount)


# --- RÉSULTAT ---
# Affichage des premières lignes pour vérifier
glimpse(donnees_secteur_final)

#Fusion des colonnes PMA et PED 
donnees_secteur_VF <- donnees_secteur_final %>%
  filter(Recipient %in% c("Developing countries", "Least developed countries")) %>%
  mutate(
    Sector = case_when(
      # Si le texte contient "Economic...", on le remplace par "Infrastructures..."
      str_detect(Sector, "Economic infrastructure and services") ~ "Infrastructures Économiques",
      
      # Si le texte contient "Humanitarian...", on le remplace
      str_detect(Sector, "Humanitarian aid") ~ "Aide Humanitaire",
      
      # Si le texte contient "Production...", on le remplace
      str_detect(Sector, "Production sectors") ~ "Secteurs Productifs",
      
      # IMPORTANT : Pour tout le reste, on garde le nom d'origine
      TRUE ~ Sector 
    )) %>%
  group_by(Sector, Year) %>%
  summarise(
    Montant_Total = sum(Amount, na.rm = TRUE)
  )
# Pour voir le résultat
print(donnees_secteur_VF)


#Ajout au tableau initial
dataset_complet_aide_fr <- data_aide_orientation_po %>%
  # On colle les infos politiques en se basant sur la colonne commune "Year"
  left_join(donnees_secteur_VF, by = "Year")
print(dataset_complet_aide_fr)


#
library(ggplot2)
library(scales)

# Astuce Design : On filtre pour ne garder que les 3-4 plus gros secteurs
# Sinon le graphique sera illisible avec 20 lignes spaghetti
# Regardez vos secteurs avec : unique(donnees_secteur_VF$Sector)
donnees_secteur_VF2 <- donnees_secteur_VF %>%
  filter(str_detect(Sector, "Economic infrastructure and services|Humanitarian aid|Production sectors")) 

ggplot() +
  
  # --- COUCHE 1 : LES BARRES (LE TOTAL) ---
  # On utilise le dataset GLOBAL
  geom_col(data = data_aide_orientation_po, 
           aes(x = Year, y = Montant_Total, fill = Label_Politique), 
           width = 0.8, alpha = 0.6) + 
  
  # --- COUCHE 2 : LES LIGNES (LES SECTEURS) ---
  # On utilise le dataset SECTORIEL (filtré ou complet)
  geom_line(data = donnees_secteur_VF2, 
            aes(x = Year, y = Montant_Total, color = Sector), 
            linewidth = 1.2) + # linewidth remplace size dans les nouvelles versions de ggplot
  
  # On ajoute des points pour marquer les années
  geom_point(data = donnees_secteur_VF2, 
             aes(x = Year, y = Montant_Total, color = Sector), 
             size = 2) +
  
  # --- DESIGN & COULEURS ---
  # 1. Couleurs Politiques (Barres)
  scale_fill_manual(values = c("Gauche" = "#eb495d", "Droite" = "#2596be", "Centre" = "#eba64a"),
                    name = "Orientation du gouvernement") +
  
  # 2. Couleurs Sectorielles (Lignes)
  # R va générer des couleurs auto, ou vous pouvez utiliser une palette distincte
  scale_color_brewer(palette = "Set1", name = "Secteur") +
  
  # --- AXES & TITRES ---
  scale_y_log10(labels = scales::number_format(scale = 1, suffix = " M$", big.mark = " ")) +
  scale_x_continuous(breaks = seq(2002, 2022, 2)) +
  
  labs(
    title = "L'aide humanitaire de la France - 2002-2022",
    subtitle = "Montant et priorités économiques dans l'allocation de l'aide française",
    y = "Montant (Millions $)",
    x = "Année",
    caption = "Source : Données OCDE (CRS)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom", # Légende en bas
    legend.box = "vertical",    # Empiler les légendes (Politique au dessus de Secteur)
    legend.margin = margin(t = -5)
  )
