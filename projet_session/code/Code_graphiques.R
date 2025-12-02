library(tidyverse)
library(ggplot2)
library(scales) 

#AFFICHAGE DONNÉES ÉVOLUTION AIDE HUMANITAIRE FRANÇAISE
donnees_aide_raw <- read_delim("/Users/charlotteamedee/amedee-account.github.io/projet_session/data/data_OCDE.csv", delim = ";", show_col_types = FALSE)

# 1. Nettoyage et Transformation
donnees_aide_clean <- donnees_aide_raw %>%
  filter(!is.na(`2002`)) %>% 
  pivot_longer(
    cols = -`Time period`,    
    names_to = "Year",        
    values_to = "Montant_Brut" 
  ) %>%
  
  # Nettoyage des chiffres
  mutate(
    Montant_Clean = str_remove_all(Montant_Brut, "\\s|\\u202f"),
    Montant_Clean = str_replace(Montant_Clean, ",", "."),
    Amount = as.numeric(Montant_Clean),
    Year = as.numeric(Year)
  ) %>%
  
  rename(Recipient = `Time period`) %>%
  select(Recipient, Year, Amount)

glimpse(donnees_aide_clean)

#ADDITION DES PED ET PMA
aide_fr_total <- donnees_aide_clean %>%
  filter(Recipient %in% c("Developing countries", "Least developed countries")) %>%
  group_by(Year) %>%
  summarise(
    Montant_Total = sum(Amount, na.rm = TRUE)
  )
print(aide_fr_total)


# Création du référentiel politique français
orientation_po_FR <- tibble(Year = 2002:2022) %>% 
  mutate(
    # --- VARIABLE 1 : IDEOLOGIE DU GOUVERNEMENT (Celui qui a le budget) ---
    GOUV_IDEOLOGY = case_when(
      # 2002-2012 : Hégémonie de la Droite 
      Year >= 2002 & Year <= 2011 ~ 0, 
      
      # 2012-2017 : Retour de la Gauche 
      Year >= 2012 & Year <= 2016 ~ 1, 
      
      # 2017-Présent : Centre / LREM 
      Year >= 2017 ~ 2, 
      
      TRUE ~ NA_real_ 
    ),
    
        Label_Politique = case_when(
      GOUV_IDEOLOGY == 0 ~ "Droite",
      GOUV_IDEOLOGY == 1 ~ "Gauche",
      GOUV_IDEOLOGY == 2 ~ "Centre"
    )
  )
print(orientation_po_FR, n = 25)

# CREATION DATASET AVEC MONTANT ET ORIENTATION POLITIQUE 
data_aide_orientation_po <- aide_fr_total %>%
  left_join(orientation_po_FR, by = "Year")

#AFFICHAGE PREMIER GRAPHIQUE : H1
ggplot(data_aide_orientation_po, aes(x = Year, y = Montant_Total)) +
  geom_col(aes(fill = Label_Politique), width = 0.8, color = "white", alpha = 0.9) +
  scale_fill_manual(values = c("Gauche" = "#eb495d",   
                               "Droite" = "#2596be",   
                               "Centre" = "#eba64a")) + 
  
  scale_y_continuous(
    labels = scales::number_format(scale = 1, suffix = " Md$", big.mark = " "), 
    expand = expansion(mult = c(0, 0.1))
  ) +
  
  scale_x_continuous(breaks = seq(2002, 2022, by = 2)) +
  labs(title = "Évolution de l'Aide Humanitaire Française (2002-2022)",
       subtitle = "Montants cumulés alloués aux Pays en Développement et aux PMA",
       x = "Année",
       y = "Montant de l'aide totale",
       fill = "Orientation du gouvernement",
       caption = "Source : Données OCDE (CRS) & Calculs propres."
  ) +
  theme_minimal(base_size = 14)+
  
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40", margin = margin(b = 15)),
    
    legend.position = "top",
    legend.justification = "center",
    legend.margin = margin(l = -10), 
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    axis.title.y.left = element_text(face = "bold", color = "gray50"),
    
    plot.caption = element_text(size = 9, color = "gray50", face = "italic", margin = margin(t = 15))
  )


#TEST HYPOTHÈSE 2
#AFFICHAGE DONNÉES PAR SECTEUR DE L'ALLOCATION DE L'AIDE HUMANITAIRE
donnees_secteur_raw <- read_delim("/Users/charlotteamedee/amedee-account.github.io/projet_session/data/data_aide_FR_secteur.csv", 
                                  delim = ";", 
                                  col_names = FALSE,       
                                  show_col_types = FALSE,
                                  col_types = cols(.default = "c")) 

annees <- as.character(donnees_secteur_raw[1, 3:ncol(donnees_secteur_raw)])

# TRANSFORMATION ET NETTOYAGE
donnees_secteur_clean <- donnees_secteur_raw %>%
  mutate(
    Sector_Raw = if_else(str_detect(X1, "^Sector:"), X1, NA_character_)
  ) %>%
  fill(Sector_Raw, .direction = "down") %>%
  filter(X1 %in% c("Developing countries", "Least developed countries")) %>%
  
  mutate(
    Sector = str_remove(Sector_Raw, "Sector:"),
    Recipient = X1                              
  ) %>%
  
  select(Sector, Recipient, all_of(3:ncol(donnees_secteur_raw)))

# ATTRIBUTION DES NOMS DE COLONNES (Années)
colnames(donnees_secteur_clean)[3:ncol(donnees_secteur_clean)] <- annees

# FORMATAGE FINAL (Wide -> Long)
donnees_secteur_final <- donnees_secteur_clean %>%
  pivot_longer(
    cols = -c(Sector, Recipient), # On garde Secteur et Pays fixes
    names_to = "Year", 
    values_to = "Montant_Brut"
  ) %>%
  mutate(
    # Nettoyage des chiffres :
    Montant_Net = str_remove_all(Montant_Brut, "\\s|\\u00A0|\\u202F"),
    Montant_Net = str_replace(Montant_Net, ",", "."),
    Amount = as.numeric(Montant_Net),
    Year = as.numeric(Year)
  ) %>%
  select(Sector, Recipient, Year, Amount)

glimpse(donnees_secteur_final)

#Fusion des colonnes PMA et PED 
donnees_secteur_VF <- donnees_secteur_final %>%
  filter(Recipient %in% c("Developing countries", "Least developed countries")) %>%
  group_by(Sector, Year) %>%
  summarise(
    Montant_Total = sum(Amount, na.rm = TRUE)
  )
print(donnees_secteur_VF)


#BASE DONNÉES 2 COMPLÈTE
dataset_complet_aide_fr <- data_aide_orientation_po %>%
  left_join(donnees_secteur_VF, by = "Year")
print(dataset_complet_aide_fr)


secteurs_a_afficher <- donnees_secteur_VF %>%
  filter(str_detect(Sector, "Economic infrastructure and services|Humanitarian aid|Production sectors"))

#GRAPHIQUE FINAL
# Mise à l'échelle
max_total <- max(data_aide_orientation_po$Montant_Total, na.rm = TRUE)
max_secteur <- max(secteurs_a_afficher$Montant_Total, na.rm = TRUE)
coeff <- max_total / max_secteur

ggplot() +
    geom_col(data = data_aide_orientation_po, 
           aes(x = Year, y = Montant_Total, fill = Label_Politique), 
           width = 0.8, alpha = 0.45) + 
  geom_line(data = secteurs_a_afficher, 
            aes(x = Year, y = Montant_Total * coeff, color = Sector), 
            linewidth = 1.2) +
  
  geom_point(data = secteurs_a_afficher, 
             aes(x = Year, y = Montant_Total * coeff, color = Sector), 
             size = 2) +
  
  scale_y_continuous(
    name = "Montant de l'aide totale",
    labels = scales::number_format(scale = 1, suffix = " M$", big.mark = " "),
  
    sec.axis = sec_axis(~ . / coeff, 
                        name = "Détail par secteurs",
                        labels = scales::number_format(scale = 1, suffix = " M$", big.mark = " "))
  ) +
  
  scale_x_continuous(breaks = seq(2002, 2022, 2)) +
  
  scale_fill_manual(values = c("Gauche" = "#eb495d", "Droite" = "#2596be", "Centre" = "#eba64a"),
                    name = "Orientation du gouvernement") +
 
  #Renommer éléments secteur de la légende 
  scale_color_brewer(palette = "Set1", name = "Secteur d'aide",
           breaks = c("Economic infrastructure and services", 
                                  "Humanitarian aid", 
                                  "Production sectors"),
                       
          labels = c("Infrastructures Économiques", 
                                  "Aide Humanitaire", 
                                  "Secteurs Productifs")
                     ) +
  
  labs(
    title = "L'aide internationale de la France de 2002 à 2022",
    subtitle = "Montant et priorités économiques dans l'allocation de l'aide française",
    x = "Année",
    caption = "Source : Données OCDE (CRS)"
  ) +
  
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = -5),
    plot.caption = element_text(color = "gray50"),
    
    axis.title.y.left = element_text(face = "bold", color = "gray50"),
    axis.title.y.right = element_text(face = "bold", color = "gray50")
  )
