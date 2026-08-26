####################################
############ Article for SPER

rm(list=ls())
options(scipen=999)

library (tidyverse)
library (readxl)
library (purrr)
library (stargazer)
library (fixest)
library (texreg)
library (tinytex)

setwd("")

# Data of Índice de Rezago Social (CONEVAL)
irs_raw_2010 = read.csv("irs_municipal_2010.csv")
irs_raw_2015 = read.csv("irs_municipal_2015.csv")
irs_raw_2020 = read.csv("irs_municipal_2020.csv")

irs_full = bind_rows(irs_raw_2010, irs_raw_2015, irs_raw_2020)

# Population data (CONAPO)
pob_all_y_raw = read_excel("1_Grupo_Quinq_00_RM.xlsx") # https://www.gob.mx/conapo/documentos/reconstruccion-y-proyecciones-de-la-poblacion-de-los-municipios-de-mexico-1990-2040

# Mortality data (INEGI) 
# https://www.inegi.org.mx/programas/edr/#datos_abiertos

def_raw_2024 = read.csv("conjunto_de_datos_defunciones_registradas24_csv.csv")
def_raw_2023 = read.csv("conjunto_de_datos_defunciones_registradas_2023_csv.csv")
def_raw_2022 = read.csv("conjunto_de_datos_defunciones_registradas_2022.csv")
def_raw_2021 = read.csv("conjunto_de_datos_defunciones_registradas_2021.csv")
def_raw_2020 = read.csv("conjunto_de_datos_defunciones_registrados_2020.csv")
def_raw_2019 = read.csv("conjunto_de_datos_defunciones_registradas_2019.csv")
def_raw_2018 = read.csv("conjunto_de_datos_defunciones_registradas_2018.csv")
def_raw_2017 = read.csv("conjunto_de_datos_defunciones_generales_2017.csv")
def_raw_2016 = read.csv("defunciones_generales_2016.csv")
def_raw_2015 = read.csv("defunciones_generales_2015.csv")
def_raw_2014 = read.csv("defunciones_generales_2014.csv")
def_raw_2013 = read.csv("defunciones_generales_2013.csv")
def_raw_2012 = read.csv("defunciones_generales_2012.csv")

raw_list = mget(ls(pattern = "^def_raw_\\d{4}$"))

combined = imap_dfr(raw_list, ~ .x %>%
                      select(ent_regis, mun_regis,
                             ent_resid, mun_resid, 
                             ent_ocurr, mun_ocurr,
                             causa_def, anio_ocur,
                             edad, sexo) %>% 
                      mutate(reg_year = as.numeric(sub("def_raw_", "", .y))))

combined = combined |> 
  mutate(grupo_causa = case_when(
    str_starts(causa_def, "J") ~                              "Diseases of the respiratory system (J00–J99)",
    str_starts(causa_def, "I") ~                              "Diseases of the circulatory system (I00–I99)",
    str_starts(causa_def, "F") ~                              "Mental and behavioural disorders (F00–F99)",
    str_starts(causa_def, "C") | str_starts(causa_def, "D") ~ "Neoplasms (C00–D48)",
    str_starts(causa_def, "R") ~                              "Ill defined illnesses (R00–R99)",
    TRUE ~ NA_character_))

def_final = combined |> 
  filter(!is.na(grupo_causa), 
         mun_resid != 999) |>  
  filter(between(anio_ocur, 2012, 2024)) |>
  rename(AÑO = anio_ocur) |> 
  mutate(CLAVE = ent_resid * 1000 + mun_resid) |> 
  count(CLAVE, AÑO, grupo_causa, name = "def_totales") |> 
  complete(CLAVE, AÑO, grupo_causa, fill = list(def_totales = 0))

# Population data
pob_all_y = pob_all_y_raw |> 
  select(CLAVE, NOM_ENT, NOM_MUN,AÑO, POB_TOTAL) |> 
  filter(between(AÑO, 2012, 2024)) |>
  group_by(CLAVE, NOM_ENT, NOM_MUN, AÑO) |> 
  summarise(POB_TOTAL = sum(POB_TOTAL)) |>
  data.frame() |> 
  mutate(city_qs = ifelse(POB_TOTAL >= 100000,1,0))

## Now join!
base_final = pob_all_y |>
  cross_join(tibble(grupo_causa = unique(na.omit(combined$grupo_causa)))) |> 
  left_join(def_final, by = c("CLAVE", "AÑO", "grupo_causa")) |>
  mutate(def_totales = replace_na(def_totales, 0)) |>
  mutate(tasa_1k = (def_totales / POB_TOTAL) * 1000) |> 
  mutate(refineria = ifelse(CLAVE %in% c(13076, 19009, 20079, 28009, 11027, 30108), 1, 0)) |> 
  mutate(irs_wave = case_when(AÑO <= 2012 ~ 2010,
                              AÑO <= 2017 ~ 2015,
                              TRUE ~ 2020)) |> 
  inner_join(irs_full, by = c("CLAVE", "irs_wave"))

## First model
modelos = feols(tasa_1k ~ refineria + city_qs + irs_value | AÑO,
                data = base_final,
                cluster = ~CLAVE,
                split = ~grupo_causa)

etable(modelos)

# Table 1
tabla_tex = etable(modelos[c(1,2,3,5)],
                   dict = c("AÑO" = "Year",
                            refineria = "Oil refinery",
                            CLAVE = "Municipality",
                            city_qs = "City?",
                            irs_value = "Social lag index",
                            tasa_1k = "Mortality rate per 1,000 inhabitants"),
                   headers = c("Circulatory", "Respiratory","Ill defined" ,"Neoplasms [Tumors]"),
                   tex = TRUE,
                   title = "Association between the presence of an operating oil refinery and the mortality rate per one thousand inhabitants at the municipal level",
                   label = "tab:oil refinery")

doc <- c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[utf8]{inputenc}",      
  "\\usepackage[T1]{fontenc}",         
  "\\usepackage[margin=1in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{makecell}",           
  "\\pagestyle{empty}",
  "\\begin{document}",
  tabla_tex,
  "\\end{document}"
)

con <- file("tabla_todos.tex", open = "w", encoding = "UTF-8")
writeLines(doc, con, useBytes = TRUE)
close(con)

tinytex::pdflatex("tabla_todos.tex")

#################################################################################
## Graphs
refinerias = base_final |> 
  filter(refineria == 1)

nacional = base_final |> 
  filter(refineria == 0) |> 
  group_by(AÑO, grupo_causa) |> 
  summarise(tasa_1k = sum(def_totales) / sum(POB_TOTAL) * 1000, 
            .groups = "drop")

ggplot() +
  geom_line(data = refinerias |> filter(grupo_causa != "Ill defined illnesses (R00–R99)"),
            aes(x = AÑO, y = tasa_1k, group = NOM_MUN, color = NOM_MUN),
            linewidth = 0.7) +
  geom_line(data = nacional |> filter(grupo_causa != "Ill defined illnesses (R00–R99)"),
            aes(x = AÑO, y = tasa_1k),
            color = "black", linewidth = 1.2, linetype = "dashed") +
  facet_wrap(~ grupo_causa, scales = "free_y") +
  scale_x_continuous(breaks = 2012:2024)+
  labs(x = "Year", 
       y = "Resident deaths per 1,000 inhabitants",
       color = "Municipalities with refineries",
       caption = "
       Dashed black line: national average of municipalities without a refinery,
       Data from INEGI (mortality) and CONAPO (yearly population estimates), graph created by Bernardo Luis and Emilio del Río") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  theme(plot.caption = element_text(hjust = 0),              
        plot.caption.position = "plot",                      
        axis.title.y = element_text(margin = margin(r = 10)))
ggsave("grafica_desc_no_agreg.png", width = 12, height = 7, dpi = 300)


### Aggregated
refinerias_grouped = refinerias |> 
  group_by(AÑO, grupo_causa) |> 
  summarise(ref_1k_tasa = sum(def_totales) / sum(POB_TOTAL) * 1000) |> 
  data.frame()

nacional |> 
  inner_join(refinerias_grouped, by = c("AÑO", "grupo_causa")) |> 
  group_by(grupo_causa) |> 
  summarise(mean_nac = mean(tasa_1k),
            mean_ref = mean(ref_1k_tasa)) |> 
  data.frame() |> 
  mutate(per_diff = ((mean_ref/mean_nac)-1))

ggplot() +
  geom_line(data = refinerias_grouped |> filter(grupo_causa != "Ill defined illnesses (R00–R99)"),
            aes(x = AÑO, y = ref_1k_tasa),
            linewidth = 0.7, color = "red") +
  geom_line(data = nacional |> filter(grupo_causa != "Ill defined illnesses (R00–R99)"),
            aes(x = AÑO, y = tasa_1k),
            color = "black", linewidth = 1.2, linetype = "dashed") +
  facet_wrap(~ grupo_causa, scales = "free_y") +
  scale_x_continuous(breaks = 2012:2024)+
  labs(x = "Year", 
       y = "Resident deaths per 1,000 inhabitants",
       caption = "
       Dashed black line: national average of municipalities without a refinery, Red line: average of municipalities with a refinery,
       Data from INEGI (mortality) and CONAPO (yearly population estimates), graph created by Bernardo Luis and Emilio del Río") +
  theme_minimal()+
  theme(plot.caption = element_text(hjust = 0),              
        plot.caption.position = "plot",                      
        axis.title.y = element_text(margin = margin(r = 10)))
ggsave("grafica_desc_agreg.png", width = 12, height = 7, dpi = 300)


### Just for ill defined causes
ggplot() +
  geom_line(data = refinerias_grouped |> filter(grupo_causa == "Ill defined illnesses (R00–R99)"),
            aes(x = AÑO, y = ref_1k_tasa),
            linewidth = 0.7, color = "red") +
  geom_line(data = nacional |> filter(grupo_causa == "Ill defined illnesses (R00–R99)"),
            aes(x = AÑO, y = tasa_1k),
            color = "black", linewidth = 1.2, linetype = "dashed") +
  facet_wrap(~ grupo_causa, scales = "free_y") +
  scale_x_continuous(breaks = 2012:2024)+
  labs(x = "Year", 
       y = "Resident deaths per 1,000 inhabitants",
       caption = "
       Dashed black line: national average of municipalities without a refinery, Red line: average of municipalities with a refinery,
       Data from INEGI (mortality) and CONAPO (yearly population estimates), graph created by Bernardo Luis and Emilio del Río") +
  theme_minimal()+
  theme(plot.caption = element_text(hjust = 0),              
        plot.caption.position = "plot",                      
        axis.title.y = element_text(margin = margin(r = 10)))
ggsave("grafica_ill_defined.png", width = 12, height = 7, dpi = 300)


####################### For 60+ ##############################
def_final = combined |> 
  filter(!is.na(grupo_causa), 
         mun_resid != 999) |>  
  filter(between(anio_ocur, 2012, 2024)) |>
  rename(AÑO = anio_ocur) |> 
  mutate(CLAVE = ent_resid * 1000 + mun_resid) |> 
  filter(between(edad, 4060, 4120)) |>
  count(CLAVE, AÑO, grupo_causa, name = "def_totales") |> 
  complete(CLAVE, AÑO, grupo_causa, fill = list(def_totales = 0))

# Population data
pob_all_y = pob_all_y_raw |> 
  mutate(POB_60_plus = rowSums(across(POB_60_64:POB_85_mm), na.rm = TRUE)) |>
  select(CLAVE, NOM_ENT, NOM_MUN,AÑO, POB_60_plus, POB_TOTAL) |>
  filter(between(AÑO, 2012, 2024)) |>
  group_by(CLAVE, NOM_ENT, NOM_MUN, AÑO) |> 
  summarise(POB_60mas = sum(POB_60_plus),
            POB_TOTAL = sum(POB_TOTAL)) |>
  data.frame() |> 
  mutate(city_qs = ifelse(POB_TOTAL >= 100000,1,0))

## Now join!
base_final = pob_all_y |>
  cross_join(tibble(grupo_causa = unique(na.omit(combined$grupo_causa)))) |> 
  left_join(def_final, by = c("CLAVE", "AÑO", "grupo_causa")) |>
  mutate(def_totales = replace_na(def_totales, 0)) |>
  mutate(tasa_1k = (def_totales / POB_60mas) * 1000) |> 
  mutate(refineria = ifelse(CLAVE %in% c(13076, 19009, 20079, 28009, 11027, 30108), 1, 0)) |> 
  mutate(irs_wave = case_when(AÑO <= 2012 ~ 2010,
                              AÑO <= 2017 ~ 2015,
                              TRUE ~ 2020)) |> 
  inner_join(irs_full, by = c("CLAVE", "irs_wave"))

## Second model
modelos_60 = feols(tasa_1k ~ refineria + city_qs + irs_value | AÑO,
                   data = base_final,
                   cluster = ~CLAVE,
                   split = ~grupo_causa)

etable(modelos_60)

# Table body
tabla_tex = etable(modelos_60[c(1,2,3,5)],
                   dict = c("AÑO" = "Year",
                            refineria = "Oil refinery",
                            CLAVE = "Municipality",
                            city_qs = "City?",
                            irs_value = "Social lag index",
                            tasa_1k = "Mortality rate per 1,000 inhabitants aged 60+"),
                   headers = c("Circulatory", "Respiratory", "Ill defined" ,"Neoplasms [Tumors]"),
                   tex = TRUE,
                   title = "Association between the presence of an operating oil refinery and the mortality rate per one thousand inhabitants aged 60 plus at the municipal level",
                   label = "tab:oil refinery")

doc <- c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[utf8]{inputenc}",      
  "\\usepackage[T1]{fontenc}",         
  "\\usepackage[margin=1in]{geometry}",
  "\\usepackage{booktabs}",
  "\\renewcommand{\\thetable}{2}",
  "\\usepackage{makecell}",            
  "\\pagestyle{empty}",
  "\\begin{document}",
  tabla_tex,
  "\\end{document}"
)

con <- file("tabla.tex", open = "w", encoding = "UTF-8")
writeLines(doc, con, useBytes = TRUE)
close(con)

tinytex::pdflatex("tabla.tex")

#########################################################################

refinerias_60 = base_final |> 
  filter(refineria == 1)

refinerias_grouped_60 = refinerias_60 |> 
  group_by(AÑO, grupo_causa) |> 
  summarise(ref_1k_tasa = sum(def_totales) / sum(POB_60mas) * 1000) |> 
  data.frame()

nacional_60 = base_final |> 
  filter(refineria == 0) |> 
  group_by(AÑO, grupo_causa) |> 
  summarise(tasa_1k = sum(def_totales) / sum(POB_60mas) * 1000, 
            .groups = "drop")

nacional_60 |> 
  inner_join(refinerias_grouped_60, by = c("AÑO", "grupo_causa")) |> 
  group_by(grupo_causa) |> 
  summarise(mean_nac = mean(tasa_1k),
            mean_ref = mean(ref_1k_tasa)) |> 
  data.frame() |> 
  mutate(per_diff = ((mean_ref/mean_nac)-1))


