
# Pruebas


library(tidyverse)
library(UnalData)
library(UnalR)
library(readxl)


# Serie General: Admitidos - Mat - Cupos - Graduados

Admitidos <- AdmitidosPre %>% filter(YEAR >= 2021) %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Admitidos") %>% relocate(Clase, .before = Total)
Mat_Pvez <- MatriculadosPVPre %>% filter(YEAR >= 2021) %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Matriculados primera vez") %>% relocate(Clase, .before = Total)
Graduados <-  GraduadosPre %>% filter(YEAR >= 2021) %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Graduados") %>% relocate(Clase, .before = Total)
Cupos <- Cupos_Pre %>% filter(YEAR >= 2021) %>% summarise(Total = sum(CUPOS), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Cupos") %>% relocate(Clase, .before = Total)
Poblaciones <- bind_rows(Cupos, Admitidos, Mat_Pvez, Graduados) %>% mutate(Variable = "POBLACION") %>% relocate(Variable, .before = YEAR)

# Gráfico Poblaciones

Poblaciones %>%
  Plot.Series(categoria = "POBLACION",
              titulo = "Evolución total cupos, admitidos, matriculados primera vez y graduados en pregrado, periodo 2021-2025",
              labelY = "Total",
              colores = color(length(unique(Poblaciones$Clase))),
              # freqRelativa = TRUE,
              ylim = c(0,NaN),
              libreria = c("highcharter"),
              estilo = list(hc.Tema = 5))


# Poblaciones Sedes Por Programas Cód SNIES

Admitidos_Pro <- AdmitidosPre %>% left_join(Historico_Programas, by = "COD_PADRE") %>% filter(YEAR >= 2021) 
Mat_Pvez_Pro <- MatriculadosPVPre %>% left_join(Historico_Programas, by = "COD_PADRE") %>% filter(YEAR >= 2021) 
Graduados_Pro <- GraduadosPre %>% left_join(Historico_Programas, by = "COD_PADRE") %>% filter(YEAR >= 2021) 
Cupos_Pro <- Cupos_Pre %>% filter(YEAR >= 2021)


# Serie Adm - Mat p Vez - Grad - Cupos - SEDE BOGOTÁ

Admitidos_Pro_Bog <- Admitidos_Pro %>% filter(SEDE_PROG == "Bogotá") %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Admitidos") %>% relocate(Clase, .before = Total)
Mat_Pvez_Pro_Bog <- Mat_Pvez_Pro %>% filter(SEDE_PROG == "Bogotá") %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Matriculados primera vez") %>% relocate(Clase, .before = Total)
Graduados_Pro_Bog <- Graduados_Pro %>% filter(SEDE_PROG == "Bogotá") %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Graduados") %>% relocate(Clase, .before = Total)
Cupos_Pro_Bog <- Cupos_Pro %>% filter(SEDE_NOMBRE == "Bogotá") %>% summarise(Total = sum(CUPOS), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Cupos") %>% relocate(Clase, .before = Total)
Poblaciones_Bog <- bind_rows(Cupos_Pro_Bog, Admitidos_Pro_Bog, Mat_Pvez_Pro_Bog, Graduados_Pro_Bog) %>% mutate(Variable = "POBLACION") %>% relocate(Variable, .before = YEAR)



# Serie Adm - Mat p Vez - Grad - Cupos - SEDE MEDELLÍN

Admitidos_Pro_Med <- Admitidos_Pro %>% filter(SEDE_PROG == "Medellín") %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Admitidos") %>% relocate(Clase, .before = Total)
Mat_Pvez_Pro_Med <- Mat_Pvez_Pro %>% filter(SEDE_PROG == "Medellín") %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Matriculados primera vez") %>% relocate(Clase, .before = Total)
Graduados_Pro_Med <- Graduados_Pro %>% filter(SEDE_PROG == "Medellín") %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Graduados") %>% relocate(Clase, .before = Total)
Cupos_Pro_Med <- Cupos_Pro %>% filter(SEDE_NOMBRE == "Medellín") %>% summarise(Total = sum(CUPOS), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Cupos") %>% relocate(Clase, .before = Total)
Poblaciones_Med <- bind_rows(Cupos_Pro_Med, Admitidos_Pro_Med, Mat_Pvez_Pro_Med, Graduados_Pro_Med) %>% mutate(Variable = "POBLACION") %>% relocate(Variable, .before = YEAR)


# Serie Adm - Mat p Vez - Grad - Cupos - SEDE MANIZALES

Admitidos_Pro_Man <- Admitidos_Pro %>% filter(SEDE_PROG == "Manizales") %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Admitidos") %>% relocate(Clase, .before = Total)
Mat_Pvez_Pro_Man <- Mat_Pvez_Pro %>% filter(SEDE_PROG == "Manizales") %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Matriculados primera vez") %>% relocate(Clase, .before = Total)
Graduados_Pro_Man <- Graduados_Pro %>% filter(SEDE_PROG == "Manizales") %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Graduados") %>% relocate(Clase, .before = Total)
Cupos_Pro_Man <- Cupos_Pro %>% filter(SEDE_NOMBRE == "Manizales") %>% summarise(Total = sum(CUPOS), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Cupos") %>% relocate(Clase, .before = Total)
Poblaciones_Man <- bind_rows(Cupos_Pro_Man, Admitidos_Pro_Man, Mat_Pvez_Pro_Man, Graduados_Pro_Man) %>% mutate(Variable = "POBLACION") %>% relocate(Variable, .before = YEAR)

# Serie Adm - Mat p Vez - Grad - Cupos - SEDE PALMIRA

Admitidos_Pro_Pal <- Admitidos_Pro %>% filter(SEDE_PROG == "Palmira") %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Admitidos") %>% relocate(Clase, .before = Total)
Mat_Pvez_Pro_Pal <- Mat_Pvez_Pro %>% filter(SEDE_PROG == "Palmira") %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Matriculados primera vez") %>% relocate(Clase, .before = Total)
Graduados_Pro_Pal <- Graduados_Pro %>% filter(SEDE_PROG == "Palmira") %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Graduados") %>% relocate(Clase, .before = Total)
Cupos_Pro_Pal <- Cupos_Pro %>% filter(SEDE_NOMBRE == "Palmira") %>% summarise(Total = sum(CUPOS), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Cupos") %>% relocate(Clase, .before = Total)
Poblaciones_Pal <- bind_rows(Cupos_Pro_Pal, Admitidos_Pro_Pal, Mat_Pvez_Pro_Pal, Graduados_Pro_Pal) %>% mutate(Variable = "POBLACION") %>% relocate(Variable, .before = YEAR)


# Serie Adm - Mat p Vez - Grad - Cupos - SEDE DE LA PAZ

Admitidos_Pro_Paz <- Admitidos_Pro %>% filter(SEDE_PROG == "De La Paz") %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Admitidos") %>% relocate(Clase, .before = Total)
Mat_Pvez_Pro_Paz <- Mat_Pvez_Pro %>% filter(SEDE_PROG == "De La Paz") %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Matriculados primera vez") %>% relocate(Clase, .before = Total)
Graduados_Pro_Paz <- Graduados_Pro %>% filter(SEDE_PROG == "De La Paz") %>% summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Graduados") %>% relocate(Clase, .before = Total)
Cupos_Pro_Paz <- Cupos_Pro %>% filter(SEDE_NOMBRE == "De La Paz") %>% summarise(Total = sum(CUPOS), .by = c(YEAR, SEMESTRE)) %>% mutate(Clase = "Cupos") %>% relocate(Clase, .before = Total)
Poblaciones_Paz <- bind_rows(Cupos_Pro_Paz, Admitidos_Pro_Paz, Mat_Pvez_Pro_Paz, Graduados_Pro_Paz) %>% mutate(Variable = "POBLACION") %>% relocate(Variable, .before = YEAR)


# Gráfico Poblaciones - Bogotá

Poblaciones_Bog %>%
  Plot.Series(categoria = "POBLACION",
              titulo = "Evolución total cupos, admitidos, matriculados primera vez y graduados en pregrado SEDE BOGOTÁ, periodo 2021-2025",
              labelY = "Total",
              colores = color(length(unique(Poblaciones$Clase))),
              # freqRelativa = TRUE,
              ylim = c(0,NaN),
              libreria = c("highcharter"),
              estilo = list(hc.Tema = 5))

# Gráfico Poblaciones - Medellín

Poblaciones_Med %>%
  Plot.Series(categoria = "POBLACION",
              titulo = "Evolución total cupos, admitidos, matriculados primera vez y graduados en pregrado SEDE MEDELLÍN, periodo 2021-2025",
              labelY = "Total",
              colores = color(length(unique(Poblaciones$Clase))),
              # freqRelativa = TRUE,
              ylim = c(0,NaN),
              libreria = c("highcharter"),
              estilo = list(hc.Tema = 5))

# Gráfico Poblaciones - Manizales

Poblaciones_Med %>%
  Plot.Series(categoria = "POBLACION",
              titulo = "Evolución total cupos, admitidos, matriculados primera vez y graduados en pregrado SEDE MANIZALES, periodo 2021-2025",
              labelY = "Total",
              colores = color(length(unique(Poblaciones$Clase))),
              # freqRelativa = TRUE,
              ylim = c(0,NaN),
              libreria = c("highcharter"),
              estilo = list(hc.Tema = 5))

# Gráfico Poblaciones - Palmira

Poblaciones_Pal %>%
  Plot.Series(categoria = "POBLACION",
              titulo = "Evolución total cupos, admitidos, matriculados primera vez y graduados en pregrado SEDE PALMIRA, periodo 2021-2025",
              labelY = "Total",
              colores = color(length(unique(Poblaciones$Clase))),
              # freqRelativa = TRUE,
              ylim = c(0,NaN),
              libreria = c("highcharter"),
              estilo = list(hc.Tema = 5))

# Gráfico Poblaciones - De La Paz

Poblaciones_Paz %>%
  Plot.Series(categoria = "POBLACION",
              titulo = "Evolución total cupos, admitidos, matriculados primera vez y graduados en pregrado SEDE LA PAZ, periodo 2021-2025",
              labelY = "Total",
              colores = color(length(unique(Poblaciones$Clase))),
              # freqRelativa = TRUE,
              ylim = c(0,NaN),
              libreria = c("highcharter"),
              estilo = list(hc.Tema = 5))


# Análisis Programas


Historico_Programas <- read_excel("Datos/Historico_Programas.xlsx", guess_max = 1000) %>% filter(TIPO_NIVEL_PRO == "Pregrado")
Cupos_Pre <- read_excel("Datos/Cupos_Pregrado.xlsx", guess_max = 5000)


# Acumulado Matriculados primera vez por programas - 2021-2026


Acum_MatriculadosPVPre <-  MatriculadosPVPre %>% 
  filter(YEAR >= 2021) %>% 
  select(YEAR, SEMESTRE, MOD_ADM, TIPO_ADM, SEDE_NOMBRE_ADM, SEDE_NOMBRE_MAT, COD_PADRE) %>% 
  summarise(Matriculados = n(), .by = c(COD_PADRE)) 

# Acumulado Cupos por programas 2021-2026

Acum_Cupos <- Cupos_Pre %>% 
  filter(YEAR >= 2021) %>% 
  summarise(Cupos = sum(CUPOS), .by = c(COD_PADRE, SEDE_NOMBRE, NOMBRE_PROGRAMA))


# Consolidado Cupos y Mat Primera Vez por Programas

General_Cupos_MatPvez <- Acum_Cupos %>% left_join(Acum_MatriculadosPVPre, by = c("COD_PADRE")) %>% 
  rename(Sede = SEDE_NOMBRE,
         Programa = NOMBRE_PROGRAMA) %>% 
mutate(Ratio = Matriculados / Cupos)

# Bases Sedes

General_Cupos_MatPvez_Bog <- General_Cupos_MatPvez %>% filter(Sede == "Bogotá") %>%  mutate(Porcentaje = round(Ratio * 100, 1)) %>%  arrange(Porcentaje)
General_Cupos_MatPvez_Med <- General_Cupos_MatPvez %>% filter(Sede == "Medellín") %>%  mutate(Porcentaje = round(Ratio * 100, 1)) %>%  arrange(Porcentaje)
General_Cupos_MatPvez_Man <- General_Cupos_MatPvez %>% filter(Sede == "Manizales") %>%  mutate(Porcentaje = round(Ratio * 100, 1)) %>%  arrange(Porcentaje)
General_Cupos_MatPvez_Pal <- General_Cupos_MatPvez %>% filter(Sede == "Palmira") %>%  mutate(Porcentaje = round(Ratio * 100, 1)) %>%  arrange(Porcentaje)
General_Cupos_MatPvez_Paz <- General_Cupos_MatPvez %>% filter(Sede == "De La Paz") %>%  mutate(Porcentaje = round(Ratio * 100, 1)) %>%  arrange(Porcentaje)


# Sede Bogotá

hchart(General_Cupos_MatPvez_Bog, "bar", hcaes(x = Programa, y = Porcentaje), name = "% Cupos utilizados") %>%
  hc_title(text = "Porcentaje matriculados primera vez vs. cupos diponibles en Pregrado, SEDE BOGOTÁ. <br> (Periodo 2021-2026)") %>%
  hc_xAxis(title = list(text = "Programa Académico")) %>%
  hc_yAxis(
    title = list(text = "Porcentaje"),
    labels = list(format = "{value}%"),
    max = 120,
    plotLines = list(
      list(
        value = 100,      # Valor donde se ubica la línea
        color = "red",    # Color rojo
        dashStyle = "Dot", # Estilo punteado (puedes usar 'Dash' para guiones)
        width = 2,        # Grosor de la línea
        zIndex = 5,       # Para que aparezca sobre las barras
        label = list(
          text = "Límite Cupos (100%)",
          align = "center",         # Centra el texto respecto a la línea
          verticalAlign = "top",    # Lo coloca en la parte superior del gráfico
          rotation = 0,             # Asegura que el texto esté horizontal
          y = -10,                  # Desplazamiento vertical para que no toque el borde
          style = list(color = "red", fontWeight = "bold"))))) %>%
  hc_plotOptions(
    bar = list(
      dataLabels = list(enabled = TRUE, format = "{point.y}%")
    )
  ) %>%
  # --- Activa el menú de hamburguesa con opciones por defecto ---
  hc_exporting(enabled = TRUE) %>% 
  hc_colors("#2b908f")

# Sede Medellín

hchart(General_Cupos_MatPvez_Med, "bar", hcaes(x = Programa, y = Porcentaje), name = "% Cupos utilizados") %>%
  hc_title(text = "Porcentaje matriculados primera vez vs. cupos diponibles en Pregrado, SEDE MEDELLÍN. <br> (Periodo 2021-2026)") %>%
  hc_xAxis(title = list(text = "Programa Académico")) %>%
  hc_yAxis(
    title = list(text = "Porcentaje"),
    labels = list(format = "{value}%"),
    max = 120,
    plotLines = list(
      list(
        value = 100,      # Valor donde se ubica la línea
        color = "red",    # Color rojo
        dashStyle = "Dot", # Estilo punteado (puedes usar 'Dash' para guiones)
        width = 2,        # Grosor de la línea
        zIndex = 5,       # Para que aparezca sobre las barras
        label = list(
          text = "Límite Cupos (100%)",
          align = "center",         # Centra el texto respecto a la línea
          verticalAlign = "top",    # Lo coloca en la parte superior del gráfico
          rotation = 0,             # Asegura que el texto esté horizontal
          y = -10,                  # Desplazamiento vertical para que no toque el borde
          style = list(color = "red", fontWeight = "bold"))))) %>%
  hc_plotOptions(
    bar = list(
      dataLabels = list(enabled = TRUE, format = "{point.y}%")
    )
  ) %>%
  # --- Activa el menú de hamburguesa con opciones por defecto ---
  hc_exporting(enabled = TRUE) %>% 
  hc_colors("#2b908f")

# Sede Manizales


hchart(General_Cupos_MatPvez_Man, "bar", hcaes(x = Programa, y = Porcentaje), name = "% Cupos utilizados") %>%
  hc_title(text = "Porcentaje matriculados primera vez vs. cupos diponibles en Pregrado, SEDE MANIZALES. <br> (Periodo 2021-2026)") %>%
  hc_xAxis(title = list(text = "Programa Académico")) %>%
  hc_yAxis(
    title = list(text = "Porcentaje"),
    labels = list(format = "{value}%"),
    max = 120,
    plotLines = list(
      list(
        value = 100,      # Valor donde se ubica la línea
        color = "red",    # Color rojo
        dashStyle = "Dot", # Estilo punteado (puedes usar 'Dash' para guiones)
        width = 2,        # Grosor de la línea
        zIndex = 5,       # Para que aparezca sobre las barras
        label = list(
          text = "Límite Cupos (100%)",
          align = "center",         # Centra el texto respecto a la línea
          verticalAlign = "top",    # Lo coloca en la parte superior del gráfico
          rotation = 0,             # Asegura que el texto esté horizontal
          y = -10,                  # Desplazamiento vertical para que no toque el borde
          style = list(color = "red", fontWeight = "bold"))))) %>%
  hc_plotOptions(
    bar = list(
      dataLabels = list(enabled = TRUE, format = "{point.y}%")
    )
  ) %>%
  # --- Activa el menú de hamburguesa con opciones por defecto ---
  hc_exporting(enabled = TRUE) %>% 
  hc_colors("#2b908f")

# Sede Palmira

hchart(General_Cupos_MatPvez_Pal, "bar", hcaes(x = Programa, y = Porcentaje), name = "% Cupos utilizados") %>%
  hc_title(text = "Porcentaje matriculados primera vez vs. cupos diponibles en Pregrado, SEDE PALMIRA. <br> (Periodo 2021-2026)") %>%
  hc_xAxis(title = list(text = "Programa Académico")) %>%
  hc_yAxis(
    title = list(text = "Porcentaje"),
    labels = list(format = "{value}%"),
    max = 120,
    plotLines = list(
      list(
        value = 100,      # Valor donde se ubica la línea
        color = "red",    # Color rojo
        dashStyle = "Dot", # Estilo punteado (puedes usar 'Dash' para guiones)
        width = 2,        # Grosor de la línea
        zIndex = 5,       # Para que aparezca sobre las barras
        label = list(
          text = "Límite Cupos (100%)",
          align = "center",         # Centra el texto respecto a la línea
          verticalAlign = "top",    # Lo coloca en la parte superior del gráfico
          rotation = 0,             # Asegura que el texto esté horizontal
          y = -10,                  # Desplazamiento vertical para que no toque el borde
          style = list(color = "red", fontWeight = "bold"))))) %>%
  hc_plotOptions(
    bar = list(
      dataLabels = list(enabled = TRUE, format = "{point.y}%")
    )
  ) %>%
  # --- Activa el menú de hamburguesa con opciones por defecto ---
  hc_exporting(enabled = TRUE) %>% 
  hc_colors("#2b908f")


# Sede La Paz

hchart(General_Cupos_MatPvez_Paz, "bar", hcaes(x = Programa, y = Porcentaje), name = "% Cupos utilizados") %>%
  hc_title(text = "Porcentaje matriculados primera vez vs. cupos diponibles en Pregrado, SEDE LA PAZ. <br> (Periodo 2021-2026)") %>%
  hc_xAxis(title = list(text = "Programa Académico")) %>%
  hc_yAxis(
    title = list(text = "Porcentaje"),
    labels = list(format = "{value}%"),
    max = 120,
    plotLines = list(
      list(
        value = 100,      # Valor donde se ubica la línea
        color = "red",    # Color rojo
        dashStyle = "Dot", # Estilo punteado (puedes usar 'Dash' para guiones)
        width = 2,        # Grosor de la línea
        zIndex = 5,       # Para que aparezca sobre las barras
        label = list(
          text = "Límite Cupos (100%)",
          align = "center",         # Centra el texto respecto a la línea
          verticalAlign = "top",    # Lo coloca en la parte superior del gráfico
          rotation = 0,             # Asegura que el texto esté horizontal
          y = -10,                  # Desplazamiento vertical para que no toque el borde
          style = list(color = "red", fontWeight = "bold"))))) %>%
  hc_plotOptions(
    bar = list(
      dataLabels = list(enabled = TRUE, format = "{point.y}%")
    )
  ) %>%
  # --- Activa el menú de hamburguesa con opciones por defecto ---
  hc_exporting(enabled = TRUE) %>% 
  hc_colors("#2b908f")








library(highcharter)
library(dplyr)

# 1. Preparación de datos
datos_univ <- data.frame(
  Programa = c("Arquitectura", "Diseño Gráfico", "Artes Plásticas", "Diseño Industrial", 
               "Cine y TV", "Música", "Música Instrumental", "Biología", 
               "Estadística", "Farmacia", "Física"),
  Ratio = c(1.014, 1.108, 0.966, 1.012, 0.986, 0.284, 0.482, 1.145, 1.03, 0.983, 0.910)
) %>%
  mutate(Porcentaje = round(Ratio * 100, 1)) %>%
  arrange(Porcentaje)

# 2. Generación del gráfico con menú por defecto
hchart(datos_univ, "bar", hcaes(x = Programa, y = Porcentaje), name = "Cumplimiento") %>%
  hc_title(text = "Porcentaje de Matriculados vs. Cupos") %>%
  hc_xAxis(title = list(text = "Programa")) %>%
  hc_yAxis(
    title = list(text = "Porcentaje"),
    labels = list(format = "{value}%"),
    max = 120 
  ) %>%
  hc_plotOptions(
    bar = list(
      dataLabels = list(enabled = TRUE, format = "{point.y}%")
    )
  ) %>%
  # --- Activa el menú de hamburguesa con opciones por defecto ---
  hc_exporting(enabled = TRUE) %>% 
  hc_colors("#2b908f")






