# Tranformación Matricula 2026-1

# Librerías
library(tidyverse)
library(readxl)
library(UnalData)

# Importar datos

Matricula_20261 <- read_excel("Datos/2026_1_Matricula.xlsx", guess_max = 50000)
Cupos_Pre <- read_excel("Datos/Cupos_Pregrado.xlsx", guess_max = 5000)

# Transformar Datos

# Extraccion listado programas --------------------------------------------

programas <- UnalData::Hprogramas
programas <- programas %>%  select(SNIES_PROGRA,
                                   COD_PADRE,
                                   PROGRAMA,
                                   ESTADO)

# reemplazo de nombres antiguo por los mas actuales -

programas$PROGRAMA_2 <- programas$PROGRAMA
programas_duplicados <- programas[duplicated(programas$COD_PADRE)|duplicated(programas$COD_PADRE, fromLast=TRUE),] %>%
  select(COD_PADRE,SNIES_PROGRA,PROGRAMA) %>%
  distinct()
programas_duplicados$SNIES_PROGRA <- as.numeric(programas_duplicados$SNIES_PROGRA)


for(i in unique(programas_duplicados$COD_PADRE)){
  nombres_posibles <- programas_duplicados %>% filter(COD_PADRE == i)
  valor_a_reemplazar = nombres_posibles %>%
    filter(SNIES_PROGRA == max(SNIES_PROGRA)) %>%
    select(PROGRAMA) %>% pull()
  programas$PROGRAMA_2[programas$SNIES_PROGRA %in%  nombres_posibles$SNIES_PROGRA] <- valor_a_reemplazar
}

# union de nuevos nombres a Matricula_20261 ------------------------------------

programas <- programas %>%  select(SNIES_PROGRA,COD_PADRE,PROGRAMA_2,ESTADO)
Matricula_20261$SNIES_PROGRA <- as.numeric(Matricula_20261$SNIES_PROGRA)
Matricula_20261 <- Matricula_20261 %>% left_join(
  y = programas,
  by = "SNIES_PROGRA",
  keep = F)

#SE REALIZA ESTA LIMPIEZA ANTES DEL CRUCE PARA OBTENER LOS ESTADOS CORRECTOS

Matricula_20261 <- Matricula_20261 %>%
  mutate(SNIES_PROGRA = case_when(
    ((TIPO_NIVEL == "Pregrado" & SEDE_NOMBRE_ADM == "Manizales") & SNIES_PROGRA == 128) ~ 16913,
    T ~ SNIES_PROGRA))

Matricula_20261$ESTADO <- ifelse(Matricula_20261$ESTADO == "Migrado","Activo",Matricula_20261$ESTADO)

# transformaciones --------------------------------------------------------

#correcion variable tipo inscrip, mod inscrip caso la paz

Matricula_20261 <- Matricula_20261 %>% mutate(
  MOD_ADM = case_when(
    (SEDE_NOMBRE_MAT == "De La Paz" & (YEAR < 2023)) ~ "Regular",
    T ~ MOD_ADM),
  
  TIPO_ADM = case_when(
    (SEDE_NOMBRE_MAT == "De La Paz" & (YEAR < 2023)) ~ "Regular",
    T ~ TIPO_ADM),
  
  PAES = case_when( PAES == "De La Paz" ~ NA,
                    T ~ PAES),
  # Correción facultad de programa de especializacion filosofia de la mente de derecho a ciencias humana
  FACULTAD = case_when(
    SNIES_PROGRA == 51680 ~ "Ciencias humanas",
    T ~ FACULTAD))

#Correción modalidiadess para codigos SNIES iguales

Matricula_20261 <- Matricula_20261 %>% mutate(
  NIVEL = case_when(
    SNIES_PROGRA == 16918 ~ "Pregrado",
    SNIES_PROGRA == 52737 ~ "Maestría",
    SNIES_PROGRA == 52729 ~ "Doctorado",
    T ~ NIVEL))

#Transformacion nuevo pbm

Matricula_20261 <- Matricula_20261 %>% mutate(
  PBM_2 = case_when(
    PBM_ORIG < 12 ~ "0 a 11",
    PBM_ORIG < 18 ~ "12 a 17",
    PBM_ORIG < 31 ~ "18 a 30",
    PBM_ORIG < 51 ~ "31 a 50",
    PBM_ORIG < 71 ~ "51 a 70",
    PBM_ORIG < 101 ~ "71 a 100",
    T ~ NA))

Matricula_20261 <- Matricula_20261 %>% mutate(TIPO_NIVEL = if_else(is.na(TIPO_NIVEL), "Sin información", TIPO_NIVEL),
                                                NIVEL = if_else(is.na(NIVEL), "Sin información", NIVEL),
                                                SEDE_NOMBRE_MAT = if_else(is.na(SEDE_NOMBRE_MAT), "Sin información", SEDE_NOMBRE_MAT),
                                                SEDE_NOMBRE_MAT = if_else(SEDE_NOMBRE_MAT == "De La Paz", "La Paz", SEDE_NOMBRE_MAT),
                                                FACULTAD = if_else(FACULTAD %in% c("Agronomía", "Ciencias grarias"), "Ciencias agrarias", FACULTAD),
                                                FACULTAD = if_else(FACULTAD %in% c("Ingenieria"), "Ingeniería", FACULTAD),
                                                FACULTAD = if_else(FACULTAD %in% c("Ciencias humanas  y económicas"), "Ciencias humanas y económicas", FACULTAD),
                                                FACULTAD = if_else(FACULTAD %in% c("Ciencias agropecuarias") & SEDE_NOMBRE_MAT == "Medellín" , "Ciencias agrarias", FACULTAD),
                                                FACULTAD = if_else(FACULTAD %in% c("Ingenieria y administración"), "Ingeniería y administración", FACULTAD),
                                                NACIONALIDAD = if_else(is.na(NACIONALIDAD), "Sin información", NACIONALIDAD),
                                                SEXO = if_else(is.na(SEXO), "Sin información", SEXO),
                                                CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD),
                                                ESTRATO = if_else(is.na(ESTRATO), "Sin información", ESTRATO),
                                                ESTRATO_ORIG = if_else(is.na(ESTRATO_ORIG), "Sin información", ESTRATO_ORIG),
                                                TIPO_COL = if_else(is.na(TIPO_COL), "Sin información", TIPO_COL),
                                                PBM = if_else(is.na(PBM), "Sin información", PBM),
                                                PBM_2 = if_else(is.na(PBM_2), "Sin información", PBM_2),
                                                MAT_PVEZ = if_else(is.na(MAT_PVEZ), "Sin información", MAT_PVEZ),
                                                MOD_ADM = if_else(is.na(MOD_ADM), "Sin información", MOD_ADM),
                                                TIPO_ADM = if_else(is.na(TIPO_ADM), "Sin información", TIPO_ADM),
                                                PAES = if_else(is.na(PAES), "Sin información", PAES),
                                                PEAMA = if_else(is.na(PEAMA), "Sin información", PEAMA),
                                                MOV_PEAMA = if_else(is.na(MOV_PEAMA), "Sin información", MOV_PEAMA),
                                                CONVENIO = if_else(is.na(CONVENIO), "Sin información", CONVENIO),
                                                TIP_CONVENIO = if_else(is.na(TIP_CONVENIO), "Sin información", TIP_CONVENIO),
                                                AREAC_SNIES = if_else(is.na(AREAC_SNIES), "Sin información", AREAC_SNIES),
                                                AREA_CINE = if_else(is.na(AREA_CINE), "Sin información", AREA_CINE),
                                                AREA_CINE = if_else(AREA_CINE %in% c("Ingeniería, Industria y Construcción"), "Ingeniería, industria y construcción", AREA_CINE),
                                                AREA_CINE = if_else(AREA_CINE %in% c("Ciencias sociales"), "Ciencias sociales, periodismo e información", AREA_CINE),
                                                AREA_CINE = if_else(AREA_CINE %in% c("Tecnologías de la información y la comunicación"), "Tecnologías de la información y la comunicación (TIC)", AREA_CINE),
                                                NIVEL = if_else(NIVEL == "Especialidades  médicas", "Especialidades médicas", NIVEL),
                                                CAT_EDAD = if_else(CAT_EDAD == "26 o  más años", "26 o más años", CAT_EDAD)
)

# modificacion facultades amazonia y caribe -------------------------------

Matricula_20261$FACULTAD <- ifelse(Matricula_20261$SEDE_NOMBRE_MAT %in% c("Amazonía","Caribe","La Paz","Orinoquía","Tumaco"),"Dirección de sede",Matricula_20261$FACULTAD)

#correcion datos sin informacion a No en convenio
#ya que solo hay 13 en todo el historial y la funcion drilldown
#no funcionn si hay mas niveles que si o no

Matricula_20261 <- Matricula_20261 %>% mutate(
  CONVENIO = case_when(
    CONVENIO == "Sin información" & (YEAR == 2018 & SEMESTRE == 2) ~ "No",
    T ~ CONVENIO))


# modalidad junto el nombre del programa ----------------------------------

Matricula_20261 <- Matricula_20261 %>%  mutate(
  PROGRAMA_2 = case_when(
    TIPO_NIVEL == "Postgrado" ~ paste(PROGRAMA_2,"-",NIVEL),
    TIPO_NIVEL == "Pregrado" ~ PROGRAMA_2,))

# correcciones

Matricula_20261 <- Matricula_20261 %>% mutate(FACULTAD = ifelse(YEAR == 2018 &
                                                                    SNIES_PROGRA %in% c(69), "Derecho, ciencias políticas y sociales", FACULTAD),
                                                FACULTAD= ifelse(YEAR == 2023 & SEDE_NOMBRE_MAT == "Palmira" & FACULTAD == "Ciencias agrarias",
                                                                 "Ciencias agropecuarias", FACULTAD))
