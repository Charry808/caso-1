library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(moments)
library(lubridate)
install.packages("moments")
library(moments)
setwd("/Users/manuelagranadoshernandez/Documents/GitHub/caso-1/")
weekly <- read_csv("hoja_1_caso.csv") |> janitor::clean_names()
glimpse(weekly)
financials <- read_csv("hoja_2_caso.csv") |> janitor::clean_names()
glimpse(financials)
df <- weekly |>
  mutate(week = mdy(week_2008_2009)) |>
  inner_join(
    financials |> mutate(week = mdy(week_2008_2009)),
    by = "week"
  )

# Instalar y cargar paquetes necesarios
packages <- c("tidyverse","lubridate","janitor","readr")

to_install <- setdiff(packages, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(packages, library, character.only = TRUE))

# Ejemplo de Merge con Formatos de Fecha Diferentes
# Autor: [Tu Nombre]
# Fecha: 2025-04-05
# 
# Objetivo:
# - Unificar dos bases con fechas en formatos distintos.
# - Convertir texto en español ("10 de agosto de 2025") a formato fecha.
# - Realizar un merge por ID y fecha estandarizada.
# ===========================================================================

library(dplyr)
library(stringr)
df <- df |>
  mutate(period = case_when(
    week <= as.Date("2008-08-02") ~ "Initial",
    week <= as.Date("2008-12-20") ~ "Pre-Promo",
    week <= as.Date("2009-03-07") ~ "Promotion",
    TRUE ~ "Post-Promo"
  ))

# --- Paso 1: Crear datos de ejemplo ---

# Base 1: Fechas en formato estándar YYYY/MM/DD
df1 <- data.frame(
  id_cliente = c(101, 102, 103),
  fecha_compra = c("2025/08/10", "2025/08/11", "2025/08/12"),
  monto = c(250, 300, 150),
  stringsAsFactors = FALSE
)

# Base 2: Fechas en formato texto en español
df2 <- data.frame(
  id_cliente = c(101, 102, 104),
  fecha_compra = c("10 de agosto de 2025", "11 de agosto de 2025", "13 de agosto de 2025"),
  producto = c("Laptop", "Celular", "Tablet"),
  stringsAsFactors = FALSE
)

# --- Paso 2: Mapeo de meses en español a números ---
meses_es <- c(
  "enero" = 1, "febrero" = 2, "marzo" = 3, "abril" = 4,
  "mayo" = 5, "junio" = 6, "julio" = 7, "agosto" = 8,
  "septiembre" = 9, "octubre" = 10, "noviembre" = 11, "diciembre" = 12
)

# --- Paso 3: Limpiar y estandarizar df1 ---
df1_clean <- df1 %>%
  mutate(
    # Convertir a fecha y luego a formato estándar DD-MM-YYYY
    fecha_dt = as.Date(fecha_compra, format = "%Y/%m/%d"),
    fecha_std = format(fecha_dt, "%d-%m-%Y")
  ) %>%
  select(id_cliente, fecha_std, monto)

# --- Paso 4: Limpiar y estandarizar df2 ---
df2_clean <- df2 %>%
  rowwise() %>%
  mutate(
    # Extraer partes usando expresiones regulares (más robusto que strsplit)
    dia = as.numeric(str_extract(fecha_compra, "\\d+(?= de)")),
    mes_texto = tolower(str_extract(fecha_compra, "(?<=de )\\w+(?= de)")),
    anio = as.numeric(str_extract(fecha_compra, "\\d{4}$")),
    # Validar que el mes exista en el mapeo
    mes_num = ifelse(mes_texto %in% names(meses_es), meses_es[mes_texto], NA),
    # Crear fecha en formato estándar
    fecha_std = ifelse(
      !is.na(mes_num),
      sprintf("%02d-%02d-%04d", dia, mes_num, anio),
      NA_character_
    )
  ) %>%
  ungroup() %>%
  select(id_cliente, fecha_std, producto)

# --- Paso 5: Realizar el merge ---
base_unificada <- df1_clean %>%
  inner_join(df2_clean, by = c("id_cliente", "fecha_std")) %>%
  rename(fecha_compra = fecha_std)

# --- Paso 6: Mostrar resultado ---
print("Base unificada tras el merge:")
print(base_unificada)

# Opcional: convertir fecha_std a clase Date para análisis temporal
base_unificada <- base_unificada %>%
  mutate(fecha_compra = as.Date(fecha_compra, format = "%d-%m-%Y"))

str(base_unificada)
# ===========================================================================
# Web Analytics at Quality Alloys, Inc.
# Script inicial con generación de gráficas
# Autor: [Tu Nombre]
# Fecha: 2025-04-05
# 
# Objetivo: Leer datos, limpiarlos y generar visualizaciones clave.
# ===========================================================================

# --- PASO 1: Cargar librerías ---
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(scales)

# --- PASO 2: Definir ruta del archivo ---
archivo <- "data/raw/Web_Analytics (2).xls"

# --- PASO 3: Leer todo el archivo Excel ---
raw_data <- read_excel(archivo, col_names = FALSE)

# Renombrar columnas para facilitar el trabajo
names(raw_data) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")

# Visualizar estructura
head(raw_data, 10)

# --- PASO 4: Extraer y limpiar datos diarios de visitas ---

# Encontrar bloque "Daily Visits"
start_visits <- which(raw_data$X1 == "Daily Visits")
end_visits <- which(raw_data$X1 == "Quality Alloys") - 1

visits_raw <- raw_data[start_visits:end_visits, ]
visits_clean <- visits_raw %>%
  slice(-1:-2) %>%  # Quitar encabezados
  select(DateStr = X1, Visits = X2) %>%
  mutate(
    Date = mdy(gsub(".*, ", "", DateStr)),  # Extraer fecha
    Visits = as.numeric(Visits)
  ) %>%
  select(Date, Visits) %>%
  filter(!is.na(Date), !is.na(Visits)) %>%
  arrange(Date)

# --- PASO 5: Extraer datos semanales financieros ---

# Buscar "Weekly Financial Data"
start_fin <- which(raw_data$X1 == "Weekly Financial Data")
end_fin <- start_fin + 60  # Aprox. 60 semanas

fin_raw <- raw_data[start_fin:end_fin, ]
financials <- fin_raw %>%
  slice(3:62) %>%  # Datos reales
  rename(
    Week = X1,
    Revenue = X2,
    Profit = X3,
    Lbs_Sold = X4,
    Inquiries = X5
  ) %>%
  mutate(
    Week = gsub(".*- ", "", Week),
    Date = mdy(Week),
    Revenue = as.numeric(gsub("[$,]", "", Revenue)),
    Profit = as.numeric(gsub("[$,]", "", Profit)),
    Lbs_Sold = as.numeric(gsub(",", "", Lbs_Sold))
  ) %>%
  select(Date, Revenue, Profit, Lbs_Sold, Inquiries) %>%
  filter(!is.na(Date))

# --- PASO 6: Unir visitas diarias con datos semanales (por fecha aproximada) ---

# Aproximar visitas semanales como suma de visitas diarias por semana
visits_weekly <- visits_clean %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(Week) %>%
  summarise(Weekly_Visits = sum(Visits), .groups = "drop")

# Unir con datos financieros
merged_data <- financials %>%
  rename(Week = Date) %>%
  mutate(Week = floor_date(Week, "week")) %>%
  left_join(visits_weekly, by = "Week") %>%
  rename(Date = Week) %>%
  filter(!is.na(Weekly_Visits))

# --- PASO 7: Definir períodos del caso ---

merged_data <- merged_data %>%
  mutate(
    Period = case_when(
      Date >= ymd("2008-05-25") & Date <= ymd("2008-07-19") ~ "Initial",
      Date >= ymd("2008-07-20") & Date <= ymd("2008-08-30") ~ "Pre-Promo",
      Date >= ymd("2008-08-31") & Date <= ymd("2008-09-27") ~ "Promotion",
      Date >= ymd("2008-09-28") & Date <= ymd("2009-08-29") ~ "Post-Promo",
      TRUE ~ "Outside"
    )
  ) %>%
  filter(Period != "Outside")

# --- PASO 8: Generar gráficas ---

# 8.1 Gráfico 1: Serie de tiempo de visitas diarias
p1 <- ggplot(visits_clean, aes(x = Date, y = Visits)) +
  geom_line(color = "steelblue", size = 0.8) +
  labs(title = "Visitas Diarias al Sitio Web", x = "Fecha", y = "Visitas") +
  theme_minimal() +
  scale_y_continuous(labels = comma)

ggsave("figures_tables/fig1_daily_visits.png", p1, width = 10, height = 6, dpi = 150)

# 8.2 Gráfico 2: Ingresos semanales
p2 <- ggplot(merged_data, aes(x = Date, y = Revenue)) +
  geom_line(color = "darkgreen", size = 0.8) +
  labs(title = "Ingresos Semanales", x = "Fecha", y = "Ingresos ($)") +
  theme_minimal() +
  scale_y_continuous(labels = comma)

ggsave("figures_tables/fig2_weekly_revenue.png", p2, width = 10, height = 6, dpi = 150)

# 8.3 Gráfico 3: Histograma de Libras Vendidas
p3 <- ggplot(merged_data, aes(x = Lbs_Sold)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(title = "Distribución de Libras Vendidas", x = "Libras Vendidas", y = "Frecuencia") +
  theme_minimal()

ggsave("figures_tables/fig3_histogram_pounds_sold.png", p3, width = 8, height = 6, dpi = 150)

# 8.4 Gráfico 4: Scatter plot - Revenue vs Weekly Visits
p4 <- ggplot(merged_data, aes(x = Weekly_Visits, y = Revenue)) +
  geom_point(alpha = 0.7, color = "purple") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Ingresos vs Visitas Semanales", x = "Visitas Semanales", y = "Ingresos ($)") +
  theme_minimal()

ggsave("figures_tables/fig4_scatter_revenue_vs_visits.png", p4, width = 8, height = 6, dpi = 150)

# 8.5 Gráfico 5: Barras - Fuentes de Tráfico
# Extraer "All Traffic Sources"
start_traffic <- which(raw_data$X1 == "All Traffic Sources")
traffic_data <- raw_data[(start_traffic + 2):(start_traffic + 6), ] %>%
  select(Source = X1, Visits = X2) %>%
  mutate(Visits = as.numeric(gsub(",", "", Visits)))

p5 <- ggplot(traffic_data, aes(x = reorder(Source, Visits), y = Visits)) +
  geom_col(fill = "gray70") +
  coord_flip() +
  labs(title = "Fuentes de Tráfico al Sitio Web", x = "Fuente", y = "Número de Visitas") +
  theme_minimal()

ggsave("figures_tables/fig5_traffic_sources.png", p5, width = 8, height = 6, dpi = 150)

# 8.6 Gráfico 6: Comparación por período (boxplot de visitas)
p6 <- ggplot(merged_data, aes(x = Period, y = Weekly_Visits, fill = Period)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribución de Visitas por Período", y = "Visitas Semanales") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures_tables/fig6_boxplot_visits_by_period.png", p6, width = 9, height = 6, dpi = 150)

# --- PASO 9: Mostrar resumen rápido ---
cat("Gráficas generadas en la carpeta 'figures_tables/'.\n")
cat("Datos procesados listos para análisis adicional.\n")