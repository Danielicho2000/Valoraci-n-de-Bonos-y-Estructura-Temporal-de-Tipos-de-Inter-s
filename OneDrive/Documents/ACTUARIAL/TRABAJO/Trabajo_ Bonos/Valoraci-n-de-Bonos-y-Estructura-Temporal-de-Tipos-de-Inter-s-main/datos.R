library(readxl)
library(dplyr)
library(tibble)

fecha_valoracion <- as.Date("2026-09-30")

# ==============================================================================
# 1. CARTERA DE BONOS
# ==============================================================================
cartera_bonos <- tibble(
  ISIN = c("DE000BU2Z064", "DE000BU2Z072", "ES0000012Q08", "IT0005729931", "US195325EP60", "XS3272223762"),
  Emisor = c("Alemania (Bund)", "Alemania (Bund)", "España (Obligación)", "Italia (BTP)", "Colombia (Global)", "Chile (Global)"),
  Categoria = c("A - Bajo Riesgo", "A - Bajo Riesgo", "B - Periférico", "B - Periférico", "C - Emergente", "C - Emergente"),
  Moneda = c("EUR", "EUR", "EUR", "EUR", "USD", "EUR"),
  Rating = c("AAA", "AAA", "A", "BBB", "BB-", "A-"),
  Fecha_Emision = as.Date(c("2026-01-09", "2026-09-30", "2026-01-27", "2026-09-29", "2024-11-07", "2026-01-14")),
  Fecha_Vencimiento = as.Date(c("2036-02-15", "2036-08-15", "2036-04-30", "2036-10-01", "2036-11-07", "2036-04-14")),
  Plazo_Residual = as.numeric(difftime(Fecha_Vencimiento, fecha_valoracion, units = "days")) / 365.25,
  Cupón_Nominal = c(0.029, 0.030, 0.033, 0.040, 0.0775, 0.03875),
  Periodicidad = c(1, 1, 1, 2, 2, 1),   # Chile paga cupón anual
  Precio_Limpio = c(96.82, 96.95, 94.54, 95.58, 101.67, 94.83),   # XS3272223762 falta corregir
  Cupón_Corrido = c(2.31, 0.649, 1.636, 4, 3.5736, 2.113),          
  ) %>%
  mutate(Precio_Sucio = Precio_Limpio + Cupón_Corrido)
print(cartera_bonos)




################################################################################


# ==============================================================================
# 2. EXTRACCIÓN DE CURVA EIOPA (Versión Limpia y Directa)
# ==============================================================================
raw_eiopa <- read_excel("EIOPA_RFR_20260930_Term_Structures.xlsx", 
                        sheet = "RFR_spot_no_VA", 
                        col_names = FALSE, 
                        col_types = "text")

# Tomamos las filas directamente desde la fila 11 en adelante usando corchetes limpios
curva_final <- tibble(
  Maturity = as.numeric(raw_eiopa[[1]][10:nrow(raw_eiopa)]),   # Columna 2 (Plazo) desde fila 11
  Rate_EUR = as.numeric(raw_eiopa[[4]][10:nrow(raw_eiopa)]),   # Columna 3 (Eur) desde fila 11
  Rate_USD = as.numeric(raw_eiopa[[42]][10:nrow(raw_eiopa)])  # Columna 42 (USD) desde fila 11
) %>%
  filter(!is.na(Maturity))

# Ver los primeros 15 registros
print(head(curva_final, 15))



