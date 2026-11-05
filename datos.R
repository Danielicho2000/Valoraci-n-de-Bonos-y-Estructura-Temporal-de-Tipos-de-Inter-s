library(dplyr)
library(tibble)

fecha_valoracion <- as.Date("2026-09-30")

# Cupón corrido en puntos por cada 100 de nominal (base ACT/ACT)
cupon_corrido <- function(cupon, freq, inicio_devengo, ref_inicio, ref_fin, fecha) {
  (cupon / freq) * as.numeric(fecha - inicio_devengo) /
    as.numeric(ref_fin - ref_inicio) * 100
}

cartera_bonos <- tibble(
  ISIN      = c("DE0001135325", "DE0001135275", "ES00000120N0",
                "IT0005530032", "XS3239891503", "XS2108987517"),
  Emisor    = c("Alemania (Bund)", "Alemania (Bund)", "España (Obligación)",
                "Italia (BTP)", "Colombia (Global)", "Chile (Global)"),
  Categoria = c("A - Bajo Riesgo", "A - Bajo Riesgo",
                "B - Periférico", "B - Periférico",
                "C - Emergente", "C - Emergente"),
  Moneda    = c("EUR", "EUR", "EUR", "EUR", "EUR", "EUR"),
  Rating = c("AAA", "AAA", "A+", "BBB+", "BB-", "A"),   # Calificación estándar (ej. S&P)
  
  Fecha_Emision     = as.Date(c("2007-01-26", "2005-01-04", "2007-06-20", #AA/MM/DD
                                "2022-09-01", "2025-11-26", "2020-01-29")),
  Fecha_Vencimiento = as.Date(c("2039-07-04", "2037-01-04", "2040-07-30",
                                "2043-09-01", "2038-11-26", "2040-01-29")),
  
  Cupon_Nominal = c(0.0425, 0.04, 0.049, 0.0445, 0.065, 0.0125),   # en decimal
  Periodicidad  = c(1, 1, 1, 2, 1, 1),
  
  # Fechas para el cupón corrido
  Inicio_Devengo = as.Date(c("2026-01-26", "2026-01-04", "2026-07-30",
                             "2026-09-01", "2025-11-26", "2026-01-29")),
  Ref_Inicio     = as.Date(c("2026-01-26", "2026-01-04", "2026-07-30",
                             "2026-09-01", "2025-11-26", "2026-01-29")),
  Ref_Fin        = as.Date(c("2027-01-26", "2027-01-04", "2027-07-30",
                             "2027-03-01", "2026-11-26", "2027-01-29")),
  
  Precio_Limpio = c(105.293, 103.435, 105.75, 94.43, 96.25, 67.09)  
) %>%
  mutate(
    Plazo_Residual = as.numeric(Fecha_Vencimiento - fecha_valoracion) / 365.25,
    Cumple_10y     = Fecha_Vencimiento >= as.Date("2036-09-30"),
    Cupon_Corrido  = cupon_corrido(Cupon_Nominal, Periodicidad,
                                   Inicio_Devengo, Ref_Inicio, Ref_Fin, fecha_valoracion),
    Precio_Sucio   = Precio_Limpio + Cupon_Corrido
  )

print(cartera_bonos %>%
        select(ISIN, Categoria, Fecha_Vencimiento, Plazo_Residual, Cumple_10y,
               Precio_Limpio, Cupon_Corrido, Precio_Sucio))




################################################################################


raw_eiopa <- read_excel("EIOPA_RFR_20260930_Term_Structures.xlsx", 
                        sheet = "RFR_spot_no_VA", 
                        col_names = FALSE, 
                        col_types = "text")
datos_crudos <- raw_eiopa[11:nrow(raw_eiopa), ]
curva_final <- tibble(
  Maturity = as.numeric(datos_crudos[[1]]),   
  Rate_EUR = as.numeric(datos_crudos[[2]]),   
  Rate_USD = as.numeric(datos_crudos[[42]])   
) %>%
  filter(!is.na(Maturity))

print(head(curva_final, 15))

