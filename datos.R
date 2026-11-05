# ============================================================
# FASE 1 
# Fecha de valoración: 2026-09-30
# ============================================================
library(dplyr)
library(tidyr)
library(tibble)
library(readxl)
#install.packages("purrr")
library(purrr)
library(lubridate)

fecha_valoracion <- as.Date("2026-09-30")

cupon_corrido <- function(cupon, freq, inicio_devengo, ref_inicio, ref_fin, fecha) {
  (cupon / freq) * as.numeric(fecha - inicio_devengo) /
    as.numeric(ref_fin - ref_inicio) * 100
}

cartera_bonos <- tibble(
  ISIN = c(
    "DE0001135325", "DE0001135275",   # A – Alemania (2 Bunds)
    "IT0005530032", "IT0005321325",   # B – Italia (Corregido ISIN BTP 2.95% Sep 2038)
    "XS3239891503", "XS3183160236"    # C – Colombia (2 globales EUR reales más largos)
  ),
  Emisor = c(
    "Alemania (Bund)",   "Alemania (Bund)",
    "Italia (BTP)",      "Italia (BTP)",
    "Colombia (Global)", "Colombia (Global)"
  ),
  Estado = c("Alemania", "Alemania", "Italia", "Italia", "Colombia", "Colombia"),
  Categoria = c(
    "A - Bajo Riesgo", "A - Bajo Riesgo",
    "B - Periférico",  "B - Periférico",
    "C - Emergente",   "C - Emergente"
  ),
  Moneda = rep("EUR", 6),
  Rating = c("AAA", "AAA", "BBB+", "BBB+", "BB+", "BB+"),
  
  Fecha_Emision = as.Date(c(
    "2007-01-26", "2005-01-04",
    "2022-09-01", "2018-01-15",
    "2025-11-26", "2025-09-19"
  )),
  Fecha_Vencimiento = as.Date(c(
    "2039-07-04", "2037-01-04",
    "2043-09-01", "2038-09-01",
    "2038-11-26", "2036-02-19"  
  )),
  
  Cupon_Nominal = c(0.0425, 0.0400, 0.0445, 0.0295, 0.0650, 0.05625),
  Periodicidad  = c(1, 1, 2, 2, 1, 1),
  
  Inicio_Devengo = as.Date(c(
    "2026-07-04", "2026-01-04",
    "2026-09-01", "2026-09-01",
    "2025-11-26", "2026-02-19"
  )),
  Ref_Inicio = as.Date(c(
    "2026-07-04", "2026-01-04",
    "2026-09-01", "2026-09-01",
    "2025-11-26", "2026-02-19"
  )),
  Ref_Fin = as.Date(c(
    "2027-07-04", "2027-01-04",
    "2027-03-01", "2027-03-01",
    "2026-11-26", "2027-02-19"
  )),
  
  # Precios limpios verificados al 2026-09-30
  Precio_Limpio = c(
    105.06,   # DE0001135325 – Borsa Italiana
    103.53,   # DE0001135275 – Repubblica Finanza / Borsa Italiana
    94.55,    # IT0005530032 – BTP 4.45% Sep 2043
    84.53,    # IT0005321325 – BTP 2.95% Sep 2038 (Corregido)
    97.793,   # XS3239891503 – Colombia 6.50% Nov 2038 (Finanzen.at / Frankfurt)
    94.95     # XS3183160236 – Colombia 5.625% Feb 2036 (Börse Frankfurt)
  )
) %>%
  mutate(
    Plazo_Residual = as.numeric(Fecha_Vencimiento - fecha_valoracion) / 365.25,
    Cumple_10y     = Fecha_Vencimiento >= as.Date("2036-01-01"),
    Cupon_Corrido  = cupon_corrido(Cupon_Nominal, Periodicidad,
                                   Inicio_Devengo, Ref_Inicio, Ref_Fin,
                                   fecha_valoracion),
    Precio_Sucio   = Precio_Limpio + Cupon_Corrido
  )

# Comprobaciones de la Fase 1
# stopifnot(nrow(cartera_bonos) == 6)
# stopifnot(all(cartera_bonos$Cumple_10y))
# stopifnot(all(table(cartera_bonos$Categoria) == 2))
# stopifnot(n_distinct(cartera_bonos$Estado) == 3)
# stopifnot(all(cartera_bonos$Moneda == "EUR"))

print(cartera_bonos %>%
        select(ISIN, Categoria, Estado, Fecha_Vencimiento,
               Plazo_Residual, Precio_Limpio, Cupon_Corrido, Precio_Sucio))


################################################################################
###############################  FASE 2   ######################################
##### PARTE 1
# Función para calcular la TIR inicial (y_0) de un bono individual
calcular_tir_bono <- function(cupon_nominal, freq, ref_fin, fecha_venc, 
                              precio_sucio, fecha_val = as.Date("2026-09-30"), 
                              vn = 1000) {
  
  cupon_nominal <- as.numeric(cupon_nominal)
  freq          <- as.numeric(freq)
  precio_sucio  <- as.numeric(precio_sucio)
  ref_fin       <- as.Date(ref_fin)
  fecha_venc    <- as.Date(fecha_venc)
  fecha_val     <- as.Date(fecha_val)
  if (cupon_nominal > 1) cupon_nominal <- cupon_nominal / 100
  # pagos mensuales
  paso_meses  <- paste0("-", 12 / freq, " months")
  fechas_pago <- rev(seq(from = fecha_venc, to = ref_fin, by = paso_meses))
  
  # Asegurar que la última fecha coincida exactamente con el vencimiento
  if (tail(fechas_pago, 1) != fecha_venc) {
    fechas_pago <- c(fechas_pago, fecha_venc)
  }
  
  tiempos <- as.numeric(fechas_pago - fecha_val) / 365.25
  n_flujos <- length(tiempos)
  
  # Cupones periódicos C_k en base 1000
  c_k <- (cupon_nominal / freq) * vn
  
  # Función objetivo f(y_0) = VP(flujos) - P_0 = 0 ecuación 1
  f_objetivo <- function(y0) {
    vp_cupones <- sum(c_k / ((1 + y0)^tiempos))
    vp_nominal <- vn / ((1 + y0)^tiempos[n_flujos])
    (vp_cupones + vp_nominal) - precio_sucio
  }
  
  # Solución en el intervalo [-5%, 50%]
  solucion <- uniroot(f_objetivo, interval = c(-0.05, 0.50), extendInt = "yes", tol = 1e-10)
  
  return(solucion$root)
}

# Aplicar a toda la cartera 
cartera_bonos <- cartera_bonos %>%
  rowwise() %>%
  mutate(
    Precio_Sucio = Precio_Limpio + Cupon_Corrido,
    TIR_y0 = calcular_tir_bono(
      cupon_nominal = Cupon_Nominal,
      freq          = Periodicidad,
      ref_fin       = Ref_Fin,
      fecha_venc    = Fecha_Vencimiento,
      precio_sucio  = Precio_Sucio,
      fecha_val     = fecha_valoracion
    ),
    TIR_y0_pct = round(TIR_y0 * 100, 4)
  ) %>%
  ungroup()

# Visualizar resultados
print(cartera_bonos %>%
        select(ISIN, Estado, Cupon_Nominal, Periodicidad, 
               Precio_Limpio, Precio_Sucio, TIR_y0))
##### PARTE 2
# Función para construir la tabla de amortización (Coste Amortizado) de un bono
construir_tabla_amortizacion <- function(isin, estado, cupon_nominal, freq, 
                                         fecha_venc, precio_sucio, y0, 
                                         fecha_val = as.Date("2026-09-30"), 
                                         vn = 1000) {
  
  cupon_nominal <- as.numeric(cupon_nominal)
  if (cupon_nominal > 1) cupon_nominal <- cupon_nominal / 100
  freq         <- as.numeric(freq)
  precio_sucio <- as.numeric(precio_sucio)
  y0           <- as.numeric(y0)
  fecha_venc   <- as.Date(fecha_venc)
  fecha_val    <- as.Date(fecha_val)
  
  paso_meses  <- paste0("-", 12 / freq, " months")
  fechas_pago <- rev(seq(from = fecha_venc, to = fecha_val, by = paso_meses))
  fechas_pago <- fechas_pago[fechas_pago > fecha_val]
  
  # Vector completo de fechas desde t_0 hasta t_n
  fechas_totales <- c(fecha_val, fechas_pago)
  n_periodos     <- length(fechas_pago)
  
  delta_t <- as.numeric(diff(fechas_totales)) / 365.25
  
  # Cupón periódico constante C_k en base VN = 1000
  c_k <- (cupon_nominal / freq) * vn
  
  # Vectores para la recurrencia mensual
  v_ini  <- numeric(n_periodos) # V_{k-1}^{CA}
  i_k    <- numeric(n_periodos) # Ingreso financiero devengado I_k
  cupon  <- rep(c_k, n_periodos)# Cupón cobrado C_k
  amort  <- numeric(n_periodos) # Amortización prima/descuento (C_k - I_k)
  v_fin  <- numeric(n_periodos) # V_k^{CA}
  
  # Condición inicial: V_0^{CA} = P_0 (Precio Sucio)
  v_actual <- precio_sucio
  
  for (k in seq_len(n_periodos)) {
    v_ini[k] <- v_actual
    # Ingreso financiero devengado: I_k = V_{k-1}^{CA} * [(1 + y_0)^(t_k - t_{k-1}) - 1]
    i_k[k]   <- v_actual * ((1 + y0)^delta_t[k] - 1)
    # Amortización de la prima o descuento: C_k - I_k
    amort[k] <- cupon[k] - i_k[k]
    # Valor contable final tras el pago del cupón k: V_k^{CA} = V_{k-1}^{CA} + I_k - C_k
    v_fin[k] <- v_actual + i_k[k] - cupon[k]
    
    v_actual <- v_fin[k]
  }
  
  # Construir tibble incluyendo el momento inicial k = 0
  tabla <- tibble(
    ISIN                   = isin,
    Estado                 = estado,
    Periodo_k              = 0:n_periodos,
    Fecha                  = fechas_totales,
    Delta_t                = c(0, round(delta_t, 6)),
    Valor_Contable_Inicial = c(NA_real_, v_ini),
    Ingreso_Financiero_Ik  = c(0, i_k),
    Cupon_Cobrado_Ck       = c(0, cupon),
    Amortizacion_Prima_Desc= c(0, amort),
    Valor_Contable_Final   = c(precio_sucio, v_fin)
  )
  
  return(tabla)
}
cartera_bonos <- cartera_bonos %>%
  mutate(
    Tabla_CA = pmap(
      list(ISIN, Estado, Cupon_Nominal, Periodicidad, 
           Fecha_Vencimiento, Precio_Sucio, TIR_y0),
      ~ construir_tabla_amortizacion(
        isin          = ..1,
        estado        = ..2,
        cupon_nominal = ..3,
        freq          = ..4,
        fecha_venc    = ..5,
        precio_sucio  = ..6,
        y0            = ..7,
        fecha_val     = fecha_valoracion,
        vn            = 1000
      )
    ) %>% set_names(ISIN) # Asigna el ISIN como nombre de cada sub-tabla
  )

# 2. Extraer la lista nombrada para acceso directo
tablas_amortizacion <- cartera_bonos$Tabla_CA
################################################################################

raw_eiopa <- read_excel(
  "EIOPA_RFR_20260930_Term_Structures.xlsx",
  sheet = "RFR_spot_no_VA",
  col_names = FALSE, col_types = "text"
)
primera_col_num <- suppressWarnings(as.numeric(raw_eiopa[[1]]))
fila_inicio     <- which(!is.na(primera_col_num))[1]
datos_curva     <- raw_eiopa[fila_inicio:nrow(raw_eiopa), ]

curva_final <- tibble(
  Maturity = as.numeric(datos_curva[[1]]),
  Rate_EUR = as.numeric(datos_curva[[2]])
) %>% filter(!is.na(Maturity), !is.na(Rate_EUR)) %>% arrange(Maturity)

stopifnot(max(curva_final$Maturity) >= 30)
