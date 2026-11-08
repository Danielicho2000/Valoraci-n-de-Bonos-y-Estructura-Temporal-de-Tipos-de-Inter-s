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
##### PARTE 1.1
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
##### PARTE 1.2
# Función para construir la tabla de amortización (Coste Amortizado) de un bono
construir_tabla_amortizacion <- function(isin, estado, cupon_nominal, freq, 
                                         fecha_venc, precio_sucio, y0, 
                                         fecha_val = as.Date("2026-09-30"), 
                                         vn = 100) {
  
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
tablas_amortizacion <- pmap(
  list(
    isin = cartera_bonos$ISIN,
    estado = cartera_bonos$Estado,
    cupon_nominal = cartera_bonos$Cupon_Nominal,
    freq = cartera_bonos$Periodicidad,
    fecha_venc = cartera_bonos$Fecha_Vencimiento,
    precio_sucio = cartera_bonos$Precio_Sucio,
    y0 = cartera_bonos$TIR_y0
  ),
  construir_tabla_amortizacion,
  fecha_val = fecha_valoracion,
  vn = 100
)
################################################################################
#PARTE 2 FASE 2*************************************************

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



raw_eiopa <- read_excel(
  "EIOPA_RFR_20260930_Term_Structures.xlsx",
  sheet = "RFR_spot_no_VA",
  range = "B11:C160",
  col_names = FALSE, col_types = c("numeric","numeric")
)
primera_col_num <- suppressWarnings(as.numeric(raw_eiopa[[1]]))
fila_inicio     <- which(!is.na(primera_col_num))[1]
datos_curva     <- raw_eiopa[fila_inicio:nrow(raw_eiopa), ]

curva_final <- tibble(
  Maturity = as.numeric(datos_curva[[1]]),
  Rate_EUR = as.numeric(datos_curva[[2]])
) %>% filter(!is.na(Maturity), !is.na(Rate_EUR)) %>% arrange(Maturity)




# Curva interpolada linealmente
r_interp <- function(tau) approx(curva_final$Maturity, curva_final$Rate_EUR,
                                 xout = tau, rule = 2)$y

# Valor razonable con spread s 
vv_spread <- function(s, cupon, freq, ref_fin, fecha_venc,
                      fecha_val = fecha_valoracion, vn = 100) {
  fechas <- rev(seq(fecha_venc, ref_fin, by = paste0("-", 12/freq, " months")))
  tau <- as.numeric(fechas - fecha_val) / 365.25
  ck  <- (cupon / freq) * vn
  sum(ck / (1 + r_interp(tau) + s)^tau) + vn / (1 + r_interp(tail(tau,1)) + s)^tail(tau,1)
}

# Spread s tal que V_VR(s) = precio sucio de mercado 
spread_bono <- function(cupon, freq, ref_fin, fecha_venc, precio_sucio) {
  uniroot(
    function(s) vv_spread(s, cupon, freq, ref_fin, fecha_venc) - precio_sucio,
    interval = c(-0.05, 0.30), tol = 1e-10
  )$root
}

# Aplicar a toda la cartera
cartera_bonos <- cartera_bonos %>%
  rowwise() %>%
  mutate(
    Spread_s      = spread_bono(Cupon_Nominal, Periodicidad, Ref_Fin,
                                Fecha_Vencimiento, Precio_Sucio),
    Spread_bps    = round(Spread_s * 1e4, 1),
    # Valor razonable solo con la curva libre de riesgo (s = 0)
    V_VR_0        = vv_spread(0, Cupon_Nominal, Periodicidad, Ref_Fin,
                              Fecha_Vencimiento),
    # Lo que corresponde al riesgo de crédito
    Parte_credito = V_VR_0 - Precio_Sucio
  ) %>%
  ungroup()

print(cartera_bonos %>%
        select(ISIN, Estado, Rating, Precio_Sucio, V_VR_0, Spread_bps, Parte_credito))

# Verificación: V_VR(s) debe reproducir el precio sucio
#verif <- cartera_bonos %>%
#  rowwise() %>%
#  mutate(err = vv_spread(Spread_s, Cupon_Nominal, Periodicidad, Ref_Fin,
#                         Fecha_Vencimiento) - Precio_Sucio) %>%
#  pull(err)
#stopifnot(all(abs(verif) < 1e-6))



################################################################################
#PARTE 3 FASE 2*************************************************
# Análisis de sensibilidad y riesgo de tipo de interés

# Flujos de cada bono
flujos_bono <- function(cupon, freq, ref_fin, fecha_venc,
                        fecha_val = fecha_valoracion, vn = 100) {
  fechas <- rev(seq(fecha_venc, ref_fin, by = paste0("-", 12/freq, " months")))
  tau <- as.numeric(fechas - fecha_val) / 365.25
  cf  <- rep((cupon / freq) * vn, length(tau))
  cf[length(cf)] <- cf[length(cf)] + vn
  tibble(tau = tau, cf = cf)
}

# Duración de Macaulay
metricas_bono <- function(cupon, freq, ref_fin, fecha_venc, y) {
  fl <- flujos_bono(cupon, freq, ref_fin, fecha_venc)
  pv <- fl$cf / (1 + y)^fl$tau          # PV(C_k) descontado a la TIR
  P  <- sum(pv)
  D_mac <- sum(fl$tau * pv) / P
  tibble(
    P_check    = P,
    D_Mac      = D_mac,
    D_mod      = D_mac / (1 + y),
    Convexidad = sum(fl$tau * (fl$tau + 1) * pv) / (P * (1 + y)^2)
  )
}

metricas <- bind_rows(pmap(
  list(cartera_bonos$Cupon_Nominal, cartera_bonos$Periodicidad,
       cartera_bonos$Ref_Fin, cartera_bonos$Fecha_Vencimiento,
       cartera_bonos$TIR_y0),
  metricas_bono
))



# Pesos por valor de mercado y métricas de la cartera 
cartera_sens <- bind_cols(cartera_bonos, select(metricas, -P_check)) %>%
  mutate(w = Precio_Sucio / sum(Precio_Sucio))

D_cartera <- sum(cartera_sens$w * cartera_sens$D_mod)
C_cartera <- sum(cartera_sens$w * cartera_sens$Convexidad)

print(cartera_sens %>%
        select(ISIN, Estado, TIR_y0, D_Mac, D_mod, Convexidad, w),
      n = Inf, width = Inf)
cat("\nDuración modificada de la cartera:", round(D_cartera, 4),
    "\nConvexidad de la cartera:", round(C_cartera, 4), "\n")

# Escenarios de desplazamiento paralelo: Taylor vs revaloración completa 
escenario <- function(ds) {
  por_bono <- cartera_sens %>%
    rowwise() %>%
    mutate(
      # Revaloración completa: curva EIOPA + ds + spread del bono
      P_full     = vv_spread(Spread_s + ds, Cupon_Nominal, Periodicidad,
                             Ref_Fin, Fecha_Vencimiento),
      Var_full   = P_full / Precio_Sucio - 1,
      # Taylor de segundo orden (ec. 6)
      Var_taylor = -D_mod * ds + 0.5 * Convexidad * ds^2,
      Error      = Var_taylor - Var_full
    ) %>%
    ungroup()
  
  # Cartera
  cartera <- tibble(
    ISIN = "Cartera", Estado = "-",
    Var_taylor = -D_cartera * ds + 0.5 * C_cartera * ds^2,
    Var_full   = sum(por_bono$P_full) / sum(por_bono$Precio_Sucio) - 1,
    Error      = Var_taylor - Var_full
  )
  
  bind_rows(select(por_bono, ISIN, Estado, Var_taylor, Var_full, Error), cartera) %>%
    mutate(Delta_y_pb = ds * 1e4)
}

res_sens <- bind_rows(escenario(-0.01), escenario(0.01))

# Resultados en porcentaje 
print(res_sens %>%
        mutate(across(c(Var_taylor, Var_full, Error), ~ round(. * 100, 4))) %>%
        select(Delta_y_pb, ISIN, Estado, Var_taylor, Var_full, Error),
      n = Inf, width = Inf)

################################################################################
###############################  FASE 3   ######################################

################################################################################
#####PARTE a
curva_vasicek <- curva_final %>%
  transmute(
    tau = as.numeric(Maturity),
    tasa_efectiva = as.numeric(Rate_EUR)
  ) %>%
  filter(
    !is.na(tau),
    !is.na(tasa_efectiva),
    tau >= 1,
    tau <= 30
  ) %>%
  arrange(tau) %>%
  mutate(
    # Conversión a capitalización continua
    rc = log1p(tasa_efectiva)
  )

################################################################################
# 1. Descargar serie histórica para estimar sigma
# Fuente: Banco Central Europeo (BCE)
# Serie: AAA Euro Area Yield Curve - Spot Rate 1Y
# Capitalización continua, expresada en porcentaje anual.
#
# Se utilizan datos históricos de octubre de 2016 a septiembre de 2026.

url_bce <- paste0(
  "https://data-api.ecb.europa.eu/service/data/YC/",
  "B.U2.EUR.4F.G_N_A.SV_C_YM.SR_1Y",
  "?startPeriod=2016-10-01",
  "&endPeriod=", format(fecha_valoracion, "%Y-%m-%d"),
  "&format=csvdata"
)

archivo_bce <- "historico_BCE_1Y.csv"
if (!file.exists(archivo_bce)) {
  download.file(
    url = url_bce,
    destfile = archivo_bce,
    mode = "wb",
    method = "libcurl"
  )
}
datos_bce <- read.csv(archivo_bce)
# Preparar serie histórica diaria
historico_bce <- datos_bce %>%
  transmute(
    Fecha = as.Date(TIME_PERIOD),
    Tasa = as.numeric(OBS_VALUE) / 100
  ) %>%
  filter(
    !is.na(Fecha),
    !is.na(Tasa),
    Fecha <= fecha_valoracion
  ) %>%
  arrange(Fecha)
# Seleccionar la última observación disponible de cada mes
historico_mensual <- historico_bce %>%
  mutate(Mes = floor_date(Fecha, "month")) %>%
  group_by(Mes) %>%
  slice_max(Fecha, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(Mes)
################################################################################
# 2. Estimación de la volatilidad sigma
# Aproximación histórica:
#
# sigma = sd(r_t - r_(t-1)) / sqrt(Delta)
#
# Se utiliza la tasa spot a 1 año como aproximación observable
# a la tasa corta no directamente observada.
#
# Delta = 1/12 años
delta <- 1 / 12
tasas_historicas <- historico_mensual$Tasa
variaciones_mensuales <- diff(tasas_historicas)
sigma_hat <- sd(variaciones_mensuales) / sqrt(delta)
library(ggplot2)
ggplot(historico_mensual, aes(x = Fecha, y = Tasa * 100)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(
    title = "Evolución histórica de la tasa spot a 1 año",
    subtitle = "Fuente: Banco Central Europeo",
    x = "Fecha",
    y = "Tasa de interés (%)"
  ) +
  theme_minimal()
################################################################################
# 3. Funciones teóricas de Vasicek
# Modelo:
# dr_t = a(b - r_t)dt + sigma*dW_t
#
# Precio de bono cupón cero:
# P(0,tau) = A(tau)*exp(-B(tau)*r0)
# Función B(tau)
B_vasicek <- function(tau, a) {
  
  -expm1(-a * tau) / a
  
}
# Logaritmo de A(tau)
log_A_vasicek <- function(tau, a, b, sigma) {
  
  B <- B_vasicek(tau, a)
  
  (b - sigma^2 / (2 * a^2)) * (B - tau) -
    (sigma^2 * B^2) / (4 * a)
  
}
# Precio teórico de un bono cupón cero
precio_cero_vasicek <- function(tau, a, b, r0, sigma) {
  
  exp(
    log_A_vasicek(tau, a, b, sigma) -
      B_vasicek(tau, a) * r0
  )
  
}
# Curva spot teórica de Vasicek
# R(0,tau) = -log(P(0,tau))/tau
spot_vasicek <- function(tau, a, b, r0, sigma) {
  
  -(
    log_A_vasicek(tau, a, b, sigma) -
      B_vasicek(tau, a) * r0
  ) / tau
  
}
################################################################################
# 4. Calibración de los parámetros a, b y r0
funcion_objetivo <- function(parametros) {
  a  <- parametros[1]
  b  <- parametros[2]
  r0 <- parametros[3]
  
  tasas_teoricas <- spot_vasicek(
    tau = curva_vasicek$tau,
    a = a,
    b = b,
    r0 = r0,
    sigma = sigma_hat
  )
  
  errores <- tasas_teoricas - curva_vasicek$rc
  1e6 * sum(errores^2)
}
# Valores iniciales de los parámetros
b_inicial  <- tail(curva_vasicek$rc, 1)
r0_inicial <- curva_vasicek$rc[1]
valores_a <- c(0.05, 0.15, 0.50, 1.50, 3.00)
calibraciones <- lapply(valores_a, function(a_inicial) {
  
  optim(
    par = c(
      a = a_inicial,
      b = b_inicial,
      r0 = r0_inicial
    ),
    fn = funcion_objetivo,
    method = "L-BFGS-B",
    lower = c(0.005, -0.10, -0.10),
    upper = c(4.000,  0.15,  0.15),
    control = list(
      maxit = 20000,
      factr = 1e7
    )
  )
  
})

# Seleccionar la calibración de menor error
errores_calibracion <- sapply(
  calibraciones,
  function(x) x$value
)

mejor_calibracion <- calibraciones[[
  which.min(errores_calibracion)
]]

if (mejor_calibracion$convergence != 0) {
  warning("Revisar convergencia de la calibración.")
}

# Parámetros estimados
a_hat  <- unname(mejor_calibracion$par["a"])
b_hat  <- unname(mejor_calibracion$par["b"])
r0_hat <- unname(mejor_calibracion$par["r0"])

parametros_vasicek <- tibble(
  Parametro = c("a", "b", "r0", "sigma"),
  Estimacion = c(a_hat, b_hat, r0_hat, sigma_hat)
)
print(parametros_vasicek)
################################################################################
# 5. Evaluación del ajuste de la curva
curva_ajustada <- curva_vasicek %>%
  mutate(
    Spot_Vasicek = spot_vasicek(
      tau = tau,
      a = a_hat,
      b = b_hat,
      r0 = r0_hat,
      sigma = sigma_hat
    ),
    Error = Spot_Vasicek - rc
  )
SSE <- sum(curva_ajustada$Error^2)
RMSE <- sqrt(mean(curva_ajustada$Error^2))

cat("\nRESULTADOS DE CALIBRACIÓN\n")
cat("a =", round(a_hat, 6), "\n")
cat("b =", round(b_hat, 6), "\n")
cat("r0 =", round(r0_hat, 6), "\n")
cat("sigma =", round(sigma_hat, 6), "\n")
cat("SSE =", format(SSE, scientific = TRUE), "\n")
cat("RMSE =", round(RMSE, 8), "\n")
cat("RMSE (bps) =", round(RMSE * 10000, 4), "\n")
cat("Convergencia =", mejor_calibracion$convergence, "\n")

# Comparación gráfica de las curvas
ggplot(curva_ajustada, aes(x = tau)) +
  geom_line(
    aes(y = rc * 100, color = "EIOPA"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = Spot_Vasicek * 100, color = "Vasicek"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  labs(
    title = "Calibración del modelo de Vasicek",
    subtitle = "Curva EIOPA EUR - 30/09/2026",
    x = "Vencimiento (años)",
    y = "Tasa spot continua (%)",
    color = "Curva"
  ) +
  theme_minimal()

################################################################################
#####PARTE b
set.seed(12345)
N <- 10000             # Número de trayectorias
T <- 1                 # Horizonte: un año
n_pasos <- 12          # Pasos mensuales
dt <- T / n_pasos

tiempos <- seq(0, T, length.out = n_pasos + 1)
################################################################################
# 1. Discretización exacta del proceso de Vasicek
# r(t+dt) = r(t)*exp(-a*dt) + b*(1-exp(-a*dt)) +
#           sigma*sqrt((1-exp(-2*a*dt))/(2*a))*Z
#
# Z ~ N(0,1)
phi <- exp(-a_hat * dt)
termino_reversion <- b_hat * (1 - phi)

desv_innovacion <- sigma_hat *
  sqrt(-expm1(-2 * a_hat * dt) / (2 * a_hat))

# Matriz de 10000 trayectorias x 13 meses
trayectorias <- matrix(
  NA_real_,
  nrow = N,
  ncol = n_pasos + 1
)

colnames(trayectorias) <- paste0("Mes_", 0:n_pasos)

# Todas las trayectorias parten de r0
trayectorias[, 1] <- r0_hat

# Generación de trayectorias
for (j in 1:n_pasos) {
  
  Z <- rnorm(N, mean = 0, sd = 1)
  
  trayectorias[, j + 1] <-
    trayectorias[, j] * phi +
    termino_reversion +
    desv_innovacion * Z
  
}
################################################################################
# 2. Estadísticas mensuales
resumen_simulacion <- tibble(
  Mes = 0:n_pasos,
  Tiempo = tiempos,
  Media = colMeans(trayectorias),
  Desviacion = apply(trayectorias, 2, sd),
  P05 = apply(trayectorias, 2, quantile, probs = 0.05),
  Mediana = apply(trayectorias, 2, median),
  P95 = apply(trayectorias, 2, quantile, probs = 0.95)
)

print(resumen_simulacion, n = Inf)
################################################################################
# 3. Evolución de la tasa media y bandas percentílicas
ggplot(resumen_simulacion, aes(x = Mes)) +
  geom_ribbon(
    aes(
      ymin = P05 * 100,
      ymax = P95 * 100
    ),
    fill = "steelblue",
    alpha = 0.20
  ) +
  geom_line(
    aes(y = Media * 100),
    color = "blue",
    linewidth = 1
  ) +
  labs(
    title = "Evolución simulada de la tasa corta",
    subtitle = "Media e intervalo percentílico del 90%",
    x = "Mes",
    y = "Tasa corta (%)"
  ) +
  theme_minimal()
################################################################################
# 4. Distribución de la tasa corta al finalizar el año
r_T <- trayectorias[, n_pasos + 1]

resultados_finales <- tibble(
  Estadistico = c(
    "Media",
    "Desviación estándar",
    "Mínimo",
    "Percentil 5%",
    "Mediana",
    "Percentil 95%",
    "Máximo"
  ),
  Valor = c(
    mean(r_T),
    sd(r_T),
    min(r_T),
    unname(quantile(r_T, 0.05)),
    median(r_T),
    unname(quantile(r_T, 0.95)),
    max(r_T)
  )
)

print(resultados_finales)

# Histograma de r_T
ggplot(
  tibble(Tasa_Final = r_T * 100),
  aes(x = Tasa_Final)
) +
  geom_histogram(
    bins = 50,
    fill = "steelblue",
    color = "white"
  ) +
  geom_vline(
    xintercept = mean(r_T) * 100,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Distribución de la tasa corta al año 1",
    subtitle = "10000 simulaciones",
    x = "Tasa corta (%)",
    y = "Frecuencia"
  ) +
  theme_minimal()
################################################################################
# 5. Verificación de la simulación exacta
# Bajo Vasicek:
#
# E[r_T] = b + (r0-b)*exp(-a*T)
#
# Var[r_T] = sigma^2*(1-exp(-2*a*T))/(2*a)
media_teorica <- b_hat +
  (r0_hat - b_hat) * exp(-a_hat * T)

varianza_teorica <- sigma_hat^2 *
  (-expm1(-2 * a_hat * T)) / (2 * a_hat)

verificacion <- tibble(
  Estadistico = c("Media", "Varianza"),
  Teorico = c(
    media_teorica,
    varianza_teorica
  ),
  Simulado = c(
    mean(r_T),
    var(r_T)
  )
) %>%
  mutate(
    Error_Absoluto = abs(Teorico - Simulado)
  )

print(verificacion)


