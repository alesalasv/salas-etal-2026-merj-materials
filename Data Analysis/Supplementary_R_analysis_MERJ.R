# ==============================================================================
# Supplementary Material: R analysis script
#
# Article:
# Remote Laboratories in Undergraduate Differential Equations:
# a pretest–posttest study in distance higher education
#
# Journal:
# Mathematics Education Research Journal (Springer Nature)
#
# Purpose:
# This script implements the descriptive and inferential analyses associated
# with the study. The required input file is 'dataset.xlsx'.
#
# Note:
# Executable code, object names, table/figure labels, and statistical choices
# have been preserved from the supplied script. Documentation comments have
# been edited and translated into English for supplementary publication.
# ==============================================================================

# 1. Package setup
paquetes <- c("readxl","dplyr","tidyr","ggplot2","lme4",
              "emmeans","knitr")

faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  stop(
    paste0(
      "Instale los siguientes paquetes antes de tejer el documento: ",
      paste(faltantes, collapse = ", ")
    )
  )
}

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(emmeans)
library(knitr)

# ------------------------------------------------------------------------------
# 2. Data import and validation
datos_raw <- read_excel("dataset.xlsx")

variables_requeridas <- c(
  "ID", "SEDE", "SEMESTRE", "NOTAPRE", "NOTAPOST",
  paste0("P", 1:5, ".PRE"),
  paste0("P", 1:5, ".POST")
)

faltan_variables <- setdiff(variables_requeridas, names(datos_raw))

if (length(faltan_variables) > 0) {
  stop(
    paste(
      "Faltan variables requeridas:",
      paste(faltan_variables, collapse = ", ")
    )
  )
}

datos <- datos_raw %>%
  mutate(
    ID = as.character(ID),
    SEMESTRE = as.character(SEMESTRE),
    semestre = recode(
      SEMESTRE,
      "20241" = "2024-I",
      "20242" = "2024-II",
      "20251" = "2025-I",
      "20252" = "2025-II",
      .default = SEMESTRE
    ),
    semestre = factor(
      semestre,
      levels = c("2024-I", "2024-II", "2025-I", "2025-II")
    ),
    ID_ANALISIS = interaction(semestre, ID, drop = TRUE),
    NOTAPRE = as.numeric(NOTAPRE),
    NOTAPOST = as.numeric(NOTAPOST),
    cambio = NOTAPOST - NOTAPRE
  )

columnas_items <- c(
  paste0("P", 1:5, ".PRE"),
  paste0("P", 1:5, ".POST")
)

datos[columnas_items] <- lapply(datos[columnas_items], as.numeric)

# Check that item responses are coded as 0/1.
valores_items <- unlist(datos[columnas_items])

if (any(!is.na(valores_items) & !(valores_items %in% c(0, 1)))) {
  stop("Se encontraron valores diferentes de 0/1 en las respuestas por ítem.")
}

# Check for missing values in the analytical variables.
n_faltantes <- sum(
  is.na(datos[c("NOTAPRE", "NOTAPOST", columnas_items)])
)

if (n_faltantes > 0) {
  warning(
    paste(
      "Se detectaron", n_faltantes,
      "valores faltantes en las variables analíticas."
    )
  )
}

# Check consistency between the total score and the five items:
# each correct response is worth 2 points.
pre_recalculada <- 2 * rowSums(
  as.matrix(datos[paste0("P", 1:5, ".PRE")]),
  na.rm = FALSE
)

post_recalculada <- 2 * rowSums(
  as.matrix(datos[paste0("P", 1:5, ".POST")]),
  na.rm = FALSE
)

if (any(datos$NOTAPRE != pre_recalculada, na.rm = TRUE) ||
    any(datos$NOTAPOST != post_recalculada, na.rm = TRUE)) {
  stop(
    "La nota total no coincide con la suma de los cinco ítems. Revise la base."
  )
}

cat("Número de estudiantes:", nrow(datos), "\n")

cat("Valores faltantes analíticos:", n_faltantes, "\n")

# ------------------------------------------------------------------------------
# 3. Descriptive analysis of PRE–POST performance
# Table 1. Overall and semester-specific performance
resumen_por_semestre <- datos %>%
  group_by(semestre) %>%
  summarise(
    n = n(),
    media_PRE = mean(NOTAPRE, na.rm = TRUE),
    DE_PRE = sd(NOTAPRE, na.rm = TRUE),
    mediana_PRE = median(NOTAPRE, na.rm = TRUE),
    Q1_PRE = quantile(NOTAPRE, 0.25, na.rm = TRUE),
    Q3_PRE = quantile(NOTAPRE, 0.75, na.rm = TRUE),
    media_POST = mean(NOTAPOST, na.rm = TRUE),
    DE_POST = sd(NOTAPOST, na.rm = TRUE),
    mediana_POST = median(NOTAPOST, na.rm = TRUE),
    Q1_POST = quantile(NOTAPOST, 0.25, na.rm = TRUE),
    Q3_POST = quantile(NOTAPOST, 0.75, na.rm = TRUE),
    cambio_medio = mean(cambio, na.rm = TRUE),
    pct_POST_10 = 100 * mean(NOTAPOST == 10, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(grupo = as.character(semestre)) %>%
  select(-semestre)

resumen_total <- datos %>%
  summarise(
    n = n(),
    media_PRE = mean(NOTAPRE, na.rm = TRUE),
    DE_PRE = sd(NOTAPRE, na.rm = TRUE),
    mediana_PRE = median(NOTAPRE, na.rm = TRUE),
    Q1_PRE = quantile(NOTAPRE, 0.25, na.rm = TRUE),
    Q3_PRE = quantile(NOTAPRE, 0.75, na.rm = TRUE),
    media_POST = mean(NOTAPOST, na.rm = TRUE),
    DE_POST = sd(NOTAPOST, na.rm = TRUE),
    mediana_POST = median(NOTAPOST, na.rm = TRUE),
    Q1_POST = quantile(NOTAPOST, 0.25, na.rm = TRUE),
    Q3_POST = quantile(NOTAPOST, 0.75, na.rm = TRUE),
    cambio_medio = mean(cambio, na.rm = TRUE),
    pct_POST_10 = 100 * mean(NOTAPOST == 10, na.rm = TRUE)
  ) %>%
  mutate(grupo = "Total")

tabla1 <- bind_rows(resumen_por_semestre, resumen_total) %>%
  transmute(
    Grupo = grupo,
    n,
    `PRE: media (DE)` = sprintf("%.2f (%.2f)", media_PRE, DE_PRE),
    `PRE: mediana [RIQ]` = sprintf(
      "%.1f [%.1f–%.1f]",
      mediana_PRE, Q1_PRE, Q3_PRE
    ),
    `POST: media (DE)` = sprintf("%.2f (%.2f)", media_POST, DE_POST),
    `POST: mediana [RIQ]` = sprintf(
      "%.1f [%.1f–%.1f]",
      mediana_POST, Q1_POST, Q3_POST
    ),
    `Cambio medio` = sprintf("%.2f", cambio_medio),
    `POST = 10 (%)` = sprintf("%.1f", pct_POST_10)
  )

kable(
  tabla1,
  format = "latex",
  align = c("l", rep("c", ncol(tabla1) - 1)),
  caption = paste(
    "Tabla 1. Desempeño en la nota total antes y después",
    "de la secuencia de laboratorios."
  )
)

# ------------------------------------------------------------------------------
# Figure 1. Individual trajectories

datos_notas_long <- datos %>%
  select(ID_ANALISIS, semestre, NOTAPRE, NOTAPOST) %>%
  pivot_longer(
    cols = c(NOTAPRE, NOTAPOST),
    names_to = "momento",
    values_to = "nota"
  ) %>%
  mutate(
    momento = recode(
      momento,
      NOTAPRE = "PRE",
      NOTAPOST = "POST"
    ),
    momento = factor(momento, levels = c("PRE", "POST"))
  )

ggplot(
  datos_notas_long,
  aes(x = momento, y = nota, group = ID_ANALISIS)
) +
  geom_line(alpha = 0.35) +
  geom_point(size = 2, alpha = 0.75) +
  facet_wrap(~ semestre) +
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, 2)
  ) +
  labs(
    x = "Momento de evaluación",
    y = "Calificación",
    title = "Cambio individual PRE–POST"
  ) +
  theme_minimal(base_size = 12)


# ------------------------------------------------------------------------------
# 4. Overall PRE–POST comparison
# ------------------------------------------------------------------------------

prueba_w <- wilcox.test(
  x = datos$NOTAPOST,
  y = datos$NOTAPRE,
  paired = TRUE,
  alternative = "two.sided",
  exact = FALSE,
  correct = FALSE,
  conf.int = TRUE,
  conf.level = 0.95
)

rank_biserial_pareado <- function(d) {
  d <- d[!is.na(d) & d != 0]

  if (length(d) == 0) {
    return(NA_real_)
  }

  rangos <- rank(abs(d), ties.method = "average")
  W_pos <- sum(rangos[d > 0])
  W_neg <- sum(rangos[d < 0])

  (W_pos - W_neg) / (W_pos + W_neg)
}

r_rb <- rank_biserial_pareado(datos$cambio)

tabla_w <- data.frame(
  `V de Wilcoxon` = unname(prueba_w$statistic),
  `p` = prueba_w$p.value,
  `Pseudomediana del cambio` = unname(prueba_w$estimate),
  `IC95% inferior` = prueba_w$conf.int[1],
  `IC95% superior` = prueba_w$conf.int[2],
  `Correlación biserial por rangos` = r_rb,
  check.names = FALSE
)

kable(
  tabla_w,
  format = "latex",
  digits = 4,
  caption = paste(
    "Tabla 2. Comparación pareada de la nota total",
    "mediante la prueba de Wilcoxon."
  )
)



# ------------------------------------------------------------------------------
# 5. Performance by item and circuit type
# ------------------------------------------------------------------------------
# Se reestructura la base de datos para que luego pueda utilizar
# and the generalized linear mixed model (GLMM).
datos_items <- datos %>%
  select(
    ID_ANALISIS,
    semestre,
    all_of(columnas_items)
  ) %>%
  pivot_longer(
    cols = all_of(columnas_items),
    names_to = c("numero_item", "momento"),
    names_pattern = "P([1-5])\\.(PRE|POST)",
    values_to = "correcta"
  ) %>%
  mutate(
    item = factor(
      paste0("P", numero_item),
      levels = paste0("P", 1:5)
    ),
    momento = factor(
      momento,
      levels = c("PRE", "POST")
    ),
    circuito = case_when(
      item %in% c("P1", "P2") ~ "RC",
      item %in% c("P3", "P4") ~ "RL",
      item == "P5" ~ "RLC"
    ),
    circuito = factor(
      circuito,
      levels = c("RC", "RL", "RLC")
    ),
    correcta = as.integer(correcta)
  )

stopifnot(nrow(datos_items) == nrow(datos) * 10)


# ------------------------------------------------------------------------------
# Table 3. Correct responses by item

tabla3_base <- datos_items %>%
  group_by(item, circuito, momento) %>%
  summarise(
    n = n(),
    aciertos = sum(correcta, na.rm = TRUE),
    porcentaje = 100 * mean(correcta, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(item, circuito, momento, aciertos, porcentaje) %>%
  pivot_wider(
    names_from = momento,
    values_from = c(aciertos, porcentaje),
    names_sep = "_"
  ) %>%
  mutate(
    cambio_pp = porcentaje_POST - porcentaje_PRE
  )

tabla3 <- tabla3_base %>%
  transmute(
    Ítem = as.character(item),
    Circuito = as.character(circuito),
    `PRE: correctas n (%)` = sprintf(
      "%d (%.1f)",
      aciertos_PRE, porcentaje_PRE
    ),
    `POST: correctas n (%)` = sprintf(
      "%d (%.1f)",
      aciertos_POST, porcentaje_POST
    ),
    `Cambio (p.p.)` = sprintf("%+.1f", cambio_pp)
  )

kable(
  tabla3,
  format = "latex",
  align = c("c", "c", "c", "c", "c"),
  caption = paste(
    "Tabla 3. Porcentaje de respuestas correctas",
    "PRE y POST por pregunta."
  )
)


# ------------------------------------------------------------------------------
# 6. Binomial GLMM: primary inferential analysis
# ------------------------------------------------------------------------------
# Fit the models and test the time-by-item interaction.

datos_modelo <- datos_items %>%
  mutate(
    momento = relevel(momento, ref = "PRE"),
    item = relevel(item, ref = "P1"),
    semestre = relevel(semestre, ref = "2024-I"),
    ID_ANALISIS = factor(ID_ANALISIS)
  )

control_glmer <- glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 200000)
)

modelo_aditivo <- glmer(
  correcta ~ momento + item + semestre + (1 | ID_ANALISIS),
  data = datos_modelo,
  family = binomial(link = "logit"),
  control = control_glmer
)

modelo_interaccion <- glmer(
  correcta ~ momento * item + semestre + (1 | ID_ANALISIS),
  data = datos_modelo,
  family = binomial(link = "logit"),
  control = control_glmer
)

lrt <- anova(
  modelo_aditivo,
  modelo_interaccion,
  test = "Chisq"
)

chi_interaccion <- lrt$Chisq[2]
gl_interaccion <- lrt$`Chi Df`[2]
p_interaccion <- lrt$`Pr(>Chisq)`[2]

# Explicit selection of the final model.
if (!is.na(p_interaccion) && p_interaccion < 0.05) {
  modelo_final <- modelo_interaccion
  nombre_modelo_final <- "Modelo con interacción momento × ítem"
} else {
  modelo_final <- modelo_aditivo
  nombre_modelo_final <- "Modelo aditivo"
}

# Diagnostics for the selected final model.
singular <- isSingular(modelo_final, tol = 1e-4)
mensajes_conv <- modelo_final@optinfo$conv$lme4$messages


# ------------------------------------------------------------------------------
# Table 4. Model comparison

tabla_modelos <- data.frame(
  Modelo = c(
    "Aditivo",
    "Momento × ítem"
  ),
  Parámetros = c(
    attr(logLik(modelo_aditivo), "df"),
    attr(logLik(modelo_interaccion), "df")
  ),
  logLik = c(
    as.numeric(logLik(modelo_aditivo)),
    as.numeric(logLik(modelo_interaccion))
  ),
  AIC = c(
    AIC(modelo_aditivo),
    AIC(modelo_interaccion)
  ),
  BIC = c(
    BIC(modelo_aditivo),
    BIC(modelo_interaccion)
  )
)

kable(
  tabla_modelos,
  format = "latex",
  digits = 2,
  align = c("l", "c", "c", "c", "c"),
  caption = "Tabla 4. Comparación del ajuste de los modelos logísticos mixtos."
)



cat(
  "\nPrueba de razón de verosimilitudes: χ²(",
  gl_interaccion,
  ") = ",
  round(chi_interaccion, 2),
  ", p = ",
  format.pval(p_interaccion, digits = 3, eps = 0.001),
  ".\n",
  sep = ""
)




# ------------------------------------------------------------------------------
# Table 5. Estimated parameters of the final model

coef_modelo <- as.data.frame(
  coef(summary(modelo_final))
)

coef_modelo$Termino <- rownames(coef_modelo)
rownames(coef_modelo) <- NULL

tabla_coeficientes <- coef_modelo %>%
  transmute(
    Término = Termino,
    Beta = Estimate,
    EE = `Std. Error`,
    z = `z value`,
    p = `Pr(>|z|)`,
    OR = exp(Estimate),
    `LI95% OR` = exp(Estimate - 1.96 * `Std. Error`),
    `LS95% OR` = exp(Estimate + 1.96 * `Std. Error`)
  )

kable(
  tabla_coeficientes,
  digits = 3,
  format = "latex",
  align = c("l", rep("c", 7)),
  caption = paste0(
    "Tabla 5. Coeficientes estimados del ",
    tolower(nombre_modelo_final),
    "."
  )
)



# ------------------------------------------------------------------------------
# Table 6. Adjusted probabilities and PRE–POST contrasts by item
emm_prob <- emmeans(
  modelo_final,
  ~ momento | item,
  type = "response",
  weights = "proportional"
)

prob_df <- as.data.frame(
  summary(
    emm_prob,
    infer = c(TRUE, TRUE)
  )
)

emm_link <- emmeans(
  modelo_final,
  ~ momento | item,
  weights = "proportional"
)

contrastes <- contrast(
  emm_link,
  method = "revpairwise",
  adjust = "none"
)

contr_df <- as.data.frame(
  summary(
    contrastes,
    type = "response",
    infer = c(TRUE, TRUE),
    adjust = "none"
  )
) %>%
  mutate(
    p_sin_ajuste = p.value,
    p.value = p.adjust(p.value, method = "holm")
  )

# Apply the Holm adjustment jointly to the five PRE–POST contrasts,
# one per item, rather than separately within each emmeans group.

# Standardize column names returned by emmeans.
if ("odds.ratio" %in% names(contr_df)) {
  contr_df <- contr_df %>%
    rename(
      OR = odds.ratio,
      OR_LI95 = asymp.LCL,
      OR_LS95 = asymp.UCL
    )
} else {
  contr_df <- contr_df %>%
    mutate(
      OR = exp(estimate),
      OR_LI95 = exp(asymp.LCL),
      OR_LS95 = exp(asymp.UCL)
    )
}

prob_wide <- prob_df %>%
  select(
    item,
    momento,
    prob,
    asymp.LCL,
    asymp.UCL
  ) %>%
  pivot_wider(
    names_from = momento,
    values_from = c(prob, asymp.LCL, asymp.UCL),
    names_sep = "_"
  ) %>%
  mutate(
    cambio_prob_pp = 100 * (prob_POST - prob_PRE)
  )

tabla6_base <- prob_wide %>%
  left_join(
    contr_df %>%
      select(
        item,
        OR,
        OR_LI95,
        OR_LS95,
        p.value
      ),
    by = "item"
  ) %>%
  mutate(
    circuito = case_when(
      item %in% c("P1", "P2") ~ "RC",
      item %in% c("P3", "P4") ~ "RL",
      item == "P5" ~ "RLC"
    )
  )

tabla6 <- tabla6_base %>%
  transmute(
    Ítem = as.character(item),
    Circuito = circuito,
    `P(correcta) PRE [IC95%]` = sprintf(
      "%.3f [%.3f–%.3f]",
      prob_PRE, asymp.LCL_PRE, asymp.UCL_PRE
    ),
    `P(correcta) POST [IC95%]` = sprintf(
      "%.3f [%.3f–%.3f]",
      prob_POST, asymp.LCL_POST, asymp.UCL_POST
    ),
    `Cambio ajustado (p.p.)` = sprintf(
      "%+.1f",
      cambio_prob_pp
    ),
    `OR POST/PRE [IC95%]` = sprintf(
      "%.2f [%.2f–%.2f]",
      OR, OR_LI95, OR_LS95
    ),
    `p ajustada (Holm)` = format.pval(
      p.value,
      digits = 3,
      eps = 0.001
    )
  )

kable(
  tabla6,
  format = "latex",
  align = c("c", "c", "c", "c", "c", "c", "c"),
  caption = paste(
    "Tabla 6. Probabilidades estimadas y contrastes PRE–POST",
    "por ítem derivados del modelo GLMM seleccionado."
  )
)



# ------------------------------------------------------------------------------
# Figure 2. Adjusted PRE–POST probabilities by item

ggplot(
  prob_df,
  aes(
    x = item,
    y = prob,
    group = momento,
    shape = momento
  )
) +
  geom_line(position = position_dodge(width = 0.12)) +
  geom_point(
    size = 3,
    position = position_dodge(width = 0.12)
  ) +
  geom_errorbar(
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL
    ),
    width = 0.08,
    position = position_dodge(width = 0.12)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = function(x) paste0(round(100 * x), "%")
  ) +
  labs(
    x = "Ítem",
    y = "Probabilidad estimada de respuesta correcta",
    shape = "Momento",
    title = "Probabilidades ajustadas derivadas del GLMM seleccionado"
  ) +
  theme_minimal(base_size = 12)



# ------------------------------------------------------------------------------
# 7. Model diagnostics
# ------------------------------------------------------------------------------
# 7.1 Convergence
summary(modelo_interaccion)
modelo_interaccion@optinfo$conv$lme4$messages

# 7.2 Singularity
isSingular(modelo_interaccion, tol = 1e-4)

# 7.3 Simulated-residual diagnostics
library(DHARMa)

residuos_sim <- simulateResiduals(
  fittedModel = modelo_interaccion,
  n = 1000
)

plot(residuos_sim)

# 7.4 Residual uniformity
testUniformity(residuos_sim)

# 7.5 Dispersion
testDispersion(residuos_sim)

# 7.6 Outliers
testOutliers(residuos_sim)




# ------------------------------------------------------------------------------
# 8. Separation/quasi-separation diagnostics
# ------------------------------------------------------------------------------
# Separation is assessed using the fixed-effects structure
# of the selected GLMM. This diagnostic does not replace the mixed model.

if (!requireNamespace("detectseparation", quietly = TRUE)) {
  stop(
    "Instale el paquete 'detectseparation' con: install.packages('detectseparation')"
  )
}

modelo_separacion <- glm(
  correcta ~ momento * item + semestre,
  data = datos_modelo,
  family = binomial(link = "logit"),
  method = detectseparation::detect_separation
)

modelo_separacion
