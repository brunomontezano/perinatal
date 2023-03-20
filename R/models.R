set.seed(1)
perinatal_data <- readr::read_rds("data-processed/perinatal_data.rds")

perinatal_data

# Counfounding variables:
# sexonovo
# aidadmae
# aescmae
# aparidad
# arenfam

# Verificar codificação segundo gDocs da Vanessa
proc_data <- perinatal_data |>
  dplyr::mutate(
    ainduz = as.factor(dplyr::case_when(ainduz %in% c("rompeu bolsa", "soro", "ambos") ~ "Sim",
                                        ainduz == "nao" ~ "Nao")),
    aprobrn = as.factor(dplyr::case_when(
      aprobrn %in% c("bercario", "aloj.conjunto") ~ "Nao",
      aprobrn == "uti" ~ "Sim"
    )),
    aerrado = as.factor(dplyr::case_when(
      aerrado == "prematuro" ~ "Sim",
      .default = "Nao"
    )),
    stress_resp = as.factor(dplyr::case_when(
      aprobrn1 == "stress resp" | aprobrn2 == "stress resp" | aprobrn3 == "stress resp" ~ "Sim",
      .default = "Nao"
    )),
    traumatismo = as.factor(dplyr::case_when(
      aprobrn1 == "traumatismo" | aprobrn2 == "traumatismo" | aprobrn3 == "traumatismo" ~ "Sim",
      .default = "Nao"
    )),
    malformacao = as.factor(dplyr::case_when(
      aprobrn1 == "malformacao" | aprobrn2 == "malformacao" | aprobrn3 == "malformacao" ~ "Sim",
      .default = "Nao"
    )),
    sofrim_fetal = as.factor(dplyr::case_when(
      aprobrn1 == "sofrim fetal" | aprobrn2 == "sofrim fetal" | aprobrn3 == "sofrim fetal" ~ "Sim",
      .default = "Nao"
    )),
    ahipert = as.factor(dplyr::case_when(
      ahipert %in% c("tratado", "nao tratado") ~ "Sim",
      .default = "Nao"
    )),
    adiabet = as.factor(dplyr::case_when(
      adiabet %in% c("tratado", "nao tratado") ~ "Sim",
      .default = "Nao"
    )),
    aoutinf = as.factor(dplyr::case_when(
      aoutinf %in% c("tratado", "nao tratado") ~ "Sim",
      .default = "Nao"
    )),
    aanemia = as.factor(dplyr::case_when(
      aanemia %in% c("tratado", "nao tratado") ~ "Sim",
      .default = "Nao"
    )),
    sangramento_parto = as.factor(dplyr::case_when(
      amotcesa == "hemorragia materna" | apartind == "sangramento" ~ "Sim",
      .default = "Nao"
    )),
    acompfum = factor(dplyr::case_when(
      acompfum == "nsa" ~ NA,
      .default = acompfum
    ), levels = c("nao", "sim")),
    aalcool = as.factor(dplyr::case_when(
      aalcool == "3" ~ "nao",
      .default = aalcool
    )),
    apesorn = as.factor(dplyr::case_when(
      apesorn < 2500 ~ "Sim",
      apesorn >= 2500 ~ "Nao"
    )),
    acompr = as.factor(dplyr::case_when(
      acompr < 47 ~ "Sim",
      acompr >= 47 ~ "Nao"
    )),
    apcrn = as.factor(dplyr::case_when(
      apcrn < 33 ~ "Sim",
      apcrn >= 33 ~ "Nao"
    )),
    aparidad = as.factor(dplyr::case_when(
      aparidad == "4 ou mais" ~ "Sim",
      aparidad %in% c("0", "1", "2", "3") ~ "Nao"
    )),
    aidadmae_20 = as.factor(dplyr::case_when(
      aidadmae < 20 ~ "Sim",
      aidadmae >= 20 ~ "Nao"
    )),
    aidadmae_34 = as.factor(dplyr::case_when(
      aidadmae > 34 ~ "Sim",
      aidadmae <= 34 ~ "Nao"
    )),
    aapgar1 = as.factor(dplyr::case_when(
      aapgar1 < 7 ~ "Sim",
      aapgar1 >= 7 ~ "Nao"
    )),
    aapgar5 = as.factor(dplyr::case_when(
      aapgar5 < 7 ~ "Sim",
      aapgar5 >= 7 ~ "Nao"
    ))
  ) |>
  dplyr::select(-c(aprobrn1, aprobrn2, aprobrn3, apartind, amotcesa, aidadmae, aidadpai,
                   aescpai))

# Rodar modelos brutos
dummy_data <- proc_data |>
  fastDummies::dummy_cols(
    remove_selected_columns = TRUE,
    remove_first_dummy = TRUE,
    ignore_na = TRUE
  )

models <- dummy_data |>
  tidyr::pivot_longer(
    cols = c(
      dplyr::everything(),-sexonovo_feminino,-aidadmae_20_Sim, -aidadmae_34_Sim,-aescmae,-dplyr::starts_with("aparidad"),-arenfam,-outcome_Sim
    ),
    names_to = "predictor",
    values_to = "value"
  ) |>
  dplyr::nest_by(predictor) |>
  dplyr::mutate(
    mod = list(glm(
    outcome_Sim ~ value, data = data, family = binomial
  )),
    mod_adjusted = list(glm(
      outcome_Sim ~ value + sexonovo_feminino + aidadmae_20_Sim + aidadmae_34_Sim + aescmae +
        aparidad_Sim + arenfam,
      data = data,
      family = binomial
    )))

models

estimates_unadjusted <- models |>
  dplyr::reframe(broom::tidy(mod)) |>
  dplyr::filter(term == "value")

estimates_unadjusted |>
  dplyr::filter(p.value < 0.05) |>
  dplyr::mutate(or = exp(estimate))

significativas_na_bruta <-
  estimates_unadjusted |>
  dplyr::filter(p.value < 0.05) |>
  dplyr::pull(predictor)

models |>
  dplyr::reframe(broom::tidy(mod_adjusted)) |>
  dplyr::filter(predictor %in% c(significativas_na_bruta) & term == "value") |>
  dplyr::mutate(or = exp(estimate))
