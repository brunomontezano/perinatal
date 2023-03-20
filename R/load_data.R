raw_data <- haven::read_sav(
  "data-raw/banco_QI_15_07.sav"
)

inclusion_criteria <- raw_data |>
  dplyr::filter((!is.na(MDD_BD1_2_notmood)) |
                  (!is.na(TB1_18anos)) |
                  (!is.na(TBII.p1_18anos))) |>
  haven::as_factor() |>
  dplyr::mutate(
    tb1_22 = dplyr::if_else(
      MDD_BD1_2_notmood == "Bipolar Disorder 1",
      "Sim", "Não"
    ),
    tb2_22 = dplyr::if_else(
      MDD_BD1_2_notmood == "Bipolar Disorder 2",
      "Sim", "Não"
    ),
    tb1_18 = dplyr::if_else(
      TB1_18anos == 1,
      "Sim", "Não"
    ),
    tb2_18 = dplyr::if_else(
      TBII.p1_18anos == 1,
      "Sim", "Não"
    ),
    outcome_subtypes = dplyr::case_when(
      tb1_22 == "Sim" ~ "TB I aos 22 anos",
      tb1_18 == "Sim" ~ "TB I aos 18 anos",
      tb2_22 == "Sim" ~ "TB II aos 22 anos",
      tb2_18 == "Sim" ~ "TB II aos 18 anos",
      tb1_22 == "Não" |
        tb2_22 == "Não" |
        tb1_18 == "Não" |
        tb2_18 == "Não" ~ "Sem TB",
      TRUE ~ NA_character_
    ),
    outcome = dplyr::case_when(
      tb1_22 == "Sim" |
        tb2_22 == "Sim" |
        tb1_18 == "Sim" |
        tb2_18 == "Sim" ~ "Sim",
      tb1_22 == "Não" |
        tb2_22 == "Não" |
        tb1_18 == "Não" |
        tb2_18 == "Não" ~ "Não",
      TRUE ~ NA_character_
    ) |> factor(levels = c("Não", "Sim"))
  ) |>
  dplyr::filter(!is.na(outcome))

vars_vanessa <- c(
  "nquest",
  "sexonovo",
  "apartip",
  "ainduz",
  "aprobrn",
  "aerrado",
  "aprobrn",
  "aprobrn1",
  "aprobrn2",
  "aprobrn3",
  "aprobrn",
  "aprobrn1",
  "aprobrn2",
  "aprobrn3",
  "aprobrn",
  "aprobrn1",
  "aprobrn2",
  "aprobrn3",
  "aprobrn",
  "aprobrn1",
  "aprobrn2",
  "aprobrn3",
  "ahipert",
  "adiabet",
  "ainfecur",
  "aoutinf",
  "aanemia",
  "amotcesa",
  "apartind",
  "afumou",
  "acompfum",
  "abebalc",
  "aalcool",
  "avivmar",
  "asexrn",
  "apesorn",
  "acompr",
  "apcrn",
  "aescol3",
  "aescpai",
  "aescmae",
  "aparidad",
  "aapoio",
  "aidadmae",
  "aidadpai",
  "apretama",
  "aapgar1",
  "aapgar5",
  "arenfam",
  "ainfecuri"
)

perinatal_data <- inclusion_criteria |>
  dplyr::select(outcome, dplyr::any_of(vars_vanessa))

readr::write_rds(
  x = perinatal_data,
  file = "data-processed/perinatal_data.rds"
)
