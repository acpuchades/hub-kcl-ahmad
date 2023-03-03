library(lubridate)
library(writexl)

source("src/ahmad.R")
source("src/biobank.R")
source("src/ufmn.R")

alsfrs_progression_category <- function(x) {
    case_when(
        x < 0.8 ~ "SP",
        x >= 0.8 & x <= 1.35 ~ "NP",
        x > 1.35 ~ "FP"
    )
}

patients_by_nhc <- biobank_data %>%
    inner_join(ufmn_patients, by = "nhc") %>%
    mutate(dni = coalesce(dni.x, dni.y)) %>%
    select(-c(dni.x, dni.y))

patients_by_dni <- biobank_data %>%
    inner_join(ufmn_patients, by = "dni") %>%
    mutate(nhc = coalesce(nhc.x, nhc.y)) %>%
    select(-c(nhc.x, nhc.y))

biobank_patients <- bind_rows(patients_by_nhc, patients_by_dni)

patient_measures <- ufmn_nutrition %>%
    group_by(pid) %>%
    slice_min(fecha_visita, n = 1) %>%
    ungroup() %>%
    select(pid, peso, estatura) %>%
    mutate(imc = peso / (estatura / 100)^2)

patient_respdata <- ufmn_respiratory %>%
    group_by(pid) %>%
    summarize(
        inicio_vmni = suppressWarnings(
            min(fecha_colocacion_vmni, na.rm = TRUE)
        ) %>% (\(x) if_else(!is.infinite(x), x, NA_Date_))
    )

patient_nutrdata <- ufmn_nutrition %>%
    group_by(pid) %>%
    summarize(
        colocacion_peg = suppressWarnings(
            min(fecha_colocacion_peg, na.rm = TRUE)
        ) %>% (\(x) if_else(!is.infinite(x), x, NA_Date_))
    )

patient_alsfrs <- ufmn_functional %>%
    group_by(pid) %>%
    arrange(fecha_visita, .by_group = TRUE) %>%
    summarize(
        first_visit = first(fecha_visita),
        first_alsfrs = first(alsfrs_total),
        last_visit = last(fecha_visita),
        last_alsfrs = last(alsfrs_total)
    )

patient_alsfrs_y1 <- ufmn_functional %>%
    group_by(pid) %>%
    mutate(
        y1_diff = (min(fecha_visita) + years(1)) - fecha_visita,
    ) %>%
    slice_min(y1_diff, n = 1) %>%
    ungroup() %>%
    filter(between(y1_diff, -dmonths(2), dmonths(4))) %>%
    select(pid, y1_visit = "fecha_visita", y1_alsfrs = "alsfrs_total")

data <-
    select(ahmad_data, patient, familial, c9_status, other_genes) %>%
    left_join(biobank_patients, by = c(patient = "biobank_id"), multiple = "first") %>%
    left_join(ufmn_clinical, by = "pid") %>%
    left_join(patient_measures, by = "pid") %>%
    left_join(patient_respdata, by = "pid") %>%
    left_join(patient_nutrdata, by = "pid") %>%
    left_join(patient_alsfrs, by = "pid") %>%
    left_join(patient_alsfrs_y1, by = "pid") %>%
    transmute(
        patient_id = patient,
        gender = sexo,
        birthdate = fecha_nacimiento,
        sample_date = extraction_date,
        familial = familial,
        c9_status = c9_status,
        other_genes = other_genes,
        phenotype = diagnosis,
        baseline_weight = peso,
        baseline_height = estatura,
        baseline_bmi = imc,
        cognitive_status = case_match(
            resultado_estudio_cognitivo,
            "Normal" ~ "Normal",
            "DCL-Cognitivo" ~ "Ci",
            "DCL-Conductual" ~ "Bi",
            "DCL-Mixto" ~ "CiBi",
            "DTA" ~ "DTA",
            "DFT" ~ "DFT"
        ),
        onset_date = fecha_inicio_clinica,
        onset_age = (onset_date - birthdate) / dyears(1),
        diagnosis_date = fecha_diagnostico,
        riluzole_start = fecha_inicio_riluzol,
        niv_date = inicio_vmni,
        peg_date = colocacion_peg,
        first_visit = first_visit,
        y1_visit = y1_visit,
        last_visit = last_visit,
        exitus_date = fecha_exitus,
        last_progression_rate = case_when(
            exitus == TRUE ~
                48 / ((fecha_exitus - fecha_inicio_clinica) / dmonths(1)),
            exitus == FALSE | is.na(exitus) ~
                (48 - last_alsfrs) / ((last_visit - fecha_inicio_clinica) / dmonths(1))
        ),
        last_progression_category = alsfrs_progression_category(last_progression_rate),
        first_progression_rate = case_when(
            (first_visit - fecha_diagnostico) < dmonths(6) ~
                (48 - first_alsfrs) / ((first_visit - fecha_inicio_clinica) / dmonths(1))
        ),
        first_progression_category = alsfrs_progression_category(first_progression_rate),
        y1_progression_rate =
            (first_alsfrs - y1_alsfrs) / ((y1_visit - first_visit) / dmonths(1)),
        y1_progression_category = alsfrs_progression_category(y1_progression_rate),
    )

write_xlsx(data, "output.xlsx")
