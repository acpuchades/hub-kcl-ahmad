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

data <-
    select(ahmad_data, patient) %>%
    left_join(biobank_patients, by = c(patient = "biobank_id"), multiple = "all") %>%
    left_join(ufmn_clinical, by = "pid") %>%
    left_join(patient_measures, by = "pid") %>%
    left_join(patient_respdata, by = "pid") %>%
    left_join(patient_nutrdata, by = "pid") %>%
    left_join(patient_alsfrs, by = "pid") %>%
    transmute(
        patient = patient,
        birthdate = fecha_nacimiento,
        extraction_date = extraction_date,
        phenotype = diagnosis,
        onset_date = fecha_inicio_clinica,
        onset_age = (onset_date - birthdate) / dyears(1),
        gender = sexo,
        familial = NA,
        c9_status = NA,
        other_genes = NA,
        weight = peso,
        height = estatura,
        bmi = imc,
        riluzole_start = fecha_inicio_riluzol,
        diagnosis_date = fecha_diagnostico,
        niv_date = inicio_vmni,
        peg_date = colocacion_peg,
        exitus_date = fecha_exitus,
        cognitive_status = NA,
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
    )

write_xlsx(data, "output.xlsx")
