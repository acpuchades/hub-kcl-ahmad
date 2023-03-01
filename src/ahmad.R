library(readxl)
library(dplyr)

ahmad_data_path <- "data/Ahmad - King´s college.xlsx"

ahmad_data <- read_excel(ahmad_data_path) %>%
    rename(
        patient = "Paciente",
        birthdate = "Data Nac",
        extraction_date = "Data obtención",
        phenotype = "Fenotipo",
        onset_date = "Data onset",
        onset_age = "Age at onset",
        gender = "Sexo",
        familial = "Familiar",
        c9_status = "C9orf72",
        other_genes = "Other genes",
        weight = "Peso (Kg)",
        height = "Altura (cm)",
        bmi = "IMC",
        riluzole_start = "Started Riluzol",
        diagnosis_date = "Fecha dtco",
        niv_date = "Fecha VMNI",
        peg_date = "Fecha Gast",
        exitus_date = "Fecha Exitus",
        cognitive_status = "Cognitivo",
        last_progression_rate = "ALSFRS slope at last visit (points/month)",
        last_progression_category = "Progression rate (0,8-1,35)...21",
        first_progression_rate = "ALSFRS slope at first visit (points/month)",
        first_progression_category = "Progression rate (0,8-1,35)...23",
        firstyear_progression_rate = "ALSFRS slope 1st year-FU (points/month)",
        firstyear_progression_category = "Progression rate (0,8-1,35) 1st year FU"
    )
