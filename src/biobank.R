library(readxl)
library(dplyr)
library(stringr)

biobank_data_path <- "data/Biobanco.xlsx"

biobank_data <- read_excel(biobank_data_path) %>%
    rename(
        biobank_id = "ID",
        full_name = "Nombre",
        nhc = "SAP",
        diagnosis = "Dtco",
        extraction_date = "Fecha muestra",
        plasma_bht = "Plasma-BHT (PB)",
        serum = "Serum (Red/S)",
        serum_bht = "Serum-BHT (SB)",
        csf = "LCR (L)",
        csf_bht = "LCR-BHT (B)",
        plasma_edta = "Plasma (EDTA/E)",
        csf_pellet = "Pellet LCR",
        edta = "EDTA",
        paxgene = "Paxgene (PX)",
        csf_paxgene = "Paxgene-LCR",
        urine = "Orina",
        feces = "Heces",
        informed_consent = "CI"
    ) %>%
    mutate(
        dni = str_extract(nhc, "DNI (\\d+[A-Z]?)", group = 1),
        nhc = suppressWarnings(as.integer(nhc)),
        extraction_date = as.Date(extraction_date)
    )
