# 01_load_data.R
# Dati Eurostat sulle regioni NUTS-2, anno 2021.
# 2021 resta l'anno più recente con tutti gli indicatori scelti disponibili.
# Classe di sviluppo costruita dal PIL pro capite in PPS:
#   - meno_sviluppata: PIL < 75% della media UE
#   - transizione: tra 75% e 100%
#   - piu_sviluppata: sopra il 100%
#
# Il PIL resta fuori dai predittori: serve solo a definire la risposta.
# Predittori: disoccupazione, istruzione, R&S, high-tech e banda larga.
# Fonte: Eurostat, tramite pacchetto eurostat.

library(eurostat)
library(dplyr)

set.seed(2026)
analysis_year <- 2021
dir.create("data",   showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

# Legenda dei dataset Eurostat usati.
# Ogni codice rimanda a una tabella precisa sul sito Eurostat.
eurostat_sources <- data.frame(
  code = c("tgs00006", "tgs00010", "tgs00109",
           "tgs00042", "tgs00039", "isoc_r_broad_h"),
  variable = c("gdp_pps_pct", "unemp_rate", "educ_tertiary",
               "rnd_gdp_pct", "hitech_emp_pct", "broadband_pct"),
  meaning = c("PIL pro capite in PPS, percentuale della media UE",
              "tasso di disoccupazione",
              "popolazione 25-64 con istruzione terziaria",
              "spesa in ricerca e sviluppo in percentuale del PIL",
              "occupazione nei settori high-tech, percentuale degli occupati",
              "famiglie con accesso a banda larga"),
  role = c("costruzione della classe", rep("predittore", 5))
)

code_for <- function(variable) eurostat_sources$code[eurostat_sources$variable == variable]

clean_region_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- sub("/.*$", "", x)
  trimws(x)
}

# Paesi UE considerati. Eurostat usa EL per la Grecia.
eu_countries <- c(
  AT = "Austria", BE = "Belgio", BG = "Bulgaria", CY = "Cipro",
  CZ = "Cechia", DE = "Germania", DK = "Danimarca", EE = "Estonia",
  EL = "Grecia", ES = "Spagna", FI = "Finlandia", FR = "Francia",
  HR = "Croazia", HU = "Ungheria", IE = "Irlanda", IT = "Italia",
  LT = "Lituania", LU = "Lussemburgo", LV = "Lettonia", MT = "Malta",
  NL = "Paesi Bassi", PL = "Polonia", PT = "Portogallo", RO = "Romania",
  SE = "Svezia", SI = "Slovenia", SK = "Slovacchia"
)

# fetch per gli indicatori predittori
fetch <- function(code, value_col, year = analysis_year) {
  message("Fetching ", code, "...")
  df <- tryCatch(
    get_eurostat(code, time_format = "num", cache = TRUE) %>%
      filter(TIME_PERIOD == year, nchar(geo) == 4) %>%   # livello NUTS-2
      group_by(geo) %>%
      summarise(!!value_col := mean(values, na.rm = TRUE), .groups = "drop"),
    error = function(e) { message("  ✗ ", e$message); NULL }
  )
  if (!is.null(df)) message("  → ", nrow(df), " regions")
  df
}

# PIL in PPS: base per definire la classe, non per stimare il modello.
gdp_raw <- tryCatch(
  get_eurostat(code_for("gdp_pps_pct"), time_format = "num", cache = TRUE),
  error = function(e) stop("Cannot fetch GDP data: ", e$message)
)
gdp <- gdp_raw %>%
  filter(TIME_PERIOD == analysis_year, nchar(geo) == 4) %>%
  group_by(geo) %>%
  summarise(gdp_pps_pct = mean(values, na.rm = TRUE), .groups = "drop")
message("GDP: ", nrow(gdp), " NUTS-2 regions")

# Indicatori descrittivi delle regioni.
unemp  <- fetch(code_for("unemp_rate"),      "unemp_rate")
educ   <- fetch(code_for("educ_tertiary"),   "educ_tertiary")
rnd    <- fetch(code_for("rnd_gdp_pct"),     "rnd_gdp_pct")
hitech <- fetch(code_for("hitech_emp_pct"),  "hitech_emp_pct")
broad  <- fetch(code_for("broadband_pct"),   "broadband_pct")

# Unione delle tabelle tramite codice regione.
datasets <- Filter(Negate(is.null), list(gdp, unemp, educ, rnd, hitech, broad))
regions  <- Reduce(function(a, b) inner_join(a, b, by = "geo"), datasets)

# Solo paesi UE; il codice paese viene ricavato dal codice NUTS.
regions <- regions %>%
  mutate(country = substr(geo, 1, 2)) %>%
  filter(
    country %in% names(eu_countries),
    !is.na(gdp_pps_pct)
  )

# Nome esteso della regione dal dizionario Eurostat.
# La normalizzazione evita caratteri strani nei CSV e, nei nomi multilingue,
# tiene la prima forma riportata da Eurostat.
geo_dic <- tryCatch(get_eurostat_dic("geo"), error = function(e) NULL)

if (!is.null(geo_dic)) {
  regions <- regions %>%
    left_join(geo_dic, by = c("geo" = "code_name")) %>%
    rename(region = full_name) %>%
    mutate(region = clean_region_name(region))
} else {
  regions <- regions %>% mutate(region = geo)
}

# PIL relativo trasformato in tre classi ordinate.
regions <- regions %>%
  mutate(
    dev_class = case_when(
      gdp_pps_pct <  75  ~ "meno_sviluppata",
      gdp_pps_pct <= 100 ~ "transizione",
      TRUE               ~ "piu_sviluppata"
    ),
    dev_class = factor(dev_class,
                       levels = c("meno_sviluppata","transizione","piu_sviluppata"))
  )

# Variabili passate ai classificatori.
# unemp_rate      = tasso di disoccupazione
# educ_tertiary   = istruzione terziaria nella popolazione adulta
# rnd_gdp_pct     = spesa in R&S in percentuale del PIL
# hitech_emp_pct  = occupazione high-tech sul totale degli occupati
# broadband_pct   = famiglie con accesso a banda larga
#
# PIL escluso dai dati finali 
predictors <- c("unemp_rate","educ_tertiary","rnd_gdp_pct","hitech_emp_pct","broadband_pct")
aq <- regions %>% select(geo, country, region, dev_class, all_of(predictors))

# Fuori le regioni con troppi dati mancanti.
aq <- aq %>% filter(rowSums(is.na(select(., all_of(predictors)))) <= 1)

# Valori mancanti residui sostituiti con la mediana della variabile.
for (col in predictors) {
  med <- median(aq[[col]], na.rm = TRUE)
  aq[[col]][is.na(aq[[col]])] <- med
}

stopifnot(nrow(aq) >= 100, !anyNA(aq))

write.csv(aq, "data/regions_raw.csv",   row.names = FALSE)
write.csv(regions, "data/regions_full.csv", row.names = FALSE)

# Esempio pratico: le righe italiane corrispondono alle regioni NUTS-2.
italy_nuts2 <- aq %>%
  filter(country == "IT") %>%
  select(geo, region, dev_class) %>%
  arrange(geo)

write.csv(italy_nuts2, "output/italy_nuts2_example.csv", row.names = FALSE)

cat("\nEsempio: regioni italiane nel dataset (livello NUTS-2)\n")
print(as.data.frame(italy_nuts2), row.names = FALSE)

# Divisione finale: training set e test set.
n         <- nrow(aq)
train_idx <- sample(seq_len(n), size = floor(0.75 * n))
train     <- aq[train_idx, ]
test      <- aq[-train_idx, ]

write.csv(train, "data/regions_train.csv", row.names = FALSE)
write.csv(test,  "data/regions_test.csv",  row.names = FALSE)

cat(sprintf("\nDataset: %d regioni NUTS-2  (%d train / %d test)\n", n, nrow(train), nrow(test)))
cat("Distribuzione classi:\n"); print(table(aq$dev_class))
cat("\nPaesi presenti:", length(unique(aq$country)), "\n")
cat("Predittori:", paste(predictors, collapse=", "), "\n")
