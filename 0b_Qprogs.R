library(dplyr)

YYYY <- 2024
YY <- 24
# ── Directory paths ───────────────────────────────────────────────────────────
stork_dir <- paste0('C:/Users/smithac/OneDrive - EC-EC/Harvest Survey A146/',YYYY)

# ── Province code lookup (numeric string → abbreviation) ─────────────────────
prov_lookup <- c(
  "01" = "NF", "02" = "PE", "03" = "NS", "04" = "NB", "05" = "PQ",
  "06" = "ON", "07" = "MB", "08" = "SK", "09" = "AB", "10" = "BC",
  "11" = "NT", "12" = "YT", "13" = "NU"
)

recode_prov <- function(x) {
  x <- trimws(x)
  ifelse(x %in% names(prov_lookup), prov_lookup[x], x)
}

# PRHUNTM maps 02-13 to blank (only NF is retained)
recode_prhuntm <- function(x) {
  x <- trimws(x)
  ifelse(x == "01", "NF",
         ifelse(x %in% sprintf("%02d", 2:13), "", x))
}

# ── Read fixed-width file ─────────────────────────────────────────────────────
col_positions <- list(
  selyear  = c(13, 16),  permit   = c(17, 22),  caste    = c(23, 23),
  potntl   = c(25, 25),  prhunt   = c(36, 37),  daywf    = c(42, 44),
  dayot    = c(45, 47),  toduk    = c(48, 50),  togok    = c(51, 53),
  cootk    = c(57, 59),  woodk    = c(60, 62),  snipk    = c(63, 65),
  dovek    = c(66, 68),  pigek    = c(69, 71),  crank    = c(72, 74),
  railk    = c(75, 77),  murrk    = c(78, 80),  prhuntg  = c(90, 91),
  zohunt   = c(96, 97),  latd     = c(98, 101), lond     = c(102, 106),
  zohuntg  = c(107, 108),latg     = c(109, 112),long    = c(113, 117),
  prsale   = c(133, 134),zosale   = c(135, 136),prsamp   = c(137, 138),
  zosamp   = c(139, 140),daym     = c(148, 150),prhuntm  = c(151, 152),
  zohuntm  = c(153, 154),latm     = c(155, 158),lonm     = c(159, 163)
)

# Build widths vector for read.fwf
# Strategy: read entire line, extract by character position
read_fwf_cols <- function(filepath, col_positions) {
  # Find max column needed
  max_col <- max(sapply(col_positions, `[`, 2))
  
  lines <- readLines(filepath)
  # Pad lines shorter than max_col
  lines <- formatC(lines, width = max_col, flag = "-")
  
  result <- as.data.frame(
    lapply(col_positions, function(pos) {
      trimws(substring(lines, pos[1], pos[2]))
    }),
    stringsAsFactors = FALSE
  )
  result
}

raw <- read_fwf_cols(
  file.path(stork_dir, paste0('HQS.',YYYY)),
  col_positions
)

# ── Convert numeric columns ───────────────────────────────────────────────────
num_cols <- c("selyear", "permit", "daywf", "dayot", "toduk", "togok",
              "cootk", "woodk", "snipk", "dovek", "pigek", "crank",
              "railk", "murrk", "zohunt", "latd", "lond", "zohuntg",
              "latg", "long", "zosale", "zosamp", "daym", "zohuntm",
              "latm", "lonm")

harvYYe <- raw
harvYYe[num_cols] <- lapply(harvYYe[num_cols], function(x) {
  v <- suppressWarnings(as.numeric(x))
  v  # blanks/non-numeric become NA, equivalent to SAS missing
})

# ── Recode province character columns ────────────────────────────────────────
for (col in c("prsamp", "prsale", "prhunt", "prhuntg")) {
  harvYYe[[col]] <- recode_prov(harvYYe[[col]])
}
harvYYe$prhuntm <- recode_prhuntm(harvYYe$prhuntm)

# ── Derived variables ─────────────────────────────────────────────────────────
na0 <- function(x) ifelse(is.na(x), 0, x)  # treat NA as 0

harvYYe <- harvYYe |> mutate(
  across(c(cootk, crank, dovek, railk, pigek, snipk, woodk, murrk),
         na0),
  succwf  = ifelse(na0(toduk) > 0 | na0(togok) > 0, "Y", "N"),
  tootk   = cootk + crank + dovek + railk + pigek + snipk + woodk,
  succm   = ifelse(murrk > 0, "Y", "N"),
  rndmurk = floor(murrk / 2),
  dayot   = ifelse(na0(dayot) == rndmurk, 0, na0(dayot)),
  succot  = ifelse(tootk > 0, "Y", "N"),
  succ    = ifelse(succwf == "Y" | succm == "Y" | succot == "Y", "Y", "N"),
  activewf = ifelse(na0(daywf) > 0 | succwf == "Y", "Y", "N"),
  activem  = ifelse(na0(daym)  > 0 | succm  == "Y", "Y", "N"),
  activeot = ifelse(na0(dayot) > 0 | succot == "Y", "Y", "N"),
  active   = ifelse(activewf == "Y" | activem == "Y" | activeot == "Y",
                    "Y", "N"),
  year     = YYYY,
  toduk    = na0(toduk)
)

# ── Clean up ZOSALE ───────────────────────────────────────────────────────────
harvYYe <- harvYYe |> mutate(
  zosale = case_when(
    # Already has a valid value — keep it
    !is.na(zosale) & zosale != 0 ~ zosale,
    # Caste A or E: use zosamp when provinces match
    caste %in% c("A", "E") & prsale == prsamp ~ zosamp,
    # Caste B or D: cascade through hunting province/zone matches
    caste %in% c("B", "D") &
      prhunt == prsale & !is.na(zohunt) & zohunt != 0  ~ zohunt,
    caste %in% c("B", "D") &
      prhuntg == prsale & !is.na(zohuntg) & zohuntg != 0 ~ zohuntg,
    caste %in% c("B", "D") &
      prhuntm == prsale & !is.na(zohuntm) & zohuntm != 0 ~ zohuntm,
    caste %in% c("B", "D") & prsale == prsamp ~ zosamp,
    TRUE ~ zosale
  )
)

# ── Clean up ZOHUNT ───────────────────────────────────────────────────────────
harvYYe <- harvYYe |> mutate(
  zohunt = case_when(
    !is.na(zohunt) & zohunt != 0 ~ zohunt,
    caste %in% c("A", "E") & prsamp == prhunt ~ zosamp,
    caste %in% c("B", "D") & potntl == "Y" &
      prhunt == prsale & !is.na(zosale) & zosale != 0  ~ zosale,
    caste %in% c("B", "D") & potntl == "Y" &
      prhunt == prsamp ~ zosamp,
    TRUE ~ zohunt
  )
)

# ── Clean up PRHUNTG ─────────────────────────────────────────────────────────
harvYYe <- harvYYe |> mutate(
  prhuntg = ifelse(trimws(prhuntg) == "00", "", prhuntg),
  zohuntg = ifelse(trimws(prhuntg) == "", NA, zohuntg),
  prhuntg = case_when(
    togok > 0 & trimws(prhuntg) == "" & trimws(prhunt)  != "" ~ prhunt,
    togok > 0 & trimws(prhuntg) == "" & trimws(prsale)  != "" ~ prsale,
    togok > 0 & trimws(prhuntg) == "" & trimws(prsamp)  != "" ~ prsamp,
    TRUE ~ prhuntg
  )
)

# ── Clean up ZOHUNTG ─────────────────────────────────────────────────────────
harvYYe <- harvYYe |> mutate(
  zohuntg = case_when(
    (is.na(zohuntg) | zohuntg == 0) & prhuntg == prhunt ~ zohunt,
    TRUE ~ zohuntg
  ),
  zohuntg = case_when(
    togok > 0 & (is.na(zohuntg) | zohuntg == 0) &
      caste %in% c("A","E") & prsamp == prhuntg ~ zosamp,
    togok > 0 & (is.na(zohuntg) | zohuntg == 0) &
      caste %in% c("B","D") & potntl == "Y" &
      prhuntg == prsale & !is.na(zosale) & zosale != 0 ~ zosale,
    togok > 0 & (is.na(zohuntg) | zohuntg == 0) &
      caste %in% c("B","D") & potntl == "Y" &
      prhuntg == prsamp ~ zosamp,
    togok > 0 & (is.na(zohuntg) | zohuntg == 0) &
      caste %in% c("B","D") & potntl == "Y" &
      prhuntg == prhunt ~ zohunt,
    TRUE ~ zohuntg
  )
)

# ── Clean up PRHUNTM ─────────────────────────────────────────────────────────
harvYYe <- harvYYe |> mutate(
  prhuntm = ifelse(trimws(prhuntm) == "00", "", prhuntm),
  zohuntm = ifelse(trimws(prhuntm) == "", NA, zohuntm),
  prhuntm = case_when(
    murrk > 0 & trimws(prhuntm) == "" & trimws(prhunt) != "" ~ prhunt,
    murrk > 0 & trimws(prhuntm) == "" & trimws(prsale) != "" ~ prsale,
    murrk > 0 & trimws(prhuntm) == "" & trimws(prsamp) != "" ~ prsamp,
    TRUE ~ prhuntm
  )
)

# ── Clean up ZOHUNTM ─────────────────────────────────────────────────────────
harvYYe <- harvYYe |> mutate(
  zohuntm = case_when(
    (is.na(zohuntm) | zohuntm == 0) & prhuntm == prhunt ~ zohunt,
    TRUE ~ zohuntm
  ),
  zohuntm = case_when(
    murrk > 0 & (is.na(zohuntm) | zohuntm == 0) &
      caste %in% c("A","E") & prsamp == prhuntm ~ zosamp,
    murrk > 0 & (is.na(zohuntm) | zohuntm == 0) &
      caste %in% c("B","D") & potntl == "Y" &
      prhuntm == prsale & !is.na(zosale) & zosale != 0 ~ zosale,
    murrk > 0 & (is.na(zohuntm) | zohuntm == 0) &
      caste %in% c("B","D") & potntl == "Y" &
      prhuntm == prsamp ~ zosamp,
    murrk > 0 & (is.na(zohuntm) | zohuntm == 0) &
      caste %in% c("B","D") & potntl == "Y" &
      prhuntm == prhunt ~ zohunt,
    TRUE ~ zohuntm
  )
)

# ── Clean up POTNTL ───────────────────────────────────────────────────────────
harvYYe <- harvYYe |> mutate(
  potntl = ifelse(active == "Y" | succ == "Y", "Y", potntl)
)

# ── Row deletions ─────────────────────────────────────────────────────────────
harvYYe <- harvYYe |> filter(
  # Drop zero/missing zosale
  !is.na(zosale) & zosale != 0,
  # Drop active hunters with no hunting location at all
  !(active == "Y" &
      (is.na(zohunt)  | zohunt  == 0) &
      (is.na(zohuntg) | zohuntg == 0) &
      (is.na(zohuntm) | zohuntm == 0)),
  # Drop blank caste
  trimws(caste) != "",
  # Drop missing zosamp
  !is.na(zosamp)
)

# ── Species harvest restrictions by province ──────────────────────────────────
harvYYe <- harvYYe |> mutate(
  # Rail: ON and YT only
  railk = ifelse(prhunt %in% c("ON", "YT"), railk, 0),
  # Crane: SK, MB, AB, YT only
  crank = ifelse(prhunt %in% c("SK", "MB", "AB", "YT"), crank, 0),
  # Coot: NB, NS, PE, NF only
  cootk = ifelse(prhunt %in% c("NB", "NS", "PE", "NF"), cootk, 0),
  # Pigeon: BC only
  pigek = ifelse(prhunt == "BC", pigek, 0),
  # Dove: ON, PQ, BC only
  dovek = ifelse(prhunt %in% c("ON", "PQ", "BC"), dovek, 0),
  # Woodcock: ON, PQ, NB, NS, PE, MB only
  woodk = ifelse(prhunt %in% c("ON", "PQ", "NB", "NS", "PE", "MB"), woodk, 0),
  # Murre: NF only (via prhuntm)
  murrk = ifelse(prhuntm == "NF", murrk, 0)
)

# ── Save output ───────────────────────────────────────────────────────────────
saveRDS(harvYYe, file.path(stork_dir, paste0("HARV",YY,".rds")))

# ── Diagnostic prints: missing locations ─────────────────────────────────────
cat("\nMISSING GOOSE HUNTING LOCATION\n")
miss_goose <- harvYYe |>
  filter(togok > 0 & (trimws(prhuntg) == "" | is.na(zohuntg)))
print(miss_goose[, c("prsamp","zosamp","caste","permit","prsale","zosale",
                     "prhunt","zohunt","togok","prhuntg","zohuntg")])

cat("\nMISSING DUCK HUNTING LOCATION\n")
miss_duck <- harvYYe |>
  filter(toduk > 0 & (trimws(prhunt) == "" | is.na(zohunt)))
print(miss_duck[, c("prsamp","zosamp","caste","permit","prsale","zosale",
                    "toduk","prhunt","zohunt","prhuntg","zohuntg")])

cat("\nMISSING MURRE HUNTING LOCATION\n")
miss_murre <- harvYYe |>
  filter(murrk > 0 & (trimws(prhuntm) == "" | is.na(zohuntm)))
print(miss_murre[, c("prsamp","zosamp","caste","permit","prsale","zosale",
                     "prhunt","zohunt","murrk","prhuntm","zohuntm")])

# ── Frequency tables (equivalent to PROC FREQ) ───────────────────────────────
cat("\nprhunt × zohunt\n");  print(table(harvYYe$prhunt,  harvYYe$zohunt))
cat("\nprhuntg × zohuntg\n"); print(table(harvYYe$prhuntg, harvYYe$zohuntg))
cat("\nprhuntm × zohuntm\n"); print(table(harvYYe$prhuntm, harvYYe$zohuntm))
cat("\nprhuntm × prhunt\n");  print(table(harvYYe$prhuntm, harvYYe$prhunt))
cat("\nprsale × zosale\n");   print(table(harvYYe$prsale,  harvYYe$zosale))
cat("\nprsamp × zosamp\n");   print(table(harvYYe$prsamp,  harvYYe$zosamp))














library(dplyr)

# ── Directory paths ───────────────────────────────────────────────────────────
#stork_dir <- 'C:/Users/smithac/OneDrive - EC-EC/Harvest Survey A146/2024'
heron_dir <- 'C:/Users/smithac/OneDrive - EC-EC/Harvest Survey A146/POPULATIONSIZE'

# ── Load input data ───────────────────────────────────────────────────────────
#harvYYe  <- readRDS(file.path(stork_dir, paste0("HARV",YY,"E.rds")))
popsizYY <- readRDS(file.path(heron_dir, paste0("POPSIZ",YY,".rds")))

# ── Sample sizes: PROC FREQ equivalent ───────────────────────────────────────
# Count records by prsamp × zosamp × caste (equivalent to TABLES with OUT=)
second <- harvYYe |>
  count(prsamp, zosamp, caste, name = "ssize")

# ── Prepare population size file ──────────────────────────────────────────────
# Rename prsamp → prsa, then recode numeric province codes to abbreviations
prov_lookup <- c(
  `1`  = "NF", `2`  = "PE", `3`  = "NS", `4`  = "NB", `5`  = "PQ",
  `6`  = "ON", `7`  = "MB", `8`  = "SK", `9`  = "AB", `10` = "BC",
  `11` = "NT", `12` = "YT", `13` = "NU"
)

f2 <- popsizYY |>
  rename(prsa = prsamp) |>
  mutate(
    prsamp = dplyr::recode(as.character(prsa), !!!prov_lookup, .default = ""),
    caste  = sample,
    zosamp = zosamp
  ) |>
  select(-sample, -prsa) |>
  arrange(prsamp, zosamp, caste)

# ── Merge and calculate simple extrapolation factors ─────────────────────────
# IN1 (left/second) drives the merge — only keep rows present in second
factYY <- second |>
  left_join(f2, by = c("prsamp", "zosamp", "caste")) |>
  mutate(sef = totperm / ssize)

saveRDS(factYY, file.path(stork_dir, paste0("FACT",YY,".rds")))

cat("Extrapolation factors\n")
print(factYY[, c("prsamp", "zosamp", "caste", "totperm", "ssize", "sef")])

cat("\nTotal sample size:\n")
print(sum(factYY$ssize, na.rm = TRUE))

# ── List incorrect sample codes (caste C or F) ───────────────────────────────
cat("\nRecords with invalid caste (C or F):\n")
temp_bad_caste <- harvYYe |>
  filter(caste %in% c("C", "F")) |>
  select(permit, prsamp, zosamp, caste)
print(temp_bad_caste)

# ── List SP-province records with caste E ────────────────────────────────────
# SP provinces are NF, PE, NS, NT, YT, and BC zone 2
cat("\nSP-province records with caste E:\n")
temp_sp_e <- harvYYe |>
  filter(
    (prsamp %in% c("NF", "PE", "NS", "NT", "YT") |
       (prsamp == "BC" & zosamp == 2)) &
      caste == "E"
  ) |>
  select(permit, prsamp, zosamp, caste)
print(temp_sp_e)



library(haven)
library(dplyr)
library(tidyr)

# ------------------------------------- 
# DEFINE LIBRARY PATHS                  
# ------------------------------------- 
#stork_dir <- "C:/Users/smithac/OneDrive - EC-EC/Harvest Survey A146/2024"
heron_path <- "C:/Users/smithac/OneDrive - EC-EC/Harvest Survey A146/PERMITSALES"

# ------------------------------------- 
# CALCULATE SAMPLE SIZE FOR EACH PZSAMP 
# BY PZSALE FOR PREVIOUS YEAR SELECTION 
# WHO ARE POTENTIAL HUNTERS             
# ------------------------------------- 

zero <- harvYYe |>
  filter(zosale != 0, zosamp != 0) |>
  filter(caste %in% c('B', 'D') & potntl == 'Y')

first <- zero |>
  group_by(caste, prsamp, zosamp, prsale, zosale) |>
  summarise(count = n(), .groups = "drop")

second <- first |>
  mutate(smalln = count) |>
  select(prsamp, zosamp, prsale, zosale, caste, smalln) |>
  arrange(prsamp, zosamp, caste, prsale, zosale)

# --------------------------- 
# ESTIMATE NUMBER OF RENEWALS 
# --------------------------- 
# fact24 <- read_sas(file.path(stork_dir, "fact24.sas7bdat"))

third <- second |>
  merge(factYY, by = c("prsamp", "zosamp", "caste"), all.x = TRUE) |>
  mutate(estren = sef * smalln) |>
  arrange(prsale, zosale)

fourth <- third |>
  group_by(prsale, zosale) |>
  summarise(estren = sum(estren, na.rm = TRUE), .groups = "drop")

# --------------------------------- 
# READ IN ACTUAL NUMBER OF RENEWALS 
# --------------------------------- 
persalYY <- readRDS(file.path(heron_path, paste0("PERSAL",YY,".rds")))

ar0 <- persalYY |>
  mutate(caste = sample) |>
  filter(caste == 'B') |>
  rename(psl = prsale)

psl_map <- c(
  "01" = "NF", "02" = "PE", "03" = "NS", "04" = "NB",
  "05" = "PQ", "06" = "ON", "07" = "MB", "08" = "SK",
  "09" = "AB", "10" = "BC", "11" = "NT", "12" = "YT", "13" = "NU"
)

ar1 <- ar0 |>
  mutate(prsale = psl_map[sprintf("%02d", psl)]) |>
  select(-psl) |>
  arrange(prsale, zosale)

# ------------------------------------ 
# MATCH ESTIMATED RENEWALS WITH ACTUAL 
# RENEWALS AND CALCULATE CORRECTION    
# ------------------------------------ 
ratfacYY <- fourth |>
  merge(ar1, by = c("prsale", "zosale"), all = TRUE) |>
  filter(prsale != '00') |>
  mutate(cfact = totsale / estren)

# Save to stork path
saveRDS(ratfacYY, file.path(stork_dir, paste0("ratfac",YY,".rds")))

# Print
ratfacYY |> select(prsale, zosale, estren, totsale, cfact) |> print()

# -------------------------- 
# MERGE INTO A SINGLE RECORD 
# -------------------------- 

ratfacYYd <- ratfacYY |> 
  mutate(caste = "D")

ratfacYYall <- ratfacYY |> 
  bind_rows(ratfacYYd) |> 
  select(prsale,zosale,caste,cfact) |> 
  drop_na()

# # Helper function equivalent to %COLCF macro
# colcf <- function(data, ps, zs, cname) {
#   data |> 
#     filter(prsale == ps, zosale == zs) |>
#     mutate(year = YYYY, !!cname := cfact) |>
#     select(year, all_of(cname))
# }
# 
# ff1 <- list(
#   colcf(ratfacYY, 'NF', 1, "CF011"),
#   colcf(ratfacYY, 'NF', 2, "CF012"),
#   colcf(ratfacYY, 'PE', 1, "CF021"),
#   colcf(ratfacYY, 'NS', 1, "CF031"),
#   colcf(ratfacYY, 'NS', 2, "CF032"),
#   colcf(ratfacYY, 'NB', 1, "CF041"),
#   colcf(ratfacYY, 'NB', 2, "CF042"),
#   colcf(ratfacYY, 'PQ', 1, "CF051"),
#   colcf(ratfacYY, 'PQ', 2, "CF052"),
#   colcf(ratfacYY, 'ON', 1, "CF061"),
#   colcf(ratfacYY, 'ON', 2, "CF062"),
#   colcf(ratfacYY, 'ON', 3, "CF063")
# ) #|>
#   bind_cols()
# 
# # Remove duplicate YEAR columns, keep only first
# ff1 <- ff1[, !duplicated(names(ff1))]
# 
# ff2 <- list(
#   colcf(ratfacYY, 'MB', 1, "CF071"),
#   colcf(ratfacYY, 'MB', 2, "CF072"),
#   colcf(ratfacYY, 'SK', 1, "CF081"),
#   colcf(ratfacYY, 'SK', 2, "CF082"),
#   colcf(ratfacYY, 'SK', 3, "CF083"),
#   colcf(ratfacYY, 'AB', 1, "CF091"),
#   colcf(ratfacYY, 'AB', 2, "CF092"),
#   colcf(ratfacYY, 'BC', 1, "CF101"),
#   colcf(ratfacYY, 'BC', 2, "CF102"),
#   colcf(ratfacYY, 'NT', 1, "CF111"),
#   colcf(ratfacYY, 'YT', 1, "CF121"),
#   colcf(ratfacYY, 'NU', 1, "CF131")
# ) |>
#   bind_cols()
# 
# ff2 <- ff2[, !duplicated(names(ff2))]
# 
# cfactYY <- bind_cols(ff1, ff2)
# cfactYY <- cfactYY[, !duplicated(names(cfactYY))]
# 
# # Save to stork path
# saveRDS(cfactYY, file.path(stork_dir, paste0("cfact",YY,".rds")))
# 
# 
# 
# library(haven)
# library(dplyr)
# 
# # ------------------------------------- 
# # DEFINE LIBRARY PATH                  
# # ------------------------------------- 
# #stork_dir <- "C:/Users/smithac/OneDrive - EC-EC/Harvest Survey A146/2024"
# 
# # Read input datasets
# #harv24e <- read_sas(file.path(stork_dir, "harv24e.sas7bdat"))
# 
# # ------------------------------- 
# # APPEND RATIO CORRECTION TO OBS. 
# # ------------------------------- 

# Merge by YEAR (one-to-one broadcast: cfactYY is a single row)
harvYYw <- harvYYe |>
  left_join(ratfacYYall, by = c("prsale","zosale","caste")) |>
  mutate(cfact = ifelse(is.na(cfact),1,cfact),
         cfact = ifelse(potntl == "N",0,cfact))
  
  # mutate(
  #   CFACT = 1.0000,
  #   CFACT = case_when(
  #     CASTE %in% c('B', 'D') & PRSALE == 'NF' & ZOSALE == 1  ~ CF011,
  #     CASTE %in% c('B', 'D') & PRSALE == 'NF' & ZOSALE == 2  ~ CF012,
  #     CASTE %in% c('B', 'D') & PRSALE == 'PE' & ZOSALE == 1  ~ CF021,
  #     CASTE %in% c('B', 'D') & PRSALE == 'NS' & ZOSALE == 1  ~ CF031,
  #     CASTE %in% c('B', 'D') & PRSALE == 'NS' & ZOSALE == 2  ~ CF032,
  #     CASTE %in% c('B', 'D') & PRSALE == 'NB' & ZOSALE == 1  ~ CF041,
  #     CASTE %in% c('B', 'D') & PRSALE == 'NB' & ZOSALE == 2  ~ CF042,
  #     CASTE %in% c('B', 'D') & PRSALE == 'PQ' & ZOSALE == 1  ~ CF051,
  #     CASTE %in% c('B', 'D') & PRSALE == 'PQ' & ZOSALE == 2  ~ CF052,
  #     CASTE %in% c('B', 'D') & PRSALE == 'ON' & ZOSALE == 1  ~ CF061,
  #     CASTE %in% c('B', 'D') & PRSALE == 'ON' & ZOSALE == 2  ~ CF062,
  #     CASTE %in% c('B', 'D') & PRSALE == 'ON' & ZOSALE == 3  ~ CF063,
  #     CASTE %in% c('B', 'D') & PRSALE == 'MB' & ZOSALE == 1  ~ CF071,
  #     CASTE %in% c('B', 'D') & PRSALE == 'MB' & ZOSALE == 2  ~ CF072,
  #     CASTE %in% c('B', 'D') & PRSALE == 'SK' & ZOSALE == 1  ~ CF081,
  #     CASTE %in% c('B', 'D') & PRSALE == 'SK' & ZOSALE == 2  ~ CF082,
  #     CASTE %in% c('B', 'D') & PRSALE == 'SK' & ZOSALE == 3  ~ CF083,
  #     CASTE %in% c('B', 'D') & PRSALE == 'AB' & ZOSALE == 1  ~ CF091,
  #     CASTE %in% c('B', 'D') & PRSALE == 'AB' & ZOSALE == 2  ~ CF092,
  #     CASTE %in% c('B', 'D') & PRSALE == 'BC' & ZOSALE == 1  ~ CF101,
  #     CASTE %in% c('B', 'D') & PRSALE == 'BC' & ZOSALE == 2  ~ CF102,
  #     CASTE %in% c('B', 'D') & PRSALE == 'NT' & ZOSALE == 1  ~ CF111,
  #     CASTE %in% c('B', 'D') & PRSALE == 'YT' & ZOSALE == 1  ~ CF121,
  #     TRUE ~ CFACT
  #   ),
  #   # --------------------- 
  #   # SET CFACT TO ZERO FOR 
  #   # NON-POTENTIAL HUNTERS 
  #   # --------------------- 
  #   CFACT = if_else(POTNTL == 'N', 0.0000, CFACT)
  # ) |>
  # select(-c(CF011, CF012, CF021, CF031, CF032,
  #           CF041, CF042, CF051, CF052, CF061, CF062, CF063,
  #           CF071, CF072, CF081, CF082, CF083, CF091, CF092,
  #           CF101, CF102, CF111, CF121)) |>
  # arrange(PRSAMP, ZOSAMP, CASTE, PRHUNT, ZOHUNT)

# Save output
saveRDS(harvYYw, file.path(stork_dir, paste0("harv",YY,"w.rds")))


# check sas ---------------------------------------------------------------

# harvYYw_sas <- haven::read_sas(file.path(stork_dir, paste0("harv",YY,"w.sas7bdat"))) |> 
#   rename_with(.fn = ~stringr::str_to_lower(.x)) |> 
#   arrange(selyear,permit)
# 
# harvYYw <- harvYYw |> 
#   arrange(selyear,permit)
# 
# plot(harvYYw$cfact,harvYYw_sas$cfact)
# nrow(harvYYw_sas) == nrow(harvYYw)
































