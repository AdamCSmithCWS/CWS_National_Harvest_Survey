

# calendar data
# 


library(haven)
library(dplyr)
library(readr)
library(tidyverse)

YY <- 25
YYYY <- 2000+YY

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


# -------------------------------------
# READ FIXED-WIDTH FILE
# -------------------------------------
f1 <- read_fwf(
  file.path(stork_dir, paste0("HQSCALG.",YYYY)),
  col_positions = fwf_cols(
    SELYEAR  = c(2, 5),
    PERMIT   = c(6, 11),
    PRHUNTG  = c(14, 15),
    CALTYPE  = c(16, 16),
    MONH     = c(17, 18),
    day01 = c(19, 20),  day02 = c(21, 22),  day03 = c(23, 24),  day04 = c(25, 26),
    day05 = c(27, 28),  day06 = c(29, 30),  day07 = c(31, 32),  day08 = c(33, 34),
    day09 = c(35, 36),  day10 = c(37, 38),  day11 = c(39, 40),  day12 = c(41, 42),
    day13 = c(43, 44),  day14 = c(45, 46),  day15 = c(47, 48),  day16 = c(49, 50),
    day17 = c(51, 52),  day18 = c(53, 54),  day19 = c(55, 56),  day20 = c(57, 58),
    day21 = c(59, 60),  day22 = c(61, 62),  day23 = c(63, 64),  day24 = c(65, 66),
    day25 = c(67, 68),  day26 = c(69, 70),  day27 = c(71, 72),  day28 = c(73, 74),
    day29 = c(75, 76),  day30 = c(77, 78),  day31 = c(79, 80),
    ZOHUNTG  = c(81, 82)
  ),
  col_types = cols(
    PRHUNTG = col_character(),
    CALTYPE = col_character(),
    .default = col_integer()
  )
) |> 
  rename_with(.fn = ~str_to_lower(.x)) |> 
  mutate(prhuntg = recode_prov(prhuntg),
         year = YYYY)
  
gcal <- f1 |> 
  pivot_longer(cols = starts_with("day"),
               names_to = "dayh",
               names_prefix = "day",
               values_to = "count") |> 
  mutate(dayh = as.integer(dayh)) |> 
  drop_na() |> 
  select(-caltype)

#gcal_sas <- read_sas(file.path(stork_dir, "gcal24.sas7bdat")) |> 
#   rename_with(.fn = ~str_to_lower(.x))
# 
# 
# table(gcal$selyear)
# table(gcal_sas$selyear)
# 
# table(gcal$day)
# table(gcal_sas$dayh)
# perm_sas <- table(gcal_sas$permit)
# perm <- table(gcal$permit)
# 
# (perm_sas[which(!names(perm_sas) %in% names(perm))])

fil.yr <- paste0("gcal",YY)
saveRDS(gcal,paste0(stork_dir,"/",fil.yr,".rds"))





# -------------------------------------
# READ FIXED-WIDTH FILE
# -------------------------------------
f1 <- read_fwf(
  file.path(stork_dir, paste0("HQSCALD.",YYYY)),
  col_positions = fwf_cols(
    SELYEAR  = c(2, 5),
    PERMIT   = c(6, 11),
    PRHUNT  = c(14, 15),
    CALTYPE  = c(16, 16),
    MONH     = c(17, 18),
    day01 = c(19, 20),  day02 = c(21, 22),  day03 = c(23, 24),  day04 = c(25, 26),
    day05 = c(27, 28),  day06 = c(29, 30),  day07 = c(31, 32),  day08 = c(33, 34),
    day09 = c(35, 36),  day10 = c(37, 38),  day11 = c(39, 40),  day12 = c(41, 42),
    day13 = c(43, 44),  day14 = c(45, 46),  day15 = c(47, 48),  day16 = c(49, 50),
    day17 = c(51, 52),  day18 = c(53, 54),  day19 = c(55, 56),  day20 = c(57, 58),
    day21 = c(59, 60),  day22 = c(61, 62),  day23 = c(63, 64),  day24 = c(65, 66),
    day25 = c(67, 68),  day26 = c(69, 70),  day27 = c(71, 72),  day28 = c(73, 74),
    day29 = c(75, 76),  day30 = c(77, 78),  day31 = c(79, 80),
    ZOHUNT  = c(81, 82)
  ),
  col_types = cols(
    PRHUNT = col_character(),
    CALTYPE = col_character(),
    .default = col_integer()
  )
) |> 
  rename_with(.fn = ~str_to_lower(.x))|> 
  mutate(prhunt = recode_prov(prhunt),
         year = YYYY)


dcal <- f1 |> 
  pivot_longer(cols = starts_with("day"),
               names_to = "dayh",
               names_prefix = "day",
               values_to = "count") |> 
  mutate(dayh = as.integer(dayh)) |> 
  drop_na() |> 
  select(-caltype)

# dcal_sas <- read_sas(file.path(stork_dir, paste0("dcal",YY,".sas7bdat"))) |> 
#   rename_with(.fn = ~str_to_lower(.x))
# 
# 
# table(dcal$selyear)
# table(dcal_sas$selyear)
# 
# table(dcal$day)
# table(dcal_sas$dayh)
# perm_sas <- table(dcal_sas$permit)
# perm <- table(dcal$permit)
# 
# (perm_sas[which(!names(perm_sas) %in% names(perm))])


fil.yr <- paste0("dcal",YY)
saveRDS(dcal,paste0(stork_dir,"/",fil.yr,".rds"))



# -------------------------------------
# READ FIXED-WIDTH FILE
# -------------------------------------
f1 <- read_fwf(
  file.path(stork_dir, paste0("HQSCALM.",YYYY)),
  col_positions = fwf_cols(
    SELYEAR  = c(2, 5),
    PERMIT   = c(6, 11),
    PRHUNTM  = c(14, 15),
    CALTYPE  = c(16, 16),
    MONH     = c(17, 18),
    day01 = c(19, 20),  day02 = c(21, 22),  day03 = c(23, 24),  day04 = c(25, 26),
    day05 = c(27, 28),  day06 = c(29, 30),  day07 = c(31, 32),  day08 = c(33, 34),
    day09 = c(35, 36),  day10 = c(37, 38),  day11 = c(39, 40),  day12 = c(41, 42),
    day13 = c(43, 44),  day14 = c(45, 46),  day15 = c(47, 48),  day16 = c(49, 50),
    day17 = c(51, 52),  day18 = c(53, 54),  day19 = c(55, 56),  day20 = c(57, 58),
    day21 = c(59, 60),  day22 = c(61, 62),  day23 = c(63, 64),  day24 = c(65, 66),
    day25 = c(67, 68),  day26 = c(69, 70),  day27 = c(71, 72),  day28 = c(73, 74),
    day29 = c(75, 76),  day30 = c(77, 78),  day31 = c(79, 80),
    ZOHUNTM  = c(81, 82)
  ),
  col_types = cols(
    PRHUNTM = col_character(),
    CALTYPE = col_character(),
    .default = col_integer()
  )
) |> 
  rename_with(.fn = ~str_to_lower(.x))|> 
  mutate(prhuntm = recode_prov(prhuntm),
         year = YYYY) |> 
  select(-caltype)


mcal <- f1 |> 
  pivot_longer(cols = starts_with("day"),
               names_to = "dayh",
               names_prefix = "day",
               values_to = "count") |> 
  mutate(dayh = as.integer(dayh)) |> 
  drop_na()

# mcal_sas <- read_sas(file.path(stork_dir, "mcal24.sas7bdat")) |> 
#   rename_with(.fn = ~str_to_lower(.x))
# 
# 
# 
# table(mcal$selyear)
# table(mcal_sas$selyear)
# 
# table(mcal$day)
# table(mcal_sas$dayh)
# perm_sas <- table(mcal_sas$permit)
# perm <- table(mcal$permit)
# 
# (perm[which(!names(perm) %in% names(perm_sas))])

fil.yr <- paste0("mcal",YY)
saveRDS(mcal,paste0(stork_dir,"/",fil.yr,".rds"))

