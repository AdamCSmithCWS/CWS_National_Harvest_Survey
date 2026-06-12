library(dplyr)

#library(haven)  # for saving .sas7bdat if needed, or use readr/base R
YYYY <- 2024
YY <- 24

# ── Read fixed-width text file ────────────────────────────────────────────────
f1 <- read.fwf(
  file = paste0("C:/Users/smithac/OneDrive - EC-EC/Harvest Survey A146/",YYYY,"/PERM",YY,"S.TXT"),
  widths = diff(c(0, 4, 6,   # popr: cols 5-6  (skip 4, read 2)
                  9, 10,     # pozone: col 10  (skip 3, read 1)
                  47, 52,    # resren: cols 48-52 (skip 37, read 5)
                  56, 61,    # nrsren: cols 57-61 (skip 4, read 5)
                  72, 77,    # resnrn: cols 73-77 (skip 11, read 5)
                  81, 86)),  # nrsnrn: cols 82-86 (skip 4, read 5)
  col.names = c("skip1", "popr", "skip2", "pozone",
                "skip3", "resren", "skip4", "nrsren",
                "skip5", "resnrn", "skip6", "nrsnrn"),
  stringsAsFactors = FALSE
)

# Drop skip columns
f1 <- f1[, c("popr", "pozone", "resren", "nrsren", "resnrn", "nrsnrn")]

# Trim whitespace from character column
f1$popr <- trimws(f1$popr)

# ── Province code lookup ──────────────────────────────────────────────────────
prov_lookup <- c(
  NF = 1, PE = 2, NS = 3, NB = 4, PQ = 5,  ON = 6,
  MB = 7, SK = 8, AB = 9, BC = 10, NT = 11, YT = 12, NU = 13
)
f1$poprov <- prov_lookup[f1$popr]   # returns NA for unmatched codes

# ── Composite province-zone code ─────────────────────────────────────────────
f1$poprzo <- f1$poprov * 10 + f1$pozone

# ── Print check ──────────────────────────────────────────────────────────────
print(f1[, c("popr", "poprov", "pozone", "poprzo")])

# ── SPLIT macro equivalent ───────────────────────────────────────────────────
# %SPLIT(FOUT, PCVAL, VSEL) → creates a subset with PCODE and COUNT columns
split_data <- function(data, pcval, vsel) {
  out        <- data[, c("poprzo")]  |> as.data.frame()
  names(out) <- "poprzo"
  out$pcode  <- pcval
  out$count  <- data[[vsel]]
  out
}

g1 <- split_data(f1, 11, "resren")   # resident renewal
g2 <- split_data(f1, 12, "nrsren")   # non-resident renewal
g3 <- split_data(f1, 21, "resnrn")   # resident non-renewal
g4 <- split_data(f1, 22, "nrsnrn")   # non-resident non-renewal

# ── Combine and save ─────────────────────────────────────────────────────────
persum <- rbind(g1, g2, g3, g4)
persum$year <- YY
persum <- persum[, c("year", "poprzo", "pcode", "count")]

# Save as RDS (native R format — equivalent to SAS LIBNAME output)
out_dir <- paste0("C:/Users/smithac/OneDrive - EC-EC/Harvest Survey A146/PERMITSUMMARY")
saveRDS(persum, file = file.path(out_dir, paste0("PERSUM",YY,".rds")))

# ── Print final output ────────────────────────────────────────────────────────
print(persum[, c("year", "poprzo", "pcode", "count")])

# compare
 



# pprog3 ------------------------------------------------------------------

lYY <- YY-1


# ── Load input data (from previous script's output) ──────────────────────────
out_dir  <- 'C:/Users/smithac/OneDrive - EC-EC/Harvest Survey A146/PERMITSUMMARY'
heron_dir <- 'C:/Users/smithac/OneDrive - EC-EC/Harvest Survey A146/POPULATIONSIZE'
crane_dir <- 'C:/Users/smithac/OneDrive - EC-EC/Harvest Survey A146/PERMITSALES'

persumYY <- readRDS(file.path(out_dir, paste0("PERSUM",YY,".rds")))
persumlYY <- readRDS(file.path(out_dir, paste0("PERSUM",lYY,".rds")))

# ── Combine years and derive columns ─────────────────────────────────────────
f1 <- rbind(persumYY, persumlYY)

f1$poprov  <- floor(f1$poprzo / 10)
f1$pozone  <- f1$poprzo - f1$poprov * 10
f1$boughtly <- floor(f1$pcode / 10)
f1$rescode  <- f1$pcode - 10 * f1$boughtly

f1 <- f1[order(f1$poprov, f1$pozone, f1$year), ]

# ── SELBR macro equivalent ───────────────────────────────────────────────────
# Filters by boughtly + rescode, renames COUNT to cname, keeps relevant cols
selbr <- function(data, bly, res, cname) {
  out <- data[data$boughtly == bly & data$rescode == res,
              c("poprov", "pozone", "year", "count")]
  names(out)[names(out) == "count"] <- cname
  out
}

g1 <- selbr(f1, 1, 1, "resren")
g2 <- selbr(f1, 1, 2, "nrsren")
g3 <- selbr(f1, 2, 1, "resnrn")
g4 <- selbr(f1, 2, 2, "nrsnrn")

# ── Merge the four subsets ────────────────────────────────────────────────────
# Full join so all province/zone/year combos are retained; NAs become 0
gg <- Reduce(function(a, b) merge(a, b, by = c("poprov", "pozone", "year"),
                                  all = TRUE),
             list(g1, g2, g3, g4))

# Replace NA with 0
gg[is.na(gg)] <- 0

# SP flag: provinces where sample A = resnrn + nrsnrn (vs resnrn only)
gg$sp <- 0
gg$sp[gg$poprov %in% c(1, 2, 3, 11, 12)] <- 1
gg$sp[gg$poprov == 10 & gg$pozone == 2]   <- 1

# ── Helper: build a sample dataset ───────────────────────────────────────────
make_sample <- function(data, yr, sample_lbl, totperm_expr,
                        pr_col, zo_col, filter_expr = NULL) {
  d <- data[data$year == yr, ]
  if (!is.null(filter_expr)) d <- d[filter_expr(d), ]
  data.frame(
    prsamp = d[[pr_col]],
    zosamp = d[[zo_col]],
    sample = sample_lbl,
    totperm = totperm_expr(d)
  )
}

# ── Population size datasets ─────────────────────────────────────────────────

# Sample A — year YY, non-renewal permits (+ NR if SP province)
sa <- make_sample(gg, YY, "A", pr_col = "poprov", zo_col = "pozone",
                  totperm_expr = function(d) ifelse(d$sp == 1, d$resnrn + d$nrsnrn, d$resnrn))
cat("Sample SA\n"); print(sa)

# Sample B — year lYY, non-renewal permits
sb <- make_sample(gg, lYY, "B", pr_col = "poprov", zo_col = "pozone",
                  totperm_expr = function(d) ifelse(d$sp == 1, d$resnrn + d$nrsnrn, d$resnrn))
cat("Sample SB\n"); print(sb)

# Sample D — year lYY, renewal permits
sd <- make_sample(gg, lYY, "D", pr_col = "poprov", zo_col = "pozone",
                  totperm_expr = function(d) ifelse(d$sp == 1, d$resren + d$nrsren, d$resren))
cat("Sample SD\n"); print(sd)

# Sample E — year YY, non-resident permits, SP==0 provinces only
se <- make_sample(gg, YY, "E", pr_col = "poprov", zo_col = "pozone",
                  filter_expr  = function(d) d$sp == 0,
                  totperm_expr = function(d) d$nrsren + d$nrsnrn)
cat("Sample SE\n"); print(se)

# ── Combine population sizes and save ─────────────────────────────────────────
popsizYY <- rbind(sa, sb, sd, se)
popsizYY$year <- YY
popsizYY <- popsizYY[order(popsizYY$prsamp, popsizYY$zosamp, popsizYY$sample), ]

saveRDS(popsizYY, file.path(heron_dir, paste0("POPSIZ",YY,".rds")))
cat("POPULATION SIZES FOR ",YYYY," SURVEY\n"); print(popsizYY)


# check sas ---------------------------------------------------------------

# popsizYY_sas <- haven::read_sas(file.path(heron_dir, paste0("POPSIZ",YY,".sas7bdat")))
# plot(popsizYY_sas$TOTPERM,popsizYY$totperm)
# nrow(popsizYY_sas) == nrow(popsizYY)

# ── Permit sales datasets ─────────────────────────────────────────────────────
make_sale <- function(data, yr, sample_lbl, totsale_expr,
                      filter_expr = NULL) {
  d <- data[data$year == yr, ]
  if (!is.null(filter_expr)) d <- d[filter_expr(d), ]
  data.frame(
    prsale  = d$poprov,
    zosale  = d$pozone,
    sample  = sample_lbl,
    totsale = totsale_expr(d)
  )
}

# Sample A — year YY, non-renewal
ta <- make_sale(gg, YY, "A",
                totsale_expr = function(d) ifelse(d$sp == 1, d$resnrn + d$nrsnrn, d$resnrn))
cat("Sample TA\n"); print(ta)

# Sample B — year YY, renewal
tb <- make_sale(gg, YY, "B",
                totsale_expr = function(d) ifelse(d$sp == 1, d$resren + d$nrsren, d$resren))
cat("Sample TB\n"); print(tb)

# Sample E — year YY, non-resident, SP==0 only
te <- make_sale(gg, YY, "E",
                filter_expr  = function(d) d$sp == 0,
                totsale_expr = function(d) d$nrsren + d$nrsnrn)
cat("Sample TE\n"); print(te)

# ── Combine permit sales and save ─────────────────────────────────────────────
persalYY <- rbind(ta, tb, te)
persalYY$year <- YY

persalYY <- persalYY |> 
  arrange()
saveRDS(persalYY, file.path(crane_dir, paste0("PERSAL",YY,".rds")))
cat(paste0("PERMIT SALES FOR ",YYYY,"\n")); print(persalYY)


# check sas ---------------------------------------------------------------

# persalYY_sas <- haven::read_sas(file.path(crane_dir, paste0("PERSAL",YY,".sas7bdat")))
# plot(persalYY_sas$TOTSALE,persalYY$totsale)
# nrow(persalYY_sas) == nrow(persalYY)


