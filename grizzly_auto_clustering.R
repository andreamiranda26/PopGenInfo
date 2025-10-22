library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(lubridate)
library(patchwork)
library(igraph)
library(ggraph)
library(binom)
library(dplyr)
library(limSolve)
library(tidyr)

setwd("/Users/jannawilloughby/Google Drive/My Drive/Willoughby lab/projects - active/popgen_information/evolution start/")
raw <- read_csv("Wyoming Grizzly bear documentsV3.csv", show_col_types = FALSE)
names(raw) <- str_replace_all(names(raw), "^Document\\s*$", "Document")

# Pick themes and metadata columns
quote_cols <- c('Law_detailed',"State_of_science", "Recovery_goals", "Human_wildlife_conflict")
meta_cols <- c("Num","Pub_date","Type_actor","Political_actor", "Document","Condensed_function")
meta_cols  <- meta_cols[meta_cols %in% names(raw)]  # intersect safely

# actor mapping
actor_levels <- c("Executive","Judicial","Legislature","Indigenous", "Journalist","Non Governmental Organization","Public")
get_col <- function(df, nm) if (nm %in% names(df)) df[[nm]] else rep(NA_character_, nrow(df))
raw$actor_raw <- raw$Condensed_function

# normalize labels (fix misspelling, expand NGO, drop everything else to NA)
raw <- raw %>%
  dplyr::mutate(
    actor_raw = dplyr::case_when(
      actor_raw == "Indegenous" ~ "Indigenous",
      TRUE ~ actor_raw
    ),
    actor = dplyr::recode(as.character(actor_raw),
                          "Executive" = "Executive",
                          "Judicial" = "Judicial",
                          "Legislature" = "Legislature",
                          "Indigenous" = "Indigenous",
                          "Journalist" = "Journalist",
                          "NGO" = "Non Governmental Organization",
                          "Public" = "Public",
                          .default = NA_character_
    ),
    actor = factor(actor, levels = actor_levels)
  )

# make sure `actor` is retained anywhere you subset/select metadata
meta_cols <- unique(c(meta_cols, "actor"))
actor_col <- "actor"

# parameters etc
min_chars   <- 30          # min characters to keep a quote

# Reshape to long: one row = one quote 
long <- raw |>
  select(any_of(c(meta_cols, quote_cols))) |>
  pivot_longer(cols = any_of(quote_cols),
               names_to = "theme", values_to = "quote") |>
  mutate(
    quote = stringr::str_squish(quote),
    Pub_date = suppressWarnings(mdy(Pub_date)),
    year = year(Pub_date)
  ) |>
  filter(!is.na(quote), nchar(quote) >= 40) |>
  distinct(Num, theme, quote, .keep_all = TRUE)

# time bins
long <- long |>
  mutate(
    time_bin = case_when(
      year < 2007                ~ "Before",
      year >= 2007 & year <= 2010 ~ "During I",
      year >= 2011 & year <= 2016 ~ "After I",
      year >= 2017 & year <= 2018 ~ "During II",
      year >= 2019 & year <= 2024 ~ "Recent",
      TRUE ~ NA_character_
    ),
    time_bin = factor(time_bin, levels = c("Before","During I","After I","During II","Recent"))
  )

# top collocations within a theme
top_collocations_for_theme <- function(long, theme_name, sizes = 2:3, min_count = 5,  top_n = 30, extra_stop = c("said","say","says","told", "grizzly","bear","bears", "wyoming","montana","idaho", "people","year","years"), quiet = FALSE) {
  df_theme <- dplyr::filter(long, theme == theme_name)
  if (nrow(df_theme) == 0) stop("No quotes found for theme: ", theme_name)
  sw <- setdiff(quanteda::stopwords("en"), c("no","not","nor"))
  toks <- quanteda::tokens(df_theme$quote, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) |> quanteda::tokens_tolower()
  
  # normalize common variants
  norm_pat <- c("u.s.", "u.s", "u-­s")
  toks <- quanteda::tokens_replace(
    toks,
    pattern = norm_pat,
    replacement = rep("us", length(norm_pat)),
    valuetype = "fixed"
  )
  toks <- quanteda::tokens_remove(toks, pattern = c(sw, extra_stop))
  coll <- quanteda.textstats::textstat_collocations( toks, size = sizes, method = "lambda", min_count = min_count )
  
  if (nrow(coll) == 0) {
    if (!quiet) message("No collocations met min_count for theme: ", theme_name)
    return(tibble::tibble(theme = character(), phrase = character(), count = integer(), lambda = numeric(), length = integer()))
  }
  
  out <- coll |>
    dplyr::arrange(dplyr::desc(lambda), dplyr::desc(count)) |>
    dplyr::mutate(
      theme = theme_name,
      phrase = vapply(collocation, paste, collapse = " ", FUN.VALUE = character(1))
    ) |>
    dplyr::select(theme, phrase, count, lambda, length) |>
    dplyr::slice_head(n = top_n)
  
  if (!quiet) {
    cat("\n=== Top", nrow(out), "collocations for theme:", theme_name, "===\n")
    to_show <- out |> dplyr::select(phrase, count, lambda)
    
    #print
    op <- options(na.print = "NA")
    on.exit(options(op), add = TRUE)
    print.data.frame(as.data.frame(to_show), row.names = FALSE)
  }
  out
}

# Human–wildlife conflict
conflict_coll <- top_collocations_for_theme(long, "Human_wildlife_conflict", sizes = 2:3, min_count = 5, top_n = 30)
dict_conflict <- quanteda::dictionary(list(
  conflict_impacts_people_property = c( "injury death", "property damage", "residents visitors", "livestock producers", "livestock owners" ),
  conflict_mitigation_management = c( "conflict prevention", "food sources", "private land", "study team", "interagency study" ),
  hunting_recreation_conflicts = c( "hunting guide", "game fish"  ),
  geo_ecosystem_context = c( "continental divide", "rocky mountain", "northern continental", "mountain front", "greater yellowstone", "glacier national", "national park", "national forest" ),
  agencies_institutions = c( "fish wildlife", "species act", "wildlife services", "recovery coordinator" ),
  climate_stressors = c( "climate change" )
))

# Recovery goals
recovery_coll <- top_collocations_for_theme( long, "Recovery_goals", sizes = 2:3, min_count = 5, top_n = 30 )
dict_recovery <- quanteda::dictionary(list(
  population_viability_capacity = c( "carrying capacity", "square miles", "no longer", "five times" ),
  geo_ecosystem_context = c( "continental divide", "northern continental", "greater yellowstone", "northern rockies", "rocky mountain" ),
  agency_leadership_actors = c( "van manen", "chris servheen", "lynn scarlett", "secretary lynn", "deputy interior", "interior secretary" ),
  law_politics_frames = c( "circular legal", "legal battles", "activist judges", "government accepted" ),
  agency_mentions = c( "fish wildlife", "interagency study", "united states" )
))

# State of science
science_coll <- top_collocations_for_theme( long, "State_of_science", sizes = 2:3, min_count = 5, top_n = 30 )
dict_science <- quanteda::dictionary(list(
  science_quality_frames = c( "sound science", "best available", "available science", "common sense" ),
  advocacy_and_opposition = c( "radical environmentalists", "defense council", "center biological", "louisa willcox", "rob bishop" ),
  conservation_science = c( "biological diversity", "conservation strategy", "draft plan" ),
  esa_legal_references = c( "endangered species", "species act", "fish wildlife", "us fish", "wildlife services", "environmental law" ),
  ecosystem_geography_context = c( "greater yellowstone", "yellowstone ecosystem", "continental divide", "northern continental", "national park", "natural resources", "forest service", "federal government", "public comment" ),
  domain_specifics = c( "trophy hunting", "wildlife biologist" )
))

# Law detailed
law_coll <- top_collocations_for_theme( long, "Law_detailed", sizes = 2:3, min_count = 5, top_n = 30 )
dict_law <- quanteda::dictionary(list(
  statutory_legal_refs = c( "regulatory mechanisms", "law sb", "interior re-issue" ),
  species_ecology_in_law = c( "ursus arctos", "arctos horribilis", "whitebark pine", "food sources", "native american" ),
  geo_contexts = c( "continental divide", "northern continental", "rocky mountain", "northern rockies", "greater yellowstone", "north cascades", "grand teton", "teton national" ),
  political_figures = c( "gillespie r-ethridge", "lang r-malta", "russ fulcher", "john barrasso", "jim risch", "steve daines", "cynthia lummis", "dana christensen", "bush administration" ),
  institutions_processes = c( "advisory council", "interagency committee" )
))

named_dicts <- list(
  Human_wildlife_conflict = dict_conflict,
  Recovery_goals          = dict_recovery,
  State_of_science        = dict_science,
  Law_detailed            = dict_law
)

# apply dictionaries
apply_theme_dict <- function(long, theme_name, dict, extra_stop = character()) {
  df_theme <- dplyr::filter(long, theme == theme_name)
  if (nrow(df_theme) == 0) return(NULL)
  
  # stopwords (keep no/not/nor)
  sw <- setdiff(quanteda::stopwords("en"), c("no","not","nor"))
  
  # tokenize + normalize (lowercase; normalize U.S. → us)
  toks <- quanteda::tokens(
    df_theme$quote,
    remove_punct   = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE
  ) |>
    quanteda::tokens_tolower() |>
    quanteda::tokens_replace(
      pattern     = c("u.s.", "u.s", "u-­s"),
      replacement = rep("us", 3),
      valuetype   = "fixed"
    )
  
  # compound
  all_patterns <- unlist(dict, use.names = FALSE)
  if (length(all_patterns) > 0) {
    toks <- quanteda::tokens_compound(
      toks,
      pattern      = quanteda::phrase(all_patterns),
      concatenator = "_"
    )
  }
  
  # remove stopwords 
  toks <- quanteda::tokens_remove(toks, pattern = c(sw, extra_stop), padding = FALSE)
  
  # Build DFM
  dfm_mat <- quanteda::dfm(toks)
  dict_aligned <- quanteda::dictionary(
    lapply(dict, function(v) gsub(" ", "_", v, fixed = TRUE))
  )
  
  # lookup
  m <- quanteda::dfm_lookup(
    dfm_mat,
    dictionary       = dict_aligned,
    valuetype        = "fixed",
    case_insensitive = TRUE,
    exclusive        = TRUE
  )
  
  out <- quanteda::convert(m, to = "data.frame") |>
    tibble::as_tibble(rownames = "doc_id") |>
    dplyr::bind_cols(dplyr::select(df_theme, -quote))
  
  if (!"theme" %in% names(out) || all(is.na(out$theme))) out$theme <- theme_name
  out
}


# group_cols controls which metadata stratify by
freq_by_groups <- function(hit_tbl, group_cols = c("time_bin","Condensed_function")) {
  if (is.null(hit_tbl)) return(NULL)
  
  # metadata we should never pivot as alleles
  meta_known <- c("doc_id","Num","Pub_date","year","time_bin","theme","Type_actor","Political_actor","Condensed_function","Document")
  meta_present <- intersect(meta_known, names(hit_tbl))
  
  # allele columns = numeric/integer/logical & not metadata
  is_allele <- vapply(hit_tbl, function(x) is.numeric(x) || is.integer(x) || is.logical(x),logical(1))
  allele_cols <- setdiff(names(hit_tbl)[is_allele], meta_present)
  if (!length(allele_cols)) stop("No allele columns found in hit_tbl.")
  
  group_cols <- intersect(group_cols, names(hit_tbl))
  if (!length(group_cols)) stop("None of the requested group_cols exist in hit_tbl.")
  
  hit_tbl |>
    tidyr::pivot_longer(tidyselect::all_of(allele_cols),
                        names_to = "allele", values_to = "hits") |>
    dplyr::mutate(hits = as.integer(hits > 0)) |>
    dplyr::group_by(.data$theme, dplyr::across(all_of(group_cols)), .data$allele) |>
    dplyr::summarise(n_hits = sum(.data$hits),
                     n_quotes = dplyr::n(),
                     .groups = "drop_last") |>
    dplyr::mutate(freq = dplyr::if_else(n_quotes > 0, n_hits / n_quotes, NA_real_)) |>
    dplyr::ungroup()
}

# Build hit tables and calculate frequency
hit_tables <- purrr::imap(named_dicts, function(dict, th) {apply_theme_dict(long, th, dict)})
freq_tables <- purrr::imap(hit_tables, ~ freq_by_groups(.x, group_cols = c("time_bin", "Condensed_function")))
freq_all <- dplyr::bind_rows(freq_tables)

#### change over time and ktotal ####
#"Law_detailed"  "State_of_science"        "Recovery_goals"          "Human_wildlife_conflict"   
# k total change over time
bins <- c("Before","During I","After I","During II","Recent")
freq_all$time_bin <- factor(freq_all$time_bin, levels = bins)

# Ktotal (allele richness) per theme × bin (collapsed over actor groups)
ktotal_df <- freq_all |>
  dplyr::group_by(theme, time_bin, allele) |>
  dplyr::summarise(total_hits = sum(n_hits, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(present = total_hits > 0) |>
  dplyr::group_by(theme, time_bin) |>
  dplyr::summarise(K = sum(present), .groups = "drop") |>
  tidyr::complete(theme, time_bin = factor(bins, levels = bins), fill = list(K = 0)) |>
  dplyr::arrange(theme, time_bin)

# presence table per theme×bin×allele (collapsed over actors)
presence <- freq_all |>
  dplyr::group_by(theme, time_bin, allele) |>
  dplyr::summarise(present = any(n_hits > 0), .groups = "drop") |>
  tidyr::complete(theme, time_bin = factor(bins, levels = bins), allele, fill = list(present = FALSE)) |>
  dplyr::arrange(theme, allele, time_bin)

# gains/losses per transition
gl <- presence |>
  dplyr::group_by(theme, allele) |>
  dplyr::arrange(time_bin, .by_group = TRUE) |>
  dplyr::mutate(prev = dplyr::lag(present, default = FALSE),
                gain = (!prev & present),
                loss = (prev & !present)) |>
  dplyr::ungroup()

# sum gains/losses per theme×bin (assign them to the "arrival" bin)
gl_bin <- gl |>
  dplyr::group_by(theme, time_bin) |>
  dplyr::summarise(gains = sum(gain), losses = sum(loss), .groups = "drop") |>
  dplyr::arrange(theme, time_bin)

# K_prev for bin t is K at previous bin; for the first bin it’s NA
kt_w_prev <- ktotal_df |>
  dplyr::group_by(theme) |>
  dplyr::arrange(time_bin, .by_group = TRUE) |>
  dplyr::mutate(K_prev = dplyr::lag(K)) |>
  dplyr::ungroup() |>
  dplyr::left_join(gl_bin, by = c("theme","time_bin"))

# estimate mu_hat and delta_hat as mean(gains/K_prev) and mean(losses/K_prev) over valid transitions
rate_est <- kt_w_prev |>
  dplyr::filter(!is.na(K_prev), K_prev > 0) |>
  dplyr::mutate(mu_hat = ifelse(is.finite(gains / K_prev), gains / K_prev, NA_real_),
                delta_hat = ifelse(is.finite(losses / K_prev), losses / K_prev, NA_real_)) |>
  dplyr::group_by(theme) |>
  dplyr::summarise(mu_hat = mean(mu_hat, na.rm = TRUE),
                   delta_hat = mean(delta_hat, na.rm = TRUE),
                   gamma_hat = mu_hat - delta_hat,
                   .groups = "drop")

# Build expected K trajectories using K_{t+1} = K_t * (1 + gamma_hat) 
themes <- unique(ktotal_df$theme)
exp_list <- lapply(themes, function(th) {
  sub <- ktotal_df[ktotal_df$theme == th, , drop = FALSE]
  gamma <- rate_est$gamma_hat[rate_est$theme == th]
  if (length(gamma) == 0 || is.na(gamma)) gamma <- 0  # fallback
  
  K_obs <- sub$K
  K_exp <- K_obs*NA
  # start at first bin with observed K
  first_idx <- which(!is.na(K_obs))[1]
  if (!is.na(first_idx)) {
    K_exp[first_idx] <- K_obs[first_idx]
    if (length(K_obs) > first_idx) {
      for (i in (first_idx+1):length(K_obs)) {
        K_exp[i] <- K_exp[i-1] * (1 + gamma)
      }
    }
  }
  data.frame(theme = th, time_bin = sub$time_bin, K_exp = K_exp)
})
Kexp_df <- do.call(rbind, exp_list)

# Prep pooled allele frequencies per locus × bin (collapse over actors)
allele_freq_df <- freq_all |>
  dplyr::group_by(theme, time_bin, allele) |>
  dplyr::summarise(
    nh = sum(n_hits, na.rm = TRUE),
    nq = sum(n_quotes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(freq = dplyr::if_else(nq > 0, nh / nq, NA_real_)) |>
  dplyr::arrange(theme, allele, time_bin)

cols <- c("Human_wildlife_conflict" = "firebrick3",
          "Recovery_goals"          = "darkorange3",
          "State_of_science"        = "goldenrod",
          "Law_detailed"            = "dodgerblue3")

# parameters 
trend_method <- "rho"       # "lm" (slope) or "rho" (Spearman)
tol_slope    <- 0.5         # if method="rho": |rho| > tol => up/down; else slope cutoff
label_top    <- 2           # how many up/down labels (when enabled)
label_toggle <- TRUE        # <-- toggle on/off labeling on the plots
min_points   <- 3           # min non-NA points per allele to compute trend
col_up   <- "chartreuse3"
col_down <- "darkorchid3"
col_flat <- "gray50"
out_dir  <- "allele_trend_outputs"  # output folder for PDFs/CSV
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
x <- seq_along(bins)

# extract locus label
get_locus <- function(df_row) {
  # If there's an explicit 'locus' column use it; else try to parse from allele text
  if ("locus" %in% names(df_row)) {
    return(as.character(df_row[["locus"]]))
  }
  al <- as.character(df_row[["allele"]])
  # Heuristic: split on ":" or "_" to get a locus-like prefix
  if (grepl(":", al)) return(strsplit(al, ":", fixed = TRUE)[[1]][1])
  if (grepl("_", al)) return(strsplit(al, "_", fixed = TRUE)[[1]][1])
  al
}

# Collect per-allele summaries across themes
trend_summary_list <- list()

for (th in unique(ktotal_df$theme)) {
  subK   <- subset(ktotal_df, theme == th)
  subKeC <- subset(Kexp_df,   theme == th)
  subK   <- subK[match(bins, as.character(subK$time_bin)), , drop = FALSE]
  subKeC <- subKeC[match(bins, as.character(subKeC$time_bin)), , drop = FALSE]
  
  y_obs <- subK$K
  y_ct  <- subKeC$K_exp
  yA    <- c(0, 12)
  
  # PDF 1: richness over time
  pdf(file = file.path(out_dir, sprintf("richness_%s.pdf", th)), width = 5, height = 5)
  on.exit(dev.off(), add = TRUE)
  
  plot(NA, xlim = c(1, length(bins)), ylim = yA,
       xaxt = "n", xlab = "", ylab = "Ktotal (number of dictionary alleles)",
       main = paste0("Allele richness over time — ", gsub("_"," ", th)))
  axis(1, at = x, labels = bins, las = 2)
  lines(x, y_obs, type = "b", lwd = 2, pch = 16, col = cols[th])
  if (any(is.finite(y_ct))) lines(x, y_ct, lty = 3, lwd = 2, col = cols[th])
  dev.off()
  
  # per-allele frequencies
  subF <- subset(allele_freq_df, theme == th)
  alleles_here <- unique(subF$allele)
  mat <- matrix(NA_real_, nrow = length(alleles_here), ncol = length(bins),
                dimnames = list(alleles_here, bins))
  for (al in alleles_here) {
    mat[al, ] <- sapply(bins, function(bn) {
      idx <- which(subF$allele == al & as.character(subF$time_bin) == bn)
      if (length(idx) == 0) NA_real_ else subF$freq[idx[1]]
    })
  }
  
  # compute trend per allele
  slope   <- setNames(rep(NA_real_, nrow(mat)), rownames(mat))
  n_used  <- slope
  start_f <- slope
  end_f   <- slope
  
  for (al in rownames(mat)) {
    y <- mat[al, ]
    keep <- is.finite(y)
    if (sum(keep) >= min_points) {
      if (trend_method == "lm") {
        fit <- lm(y[keep] ~ x[keep])
        slope[al] <- unname(coef(fit)[2])
      } else if (trend_method == "rho") {
        slope[al] <- suppressWarnings(cor(x[keep], y[keep],
                                          method = "spearman", use = "complete.obs"))
      }
      n_used[al] <- sum(keep)
      # record first/last observed freq (useful context)
      start_f[al] <- y[min(which(keep))]
      end_f[al]   <- y[max(which(keep))]
    }
  }
  
  # classify alleles
  trend_cat <- ifelse(is.na(slope), "insufficient",
                      ifelse(slope >  tol_slope, "up",
                             ifelse(slope < -tol_slope, "down", "flat")))
  col_line <- ifelse(trend_cat == "up",   col_up,
                     ifelse(trend_cat == "down", col_down,
                            ifelse(trend_cat == "flat", col_flat, "gray85")))
  
  mean_freq <- apply(mat, 2, function(col) mean(col, na.rm = TRUE))
  yB <- c(0, 0.5)
  
  # PDF 2: per-allele trends
  pdf(file = file.path(out_dir, sprintf("allele_trends_%s.pdf", th)), width = 5, height = 5)
  on.exit(dev.off(), add = TRUE)
  
  plot(NA, xlim = c(1, length(bins)), ylim = yB,
       xaxt = "n", xlab = "", ylab = "Allele frequency (pooled across actors)",
       main = paste0("Per-allele change by trend — ", gsub("_"," ", th)))
  axis(1, at = x, labels = bins, las = 2)
  for (i in seq_len(nrow(mat))) {
    lines(x, mat[i, ], col = col_line[i], lwd = 1.1, type = "o", pch = 20, cex = 0.55)
  }
  lines(x, mean_freq, lwd = 2.2, col = "black", type = "o", pch = 16, cex = 0.9)
  
  # legend counts
  n_up   <- sum(trend_cat == "up",   na.rm = TRUE)
  n_down <- sum(trend_cat == "down", na.rm = TRUE)
  n_flat <- sum(trend_cat == "flat", na.rm = TRUE)
  n_ins  <- sum(trend_cat == "insufficient", na.rm = TRUE)
  legend("topleft",
         legend = c(paste0("Increase (n=", n_up, ")"),
                    paste0("Decrease (n=", n_down, ")"),
                    paste0("Flat (n=", n_flat, ")"),
                    if (n_ins > 0) paste0("Insufficient (n=", n_ins, ")") else NULL,
                    "Mean (black)"),
         col = c(col_up, col_down, col_flat, if (n_ins > 0) "gray85" else NULL, "black"),
         lty = 1, lwd = 1.1, pch = 20, bty = "n", cex = 0.9)
  
  # optional labels for top movers
  effective_label_top <- if (isTRUE(label_toggle)) label_top else 0
  if (effective_label_top > 0) {
    ups   <- names(sort(slope[trend_cat == "up"],   decreasing = TRUE))[seq_len(min(effective_label_top, n_up))]
    downs <- names(sort(slope[trend_cat == "down"], decreasing = FALSE))[seq_len(min(effective_label_top, n_down))]
    lab_set <- unique(c(ups, downs))
    for (al in lab_set) {
      y <- mat[al, ]; keep <- which(is.finite(y))
      if (length(keep) > 0) {
        j <- max(keep)
        text(x[j], y[j], labels = al, pos = 4, cex = 0.75,
             col = if (trend_cat[al] == "up") col_up else col_down, xpd = NA, offset = 0.3)
      }
    }
  }
  dev.off()
  
  # per-allele summary
  # join locus labels (try to pull from subF row by row)
  loci_vec <- sapply(rownames(mat), function(al) {
    # take the first row for that allele in subF to derive locus
    row1 <- subF[subF$allele == al, , drop = FALSE]
    if (nrow(row1) == 0) return(NA_character_)
    get_locus(row1[1, ])
  })
  
  trend_summary_list[[th]] <- data.frame(
    theme      = th,
    allele     = rownames(mat),
    locus      = loci_vec,
    method     = trend_method,
    trend_val  = as.numeric(slope),       # rho or slope depending on method
    tol_used   = tol_slope,
    n_points   = as.numeric(n_used),
    start_freq = as.numeric(start_f),
    end_freq   = as.numeric(end_f),
    category   = trend_cat,
    stringsAsFactors = FALSE
  )
}

# summary table 
trend_summary <- do.call(rbind, trend_summary_list)
csv_path <- file.path(out_dir, sprintf("allele_trend_table_method_%s.csv", trend_method))
write.csv(trend_summary, csv_path, row.names = FALSE)


# Export observed vs expected richness and quantify deviation
richness_tbl <- ktotal_df %>%
  select(theme, time_bin, K_obs = K) %>%
  mutate(time_bin = as.character(time_bin)) %>%
  right_join(
    Kexp_df %>%
      select(theme, time_bin, K_exp) %>%
      mutate(time_bin = as.character(time_bin)),
    by = c("theme","time_bin")
  ) %>%
  # keep only the bins you actually plotted, in that order
  filter(time_bin %in% as.character(bins)) %>%
  mutate(time_bin = factor(time_bin, levels = as.character(bins))) %>%
  arrange(theme, time_bin)

# Compute per-bin absolute/relative deviations (safe division)
richness_tbl <- richness_tbl %>%
  mutate(
    abs_dev = ifelse(is.finite(K_obs) & is.finite(K_exp), abs(K_obs - K_exp), NA_real_),
    rel_dev = ifelse(is.finite(K_obs) & is.finite(K_exp) & K_exp > 0,
                     abs(K_obs - K_exp) / K_exp, NA_real_)
  )

# Summaries per theme
richness_dev_by_theme <- richness_tbl %>%
  group_by(theme) %>%
  summarise(
    n_bins        = sum(is.finite(K_obs) & is.finite(K_exp)),
    MAD           = mean(abs_dev, na.rm = TRUE),                 # mean absolute deviation
    rel_MAD       = mean(rel_dev, na.rm = TRUE),                 # relative MAD (unitless)
    RMSE          = sqrt(mean((K_obs - K_exp)^2, na.rm = TRUE)), # root mean squared error
    max_abs_dev   = suppressWarnings(max(abs_dev, na.rm = TRUE)),
    max_rel_dev   = suppressWarnings(max(rel_dev, na.rm = TRUE)),
    .groups = "drop"
  )

# overall summary across themes
richness_dev_overall <- richness_tbl %>%
  summarise(
    n_bins_total = sum(is.finite(K_obs) & is.finite(K_exp)),
    MAD          = mean(abs_dev, na.rm = TRUE),
    rel_MAD      = mean(rel_dev, na.rm = TRUE),
    RMSE         = sqrt(mean((K_obs - K_exp)^2, na.rm = TRUE)),
    max_abs_dev  = suppressWarnings(max(abs_dev, na.rm = TRUE)),
    max_rel_dev  = suppressWarnings(max(rel_dev, na.rm = TRUE))
  )

# Write to disk
write.csv(richness_tbl, file.path(out_dir, "richness_observed_expected_by_bin.csv"), row.names = FALSE)
write.csv(richness_dev_by_theme, file.path(out_dir, "richness_deviation_by_theme.csv"), row.names = FALSE)
write.csv(richness_dev_overall, file.path(out_dir, "richness_deviation_overall.csv"), row.names = FALSE)


#### migration between actors ####
# generate new freq tables
freq_tables_actor <- purrr::imap(
  hit_tables,
  ~ freq_by_groups(.x, group_cols = c("time_bin", "actor"))
)
freq_actor_all <- dplyr::bind_rows(freq_tables_actor)

# align bin order and normalize columns
allele_freq_df_actor <- freq_actor_all %>%
  dplyr::mutate(
    time_bin = factor(as.character(time_bin), levels = bins),
    actor    = as.character(actor),
    # make sure freq is numeric with NAs where undefined
    freq     = dplyr::if_else(is.finite(freq), as.numeric(freq), NA_real_)
  ) %>%
  dplyr::select(theme, allele, time_bin, actor, n_hits, n_quotes, freq) %>%
  dplyr::arrange(theme, actor, allele, time_bin)

actor_order <-  c("Executive","Legislature","Journalist","Non Governmental Organization")#c("Executive","Legislature","Judicial","Indigenous", "Journalist","Non Governmental Organization","Public")

# summarise to actor x topic within each theme
allele_summary <- allele_freq_df_actor %>%
  filter(!is.na(actor)) %>%
  mutate(actor = factor(actor, levels = actor_order)) %>%
  group_by(theme, allele, actor) %>%
  summarise(
    mean_freq    = mean(freq, na.rm = TRUE),   # average across time bins
    bins_present = sum(freq > 0, na.rm = TRUE),# how many bins had any presence
    .groups = "drop"
  )

# order topics (alleles) within each theme by overall mean across actors
allele_means <- allele_summary %>%
  group_by(theme, allele) %>%
  summarise(m = mean(mean_freq, na.rm = TRUE), .groups = "drop")

plot_df <- allele_summary %>%
  left_join(allele_means, by = c("theme","allele")) %>%
  mutate(allele = fct_reorder(allele, m, .desc = TRUE)) %>%
  arrange(theme, allele)

# heatmap
p_topics_by_actor <- ggplot(plot_df, aes(x = actor, y = allele, fill = mean_freq)) +
  geom_tile(color = "white", linewidth = 0.2) +
  # annotate tiles with number of bins where topic was present for that actor
  geom_text(aes(label = ifelse(bins_present > 0, bins_present, "")), size = 3) +
  scale_fill_gradient(name = "Mean freq\n(across bins)",
                      limits = c(0, 1), breaks = c(0, .25, .5, .75, 1),
                      low = "grey95", high = "steelblue") +
  facet_wrap(~ theme, scales = "free_y") +
  labs(x = "Actor", y = "Topic (allele)",
       title = "Topic occurrence by actor within each theme",
       subtitle = "Fill = mean per-bin frequency (n_hits / n_quotes). Number = bins with any presence (0–5).") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(face = "bold")
  )

p_topics_by_actor
ggsave("topics_by_actor_heatmap.png", p_topics_by_actor, width = 11, height = 8, dpi = 300)


df_presence <- allele_freq_df_actor %>%
  filter(!is.na(actor)) %>%
  mutate(actor = factor(actor, levels = actor_order),
         present = as.integer(freq > 0))

p_when <- ggplot(df_presence,
                 aes(x = time_bin, y = allele, fill = present)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient(name = "Present", limits = c(0,1),
                      breaks = c(0,1), labels = c("No","Yes"),
                      low = "grey95", high = "steelblue") +
  facet_grid(theme ~ actor, scales = "free_y", space = "free_y") +
  labs(x = "Time bin", y = "Topic (allele)",
       title = "When topics appear: presence by actor, time, and theme") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(face = "bold"))

p_when
ggsave("topics_by_actor_over_time.png", p_when, width = 12, height = 9, dpi = 300)

# simple migration of topics
actors_focus <- c("Executive","Legislature","Journalist","Non Governmental Organization")

# Topics by actor over time plot
first_times <- allele_freq_df_actor %>%
  filter(actor %in% actors_focus) %>%
  mutate(t = match(as.character(time_bin), bins),
         present = freq > 0) %>%
  group_by(theme, allele, actor) %>%
  summarise(
    first_t = if (any(present, na.rm = TRUE)) min(t[present], na.rm = TRUE) else NA_integer_,
    .groups = "drop"
  )
plot_df <- migration_simple %>%
  mutate(
    A = factor(A, levels = actors_focus),
    B = factor(B, levels = actors_focus)
  )

p_mig <- ggplot(plot_df, aes(A, B, fill = m_AB)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(is.na(m_AB), "–", sprintf("%.2f\n%s/%s", m_AB, events, opportunities))),
            size = 3, lineheight = 0.9) +
  scale_fill_gradient(limits = c(0,1), na.value = "grey90",
                      low = "grey95", high = "steelblue", name = "m_AB") +
  facet_wrap(~ theme) +
  labs(title = "Simple topic migration (first-adoption order)",
       subtitle = "Tile shows m_AB (events/opportunities). Event: first(B) > first(A).",
       x = "A (source earlier)", y = "B (target later)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(face = "bold"))
p_mig
ggsave("simple_topic_migration.png", p_mig, width = 10, height = 8, dpi = 300)


# migration rate calculations island-model estimator
# Δf_B = m_(A->B) * (f_A - f_B)  =>  m_hat = sum(Δf_B * ΔX) / sum(ΔX^2)
estimate_m_pairwise <- function(allele_freq_df_actor, theme, A, B, bins, topics = NULL, clip01 = TRUE) {
  stopifnot(A != B)
  df <- allele_freq_df_actor %>%
    filter(theme == !!theme, actor %in% c(A,B)) %>%
    mutate(t = match(as.character(time_bin), bins)) %>%
    select(allele, actor, t, freq)
  if (is.null(topics)) {
    topics <- allele_freq_df_actor %>% filter(theme == !!theme) %>% pull(allele) %>% unique()
  }
  df <- df %>% filter(allele %in% topics)
  
  # (allele, t) wide by actor; add t+1 for B
  wide <- df %>%
    pivot_wider(names_from = actor, values_from = freq, values_fill = 0) %>%
    arrange(allele, t) %>%
    group_by(allele) %>%
    mutate(B_tp1 = dplyr::lead(.data[[B]])) %>%
    ungroup() %>%
    filter(!is.na(B_tp1))
  if (!nrow(wide)) {
    return(tibble(theme = theme, A = A, B = B, n_rows = 0L, n_topics = 0L,
                  m_hat = NA_real_, self_retention = NA_real_, denom = NA_real_))
  }
  d <- wide %>%
    transmute(
      allele, t,
      d_fB = B_tp1 - .data[[B]],
      d_X  = .data[[A]] - .data[[B]]
    ) %>%
    filter(is.finite(d_fB), is.finite(d_X), d_X != 0)
  if (!nrow(d)) {
    return(tibble(theme = theme, A = A, B = B, n_rows = 0L,
                  n_topics = n_distinct(wide$allele),
                  m_hat = NA_real_, self_retention = NA_real_, denom = 0))
  }
  num   <- sum(d$d_fB * d$d_X)
  denom <- sum(d$d_X^2)
  m_hat <- if (denom > 0) num / denom else NA_real_
  if (clip01 && is.finite(m_hat)) m_hat <- min(1, max(0, m_hat))
  tibble(theme = theme, A = A, B = B,
         n_rows = nrow(d),
         n_topics = n_distinct(d$allele),
         m_hat = m_hat,
         self_retention = ifelse(is.na(m_hat), NA_real_, 1 - m_hat),
         denom = denom)
}

# bootstrap
estimate_m_pairwise_boot <- function(allele_freq_df_actor, theme, A, B, bins, topics = NULL, R = 2000, seed = 1L) {
  set.seed(seed)
  if (is.null(topics)) {
    topics <- allele_freq_df_actor %>% filter(theme == !!theme) %>% pull(allele) %>% unique()
  }
  point <- estimate_m_pairwise(allele_freq_df_actor, theme, A, B, bins, topics)
  if (length(topics) < 2L || is.na(point$m_hat)) {
    return(point %>% mutate(ci_low = NA_real_, ci_high = NA_real_, R = 0L))
  }
  ms <- numeric(R)
  for (r in seq_len(R)) {
    boot_topics <- sample(topics, replace = TRUE, size = length(topics))
    ms[r] <- estimate_m_pairwise(allele_freq_df_actor, theme, A, B, bins, topics = boot_topics)$m_hat
  }
  qs <- stats::quantile(ms[is.finite(ms)], c(0.025, 0.975), na.rm = TRUE)
  point %>% mutate(ci_low = unname(qs[1]), ci_high = unname(qs[2]), R = R)
}

# run pairs
run_pairwise_by_theme <- function(allele_freq_df_actor, bins,themes = unique(allele_freq_df_actor$theme),actors = actors_focus,R = 2000, seed = 1L) {
  out <- list()
  for (th in themes) {
    topics_th <- allele_freq_df_actor %>% filter(theme == th) %>% pull(allele) %>% unique()
    for (A in actors) for (B in actors) {
      if (A == B) next
      est <- estimate_m_pairwise(allele_freq_df_actor, th, A, B, bins, topics = topics_th)
      estb <- estimate_m_pairwise_boot(allele_freq_df_actor, th, A, B, bins,
                                       topics = topics_th, R = R, seed = seed)
      # keep the CI columns from estb (identical ids)
      est$ci_low  <- estb$ci_low
      est$ci_high <- estb$ci_high
      est$R       <- estb$R
      out[[length(out)+1]] <- est
    }
  }
  dplyr::bind_rows(out) %>%
    arrange(theme, A, B)
}

# actually run the functions
themes <- c("Human_wildlife_conflict","Law_detailed","Recovery_goals","State_of_science")
res_mig <- run_pairwise_by_theme(allele_freq_df_actor, bins,themes = themes,actors = actors_focus,R = 2000, seed = 42)

# output
res_mig %>%
  mutate(sig = ifelse(is.finite(ci_low) & ci_low > 0, "increase",
                      ifelse(is.finite(ci_high) & ci_high < 0, "decrease", "ns"))) %>%
  select(theme, A, B, m_hat, ci_low, ci_high, n_rows, n_topics, sig) %>%
  arrange(theme, desc(m_hat)) %>%
  print(n = 100)

plot_df <- res_mig %>%
  mutate(A = factor(A, levels = actors_focus),
         B = factor(B, levels = actors_focus),
         lab = ifelse(is.na(m_hat), "–",
                      sprintf("%.2f\n[%0.2f,%0.2f]", m_hat, ci_low, ci_high)))
ggplot(plot_df, aes(A, B, fill = m_hat)) +
  geom_tile(color = "white") +
  geom_text(aes(label = lab), size = 3) +
  scale_fill_gradient(low = "grey95", high = "steelblue", limits = c(0,1), na.value = "grey90",
                      name = "m (A→B)") +
  facet_wrap(~ theme) +
  labs(title = "Pairwise island-model migration rates by theme",
       subtitle = "Pooled over all topics within each theme; labels show m and 95% bootstrap CI",
       x = "Source A", y = "Target B") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(face = "bold"))

#migration rate output table
# build the (theme, A, B) grid
grid_pairs <- tidyr::expand_grid(
  theme = themes,
  A = actors_focus,
  B = actors_focus
) %>% 
  filter(A != B)
events_df <- purrr::pmap_dfr(
  grid_pairs,
  function(theme, A, B) {
    count_events_pairwise(allele_freq_df_actor, theme, A, B, bins)
  }
)

# join to your res_mig 
res_with_events <- res_mig %>%
  left_join(events_df, by = c("theme","A","B")) %>%
  mutate(
    m_ci = ifelse(is.na(m_hat), "–",
                  sprintf("%.2f [%0.2f, %0.2f]", m_hat, ci_low, ci_high))
  ) %>%
  select(theme, A, B, m_hat, m_ci, n_rows, n_topics, events) %>%
  arrange(theme, desc(m_hat), desc(events))

# table per theme
for (th in unique(res_with_events$theme)) {
  cat("\n\n### ", th, "\n", sep = "")
  df_th <- res_with_events %>% filter(theme == th) %>% select(-theme)
  print(knitr::kable(
    df_th,
    digits = 2,
    col.names = c("A", "B", "m_hat", "m [95% CI]", "topic–bins", "topics", "events")
  ))
}

# export
table_export <- res_with_events %>%
  rename(
    `Theme` = theme,
    `Source_A` = A,
    `Target_B` = B,
    `m_hat` = m_hat,
    `m_95CI` = m_ci,
    `Topic_bin_transitions` = n_rows,
    `Distinct_topics` = n_topics,
    `Events` = events
  ) %>%
  arrange(Theme, desc(m_hat), desc(Events))

# write to CSV
write_csv(table_export, "migration_rates_by_theme.csv")

