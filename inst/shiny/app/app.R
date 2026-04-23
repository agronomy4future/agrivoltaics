if(!require(lme4))         install.packages("lme4");         library(lme4)
if(!require(lmerTest))     install.packages("lmerTest");     library(lmerTest)
if(!require(shiny))        install.packages("shiny");        library(shiny)
if(!require(shinydashboard)) install.packages("shinydashboard"); library(shinydashboard)
if(!require(emmeans))      install.packages("emmeans");      library(emmeans)
if(!require(multcomp))     install.packages("multcomp");     library(multcomp)
if(!require(multcompView)) install.packages("multcompView"); library(multcompView)
if(!require(ggplot2))      install.packages("ggplot2");      library(ggplot2)
if(!require(DT))           install.packages("DT");           library(DT)
if(!require(readr))        install.packages("readr");        library(readr)
if(!require(dplyr))        install.packages("dplyr");        library(dplyr)
if(!require(readxl))       install.packages("readxl");       library(readxl)


# ─────────────────────────────────────────────
#  CASE DESCRIPTIONS
# ─────────────────────────────────────────────
CASE_DESC <- c(
  "1" = "Single crop · single row",
  "2" = "Single crop · multiple rows",
  "3" = "Single crop · single row · multiple seasons",
  "4" = "Single crop · multiple rows · multiple seasons",
  "5" = "Multiple cultivars · single row",
  "6" = "Multiple cultivars · multiple rows",
  "7" = "Multiple cultivars · single row · multiple seasons",
  "8" = "Multiple cultivars · multiple rows · multiple seasons"
)

# ─────────────────────────────────────────────
#  FORMULA BUILDER
#  Block independent = AV and Control have different block names
#  Block dependent   = AV and Control share the same block names
#  Replicate         = more than one observation per Block (or Block:Row)
#  Annual            = new plants each season
#  Perennial         = same plants measured across seasons (requires >= 4 seasons)
# ─────────────────────────────────────────────
build_formula <- function(v) {
  has <- function(k) !is.null(v[[k]]) && nzchar(v[[k]]) && v[[k]] != "none"

  # 특수문자 포함 컬럼명 backtick 처리
  bt <- function(x) if (grepl("[^a-zA-Z0-9_.]", x)) paste0("`", x, "`") else x
  Y      <- bt(v$y)
  SITE   <- bt(v$site)
  BLOCK  <- bt(v$block)
  ROW1   <- if (has("row"))      v$row      else NULL
  SEASON <- if (has("season"))   v$season   else NULL
  GENO   <- if (has("genotype")) v$genotype else NULL

  block_dep   <- isTRUE(v$block_type == "dependent")
  has_rep     <- isTRUE(v$has_replicates)
  is_peren    <- isTRUE(v$crop_type == "perennial")
  n_season    <- if (!is.null(v$n_season)) as.integer(v$n_season) else 0L
  peren_valid <- is_peren && n_season >= 4

  has_row    <- !is.null(ROW1)
  has_season <- !is.null(SEASON)
  has_geno   <- !is.null(GENO)

  case_num <-
    if      (!has_geno && !has_row && !has_season) 1L
    else if (!has_geno &&  has_row && !has_season) 2L
    else if (!has_geno && !has_row &&  has_season) 3L
    else if (!has_geno &&  has_row &&  has_season) 4L
    else if ( has_geno && !has_row && !has_season) 5L
    else if ( has_geno &&  has_row && !has_season) 6L
    else if ( has_geno && !has_row &&  has_season) 7L
    else                                           8L

  fixed <- switch(as.character(case_num),
    "1" = SITE,
    "2" = c(SITE, ROW1, sprintf("%s:%s", SITE, ROW1)),
    "3" = c(SEASON, SITE, sprintf("%s:%s", SEASON, SITE)),
    "4" = c(SEASON, SITE, ROW1,
            sprintf("%s:%s", SEASON, SITE),
            sprintf("%s:%s", SEASON, ROW1),
            sprintf("%s:%s:%s", SEASON, SITE, ROW1)),
    "5" = c(SITE, GENO, sprintf("%s:%s", SITE, GENO)),
    "6" = sprintf("%s * %s * %s", SITE, GENO, ROW1),
    "7" = sprintf("%s * %s * %s", SITE, GENO, SEASON),
    "8" = sprintf("%s * %s * %s * %s", SITE, GENO, SEASON, ROW1)
  )

  random <- if (!has_rep) {
    switch(as.character(case_num),
      "1" = BLOCK,
      "2" = if (block_dep) BLOCK else BLOCK,
      "3" = BLOCK,
      "4" = if (block_dep) BLOCK else BLOCK,
      "5" = if (block_dep) BLOCK else BLOCK,
      "6" = if (block_dep) BLOCK else BLOCK,
      "7" = BLOCK,
      "8" = if (block_dep) BLOCK else BLOCK
    )
  } else {
    switch(as.character(case_num),
      "1" = sprintf("(1|%s:%s)", SITE, BLOCK),
      "2" = if (block_dep)
              c(sprintf("(1|%s)", BLOCK), sprintf("(1|%s:%s:%s)", SITE, BLOCK, ROW1))
            else
              c(sprintf("(1|%s)", BLOCK), sprintf("(1|%s:%s)", BLOCK, ROW1)),
      "3" = if (block_dep) {
              if (peren_valid)
                c(sprintf("(1|%s)", BLOCK), sprintf("(1|%s:%s:%s)", SITE, BLOCK, SEASON))
              else
                sprintf("(1|%s)", BLOCK)
            } else {
              sprintf("(1|%s:%s)", SITE, BLOCK)
            },
      "4" = if (block_dep) {
              if (peren_valid)
                c(sprintf("(1|%s)", BLOCK), sprintf("(1|%s:%s:%s:%s)", SITE, BLOCK, ROW1, SEASON))
              else
                c(sprintf("(1|%s)", BLOCK), sprintf("(1|%s:%s:%s)", SITE, BLOCK, ROW1))
            } else {
              if (peren_valid)
                c(sprintf("(1|%s:%s)", SITE, BLOCK), sprintf("(1|%s:%s:%s:%s)", SITE, BLOCK, ROW1, SEASON))
              else
                c(sprintf("(1|%s:%s)", SITE, BLOCK), sprintf("(1|%s:%s)", BLOCK, ROW1))
            },
      "5" = if (block_dep) sprintf("(1|%s)", BLOCK) else sprintf("(1|%s:%s)", SITE, BLOCK),
      "6" = if (block_dep)
              c(sprintf("(1|%s)", BLOCK), sprintf("(1|%s:%s:%s)", SITE, BLOCK, ROW1))
            else
              c(sprintf("(1|%s:%s)", SITE, BLOCK), sprintf("(1|%s:%s)", BLOCK, ROW1)),
      "7" = if (block_dep) {
              if (peren_valid)
                c(sprintf("(1|%s)", BLOCK), sprintf("(1|%s:%s:%s)", SITE, BLOCK, SEASON))
              else
                sprintf("(1|%s)", BLOCK)
            } else {
              sprintf("(1|%s:%s)", SITE, BLOCK)
            },
      "8" = if (block_dep) {
              if (peren_valid)
                c(sprintf("(1|%s)", BLOCK), sprintf("(1|%s:%s:%s:%s)", SITE, BLOCK, ROW1, SEASON))
              else
                c(sprintf("(1|%s)", BLOCK), sprintf("(1|%s:%s:%s)", SITE, BLOCK, ROW1))
            } else {
              if (peren_valid)
                c(sprintf("(1|%s:%s)", SITE, BLOCK), sprintf("(1|%s:%s:%s:%s)", SITE, BLOCK, ROW1, SEASON))
              else
                c(sprintf("(1|%s:%s)", SITE, BLOCK), sprintf("(1|%s:%s)", BLOCK, ROW1))
            }
    )
  }

  use_lm <- !has_rep
  formula_str <- if (use_lm)
    sprintf("%s ~ %s + %s", Y, paste(fixed, collapse=" + "), random)
  else
    sprintf("%s ~ %s + %s", Y, paste(fixed, collapse=" + "), paste(random, collapse=" + "))
  model_fn <- if (use_lm) "lm" else "lmer"

  warnings <- character(0)
  if (!has_rep)
    warnings <- c(warnings,
      "⚠ No replicates detected. Block is used as a fixed effect and lm() is applied.")
  if (!has_rep && case_num == 1 && !block_dep)
    warnings <- c(warnings,
      "⚠ Even though block is independent, Block (shared names) is used due to confounding with Site.")
  if (has_season && is_peren && n_season < 4)
    warnings <- c(warnings,
      sprintf("⚠ Perennial selected but only %d season(s) detected. Minimum 4 seasons required. Annual model applied instead.", n_season))
  if (case_num %in% 6:8)
    warnings <- c(warnings,
      "⚠ Plot variable not used: including Plot would absorb Genotype variance into random effects, removing Genotype significance.")

  list(formula_str=formula_str, model_fn=model_fn, case_num=case_num,
       fixed=fixed, random=random, warnings=warnings, peren_valid=peren_valid)
}

# ─────────────────────────────────────────────
#  LAYOUT SVG GENERATOR
# ─────────────────────────────────────────────

# ─────────────────────────────────────────────
#  HELPER FUNCTIONS
# ─────────────────────────────────────────────
round_df <- function(df, digits=3) {
  num_cols <- sapply(df, is.numeric)
  df[num_cols] <- lapply(df[num_cols], round, digits=digits)
  df
}

run_analysis_for_df <- function(df_loc, v) {
  # 특수문자 컬럼명 backtick 처리는 build_formula 에서 처리됨
  result <- build_formula(v)
  # 모든 문자형 컬럼 factor 변환
  for (col in colnames(df_loc))
    if (is.character(df_loc[[col]])) df_loc[[col]] <- as.factor(df_loc[[col]])
  fac_cols <- unique(c(v$site, v$block,
    if (v$genotype != "none") v$genotype,
    if (v$row      != "none") v$row,
    if (v$season   != "none") v$season))
  for (col in fac_cols)
    if (col %in% colnames(df_loc)) df_loc[[col]] <- droplevels(as.factor(df_loc[[col]]))
  df_loc[[v$y]] <- as.numeric(df_loc[[v$y]])
  formula_obj <- as.formula(result$formula_str)
  model <- if (result$model_fn == "lm") {
    tryCatch(lm(formula_obj, data=df_loc),
             error=function(e) stop(paste("lm() failed:", e$message)))
  } else {
    tryCatch(lmer(formula_obj, data=df_loc, REML=TRUE),
             error=function(e) stop(paste("lmer() failed:", e$message)))
  }
  vc_tbl <- if (result$model_fn == "lmer") {
    vc <- as.data.frame(VarCorr(model))
    vc_total <- sum(vc$vcov)
    vc$pct  <- round(vc$vcov / vc_total * 100, 2)
    vc$vcov <- round(vc$vcov, 3)
    setNames(vc[, c("grp","vcov","pct")], c("Groups","Variance","% of Total"))
  } else NULL
  anova_tbl <- as.data.frame(
    if (result$model_fn == "lmer") anova(model, type=3) else anova(model))
  anova_tbl <- round(anova_tbl, 4)
  p_col <- if ("Pr(>F)" %in% colnames(anova_tbl)) "Pr(>F)" else NULL
  if (!is.null(p_col))
    anova_tbl$Sig <- ifelse(anova_tbl[[p_col]]<0.001,"***",
                     ifelse(anova_tbl[[p_col]]<0.01, "**",
                     ifelse(anova_tbl[[p_col]]<0.05, "*",
                     ifelse(anova_tbl[[p_col]]<0.1,  ".", "ns"))))
  fixed_terms       <- trimws(unlist(strsplit(paste(result$fixed, collapse="+"), "\\+")))
  interaction_terms <- fixed_terms[grepl(":", fixed_terms)]
  default_term      <- if (length(interaction_terms) > 0)
                         interaction_terms[which.max(nchar(interaction_terms))]
                       else v$site
  # df_loc 저장 안함 (메모리 절약)
  rm(df_loc)
  gc()
  list(model=model, vc_tbl=vc_tbl, anova_tbl=anova_tbl,
       anova_terms=rownames(anova_tbl), default_term=default_term,
       formula_str=result$formula_str, model_fn=result$model_fn,
       case_num=result$case_num, warnings=result$warnings, v=v)
}

get_layout_svg <- function(case_num, v) {
  has <- function(k) !is.null(v[[k]]) && v[[k]] != "" && v[[k]] != "none"
  site_l   <- if(has("site"))     v$site     else "Site"
  block_l  <- if(has("block"))    v$block    else "Block"
  geno_l   <- if(has("genotype")) v$genotype else "Genotype"
  row_l    <- if(has("row"))      v$row      else "Row"
  season_l <- if(has("season"))   v$season   else "Season"

  svgs <- list(
    "1" = '
<svg viewBox="0 0 520 210" xmlns="http://www.w3.org/2000/svg" style="max-width:100%;font-family:monospace">
  <text x="260" y="16" text-anchor="middle" fill="#e6edf3" font-size="11" font-weight="bold">Single Crop · Single Row</text>
  <rect x="18" y="26" width="90" height="12" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="1"/>
  <text x="63" y="36" text-anchor="middle" fill="#f0a832" font-size="8" font-weight="bold">☀ Solar Panels</text>
  <line x1="10" y1="44" x2="10" y2="100" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="44" x2="16" y2="44" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="100" x2="16" y2="100" stroke="#8b949e" stroke-width="1.5"/>
  <text x="6" y="75" text-anchor="middle" fill="#8b949e" font-size="7" transform="rotate(-90,6,75)">Block 1</text>
  <rect x="18" y="44" width="90" height="56" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="63" y="65" text-anchor="middle" fill="#ff6b6b" font-size="9" font-weight="bold">AV</text>
  <text x="63" y="79" text-anchor="middle" fill="#8b949e" font-size="7">Block 1 = Plot 1</text>
  <rect x="118" y="44" width="90" height="56" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="163" y="65" text-anchor="middle" fill="#00d4aa" font-size="9" font-weight="bold">Control</text>
  <text x="163" y="79" text-anchor="middle" fill="#8b949e" font-size="7">Block 1 = Plot 1</text>
  <line x1="10" y1="108" x2="10" y2="164" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="108" x2="16" y2="108" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="164" x2="16" y2="164" stroke="#8b949e" stroke-width="1.5"/>
  <text x="6" y="139" text-anchor="middle" fill="#8b949e" font-size="7" transform="rotate(-90,6,139)">Block 2</text>
  <rect x="18" y="108" width="90" height="56" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="63" y="129" text-anchor="middle" fill="#ff6b6b" font-size="9" font-weight="bold">AV</text>
  <text x="63" y="143" text-anchor="middle" fill="#8b949e" font-size="7">Block 2 = Plot 2</text>
  <rect x="118" y="108" width="90" height="56" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="163" y="129" text-anchor="middle" fill="#00d4aa" font-size="9" font-weight="bold">Control</text>
  <text x="163" y="143" text-anchor="middle" fill="#8b949e" font-size="7">Block 2 = Plot 2</text>
  <rect x="240" y="44" width="260" height="50" rx="6" fill="#1c2230" stroke="#2a3441" stroke-width="1"/>
  <text x="370" y="62" text-anchor="middle" fill="#8b949e" font-size="9">Random effect:</text>
  <text x="370" y="78" text-anchor="middle" fill="#00d4aa" font-size="10" font-weight="bold">(1 | Site:Block)</text>
  <rect x="240" y="104" width="260" height="34" rx="6" fill="#1c2230" stroke="#2a3441" stroke-width="1"/>
  <text x="370" y="118" text-anchor="middle" fill="#8b949e" font-size="9">Fixed effect:</text>
  <text x="370" y="132" text-anchor="middle" fill="#ff6b6b" font-size="10" font-weight="bold">Site</text>
  <rect x="240" y="148" width="260" height="36" rx="5" fill="#f0a83220" stroke="#f0a832" stroke-width="1"/>
  <text x="370" y="163" text-anchor="middle" fill="#f0a832" font-size="9">⚠ Treatment confounded with space</text>
  <text x="370" y="177" text-anchor="middle" fill="#f0a832" font-size="9">→ LMM accounts for this</text>
  <rect x="18" y="172" width="190" height="26" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1"/>
  <text x="113" y="187" text-anchor="middle" fill="#00d4aa" font-size="8">lmer(Yield ~ Site + (1|Site:Block))</text>
</svg>',

    "2" = '
<svg viewBox="0 0 540 220" xmlns="http://www.w3.org/2000/svg" style="max-width:100%;font-family:monospace">
  <text x="270" y="15" text-anchor="middle" fill="#e6edf3" font-size="11" font-weight="bold">Single Crop · Multiple Rows</text>
  <rect x="18" y="24" width="200" height="12" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="1"/>
  <text x="118" y="34" text-anchor="middle" fill="#f0a832" font-size="8">☀ Solar Panels</text>
  <line x1="10" y1="42" x2="10" y2="128" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="42" x2="16" y2="42" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="128" x2="16" y2="128" stroke="#8b949e" stroke-width="1.5"/>
  <text x="6" y="85" text-anchor="middle" fill="#8b949e" font-size="7" transform="rotate(-90,6,85)">Block 1</text>
  <rect x="18" y="42" width="200" height="86" rx="6" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="118" y="56" text-anchor="middle" fill="#ff6b6b" font-size="9" font-weight="bold">AV (Block 1 = Plot 1)</text>
  <rect x="22" y="60" width="192" height="22" rx="3" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <text x="118" y="75" text-anchor="middle" fill="#58a6ff" font-size="8">Row = Side1</text>
  <rect x="22" y="84" width="192" height="22" rx="3" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.8"/>
  <text x="118" y="99" text-anchor="middle" fill="#00d4aa" font-size="8">Row = Middle</text>
  <rect x="22" y="108" width="192" height="16" rx="3" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <text x="118" y="120" text-anchor="middle" fill="#58a6ff" font-size="8">Row = Side2</text>
  <rect x="228" y="42" width="200" height="86" rx="6" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="328" y="56" text-anchor="middle" fill="#00d4aa" font-size="9" font-weight="bold">Control (Block 1 = Plot 1)</text>
  <rect x="232" y="60" width="192" height="22" rx="3" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <text x="328" y="75" text-anchor="middle" fill="#58a6ff" font-size="8">Row = Side1</text>
  <rect x="232" y="84" width="192" height="22" rx="3" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.8"/>
  <text x="328" y="99" text-anchor="middle" fill="#00d4aa" font-size="8">Row = Middle</text>
  <rect x="232" y="108" width="192" height="16" rx="3" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <text x="328" y="120" text-anchor="middle" fill="#58a6ff" font-size="8">Row = Side2</text>
  <rect x="18" y="140" width="410" height="26" rx="5" fill="#58a6ff18" stroke="#58a6ff" stroke-width="1"/>
  <text x="223" y="155" text-anchor="middle" fill="#58a6ff" font-size="9">lmer(Yield ~ Site + Row + Site:Row + (1|Block) + (1|Block:Row))</text>
</svg>',

    "3" = '
<svg viewBox="0 0 540 210" xmlns="http://www.w3.org/2000/svg" style="max-width:100%;font-family:monospace">
  <text x="270" y="14" text-anchor="middle" fill="#e6edf3" font-size="11" font-weight="bold">Single Crop · Single Row · Multiple Seasons</text>
  <rect x="18" y="22" width="230" height="13" rx="4" fill="#3fb95033" stroke="#3fb950" stroke-width="1"/>
  <text x="133" y="33" text-anchor="middle" fill="#3fb950" font-size="8" font-weight="bold">Season = 1</text>
  <rect x="18" y="40" width="100" height="12" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="1"/>
  <text x="68" y="50" text-anchor="middle" fill="#f0a832" font-size="8">☀ Solar Panels</text>
  <line x1="10" y1="56" x2="10" y2="110" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="56" x2="16" y2="56" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="110" x2="16" y2="110" stroke="#8b949e" stroke-width="1.5"/>
  <text x="6" y="83" text-anchor="middle" fill="#8b949e" font-size="7" transform="rotate(-90,6,83)">Block 1</text>
  <rect x="18" y="56" width="100" height="54" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="68" y="73" text-anchor="middle" fill="#ff6b6b" font-size="8" font-weight="bold">AV</text>
  <text x="68" y="85" text-anchor="middle" fill="#8b949e" font-size="7">Block 1 = Plot 1</text>
  <rect x="128" y="56" width="100" height="54" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="178" y="73" text-anchor="middle" fill="#00d4aa" font-size="8" font-weight="bold">Control</text>
  <text x="178" y="85" text-anchor="middle" fill="#8b949e" font-size="7">Block 1 = Plot 1</text>
  <line x1="256" y1="83" x2="278" y2="83" stroke="#3fb950" stroke-width="2" stroke-dasharray="4,3"/>
  <text x="267" y="79" text-anchor="middle" fill="#3fb950" font-size="9">→</text>
  <rect x="290" y="22" width="230" height="13" rx="4" fill="#3fb95033" stroke="#3fb950" stroke-width="1"/>
  <text x="405" y="33" text-anchor="middle" fill="#3fb950" font-size="8" font-weight="bold">Season = 2</text>
  <rect x="290" y="40" width="100" height="12" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="1"/>
  <text x="340" y="50" text-anchor="middle" fill="#f0a832" font-size="8">☀ Solar Panels</text>
  <rect x="290" y="56" width="100" height="54" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="340" y="73" text-anchor="middle" fill="#ff6b6b" font-size="8" font-weight="bold">AV</text>
  <text x="340" y="85" text-anchor="middle" fill="#8b949e" font-size="7">Block 1 = Plot 1</text>
  <rect x="400" y="56" width="100" height="54" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="450" y="73" text-anchor="middle" fill="#00d4aa" font-size="8" font-weight="bold">Control</text>
  <text x="450" y="85" text-anchor="middle" fill="#8b949e" font-size="7">Block 1 = Plot 1</text>
  <rect x="18" y="120" width="500" height="26" rx="5" fill="#3fb95033" stroke="#3fb950" stroke-width="1"/>
  <text x="268" y="132" text-anchor="middle" fill="#3fb950" font-size="8" font-weight="bold">Annual crop:</text>
  <text x="268" y="143" text-anchor="middle" fill="#3fb950" font-size="8">lmer(Yield ~ Season + Site + Season:Site + (1|Block))</text>
  <rect x="18" y="150" width="500" height="26" rx="5" fill="#58a6ff18" stroke="#58a6ff" stroke-width="1"/>
  <text x="268" y="162" text-anchor="middle" fill="#58a6ff" font-size="8" font-weight="bold">Perennial crop:</text>
  <text x="268" y="173" text-anchor="middle" fill="#58a6ff" font-size="8">lmer(Yield ~ Season + Site + Season:Site + (1|Block) + (1|Site:Block:Season))</text>
</svg>',

    "5" = '
<svg viewBox="0 0 560 220" xmlns="http://www.w3.org/2000/svg" style="max-width:100%;font-family:monospace">
  <text x="280" y="14" text-anchor="middle" fill="#e6edf3" font-size="11" font-weight="bold">Multiple Cultivars · Single Row</text>
  <rect x="18" y="22" width="240" height="12" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="1"/>
  <text x="138" y="32" text-anchor="middle" fill="#f0a832" font-size="8">☀ Solar Panels</text>
  <rect x="18" y="40" width="240" height="80" rx="6" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="138" y="56" text-anchor="middle" fill="#ff6b6b" font-size="9" font-weight="bold">AV</text>
  <rect x="26" y="62" width="106" height="50" rx="5" fill="#bc8cff22" stroke="#bc8cff" stroke-width="1.2"/>
  <text x="79" y="82" text-anchor="middle" fill="#bc8cff" font-size="9" font-weight="bold">Cultivar = cv1</text>
  <text x="79" y="96" text-anchor="middle" fill="#8b949e" font-size="8">Plot</text>
  <rect x="140" y="62" width="106" height="50" rx="5" fill="#f0a83233" stroke="#f0a832" stroke-width="1.2"/>
  <text x="193" y="82" text-anchor="middle" fill="#f0a832" font-size="9" font-weight="bold">Cultivar = cv2</text>
  <text x="193" y="96" text-anchor="middle" fill="#8b949e" font-size="8">Plot</text>
  <rect x="296" y="40" width="240" height="80" rx="6" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="416" y="56" text-anchor="middle" fill="#00d4aa" font-size="9" font-weight="bold">Control</text>
  <rect x="304" y="62" width="106" height="50" rx="5" fill="#bc8cff22" stroke="#bc8cff" stroke-width="1.2"/>
  <text x="357" y="82" text-anchor="middle" fill="#bc8cff" font-size="9" font-weight="bold">Cultivar = cv1</text>
  <text x="357" y="96" text-anchor="middle" fill="#8b949e" font-size="8">Plot</text>
  <rect x="418" y="62" width="106" height="50" rx="5" fill="#f0a83233" stroke="#f0a832" stroke-width="1.2"/>
  <text x="471" y="82" text-anchor="middle" fill="#f0a832" font-size="9" font-weight="bold">Cultivar = cv2</text>
  <text x="471" y="96" text-anchor="middle" fill="#8b949e" font-size="8">Plot</text>
  <line x1="8" y1="40" x2="8" y2="120" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="8" y1="40" x2="14" y2="40" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="8" y1="120" x2="14" y2="120" stroke="#8b949e" stroke-width="1.5"/>
  <text x="4" y="83" text-anchor="middle" fill="#8b949e" font-size="7" transform="rotate(-90,4,83)">Block 1</text>
  <text x="18" y="155" fill="#00d4aa" font-size="8">lmer(Yield ~ Site + Cultivar + Site:Cultivar + (1|Block))</text>
</svg>',

    "8" = '
<svg viewBox="0 0 580 280" xmlns="http://www.w3.org/2000/svg" style="max-width:100%;font-family:monospace">
  <text x="290" y="13" text-anchor="middle" fill="#e6edf3" font-size="11" font-weight="bold">Multiple Cultivars · Multiple Rows · Multiple Seasons</text>
  <rect x="16" y="20" width="262" height="12" rx="3" fill="#3fb95033" stroke="#3fb950" stroke-width="1"/>
  <text x="147" y="30" text-anchor="middle" fill="#3fb950" font-size="8">Season = 1</text>
  <line x1="8" y1="36" x2="8" y2="136" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="8" y1="36" x2="14" y2="36" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="8" y1="136" x2="14" y2="136" stroke="#8b949e" stroke-width="1.5"/>
  <text x="4" y="86" text-anchor="middle" fill="#8b949e" font-size="7" transform="rotate(-90,4,86)">Block 1</text>
  <rect x="16" y="36" width="120" height="100" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.2"/>
  <text x="76" y="50" text-anchor="middle" fill="#ff6b6b" font-size="8" font-weight="bold">AV</text>
  <rect x="22" y="54" width="52" height="76" rx="3" fill="#bc8cff22" stroke="#bc8cff" stroke-width="0.8"/>
  <text x="48" y="66" text-anchor="middle" fill="#bc8cff" font-size="7">cv1</text>
  <rect x="26" y="70" width="44" height="18" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <text x="48" y="82" text-anchor="middle" fill="#58a6ff" font-size="6">Row</text>
  <rect x="26" y="90" width="44" height="18" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.6"/>
  <text x="48" y="102" text-anchor="middle" fill="#00d4aa" font-size="6">Row</text>
  <rect x="26" y="110" width="44" height="16" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <text x="48" y="121" text-anchor="middle" fill="#58a6ff" font-size="6">Row</text>
  <rect x="78" y="54" width="52" height="76" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="0.8"/>
  <text x="104" y="66" text-anchor="middle" fill="#f0a832" font-size="7">cv2</text>
  <rect x="82" y="70" width="44" height="18" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <rect x="82" y="90" width="44" height="18" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.6"/>
  <rect x="82" y="110" width="44" height="16" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <rect x="144" y="36" width="120" height="100" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.2"/>
  <text x="204" y="50" text-anchor="middle" fill="#00d4aa" font-size="8" font-weight="bold">Control</text>
  <rect x="150" y="54" width="52" height="76" rx="3" fill="#bc8cff22" stroke="#bc8cff" stroke-width="0.8"/>
  <text x="176" y="66" text-anchor="middle" fill="#bc8cff" font-size="7">cv1</text>
  <rect x="154" y="70" width="44" height="18" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <rect x="154" y="90" width="44" height="18" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.6"/>
  <rect x="154" y="110" width="44" height="16" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <rect x="206" y="54" width="52" height="76" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="0.8"/>
  <text x="232" y="66" text-anchor="middle" fill="#f0a832" font-size="7">cv2</text>
  <rect x="210" y="70" width="44" height="18" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <rect x="210" y="90" width="44" height="18" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.6"/>
  <rect x="210" y="110" width="44" height="16" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <line x1="256" y1="86" x2="296" y2="86" stroke="#3fb950" stroke-width="2" stroke-dasharray="4,3"/>
  <rect x="298" y="20" width="262" height="12" rx="3" fill="#3fb95033" stroke="#3fb950" stroke-width="1"/>
  <text x="429" y="30" text-anchor="middle" fill="#3fb950" font-size="8">Season = 2</text>
  <rect x="298" y="36" width="120" height="100" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.2"/>
  <text x="358" y="50" text-anchor="middle" fill="#ff6b6b" font-size="8" font-weight="bold">AV</text>
  <rect x="304" y="54" width="52" height="76" rx="3" fill="#bc8cff22" stroke="#bc8cff" stroke-width="0.8"/>
  <text x="330" y="66" text-anchor="middle" fill="#bc8cff" font-size="7">cv1</text>
  <rect x="308" y="70" width="44" height="18" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <rect x="308" y="90" width="44" height="18" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.6"/>
  <rect x="308" y="110" width="44" height="16" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.6"/>
  <rect x="360" y="54" width="52" height="76" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="0.8"/>
  <text x="386" y="66" text-anchor="middle" fill="#f0a832" font-size="7">cv2</text>
  <rect x="426" y="36" width="120" height="100" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.2"/>
  <text x="486" y="50" text-anchor="middle" fill="#00d4aa" font-size="8" font-weight="bold">Control</text>
  <rect x="432" y="54" width="52" height="76" rx="3" fill="#bc8cff22" stroke="#bc8cff" stroke-width="0.8"/>
  <text x="458" y="66" text-anchor="middle" fill="#bc8cff" font-size="7">cv1</text>
  <rect x="488" y="54" width="52" height="76" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="0.8"/>
  <text x="514" y="66" text-anchor="middle" fill="#f0a832" font-size="7">cv2</text>
  <text x="8" y="149" fill="#00d4aa" font-size="7">lmer(Yield ~ Site * Cultivar * Season * Row + (1|Block) + (1|Site:Block:Row:Season), data = df)</text>
  <rect x="16" y="158" width="544" height="80" rx="6" fill="#1c2230" stroke="#2a3441" stroke-width="1"/>
  <text x="288" y="172" text-anchor="middle" fill="#e6edf3" font-size="9" font-weight="bold">Most complex layout — multiple cultivars, rows, and seasons</text>
  <text x="288" y="185" text-anchor="middle" fill="#8b949e" font-size="9">Site:Block:Row:Season captures all spatial x temporal variation</text>
  <text x="288" y="198" text-anchor="middle" fill="#8b949e" font-size="9">Use * operator to include all interactions and avoid rank deficiency</text>
  <text x="288" y="212" text-anchor="middle" fill="#f0a832" font-size="8">Plot variable not used: would absorb Genotype variance</text>
</svg>'
  )

  svg4 <- '
<svg viewBox="0 0 560 165" xmlns="http://www.w3.org/2000/svg" style="max-width:100%;font-family:monospace">
  <text x="280" y="15" text-anchor="middle" fill="#e6edf3" font-size="11" font-weight="bold">Single Crop · Multiple Rows · Multiple Seasons</text>
  <rect x="18" y="22" width="240" height="12" rx="3" fill="#3fb95033" stroke="#3fb950" stroke-width="1"/>
  <text x="138" y="32" text-anchor="middle" fill="#3fb950" font-size="8">Season = 1</text>
  <rect x="18" y="36" width="110" height="8" rx="2" fill="#f0a83233" stroke="#f0a832" stroke-width="1"/>
  <text x="73" y="44" text-anchor="middle" fill="#f0a832" font-size="7">☀ Solar Panels</text>
  <line x1="10" y1="46" x2="10" y2="116" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="46" x2="16" y2="46" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="116" x2="16" y2="116" stroke="#8b949e" stroke-width="1.5"/>
  <text x="6" y="81" text-anchor="middle" fill="#8b949e" font-size="7" transform="rotate(-90,6,81)">Block 1</text>
  <rect x="18" y="46" width="110" height="70" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="73" y="57" text-anchor="middle" fill="#ff6b6b" font-size="7" font-weight="bold">AV</text>
  <rect x="22" y="62" width="102" height="14" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <text x="73" y="73" text-anchor="middle" fill="#58a6ff" font-size="7">Row = Side1</text>
  <rect x="22" y="78" width="102" height="14" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.8"/>
  <text x="73" y="89" text-anchor="middle" fill="#00d4aa" font-size="7">Row = Middle</text>
  <rect x="22" y="94" width="102" height="14" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <text x="73" y="105" text-anchor="middle" fill="#58a6ff" font-size="7">Row = Side2</text>
  <rect x="136" y="46" width="110" height="70" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="191" y="57" text-anchor="middle" fill="#00d4aa" font-size="7" font-weight="bold">Control</text>
  <rect x="140" y="62" width="102" height="14" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <rect x="140" y="78" width="102" height="14" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.8"/>
  <rect x="140" y="94" width="102" height="14" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <line x1="256" y1="81" x2="274" y2="81" stroke="#3fb950" stroke-width="2" stroke-dasharray="4,3"/>
  <rect x="276" y="22" width="240" height="12" rx="3" fill="#3fb95033" stroke="#3fb950" stroke-width="1"/>
  <text x="396" y="32" text-anchor="middle" fill="#3fb950" font-size="8">Season = 2</text>
  <rect x="276" y="36" width="110" height="8" rx="2" fill="#f0a83233" stroke="#f0a832" stroke-width="1"/>
  <rect x="276" y="46" width="110" height="70" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="331" y="57" text-anchor="middle" fill="#ff6b6b" font-size="7" font-weight="bold">AV</text>
  <rect x="280" y="62" width="102" height="14" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <rect x="280" y="78" width="102" height="14" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.8"/>
  <rect x="280" y="94" width="102" height="14" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <rect x="394" y="46" width="110" height="70" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="449" y="57" text-anchor="middle" fill="#00d4aa" font-size="7" font-weight="bold">Control</text>
  <rect x="398" y="62" width="102" height="14" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <rect x="398" y="78" width="102" height="14" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.8"/>
  <rect x="398" y="94" width="102" height="14" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <text x="18" y="130" fill="#3fb950" font-size="7">Annual: lmer(Yield ~ Season+Site+Row+Season:Site+Season:Row+Season:Site:Row + (1|Site:Block)+(1|Block:Row))</text>
  <text x="18" y="143" fill="#58a6ff" font-size="7">Perennial: lmer(Yield ~ Season+Site+Row+Season:Site+Season:Row+Season:Site:Row + (1|Block)+(1|Site:Block:Row:Season))</text>
</svg>'
  if (case_num == 4) return(svg4)

  svg6 <- '
<svg viewBox="0 0 560 200" xmlns="http://www.w3.org/2000/svg" style="max-width:100%;font-family:monospace">
  <text x="280" y="14" text-anchor="middle" fill="#e6edf3" font-size="11" font-weight="bold">Multiple Cultivars · Multiple Rows</text>
  <rect x="18" y="22" width="240" height="12" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="1"/>
  <text x="138" y="32" text-anchor="middle" fill="#f0a832" font-size="8">☀ Solar Panels (AV)</text>
  <rect x="18" y="40" width="240" height="98" rx="6" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="138" y="56" text-anchor="middle" fill="#ff6b6b" font-size="9" font-weight="bold">AV Site</text>
  <rect x="26" y="62" width="106" height="68" rx="5" fill="#bc8cff22" stroke="#bc8cff" stroke-width="1.2"/>
  <text x="79" y="76" text-anchor="middle" fill="#bc8cff" font-size="9" font-weight="bold">Cultivar = cv1</text>
  <rect x="30" y="80" width="98" height="14" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <text x="79" y="91" text-anchor="middle" fill="#58a6ff" font-size="7">Row = Side1</text>
  <rect x="30" y="96" width="98" height="14" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.8"/>
  <text x="79" y="107" text-anchor="middle" fill="#00d4aa" font-size="7">Row = Middle</text>
  <rect x="30" y="112" width="98" height="14" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <text x="79" y="123" text-anchor="middle" fill="#58a6ff" font-size="7">Row = Side2</text>
  <rect x="140" y="62" width="106" height="68" rx="5" fill="#f0a83233" stroke="#f0a832" stroke-width="1.2"/>
  <text x="193" y="76" text-anchor="middle" fill="#f0a832" font-size="9" font-weight="bold">Cultivar = cv2</text>
  <rect x="144" y="80" width="98" height="14" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <rect x="144" y="96" width="98" height="14" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.8"/>
  <rect x="144" y="112" width="98" height="14" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <rect x="296" y="40" width="240" height="98" rx="6" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="416" y="56" text-anchor="middle" fill="#00d4aa" font-size="9" font-weight="bold">Control Site</text>
  <rect x="304" y="62" width="106" height="68" rx="5" fill="#bc8cff22" stroke="#bc8cff" stroke-width="1.2"/>
  <text x="357" y="76" text-anchor="middle" fill="#bc8cff" font-size="9" font-weight="bold">Cultivar = cv1</text>
  <rect x="308" y="80" width="98" height="14" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <rect x="308" y="96" width="98" height="14" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.8"/>
  <rect x="308" y="112" width="98" height="14" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <rect x="418" y="62" width="106" height="68" rx="5" fill="#f0a83233" stroke="#f0a832" stroke-width="1.2"/>
  <text x="471" y="76" text-anchor="middle" fill="#f0a832" font-size="9" font-weight="bold">Cultivar = cv2</text>
  <rect x="422" y="80" width="98" height="14" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <rect x="422" y="96" width="98" height="14" rx="2" fill="#00d4aa22" stroke="#00d4aa" stroke-width="0.8"/>
  <rect x="422" y="112" width="98" height="14" rx="2" fill="#58a6ff22" stroke="#58a6ff" stroke-width="0.8"/>
  <line x1="8" y1="40" x2="8" y2="140" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="8" y1="40" x2="14" y2="40" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="8" y1="140" x2="14" y2="140" stroke="#8b949e" stroke-width="1.5"/>
  <text x="4" y="93" text-anchor="middle" fill="#8b949e" font-size="7" transform="rotate(-90,4,93)">Block 1</text>
  <text x="18" y="158" fill="#00d4aa" font-size="8">lmer(Yield ~ Site * Cultivar * Row + (1|Block) + (1|Site:Block:Row))</text>
  <text x="18" y="171" fill="#f0a832" font-size="7">⚠ Plot not used: would absorb Cultivar variance into random effects</text>
</svg>'
  if (case_num == 6) return(svg6)

  svg7 <- '
<svg viewBox="0 0 580 240" xmlns="http://www.w3.org/2000/svg" style="max-width:100%;font-family:monospace">
  <text x="290" y="14" text-anchor="middle" fill="#e6edf3" font-size="11" font-weight="bold">Multiple Cultivars · Single Row · Multiple Seasons</text>
  <rect x="18" y="22" width="252" height="13" rx="4" fill="#3fb95033" stroke="#3fb950" stroke-width="1"/>
  <text x="144" y="33" text-anchor="middle" fill="#3fb950" font-size="8" font-weight="bold">Season = 1</text>
  <rect x="18" y="40" width="252" height="90" rx="6" fill="#1c2230" stroke="#2a3441" stroke-width="1"/>
  <rect x="26" y="50" width="110" height="72" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="81" y="64" text-anchor="middle" fill="#ff6b6b" font-size="8" font-weight="bold">AV</text>
  <rect x="36" y="68" width="44" height="46" rx="3" fill="#bc8cff22" stroke="#bc8cff" stroke-width="1"/>
  <text x="58" y="84" text-anchor="middle" fill="#bc8cff" font-size="7" font-weight="bold">Cultivar=</text>
  <text x="58" y="94" text-anchor="middle" fill="#bc8cff" font-size="7" font-weight="bold">cv1</text>
  <text x="58" y="104" text-anchor="middle" fill="#8b949e" font-size="6">Plot</text>
  <rect x="84" y="68" width="44" height="46" rx="3" fill="#f0a83222" stroke="#f0a832" stroke-width="1"/>
  <text x="106" y="84" text-anchor="middle" fill="#f0a832" font-size="7" font-weight="bold">Cultivar=</text>
  <text x="106" y="94" text-anchor="middle" fill="#f0a832" font-size="7" font-weight="bold">cv2</text>
  <text x="106" y="104" text-anchor="middle" fill="#8b949e" font-size="6">Plot</text>
  <line x1="16" y1="50" x2="16" y2="122" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="16" y1="50" x2="22" y2="50" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="16" y1="122" x2="22" y2="122" stroke="#8b949e" stroke-width="1.5"/>
  <text x="8" y="86" text-anchor="middle" fill="#8b949e" font-size="7" transform="rotate(-90,8,86)">Block</text>
  <rect x="144" y="50" width="118" height="72" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="203" y="64" text-anchor="middle" fill="#00d4aa" font-size="8" font-weight="bold">Control</text>
  <rect x="148" y="68" width="44" height="46" rx="3" fill="#bc8cff22" stroke="#bc8cff" stroke-width="1"/>
  <text x="170" y="84" text-anchor="middle" fill="#bc8cff" font-size="7" font-weight="bold">Cultivar=</text>
  <text x="170" y="94" text-anchor="middle" fill="#bc8cff" font-size="7" font-weight="bold">cv1</text>
  <rect x="196" y="68" width="44" height="46" rx="3" fill="#f0a83222" stroke="#f0a832" stroke-width="1"/>
  <text x="218" y="84" text-anchor="middle" fill="#f0a832" font-size="7" font-weight="bold">Cultivar=</text>
  <text x="218" y="94" text-anchor="middle" fill="#f0a832" font-size="7" font-weight="bold">cv2</text>
  <line x1="278" y1="83" x2="302" y2="83" stroke="#3fb950" stroke-width="2" stroke-dasharray="4,3"/>
  <text x="290" y="79" text-anchor="middle" fill="#3fb950" font-size="9">→</text>
  <rect x="308" y="22" width="252" height="13" rx="4" fill="#3fb95033" stroke="#3fb950" stroke-width="1"/>
  <text x="434" y="33" text-anchor="middle" fill="#3fb950" font-size="8" font-weight="bold">Season = 2</text>
  <rect x="308" y="40" width="252" height="90" rx="6" fill="#1c2230" stroke="#2a3441" stroke-width="1"/>
  <rect x="316" y="50" width="110" height="72" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="371" y="64" text-anchor="middle" fill="#ff6b6b" font-size="8" font-weight="bold">AV</text>
  <rect x="326" y="68" width="44" height="46" rx="3" fill="#bc8cff22" stroke="#bc8cff" stroke-width="1"/>
  <text x="348" y="84" text-anchor="middle" fill="#bc8cff" font-size="7" font-weight="bold">Cultivar=</text>
  <text x="348" y="94" text-anchor="middle" fill="#bc8cff" font-size="7" font-weight="bold">cv1</text>
  <rect x="374" y="68" width="44" height="46" rx="3" fill="#f0a83222" stroke="#f0a832" stroke-width="1"/>
  <text x="396" y="84" text-anchor="middle" fill="#f0a832" font-size="7" font-weight="bold">Cultivar=</text>
  <text x="396" y="94" text-anchor="middle" fill="#f0a832" font-size="7" font-weight="bold">cv2</text>
  <rect x="434" y="50" width="118" height="72" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="493" y="64" text-anchor="middle" fill="#00d4aa" font-size="8" font-weight="bold">Control</text>
  <rect x="438" y="68" width="44" height="46" rx="3" fill="#bc8cff22" stroke="#bc8cff" stroke-width="1"/>
  <text x="460" y="84" text-anchor="middle" fill="#bc8cff" font-size="7" font-weight="bold">Cultivar=</text>
  <text x="460" y="94" text-anchor="middle" fill="#bc8cff" font-size="7" font-weight="bold">cv1</text>
  <rect x="486" y="68" width="44" height="46" rx="3" fill="#f0a83222" stroke="#f0a832" stroke-width="1"/>
  <text x="508" y="84" text-anchor="middle" fill="#f0a832" font-size="7" font-weight="bold">Cultivar=</text>
  <text x="508" y="94" text-anchor="middle" fill="#f0a832" font-size="7" font-weight="bold">cv2</text>
  <text x="18" y="148" fill="#3fb950" font-size="8">Annual: lmer(Yield ~ Site * Cultivar * Season + (1|Block))</text>
  <text x="18" y="161" fill="#58a6ff" font-size="8">Perennial: lmer(Yield ~ Site * Cultivar * Season + (1|Block) + (1|Site:Block:Season))</text>
  <rect x="18" y="170" width="544" height="36" rx="6" fill="#3fb95033" stroke="#3fb950" stroke-width="1"/>
  <text x="290" y="185" text-anchor="middle" fill="#3fb950" font-size="9">⚠ Plot not used: would absorb Cultivar variance into random effects</text>
  <text x="290" y="198" text-anchor="middle" fill="#3fb950" font-size="9">Block:Season accounts for spatial variation that differs between seasons</text>
</svg>'
  if (case_num == 7) return(svg7)

  svg <- svgs[[as.character(case_num)]]
  if (is.null(svg)) return("<p style='color:#8b949e'>Layout diagram not available</p>")
  svg
}

# ─────────────────────────────────────────────
#  UI
# ─────────────────────────────────────────────
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Agrivoltaics Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analysis",      tabName = "analysis", icon = icon("flask")),

      menuItem("About",         tabName = "about",   icon = icon("info-circle"))
    ),
    hr(),
    div(style = "padding: 10px;",

        tags$span(style = "font-size:13px;color:#555;",
          "\u00a9 J.K Kim (kimjk@agronomy4future.com)", br(),
          "All Rights Reserved"),
        br(), br(),
        tags$div(style = "font-size:13px;color:#2c2c2c;line-height:1.8;",
          tags$strong("How to Cite"), br(),
          "Kim, J., 2026.", br(),
          "Agrivoltaics Stat: A web-based", br(),
          "statistical analysis tool for", br(),
          "agrivoltaics field experiments", br(),
          "[Software]. Available at:", br(),
          "agrivoltaics.agronomy4future.com"
        ),
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      body,.content-wrapper,.main-sidebar,.sidebar{background-color:#faf8f3!important;color:#2c2c2c!important}
      .box{background:#f5f2eb!important;border-top-color:#00d4aa!important}
      .box-header{background:#2c3e50!important}
      .box-title{color:#ffffff!important;font-family:monospace}
      .nav-tabs-custom>.tab-content{background:#f5f2eb!important}
      .nav-tabs-custom .nav-tabs li.active a{background:#ede9df!important;color:#00a882!important;border-top-color:#00d4aa!important}
      .nav-tabs-custom .nav-tabs li a{background:#faf8f3!important;color:#666!important}
      table.dataTable{background:#ffffff!important;color:#2c2c2c!important}
      table.dataTable thead{background:#ede9df!important;color:#555!important}
      .dataTables_wrapper{color:#2c2c2c!important}
      .form-control{background:#ffffff!important;color:#2c2c2c!important;border-color:#ccc!important}
      .selectize-input{background:#ffffff!important;color:#2c2c2c!important;border-color:#ccc!important}
      .selectize-dropdown{background:#ffffff!important;color:#2c2c2c!important}
      .selectize-input *{color:#2c2c2c!important}
      .selectize-dropdown *{color:#2c2c2c!important}
      select,select option{color:#2c2c2c!important;background:#ffffff!important}
      input{color:#2c2c2c!important;background:#ffffff!important}
      label{color:#2c2c2c!important}
      .control-label{color:#2c2c2c!important}
      .shiny-input-container{color:#2c2c2c!important}
      .btn-primary{background:#00a882!important;border-color:#007a60!important;color:#ffffff!important;font-weight:bold}
      .btn-default{background:#4a4a4a!important;border-color:#555!important;color:#ffffff!important}
      .btn-file{background:#4a4a4a!important;border-color:#555!important;color:#ffffff!important}
      .formula-box{background:#f0ede4;border:1px solid #2a3441;border-left:3px solid #00d4aa;border-radius:8px;padding:12px 16px;font-family:monospace;font-size:15px;color:#00a882;margin-top:10px;word-break:break-all}
      .case-badge{display:inline-block;background:#f0a83220;border:1px solid #f0a832;color:#f0a832;font-size:14px;font-weight:bold;padding:3px 12px;border-radius:20px;margin-top:8px;font-family:monospace}
      .warn-box{background:#fff3cd;border:1px solid #f0a832;border-radius:8px;padding:10px 14px;font-size:13px;color:#856404;margin-top:8px}
      .section-title{font-size:13px;color:#888;text-transform:uppercase;letter-spacing:.1em;font-weight:bold;margin:16px 0 8px;border-bottom:1px solid #2a3441;padding-bottom:4px}
      .info-notice{background:#e8f4f8;border:1px solid #b0d4e8;border-radius:8px;padding:10px 14px;font-size:15px;color:#555;margin-bottom:14px}
      .skin-black .main-header .logo{background:#ede9df!important;border-bottom:2px solid #00d4aa!important}
      .skin-black .main-header .navbar{background:#ede9df!important}
      .skin-black .main-sidebar{background:#e8e4d9!important}
      .skin-black .sidebar-menu > li > a{color:#2c2c2c!important}
      .skin-black .sidebar-menu > li.active > a{color:#ffffff!important}
      pre{background:#f0ede4!important;color:#555!important;border-color:#ccc!important;font-size:15px}
      table.dataTable td{font-size:15px!important}
      table.dataTable th{font-size:15px!important}
      .dataTables_wrapper{font-size:15px!important}
      .radio label{color:#2c2c2c!important}
      .checkbox label{color:#2c2c2c!important}
    "))),
    tabItems(
      tabItem(tabName = "analysis",
        fluidRow(
          box(width=12, title="Step 01 — Upload Field Data", status="primary", solidHeader=FALSE,
              div(class="info-notice",
                  icon("info-circle"),
                  " Upload a CSV file containing your agrivoltaics field experiment data. ",
                  strong("Block"), " and ", strong("Site (AV/Control)"), " columns are required."),
              fileInput("file", NULL, accept=c(".csv",".xlsx",".xls"),
                        buttonLabel="Browse CSV/Excel...", placeholder="No file selected"),
              uiOutput("sheet_selector_ui"),
              DTOutput("preview_table"))
        ),
        fluidRow(
          box(width=6, title="Step 02 — Assign Variables", status="primary",
              div(class="section-title", "Required Variables"),
              fluidRow(
                column(4, selectInput("sel_y",    "Y — Output variable",  choices=c("— select —"=""))),
                column(4, selectInput("sel_site",  "Site (AV/Control)",   choices=c("— select —"=""))),
                column(4, selectInput("sel_block", "Block",               choices=c("— select —"="")))
              ),
              div(class="section-title", "Optional Variables"),
              fluidRow(
                column(3, selectInput("sel_genotype","Genotype",  choices=c("— not used —"="none"))),
                column(3, selectInput("sel_row",     "Row",       choices=c("— not used —"="none"))),
                column(3, selectInput("sel_season",  "Season",    choices=c("— not used —"="none"))),
                column(3, selectInput("sel_location","Location",  choices=c("— not used —"="none")))
              ),
              uiOutput("location_info_ui"),
              div(class="section-title", "Subset Data (Optional)"),
              uiOutput("subset_block_ui"),
              uiOutput("subset_genotype_ui"),
              uiOutput("subset_row_ui"),
              uiOutput("subset_season_ui"),
              uiOutput("subset_preview_text"),
              div(class="section-title", "Block Structure"),
              div(class="info-notice",
                  "Are block names ", strong("shared"), " between AV and Control?", br(), br(),
                  strong("• Independent:"), " AV and Control sites are spatially separated — assign different block names for each site (e.g., AV: A/B/C/D · Control: I/II/III/IV). Using a unique Plot ID per observation is also valid.", br(), br(),
                  strong("• Dependent:"), " AV and Control plots are located within the same block — both share the same block names (e.g., both use I/II/III/IV)."),
              radioButtons("block_type", label=NULL,
                           choices=c("Independent (different names)"="independent",
                                     "Dependent (shared names)"="dependent"),
                           selected="independent"),
              div(class="section-title", "Replicates"),
              div(class="info-notice",
                  "Does data have replicates? (more than one observation per Block or Block:Row)"),
              radioButtons("has_replicates", label=NULL,
                           choices=c("Yes — replicates exist"="yes",
                                     "No  — one obs per Block/Row"="no"),
                           selected="yes"),
              uiOutput("crop_type_ui"),
              hr(),
              div(class="section-title", "Detected Model Formula"),
              uiOutput("formula_preview"),
              br(),
              actionButton("btn_run", "\u25b6  Run Analysis",
                           class="btn-primary", style="width:100%")),
          box(width=6, title="Field Layout Diagram", status="primary",
              uiOutput("layout_diagram"))
        ),
        uiOutput("results_ui")
      ),

      tabItem(tabName = "about",
        box(width=12, title="About", status="primary",
            h4(strong("Why Agrivoltaics Needs Its Own Statistical Approach")),
            br(),
            div(style="background:#1c2230;border-radius:8px;padding:10px;max-width:600px;",
                HTML('<svg viewBox="0 0 520 210" xmlns="http://www.w3.org/2000/svg" style="max-width:100%;font-family:monospace">
  <text x="260" y="16" text-anchor="middle" fill="#e6edf3" font-size="11" font-weight="bold">Single Crop · Single Row</text>
  <rect x="18" y="26" width="90" height="12" rx="3" fill="#f0a83233" stroke="#f0a832" stroke-width="1"/>
  <text x="63" y="36" text-anchor="middle" fill="#f0a832" font-size="8" font-weight="bold">&#9728; Solar Panels</text>
  <line x1="10" y1="44" x2="10" y2="100" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="44" x2="16" y2="44" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="100" x2="16" y2="100" stroke="#8b949e" stroke-width="1.5"/>
  <text x="6" y="75" text-anchor="middle" fill="#8b949e" font-size="7" transform="rotate(-90,6,75)">Block 1</text>
  <rect x="18" y="44" width="90" height="56" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="63" y="65" text-anchor="middle" fill="#ff6b6b" font-size="9" font-weight="bold">AV</text>
  <text x="63" y="79" text-anchor="middle" fill="#8b949e" font-size="7">Block 1 = Plot 1</text>
  <rect x="118" y="44" width="90" height="56" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="163" y="65" text-anchor="middle" fill="#00d4aa" font-size="9" font-weight="bold">Control</text>
  <text x="163" y="79" text-anchor="middle" fill="#8b949e" font-size="7">Block 1 = Plot 1</text>
  <line x1="10" y1="108" x2="10" y2="164" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="108" x2="16" y2="108" stroke="#8b949e" stroke-width="1.5"/>
  <line x1="10" y1="164" x2="16" y2="164" stroke="#8b949e" stroke-width="1.5"/>
  <text x="6" y="139" text-anchor="middle" fill="#8b949e" font-size="7" transform="rotate(-90,6,139)">Block 2</text>
  <rect x="18" y="108" width="90" height="56" rx="5" fill="#ff6b6b22" stroke="#ff6b6b" stroke-width="1.5"/>
  <text x="63" y="129" text-anchor="middle" fill="#ff6b6b" font-size="9" font-weight="bold">AV</text>
  <text x="63" y="143" text-anchor="middle" fill="#8b949e" font-size="7">Block 2 = Plot 2</text>
  <rect x="118" y="108" width="90" height="56" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1.5"/>
  <text x="163" y="129" text-anchor="middle" fill="#00d4aa" font-size="9" font-weight="bold">Control</text>
  <text x="163" y="143" text-anchor="middle" fill="#8b949e" font-size="7">Block 2 = Plot 2</text>
  <rect x="240" y="44" width="260" height="50" rx="6" fill="#1c2230" stroke="#2a3441" stroke-width="1"/>
  <text x="370" y="62" text-anchor="middle" fill="#8b949e" font-size="9">Random effect:</text>
  <text x="370" y="78" text-anchor="middle" fill="#00d4aa" font-size="10" font-weight="bold">(1 | Site:Block)</text>
  <rect x="240" y="104" width="260" height="34" rx="6" fill="#1c2230" stroke="#2a3441" stroke-width="1"/>
  <text x="370" y="118" text-anchor="middle" fill="#8b949e" font-size="9">Fixed effect:</text>
  <text x="370" y="132" text-anchor="middle" fill="#ff6b6b" font-size="10" font-weight="bold">Site</text>
  <rect x="240" y="148" width="260" height="36" rx="5" fill="#f0a83220" stroke="#f0a832" stroke-width="1"/>
  <text x="370" y="163" text-anchor="middle" fill="#f0a832" font-size="9">&#9888; Treatment confounded with space</text>
  <text x="370" y="177" text-anchor="middle" fill="#f0a832" font-size="9">&#8594; LMM accounts for this</text>
  <rect x="18" y="172" width="190" height="26" rx="5" fill="#00d4aa22" stroke="#00d4aa" stroke-width="1"/>
  <text x="113" y="187" text-anchor="middle" fill="#00d4aa" font-size="8">lmer(Yield ~ Site + (1|Site:Block))</text>
</svg>')
            ),
            br(),
            p("Standard experimental designs — such as split-plot or randomized complete block design (RCBD) — are commonly applied in agrivoltaics research, but they are fundamentally misaligned with the actual structure of agrivoltaics experiments."),
            br(),
            p("In a typical agrivoltaics study, the treatment (solar panel shading) is permanently fixed in space. Full randomization of treatment and control plots — as required by split-plot or RCBD assumptions — would demand two to four times the land area, and removing solar panels to create control conditions is simply not feasible. More critically, the treatment is perfectly confounded with space: any observed difference between AV and control plots may reflect spatial variation rather than the treatment effect itself."),
            br(),
            p("Despite these constraints, many researchers continue to apply split-plot or RCBD models without accounting for the spatial structure inherent to agrivoltaics systems. This leads to misrepresentation of treatment effects and inflated or deflated estimates of variability."),
            br(),
            p(strong("Agrivoltaics Stat"), " was developed to address this gap. By applying linear mixed models (LMM) that explicitly account for the spatial and structural constraints of agrivoltaics experiments, this tool ensures that statistical inference is both valid and appropriate for real field conditions."),
            br(),
            div(style="max-width:600px;",
            HTML('<svg width="100%" viewBox="0 0 680 430" xmlns="http://www.w3.org/2000/svg">
  <text style="font-size:14px;font-weight:500" x="340" y="22" text-anchor="middle" fill="#2c2c2c">Terminology</text>
  <text style="font-size:14px;font-weight:500" x="170" y="48" text-anchor="middle" fill="#2c2c2c">AV</text>
  <text style="font-size:14px;font-weight:500" x="490" y="48" text-anchor="middle" fill="#2c2c2c">Control</text>
  <rect x="30" y="58" width="280" height="260" rx="8" fill="none" stroke="#E24B4A" stroke-width="1.5" stroke-dasharray="6,3"/>
  <rect x="350" y="58" width="280" height="260" rx="8" fill="none" stroke="#E24B4A" stroke-width="1.5" stroke-dasharray="6,3"/>
  <rect x="46" y="76" width="52" height="52" rx="4" fill="#1D9E7526" stroke="#1D9E75" stroke-width="1"/>
  <line x1="58" y1="88" x2="58" y2="116" stroke="#EF9F27" stroke-width="1.5" stroke-dasharray="4,3"/>
  <line x1="72" y1="88" x2="72" y2="116" stroke="#EF9F27" stroke-width="1.5" stroke-dasharray="4,3"/>
  <line x1="86" y1="88" x2="86" y2="116" stroke="#EF9F27" stroke-width="1.5" stroke-dasharray="4,3"/>
  <rect x="104" y="76" width="52" height="52" rx="4" fill="#1D9E7526" stroke="#1D9E75" stroke-width="1"/>
  <text style="font-size:12px" x="130" y="106" text-anchor="middle" fill="#2c2c2c">Crop 1</text>
  <rect x="162" y="76" width="52" height="52" rx="4" fill="#1D9E7526" stroke="#1D9E75" stroke-width="1"/>
  <text style="font-size:12px" x="188" y="106" text-anchor="middle" fill="#2c2c2c">Crop 3</text>
  <rect x="220" y="76" width="52" height="52" rx="4" fill="#1D9E7526" stroke="#1D9E75" stroke-width="1"/>
  <text style="font-size:12px" x="246" y="106" text-anchor="middle" fill="#2c2c2c">Crop 2</text>
  <rect x="46" y="136" width="52" height="52" rx="4" fill="#1D9E7526" stroke="#1D9E75" stroke-width="1"/>
  <text style="font-size:12px" x="72" y="166" text-anchor="middle" fill="#2c2c2c">Crop 1</text>
  <rect x="104" y="136" width="52" height="52" rx="4" fill="#1D9E7526" stroke="#1D9E75" stroke-width="1"/>
  <text style="font-size:12px" x="130" y="166" text-anchor="middle" fill="#2c2c2c">Crop 2</text>
  <rect x="162" y="136" width="52" height="52" rx="4" fill="#1D9E7526" stroke="#1D9E75" stroke-width="1"/>
  <text style="font-size:12px" x="188" y="166" text-anchor="middle" fill="#2c2c2c">Crop 1</text>
  <rect x="220" y="136" width="52" height="52" rx="4" fill="#1D9E7526" stroke="#1D9E75" stroke-width="1"/>
  <text style="font-size:12px" x="246" y="166" text-anchor="middle" fill="#2c2c2c">Crop 1</text>
  <rect x="46" y="196" width="52" height="52" rx="4" fill="#1D9E7526" stroke="#1D9E75" stroke-width="1"/>
  <text style="font-size:12px" x="72" y="226" text-anchor="middle" fill="#2c2c2c">Crop 3</text>
  <rect x="104" y="196" width="52" height="52" rx="4" fill="#1D9E7526" stroke="#1D9E75" stroke-width="1"/>
  <text style="font-size:12px" x="130" y="226" text-anchor="middle" fill="#2c2c2c">Crop 3</text>
  <rect x="162" y="196" width="52" height="52" rx="4" fill="#1D9E7526" stroke="#1D9E75" stroke-width="1"/>
  <text style="font-size:12px" x="188" y="226" text-anchor="middle" fill="#2c2c2c">Crop 2</text>
  <rect x="220" y="196" width="52" height="52" rx="4" fill="#1D9E7526" stroke="#1D9E75" stroke-width="1"/>
  <text style="font-size:12px" x="246" y="226" text-anchor="middle" fill="#2c2c2c">Crop 3</text>
  <line x1="98" y1="70" x2="98" y2="258" stroke="#8b949e" stroke-width="0.8" stroke-dasharray="3,3"/>
  <line x1="156" y1="70" x2="156" y2="258" stroke="#8b949e" stroke-width="0.8" stroke-dasharray="3,3"/>
  <line x1="214" y1="70" x2="214" y2="258" stroke="#8b949e" stroke-width="0.8" stroke-dasharray="3,3"/>
  <rect x="366" y="76" width="52" height="52" rx="4" fill="#1D9E7514" stroke="#1D9E75" stroke-width="0.8"/>
  <rect x="424" y="76" width="52" height="52" rx="4" fill="#1D9E7514" stroke="#1D9E75" stroke-width="0.8"/>
  <rect x="482" y="76" width="52" height="52" rx="4" fill="#1D9E7514" stroke="#1D9E75" stroke-width="0.8"/>
  <rect x="540" y="76" width="52" height="52" rx="4" fill="#1D9E7514" stroke="#1D9E75" stroke-width="0.8"/>
  <rect x="366" y="136" width="52" height="52" rx="4" fill="#1D9E7514" stroke="#1D9E75" stroke-width="0.8"/>
  <rect x="424" y="136" width="52" height="52" rx="4" fill="#1D9E7514" stroke="#1D9E75" stroke-width="0.8"/>
  <rect x="482" y="136" width="52" height="52" rx="4" fill="#1D9E7514" stroke="#1D9E75" stroke-width="0.8"/>
  <rect x="540" y="136" width="52" height="52" rx="4" fill="#1D9E7514" stroke="#1D9E75" stroke-width="0.8"/>
  <rect x="366" y="196" width="52" height="52" rx="4" fill="#1D9E7514" stroke="#1D9E75" stroke-width="0.8"/>
  <rect x="424" y="196" width="52" height="52" rx="4" fill="#1D9E7514" stroke="#1D9E75" stroke-width="0.8"/>
  <rect x="482" y="196" width="52" height="52" rx="4" fill="#1D9E7514" stroke="#1D9E75" stroke-width="0.8"/>
  <rect x="540" y="196" width="52" height="52" rx="4" fill="#1D9E7514" stroke="#1D9E75" stroke-width="0.8"/>
  <line x1="30" y1="335" x2="650" y2="335" stroke="#cccccc" stroke-width="0.8"/>
  <rect x="40" y="348" width="32" height="22" rx="3" fill="none" stroke="#E24B4A" stroke-width="1.5" stroke-dasharray="5,3"/>
  <text style="font-size:12px" x="80" y="362" fill="#2c2c2c">Site</text>
  <rect x="130" y="348" width="32" height="22" rx="3" fill="#1D9E7526" stroke="#1D9E75" stroke-width="1"/>
  <text style="font-size:12px" x="170" y="362" fill="#2c2c2c">Plot</text>
  <rect x="220" y="342" width="52" height="68" rx="4" fill="none" stroke="#8b949e" stroke-width="1.2"/>
  <rect x="226" y="348" width="40" height="17" rx="2" fill="#1D9E7526" stroke="#1D9E75" stroke-width="0.8"/>
  <text style="font-size:12px" x="246" y="360" text-anchor="middle" fill="#2c2c2c">Crop 1</text>
  <rect x="226" y="369" width="40" height="17" rx="2" fill="#1D9E7526" stroke="#1D9E75" stroke-width="0.8"/>
  <text style="font-size:12px" x="246" y="381" text-anchor="middle" fill="#2c2c2c">Crop 2</text>
  <rect x="226" y="390" width="40" height="17" rx="2" fill="#1D9E7526" stroke="#1D9E75" stroke-width="0.8"/>
  <text style="font-size:12px" x="246" y="402" text-anchor="middle" fill="#2c2c2c">Crop 3</text>
  <text style="font-size:12px" x="282" y="380" fill="#2c2c2c">Block</text>
  <rect x="360" y="348" width="32" height="22" rx="3" fill="#1D9E7520" stroke="#1D9E75" stroke-width="0.8"/>
  <line x1="368" y1="351" x2="368" y2="367" stroke="#EF9F27" stroke-width="1.5" stroke-dasharray="3,2"/>
  <line x1="376" y1="351" x2="376" y2="367" stroke="#EF9F27" stroke-width="1.5" stroke-dasharray="3,2"/>
  <line x1="384" y1="351" x2="384" y2="367" stroke="#EF9F27" stroke-width="1.5" stroke-dasharray="3,2"/>
  <text style="font-size:12px" x="400" y="362" fill="#2c2c2c">Row</text>
</svg>')
            ),
            br(),
            hr(),
            p(strong("Reference:")),
            p(a("[STAT Article] Statistical Models in Agrivoltaics: Linear Mixed Models Across Different Field Layouts",
                href="https://agronomy4future.com/archives/24404", target="_blank")),
            br(),
            p(strong("R Packages:")),
            p("lme4, lmerTest, emmeans, multcomp, multcompView, ggplot2"),
            br(),
            p(strong("8 Layout Cases supported:")),
            lapply(seq_along(CASE_DESC), function(i) p(paste0("Case ", i, ": ", CASE_DESC[i])))
        )
      )
    )
  )
)

# ─────────────────────────────────────────────
#  SERVER
# ─────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(df = NULL)

  observeEvent(input$file, {
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    df <- tryCatch({
      if (ext == "xlsx" || ext == "xls")
        as.data.frame(readxl::read_excel(input$file$datapath))
      else
        as.data.frame(read_csv(input$file$datapath, show_col_types=FALSE))
    }, error=function(e) NULL)
    if (is.null(df)) return()
    rv$df <- df
    cols  <- colnames(rv$df)
    req_ch <- setNames(cols, cols)
    opt_ch <- c("— not used —"="none", setNames(cols, cols))
    updateSelectInput(session, "sel_y",        choices=c("— select —"="", req_ch))
    updateSelectInput(session, "sel_site",      choices=c("— select —"="", req_ch))
    updateSelectInput(session, "sel_block",     choices=c("— select —"="", req_ch))
    updateSelectInput(session, "sel_genotype",  choices=opt_ch)
    updateSelectInput(session, "sel_row",       choices=opt_ch)
    updateSelectInput(session, "sel_season",    choices=opt_ch)
    updateSelectInput(session, "sel_location",  choices=opt_ch)

  })

  # ── Sheet 선택 UI (xlsx일 때만 표시)
  output$sheet_selector_ui <- renderUI({
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    if (!ext %in% c("xlsx", "xls")) return(NULL)
    sheets <- tryCatch(readxl::excel_sheets(input$file$datapath), error = function(e) NULL)
    if (is.null(sheets)) return(NULL)
    selectInput("sel_sheet", "Select Sheet", choices = sheets, selected = sheets[1])
  })

  # ── 시트 변경시 데이터 재로드
  observeEvent(input$sel_sheet, {
    req(input$file)
    req(input$sel_sheet)
    ext <- tolower(tools::file_ext(input$file$name))
    if (!ext %in% c("xlsx","xls")) return()
    df <- tryCatch(
      as.data.frame(readxl::read_excel(input$file$datapath, sheet = input$sel_sheet)),
      error = function(e) NULL
    )
    if (is.null(df)) return()
    rv$df <- df
    cols <- colnames(rv$df)
    req_ch <- setNames(cols, cols)
    opt_ch <- c("— not used —"="none", setNames(cols, cols))
    updateSelectInput(session, "sel_y",        choices=c("— select —"="", req_ch))
    updateSelectInput(session, "sel_site",      choices=c("— select —"="", req_ch))
    updateSelectInput(session, "sel_block",     choices=c("— select —"="", req_ch))
    updateSelectInput(session, "sel_genotype",  choices=opt_ch)
    updateSelectInput(session, "sel_row",       choices=opt_ch)
    updateSelectInput(session, "sel_season",    choices=opt_ch)
    updateSelectInput(session, "sel_location",  choices=opt_ch)
  }, ignoreInit = TRUE)

  output$preview_table <- renderDT({
    req(rv$df)
    datatable(head(rv$df,5), options=list(dom="t", scrollX=TRUE, pageLength=5),
              style="bootstrap", class="compact")
  })

  output$crop_type_ui <- renderUI({
    req(input$sel_season)
    if (input$sel_season == "none") return(NULL)
    tagList(
      div(class="section-title", "Crop Type"),
      div(class="info-notice",
          "Are the same plants measured across seasons?", br(),
          "• Annual: new plants each season", br(),
          "• Perennial: same plants re-measured (requires \u2265 4 seasons for reliable estimation)"),
      radioButtons("crop_type", label=NULL,
                   choices=c("Annual"="annual","Perennial"="perennial"),
                   selected="annual")
    )
  })

  # Per-variable subset dropdowns
  make_subset_ui <- function(sel_input, label_name) {
    renderUI({
      req(rv$df)
      col <- sel_input()
      if (is.null(col) || col == "" || col == "none") return(NULL)
      if (!col %in% colnames(rv$df)) return(NULL)
      vals <- sort(unique(as.character(rv$df[[col]])))
      input_id <- paste0("subset_", gsub("[^a-zA-Z0-9]", "_", label_name))
      checkboxGroupInput(input_id,
                         label = paste("Filter", label_name, ":"),
                         choices = vals, selected = vals, inline = TRUE)
    })
  }

  output$subset_block_ui <- renderUI({
    req(rv$df); req(input$sel_block)
    if (!nzchar(input$sel_block)) return(NULL)
    col <- input$sel_block
    if (!col %in% colnames(rv$df)) return(NULL)
    vals <- sort(unique(as.character(rv$df[[col]])))
    checkboxGroupInput("subset_block", label=paste("Filter", col, ":"),
                       choices=vals, selected=vals, inline=TRUE)
  })

  output$subset_genotype_ui <- renderUI({
    req(rv$df); req(input$sel_genotype)
    if (is.null(input$sel_genotype) || input$sel_genotype == "none") return(NULL)
    col <- input$sel_genotype
    if (!col %in% colnames(rv$df)) return(NULL)
    vals <- sort(unique(as.character(rv$df[[col]])))
    checkboxGroupInput("subset_genotype", label=paste("Filter", col, ":"),
                       choices=vals, selected=vals, inline=TRUE)
  })

  output$subset_row_ui <- renderUI({
    req(rv$df); req(input$sel_row)
    if (is.null(input$sel_row) || input$sel_row == "none") return(NULL)
    col <- input$sel_row
    if (!col %in% colnames(rv$df)) return(NULL)
    vals <- sort(unique(as.character(rv$df[[col]])))
    checkboxGroupInput("subset_row", label=paste("Filter", col, ":"),
                       choices=vals, selected=vals, inline=TRUE)
  })

  output$subset_season_ui <- renderUI({
    req(rv$df); req(input$sel_season)
    if (is.null(input$sel_season) || input$sel_season == "none") return(NULL)
    col <- input$sel_season
    if (!col %in% colnames(rv$df)) return(NULL)
    vals <- sort(unique(as.character(rv$df[[col]])))
    checkboxGroupInput("subset_season", label=paste("Filter", col, ":"),
                       choices=vals, selected=vals, inline=TRUE)
  })

  filtered_df <- reactive({
    req(rv$df)
    df <- rv$df
    # Block filter (only if at least 1 selected)
    if (!is.null(input$sel_block) && nzchar(input$sel_block) &&
        input$sel_block %in% colnames(df) &&
        !is.null(input$subset_block) && length(input$subset_block) > 0) {
      df <- df[as.character(df[[input$sel_block]]) %in% input$subset_block, ]
    }
    # Genotype filter (only if at least 1 selected)
    if (!is.null(input$sel_genotype) && input$sel_genotype != "none" &&
        input$sel_genotype %in% colnames(df) &&
        !is.null(input$subset_genotype) && length(input$subset_genotype) > 0) {
      df <- df[as.character(df[[input$sel_genotype]]) %in% input$subset_genotype, ]
    }
    # Row filter (only if at least 1 selected)
    if (!is.null(input$sel_row) && input$sel_row != "none" &&
        input$sel_row %in% colnames(df) &&
        !is.null(input$subset_row) && length(input$subset_row) > 0) {
      df <- df[as.character(df[[input$sel_row]]) %in% input$subset_row, ]
    }
    # Season filter (only if at least 1 selected)
    if (!is.null(input$sel_season) && input$sel_season != "none" &&
        input$sel_season %in% colnames(df) &&
        !is.null(input$subset_season) && length(input$subset_season) > 0) {
      df <- df[as.character(df[[input$sel_season]]) %in% input$subset_season, ]
    }
    df
  })

  output$subset_preview_text <- renderUI({
    req(rv$df)
    # Check if any variable has 0 selected
    warn_msgs <- c()
    if (!is.null(input$sel_block) && nzchar(input$sel_block) &&
        !is.null(input$subset_block) && length(input$subset_block) == 0)
      warn_msgs <- c(warn_msgs, paste("⚠", input$sel_block, ": at least 1 value must be selected"))
    if (!is.null(input$sel_genotype) && input$sel_genotype != "none" &&
        !is.null(input$subset_genotype) && length(input$subset_genotype) == 0)
      warn_msgs <- c(warn_msgs, paste("⚠", input$sel_genotype, ": at least 1 value must be selected"))
    if (!is.null(input$sel_row) && input$sel_row != "none" &&
        !is.null(input$subset_row) && length(input$subset_row) == 0)
      warn_msgs <- c(warn_msgs, paste("⚠", input$sel_row, ": at least 1 value must be selected"))
    if (!is.null(input$sel_season) && input$sel_season != "none" &&
        !is.null(input$subset_season) && length(input$subset_season) == 0)
      warn_msgs <- c(warn_msgs, paste("⚠", input$sel_season, ": at least 1 value must be selected"))
    n_orig <- nrow(rv$df)
    n_filt <- nrow(filtered_df())
    tags <- list()
    if (length(warn_msgs) > 0)
      tags <- c(tags, list(div(style="margin-top:6px;padding:8px 12px;background:#fff3cd;border:1px solid #f0a832;border-radius:6px;font-size:13px;color:#856404;",
                              lapply(warn_msgs, function(w) div(w)))))
    if (n_orig != n_filt)
      tags <- c(tags, list(div(style="margin-top:6px;padding:8px 12px;background:#e8f4f8;border-radius:6px;font-size:13px;color:#2c2c2c;",
                               icon("filter"),
                               sprintf("  %d rows selected out of %d total rows", n_filt, n_orig))))
    if (length(tags) == 0) return(NULL)
    tagList(tags)
  })

  # Enforce minimum 1 selection for each subset checkbox
  observeEvent(input$subset_block, {
    req(input$sel_block); req(nzchar(input$sel_block))
    if (length(input$subset_block) == 0) {
      col <- input$sel_block
      if (col %in% colnames(rv$df)) {
        vals <- sort(unique(as.character(rv$df[[col]])))
        updateCheckboxGroupInput(session, "subset_block", selected=vals[1])
      }
    }
  }, ignoreNULL=FALSE)

  observeEvent(input$subset_genotype, {
    req(input$sel_genotype); req(input$sel_genotype != "none")
    if (length(input$subset_genotype) == 0) {
      col <- input$sel_genotype
      if (col %in% colnames(rv$df)) {
        vals <- sort(unique(as.character(rv$df[[col]])))
        updateCheckboxGroupInput(session, "subset_genotype", selected=vals[1])
      }
    }
  }, ignoreNULL=FALSE)

  observeEvent(input$subset_row, {
    req(input$sel_row); req(input$sel_row != "none")
    if (length(input$subset_row) == 0) {
      col <- input$sel_row
      if (col %in% colnames(rv$df)) {
        vals <- sort(unique(as.character(rv$df[[col]])))
        updateCheckboxGroupInput(session, "subset_row", selected=vals[1])
      }
    }
  }, ignoreNULL=FALSE)

  observeEvent(input$subset_season, {
    req(input$sel_season); req(input$sel_season != "none")
    if (length(input$subset_season) == 0) {
      col <- input$sel_season
      if (col %in% colnames(rv$df)) {
        vals <- sort(unique(as.character(rv$df[[col]])))
        updateCheckboxGroupInput(session, "subset_season", selected=vals[1])
      }
    }
  }, ignoreNULL=FALSE)

  # ── Location info UI
  output$location_info_ui <- renderUI({
    req(input$sel_location)
    if (is.null(input$sel_location) || input$sel_location == "none") return(NULL)
    col <- input$sel_location
    req(rv$df)
    if (!col %in% colnames(rv$df)) return(NULL)
    locs <- sort(unique(as.character(rv$df[[col]])))
    div(class="info-notice", style="background:#e8f4e8;border-color:#00a882;margin-top:6px;",
        icon("map-marker-alt"),
        sprintf("  %d location(s) detected: %s", length(locs), paste(locs, collapse=", ")), br(),
        "→ Results will be shown separately per location in Step 03.", br(),
        "→ A Location Comparison tab will be added automatically."
    )
  })

  get_vars <- reactive({
    has_season <- !is.null(input$sel_season) && input$sel_season != "none"
    n_season   <- 0L
    if (has_season && !is.null(rv$df))
      n_season <- length(unique(rv$df[[input$sel_season]]))
    list(
      y              = input$sel_y,
      site           = input$sel_site,
      block          = input$sel_block,
      genotype       = if (!is.null(input$sel_genotype)) input$sel_genotype else "none",
      row            = if (!is.null(input$sel_row))      input$sel_row      else "none",
      season         = if (!is.null(input$sel_season))   input$sel_season   else "none",
      location       = if (!is.null(input$sel_location)) input$sel_location else "none",
      block_type     = input$block_type,
      has_replicates = (input$has_replicates == "yes"),
      crop_type      = if (!is.null(input$crop_type)) input$crop_type else "annual",
      n_season       = n_season
    )
  })

  output$formula_preview <- renderUI({
    v <- get_vars()
    if (!nzchar(v$y) || !nzchar(v$site) || !nzchar(v$block))
      return(div(class="formula-box", style="color:#aaa;font-style:italic;font-size:15px",
                 "Select Y, Site, and Block to preview the model formula"))
    result <- tryCatch(build_formula(v), error=function(e) NULL)
    if (is.null(result)) return(NULL)
    fn_prefix <- if (result$model_fn=="lm") "lm(" else "lmer("
    warn_tags <- lapply(result$warnings, function(w) div(class="warn-box", w))
    tagList(
      div(class="formula-box", paste0(fn_prefix, result$formula_str, ", data = df)")),
      div(class="case-badge", paste0("\U0001f4d0 Case ", result$case_num, " \u2014 ", CASE_DESC[as.character(result$case_num)])),
      if (length(warn_tags)) tagList(warn_tags)
    )
  })

  output$layout_diagram <- renderUI({
    v <- get_vars()
    if (!nzchar(v$y) || !nzchar(v$site) || !nzchar(v$block))
      return(div(style="color:#aaa;font-size:12px;padding:20px;text-align:center",
                 "Assign variables to preview the field layout diagram"))
    result <- tryCatch(build_formula(v), error=function(e) NULL)
    if (is.null(result)) return(NULL)
    svg_html <- get_layout_svg(result$case_num, v)
    tagList(div(style="background:#090d12;border-radius:8px;padding:12px;", HTML(svg_html)))
  })

  analysis_result <- eventReactive(input$btn_run, {
    req(rv$df)
    v <- get_vars()
    req(nzchar(v$y), nzchar(v$site), nzchar(v$block))
    df_base <- filtered_df()

    has_loc <- !is.null(v$location) && v$location != "none" &&
               v$location %in% colnames(df_base)

    if (!has_loc) {
      res <- tryCatch(run_analysis_for_df(df_base, v),
                      error=function(e) stop(e$message))
      gc()
      return(list(mode="single", result=res))
    } else {
      locs <- sort(unique(as.character(df_base[[v$location]])))
      loc_results <- lapply(locs, function(loc) {
        df_loc <- df_base[as.character(df_base[[v$location]]) == loc, ]
        res <- tryCatch(suppressMessages(suppressWarnings(
                   run_analysis_for_df(df_loc, v))),
                 error=function(e) list(error=e$message, loc=loc))
        gc()
        res
      })
      names(loc_results) <- locs
      gc()

      loc_compare <- tryCatch({
        df_cmp <- as.data.frame(df_base, stringsAsFactors=FALSE)
        # y 컬럼 numeric 변환
        df_cmp[[v$y]] <- as.numeric(as.character(df_cmp[[v$y]]))
        # 모든 컬럼 강제 factor 변환 (y 제외)
        for (col in colnames(df_cmp))
          if (col != v$y) df_cmp[[col]] <- factor(as.character(df_cmp[[col]]))
        # 디버그: str 출력
        message(paste(capture.output(str(df_cmp)), collapse="
"))
        # Case에 따라 Location Comparison 모델 동적 생성
        has_row    <- !is.null(v$row)    && v$row    != "none" && v$row    %in% colnames(df_cmp)
        has_season <- !is.null(v$season) && v$season != "none" && v$season %in% colnames(df_cmp)
        has_geno   <- !is.null(v$genotype) && v$genotype != "none" && v$genotype %in% colnames(df_cmp)
        bt <- function(x) if (grepl("[^a-zA-Z0-9_.]", x)) paste0("`", x, "`") else x
        SITE  <- bt(v$site);     LOC   <- bt(v$location)
        BLOCK <- bt(v$block);    Y     <- bt(v$y)
        ROW    <- if (has_row)    v$row      else NULL
        SEASON <- if (has_season) v$season   else NULL
        GENO   <- if (has_geno)   v$genotype else NULL

        # Fixed effects: 기본 Site * Location
        fixed_terms <- sprintf("%s * %s", SITE, LOC)
        if (has_row)
          fixed_terms <- paste(fixed_terms, ROW,
            sprintf("%s:%s", SITE, ROW), sprintf("%s:%s", LOC, ROW),
            sprintf("%s:%s:%s", SITE, LOC, ROW), sep=" + ")
        if (has_season)
          fixed_terms <- paste(fixed_terms, SEASON,
            sprintf("%s:%s", SITE, SEASON), sprintf("%s:%s", LOC, SEASON),
            sprintf("%s:%s:%s", SITE, LOC, SEASON), sep=" + ")
        if (has_geno)
          fixed_terms <- paste(fixed_terms, GENO,
            sprintf("%s:%s", SITE, GENO), sprintf("%s:%s", LOC, GENO),
            sprintf("%s:%s:%s", SITE, LOC, GENO), sep=" + ")

        # Random effects
        rand_terms <- sprintf("(1|%s:%s)", LOC, BLOCK)
        if (has_row)
          rand_terms <- paste(rand_terms,
            sprintf("(1|%s:%s:%s)", LOC, BLOCK, ROW), sep=" + ")
        if (has_season)
          rand_terms <- paste(rand_terms,
            sprintf("(1|%s:%s:%s)", LOC, BLOCK, SEASON), sep=" + ")

        cmp_formula_str <- sprintf("%s ~ %s + %s", Y, fixed_terms, rand_terms)
        cmp_formula <- as.formula(cmp_formula_str)
        cmp_model <- lmer(cmp_formula, data=df_cmp, REML=TRUE)
        cmp_anova <- as.data.frame(anova(cmp_model, type=3))
        cmp_anova <- round(cmp_anova, 4)
        p_col <- if ("Pr(>F)" %in% colnames(cmp_anova)) "Pr(>F)" else NULL
        if (!is.null(p_col))
          cmp_anova$Sig <- ifelse(cmp_anova[[p_col]]<0.001,"***",
                           ifelse(cmp_anova[[p_col]]<0.01,"**",
                           ifelse(cmp_anova[[p_col]]<0.05,"*",
                           ifelse(cmp_anova[[p_col]]<0.1,".","ns"))))

        # p<0.05 유의한 항목 확인 (emmeans는 renderDT에서 계산)
        sig_terms <- tryCatch({
          p_col <- if ("Pr(>F)" %in% colnames(cmp_anova)) "Pr(>F)" else NULL
          if (is.null(p_col)) character(0)
          else rownames(cmp_anova)[cmp_anova[[p_col]] < 0.05]
        }, error=function(e) character(0))
        # 메모리 절약 - model과 df 저장 안함
        cmp_anova_out <- cmp_anova
        formula_out   <- deparse(cmp_formula)
        anova_terms   <- rownames(cmp_anova)
        rm(df_cmp, cmp_anova)
        gc()
        list(model=cmp_model, anova=cmp_anova_out,
             anova_terms=anova_terms,
             formula=formula_out,
             loc_col=v$location, site_col=v$site,
             row_col=v$row, y_col=v$y)
      }, error=function(e) list(error=e$message))

      gc()
      return(list(mode="multi", loc_results=loc_results,
                  locs=locs, loc_compare=loc_compare, v=v))
    }
  })

  # output 할당을 renderUI 밖에서 처리
  observeEvent(input$btn_run, {
    ar <- tryCatch(analysis_result(), error=function(e) NULL)
    if (is.null(ar) || is.null(ar$mode)) return()
    if (ar$mode == "single") {
      result <- ar$result
      rv$single_result <- result
      output$vc_tbl_main <- renderDT({
        req(result$vc_tbl)
        datatable(result$vc_tbl, options=list(dom="t", pageLength=20, scrollX=TRUE),
                  style="bootstrap", class="compact", rownames=FALSE)
      })
      output$anova_tbl_main <- renderDT({
        datatable(result$anova_tbl, options=list(dom="t", pageLength=20, scrollX=TRUE),
                  style="bootstrap", class="compact")
      })
      output$posthoc_ui_main <- renderUI({
        req(rv$single_result)
        res <- rv$single_result
        tagList(
          div(class="warn-box",
              "⚠ Note: Post-hoc letters reflect mean differences only. ",
              "If ANOVA p > 0.05, letter differences should be interpreted with caution."),
          br(),
          selectInput("posthoc_term_main", "Select term for post-hoc:",
                      choices=res$anova_terms, selected=res$default_term),
          DTOutput("posthoc_tbl_main"))
      })
      output$posthoc_tbl_main <- renderDT({
        req(input$posthoc_term_main)
        req(rv$single_result)
        res <- rv$single_result
        tryCatch({
          em  <- suppressMessages(suppressWarnings(
                   emmeans(res$model, as.formula(paste("~", input$posthoc_term_main)))))
          cld <- suppressMessages(suppressWarnings(
                   as.data.frame(multcomp::cld(em, Letters=letters, adjust="tukey"))))
          cld$.group <- trimws(cld$.group)
          cld <- cld[order(-cld$emmean), ]
          rm(em); gc()
          datatable(round_df(cld), options=list(dom="t", scrollX=TRUE),
                    style="bootstrap", class="compact", rownames=FALSE)
        }, error=function(e) datatable(data.frame(Error=e$message), options=list(dom="t")))
      })

    }
    if (ar$mode == "multi") {
      locs    <- ar$locs
      loc_res <- ar$loc_results
      loc_cmp <- ar$loc_compare
      lapply(seq_along(locs), function(i) {
        loc      <- locs[[i]]
        res      <- loc_res[[loc]]
        if (!is.null(res$error)) return()
        tab_id    <- paste0("loc", i)
        local({
          .res       <- res
          .anova_id  <- paste0("anova_", tab_id)
          .vc_id     <- paste0("vc_",    tab_id)
          .ph_ui_id  <- paste0("ph_ui_", tab_id)
          .ph_tbl_id <- paste0("ph_tbl_",tab_id)
          .sel_id    <- paste0("sel_",   tab_id)
          output[[.vc_id]] <- renderDT({
            req(.res$vc_tbl)
            datatable(.res$vc_tbl, options=list(dom="t", pageLength=20, scrollX=TRUE),
                      style="bootstrap", class="compact", rownames=FALSE)
          })
          output[[.anova_id]] <- renderDT({
            datatable(.res$anova_tbl, options=list(dom="t", pageLength=20, scrollX=TRUE),
                      style="bootstrap", class="compact")
          })

        })
      })
    }
  })

  output$results_ui <- renderUI({
    ar <- tryCatch(analysis_result(), error=function(e) {
      return(fluidRow(box(width=12, status="danger", title="Analysis Error",
        div(style="padding:16px;color:#c0392b;font-family:monospace;font-size:15px;",
            p(icon("exclamation-triangle"), strong(e$message))))))
    })
    req(!is.null(ar))
    if (inherits(ar, "shiny.tag.list")) return(ar)
    if (is.null(ar$mode)) return(NULL)
    # ── Single mode
    if (ar$mode == "single") {
      result <- ar$result
      warn_tags <- lapply(result$warnings, function(w) div(class="warn-box", w))

      output$vc_tbl_main <- renderDT({
        req(result$vc_tbl)
        datatable(result$vc_tbl,
                  options=list(dom="t", pageLength=20, scrollX=TRUE),
                  style="bootstrap", class="compact", rownames=FALSE)
      })
      output$anova_tbl_main <- renderDT({
        datatable(result$anova_tbl,
                  options=list(dom="t", pageLength=20, scrollX=TRUE),
                  style="bootstrap", class="compact")
      })
      output$posthoc_ui_main <- renderUI({
        tagList(
          div(class="warn-box",
              "⚠ Note: Post-hoc letters reflect mean differences only. ",
              "If ANOVA p > 0.05, letter differences should be interpreted with caution."),
          br(),
          selectInput("posthoc_term_main", "Select term for post-hoc:",
                      choices=result$anova_terms, selected=result$default_term),
          DTOutput("posthoc_tbl_main"), br()
        )
      })
      output$posthoc_tbl_main <- renderDT({
        req(input$posthoc_term_main)
        tryCatch({
          em  <- emmeans(result$model,
                         as.formula(paste("~", input$posthoc_term_main)))
          cld <- as.data.frame(multcomp::cld(em, Letters=letters, adjust="tukey"))
          cld <- round_df(cld)
          datatable(cld, options=list(dom="t", scrollX=TRUE),
                    style="bootstrap", class="compact", rownames=FALSE)
        }, error=function(e)
          datatable(data.frame(Error=e$message), options=list(dom="t")))
      })

      return(tagList(fluidRow(
        box(width=12, title="Step 03 — Analysis Results", status="success",
            div(class="case-badge",
                paste0("Case ", result$case_num, " — ",
                       CASE_DESC[as.character(result$case_num)])),
            br(), br(),
            do.call(tabsetPanel, Filter(Negate(is.null), list(
              if (!is.null(result$vc_tbl))
                tabPanel("Variance Components", br(),
                         DTOutput("vc_tbl_main"), br()),
              tabPanel("ANOVA Table",        br(), DTOutput("anova_tbl_main"), br()),
              tabPanel("Post-hoc (emmeans)", br(), uiOutput("posthoc_ui_main"), br())

            )))
        )
      )))
    }

    # ── Multi mode
    if (isTRUE(ar$mode == "multi")) {
      locs    <- ar$locs
      loc_res <- ar$loc_results
      loc_cmp <- ar$loc_compare
      v       <- ar$v

      loc_tabs <- lapply(seq_along(locs), function(i) {
        loc       <- locs[[i]]
        res       <- loc_res[[loc]]
        tab_id    <- paste0("loc", i)
        anova_id  <- paste0("anova_", tab_id)
        vc_id     <- paste0("vc_",    tab_id)
        vc_id     <- paste0("vc_",    tab_id)
        ph_ui_id  <- paste0("ph_ui_", tab_id)
        ph_tbl_id <- paste0("ph_tbl_",tab_id)
        sel_id    <- paste0("sel_",   tab_id)
        if (!is.null(res$error)) {
          return(tabPanel(as.character(loc),
            div(class="warn-box",
                paste("⚠ Analysis failed:", as.character(res$error)))))
        }

        local({
          .res       <- res
          .anova_id  <- anova_id
          .vc_id     <- vc_id
          .ph_ui_id  <- ph_ui_id
          .ph_tbl_id <- ph_tbl_id
          .sel_id    <- sel_id

          output[[.vc_id]] <- renderDT({
            req(.res$vc_tbl)
            datatable(.res$vc_tbl,
                      options=list(dom="t", pageLength=20, scrollX=TRUE),
                      style="bootstrap", class="compact", rownames=FALSE)
          })
          output[[.anova_id]] <- renderDT({
            datatable(.res$anova_tbl,
                      options=list(dom="t", pageLength=20, scrollX=TRUE),
                      style="bootstrap", class="compact")
          })
          output[[.ph_ui_id]] <- renderUI({
            tagList(
              div(class="warn-box",
                  "⚠ Note: Post-hoc letters reflect mean differences only. ",
                  "If ANOVA p > 0.05, letter differences should be interpreted with caution."),
              br(),
              selectInput(.sel_id, "Select term for post-hoc:",
                          choices=.res$anova_terms, selected=.res$default_term),
              DTOutput(.ph_tbl_id))
          })
          output[[.ph_tbl_id]] <- renderDT({
            req(input[[.sel_id]])
            tryCatch({
              em  <- suppressMessages(suppressWarnings(
                       emmeans(.res$model, as.formula(paste("~", input[[.sel_id]])))))
              cld <- suppressMessages(suppressWarnings(
                       as.data.frame(multcomp::cld(em, Letters=letters, adjust="tukey"))))
              cld$.group <- trimws(cld$.group)
              cld <- cld[order(-cld$emmean), ]
              rm(em); gc()
              datatable(round_df(cld), options=list(dom="t", scrollX=TRUE),
                        style="bootstrap", class="compact", rownames=FALSE)
            }, error=function(e)
              datatable(data.frame(Error=e$message), options=list(dom="t")))
          })

        })

        warn_tags_loc <- lapply(res$warnings, function(w) div(class="warn-box", w))
        warn_section  <- if (length(warn_tags_loc) > 0) tagList(br(), warn_tags_loc) else div()

        tabPanel(as.character(loc),
          div(class="case-badge",
              as.character(paste0("Case ", res$case_num))),
          warn_section,
          br(),
          div(class="formula-box",
              paste0(if(res$model_fn=="lm") "lm(" else "lmer(",
                     res$formula_str, ", data = df)")),
          br(),
          h4("Variance Components"),
          if (!is.null(res$vc_tbl)) DTOutput(vc_id) else div(),
          br(),
          h4("ANOVA Table"),
          DTOutput(anova_id),
          br(),
          h4("Post-hoc (emmeans)"),
          uiOutput(ph_ui_id),
          br(),

        )
      })

      if (!is.null(loc_cmp$error)) {
        output$loc_cmp_anova   <- renderDT({ datatable(data.frame(Error=loc_cmp$error)) })
        output$loc_cmp_em_loc   <- renderDT({ datatable(data.frame()) })
        output$loc_cmp_em_site  <- renderDT({ datatable(data.frame()) })
        output$loc_cmp_em_inter <- renderDT({ datatable(data.frame()) })
        output$loc_cmp_em_row   <- renderDT({ datatable(data.frame()) })
      } else {
        output$loc_cmp_anova <- renderDT({
          datatable(loc_cmp$anova,
                    options=list(dom="t", pageLength=20, scrollX=TRUE),
                    style="bootstrap", class="compact")
        })
        output$loc_cmp_posthoc_ui <- renderUI({
          tagList(
            div(class="warn-box",
                "⚠ Note: Post-hoc letters reflect mean differences only. ",
                "If ANOVA p > 0.05, letter differences should be interpreted with caution."),
            br(),
            selectInput("loc_cmp_term", "Select term for post-hoc:",
                        choices=loc_cmp$anova_terms,
                        selected=loc_cmp$anova_terms[1]),
            DTOutput("loc_cmp_posthoc_tbl")
          )
        })
        output$loc_cmp_posthoc_tbl <- renderDT({
          req(input$loc_cmp_term)
          req(!is.null(loc_cmp$model))
          tryCatch({
            em  <- suppressMessages(suppressWarnings(
                     emmeans(loc_cmp$model,
                             as.formula(paste("~", input$loc_cmp_term)))))
            cld <- suppressMessages(suppressWarnings(
                     as.data.frame(multcomp::cld(em, Letters=letters, adjust="tukey"))))
            cld$.group <- trimws(cld$.group)
            cld <- cld[order(-cld$emmean), ]
            rm(em); gc()
            datatable(round_df(cld),
                      options=list(dom="t", scrollX=TRUE),
                      style="bootstrap", class="compact", rownames=FALSE)
          }, error=function(e)
            datatable(data.frame(Error=e$message), options=list(dom="t")))
        })
      }
      cmp_content <- if (!is.null(loc_cmp$error)) {
        div(class="warn-box",
            as.character(paste("⚠ Location comparison failed:", loc_cmp$error[1])))
      } else {
        formula_str <- as.character(loc_cmp$formula[1])
        div(
          div(class="formula-box",
              paste0("lmer(", formula_str, ", data = df)")),
          br(),
          h4("ANOVA Table"),
          DTOutput("loc_cmp_anova"), br(),
          h4("Post-hoc (emmeans)"),
          uiOutput("loc_cmp_posthoc_ui")
        )
      }
      cmp_tab <- tabPanel("Location Comparison", br(), cmp_content)

      all_tabs <- Filter(Negate(is.null), c(loc_tabs, list(cmp_tab)))
      return(tagList(fluidRow(
        box(width=12,
            title="Step 03 - Analysis Results (by Location)",
            status="success",
            do.call(tabsetPanel, all_tabs))
      )))
    }
  })



}

shinyApp(ui = ui, server = server)
