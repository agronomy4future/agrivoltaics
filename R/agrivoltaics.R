#' Fit Mixed-Effects Models for Agrivoltaic Experiments
#'
#' @description
#' In agrivoltaics studies, randomization between the solar panel treatment and the control is often not feasible because of
#' the fixed layout and structural constraints of the panels in the field. As a result, the assumptions underlying standard
#' ANOVA or a split-plot design—particularly the requirement for random assignment of treatments—are violated.
#' `agrivoltaics()` function streamlines the analysis of agrivoltaic experiments by automatically building and fitting
#' linear mixed-effects models using `lme4::lmer()`.It handles the specification of fixed and random effects, generates biologically
#' meaningful interaction terms, and produces variance component breakdowns and Type III ANOVA tables for interpretation of treatment
#' effects.
#'
#' @param output The response variable (e.g., yield, biomass). **Required**.
#' @param treatment The main treatment factor (e.g., shading treatment). **Required**.
#' @param genotype (Optional) Genotype or variety factor.
#' @param plot (Optional) Plot identifier (random effect if present).
#' @param block Experimental block factor (random effect). **Required**.
#' @param row (Optional) Row factor, can generate treatment:row interactions.
#' @param season (Optional) Season or year factor, can generate repeated-measures structures.
#' @param location (Optional) Location factor (e.g., site or farm).
#' @param data A `data.frame` containing all specified variables.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Builds a fixed-effects model formula including treatments and optional
#'   factors (genotype, row, season, location) with biologically meaningful interactions.
#'   \item Adds random effects for block, plot, and block:season if repeated measures exist.
#'   \item Fits the model using `lmer()`.
#'   \item Prints variance components and their percentages, as well as a Type III ANOVA table.
#' }
#'
#' @return A fitted `lmer` model object (from \pkg{lme4}), returned invisibly.
#'
#' @examples
#' #upload required package
#' if(!require(remotes)) install.packages("remotes")
#' if (!requireNamespace("agrivoltaics", quietly = TRUE)) {
#' remotes::install_github("agronomy4future/agrivoltaics", force= TRUE)
#' }
#' library(remotes)
#' library(agrivoltaics)
#'
#'# data (example) upload
#'if(!require(readr)) install.packages("readr")
#'library(readr)
#'github="https://raw.githubusercontent.com/agronomy4future/raw_data_practice/refs/heads/main/agrivoltaics.csv"
#'df= data.frame(read_csv(url(github), show_col_types=FALSE))
#'set.seed(100)
#'print(df[sample(nrow(df),5),])
#'
#'Season      Location AV_Site Genotype Plot Block Row  Yield
#'2015 season East     AV      cv1      101  I     East 195.38
#'2016 season MidWest  Control cv1      115  IV    East 625.07
#'2015 season East     AV      cv2      102  II    East 135.86
#'2015 season MidWest  Control cv1      109  I     West 384.36
#'2016 season East     AV      cv1      107  III   East 125.11
#'.
#'.
#'.
#'
#' ■ Code source: https://github.com/agronomy4future/agrivoltaics
#'
#' \dontrun{
#'
#' df$Plot= as.factor(df$Plot)
#' df$Yield= as.numeric (df$Yield)
#'
#' model= agrivoltaics(
#'   output= Yield,
#'   treatment= AV_Site,
#'   genotype= Genotype (or NULL),
#'   plot= Plot (or NULL),
#'   block= Block,
#'   row= Row (or NULL),
#'   season= Season (or NULL),
#'   location= Location (or NULL),
#'   data= df
#' )
#'
#'### pairwise mean comparison (LSD: adjust= "none", Tukey: adjust= "sidak")
#'     pairwise= contrast(emmeans(model, ~ variable), method= "pairwise", adjust= "sidak")
#'     print(summary(pairwise))
#'
#'### Post-hoc analysis (LSD: adjust= "none", Tukey: adjust= "sidak")
#'     post_hoc= cld (emmeans(model, ~ variable), adjust= "sidak", Letters=letters, reverse = TRUE)
#'     print(post_hoc)
#'
#'
#'### Practice ###
#' model= agrivoltaics(
#'  output= Yield,
#'  treatment= AV_Site,
#'  genotype= Genotype,
#'  plot= NULL,
#'  block= Block,
#'  row= Row,
#'  season= NULL,
#'  location= NULL,
#'  data= df
#')
#'
#' post_hoc= cld (emmeans(model, ~ AV_Site:Row), adjust= "sidak", Letters=letters, reverse = TRUE)
#' print(post_hoc)
#'
#' AV_Site Row    emmean SE   df   lower.CL upper.CL .group
#' Control East   524    11.2 23.4 491.5    556       a
#' Control West   471    11.1 22.9 438.7    503        b
#' Control Middle 385    14.9 66.2 344.7    426         c
#' AV      Middle 184    14.9 66.2 143.2    224          d
#' AV      East   157    11.1 22.9 125.5    189          de
#' AV      West   131    11.1 22.9 99.4     163          e
#'
#'
#' }
#' @import lme4
#' @import lmerTest
#' @import rlang
#' @export
agrivoltaics = function(output,
                        treatment= NULL,
                        genotype= NULL,
                        plot= NULL,
                        block= NULL,
                        row= NULL,
                        season= NULL,
                        location= NULL,
                        data) {

  ##### Load required packages
  if(!require(lme4)) install.packages("lme4")
  if(!require(lmerTest)) install.packages("lmerTest")
  if(!require(emmeans)) install.packages("emmeans")
  if(!require(pbkrtest)) install.packages("pbkrtest")
  if(!require(multcompView)) install.packages("multcompView")
  if(!require(multcomp)) install.packages("multcomp")
  suppressPackageStartupMessages({
    library(lme4)
    library(lmerTest)
    library(emmeans)
    library(pbkrtest)
    library(multcompView)
    library(multcomp)
  })
  ##### Capture variables as quosures and convert to names
  output   = enquo(output)
  treatment= enquo(treatment)
  genotype = enquo(genotype)
  plot     = enquo(plot)
  block    = enquo(block)
  row      = enquo(row)
  season   = enquo(season)
  location = enquo(location)

  varname = function(q) if (quo_is_null(q)) NULL else as_label(q)

  out_var      = varname(output)
  treatment_var= varname(treatment)
  genotype_var = varname(genotype)
  plot_var     = varname(plot)
  block_var    = varname(block)
  row_var      = varname(row)
  season_var   = varname(season)
  location_var = varname(location)

  ##### Validation: output, treatment, and block are essential
  if (is.null(out_var) || is.null(treatment_var) || is.null(block_var)) {
    stop("'output', 'treatment', and 'block' must be provided.")
  }

  ##### FIXED EFFECTS
  fixed_terms = c(treatment_var)
  if (!is.null(genotype_var)) fixed_terms = c(fixed_terms, genotype_var)
  if (!is.null(season_var))   fixed_terms = c(fixed_terms, season_var)
  if (!is.null(location_var)) fixed_terms = c(fixed_terms, location_var)
  if (!is.null(row_var))      fixed_terms = c(fixed_terms, row_var)

  ##### INTERACTIONS – only up to 3-way
  interactions = c()

  if (!is.null(row_var)) interactions = c(interactions, paste0(treatment_var, ":", row_var))
  if (!is.null(genotype_var)) interactions = c(interactions, paste0(treatment_var, ":", genotype_var))
  if (!is.null(season_var)) interactions = c(interactions, paste0(treatment_var, ":", season_var))
  if (!is.null(location_var)) interactions = c(interactions, paste0(treatment_var, ":", location_var))

  if (!is.null(row_var) && !is.null(location_var)) interactions = c(interactions, paste0(row_var, ":", location_var))
  if (!is.null(genotype_var) && !is.null(location_var)) interactions = c(interactions, paste0(genotype_var, ":", location_var))
  if (!is.null(season_var) && !is.null(location_var)) interactions = c(interactions, paste0(season_var, ":", location_var))
  if (!is.null(genotype_var) && !is.null(season_var)) interactions = c(interactions, paste0(genotype_var, ":", season_var))

  ## --- 3-way interactions involving row or treatment
  if (!is.null(row_var) && !is.null(genotype_var)) interactions = c(interactions, paste0(treatment_var, ":", genotype_var, ":", row_var))
  if (!is.null(row_var) && !is.null(season_var)) interactions = c(interactions, paste0(treatment_var, ":", season_var, ":", row_var))
  if (!is.null(row_var) && !is.null(location_var)) interactions = c(interactions, paste0(treatment_var, ":", location_var, ":", row_var))
  if (!is.null(row_var) && !is.null(genotype_var) && !is.null(season_var)) interactions = c(interactions, paste0(genotype_var, ":", season_var, ":", row_var))
  if (!is.null(row_var) && !is.null(genotype_var) && !is.null(location_var)) interactions = c(interactions, paste0(genotype_var, ":", location_var, ":", row_var))
  if (!is.null(row_var) && !is.null(season_var) && !is.null(location_var)) interactions = c(interactions, paste0(season_var, ":", location_var, ":", row_var))

  ##### Merge into one fixed-effect formula
  fixed_formula = paste(c(fixed_terms, interactions), collapse = " + ")

  ##### RANDOM EFFECTS (key update for treatment:block nesting)
  random_effects = c()

  # If only treatment & block → assume block nested in treatment
  if (!is.null(treatment_var) && !is.null(block_var)) {
    random_effects = c(random_effects, paste0("(1|", treatment_var, ":", block_var, ")"))
  } else {
    random_effects = c(random_effects, paste0("(1|", block_var, ")"))
  }

  # If season exists → add block:season random effect
  if (!is.null(season_var)) {
    random_effects = c(random_effects, paste0("(1|", block_var, ":", season_var, ")"))
  }

  # If plot exists → add plot random effect (nested in block)
  if (!is.null(plot_var)) {
    random_effects = c(random_effects, paste0("(1|", block_var, ":", plot_var, ")"))
  }

  # If row exists → row nested within plot (or block if no plot)
  if (!is.null(row_var)) {
    if (!is.null(plot_var)) {
      random_effects = c(random_effects, paste0("(1|", plot_var, ":", row_var, ")"))
    } else {
      random_effects = c(random_effects, paste0("(1|", block_var, ":", row_var, ")"))
    }
  }

  random_formula = paste(random_effects, collapse = " + ")

  ##### FINAL MODEL FORMULA
  full_formula = paste(out_var, "~", fixed_formula, "+", random_formula)
  model_formula = as.formula(full_formula)

  cat("\n MODEL FORMULA USED:\n")
  cat(deparse(model_formula), "\n\n")

  ##### FIT MODEL
  model = lmer(model_formula, data = data)

  ##### DIAGNOSTICS & OUTPUT
  if (isSingular(model, tol = 1e-4)) {
    warning("️ Model fit is singular. Some variance components may be near zero or redundant.")
  }

  cat("\n VARIANCE COMPONENTS:\n")
  print(VarCorr(model), comp = "Variance")

  var_df <- as.data.frame(VarCorr(model))
  total_var <- sum(var_df$vcov)

  cat("\n VARIANCE COMPONENT BREAKDOWN (%):\n")
  for (i in seq_len(nrow(var_df))) {
    label <- ifelse(var_df$grp[i] == "Residual", "Residual", paste0("Random [", var_df$grp[i], "]"))
    pct <- round(var_df$vcov[i] / total_var * 100, 2)
    cat(label, ": ", pct, "%\n", sep = "")
  }

  cat("\n TYPE III ANOVA:\n")
  print(anova(model))

  invisible(model)
}
