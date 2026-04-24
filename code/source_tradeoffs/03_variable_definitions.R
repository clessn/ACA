# ==============================================================
# 03_variable_definitions.R
# IVs, RHS formula, label vectors, battery definitions,
# and DV detection. Single source of truth for all metadata.
# ==============================================================


# ── 1. IV LIST ────────────────────────────────────────────────

ivs <- list(
  # ── Region & identity ─────────────────────────────────────
  list(type="binary",  var="ideo_define_QC_first_bin", low=0, high=1, label="Quebecker First (No=0 vs Yes=1)"),
  list(type="binary",  var="quebec_bin",               low=0, high=1, label="Quebec (No=0 vs Yes=1)"),
  list(type="binary",  var="alberta_bin",              low=0, high=1, label="Alberta (No=0 vs Yes=1)"),
  list(type="binary",  var="ontario_bin",              low=0, high=1, label="Ontario (No=0 vs Yes=1)"),
  list(type="binary",  var="region_eastcoast_bin",     low=0, high=1, label="Atlantic Canada (No=0 vs Yes=1)"),
  # ── Socio-economic ────────────────────────────────────────
  list(type="binary",  var="incomeHigh_bin",           low=0, high=1, label="High Income (No=0 vs Yes=1)"),
  list(type="binary",  var="ses_male_bin",             low=0, high=1, label="Gender (Female=0 vs Male=1)"),
  list(type="binary",  var="age18_34_bin",             low=0, high=1, label="Age 18-34 (No=0 vs Yes=1)"),
  list(type="binary",  var="age55plus_bin",            low=0, high=1, label="Age 55+ (No=0 vs Yes=1)"),
  list(type="binary",  var="univ_educ_bin",            low=0, high=1, label="University Education (No=0 vs Yes=1)"),
  list(type="binary",  var="employ_fulltime_bin",      low=0, high=1, label="Employed Full-Time (No=0 vs Yes=1)"),
  list(type="binary",  var="children_bin",             low=0, high=1, label="Has children"),
  # ── Institutional trust ───────────────────────────────────
  list(type="binary",  var="trust_inst_fed_bin",       low=0, high=1, label="Trust: Federal government (Low=0 vs High=1)"),
  list(type="binary",  var="trust_inst_prov_bin",      low=0, high=1, label="Trust: Provincial government (Low=0 vs High=1)"),
  # ── Ideology & vote ───────────────────────────────────────
  list(type="numeric", var="ideo_right_num",           low=0, high=1, label="Ideology: Left (0) vs Right (1)"),
  list(type="binary",  var="vote_PLC_bin",             low=0, high=1, label="Liberal Party of Canada (No=0 vs Yes=1)"),
  list(type="binary",  var="vote_PCC_bin",             low=0, high=1, label="Conservative Party of Canada (No=0 vs Yes=1)"),
  list(type="binary",  var="ideo_vote_fed_left",       low=0, high=1, label="Federal vote: Left (Right=0 vs Left=1)"),
  list(type="binary",  var="ideo_vote_prov_left",      low=0, high=1, label="Provincial vote: Left (Right=0 vs Left=1)")
)

# ── 2. RHS FORMULA ────────────────────────────────────────────
# Ontario is the omitted region reference category

rhs <- "ideo_define_QC_first_bin + quebec_bin + alberta_bin + region_eastcoast_bin +
        incomeHigh_bin + ses_male_bin + age18_34_bin + age55plus_bin +
        univ_educ_bin + employ_fulltime_bin + children_bin +
        trust_inst_fed_bin + trust_inst_prov_bin +
        ideo_right_num + vote_PLC_bin + vote_PCC_bin +
        ideo_vote_fed_left + ideo_vote_prov_left"


# ── 3. TERM LABELS ────────────────────────────────────────────

term_labels <- c(
  # Individual-level traits
  "incomeHigh_bin"           = "High Income",
  "ses_male_bin"             = "Male",
  "age18_34_bin"             = "Age 18-34",
  "age55plus_bin"            = "Age 55+",
  "univ_educ_bin"            = "University Education",
  "employ_fulltime_bin"      = "Employed Full-Time",
  "children_bin"             = "Has Children",
  # Political attitudes
  "ideo_right_num"           = "Ideology (Right)",
  "vote_PLC_bin"             = "Liberal Party of Canada",
  "vote_PCC_bin"             = "Conservative Party of Canada",
  "ideo_vote_fed_left"       = "Federal vote (Left)",
  "ideo_vote_prov_left"      = "Provincial vote (Left)",
  # Regionalism
  "ideo_define_QC_first_bin" = "Quebecker First",
  "quebec_bin"               = "Quebec",
  "alberta_bin"              = "Alberta",
  "region_eastcoast_bin"     = "Atlantic Canada",
  # Institutional trust
  "trust_inst_fed_bin"       = "Trust: Federal government",
  "trust_inst_prov_bin"      = "Trust: Provincial government"
)

# Canonical IV order for plots — listed top-to-bottom here,
# which becomes bottom-to-top on the plot y-axis (ggplot default)
iv_order <- c(
  "High Income",
  "Male",
  "Age 18-34",
  "Age 55+",
  "University Education",
  "Employed Full-Time",
  "Has Children",
  "Ideology (Right)",
  "Federal vote (Left)",
  "Provincial vote (Left)",
  "Liberal Party of Canada",
  "Conservative Party of Canada",
  "Quebecker First",
  "Quebec",
  "Alberta",
  "Atlantic Canada",
  "French-speaking",
  "Trust: Federal government",
  "Trust: Provincial government"
)


# ── HYPOTHESIS VARIABLES FOR FIGURES ─────────────────────────
# Variables for which we have theoretical expectations.
# Controls remain in all models and regression tables — this list
# governs coefficient plot outputs only (applied via dplyr::filter).
# Single source of truth: edit here, all regression scripts inherit.
hyp_vars <- c(
  "incomeHigh_bin",
  "univ_educ_bin",
  "employ_fulltime_bin",
  "age18_34_bin",
  "children_bin",
  "ideo_right_num",
  "vote_PLC_bin",
  "vote_PCC_bin",
  "ideo_define_QC_first_bin",
  "quebec_bin",
  "alberta_bin",
  "region_eastcoast_bin",
  "trust_inst_fed_bin",
  "trust_inst_prov_bin"
)


# ── 4. POLICY LABELS ──────────────────────────────────────────

imp_policy_labels <- c(
  "health"   = "Healthcare",
  "edu"      = "Education",
  "pensions" = "Pensions",
  "taxes"    = "Tax reduction",
  "debt"     = "Debt reduction"
)

prio_policy_labels <- c(
  "health"  = "Healthcare access",
  "seniors" = "Home care for seniors",
  "cc"      = "Subsidized child care",
  "ecn"     = "Support businesses and economic growth",
  "clim"    = "Fight against climate change"
)

policy_labels <- c(imp_policy_labels, prio_policy_labels)


# ── 5. AUTO-DETECT DV VECTORS ─────────────────────────────────

imp_bin_vars  <- df |> dplyr::select(matches("^budget_imp_.*_bin$")) |> names()
imp_policies  <- str_extract(imp_bin_vars, "(?<=budget_imp_).*(?=_bin)")
imp_bin_df    <- tibble(
  var    = imp_bin_vars,
  policy = imp_policies,
  label = dplyr::recode(imp_policies, !!!imp_policy_labels, .default = imp_policies)
)

prio_num_vars <- df |> dplyr::select(matches("^budget_prio_[a-z]+$")) |> names()
prio_policies <- str_remove(prio_num_vars, "budget_prio_")
prio_num_df   <- tibble(
  var    = prio_num_vars,
  policy = prio_policies,
  label = dplyr::recode(prio_policies, !!!prio_policy_labels, .default = prio_policies)
)

prio_pref_vars    <- df |> dplyr::select(matches("^budget_prio_.*_pref$"))    |> names()
prio_intense_vars <- df |> dplyr::select(matches("^budget_prio_.*_intense$")) |> names()


# ── 6. TRADEOFF BATTERY DEFINITIONS ──────────────────────────

tradeoff_labels <- list(
  cc1 = c(
    "tradeoff_cc1_tax"      = "Raise taxes",
    "tradeoff_cc1_cut"      = "Cut other spending",
    "tradeoff_cc1_debt"     = "Increase debt",
    "tradeoff_cc1_no_spend" = "No increase, regardless of implications for issue"
  ),
  ge = c(
    "tradeoff_ge_tax"      = "Raise taxes",
    "tradeoff_ge_cut"      = "Cut other spending",
    "tradeoff_ge_debt"     = "Increase debt",
    "tradeoff_ge_no_spend" = "No increase, regardless of implications for issue"
  ),
  tax = c(
    "tradeoff_tax_less_services" = "No increase, even if fewer services",
    "tradeoff_tax_sales_tax"     = "Sales tax increase",
    "tradeoff_tax_inc_tax"       = "Income tax increase",
    "tradeoff_tax_wealth_tax"    = "Wealth tax"
  ),
  hc = c(
    "tradeoff_hc_all"      = "Increase home care for all, reduce maximum old-age pension",
    "tradeoff_hc_spend"    = "Target low-income seniors, home care",
    "tradeoff_hc_pensions" = "Target low-income seniors, pensions"
  ),
  cc2 = c(
    "tradeoff_cc2_all"          = "Increase child care for all, reduce other family benefits",
    "tradeoff_cc2_low_inc"      = "Target low-income families, childcare",
    "tradeoff_cc2_educ_all"     = "Target education quality for all, reduce other family benefits",
    "tradeoff_cc2_educ_low_inc" = "Target education quality for low-income"
  )
)

battery_titles <- c(
  cc1 = "Increase childcare spending, how to fund it",
  ge  = "Increase green economy spending, how to fund it",
  tax = "Raise revenue, tax preferences",
  hc  = "Home care priorities, target group",
  cc2 = "Childcare priorities, target group"
)

batteries <- list(
  cc1 = list(
    title   = battery_titles[["cc1"]],
    raw     = c("tradeoff_cc1_tax", "tradeoff_cc1_cut",
                "tradeoff_cc1_debt", "tradeoff_cc1_no_spend"),
    pref    = c("tradeoff_cc1_tax_pref", "tradeoff_cc1_cut_pref",
                "tradeoff_cc1_debt_pref", "tradeoff_cc1_no_spend_pref"),
    intense = c("tradeoff_cc1_tax_intense", "tradeoff_cc1_cut_intense",
                "tradeoff_cc1_debt_intense", "tradeoff_cc1_no_spend_intense"),
    labels  = c(
      tradeoff_cc1_tax              = "Raise taxes",
      tradeoff_cc1_cut              = "Cut other spending",
      tradeoff_cc1_debt             = "Increase debt",
      tradeoff_cc1_no_spend         = "No increase, regardless of implications for issue",
      tradeoff_cc1_tax_pref         = "Raise taxes",
      tradeoff_cc1_cut_pref         = "Cut other spending",
      tradeoff_cc1_debt_pref        = "Increase debt",
      tradeoff_cc1_no_spend_pref    = "No increase, regardless of implications for issue",
      tradeoff_cc1_tax_intense      = "Raise taxes",
      tradeoff_cc1_cut_intense      = "Cut other spending",
      tradeoff_cc1_debt_intense     = "Increase debt",
      tradeoff_cc1_no_spend_intense = "No increase, regardless of implications for issue"
    )
  ),
  ge = list(
    title   = battery_titles[["ge"]],
    raw     = c("tradeoff_ge_tax", "tradeoff_ge_cut",
                "tradeoff_ge_debt", "tradeoff_ge_no_spend"),
    pref    = c("tradeoff_ge_tax_pref", "tradeoff_ge_cut_pref",
                "tradeoff_ge_debt_pref", "tradeoff_ge_no_spend_pref"),
    intense = c("tradeoff_ge_tax_intense", "tradeoff_ge_cut_intense",
                "tradeoff_ge_debt_intense", "tradeoff_ge_no_spend_intense"),
    labels  = c(
      tradeoff_ge_tax              = "Raise taxes",
      tradeoff_ge_cut              = "Cut other spending",
      tradeoff_ge_debt             = "Increase debt",
      tradeoff_ge_no_spend         = "No increase, regardless of implications for issue",
      tradeoff_ge_tax_pref         = "Raise taxes",
      tradeoff_ge_cut_pref         = "Cut other spending",
      tradeoff_ge_debt_pref        = "Increase debt",
      tradeoff_ge_no_spend_pref    = "No increase, regardless of implications for issue",
      tradeoff_ge_tax_intense      = "Raise taxes",
      tradeoff_ge_cut_intense      = "Cut other spending",
      tradeoff_ge_debt_intense     = "Increase debt",
      tradeoff_ge_no_spend_intense = "No increase, regardless of implications for issue"
    )
  ),
  tax = list(
    title   = battery_titles[["tax"]],
    raw     = c("tradeoff_tax_less_services", "tradeoff_tax_sales_tax",
                "tradeoff_tax_inc_tax", "tradeoff_tax_wealth_tax"),
    pref    = c("tradeoff_tax_less_services_pref", "tradeoff_tax_sales_tax_pref",
                "tradeoff_tax_inc_tax_pref", "tradeoff_tax_wealth_tax_pref"),
    intense = c("tradeoff_tax_less_services_intense", "tradeoff_tax_sales_tax_intense",
                "tradeoff_tax_inc_tax_intense", "tradeoff_tax_wealth_tax_intense"),
    labels  = c(
      tradeoff_tax_less_services         = "No increase, even if fewer services",
      tradeoff_tax_sales_tax             = "Sales tax increase",
      tradeoff_tax_inc_tax               = "Income tax increase",
      tradeoff_tax_wealth_tax            = "Wealth tax",
      tradeoff_tax_less_services_pref    = "No increase, even if fewer services",
      tradeoff_tax_sales_tax_pref        = "Sales tax increase",
      tradeoff_tax_inc_tax_pref          = "Income tax increase",
      tradeoff_tax_wealth_tax_pref       = "Wealth tax",
      tradeoff_tax_less_services_intense = "No increase, even if fewer services",
      tradeoff_tax_sales_tax_intense     = "Sales tax increase",
      tradeoff_tax_inc_tax_intense       = "Income tax increase",
      tradeoff_tax_wealth_tax_intense    = "Wealth tax"
    )
  ),
  hc = list(
    title   = battery_titles[["hc"]],
    raw     = c("tradeoff_hc_all", "tradeoff_hc_spend", "tradeoff_hc_pensions"),
    pref    = c("tradeoff_hc_all_pref", "tradeoff_hc_spend_pref", "tradeoff_hc_pensions_pref"),
    intense = c("tradeoff_hc_all_intense", "tradeoff_hc_spend_intense", "tradeoff_hc_pensions_intense"),
    labels  = c(
      tradeoff_hc_all              = "Increase home care for all, reduce max old-age pension",
      tradeoff_hc_spend            = "Target low-income seniors, home care",
      tradeoff_hc_pensions         = "Target low-income seniors, pensions",
      tradeoff_hc_all_pref         = "Increase home care for all, reduce max old-age pension",
      tradeoff_hc_spend_pref       = "Target low-income seniors, home care",
      tradeoff_hc_pensions_pref    = "Target low-income seniors, pensions",
      tradeoff_hc_all_intense      = "Increase home care for all, reduce max old-age pension",
      tradeoff_hc_spend_intense    = "Target low-income seniors, home care",
      tradeoff_hc_pensions_intense = "Target low-income seniors, pensions"
    )
  ),
  cc2 = list(
    title   = battery_titles[["cc2"]],
    raw     = c("tradeoff_cc2_all", "tradeoff_cc2_low_inc",
                "tradeoff_cc2_educ_all", "tradeoff_cc2_educ_low_inc"),
    pref    = c("tradeoff_cc2_all_pref", "tradeoff_cc2_low_inc_pref",
                "tradeoff_cc2_educ_all_pref", "tradeoff_cc2_educ_low_inc_pref"),
    intense = c("tradeoff_cc2_all_intense", "tradeoff_cc2_low_inc_intense",
                "tradeoff_cc2_educ_all_intense", "tradeoff_cc2_educ_low_inc_intense"),
    labels  = c(
      tradeoff_cc2_all                  = "Increase child care for all, reduce other family benefits",
      tradeoff_cc2_low_inc              = "Target low-income families, childcare",
      tradeoff_cc2_educ_all             = "Target education quality for all, reduce other family benefits",
      tradeoff_cc2_educ_low_inc         = "Target education quality for low-income",
      tradeoff_cc2_all_pref             = "Increase child care for all, reduce other family benefits",
      tradeoff_cc2_low_inc_pref         = "Target low-income families, childcare",
      tradeoff_cc2_educ_all_pref        = "Target education quality for all, reduce other family benefits",
      tradeoff_cc2_educ_low_inc_pref    = "Target education quality for low-income",
      tradeoff_cc2_all_intense          = "Increase child care for all, reduce other family benefits",
      tradeoff_cc2_low_inc_intense      = "Target low-income families, childcare",
      tradeoff_cc2_educ_all_intense     = "Target education quality for all, reduce other family benefits",
      tradeoff_cc2_educ_low_inc_intense = "Target education quality for low-income"
    )
  )
)


# ── 7. INTENSE DV DEFINITIONS ─────────────────────────────────

intense_dvs <- c(
  "tradeoff_tax_wealth_tax_intense",
  "tradeoff_hc_pensions_intense",
  "soc_pol_intense",
  "cc_target_intense",
  "tradeoff_hc_all_intense",
  "hc_univ_intense",
  "tradeoff_tax_inc_tax_intense",
  "tradeoff_tax_less_services_intense",
  "cc_univ_intense",
  "tradeoff_hc_spend_intense",
  "tradeoff_cc1_no_spend_intense",
  "hc_target_intense",
  "tradeoff_ge_no_spend_intense"
)

intense_labels <- c(
  "tradeoff_tax_wealth_tax_intense"    = "Wealth tax (intense)",
  "tradeoff_hc_pensions_intense"       = "Target low-income seniors, pensions (intense)",
  "soc_pol_intense"                    = "Social policy composite (intense)",
  "cc_target_intense"                  = "Childcare: narrow beneficiaries (intense)",
  "tradeoff_hc_all_intense"            = "Home care for all seniors (intense)",
  "hc_univ_intense"                    = "Home care: broad beneficiaries (intense)",
  "tradeoff_tax_inc_tax_intense"       = "Income tax increase (intense)",
  "tradeoff_tax_less_services_intense" = "No tax increase, fewer services (intense)",
  "cc_univ_intense"                    = "Childcare: broad beneficiaries (intense)",
  "tradeoff_hc_spend_intense"          = "Target low-income seniors, home care (intense)",
  "tradeoff_cc1_no_spend_intense"      = "Don't spend more on childcare (intense)",
  "hc_target_intense"                  = "Home care: narrow beneficiaries (intense)",
  "tradeoff_ge_no_spend_intense"       = "Don't spend more on green economy (intense)"
)


# ── 8. UNIVERSAL/TARGETED DV DEFINITIONS ─────────────────────

uc_dvs <- list(
  bin = c(
    "hc_univ_bin"   = "Home care: broad beneficiaries (first choice)",
    "hc_target_bin" = "Home care: narrow beneficiaries (first choice)",
    "cc_univ_bin"   = "Childcare: broad beneficiaries (first choice)",
    "cc_target_bin" = "Childcare: narrow beneficiaries (first choice)"
  ),
  intense = c(
    "hc_univ_intense"   = "Home care: broad beneficiaries (intense)",
    "hc_target_intense" = "Home care: narrow beneficiaries (intense)",
    "cc_univ_intense"   = "Childcare: broad beneficiaries (intense)",
    "cc_target_intense" = "Childcare: narrow beneficiaries (intense)"
  )
)


# ── 9. REGIONAL LEVELS ────────────────────────────────────────

region_var    <- "ses_region_cat"
region_levels <- c("Ontario", "Quebec", "Alberta", "Atlantic Canada")

# ── 10. CANONICAL DV ORDER FOR PLOTS ─────────────────────────
# Add any new DVs here to control their panel order across all plots

dv_order <- c(
  # Importance battery
  "Healthcare", "Education", "Pensions", "Tax reduction", "Debt reduction",
  # Priority first choice
  "Healthcare access", "Home care for seniors", "Subsidized child care",
  "Support businesses and economic growth", "Fight against climate change",
  # Tradeoff cc1
  "Raise taxes", "Cut other spending", "Increase debt", "No increase, regardless of implications for issue",
  # Tradeoff tax
  "No increase, even if fewer services", "Sales tax increase",
  "Income tax increase", "Wealth tax",
  # Tradeoff hc
  "Increase home care for all, reduce max old-age pension",
  "Target low-income seniors, home care", "Target low-income seniors, pensions",
  # Tradeoff cc2
  "Increase child care for all, reduce other family benefits",
  "Target low-income families, childcare",
  "Target education quality for all, reduce other family benefits",
  "Target education quality for low-income",
  # Intense composites
  "Social policy composite (intense)",
  "Home care: broad beneficiaries (intense)", "Home care: narrow beneficiaries (intense)",
  "Childcare: broad beneficiaries (intense)", "Childcare: narrow beneficiaries (intense)",
  "Home care for all seniors (intense)",
  "Target low-income seniors, home care (intense)",
  "Target low-income seniors, pensions (intense)",
  "Income tax increase (intense)", "Wealth tax (intense)",
  "No tax increase, fewer services (intense)",
  "Don't spend more on childcare (intense)",
  "Don't spend more on green economy (intense)",
  # UC composites
  "Home care: broad beneficiaries (first choice)", "Home care: narrow beneficiaries (first choice)",
  "Childcare: broad beneficiaries (first choice)", "Childcare: narrow beneficiaries (first choice)"
)