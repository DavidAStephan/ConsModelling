# Australia ECM — Data Documentation

This document describes every data source used in the Australia replication, what
we extract from it, where it lives in the repo, and what's known to be missing
or fragile. The intended reader is someone who needs to either rerun the
pipeline cold, swap a series for a better one, or back-extend the sample.

The pipeline assembles a single `master` tibble keyed on quarterly dates
1980Q1–2024Q4 (n=180). All data construction lives in
[australia_data_download.R](../R/australia_data_download.R). Estimation
([australia_estimation.R](../R/australia_estimation.R)) only renames a few
columns and adds derived terms; it does not download anything.

---

## 1. Source taxonomy

Three classes of source:

1. **ABS time-series workbooks** — Excel files in [`data_raw/`](../data_raw/),
   read via `readabs::read_abs_local()` and parsed with the project's
   `read_abs_ts_workbook()` helper (which works around a Windows path bug in
   `readabs`).
2. **RBA series** — fetched live via the `readrba` package on each fresh run
   (no local cache file). Falls back to ABS-implied rates if `readrba` is not
   installed or the network is unavailable.
3. **Project-supplied CSVs** — pre-2003 house-price back-fill and a handful of
   reference files used by the Italy comparator.

All ABS workbooks are cached as RDS in [`Ausreplication/.cache/`](../.cache/)
after the first parse, so subsequent runs are fast.

---

## 2. ABS workbooks (used)

### 2.1 ABS 5206008 — Household Final Consumption Expenditure
- **File:** [`data_raw/5206008_Household_Final_Consumption_Expenditure.xlsx`](../data_raw/5206008_Household_Final_Consumption_Expenditure.xlsx)
- **Cache tag:** `abs_hfce`
- **Vintage:** as of 2025-05-05 (file mtime). Data through **2024Q4**.
- **What we extract:**
  - `cons_real` — `^FINAL CONSUMPTION EXPENDITURE.*Chain volume`, seasonally
    adjusted, quarterly, $m chain volume measures (reference year 2022-23).
  - `cons_nom` — same series but `Current prices`. Used together with
    `cons_real` to derive `cons_deflator = 100 * cons_nom / cons_real`.
- **Sample:** 1959Q3–2024Q4 (we trim to 1980Q1+).
- **Series ID convention:** ABS workbook drives series selection by `series_name`
  pattern; the helper [`pick_abs()`](../R/australia_data_download.R#L130) prefers
  `Seasonally Adjusted` then `Trend` then `Original`. We do not pin a series
  ID, so a future ABS rebase that renames the SA series will silently switch
  to a different vintage. **Gap to fix: pin the chain-volume series_id**
  (currently `A2304402X` for SA HFCE chain volume — verify and pin).

### 2.2 ABS 5206020 — Household Income Account
- **File:** [`data_raw/5206020_Household_Income.xlsx`](../data_raw/5206020_Household_Income.xlsx)
- **Cache tag:** `abs_hh_income`
- **What we extract:**
  - `ydi_nom` — gross disposable income, current prices, seasonally adjusted.
    Identified by name pattern `gross disposable income` (case insensitive),
    fallback to series_id `A2302939L`.
  - `mort_int_paid` — property income payable on dwellings (mortgage interest).
    Pattern `property income payable.*interest.*dwelling|mortgage interest`.
    **Currently used only as a fallback for the implicit mortgage rate** when
    `readrba` is unavailable.
- **Sample:** 1959Q3–2024Q4.
- **Gap to fix:** This workbook also contains `Compensation of employees` and
  `Social assistance benefits in cash` — neither is currently extracted. They
  are needed for the Italy-style **scaled-income** robustness check (Italy
  averages labour+transfer income with total disposable to down-weight
  mismeasured property income). The Italy-style robustness block currently
  skips this column. To fix:
  - Add `pick_abs(inc_raw, "Compensation of employees.*Total")` →
    `wages_nom`.
  - Add `pick_abs(inc_raw, "Social assistance benefits.*current")` →
    `social_benefits_nom`.
  - Construct `labour_transfer_nom = wages_nom + social_benefits_nom`,
    deflate by `cons_deflator`, divide by `pop_millions`, log → call
    `labour_transfer_income_real_pc` (the name the estimation script
    already looks for).

### 2.3 ABS 5232035 — Household Balance Sheet
- **File:** [`data_raw/5232035.xlsx`](../data_raw/5232035.xlsx)
- **Cache tag:** `abs_hh_bs`
- **What we extract** (all extracted via `pick_abs_bs()` which restricts to
  household sector and matches by name pattern; values rescaled from
  `$ Billions` → `$ Millions` by [`rescale_to_millions`](../R/model_helpers.R#L142)):
  - `fin_deposits` — pattern `currency and deposits`
  - `fin_equities` — pattern `shares and other equity`
  - `fin_super` — pattern `superannuation`
  - `fin_loans` — pattern `liabilities.*loans|loans and placements` (this is
    **total household debt**, including mortgages + consumer credit)
  - `housing_wealth` — pattern `residential land and dwellings`
  - `closing_net_worth` — series ID `A83722648X` (one of the few we pin
    explicitly; falls back to component sum if missing)
- **Sample binding constraint:** ABS 5232035 starts **1988Q3**. This is the
  binding sample start for every disaggregated wealth specification (Specs
  4–8). Earlier observations on consumption/income exist (back to 1959Q3 for
  HFCE, 1980Q1 for our spine) but `nla_y`, `eq_y`, `super_y`, `ha_y` are NA
  before 1988Q3.
- **Vintage:** ABS reclassifications happen periodically (notably in the
  ESA→SNA08 transitions). The current vintage is post-SNA08; if a future
  release reclassifies super out of "household financial assets", the
  pattern match will silently miss it.
- **Gap to fix:** **Sample back-extension to 1980Q1** would require sourcing
  pre-1988 ABS Financial Accounts annual data and applying Italy-style
  Bonci-Coletta splicing (see Italy.pdf Appendix A.2). The Williams 4-knot
  CCI spline explicitly needs the 1979Q1 deregulation knot identifiable,
  which currently fails because the SDMMA is constant in the post-1988
  window (see `australia_williams_cci_knots.csv` — 1979 knot is "aliased").

### 2.4 ABS 6202001 — Labour Force
- **File:** [`data_raw/6202001.xlsx`](../data_raw/6202001.xlsx)
- **Cache tag:** `abs_labour`
- **What we extract** (monthly source, averaged to quarterly):
  - `unemp_rate` — pattern `^Unemployment rate.*Persons`, SA
  - `labour_force` — pattern `^Labour force.*Persons`, SA, persons (thousands)
- **Sample:** 1978Q2–2024Q4 (the start date is ABS, not our spine).
- **Note:** `lf_share = labour_force / pop_millions` is constructed downstream;
  both numerator and denominator are nominally in thousands of persons (the
  master column is named `pop_millions` for legacy reasons but actually carries
  thousands — see Section 6 unit gotcha).

### 2.5 ABS 3101059 — Estimated Resident Population
- **File:** [`data_raw/3101059.xlsx`](../data_raw/3101059.xlsx)
- **Cache tag:** `abs_pop`
- **What we extract:**
  - `pop_thousands` (renamed `pop_millions` despite being in thousands —
    see Section 6) — sum of `^Estimated Resident Population ; Persons ; <age> ;`
    over all single-year cohorts. Annual source, splined to quarterly via
    [`annual_to_quarterly_spline()`](../R/australia_data_download.R#L114).
  - `prime_age_share` — sum of single-year cohorts aged 25–54 over total ERP.
    Uses the `Male` + `Female` split (not `Persons`) because in current ABS
    vintages the `Persons` series only goes up to age 47 — see "Bug" below.
- **Bug, partially worked around (acknowledged in code at
  [`australia_data_download.R:303`](../R/australia_data_download.R#L303)):**
  the existing `pop_q` total uses the `^Persons` pattern which only matches
  ages 0–47 in the current vintage, undercounting Australia's population by
  ~40%. `prime_age_share` sidesteps this by using consistent Male+Female
  numerator and denominator (so the *ratio* is correct), but `pop_millions`
  itself is wrong-by-a-large-factor. Per-capita series like `cons_real_pc`
  divide by this wrong total, but the bias cancels because every per-capita
  series uses the same denominator and the dependent variable is in *changes*
  of *logs*. **Gap to fix: rebuild `pop_q` to use `Male + Female` cohorts
  the same way `prime_age_share` does**, and rename `pop_millions` →
  `pop_thousands` everywhere (single-line rename across data_download.R and
  estimation.R).
- **Vintage caveat:** ABS sometimes restructures cohort series; future
  vintages may extend `Persons` past age 47, in which case the workaround
  becomes unnecessary (but the column rename is still needed).

### 2.6 ABS 643201 — Total Value of Dwellings
- **File:** [`data_raw/643201.xlsx`](../data_raw/643201.xlsx)
- **Cache tag:** `abs_tvd`
- **What we extract:**
  - `hpi_current` — `Mean price of residential dwellings.*Australia`,
    Original (no SA available). Quarterly, $ thousands per dwelling.
- **Sample:** 2003Q3–2024Q4. This is the **anchor** of the chained
  house-price series — the contemporaneous portion of `hpi`.
- **Note:** This is a *level* series (mean dollar price), not an index.
  The splicing chain rescales the bridge and legacy series to its level.

### 2.7 ABS 641601 — Residential Property Price Indexes
- **File:** [`data_raw/641601.xlsx`](../data_raw/641601.xlsx)
- **Cache tag:** `abs_rppi`
- **What we extract:**
  - `hpi_bridge` — `Residential Property Price Index.*eight capital cities`,
    Original. Quarterly, index level (2011-12 = 100).
- **Sample:** 2003Q4–2017Q2 (workbook discontinued by ABS after 2017).
- **Role:** middle layer of the splice — bridges the legacy 1986–2003 series
  to the current 643201 series.

### 2.8 ABS 560101 — Lending Indicators
- **File:** [`data_raw/560101.xlsx`](../data_raw/560101.xlsx)
- **Cache tag:** `abs_credit`
- **What we extract** (monthly source, averaged to quarterly):
  - `housing_loan_flow` — `Households.*Housing Finance.*Total dwellings.*New loan commitments.*Value`, SA, $ millions
  - `fhb_loans` — `; First Home Buyers ;.*New loan commitments.*Number`, SA.
    The leading `; ` is critical: without it the pattern also matches
    `; Non-First Home Buyers ;`, and `pick_abs()` resolves both regexes to
    the same series, leaving `fhb_share` constant at 0.5. **This bug was
    fixed during Step T3.1**; if you change the regex, retain the leading
    `; `.
  - `non_fhb_loans` — `; Non-First Home Buyers ;.*New loan commitments.*Number`, SA.
- **Sample:** 2002Q3–2024Q4 (binding constraint for the post-2002 CCI proxy).
- **Construction:** `fhb_share = fhb_loans / (fhb_loans + non_fhb_loans)`.
  Range after the regex fix: 0.22–0.48.

### 2.9 ABS 5204055011do001-do005 — National Accounts supplements
- **Files:** [`data_raw/5204055011do001.xlsx`](../data_raw/5204055011do001.xlsx)
  through `do005.xlsx`.
- **Status: NOT USED.** These workbooks are present in `data_raw/` but no
  call site references them. They appear to be leftover from an earlier
  exploration. **Gap to fix: either delete them or wire them in if they
  contain something useful** (annual national-accounts depth that could
  back-extend the disposable-income series, perhaps).

### 2.10 ABS 5232001–5232034, 5232036+ — other balance-sheet workbooks
- **Files:** 60 workbooks under `data_raw/` matching `5232*.xlsx`.
- **Status: NOT USED** except for `5232035`. These are sectoral or
  by-instrument breakdowns; if needed for a deeper wealth decomposition
  (e.g., separating bonds from deposits — Italy treats bonds as a
  semi-liquid bucket) they're already on disk. **Gap to investigate:**
  whether any of these series provide a better breakdown of household
  illiquid wealth into bonds vs equities than the aggregated 5232035
  treatment we currently use.

### 2.11 ABS 3101059 supplementary cohort detail
The cohort handling in 3101059 is fragile across vintages. Any change to ABS
naming conventions for single-year cohorts will silently break either
`pop_millions` (silently — the variable will exist but be wrong) or
`prime_age_share` (loudly — `tryCatch` falls back to NA). **The data
assertion at end of the script does not catch this** because both
`prime_age_share` and `pop_millions` have looser bounds than the actual
problem.

---

## 3. RBA series (live fetch)

The pipeline pulls two RBA series via `readrba::read_rba_seriesid()` on each
fresh run. **No local cache file exists for RBA data**; if `readrba` is not
installed, the pipeline falls back to ABS-derived implicit rates with
substantially different scale.

### 3.1 RBA FILRHLBVS — Standard variable owner-occupier mortgage rate
- **Used as:** `mortgage_rate`
- **Frequency:** monthly, averaged to quarterly
- **Range:** ~3% to ~17% historically; ~6–8% in recent years
- **Fallback:** if `readrba` is unavailable, the pipeline computes
  `mortgage_rate = 400 * mort_int_paid / lag(fin_loans, 1L)` (annualised
  effective rate from ABS national accounts). The fallback gives
  systematically *lower* rates (~2–8%) because `mort_int_paid` is net of
  capitalised interest and includes interest-only loans differently. **This
  has implications for `real_rate`, `mortgage_burden`, and any downstream
  Drehmann adjustment.** The previous pre-RBA-fallback runs of this
  pipeline used the implicit rate; the published BIS-Williams paper uses the
  RBA SVR. **Gap to fix: cache the RBA series locally as `data_raw/rba_filrhlbvs.csv`
  so the choice between RBA and implicit is deterministic and visible in
  source control.**

### 3.2 RBA FIRMMCRTD / FOOIRATCR — Cash rate
- **Used as:** `cash_rate`
- **Status:** Loaded but only used inside the legacy spread-backfill CCI
  branch (which is currently disabled since `USE_INSTITUTIONAL_CCI = FALSE`
  by default). If `USE_INSTITUTIONAL_CCI` is flipped to `TRUE`, this series
  becomes load-bearing.

---

## 4. Project-supplied CSVs

### 4.1 `data_raw/houseprice_old.csv`
- **Format:** 77 rows, two columns: `Date` (e.g. `Jun-1986`), `HousePriceOld`
  (numeric, index level).
- **Provenance:** **UNDOCUMENTED IN-REPO.** The README mentions "the legacy
  ABS eight-capital-city residential property price index" but the actual
  source workbook isn't preserved. Looking at the values (61.3 in Jun-1986
  rising), it's consistent with a pre-2003 ABS RPPI vintage (Cat 6416
  series, base year unclear — the splicing rescales it to the current
  643201 level so the base is irrelevant downstream).
- **Sample:** 1986Q2–2003Q3 (overlaps the start of `hpi_bridge`).
- **Role:** earliest layer of the spliced house-price series; provides
  pre-2003 coverage.
- **Gap to fix: document the provenance** — record the original ABS catalogue
  number, vintage date, source URL, and date downloaded. If sourced from a
  third-party (e.g. an RBA chartpack or a private compilation), record that.

### 4.2 `data_raw/e13-data.csv`
- **Format:** RBA E13 housing-loan-payments table (12 columns: scheduled vs
  excess repayments, interest, payment-to-income ratios, offset/redraw share,
  split by owner-occupied vs investment).
- **Source:** Published quarterly by RBA jointly with APRA.
- **Status: WIRED IN.** Two ratios extracted:
  - **`mortgage_interest_burden_rba`** — series `LPHTICRI`, interest charged
    on total housing loans / household disposable income, in fraction.
    Quarterly SA; coverage 2009Q1–2024Q4 (n=64).
  - **`mortgage_payment_burden_rba`** — series `LPHTSPRI`, scheduled
    repayments on total housing loans / disposable income, in fraction.
    The closer Muellbauer cash-flow burden analogue (interest + principal).
    Same coverage and frequency.
- **Coverage caveat:** starts only 2009Q1 because RBA-APRA Common Reporting
  Standard for housing loans started then. CANNOT replace the synthetic
  `mortgage_burden` (which goes back to 1988Q3) without losing ~80 quarters
  of Spec 7 history. Both series coexist in `master`.
- **Substantive comparison vs synthetic** (over 2009Q1–2024Q4 overlap):
  - Synthetic mortgage_burden mean = 0.107 (overstated — uses total
    household debt × headline SVR)
  - RBA interest burden mean = 0.053 (housing-only interest)
  - RBA payment burden mean = 0.081 (interest + principal)
  - cor(synthetic, RBA payment) = **0.93** — synthetic captures the cycle
    well but is biased ~30% high in level
- **Open follow-up:** decide whether to add a Spec 7b that uses
  `mortgage_payment_burden_rba` over the post-2009 sample for an explicit
  measured-vs-synthetic comparison.

### 4.3 `outputs/italy_table1_results.csv` (reference benchmark)
- **Format:** Hand-coded Italy reference numbers from De Bonis et al.
  (2024) Table 1, used by `build_comparison_table()` in
  `australia_estimation.R` for the cross-country comparison output
  `italy_australia_comparison.csv`.
- **Status: REFERENCE ONLY.** This file contains *published* coefficient
  values from the Italian comparator paper; the Italy estimation pipeline
  itself was removed during the May 2026 repo cleanup. The Australia
  pipeline does not re-estimate Italy.

---

## 5. Splicing logic

### 5.1 House price index (3-layer splice)
Implemented in
[`australia_data_download.R:599-624`](../R/australia_data_download.R#L599) via
the `splice_hpi()` helper.

```
Layer 1 (LEGACY):  houseprice_old.csv     [1986Q2 – 2003Q3]
       ↓ chain on overlap with bridge
Layer 2 (BRIDGE):  ABS 641601 RPPI 8CC    [2003Q4 – 2017Q2]
       ↓ chain on overlap with current
Layer 3 (CURRENT): ABS 643201 mean price  [2003Q3 – 2024Q4]
```

The chain mechanism: for each adjacent pair, compute
`scale = mean(value_overlay / value_base)` over their overlap, multiply the
base series by `scale`, then bind the rescaled base to the overlay. This
preserves *growth rates* of the base while pinning *levels* to the overlay.
Inevitable distortion: the overlap window is short (a few quarters between
RPPI start and TVD start), so the scale factor is sensitive to those
quarters.

**Gap to consider:** the splice could be replaced by a single longer source
(e.g. CoreLogic's historical hedonic series, which goes back to 1980 with a
single methodology). Trade-off: CoreLogic is proprietary; the ABS-only
chain is reproducible from public data.

### 5.2 Credit-conditions index (CCI) — multi-source
The CCI currently has **two operational paths** controlled by the
`USE_INSTITUTIONAL_CCI` flag in `australia_data_download.R:70`:

**Path A — Default (flag = FALSE):** flow-based observable CCI from 2002Q3
only.
```
cci_ratio = log(housing_loan_flow / ydi_ann_8qma)
```
Sample: 90 quarters (2002Q3+). Specs 2 and 5 effectively start 2002Q3 with
this path. The pre-2002 spread-backfill that previously existed has been
dropped (was theoretically suspect — spread reflects funding cost, not
credit availability).

**Path B — Optional (flag = TRUE):** Maximal-GETS Australian institutional
CCI spline + observable-CCI overlay.
- **15 candidate** smoothed-step dummies at the documented Australian
  financial-policy turning points (Campbell '79, housing dereg '86,
  state-bank distress '90, banking distress '92/'93, Wallis/APRA '98,
  GFC '07, deposit guarantee '08, FHB Boost '09, APRA macropru '14/'17,
  Hayne RC '19, APRA cap removal '19Q3, COVID '20, buffer hike '21).
- Each SDMMA = 5-quarter MA of 4-quarter MA of a 0/1 step at the knot
  date, giving an 8-quarter S-shaped transition.
- Coefficients estimated inside the consumption equation by general-to-
  specific drop-on-violation (Hendry/Krolzig 2005) of sign priors
  (deregulation/loosening = +, retrenchment/tightening = −).
- Surviving knots combined into `cci_williams`, peak-normalised to 1.
- See [`model_helpers.R`](../R/model_helpers.R) `build_williams_cci_basis()`
  (and `build_williams_cci_basis_canonical()` for Williams' original 4-knot
  set retained as a robustness benchmark) plus
  [`australia_estimation.R`](../R/australia_estimation.R)
  `fit_consumption_with_williams_cci()`.
- **Why maximal-GETS instead of Williams' canonical 4-knot:** the May 2026
  knot experiment (NS-115) showed that on the 1988+ sample only one of
  Williams' four knots (2007Q1) survives sign-prior reduction; the others
  alias (1979) or violate priors (1992, 1998). The maximal-GETS approach
  lets the data choose which of 15 candidate institutional events generate
  identifiable variation, producing 5-6 surviving knots with a richer
  empirical signal. See
  [`knot_experiment_findings.md`](knot_experiment_findings.md) for the
  full analysis.
- **Current outcome on the 1988Q4+ sample:** 6 knots survive — 1992Q1
  (banking distress), 2007Q3 (GFC), 2009Q1 (FHB Boost), 2019Q1 (Hayne
  RC), 2020Q2 (COVID/JobKeeper), 2021Q4 (APRA buffer hike). 1979Q1 and
  1986Q1 aliased; 1990Q3, 1993Q1, 1998Q3, 2008Q4, 2014Q4, 2017Q1, 2019Q3
  sign-violators (dropped). See
  [`outputs/australia_williams_cci_knots.csv`](../outputs/australia_williams_cci_knots.csv).

**Gap to fix (would unlock Path B fully):** sample back-extension to 1980Q1
via Bonci-Coletta splicing of pre-1988 ABS annual balance-sheet data. The
1979 deregulation knot is only identifiable if the post-1980 SDMMA isn't
constant; with current data it transitions before our sample begins.

---

## 6. Unit conventions and gotchas

### 6.1 Currency units
ABS national-accounts flows (5206008, 5206020) are reported in **$ millions**.
ABS balance sheets (5232035) are reported in **$ billions**. The
[`rescale_to_millions()`](../R/model_helpers.R#L142) helper converts
balance-sheet values to **$ millions** at parse time, so all downstream
ratios (`*_y` series) are dimensionless. Real series are deflated by
`cons_deflator_norm` (the chain-volume implicit deflator, normalised so
2022-23 = 100). Per-capita series divide by `pop_millions`.

### 6.2 The `pop_millions` mis-naming
**`pop_millions` is in fact in *thousands* of persons.** This is a legacy
naming bug. Since both numerator and denominator of every per-capita ratio
share this column, the 1000× scale error cancels in `cons_real_pc` and
`ydi_real_pc`. It does *not* cancel in:
- The `lf_share = labour_force / pop_millions` ratio (both numerator and
  denominator share the same wrong unit, so the ratio is dimensionally
  correct — but the *interpretation* in messages and assertions assumes a
  share, which it is, despite the unit confusion).
- Any downstream code that reads `pop_millions` expecting actual millions
  and multiplies by something with a "per million" semantic. **There is no
  such code currently**, but if you add one (e.g. computing GDP per capita
  for cross-country normalisation), beware.

### 6.3 Date conventions
ABS workbooks use the **first day of the *last* month** of each quarter
(Sep 1, Dec 1, Mar 1, Jun 1). Our spine uses the **first day of the *first*
month** (Jul 1, Oct 1, Jan 1, Apr 1). The conversion is done by
`abs_to_qstart()` via `lubridate::floor_date(date, unit = "quarter")` at
each parse site. Failing to convert produces a silent join failure (no rows
match in the `master` left-join chain).

### 6.4 Frequency conversions
- Monthly → quarterly: simple average over 3 months
  (`monthly_to_quarterly()`).
- Annual → quarterly: cubic spline interpolation
  (`annual_to_quarterly_spline()`), with `approx(rule = 2)` fallback. Used
  for `pop_q` and `prime_age_share`. **Spline is appropriate for slow-moving
  demographic series; would be wrong for series with intra-annual
  seasonality**.

### 6.5 Mortgage rate: RBA vs ABS-implicit
See Section 3.1 — the choice between live RBA fetch and ABS fallback can
shift `mortgage_rate` by 2–8 percentage points and silently change every
downstream interest-rate-related coefficient. **The current pipeline does
not record which source was used in the output dataset metadata.**

---

## 7. Coverage table (current vintage)

From the most recent run, `master` has 180 quarterly rows (1980Q1–2024Q4)
and 50+ columns. Key coverage windows (see
[`outputs/australia_model_dataset.csv`](../outputs/australia_model_dataset.csv)
for the full list):

| Variable               | First obs   | Last obs    | n   | Notes                                          |
| ---------------------- | ----------- | ----------- | --- | ---------------------------------------------- |
| `cons_real`            | 1980-01-01  | 2024-10-01  | 180 | ABS 5206008 chain volume                       |
| `ydi_nom`              | 1980-01-01  | 2024-10-01  | 180 | ABS 5206020                                    |
| `unemp_rate`           | 1980-01-01  | 2024-10-01  | 180 | ABS 6202001                                    |
| `pop_millions`         | 1980-01-01  | 2024-10-01  | 180 | (in thousands; see §6.2)                       |
| `prime_age_share`      | 1980-01-01  | 2024-10-01  | 180 | Splined annual ERP                             |
| `hpi`                  | 1986-04-01  | 2024-10-01  | 155 | Spliced 3-layer                                |
| `fin_deposits`/`_loans`/`super`/`equities`/`housing_wealth` | 1988-07-01 | 2024-10-01 | 146 | **Binding sample start**                       |
| `mortgage_rate`        | 1988-10-01  | 2024-10-01  | 145 | RBA SVR (or ABS-implicit fallback)             |
| `housing_loan_flow`    | 2002-07-01  | 2024-10-01  | 90  | ABS 560101                                     |
| `fhb_share`            | 2002-07-01  | 2024-10-01  | 90  | (after regex fix; was constant 0.5)            |
| `cci_ratio`            | 2002-07-01  | 2024-10-01  | 90  | Specs 2, 5 effectively start here              |
| `mortgage_burden`      | 1988-10-01  | 2024-10-01  | 145 | Synthetic                                      |
| `labour_force`         | (?) live    | 2024-10-01  |     | Added recently; not yet in cached RDS          |
| `lf_share`             | (?) live    | 2024-10-01  |     | Added recently; not yet in cached RDS          |

**Discrepancy between cached RDS and current pipeline:** the cached
[`outputs/australia_model_dataset.rds`](../outputs/australia_model_dataset.rds)
was produced before `labour_force`, `lf_share`, the new Australian narrative
dummies, `nla_y_unrestricted`, and the cohort additions were wired in. The
estimation script's `add_model_variables()` reconstructs these on the fly
when running `run_estimation_from_rds.R`, so the pipeline still works end-to-
end, but the RDS itself is stale. **A fresh run of `australia_consumption_model.R`
(which downloads data) is needed to refresh the RDS.** Until then, anyone
reading the RDS directly will see an older variable set.

---

## 8. Synthetic / derived series

These are constructed from raw inputs in `australia_data_download.R` and used
directly in estimation. None of them are downloaded.

| Series | Definition | Sample | Used in |
| --- | --- | --- | --- |
| `cons_deflator` | `100 * cons_nom / cons_real` | 1980Q1+ | All real series |
| `cons_deflator_norm` | `cons_deflator / mean(2022 obs) * 100` | 1980Q1+ | Per-capita real series |
| `ydi_real_pc` | `ydi_nom / cons_deflator_norm * 100 / pop_millions` | 1980Q1+ | All specs (`lincome`) |
| `cons_real_pc` | `cons_real / pop_millions` | 1980Q1+ | All specs (`lcons`) |
| `ydi_ann_nom` | `4 * ydi_nom` | 1980Q1+ | Wealth/income ratios denominator |
| `ydi_ann_8qma` | 8-quarter MA of `ydi_ann_nom` | 1981Q4+ | CCI denominator |
| `ha_y` | `housing_wealth_r / ydi_ann_r` | 1988Q3+ | Specs 4-7 |
| `nla_y` | `(fin_deposits - fin_loans) / ydi_ann` | 1988Q3+ | Specs 4-7 |
| `nla_y_unrestricted` | `fin_deposits / ydi_ann` | 1988Q3+ | NLA Wald restriction test only |
| `debt_y` | `fin_loans / ydi_ann` | 1988Q3+ | NLA Wald restriction test only |
| `eq_y`, `super_y`, `ilfa_y` | corresponding `_r / ydi_ann_r` | 1988Q3+ | Specs 4-7 |
| `networth_y` | `closing_net_worth_r / ydi_ann_r` | 1988Q3+ | Specs 1-3 |
| `ln_hp_over_y` | `log(hpi / (real per-capita income))` | 1986Q2+ | All specs (mandated) |
| `hicp_4q_ann` | 4-quarter % change in deflator | 1981Q1+ | Real-rate denominator |
| `real_rate` | `mortgage_rate - hicp_4q_ann` | 1981Q1+ | All specs |
| `mortgage_burden` | `(fin_loans * mortgage_rate / 100) / ydi_ann_nom` | 1988Q4+ | Spec 7 (now dropped — see §10) |
| `cci_ratio` | `log(housing_loan_flow / ydi_ann_8qma)` | 2002Q3+ | Specs 2, 5 SR term |
| `fhb_share` | `fhb_loans / (fhb_loans + non_fhb_loans)` | 2002Q3+ | Spec 7 |
| Williams basis `sdmma_*` | 5q-MA of 4q-MA of step at 1979/1992/1998/2007 | 1980Q1+ when flag on | Spec 8 (when CCI on) |
| `cci_williams` | Sum of surviving SDMMAs × OLS coefs, peak-normalised | 1980Q1+ when flag on | Spec 8 |

### Dummies
| Variable | Definition | Source |
| --- | --- | --- |
| `d2000_gst` | 1 at 2000Q3 | One-off GST introduction (consumption pull-forward) |
| `d2008_gfc` | 1 at 2008Q3 | GFC episode |
| `d2020_covid` | 1 at 2020Q2 | COVID lockdown shock |
| `d2020_rebound` | 1 at 2020Q3 | Mechanical reversal |
| `d_neg_gearing_8587` | 1 from 1985Q3 to 1987Q3 (9 quarters) | Negative-gearing tax restriction (Aust paper p.14) |
| `d_recession_1991` | 1 at 1991Q2 | "Recession we had to have" |
| `d_apra_2014` | Logistic centred 2014Q4 | Macroprudential investor-loan caps (smooth transition) |
| `d_apra_2017` | Logistic centred 2017Q2 | Macroprudential interest-only caps (smooth transition) |
| `d_jobkeeper_2020` | 1 from 2020Q2 to 2021Q1 (4 quarters) | JobKeeper income support |

**Gap on dummies:** the macroprudential ogive uses `half_width = 2.5`
(quarters). This is a modelling choice without a paper citation; it could
plausibly be 1.5 or 4.5. Sensitivity not tested.

---

## 9. Known data gaps — concrete fix-list

In rough order of payoff:

1. **Source `compensation_of_employees` and `social_assistance_benefits`
   from 5206020** to enable Italy-style scaled-income robustness check.
   Currently the IV-style robustness block skips this column. ~30 mins of
   work in `australia_data_download.R`. **(§2.2)**
2. **Cache the RBA mortgage rate locally** as
   `data_raw/rba_filrhlbvs.csv` and load deterministically; record source
   + vintage in the cached RDS metadata. Removes the silent RBA-vs-ABS
   fallback divergence. **(§3.1)**
3. **Refresh the cached RDS** by running a full
   `australia_consumption_model.R` so the RDS contains all the new
   variables (currently `add_model_variables()` reconstructs them on the
   fly). Just runtime; no code change. **(§7)**
4. **Document the provenance of `houseprice_old.csv`** — original ABS
   catalogue number, vintage, download date. ~15 mins. **(§4.1)**
5. **Fix the `pop_millions` naming** — rename to `pop_thousands`
   everywhere, fix `pop_q` to use Male+Female cohorts. Single-line rename
   plus a one-helper change. **(§2.5, §6.2)**
6. **(DONE)** RBA E13 wired in as `mortgage_interest_burden_rba` and
   `mortgage_payment_burden_rba` (2009Q1+, 64 obs). Coexists with the
   synthetic `mortgage_burden` (1988Q3+) so Spec 7's pre-2009 history
   is preserved. Open question: add a Spec 7b that uses the RBA payment
   burden over post-2009 sample for an explicit measured-vs-synthetic
   comparison. **(§4.2)**
7. **Source pre-1988 ABS Financial Accounts annual data** for sample back-
   extension via Bonci-Coletta splicing. Required to identify the 1979
   deregulation knot of the Williams CCI. Days of work; needs ABS Time
   Series Service or Stat Pioneer. **(§2.3, §5.2)**
8. **Pin ABS series IDs** on the workbooks we already use, instead of
   relying on regex matches. Insulates against ABS rebases that rename
   series. **(§2.1, all sections)**
9. **Either delete or wire in** the unused 60+ workbooks under
   `data_raw/52*.xlsx` (other 5232 sectoral breakdowns) and the 5
   `5204055011do001-005` workbooks. **(§2.9, §2.10)**
10. **Investigate Italy-style bonds-as-separate-bucket** — Italy treats
    bonds as a semi-liquid wealth bucket distinct from equities/super.
    The relevant breakdown may already be in one of the unused 5232
    workbooks. **(§2.10)**
11. **Add a vintage-trace column to `master`** recording the file mtime of
    each source workbook at parse time, so the output RDS carries
    reproducibility metadata. ~20 mins.

---

## 10. Caching and reproducibility

### Cache location
[`Ausreplication/.cache/`](../.cache/) holds RDS pickles of parsed ABS
workbooks (one per `read_abs_cached()` call). Cache is keyed by the `tag`
argument, not by file mtime — **changing the workbook in `data_raw/` does
NOT invalidate the cache**. To force a re-parse:
- Delete the relevant `.cache/abs_*.rds`, or
- Delete the entire `.cache/` directory.

### Three execution modes
1. **Cold rebuild (downloads + estimates):**
   ```
   Rscript Ausreplication/R/australia_consumption_model.R
   ```
   Reads ABS workbooks (cached), fetches RBA series live, builds `master`,
   saves `australia_model_dataset.rds`, then runs estimation. Required if
   `data_raw/` workbooks change.
2. **Fast re-estimate from RDS (no data work):**
   ```
   Rscript Ausreplication/R/run_estimation_from_rds.R
   ```
   Loads the pre-built RDS, runs only the estimation script. Used by CI and
   for quick iteration. **Bit-identical reproduction.**
3. **Re-estimate from a portable CSV (no downloads, hand-editable):**
   ```
   Rscript Ausreplication/R/load_master_from_csv.R
   ```
   Loads `data_raw/master_data.csv`, reconstructs `master`, runs estimation.
   Useful when:
   - You don't have internet access for the live RBA fetch
   - You want a frozen, version-controllable, human-readable snapshot
   - You want to patch a known data error by hand-editing one cell of the
     CSV before running

### CSV workflow (for offline / hand-edit use)

Generate the CSV from the current cached RDS:
```
Rscript Ausreplication/R/export_master_csv.R
```
This produces [`data_raw/master_data.csv`](../data_raw/master_data.csv) (180
rows × 60 columns, ~159 KB) using base R `write.table` with 17 significant
digits — full double-precision binary round-trip is preserved at the bit
level for ~99% of cells, with the rest at machine epsilon (~1e-10 max abs
diff per column, all of which are billion-scale balance-sheet values where
the diff is at the limit of IEEE 754 double precision).

The exporter also backfills variables that the estimation script normally
reconstructs at runtime (`nla_y_unrestricted`, the new Australian narrative
dummies) so the CSV is self-contained.

Then load and re-run with:
```
Rscript Ausreplication/R/load_master_from_csv.R
```

**Caveat: CSV vs RDS path can diverge on Chow-borderline selector flags.**
The numerical noise (~1e-10) is at machine precision, but
`strucchange::sctest` is bit-sensitive when the Chow statistic is near a
critical value. The substantive results (BICs to 12+ digits, all
coefficients to 6+ digits, qualitative signs and magnitudes) are
essentially identical between RDS and CSV paths. Only edge-case
`pass_stability` flags can flip on specs whose Chow p-value is near 0.01.
**The RDS remains the canonical source for bit-identical reproduction.** The
CSV is for portability and hand-editing.

If your run produces a different preferred-spec selection than the
documented Spec 6, check whether you ran the CSV path; the substantive
analysis is unchanged either way.

### Not in source control (but should be)
- `Ausreplication/outputs/australia_model_dataset.rds` IS in source control
  (small enough — ~50 KB) so CI can use it.
- Cache directory `.cache/` is **not** in source control. CI rebuilds from
  the workbooks on first run.
- RBA fetch results are not cached anywhere — see Gap §3.1.

---

## 11. Italy comparator (REMOVED — May 2026 cleanup)

The original repository included a parallel Italy estimation pipeline
(under `data_raw_italy/` and `R/italy_*.R`). This was removed during the
May 2026 repo cleanup because the Australia paper does not re-estimate
Italy; instead it compares to *published* numbers from De Bonis et al.
(2024). The single reference file retained is
[`outputs/italy_table1_results.csv`](../outputs/italy_table1_results.csv)
(hand-coded from the Italy paper's Table 1), which is read by
`build_comparison_table()` to produce
[`outputs/italy_australia_comparison.csv`](../outputs/italy_australia_comparison.csv)
and `italy_australia_lambda.csv`. These are *outputs* of the Australia
pipeline, not Italy outputs.
