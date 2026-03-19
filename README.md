# Australian Household Consumption Model

This project estimates a Muellbauer-style integrated household consumption equation for Australia in R using public ABS and RBA-compatible public data.

The main script is [consumption_model_australia.R](R/consumption_model_australia.R), with reusable helpers in [model_helpers.R](R/model_helpers.R).

What the script does:

- Downloads ABS workbooks using `readabs`.
- Downloads the ABS household income workbook used for seasonally adjusted gross disposable income.
- Downloads an ABS population-by-age workbook used to construct interpolated cohort shares.
- Parses ABS time-series workbooks with a custom reader to avoid current Windows path issues in `readabs::read_abs_local()`.
- Normalises ABS units so stock variables reported in `$ Billions` are rescaled onto the same `$ Millions` basis as the national-accounts income flow series before wealth-to-income ratios are constructed.
- Splices `data_raw/houseprice_old.csv` onto the ABS residential property price index to extend the house-price history back before the ABS series begins.
- Builds household income, consumption, housing wealth, illiquid financial wealth, liquid assets, debt, house prices, lending, unemployment, and a working-age-population proxy series.
- Scales real consumption and real disposable income by civilian population aged 15 years and over and constructs a permanent-income proxy from an adaptive filter on real income per person.
- Estimates a latent credit conditions index in a state-space model using `KFAS`.
- Adds Muellbauer-style mortgage and cohort terms, including a mortgage cash-flow burden, an implicit real mortgage rate, a prime working-age population share, and a first-home-buyer loan share.
- Builds a transparent spline-based credit conditions composite alongside the state-space factor and allows model selection to choose between them.
- Estimates and compares several Muellbauer-style long-run consumption equations, including interaction-rich specifications where credit conditions tilt the housing-collateral, expected-income, and uncertainty channels, then fits a short-run error-correction model using the selected long-run specification.
- Writes cleaned data, coefficient tables, charts, and a text summary to `outputs/`.

Run from PowerShell:

```powershell
Rscript --vanilla R/consumption_model_australia.R
```

Main outputs:

- [model_summary.txt](outputs/model_summary.txt)
- [model_dataset.csv](outputs/model_dataset.csv)
- [long_run_coefficients.csv](outputs/long_run_coefficients.csv)
- [ecm_coefficients.csv](outputs/ecm_coefficients.csv)
- [long_run_model_comparison.csv](outputs/long_run_model_comparison.csv)
- [credit_conditions_index.png](outputs/credit_conditions_index.png)
- [credit_conditions_comparison.png](outputs/credit_conditions_comparison.png)
- [long_run_fit.png](outputs/long_run_fit.png)
- [ecm_fit.png](outputs/ecm_fit.png)

Current limitations:

- The working sample now starts in 1989Q3, which is the binding start date from the balance-sheet data rather than the house-price series.
- The preferred specification currently comes from a model-comparison exercise rather than a tightly imposed theory restriction, so coefficient stability and sign restrictions should still be reviewed before using the equation for policy work.
- The household balance-sheet tables are current price stocks, so the script deflates them using a household consumption deflator constructed from ABS HFCE current-price and chain-volume series.
- The cohort-share term is interpolated from annual ERP age data, which is useful for slow-moving demographics but not a substitute for true quarterly age-structure estimates.
