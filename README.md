# Australian Household Consumption Model

This project estimates a Muellbauer-style integrated household consumption equation for Australia in R using public ABS data.

The main script is [consumption_model_australia.R](C:/Users/david/Documents/ConsModelling/R/consumption_model_australia.R), with reusable helpers in [model_helpers.R](C:/Users/david/Documents/ConsModelling/R/model_helpers.R).

What the script does:

- Downloads ABS workbooks using `readabs`.
- Downloads the ABS household income workbook used for seasonally adjusted gross disposable income.
- Parses ABS time-series workbooks with a custom reader to avoid current Windows path issues in `readabs::read_abs_local()`.
- Splices `data_raw/houseprice_old.csv` onto the ABS residential property price index to extend the house-price history back before the ABS series begins.
- Builds household income, consumption, housing wealth, illiquid financial wealth, liquid assets, debt, house prices, lending, unemployment, and a working-age-population proxy series.
- Scales real consumption and real disposable income by civilian population aged 15 years and over and constructs a permanent-income proxy from an adaptive filter on real income per person.
- Estimates a latent credit conditions index in a state-space model using `KFAS`.
- Estimates and compares several Muellbauer-style long-run consumption equations, then fits a short-run error-correction model using the selected long-run specification.
- Writes cleaned data, coefficient tables, charts, and a text summary to `outputs/`.

Run from PowerShell:

```powershell
& 'C:\Program Files\R\R-4.2.1\bin\Rscript.exe' --vanilla 'C:\Users\david\Documents\ConsModelling\R\consumption_model_australia.R'
```

Main outputs:

- [model_summary.txt](C:/Users/david/Documents/ConsModelling/outputs/model_summary.txt)
- [model_dataset.csv](C:/Users/david/Documents/ConsModelling/outputs/model_dataset.csv)
- [long_run_coefficients.csv](C:/Users/david/Documents/ConsModelling/outputs/long_run_coefficients.csv)
- [ecm_coefficients.csv](C:/Users/david/Documents/ConsModelling/outputs/ecm_coefficients.csv)
- [long_run_model_comparison.csv](C:/Users/david/Documents/ConsModelling/outputs/long_run_model_comparison.csv)
- [credit_conditions_index.png](C:/Users/david/Documents/ConsModelling/outputs/credit_conditions_index.png)
- [long_run_fit.png](C:/Users/david/Documents/ConsModelling/outputs/long_run_fit.png)
- [ecm_fit.png](C:/Users/david/Documents/ConsModelling/outputs/ecm_fit.png)

Current limitations:

- The working sample now starts in 1989Q3, which is the binding start date from the balance-sheet data rather than the house-price series.
- The long-run relation is economically interpretable, but the residual-based cointegration evidence is not yet strong. This should be treated as a first-pass empirical system, not a final policy model.
- The household balance-sheet tables are current price stocks, so the script deflates them using a household consumption deflator constructed from ABS HFCE current-price and chain-volume series.
