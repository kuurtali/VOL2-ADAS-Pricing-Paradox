# ADAS Pricing Paradox · Vol 2

[Live demo](https://5pax70-mehmet0ali-kurt.shinyapps.io/vol2-adas-pricing-paradox/) · [Vol 1](https://github.com/kuurtali/ADAS-Pricing-Paradox) · [Portfolio](https://github.com/kuurtali) · [Report an issue](https://github.com/kuurtali/VOL2-ADAS-Pricing-Paradox/issues)

A synthetic-data actuarial pricing demonstration with approximately **200,000 policies**, driver–vehicle interaction terms, Poisson/Gamma models and saved Gini/lift analyses.

The results describe the chosen synthetic portfolio. They do not establish that real ADAS vehicles should receive a surcharge or that a segment is more profitable.

## What is included

- Shiny pricing interface using saved model coefficients.
- SQL feature engineering and an R modeling pipeline.
- Saved numerical results, model coefficients and charts.
- A Power BI report.

## Saved results

Source: [outputs/results.json](outputs/results.json). Its segment counts total **199,999** records. Percentage changes below are computed from saved segment means against ADAS level 0.

| ADAS level | Frequency | Severity | Mean risk premium |
|---|---:|---:|---:|
| 0 · Baseline | — | — | — |
| 1 · Basic | −11.3% | +11.8% | −1.0% |
| 2 · Advanced | −29.9% | +48.3% | +3.8% |

| Recorded metric | Value |
|---|---:|
| Frequency RMSE | 0.2762 |
| Gini index | 0.3247 |

These values were read from the saved outputs; this documentation update did not retrain the models. Their usefulness depends on the validation split and metric definition. A low RMSE by itself is not evidence of good performance on sparse counts.

**Vol 1 comparison:** the repositories use different portfolio sizes, and the published Vol 1 frequency RMSE is 0.3133. A controlled comparison on the same data and split is needed before claiming a percentage improvement from adding interactions.

## Method

SQL derives driver, vehicle and traffic categories. R creates an approximately 80/20 split with seed 123 and fits:

```r
Claim_Count ~ Safety_Package_Level +
  Driver_Profile * Vehicle_Class + Traffic_Zone +
  offset(log(Exposure))
# Poisson, log link

Claim_Amount ~ Safety_Package_Level + Vehicle_Class + Driver_Profile
# Gamma, log link; positive amounts only
```

Interaction terms capture conditional mean relationships; they do not by themselves prove that overdispersion is resolved. Preserve the distinction between policy-level aggregate loss and per-claim severity when preparing `Claim_Amount`.

[Frequency model summary](outputs/freq_model_summary.txt) · [Severity model summary](outputs/sev_model_summary.txt) · [Saved charts](outputs/figures)

## Run the demo

Open `app.R` in RStudio and select **Run App**, or use the live demo above. The Shiny app uses stored coefficients rather than retraining a model for each request.

## Reproduce the analysis

**Input data is missing from a fresh clone.** The repository excludes `SQL_Output2.csv` and `ham_data_final.csv`, and does not include a complete synthetic-data generator.

1. Supply the original raw data and generate the feature table with `SQL_Code.sql`, or supply the original `SQL_Output2.csv`.
2. From the repository root in R, run `source("R_Code.r")`.
3. Review the new `outputs/` files and `R_Output.csv`.
4. If using Power BI, open `ADAS_Actuarial_Pricing2.pbix` and update its local data path.

The R script tries `SQL_Output2.csv` first and then `ham_data_final.csv`. The fallback must still provide the feature columns needed by the model. Missing R packages are installed by the script.

## Files

| File | Purpose |
|---|---|
| `app.R` | Shiny demo |
| `SQL_Code.sql` | Feature engineering query |
| `R_Code.r` | Modeling and result exports |
| `outputs/results.json` | Saved metrics and segment means |
| `outputs/figures/` | Saved diagnostic, lift and comparison charts |
| `ADAS_Actuarial_Pricing2.pbix` | Power BI report |

## Next improvements

Restore the input generator/schema, make the environment reproducible, verify exposure and claim-amount units, and compare main-effects and interaction models on the same held-out data.

## License

[MIT](LICENSE) for repository code.
