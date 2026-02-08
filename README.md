# RiskBasedComparison

To replicate the results reported in Trucíos (2026), use the script `Empirical_Comparison_Large_Panels.R` and set the `market` argument (line 29) as follows:

- Brazilian case: `market <- "B3"`
- US case: `market <- "US"`

The datasets used in this paper are not available in this repository due to licensing restrictions or file-size limitations. 

## Additional codes

- `Figure.R` replicates Figure 1.
- `Bootstrap_Tests.R` performs the bootstrap tests of Ledoit and Wolf (2008, 2011).

## References

- Ledoit, O. and Wolf, M. (2011). _Robust performances hypothesis testing with the variance_.
Wilmott, 2011(55):86–89.
- Ledoit, O. and Wolf, M. (2008). _Robust performance hypothesis testing with the sharpe ratio_.
Journal of Empirical Finance, 15(5):850–859.
- Trucíos, C. (2026). _Hierarchical risk clustering versus traditional risk-based portfolios: an empirical out-of-sample comparison_. Empirical Economics (Forthcoming)

