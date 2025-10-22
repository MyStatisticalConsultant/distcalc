# Distribution Calculator — Shiny App

An interactive Shiny application for **computing and visualizing probability 
distributions**: **Normal**, **t**, **Chi-square (χ²)**, and **F**.

The calculator makes it easy to:

1. **Compute areas (probabilities)** under a selected distribution curve.
2. **Simulate random samples** from a distribution.
3. **Find critical values** for a given distribution and significance level(s).
4. **See the R code** used to generate the results.

> Live app: **https://statisticar-distcalc.share.connect.posit.cloud**  
> Feedback: **Zlatko@MyStatisticalConsultant.com**

## What this app does

- **Distributions**: Normal, t, χ², and F.
- **Parameters**: interactively adjust parameters (e.g., μ and σ for Normal; degrees 
of freedom for t/χ²/F) and see results instantly.  
- **Probabilities & critical values**: compute $P(X \le x)$, $P(a \le X \le b)$, 
tail probabilities, and quantiles/critical thresholds for a chosen $\alpha$.
- **Simulation**: draw random samples, view the histogram, and compare with 
the theoretical density
- **Visualization**: shaded regions under the curve, marked critical points, 
and clean plots for each distribution.
- **R code**: a panel shows the corresponding R functions 
(`pnorm/qnorm/rnorm`, `dt/qt/rt`, `pchisq/qchisq/rchisq`, `pf/qf/rf`) 
so you can reproduce results outside the app.

> **How to open the calculator**: click **Distribution Calculator** in the left sidebar.
This displays the output in **four boxes** (probabilities/shaded area, critical 
values, simulation, and R code).

## Why it’s useful

- **Teaching & learning**: connect formulas, graphics, and R functions interactively.
- **Quick analytics**: check thresholds and probabilities for standard tests and 
confidence intervals.
- **Reproducibility**: copy the shown R code into your own scripts.

## What you can do

- Select a distribution (Normal, t, χ², F) and set its **parameters / degrees of freedom**.
- Define an interval (`x`, or `a–b`) and compute probabilities with shaded regions on the plot.
- Enter a **significance level** $\alpha$ to get **critical values** (one- or two-tailed).
- Run a **simulation** (choose sample size and seed), then compare the histogram with the theoretical curve.
- Review and copy the **R code** that reproduces your current settings.

## Run locally

### 1) Clone the repository

```bash
git clone https://github.com/MyStatisticalConsultant/distcalc.git
cd distcalc
```

### 2) Open in RStudio

- Open `app.R` (or the RStudio project file if present).

### 3) Install packages

```r
install.packages(c(
  "shiny", "shinydashboard",
  "markdown, "openintro"
))
```

> The exact list may vary slightly with your version. If the repo uses **renv**, run `renv::restore()` instead.

### 4) Run the app

```r
shiny::runApp()    # o  click "Run App" in RStudio
```
The console will show a local URL (e.g., http://127.0.0.1:XXXX). Open it in your browser.

## Using the app

1. Click **Distribution Calculator** in the left sidebar.
2. Choose a distribution (Normal, t, χ², F).
3. Set the **parameters/df**, the **interval** (`x` or `a–b`), and α if needed.
4. Review the **four output panels**:
   - **Probabilities / shaded area** under the curve
   - **Critical values** for your selected α and tail setting
   - **Sample simulation** (histogram + theoretical curve)
   - ***R code** you can copy into your own scripts

## Feedback & support

- Live app: https://statisticar-distcalc.share.connect.posit.cloud
- Email: Zlatko@MyStatisticalConsultant.com

When reporting an issue, please include the chosen distribution, parameters, 
interval, α, and (for simulations) sample size/seed.

## License

This project is licensed under the **MIT License** — see the [LICENSE](LICENSE) file for details.

<sub>© 2025 Zlatko J. Kovačić</sub>
