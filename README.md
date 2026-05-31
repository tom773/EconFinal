# This is the github repository for the ECON1195 Final Assessment

## How to run:

1. Download the Zip file [here](https://github.com/tom773/EconFinal/archive/refs/heads/master.zip)
2. Extract it somewhere (documents or desktop is fine)
3. Open Rstudio then, in the console in the bottom left corner, paste this 
```r
pkgs <- c("tidyverse", "quantmod", "gt", "patchwork", "tseries",
          "TTR", "FinTS", "rugarch", "broom", "ggrepel",
          "e1071", "showtext", "sysfonts")
install.packages(setdiff(pkgs, rownames(installed.packages())))
```
4. Open main.r in Rstudio, or create a new project by pointing to the downloaded folder. Or set working directory to that folder.
5. In console type ```source("main.r")``` and then ```run_all()```

* If you downloaded the ZIP, the tables and plots folders will be full. Each time you run ```run_all()``` they will be overwritten. That wont change them unless something in the code has changed

## Making changes:

You can change the numbers from the console, or via the actual code. 

Example for changing the date range:

```r
run_all(from = "2018-01-01", to = "2023-12-31")
```

This tells ```prepare_data()``` to use those dates instead of the default (2020-01-01 - 2025-01-01).

You could also manually run that function, store the result in an object and then run individual sections:

```r
dat <- prepare_data(from = "2018-01-01", to = "2023-12-31")
run_corr(dat)
run_garch(dat)
```

### Change the portfolio recommendation

run_reccs() is the most important section. Running with its default inputs:
```r
run_reccs(
  dat,
  A0_total = 1500000,                          # total dollars to invest
  p_var    = 0.05,                             # VaR tail probability (0.05 = 95% VaR)
  w_rec    = c(KO = 0.05, NVDA = 0.65, XOM = 0.30)  # recommended weights (must sum to 1)
)
```

So to test a more conservative book with $2M and a 99% VaR:

```r
rrun_reccs(dat, A0_total = 2000000, p_var = 0.01,
          w_rec = c(KO = 0.40, NVDA = 0.20, XOM = 0.40))
```

#### Changing the volatility model (Probs don't need to do this)

The GARCH specification lives in setup.R in a function called .garch_spec()
it's an ARMA(0,0)-GARCH(1,1) with a Student-t distribution by default. This is what the assignment spec
called for IIRC.

If you want to experiment with the distribution or the GARCH order, that's the one
function to edit; both garch.r and reccs.r use it, so a change there flows
through consistently.

## If something goes wrong

```could not find function "..."``` - the packages didn't load. Re-run
source("main.r") (after setting the working directory), and make sure the
install block in section 1a finished without errors.

It can't find ```setup.R``` / ```theme_dark_roboto.R``` / ```data/ffm.csv``` - your
working directory isn't the project folder. Redo step 4 of "How to run".

A ```font``` / ```showtext``` warning - harmless; the charts still render. It just
means the Roboto download was skipped (usually no internet).

The download is slow or fails - that's Yahoo Finance. Check your
connection and try ```dat <- prepare_data()``` again.

