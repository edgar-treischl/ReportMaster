
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ReportMaster

<!-- badges: start -->

<!-- badges: end -->

The ReportMaster is designed to create dynamic survey reports for Lime
Survey automatically. The package checks if a survey is expired, it
retrieves data via the Lime Survey API, combines it with meta data to
create dynamic reports.

## Installation

You can install the development version of ReportMaster from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("edgar-treischl/ReportMaster")
```

## ReportMaster in Action

The `run_CronJob()` function is the main function of the package. It
checks for expired surveys, creates reports, and notifies the user via
email in case of an error. Thus, the function is designed to be run as a
cron job.

``` r
library(ReportMaster)
run_CronJob()
# Lucky fellow, there are new expired surveys!
# Joining with `by = join_by(sid)`
# Joining with `by = join_by(sid)`
# ℹ Get parameters for: Adalbert-Raps-Schule Staatliche Berufsoberschule Kulmbach
# Joining with `by = join_by(sid)`
# Joining with `by = join_by(sid)`
# Joining with `by = join_by(sid)`
# Joining with `by = join_by(sid, vars)`
# ✔ Downloaded data from LimeSurvey.
# ✔ All parameters set.   
# ✔ Create data and plots [3.3s]
# ✔ Get report infos [881ms]
# ✔ Create plots [2.1s]────────────────────────────────────────────────────── 100%
# ✔ Create tables [4.4s]───────────────────────────────────────────────────── 100%
# ✔ Exported PDF file for school '0850' and group 'elt'
```

Check out the get started vignette for more information about the
package.

## To Be Done

Anpassung der Daten für die Filterfragen. Bislang werfen wir alles weg,
was “k. A.” ist.

``` r
data <- data |> dplyr::filter(vals != "k. A.")
```

Dokumentation des Automatisierungssystems. Beispiel: In Zukunft wird ein
neues Template geben, wie greift eine Veränderung der Templates in den
Code ein?

``` r
surveystring <- "0001_202425_ubb_allg_gy_eva_00_2022_p1"
```

- SNR: Identifikation aller Umfragen via ersten vier Zeichen: 0001
- UBB: Identifikation der UBB/Umfrage via String `_ubb_`
- Schulart: Identifikation der Schulart via String `_gy_`, `_rs_`, etc.
- Ganztag: Identifikation der Ganztagsschule via String `_p2` (`p3` und
  `p4` werden im Vorfeld auf `p1` bzw. `p2` zurück gesetzt)
