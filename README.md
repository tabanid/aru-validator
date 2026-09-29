# ARU Validator

A small app for checking bird detections from autonomous recording units (ARUs).
It opens a validation database (one SQLite `.db` file per species), plays each short
clip with its spectrogram, and saves your label (yes / no / other bird / insect /
peeper / unsure) straight back into the database.

## Install or update

Open R and paste this line into the console:

```r
source("https://raw.githubusercontent.com/tabanid/aru-validator/main/install_validator.R")
```

It asks where to put the app (first time only), downloads it, installs any missing
R packages, adds an **ARU Validator** shortcut to the Desktop (Windows), and starts
it. Run the same line again to update.

## Documents

- [QUICK_START.pdf](QUICK_START.pdf): install and first use, one page
- [VALIDATOR_GUIDE.pdf](VALIDATOR_GUIDE.pdf): the full guide
- [CHEATSHEET.pdf](CHEATSHEET.pdf): keys and codes, one page

This repository holds a generated copy of the app. It is published from a private
project repository; changes made here are overwritten.
