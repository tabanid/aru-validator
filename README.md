# ARU Validator

A small app for checking bird detections from autonomous recording units (ARUs).
It opens a validation database (one SQLite `.db` file per species), plays each short
clip with its spectrogram, and saves your label (yes / no / unsure), and the other
species you heard on the clip, straight back into the database.

## Install or update

Open R and paste this line into the console:

```r
source("https://raw.githubusercontent.com/tabanid/aru-validator/main/install_validator.R")
```

It asks where to put the app (first time only), downloads it, installs any missing
R packages, adds an **ARU Validator** shortcut to the Desktop (Windows), and starts
it. Run the same line again to update.

## Practice databases

The `example/` folder holds three small databases to practise on: 30 nocturnal
flight calls each of Canada Warbler (`CAWA-example.db`), White-throated Sparrow
(`WTSP-example.db`) and "zeep" calls (`ZEEP-example.db`), 3 s clips with the call
in the middle, unlabelled. Open one from the app. Locations are given only as a
region (e.g. S NB, N NS). An update keeps your copies and any labels in them.

## Documents

- [QUICK_START.pdf](QUICK_START.pdf): install and first use, one page
- [VALIDATOR_GUIDE.pdf](VALIDATOR_GUIDE.pdf): the full guide
- [CHEATSHEET.pdf](CHEATSHEET.pdf): keys and codes, one page

This repository holds a generated copy of the app. It is published from a private
project repository; changes made here are overwritten.
