# nfl-production-charting

## About

The `nfl-production-charting` project facilitates charting positional performance for the *Homies Dynasty Times* fantasy football newsletter.

A series of R scripts import, wrangle, and tidy the data. Data is sourced primarily from the `nflverse`, and more specifically, by using the `nflreadr` and `nflfastR` packages.

Additional data is sourced using the free version of [Fantasy Points Data Suite](https://www.fantasypoints.com).

Output is consistent charting using the custom `baseliner` theme:

![Passing Production](cpoe_epa.png)

![Receiving Production](wopr_fppt.png)

![Rushing Production](wopg_fppwo.png)