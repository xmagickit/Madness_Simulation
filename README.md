
# 2025 March Madness Forecast

This repository contains the code for fitting a dynamic Bayesian model
of NCAA team performance and simulating potential March Madness outcomes
based on the results. The model is written in
[Stan](https://mc-stan.org/) and the core pipeline is written in
[R](https://www.r-project.org/), with some modest
[Python](https://www.python.org/) and
[JavaScript](https://www.javascript.com/) as needed.

READMEs in each directory provide details on the directory contents. In
particular, the [stan/](stan/) README walks through the model in detail.
A general overview of the model methodology can be found in the [*How
this
works*](https://www.thedatadiary.net/posts/2025-03-16-march-madness/)
article, and the full output for the
[men’s](https://www.thedatadiary.net/projects/2025-march-madness/mens)
and
[women’s](https://www.thedatadiary.net/projects/2025-march-madness/womens)
brackets can be found at [the data
diary](https://www.thedatadiary.net/).

## Version history

### 1.6

###### 2025-04-07

- Modified `run_bracket_model()` to pull in the result of the
  championship game.

### 1.5

###### 2025-03-30

- Added explicit anti-join columns when writing model outputs to disk.

### 1.4

###### 2025-03-28

- Fixed minor bug in `run_bracket_model()` to enforce correct date
  filtering when finding last `wid0` used.

### 1.3

###### 2025-03-25

- `run_*_model()` functions now de-duplicate results by date/league
  before writing results out.
- `run_bracket_model()`, `generate_html_bracket()`, and
  `generate_html_table()` skip processing when no games have been
  played.

### 1.2

###### 2025-03-23

- Modify `extract_teams()` so that partially-filled games return a `0`
  instead of `NA`
- Manual override for Men’s BYU Sweet 16 game (no link available in
  ESPN)

### 1.1

###### 2025-03-21

- Modified `images.py` so that Tennessee’s women’s logo appears
  correctly.
- Updated women’s regional names.

### 1.0

###### 2025-03-20

- Initial release!
- Fixes a few minor pre-release bugs to ensure pipeline runs on 2025
  tournament data

### 0.0.9000

###### 2025-03-16

- Initial pre-release
