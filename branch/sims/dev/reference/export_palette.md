# Export palette to various formats

Export a huerd palette to common formats used in design and development
workflows.

## Usage

``` r
export_palette(
  palette,
  format = c("hex", "css", "sass", "json", "csv"),
  names = NULL,
  file = NULL
)
```

## Arguments

- palette:

  A `huerd_palette` object or character vector of hex colors.

- format:

  Output format. One of:

  - `"hex"`: Character vector of hex colors (default)

  - `"css"`: CSS custom properties (variables)

  - `"sass"`: Sass/SCSS variables

  - `"json"`: JSON array

  - `"csv"`: CSV format with color names

- names:

  Optional character vector of names for the colors. If `NULL`, colors
  are named `color_1`, `color_2`, etc.

- file:

  Optional file path to write the output. If `NULL`, returns the
  formatted string.

## Value

If `file` is `NULL`, returns the formatted palette as a character string
(invisibly for `"hex"`). If `file` is specified

## Examples

``` r
pal <- generate_palette(5, progress = FALSE)

# Get as hex vector (default)
export_palette(pal)

# CSS custom properties
export_palette(pal, format = "css")
#> [1] ":root {\n  --color_1: #005600;\n  --color_2: #FF005C;\n  --color_3: #FF8800;\n  --color_4: #AAD6B8;\n  --color_5: #00FF00;\n}"

# With custom names
export_palette(pal, format = "css",
               names = c("primary", "secondary", "accent", "bg", "text"))
#> [1] ":root {\n  --primary: #005600;\n  --secondary: #FF005C;\n  --accent: #FF8800;\n  --bg: #AAD6B8;\n  --text: #00FF00;\n}"

# JSON for web applications
export_palette(pal, format = "json")
#> [1] "{\n    \"color_1\": \"#005600\",\n    \"color_2\": \"#FF005C\",\n    \"color_3\": \"#FF8800\",\n    \"color_4\": \"#AAD6B8\",\n    \"color_5\": \"#00FF00\"\n}"
```
