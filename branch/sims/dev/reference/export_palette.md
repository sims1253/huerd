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

  - `"json"`: JSON object keyed by color names

  - `"csv"`: CSV format with color names

- names:

  Optional character vector of names for the colors. If `NULL`, colors
  are named `color_1`, `color_2`, etc.

- file:

  Optional file path to write the output. If `NULL`, returns the
  formatted string. If specified, the function writes the formatted
  palette to the given file path and (invisibly) returns the file path
  as a character string.

## Value

If `file` is `NULL`: for `"hex"`, the palette as a character vector
(returned invisibly); for all other formats, the formatted palette as a
single character string. If `file` is specified, the function writes the
formatted palette to the given file and (invisibly) returns the file
path as a character string.

## Examples

``` r
pal <- generate_palette(5, progress = FALSE)

# Get as hex vector (default)
export_palette(pal)

# CSS custom properties
export_palette(pal, format = "css")
#> [1] ":root {\n  --color_1: #3400D8;\n  --color_2: #FF0000;\n  --color_3: #CB918B;\n  --color_4: #00D800;\n  --color_5: #FFF500;\n}"

# With custom names
export_palette(pal, format = "css",
               names = c("primary", "secondary", "accent", "bg", "text"))
#> [1] ":root {\n  --primary: #3400D8;\n  --secondary: #FF0000;\n  --accent: #CB918B;\n  --bg: #00D800;\n  --text: #FFF500;\n}"

# JSON for web applications
export_palette(pal, format = "json")
#> [1] "{\n    \"color_1\": \"#3400D8\",\n    \"color_2\": \"#FF0000\",\n    \"color_3\": \"#CB918B\",\n    \"color_4\": \"#00D800\",\n    \"color_5\": \"#FFF500\"\n}"
```
