## ── App Theme ────────────────────────────────────────────────────────────────
## Bootstrap 5 theme with semantic colors.

library(bslib)

app_theme <- bs_theme(

  version = 5,
  bootswatch = "flatly",
  primary = "#2c3e50",
  success = "#27ae60",
  warning = "#f39c12",
  danger = "#e74c3c",
  info = "#3498db",
  "font-size-base" = "0.9rem"
)
