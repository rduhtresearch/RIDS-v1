DEV_BANNER_TEXT <- paste(
  "This is a development build —",
  "Features may be incomplete and subject to change",
  "Please report any issues."
  

)

dev_banner <- function() {
  div(
    class = "dev-banner",
    style = paste(
      "background: #fdf6e3;",
      "color: #5c4a1a;",
      "border-left: 4px solid #d49434;",
      "border-radius: 0 4px 4px 0;",
      "padding: 0.5rem 1rem;",
      "font-size: 0.82rem;",
      "letter-spacing: 0.01em;",
      "max-width: 720px;",
      "margin: 0.75rem auto;",
      "z-index: 1050;"
    ),
    DEV_BANNER_TEXT
  )
}