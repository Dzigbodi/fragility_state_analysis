
ui <- page_navbar(
  title = tags$a(
    href = "#",
    class = "navbar-brand d-flex align-items-center",
    tags$img(
      src = "fsd.png",
      class = "brand-logo",
      style = "margin-right:10px;"
    )
  ),
  window_title = "Fragility Exits",
  theme = my_theme,
  
  navbar_options = navbar_options(
    collapsible = TRUE
  ),
  
  nav_panel(
    title = "Fragility Exits",
    uiOutput("fragility_exits_ui")
  ),
  nav_panel(
    title = "Help",
    uiOutput("help_ui")
  )
)

