overview_template = htmlTemplate("pages/overview.html",
    avg_comp = uiOutput("avg_comp"),
    med_comp = uiOutput("med_comp"),
    num_jobs = uiOutput("num_jobs"),
    time_series = plotlyOutput("time_series", width = "100%", height = "100%")
)