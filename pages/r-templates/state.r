state_template = htmlTemplate("pages/state.html",
    map = leafletOutput("state_map", width = "100%", height = "100%"),
    location = uiOutput("state_str"),
    avg_comp = uiOutput("state_avg_comp"),
    med_comp = uiOutput("state_med_comp"),
    num_jobs = uiOutput("state_num_jobs"),
    time_series = plotlyOutput("state_time_series", width = "100%", height = "100%"),
    table = DTOutput("state_table", width = "100%", height = "100%")
)