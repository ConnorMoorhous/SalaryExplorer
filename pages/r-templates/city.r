city_template = htmlTemplate("pages/city.html",
    map = leafletOutput("city_map", width = "100%", height = "100%"),
    location = uiOutput("city_str"),
    avg_comp = uiOutput("city_avg_comp"),
    med_comp = uiOutput("city_med_comp"),
    num_jobs = uiOutput("city_num_jobs"),
    time_series = plotlyOutput("city_time_series", width = "100%", height = "100%"),
    table = DTOutput("city_table", width = "100%", height = "100%"),
    teleport = plotlyOutput("teleport", width = "600px", height = "700px")
)