# load page templates
source("pages/r-templates/overview.r")
source("pages/r-templates/city.r")
source("pages/r-templates/state.r")

function(input, output, session) {

  # current page
  output$content = renderUI(
    {
      switch(input$nav,
        "overview" = overview_template,
        "city" = city_template,
        "state" = state_template,
        overview_template
      )
    }
  )
  
  # data filtered by user input
  filtered_dat = reactive(
    {
      validate(
        need(input$title, ""),
        need(input$education, ""),
        need(input$gender, ""),
        need(input$race, "")
      )

      dat %>%
        filter(between(date, input$dateRange[1], input$dateRange[2])) %>%
        filter(title %in% input$title) %>%
        filter(education_level %in% input$education) %>%
        filter(gender %in% input$gender) %>%
        filter(race %in% input$race)
    }
  )

  # summary data for cities
  summarized_dat_city = reactive(
    {
      filtered_dat() %>%
        group_by(city, state) %>%
        mutate(
          avg_comp = round(mean(total_annual_compensation)),
          med_comp = round(median(total_annual_compensation)),
          jobs_count = n()
        ) %>%
        select(city, state, avg_comp, med_comp, jobs_count, lat, lng) %>%
        distinct(city, state, .keep_all = TRUE) %>%
        ungroup()
    }
  )

  # summary data for states
  summarized_dat_state = reactive(
    {
      temp = filtered_dat() %>%
        group_by(state) %>%
        mutate(
          avg_comp = round(mean(total_annual_compensation)),
          med_comp = round(median(total_annual_compensation)),
          jobs_count = n()
        ) %>%
        select(state, avg_comp, med_comp, jobs_count) %>%
        distinct(state, .keep_all = TRUE) %>%
        ungroup() %>%
        filter(!is.na(jobs_count))
    }
  )

  # summary data for whole data set (entire US)
  summarized_dat_national = reactive(
    {
      filtered_dat() %>%
        summarize(
          med_comp = median(total_annual_compensation),
          avg_comp = mean(total_annual_compensation)
        )
    }
  )

  selected_city_dat = reactive(
    {
      req(selected_city())
      req(selected_city_state())

      filtered_dat() %>%
        filter(state %in% selected_city_state()) %>%
        filter(city %in% selected_city())
    }
  )

  selected_city_dat_summary = reactive(
    {
      selected_city_dat() %>%
        mutate(
          avg_comp = round(mean(total_annual_compensation)),
          med_comp = round(median(total_annual_compensation)),
          jobs_count = n()
        ) %>%
        slice(1) %>%
        pivot_longer(16:32, names_to = "category", values_to = "score") %>%
        select(city, state, avg_comp, med_comp, jobs_count, category, score, lat, lng)
    }
  )

  selected_state_dat = reactive(
    {
      req(selected_state())

      filtered_dat() %>%
        filter(state %in% selected_state())
    }
  )

  selected_state_dat_summary = reactive(
    {
      selected_state_dat() %>%
        mutate(
          avg_comp = round(mean(total_annual_compensation)),
          med_comp = round(median(total_annual_compensation)),
          jobs_count = n()
        ) %>%
        slice(1) %>%
        pivot_longer(16:32, names_to = "category", values_to = "score") %>%
        select(state, avg_comp, med_comp, jobs_count)
    }
  )

  output$state_map = renderLeaflet({

    validate(
      need(input$title, "Please select a title"),
      need(input$education, "Please select an education level"),
      need(input$gender, "Please select a gender"),
      need(input$race, "Please select a race")
    )

    require(summarized_dat_state())

    mdat = merge(states, summarized_dat_state(), by.x = 'STUSPS', by.y = 'state', all.x = T) %>%
        rename(state = STUSPS) %>%
        filter(!is.na(jobs_count))
    
    pal = colorNumeric(
      palette = "Greens",
      domain = mdat$med_comp,
      na.color = "grey"
      )

    map = leaflet(mdat) %>%
      addTiles() %>%
      setView(-98.5795, 39.8282, zoom = 3) %>%
      addPolygons(fillColor = ~pal(mdat$med_comp),
        layerId = ~mdat$state,
        weight = 0.6,
        opacity = 1,
        color = "black",
        fillOpacity = 0.8,
        smoothFactor = 0.2,
        label = lapply(paste0(
          "<b>", mdat$NAME, "</b><br>",
          "Median Compensation: ", scales::label_number_si(accuracy = 1)(mdat$med_comp), "<br>",
          "Average Compensation: ", scales::label_number_si(accuracy = 1)(mdat$avg_comp), "<br>",
          "Reported Jobs: ", scales::label_number_si(accuracy = 1)(mdat$jobs_count), "<br>"),
          HTML)
      )

    css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
    html_fix <- htmltools::tags$style(type = "text/css", css_fix) # Convert CSS to HTML

    map %>%
      addLegend(position = "bottomleft", pal = pal, values = mdat$med_comp,
                title = "Median Compensation", opacity = 1) %>%
      htmlwidgets::prependContent(html_fix)
  })

  output$city_map = renderLeaflet({

    validate(
      need(input$title, "Please select a title"),
      need(input$education, "Please select an education level"),
      need(input$gender, "Please select a gender"),
      need(input$race, "Please select a race")
    )

    mdat = summarized_dat_city()
    
    pal = colorNumeric(
      palette = c("#fcde9c","#faa476","#f0746e","#e34f6f","#dc3977","#b9257a","#7c1d6f"),
      domain = mdat$med_comp,
      na.color = "grey"
      )

    map = leaflet(mdat) %>%
      addTiles() %>%
      setView(-98.5795, 39.8282, zoom = 3) %>%
        addCircleMarkers(lat = ~lat, lng = ~lng,
          color = ~pal(med_comp),
          radius = ~rescale(jobs_count, c(8, 24)),
          stroke = T,
          fillOpacity = 0.7,
          weight = 1,
          label = lapply(paste0(
          "<b>", mdat$city, ", ", mdat$state, "</b><br>",
          "Median Compensation: ", scales::label_number_si(accuracy = 1)(mdat$med_comp), "<br>",
          "Average Compensation: ", scales::label_number_si(accuracy = 1)(mdat$avg_comp), "<br>",
          "Reported Jobs: ", scales::label_number_si(accuracy = 1)(mdat$jobs_count), "<br>"),
          HTML))

    css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
    html_fix <- htmltools::tags$style(type = "text/css", css_fix) # Convert CSS to HTML

    map %>%
      addLegend(position = "bottomleft", pal = pal, values = mdat$med_comp,
                title = "Median Compensation", opacity = 1) %>%
      htmlwidgets::prependContent(html_fix)
  })



  output$avg_comp = reactive({
    paste0("$", filtered_dat() %>%
      summarize(avg_comp = mean(total_annual_compensation)) %>%
      slice_max(avg_comp, n = 1) %>%
      as.numeric() %>%
      abreviate_num()
      )
  })

  output$med_comp = reactive({
    paste0("$", filtered_dat() %>%
      summarize(med_comp = median(total_annual_compensation)) %>%
      slice_max(med_comp, n = 1) %>%
      as.numeric() %>%
      abreviate_num()
      )
  })

  output$num_jobs = reactive({
    nrow(filtered_dat()) %>%
    abreviate_num()
  })

  output$city_str = reactive({
    validate(
      need(selected_city(), "City Details"),
      need(selected_city_state(), "City Details")
    )
    paste0(selected_city(), ", ", selected_city_state())
  })

  output$state_str = reactive({
    validate(
      need(selected_state(), "State Details")
    )

      usdata::abbr2state(selected_state())
  })

  output$city_avg_comp = reactive({
    validate(
      need(selected_city(), "0")
    )

    paste0("$", selected_city_dat() %>%
      summarize(avg_comp = mean(total_annual_compensation)) %>%
      slice_max(avg_comp, n = 1) %>%
      as.numeric() %>%
      abreviate_num()
      )
  })

  output$city_med_comp = reactive({
    validate(
      need(selected_city(), "0")
    )

    paste0("$", selected_city_dat() %>%
      summarize(med_comp = median(total_annual_compensation)) %>%
      slice_max(med_comp, n = 1) %>%
      as.numeric() %>%
      abreviate_num()
      )
  })

  output$city_num_jobs = reactive({
    validate(
      need(selected_city(), "0")
    )

    nrow(selected_city_dat()) %>%
    abreviate_num()
  })

  output$state_med_comp = reactive({
    validate(
      need(selected_state(), "0")
    )

    paste0("$", selected_state_dat() %>%
      summarize(med_comp = median(total_annual_compensation)) %>%
      slice_max(med_comp, n = 1) %>%
      as.numeric() %>%
      abreviate_num()
      )
  })

  output$state_num_jobs = reactive({
    validate(
      need(selected_state(), "0")
    )

    nrow(selected_state_dat()) %>%
    abreviate_num()
  })

  output$state_avg_comp = reactive({
    validate(
      need(selected_state(), "0")
    )

    paste0("$", selected_state_dat() %>%
      summarize(avg_comp = mean(total_annual_compensation)) %>%
      slice_max(avg_comp, n = 1) %>%
      as.numeric() %>%
      abreviate_num()
      )
  })

  time_dat = reactive({
    filtered_dat() %>%
      mutate(month = month(date), year = year(date)) %>%
      group_by(year, month) %>%
      summarise(
        avg_comp = round(mean(total_annual_compensation), -3),
        med_comp = round(median(total_annual_compensation), -3),
        jobs_count = n()
      ) %>%
      ungroup() %>%
      mutate(date = ymd(paste(year, month, "1"))) %>%
      select(date, avg_comp, med_comp, jobs_count)
  })

  city_time_dat = reactive({
    selected_city_dat() %>%
      mutate(month = month(date), year = year(date)) %>%
      group_by(year, month) %>%
      summarise(
        avg_comp = round(mean(total_annual_compensation), -3),
        med_comp = round(median(total_annual_compensation), -3),
        jobs_count = n()
      ) %>%
      ungroup() %>%
      mutate(date = ymd(paste(year, month, "1"))) %>%
      select(date, avg_comp, med_comp, jobs_count)
  })

   state_time_dat = reactive({
    selected_state_dat() %>%
      mutate(month = month(date), year = year(date)) %>%
      group_by(year, month) %>%
      summarise(
        avg_comp = round(mean(total_annual_compensation), -3),
        med_comp = round(median(total_annual_compensation), -3),
        jobs_count = n()
      ) %>%
      ungroup() %>%
      mutate(date = ymd(paste(year, month, "1"))) %>%
      select(date, avg_comp, med_comp, jobs_count)
  })

  output$time_series = renderPlotly({

    low = min(min(time_dat()$avg_comp), min(time_dat()$med_comp))
    high = max(max(time_dat()$avg_comp), max(time_dat()$med_comp))
    range = high - low

    plot_ly(time_dat()) %>%
      add_trace(
        type = 'scatter',
        mode = 'lines+markers',
        x = ~date,
        y = ~avg_comp,
        name = "Average"
      ) %>%
      add_trace(
        type = 'scatter',
        mode = 'lines+markers',
        x = ~date,
        y = ~med_comp,
        name = "Median"
      ) %>%
      add_trace(
        type = 'bar',
        x = ~date,
        y = ~jobs_count,
        yaxis = "y2",
        name = "Reported Jobs"
      ) %>%
      layout(showlegend = T,
        legend = list(orientation ="h", x = 0.55, y = 1),
        title = '',
        margin = list(l = 20, r = 50),
        xaxis = list(title = "",
          rangeslider = list(visible = T),
          rangeselector = list(
            buttons = list(
              list(count = 1, label = "1m", step = "month", stepmode = "backward"),
              list(count = 6, label = "6m", step = "month", stepmode = "backward"),
              list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
              list(count = 1, label = "1y", step = "year", stepmode = "backward"),
              list(step = "all")
              )
            )
          ),
        yaxis = list(
          title = "Compensation ($)",
          range= c(max(0, low - range / 2), high + range / 5)
        ),
        yaxis2 = list(
          tickfont = list(color = "green"),
          overlaying = "y",
          side = "right",
          range = c(0, max(time_dat()$jobs_count)*5)
        )
        ) %>%
      layout(
        xaxis = list(zerolinecolor = '#ffff',
                    zerolinewidth = 2,
                    gridcolor = 'ffff'),
        yaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displaylogo = FALSE)
  })

  output$city_time_series = renderPlotly({

    low = min(min(city_time_dat()$avg_comp), min(city_time_dat()$med_comp))
    high = max(max(city_time_dat()$avg_comp), max(city_time_dat()$med_comp))
    range = high - low

    plot_ly(city_time_dat()) %>%
      add_trace(
        type = 'scatter',
        mode = 'lines+markers',
        x = ~date,
        y = ~avg_comp,
        name = "Average"
      ) %>%
      add_trace(
        type = 'scatter',
        mode = 'lines+markers',
        x = ~date,
        y = ~med_comp,
        name = "Median"
      ) %>%
      add_trace(
        type = 'bar',
        x = ~date,
        y = ~jobs_count,
        yaxis = "y2",
        name = "Reported Jobs"
      ) %>%
      layout(showlegend = T,
        legend = list(orientation ="h", x = 0.55, y = 1),
        title = '',
        margin = list(l = 20, r = 50),
        xaxis = list(title = "",
          rangeslider = list(visible = T),
          rangeselector = list(
            buttons = list(
              list(count = 1, label = "1m", step = "month", stepmode = "backward"),
              list(count = 6, label = "6m", step = "month", stepmode = "backward"),
              list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
              list(count = 1, label = "1y", step = "year", stepmode = "backward"),
              list(step = "all")
              )
            )
          ),
        yaxis = list(
          title = "Compensation ($)",
          range = c(max(0, low - range / 2), high + range / 5)
        ),
        yaxis2 = list(
          tickfont = list(color = "green"),
          overlaying = "y",
          side = "right",
          range = c(0, max(city_time_dat()$jobs_count)*5)
        )
        ) %>%
      layout(
        xaxis = list(zerolinecolor = '#ffff',
                    zerolinewidth = 2,
                    gridcolor = 'ffff'),
        yaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displaylogo = FALSE)
  })

  output$state_time_series = renderPlotly({

    low = min(min(state_time_dat()$avg_comp), min(state_time_dat()$med_comp))
    high = max(max(state_time_dat()$avg_comp), max(state_time_dat()$med_comp))
    range = high - low

    plot_ly(state_time_dat()) %>%
      add_trace(
        type = 'scatter',
        mode = 'lines+markers',
        x = ~date,
        y = ~avg_comp,
        name = "Average"
      ) %>%
      add_trace(
        type = 'scatter',
        mode = 'lines+markers',
        x = ~date,
        y = ~med_comp,
        name = "Median"
      ) %>%
      add_trace(
        type = 'bar',
        x = ~date,
        y = ~jobs_count,
        yaxis = "y2",
        name = "Reported Jobs"
      ) %>%
      layout(showlegend = T,
        legend = list(orientation ="h", x = 0.55, y = 1),
        title = '',
        margin = list(l = 20, r = 50),
        xaxis = list(title = "",
          rangeslider = list(visible = T),
          rangeselector = list(
            buttons = list(
              list(count = 1, label = "1m", step = "month", stepmode = "backward"),
              list(count = 6, label = "6m", step = "month", stepmode = "backward"),
              list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
              list(count = 1, label = "1y", step = "year", stepmode = "backward"),
              list(step = "all")
              )
            )
          ),
        yaxis = list(
          title = "Compensation ($)",
          range = c(max(0, low - range / 2), high + range / 5)
        ),
        yaxis2 = list(
          tickfont = list(color = "green"),
          overlaying = "y",
          side = "right",
          range = c(0, max(state_time_dat()$jobs_count)*5)
        )
        ) %>%
      layout(
        xaxis = list(zerolinecolor = '#ffff',
                    zerolinewidth = 2,
                    gridcolor = 'ffff'),
        yaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displaylogo = FALSE)
  })

  output$city_table = renderDataTable(
    {
      summarized_dat_city() %>%
        mutate(city = paste0(city, ", ", state)) %>%
        select(city, med_comp, avg_comp, jobs_count) %>%
        arrange(desc(med_comp)) %>%
        datatable(options = list(
          dom = "t",
          lengthChange = FALSE,
          rownames = FALSE,
          bPaginate = FALSE,
          bInfo = FALSE,
          scrollY = '380px'
          ),
          selection = "single",
          filter = list(position = "top"),
          rownames = FALSE,
          colnames = c("City", "Median Compensation ($)", "Average Compensation ($)", "Reported Jobs")
        ) %>%
        formatRound('med_comp', digits = 0) %>%
        formatRound('avg_comp', digits = 0) %>%
        formatRound('jobs_count', digits = 0)
    }
  )

  output$state_table = renderDataTable(
    {
      summarized_dat_state() %>%
        select(state, med_comp, avg_comp, jobs_count) %>%
        mutate(state = usdata::abbr2state(state)) %>%
        arrange(desc(med_comp)) %>%
        datatable(options = list(
          dom = "t",
          lengthChange = FALSE,
          rownames = FALSE,
          bPaginate = FALSE,
          bInfo = FALSE,
          scrollY = '380px'
          ),
          selection = "single",
          filter = list(position = "top"),
          rownames = FALSE,
          colnames = c("State", "Median Compensation ($)", "Average Compensation ($)", "Reported Jobs")
        ) %>%
        formatRound('med_comp', digits = 0) %>%
        formatRound('avg_comp', digits = 0) %>%
        formatRound('jobs_count', digits = 0)
    }
  )

  output$teleport = renderPlotly(
    {
      plot_ly(selected_city_dat_summary()) %>%
      add_trace(
        type = "bar",
        x = 10,
        y = ~category,
        color = I("lightgrey"),
        hoverinfo = 'skip'
      ) %>%
      add_trace(
        type = "bar",
        x = ~replace(round(score, 2), is.na(score), 0),
        y = ~category,
        yaxis = "y2",
        name = ""
      ) %>%
      layout(title = '',
        bargap = .5,
        showlegend = FALSE,
        dragmode = FALSE,
        margin = list(pad = 20),
        xaxis = list(
          title = "",
          range = c(0, 10),
          visible = FALSE
          ),
        yaxis = list(
          title = "",
          margin = list(r=10)
        ),
        yaxis2 = list(
          overlaying = "y"
        ),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
    }
  )

  selected_city = reactiveVal("Omaha")
  selected_city_state = reactiveVal("NE")
  selected_state = reactiveVal("NE")

  observeEvent(input$nav, {
    
  })

  observeEvent(input$city_map_marker_click, {
    click = input$city_map_marker_click
    print(click)
    if (!is.null(click)) {
      selection = filtered_dat() %>%
        filter(lat == click$lat, lng == click$lng)

      selected_city(selection %>% pull(city) %>% first())
      selected_city_state(selection %>% pull(state) %>% first())
    }
  })

  observeEvent(input$city_table_rows_selected, {
    click = input$city_table_rows_selected
    if (!is.null(click)) {
      selection = summarized_dat_city() %>%
        arrange(desc(med_comp)) %>%
        slice(click)

      selected_city(selection$city)
      selected_city_state(selection$state)
    }
  })

  observeEvent(input$state_map_shape_click, {
    click = input$state_map_shape_click
    print(click)
    if (!is.null(click)) {
      selection = filtered_dat() %>%
        filter(state == click$id)

      selected_state(selection %>% pull(state) %>% first())
    }
  })

  observeEvent(input$state_table_rows_selected, {
    click = input$state_table_rows_selected
    if (!is.null(click)) {
      selection = summarized_dat_state() %>%
        arrange(desc(med_comp)) %>%
        slice(click)

      selected_state(selection$state)
    }
  })

  observe({

    ## Change label if all options selected ##
    # Title
    if (length(input$title) == length(unique(dat$title))) {
      session$sendCustomMessage(type = "controlLabel", message = list(id = "title", label = "All"))
    }

    # Education
    if (length(input$education) == length(unique(dat$education_level))) {
      session$sendCustomMessage(type = "controlLabel", message = list(id = "education", label = "All"))
    }

    # Gender
    if (length(input$gender) == length(unique(dat$gender))) {
      session$sendCustomMessage(type = "controlLabel", message = list(id = "gender", label = "All"))
    }

    # Race
    if (length(input$race) == length(unique(dat$race))) {
      session$sendCustomMessage(type = "controlLabel", message = list(id = "race", label = "All"))
    }
  })
}