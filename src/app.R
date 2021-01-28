library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(tidyverse)
library(plotly)
library(here)

data_raw <- read_csv(here('data', 'processed.csv')) %>%
  mutate(work_interfere = factor(work_interfere, levels = c('Often', 'Sometimes', 'Rarely', 'Never')))

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

plots_general_overview <- dbcRow(list(
  dbcCol(list(
    dbcRow(dccGraph(id = 'plot-bar1')),
    dbcRow(dccGraph(id = 'plot-bar2'))
  ), md = 4),
  dbcCol(dccGraph(id = 'plot-map'))
))

tab_general_overview_content <- dbcCard(
  dbcCardBody(list(
    dbcRow(list(
      dbcCol(list(
        htmlH5('State:'),
        dccDropdown(
          id = 'overview-state-dropdown',
          value = list(), multi = T,
          options = map(sort(unique(data_raw$state)), ~list(label = .x, value = .x))
        ),
        htmlH5('Company Size:'),
        dccDropdown(
          id = 'overview-company-size-dropdown',
          value = list(), multi = T,
          options = map(sort(unique(data_raw$no_employees)), ~list(label = .x, value = .x))
        ),
        htmlH5('Gender:'),
        dccDropdown(
          id = 'overview-gender-dropdown',
          value = list(), multi = T,
          options = map(sort(unique(data_raw$Gender)), ~list(label = .x, value = .x))
        ),
        htmlH5('Age:'),
        dccRangeSlider(
          id = 'overview-age-slider',
          min = min(data_raw$Age), max = max(data_raw$Age), step = 1,
          tooltip = list(placement = 'bottomLeft'),
          value = list(min(data_raw$Age), max(data_raw$Age))
        ),
        htmlDiv(id = 'overview-age-range-text')
      ), md = 2),
      dbcCol(dccLoading(
        id = 'overview-loading',
        type = 'default',
        children = plots_general_overview
      ))
    ))
  ))
)

app$layout(dbcContainer(list(
  dbcRow(htmlH2('Mental Health in Tech Dashboard'), justify = 'center'),
  dbcRow(list(
    dbcCol(list(
      dbcTabs(
        list(
          dbcTab(tab_general_overview_content,
                 label = 'General Overview',
                 tab_id = 'tab-general-overview'),
          dbcTab(label = 'Company Support',
                 tab_id = 'tab-company-support')
        ),
        id = 'main-tabs',
        active_tab = 'tab-general-overview'
      )
    ))
  ))
), fluid = T, style = list('border-width' = '10')))


app$callback(
  list(
    output('plot-bar1', 'figure'),
    output('plot-bar2', 'figure'),
    output('plot-map', 'figure')
  ),
  list(
    input('overview-state-dropdown', 'value'),
    input('overview-company-size-dropdown', 'value'),
    input('overview-gender-dropdown', 'value'),
    input('overview-age-slider', 'value')
  ),
  function(states, company_sizes, genders, age_range) {
    data_filtered <- data_raw

    if (length(states) > 0) {
      data_filtered <- data_filtered %>%
        filter(state %in% states)
    }

    if (length(company_sizes) > 0) {
      data_filtered <- data_filtered %>%
        filter(no_employees %in% company_sizes)
    }

    if (length(genders) > 0) {
      data_filtered <- data_filtered %>%
        filter(Gender %in% genders)
    }

    if (length(age_range) > 0) {
      data_filtered <- data_filtered %>%
        filter(Age >= age_range[1] & Age <= age_range[2])
    }

    p_wi <- data_filtered %>% ggplot(aes(y = work_interfere)) +
      geom_bar(stat = 'count') +
      labs(x = 'Count of Records', y = 'Work Interfere',
           title = 'Work interference')

    p_wp <- data_filtered %>%
      add_count(wellness_program) %>%
      ggplot(aes(y = reorder(wellness_program, -n))) +
      geom_bar() +
      ylab('') +
      ggtitle('Has your employer ever discussed mental health as part of an employee wellness program?') +
      theme(
        text = element_text(size=10)
      )


    data_grouped <- data_filtered %>%
      group_by(state) %>%
      tally()
    p_map <- plot_ly(data_grouped, type = 'choropleth', locationmode = 'USA-states', locations = ~state, z = ~n) %>%
      layout(geo = list(scope = 'usa', projection = list(type = 'albers usa')),
             title = 'Count of records by state')

    list(ggplotly(p_wi),
         ggplotly(p_wp),
         subplot(p_map, ggplotly(p_wi)))
  }
)


app$callback(
  list(output('overview-age-range-text', 'children')),
  list(input('overview-age-slider', 'value')),
  function(age_range) {
    list(str_interp('Selected age range: ${age_range[1]} - ${age_range[2]}'))
  })

app$run_server(host = '0.0.0.0', debug = F)
