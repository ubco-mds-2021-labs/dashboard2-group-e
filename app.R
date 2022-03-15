library(dash)
library(dashBootstrapComponents)
library(plotly)
library(reshape2)
library(dplyr)
library(EBImage)
library(usmap)
library(plotly)

# Read data
df <- read.csv("data/Superstore.csv")

# Data Wrangling
df_plot1 <- df
df_plot4 <- df
df_plot6 <- df
colnames(df_plot1)[9] <- "SubCategory"
df_plot1 <- df_plot1 %>%
  group_by(Category, SubCategory) %>%
  summarise(Sales = sum(Sales), Profit = sum(Profit))
df_plot2 <- df |>
  group_by(Segment) |>
  summarize(Sales = sum(Sales), Quantity = sum(Quantity), Profit = sum(Profit))








wrangle_data <- function(unwrangled_data) {

  #' Wrangles data
  #'
  #' @description Takes an unwrangled data set and formats it in the appropriate way
  #'
  #' @param unwrangled_data A dataframe that needs to be wrangled according to specific requirements
  #'
  #' @return
  #'  A dataframe with a Profit_Margin column where the profit and sales are grouped over all cities. New columns are also
  #'  added for the abbreviation of the name of the state and its ID.



  # Removing outlier
  df <- unwrangled_data[unwrangled_data$Profit != -6599.9780, ]

  # To sum profit and sales over all cities, for each state
  df <- df %>%
    group_by(State, Category, Sub.Category, Ship.Mode, Segment) %>%
    summarise(Profit = sum(Profit), Sales = sum(Sales))

  df <- as.data.frame(df)

  # Adding column for profit margin
  df$Profit.Margin <- df$Profit / df$Sales

  # For rows that don't have sales (when calculating profit margin we get 0/0 = NaN --> replace with 0)
  df[is.na(df)] <- 0

  # Adding id (ansi code) to corresponding state in order to do chloropleth map
  # per https://gist.github.com/mbostock/4090848#gistcomment-2102151

  ansi <- read.csv("https://www2.census.gov/geo/docs/reference/state.txt", sep = "|")
  names(ansi) <- c("id", "abbr", "State", "statens")
  ansi <- ansi %>% select(id, abbr, State)

  # getting the id to match with the state from the original dataframe
  df <- left_join(df, ansi, by = "State")
  df
}

df_plot5 <- wrangle_data(df)

update_data <- function(ship_mode, segment, category, sub_category) {

  #' Updates data
  #'
  #' @description
  #' Appends lines for the states that have no sales for the specified combination of arguments (thus not present in the original dataframe).
  #' Their sales and profit are set to 0.
  #' @param ship_mode Mode of shipment (can be one of First Class, Second Class, Same Day or Standard Class).
  #' @param segment Component of a business (can be one of Consumer, Corporate or Home Office)
  #' @param category Category of product (can be one of Furniture, Office Supplies or Technology)
  #' @param sub_category Sub-Category of product (ex: sub-category of Furniture is Chairs)
  #'
  #' @return
  #' Returns a data frame with all the rows resulting from the selected parameters for all the States. The states that
  #' don't have sales for this combination of arguments have sales/profit/profit margin equal to 0

  all_states <- unique(df_plot5$State)

  # Data frame limited to what the user chooses with the drop down (excluding state parameter)
  selected_df <- df_plot5 %>% filter(Category == category, Sub.Category == sub_category, Ship.Mode == ship_mode, Segment == segment)

  # states that occur at least one for the chosen selection
  selected_states <- unique(selected_df$State)

  # list of states that are not in our selection (so no sales for this set of parameters)
  state_no_sales <- setdiff(all_states, selected_states)

  # Appending lines for the states that have no sales for that specific selection and putting their sales and profit = 0.
  # If we dont do this, we get a map with blank states. Instead we want the states to have an outline but be considered in the chloropleth as
  # not having sales

  new_lines <- data.frame(
    State = state_no_sales,
    Category = rep(category, length(state_no_sales)),
    Sub.Category = rep(sub_category, length(state_no_sales)),
    Ship.Mode = rep(ship_mode, length(state_no_sales)),
    Segment = rep(segment, length(state_no_sales)),
    Sales = rep(NA, length(state_no_sales)),
    Profit = rep(NA, length(state_no_sales))
  )


  updated_data <- rbind(select(df_plot5, -c(Profit.Margin, id, abbr)), new_lines) # remove profit margin, id and abbr so that same # of columns as new_lines

  # using wrangle_data() to add the profit margin, `id` and `abbr` column necessary for the map plot
  wrangle_data(updated_data)
}



# Create instance of app
app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

# Logo
img <- readImage("assets/logo_flipped.png")
logo <- plot_ly(type = "image", z = img * 255)

logo <- logo %>% layout(
  margin = list(l = 0, r = 0, b = 0, t = 0),
  width = 600, height = 200,
  xaxis = list(showticklabels = FALSE, ticks = ""),
  yaxis = list(showticklabels = FALSE, ticks = "")
)

logo <- logo %>%
  layout(plot_bgcolor = "#f9f8eb") %>%
  layout(paper_bgcolor = "#f9f8eb")

#########################################################
########## Layout with components of all plots###########
#########################################################


# Components for map plot

sales_card <- dbcCard(
  list(
    dbcCardHeader("Sales", class_name = "text-center"),
    dbcCardBody(
      list(
        h4(children = "", className = "text-center", id = "sales_card")
      )
    )
  )
)

profit_card <- dbcCard(
  list(
    dbcCardHeader("Profit", class_name = "text-center"),
    dbcCardBody(
      list(
        h4(children = "", className = "text-center", id = "profit_card")
      )
    )
  )
)

margin_card <- dbcCard(
  list(
    dbcCardHeader("Profit Margin", class_name = "text-center"),
    dbcCardBody(
      list(
        h4(children = "", className = "text-center", id = "margin_card")
      )
    )
  )
)

plot_map <- function(metric = "Sales", state = "Colorado", ship_mode = "First Class", segment = "Consumer", category = "Furniture", sub_category = "Bookcases") {

  #' Map of the US
  #'
  #' @param metric Variable according to which we wish to show the choropleth map (one of either Sales, Profit or Profit Margin)
  #' @param state Name of state within the United States (defaults to Colorado)
  #' @param ship_mode Mode of shipment (can be one of First Class, Second Class, Same Day or Standard Class). Defaults to "First Class".
  #' @param segment Component of a business (can be one of Consumer, Corporate or Home Office). Defaults to "Consumer".
  #' @param category Category of product (can be one of Furniture, Office Supplies or Technology). Defaults to "Furniture".
  #' @param sub_category Sub-Category of product (ex: sub-category of Furniture is Chairs). Defaults to "Bookcases".
  #'
  #' @return A map of the United States where each state is colored in proportion to the metric chosen.

  # Add lines for the states that have no sales for this combination of variables (so that they show up on map)
  updated_df <- update_data(ship_mode = ship_mode, segment = segment, category = category, sub_category = sub_category)

  # Data frame for plot_us() function (takes only 2 columns, 1-state 2-metric)
  map_data <- updated_df %>%
    filter(Ship.Mode == ship_mode, Segment == segment, Category == category, Sub.Category == sub_category) %>%
    mutate(Profit.Margin = Profit.Margin * 100) %>%
    select(State, metric) %>%
    rename(state = State)

  if (metric == "Profit.Margin") {
    legend_format <- scales::percent_format(scale = 1)
  } else {
    legend_format <- scales::dollar_format()
  }

  map <- plot_usmap(region = "states", data = map_data, values = metric, exclude = c("AK", "HI")) +
    scale_fill_gradient2(high = "dodgerblue4", na.value = "white", labels = legend_format) + theme_classic() + theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "right",
      legend.background = element_rect(fill = NA),
      panel.background = element_rect(fill = "#F9F8EB"),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#F9F8EB")
    )
  ggplotly(map)
}

state_dropdown <- dbcCol(
  dccDropdown(
    placeholder = "Select a state",
    id = "dropdown_state",
    value = "Colorado",
    options = unique(df_plot5$State) %>%
      purrr::map(function(state) list(label = state, value = state))
  )
)

mode_dropdown <- dbcCol(
  dccDropdown(
    placeholder = "Select a shipment mode",
    id = "dropdown_ship_mode",
    value = "First Class",
    options = unique(df_plot5$Ship.Mode) %>%
      purrr::map(function(ship) list(label = ship, value = ship))
  )
)

segment_dropdown <- dbcCol(
  dccDropdown(
    placeholder = "Select a segment",
    id = "dropdown_segment",
    value = "Consumer",
    options = unique(df_plot5$Segment) %>%
      purrr::map(function(seg) list(label = seg, value = seg))
  )
)

category_dropdown <- dbcCol(
  dccDropdown(
    placeholder = "Select a category",
    id = "dropdown_category",
    value = "Furniture",
    options = unique(df_plot5$Category) %>%
      purrr::map(function(seg) list(label = seg, value = seg))
  )
)

subcategory_dropdown <- dbcCol(
  dccDropdown(
    placeholder = "Select a sub-category",
    id = "dropdown_sub_category",
    value = "Bookcases",
    options = list()
  )
)

# total_map_container <-
#     dbcRow(
#       list(
#         dbcCol(sales_card, width = 4),
#         dbcCol(profit_card, width = 4),
#         dbcCol(margin_card, width = 4)
#       )
#     ),
#     dbcRow(
#       dccGraph(id = "map_plot")
#     ),
#     dbcRow(
#       dccRadioItems(
#         id = "radiobutton_map",
#         value = "Sales",
#         inputStyle = list("margin-right" = "5px", "margin-left"= "20px"),
#         options = list(
#           list(label = "Profit", value = "Profit"),
#           list(label = "Sales", value = "Sales"),
#           list(label = "Profit Margin", value = "Profit.Margin")
#         )
#       )
#     ),
#     dbcRow(
#       list(
#         state_dropdown,
#         mode_dropdown,
#         segment_dropdown,
#         category_dropdown,
#         subcategory_dropdown
#       )
#     )




app$layout(
  dbcContainer(
    dbcRow(list(
      dbcRow(list(
        dbcRow(dccGraph(id = "logo", figure = logo)),
        dbcCol(list(
          dccDropdown(
            placeholder = "Select a category",
            id = "category-dropdown",
            value = "Furniture",
            options = unique(df_plot1$Category) %>%
              purrr::map(function(cate) list(label = cate, value = cate)), style = list(
              "width" = "20", "font-weight" = "bold",
              "padding-left" = "400px"
            )
          ),
          dccGraph(id = "barchart")
        ), width = "4"),
        dbcCol(list(
          html$h4(id = "output-title4"),
          dccDropdown(
            placeholder = "Select a state",
            id = "states-dropdown",
            value = "New York",
            options = unique(df_plot4$State) %>%
              purrr::map(function(state) list(label = state, value = state))
          ),
          dccGraph(id = "bar-graph-top-items")
        ), width = "4"),
        dbcCol(list(
          html$h4(id = "output-title6"),
          dccDropdown(
            placeholder = "Select an item",
            id = "subcat-dropdown",
            value = "Binders",
            options = unique(df_plot6$Sub.Category) %>%
              purrr::map(function(subcat) list(label = subcat, value = subcat))
          ),
          dccGraph(id = "bar-graph-top-states")
        ), width = "4")
      )),
      dbcRow(list(
        dbcCol(list(
          dccGraph(id = "pie-graph-with-radio"),
          dccRadioItems(
            id = "metrics",
            value = "Sales",
            inputStyle = list("margin-right" = "5px", "margin-left" = "20px"),
            options = list(
              list(label = "Profit", value = "Profit"),
              list(label = "Sales", value = "Sales"),
              list(label = "Quantity", value = "Quantity")
            )
          )
        ), width = "4"),
        dbcCol(list(
          dbcRow(
            list(
              dbcCol(sales_card, width = 4),
              dbcCol(profit_card, width = 4),
              dbcCol(margin_card, width = 4)
            )
          ),
          dbcRow(
            dccGraph(id = "map_plot")
          ),
          dbcRow(
            dccRadioItems(
              id = "radiobutton_map",
              value = "Sales",
              inputStyle = list("margin-right" = "5px", "margin-left" = "20px"),
              options = list(
                list(label = "Profit", value = "Profit"),
                list(label = "Sales", value = "Sales"),
                list(label = "Profit Margin", value = "Profit.Margin")
              )
            )
          ),
          dbcRow(
            list(
              state_dropdown,
              mode_dropdown,
              segment_dropdown,
              category_dropdown,
              subcategory_dropdown
            )
          )
        ), width = "8")
      ))
    )),
    style = list("max-width" = "100%", "colour" = "#f9f8eb", backgroundColor = "#f9f8eb")
  )
)

#############################
####### Callbacks ###########
#############################

## Callback: Plot 1
app$callback(
  output(id = "barchart", property = "figure"),
  list(input(id = "category-dropdown", property = "value")),
  function(category) {
    sub_categories <- df_plot1$SubCategory[df_plot1$Category == category]
    sales <- df_plot1$Sales[df_plot1$Category == category]
    profit <- df_plot1$Profit[df_plot1$Category == category]
    df_new <- data.frame(sub_categories, sales, profit)
    df_new1 <- melt(df_new, id = "sub_categories")

    plot1 <- ggplot(df_new1, aes(sub_categories, value, fill = variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("midnightblue", "red")) +
      ggtitle("Overall Sales & Profit by Category") +
      scale_y_continuous(name = " ", labels = scales::comma) +
      labs(x = "Sub-categories", fill = " ") +
      theme(
        panel.background = element_rect(
          fill = "#f9f8eb", colour = "#f9f8eb",
          size = 2, linetype = "solid"
        ), plot.background = element_rect(fill = "#f9f8eb"),
        plot.title = element_text(face = "bold", size = (20)),
        legend.text = element_text(face = "bold", size = (15)),
        axis.title = element_text(size = (10), face = "bold"),
        axis.text = element_text(face = "bold", size = (12)),
        legend.background = element_rect(fill = "#f9f8eb")
      )


    plot1 <- ggplotly(plot1)
  }
)

## Callback: Plot 2
app$callback(
  list(
    output(id = "output-title4", property = "children"),
    output(id = "bar-graph-top-items", property = "figure")
  ),
  list(
    input(id = "states-dropdown", property = "value")
  ),
  function(selected_state) {
    filtered_df4 <-
      df_plot4 |>
      select(State, Sub.Category, Quantity) |>
      filter(State == selected_state) |>
      group_by(Sub.Category) |>
      summarise(Quantity = sum(Quantity)) |>
      arrange(-Quantity)
    filtered_df4 <- filtered_df4[1:5, ]

    plot4 <-
      ggplot(filtered_df4, aes(x = Sub.Category, y = Quantity)) +
      geom_bar(stat = "identity", fill = "midnightblue") +
      theme(panel.background = element_rect(
        fill = "#f9f8eb", colour = "#f9f8eb",
        size = 2, linetype = "solid"
      ), plot.background = element_rect(fill = "#f9f8eb"))

    ggplotly4 <- ggplotly(plot4)
    title4 <- paste0("Top 5 Items Sold in: ", selected_state)

    list(title4, ggplotly4)
  }
)

## Callback: Plot 3
app$callback(
  list(
    output(id = "output-title6", property = "children"),
    output(id = "bar-graph-top-states", property = "figure")
  ),
  list(
    input(id = "subcat-dropdown", property = "value")
  ),
  function(selected_item) {
    filtered_df6 <-
      df_plot6 |>
      select(State, Sub.Category, Quantity) |>
      filter(Sub.Category == selected_item) |>
      group_by(State) |>
      summarise(Quantity = sum(Quantity)) |>
      arrange(-Quantity)
    filtered_df6 <- filtered_df6[1:5, ]

    plot6 <-
      ggplot(filtered_df6, aes(x = State, y = Quantity)) +
      geom_bar(stat = "identity", fill = "midnightblue") +
      theme(panel.background = element_rect(
        fill = "#f9f8eb", colour = "#f9f8eb",
        size = 2, linetype = "solid"
      ), plot.background = element_rect(fill = "#f9f8eb"))

    ggplotly6 <- ggplotly(plot6)
    title6 <- paste0("Top 5 States With Highest Quantity Sold in: ", selected_item)

    list(title6, ggplotly6)
  }
)


## Callback: Plot 4
app$callback(
  output(id = "pie-graph-with-radio", property = "figure"),
  list(input(id = "metrics", property = "value")),
  function(selected_metrics) {
    to_plot <- df_plot2 |> select(Segment, selected_metrics)
    colnames(to_plot) <- c("Segment", "Metrics")

    colors <- c("red", "midnightblue", "lightgray")

    fig <- plot_ly(to_plot,
      labels = ~Segment,
      values = ~Metrics,
      type = "pie",
      marker = list(colors = colors)
    )
    fig <- fig %>% layout(
      title = "Metrics Proportion by Segment",
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
    )

    fig <- fig %>%
      layout(plot_bgcolor = "#f9f8eb") %>%
      layout(paper_bgcolor = "#f9f8eb")


    fig
  }
)





all_category <- list(
  "Furniture" = list("Bookcases", "Chairs", "Furnishings", "Tables"),
  "Office Supplies" = list(
    "Appliances",
    "Art",
    "Binders",
    "Envelopes",
    "Fasteners",
    "Labels",
    "Paper",
    "Storage",
    "Supplies"
  ),
  "Technology" = list("Accesories", "Copiers", "Machines", "Phones")
)




# Updates cards

app$callback(
  output("dropdown_sub_category", "options"),
  list(input("dropdown_category", "value")),
  function(selected_category) {
    unlist(all_category[[selected_category]]) %>%
      purrr::map(function(seg) list(label = seg, value = seg))
  }
)

app$callback(
  list(
    output("sales_card", "children"),
    output("profit_card", "children"),
    output("margin_card", "children")
  ),
  list(
    input("dropdown_ship_mode", "value"),
    input("dropdown_segment", "value"),
    input("dropdown_state", "value"),
    input("dropdown_category", "value"),
    input("dropdown_sub_category", "value")
  ),
  function(ship_mode, segment, state, category, sub_category) {
    updated_df <- df_plot5 %>%
      filter(Ship.Mode == ship_mode, Segment == segment, Category == category, Sub.Category == sub_category, State == state) %>%
      select(Sales, Profit, Profit.Margin)


    sales <- updated_df$Sales
    profit <- updated_df$Profit
    margin <- (updated_df$Profit.Margin) * 100

    if (length(sales) != 0) {
      sales_formatted <- paste("$", round(sales, 2))
      profit_formatted <- paste("$", round(profit, 2))
      margin_formatted <- paste(round(margin, 2), "%")
    } else {
      sales_formatted <- "-"
      profit_formatted <- "-"
      margin_formatted <- "-"
    }
    list(sales_formatted, profit_formatted, margin_formatted)
  }
)


app$callback(
  output("map_plot", "figure"),
  list(
    input("radiobutton_map", "value"),
    input("dropdown_state", "value"),
    input("dropdown_ship_mode", "value"),
    input("dropdown_segment", "value"),
    input("dropdown_category", "value"),
    input("dropdown_sub_category", "value")
  ),
  function(metric, state, ship_mode, segment, category, sub_category) {
    plot_map(metric, state, ship_mode, segment, category, sub_category)
  }
)




# Run server
app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050))
