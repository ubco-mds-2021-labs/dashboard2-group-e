library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(plotly)
library(reshape2)
library(dplyr)
library(EBImage)

# Read data
df <- read.csv("data/Superstore.csv")

# Data Wrangling
df_plot1 <- df
df_plot4 <- df
df_plot6 <- df
colnames(df_plot1)[9] <- "SubCategory"
df_plot1 <- df_plot1 %>% group_by(Category, SubCategory) %>% summarise(Sales=sum(Sales), Profit=sum(Profit))
df_plot2 <- df |> group_by(Segment) |> summarize(Sales=sum(Sales), Quantity=sum(Quantity), Profit=sum(Profit))

# Create instance of app 
app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

# Logo
img <- readImage('assets/logo_flipped.png')
logo <- plot_ly(type="image", z=img*255)

logo <- logo %>% layout(margin=list(l=0, r=0, b=0, t=0),
                        width = 600, height = 200,
                        xaxis=list(showticklabels=FALSE, ticks=""),
                        yaxis=list(showticklabels=FALSE, ticks=""))

logo <- logo %>% layout(plot_bgcolor='#f9f8eb') %>% 
  layout(paper_bgcolor='#f9f8eb')

#########################################################
########## Layout with components of all plots###########
#########################################################


app$layout(
  dbcContainer( 
    dbcRow(list(
      
      dbcRow(list(
        dbcRow(dccGraph(id='logo', figure=logo)),
        dbcCol(list(
          dccDropdown(
            placeholder = "Select a category",
            id = "category-dropdown",
            value = "Furniture", 
            options = unique(df_plot1$Category) %>%
              purrr::map(function(cate) list(label = cate, value = cate)), style=list('width'='20', 'font-weight'='bold',
                                                                                      'padding-left'='400px')),
          dccGraph(id='barchart')
        ), width='4'
        ),
        dbcCol(list(
          html$h4(id="output-title4"),
          dccDropdown(
            placeholder = "Select a state",
            id = "states-dropdown",
            value = "New York",
            options = unique(df_plot4$State) %>%
              purrr::map(function(state) list(label = state, value = state))),
          dccGraph(id='bar-graph-top-items')
        ), width='4'
        ),
        dbcCol(list(
          html$h4(id="output-title6"),
          dccDropdown(
            placeholder = "Select an item",
            id = "subcat-dropdown",
            value = "Binders",
            options = unique(df_plot6$Sub.Category) %>%
              purrr::map(function(subcat) list(label = subcat, value = subcat))),
          dccGraph(id='bar-graph-top-states')
        ), width='4'
        )
      )
      ),
      
      dbcRow(list(
        dbcCol(list(
          dbcRow(list(
            dccGraph(id='pie-graph-with-radio'),
            dccRadioItems(
              id = "metrics",
              value = "Sales",
              inputStyle = list("margin-right" = "5px", "margin-left"= "20px"),
              options = list(
                list(label = "Profit", value = "Profit"),
                list(label = "Sales", value = "Sales"),
                list(label = "Quantity", value = "Quantity")
              ))
          )),
          dbcRow(list(
            dccDropdown(
              placeholder = "Select a category",
              id = "category-plota",
              value = "Furniture", 
              options = unique(df_plot1$Category) %>%
                purrr::map(function(cate) list(label = cate, value = cate)), style=list('width'='20', 'font-weight'='bold',
                                                                                        'padding-left'='400px')),
            dccGraph(id='bara')
            
          ))
          
        ), width='4'
        ),
        
        
        dbcCol(list(
          dccDropdown(
            placeholder = "Select a category",
            id = "category-plotb",
            value = "Furniture", 
            options = unique(df_plot1$Category) %>%
              purrr::map(function(cate) list(label = cate, value = cate)), style=list('width'='20', 'font-weight'='bold',
                                                                                      'padding-left'='400px')),
          dccGraph(id='barb')
          
          
        ), width='8', style = list(max_height='10%')
        )
      )
      )       
      
      
    )
    ), style = list('max-width' = '100%', 'colour'='#f9f8eb', backgroundColor='#f9f8eb')
  )
)


## Callback: Plot 1
app$callback(
  output(id = 'barchart', property = 'figure'),
  list(input(id = 'category-dropdown', property = 'value')),
  function(category){
    
    sub_categories<- df_plot1$SubCategory[df_plot1$Category==category]
    sales <- df_plot1$Sales[df_plot1$Category==category]
    profit <- df_plot1$Profit[df_plot1$Category==category]
    df_new <-  data.frame(sub_categories, sales, profit)
    df_new1 <- melt(df_new,id='sub_categories')
    
    plot1<- ggplot(df_new1,aes(sub_categories,value,fill=variable))+
      geom_bar(stat="identity",position="dodge")+scale_fill_manual(values = c("midnightblue","red"))+
      ggtitle("Overall Sales & Profit by Category") +scale_y_continuous(name=" ", labels = scales::comma)+
      labs(x = "Sub-categories", fill=" ")+
      theme(panel.background = element_rect(fill ="#f9f8eb", colour = "#f9f8eb",
                                            size = 2, linetype = "solid"), plot.background = element_rect(fill = "#f9f8eb"),
            plot.title = element_text(face = "bold", size = (20)),
            legend.text = element_text(face ="bold", size=(15)), 
            axis.title = element_text(size = (10), face='bold'),
            axis.text = element_text(face='bold', size = (12)),
            legend.background = element_rect(fill = "#f9f8eb")   
      )
    
    
    plot1 <- ggplotly(plot1)
  }
)

## Callback: Plot 2
app$callback(
  list(
    output(id = 'output-title4', property = 'children'),
    output(id= 'bar-graph-top-items', property = 'figure')),
  list(
    input(id = 'states-dropdown', property = 'value')),
  function(selected_state){
    filtered_df4 <-
      df_plot4 |> 
      select(State, Sub.Category, Quantity) |> 
      filter(State == selected_state) |> 
      group_by(Sub.Category) |> 
      summarise(Quantity = sum(Quantity)) |> 
      arrange(-Quantity)
    filtered_df4 <- filtered_df4[1:5,]
    
    plot4 <-
      ggplot(filtered_df4, aes(x=Sub.Category, y=Quantity)) +
      geom_bar(stat = "identity", fill="midnightblue")+theme(panel.background = element_rect(fill ="#f9f8eb", colour = "#f9f8eb",
                                                                                             size = 2, linetype = "solid"), plot.background = element_rect(fill = "#f9f8eb"))
    
    ggplotly4 <- ggplotly(plot4)
    title4 <- paste0("Top 5 Items Sold in: ", selected_state)
    
    list(title4, ggplotly4)
  }
)

## Callback: Plot 3
app$callback(
  list(
    output(id = 'output-title6', property = 'children'),
    output(id= 'bar-graph-top-states', property = 'figure')),
  list(
    input(id = 'subcat-dropdown', property = 'value')),
  function(selected_item){
    filtered_df6 <-
      df_plot6 |> 
      select(State, Sub.Category, Quantity) |> 
      filter(Sub.Category == selected_item) |> 
      group_by(State) |> 
      summarise(Quantity = sum(Quantity)) |> 
      arrange(-Quantity)
    filtered_df6 <- filtered_df6[1:5,]
    
    plot6 <-
      ggplot(filtered_df6, aes(x=State, y=Quantity)) +
      geom_bar(stat = "identity", fill="midnightblue")+theme(panel.background = element_rect(fill ="#f9f8eb", colour = "#f9f8eb",
                                                                                             size = 2, linetype = "solid"), plot.background = element_rect(fill = "#f9f8eb"))
    
    ggplotly6 <- ggplotly(plot6)
    title6 <- paste0("Top 5 States With Highest Quantity Sold in: ", selected_item)
    
    list(title6, ggplotly6)
  }
)


## Callback: Plot 4
app$callback(
  output(id = 'pie-graph-with-radio', property = 'figure'),
  list(input(id = 'metrics', property = 'value')),
  function(selected_metrics){
    to_plot <- df_plot2 |> select(Segment, selected_metrics)
    colnames(to_plot) <- c("Segment", "Metrics")
    
    colors <- c('red', 'midnightblue', 'lightgray')
    
    fig <- plot_ly(to_plot,
                   labels = ~Segment,
                   values = ~Metrics,
                   type = 'pie',
                   marker = list(colors = colors))
    fig <- fig %>% layout(title = 'Metrics Proportion by Segment',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig <- fig %>% layout(plot_bgcolor='#f9f8eb') %>% 
      layout(paper_bgcolor='#f9f8eb')
    
    
    fig
  }
)



## Callback: Plot 5
app$callback(
  output(id = 'bara', property = 'figure'),
  list(input(id = 'category-plota', property = 'value')),
  function(category){
    
    sub_categories<- df_plot1$SubCategory[df_plot1$Category==category]
    sales <- df_plot1$Sales[df_plot1$Category==category]
    profit <- df_plot1$Profit[df_plot1$Category==category]
    df_new <-  data.frame(sub_categories, sales, profit)
    df_new1 <- melt(df_new,id='sub_categories')
    
    plot1<- ggplot(df_new1,aes(sub_categories,value,fill=variable))+
      geom_bar(stat="identity",position="dodge")+scale_fill_manual(values = c("midnightblue","red"))+
      ggtitle("Overall Sales & Profit by Category") +scale_y_continuous(name=" ", labels = scales::comma)+
      labs(x = "Sub-categories", fill=" ")+
      theme(panel.background = element_rect(fill ="#f9f8eb", colour = "#f9f8eb",
                                            size = 2, linetype = "solid"), plot.background = element_rect(fill = "#f9f8eb"),
            plot.title = element_text(face = "bold", size = (20)),
            legend.text = element_text(face ="bold", size=(15)), 
            axis.title = element_text(size = (10), face='bold'),
            axis.text = element_text(face='bold', size = (12)),
            legend.background = element_rect(fill = "#f9f8eb")   
      )
    
    
    plot1 <- ggplotly(plot1)
  }
)


## Callback: Plot 6
app$callback(
  output(id = 'barb', property = 'figure'),
  list(input(id = 'category-plotb', property = 'value')),
  function(category){
    
    sub_categories<- df_plot1$SubCategory[df_plot1$Category==category]
    sales <- df_plot1$Sales[df_plot1$Category==category]
    profit <- df_plot1$Profit[df_plot1$Category==category]
    df_new <-  data.frame(sub_categories, sales, profit)
    df_new1 <- melt(df_new,id='sub_categories')
    
    plot1<- ggplot(df_new1,aes(sub_categories,value,fill=variable))+
      geom_bar(stat="identity",position="dodge")+scale_fill_manual(values = c("midnightblue","red"))+
      ggtitle("Overall Sales & Profit by Category") +scale_y_continuous(name=" ", labels = scales::comma)+
      labs(x = "Sub-categories", fill=" ")+
      theme(panel.background = element_rect(fill ="#f9f8eb", colour = "#f9f8eb",
                                            size = 2, linetype = "solid"), plot.background = element_rect(fill = "#f9f8eb"),
            plot.title = element_text(face = "bold", size = (20)),
            legend.text = element_text(face ="bold", size=(15)), 
            axis.title = element_text(size = (10), face='bold'),
            axis.text = element_text(face='bold', size = (12)),
            legend.background = element_rect(fill = "#f9f8eb")   
      )
    
    
    plot1 <- ggplotly(plot1)
  }
)

# Run server
app$run_server(debug = T)