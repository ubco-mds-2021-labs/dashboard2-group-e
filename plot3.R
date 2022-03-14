library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dplyr)
library(plotly)
library(dashBootstrapComponents)
library(ggplot2)
library(ggpubr)

df_plot3<-read.csv("data/Superstore.csv")
head(df_plot3)

categorise <- function(row){
  row["Discount"] = as.double(row["Discount"])
  if (row['Discount'] > 0) {
    return('Yes')
  } else {
    return('No')
  }
}

df_plot3<-filter(df_plot3,"Profit"!=-6599.9780)
head(df_plot3)

df_plot3 <-data.frame(df_plot3 %>% group_by(State, Category,df_plot3["Sub.Category"],df_plot3["Ship.Mode"],Segment) %>%summarise(Sales = sum(Sales),Profit = sum(Profit),Discount=sum(Discount)))
df_plot3["Profit_Margin"]<- df_plot3['Profit']/df_plot3['Sales']
df_plot3["Discount_Status"]<-apply(df_plot3,1,categorise)

category_list = c(unique(df_plot3$Category),"All")


data.frame(df_plot3 %>% group_by(df_plot3["Discount_Status"]) %>%summarise(Profit_Margin = length(Profit_Margin)))





# Create a Dash app
app <- dash_app()

app$layout(
  htmlDiv(
    list(
      
      dccRadioItems(
        id = "metrics",
        value = "Sales",
        inputStyle = list("margin-left"= "5px"),
        options = list(list(label = "Sales", value = "Sales"),list(label = "Profit Margin", value = "Profit_Margin")),
        labelStyle =list("margin-left"= "5px")
      ),
      
      htmlBr(),

      dccDropdown(
        options =category_list %>% purrr::map(function(col) list(label = col, value = col)), 
        value = 'Furniture',id="sub-dropdown",style = list("margin-left"= "5px","width"=200)
      ),
      htmlBr(),
      htmlDiv(id='mkgraph')
      
    )
  )
  
)








app$callback(
  list(output('mkgraph', 'children')),
  list(input('metrics', 'value') ,input('sub-dropdown', 'value')),
  function(input_value,value2) 
  {
    
    if(value2 !="All"){df_plot3_1<-filter(df_plot3,Category==value2)
    
    
    }
    else{df_plot3_1<-df_plot3
    
    value2 = "All Categories"
    }
    
    
    if (input_value=="Sales"){   
      df_plot3_1[input_value] = sapply( df_plot3_1[input_value] ,as.integer)
      df_plot3_2 <-data.frame(df_plot3_1 %>% group_by(Category,df_plot3_1["Sub.Category"],df_plot3_1["Discount_Status"]) %>%summarise(Sales = sum(Sales)))
      p <- ggplot(df_plot3_2) +
        aes(x =Sub.Category,y = Sales,fill=Discount_Status)+
        geom_bar(stat="identity",position = "dodge",width=0.5)+
        xlab("Sub Category")+scale_fill_manual(values = c("midnightblue","red"))+
        labs(fill="Discount Applied?", title = paste("Sales by",value2,sep=" "))+
        font("title",size=24,color="black",face="bold")+
        font("x.title",size=18,color="black")+
        font("y.title",size=18,color="black")+
        theme(plot.title = element_text(hjust = 0.5))
        
        }
    
    
    else{     
      df_plot3_3 <-data.frame(df_plot3_1 %>% group_by(df_plot3_1["Category"],df_plot3_1["Sub.Category"],df_plot3_1["Discount_Status"]) %>%summarise(Sales=mean(Sales),Profit_Margin=mean(Profit_Margin)))
      summary(df_plot3_3)
      p <- ggplot(df_plot3_3)+
        aes(x =Sub.Category,y = Profit_Margin,fill=Discount_Status)+
        xlab("Sub Category")+scale_y_continuous(labels = scales::percent)+
        ylab("Profit Margin")+geom_bar(stat="identity",position = "dodge",width=0.5)+
        scale_fill_manual(values = c("midnightblue","red"))+
        labs(fill="Discount Applied?", title = paste("Profit Margin by",value2,sep=" "))+
        font("title",size=24,color="black",face="bold")+
        font("x.title",size=18,color="black")+
        font("y.title",size=18,color="black")+
        theme(plot.title = element_text(hjust = 0.5))
      
      
      
      
    }
    
    return(list(dccGraph(figure = ggplotly(p))))
    
    
    
    
    
    
    
    
  })





# Run the app
app %>% run_app()
#app$run_server()
#link for Dash: http://127.0.0.1:8050/
