tadaplot <- TADA_Scatterplot(Data_WV_Mod1_Output)
temp <- tadaplot[["PH"]]
temp
temp %>% 
  plotly::add_trace(y=5, type = 'scatter', mode = 'lines', line = list(color = 'black'), mode = 'lines') %>%
  plotly::add_trace(y=9, type = 'scatter', mode = 'lines', line = list(color = 'black'), mode = 'lines')
TADA_GroupedScatterplot(Data_WV_Mod1_Output, group_col = "MonitoringLocationIdentifier")
