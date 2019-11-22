library(shiny)
library(RODBC)
library(odbc)
library(highcharter)
library(plotly)


server<-function(input, output, session){
  
  ################ User Details ###################
  
  user_details<-session$user
  output$user_info<-renderUI({
    
    HTML('<div>
         <p> Hi ',user_details,'</p>
         <a href="#"><i class="fa fa-circle text-success"></i> Online</a>
         </div>')
    
  }) 
  
  
  
  ############### SQL Server  Local Conenction ##########
  # sql_data_con<-dbConnect(odbc::odbc(),
  #                      Driver="SQL Server",
  #                      Server ="fdxx90sqlvcl1.jdnet.deere.com",
  #                      Database = "HX_AssemblyDataPool",
  #                     port ="1433",
  #                      UID = "Jdaat_reporter",
  #                      PWD = "Jdaat2014"
  #   )
  
  
  ############# Today TAB #####################
  
  ############# Top  Override Gauge ###############
  
  oc_count_plot_gen<-reactive({
    invalidateLater(180000)
    qr<-paste0("Select count(distinct overrideid) as cn from OVERRIDE_LOG with (NOLOCK) where convert(date,entrytimestamp)=convert(date,getdate())")
    cn_data<-dbGetQuery(sql_data_con,qr)
    dataLabel<- paste0("<div style=text-align:center><span style=font-size:300%;>", cn_data$cn,
                       "</span><br/><span style='font-size:100%;color:#808080;'></div>")
    hc <- highchart() %>%
      hc_chart(type = "gauge") %>%
      hc_pane(startAngle = -150,
              endAngle = 150) %>%
      hc_yAxis(
        min = 0,
        max = 400
      )
    hc %>%  hc_add_series(data = cn_data$cn, name = "Override Count",dataLabels=list(borderWidth=0, useHTML=TRUE,format=dataLabel))
  })
  
  
  output$oc_count_plot<-renderHighchart({
    oc_count_plot_gen()
  })
  
  
  ############# Top OverRide Parameter ID ###################
  
  oc_parameter_plot_gen<-reactive({
    invalidateLater(180000)
    qr<-paste0("Select  top 10
                OVERRIDE_LOG.parameterid,
               ASSEMBLY_PARAMETERS.Name,
               count(OVERRIDE_LOG.parameterid)as fail_count from OVERRIDE_LOG with(NOLOCK)
               inner join
               ASSEMBLY_PARAMETERS
               on
               OVERRIDE_LOG.parameterid=assembly_parameters.ParameterID
               and assembly_parameters.disabled_timestamp is NUll
               where
               convert(date,OVERRIDE_LOG.entrytimestamp)=convert(date,getdate()) 
               group by OVERRIDE_LOG.parameterid,ASSEMBLY_PARAMETERS.Name order by count(OVERRIDE_LOG.parameterid) desc ")
    oc_parameter_data<-dbGetQuery(sql_data_con,qr)
    
    hc <- highchart() %>%
      hc_chart(type = "column")%>%
      hc_xAxis(
        title="Parameter Overrided",
        categories = oc_parameter_data$Name) %>%
      hc_yAxis(title = list(text = "Override Count"))%>%
      hc_add_series(name = "Override Count", data = oc_parameter_data$fail_count,color="#367C2B")
    hc
  })
  
  
  output$oc_parameter_plot<-renderHighchart({
    oc_parameter_plot_gen()
  })  
  
  
  ############# Over_Under_Specification_Chart ################ 
  
  over_under_specification_plot_gen<-reactive({
    invalidateLater(300000)
    qr<-paste0("Select top 25
                stations.name,
               ASSEMBLY_PARAMETERS.Name,
               count(case when assembly_results_data.IsAcceptable = 1 then 1 else null end) as ACCEPTED,
               count(case when assembly_results_data.IsAcceptable = 0 then 1 else null end) as REJECTED
               from 
               stations
               inner join
               STATION_PARAMETER_GROUPS
               on 
               STATIONS.stationid=station_parameter_groups.stationid
               and STATIONS.archived_timestamp is null
               inner join 
               ASSEMBLY_PARAMETERS
               on
               station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid
               and station_parameter_groups.archived_timestamp is null
               inner join
               ASSEMBLY_RESULTS_DATA 
               on
               ASSEMBLY_RESULTS_DATA.parameterid=ASSEMBLY_PARAMETERS.parameterid
               and assembly_parameters.disabled_timestamp is NUll
               where convert(date,ASSEMBLY_RESULTS_DATA.entrytimestamp)=convert(date,getdate())
               group by stations.name,
               ASSEMBLY_PARAMETERS.Name 
               having count(case when ASSEMBLY_RESULTS_DATA.IsAcceptable = 0 then 1 else null end) > 0
               order by count(case when assembly_results_data.IsAcceptable = 0 then 1 else null end) desc")
    
    today_spec_data<-dbGetQuery(sql_data_con,qr)
    if(nrow(today_spec_data)==0)
    {
      returnValue()
    }
    else
    {
      today_spec_data$INCIDENT<-c(1:nrow(today_spec_data))
      hc<-highchart() %>%
        hc_add_series(data = today_spec_data, hcaes(INCIDENT, ACCEPTED), name = "ACCEPTED", type = "line",color="#367C2B",
                      tooltip = list(pointFormat = "<b>Station Name:</b> {point.name}  <br/>
                                              <b>Parameter Name:</b> {point.Name}<br/>
                                               <b>Accepted:</b> {point.ACCEPTED}<br/>")) %>%
        hc_add_series(data = today_spec_data, hcaes(INCIDENT, REJECTED), name = "REJECTED", type = "line",color="#FFDE00",
                      tooltip = list(pointFormat = "<b>Rejected:</b> {point.REJECTED}<br/>")) %>%
        hc_yAxis(title = list(text = "Hours")) %>%
        hc_tooltip(headerFormat='',shared=TRUE)
      hc
    }
  })
  
  
  output$over_under_specification_plot<-renderHighchart({
    over_under_specification_plot_gen()
  })
  
  
  
  
  
  
  
  
  
  ########## Performance TAB ########################
  
  ############### Dept List creation ###################  
  
  output$hx_dept_list_creation<-renderUI({
    qr<-paste0("Select distinct deptid,Name from departments where name not like '%Archive' and name not like '%Test%' and archived_timestamp is null 
               order by deptid")
    dept_list<-dbGetQuery(sql_data_con,qr)
    selectInput("selected_dept","Department",choices = as.list(dept_list$Name),width = "85%")
  })
  
  
  ############ Other List Element Generation ###################
  
  per_list_data_creation<-reactive({
    qr<-paste0("Select 
              STATIONS.Name  as STATION_NAME,
             SPG.Name as PARAMETER_GROUP_NAME,
             AP.NAME as PARAMETER_NAME,
             AP.ParameterID 
             FROM ASSEMBLY_PARAMETERS AP WITH (NOLOCK)
             inner join ASSEMBLY_RESULTS_DATA ARD
             on ARD.ParameterID=AP.ParameterID
             INNER JOIN STATION_PARAMETER_GROUPS SPG ON AP.GROUPID=SPG.GROUPID 
             INNER JOIN STATIONS ON STATIONS.STATIONID=SPG.STATIONID 
             INNER JOIN DEPARTMENTS ON DEPARTMENTS.DEPTID=STATIONS.DEPTID
             INNER JOIN BUILDINGS ON BUILDINGS.BuildingID=DEPARTMENTS.BuildingID
             INNER JOIN JD_SITES ON JD_SITES.SiteID=BUILDINGS.SiteID
             where
             (ARD.ParameterChildName = 'Torque')
             and
             (DEPARTMENTS.DeptID  in (select deptid from departments where 
              name ='",input$selected_dept,"' and archived_timestamp is null)) 
             AND 
             (AP.DATATYPE=7) 
             AND 
             (AP.ENABLEPARAMETER=1)  
             AND 
             (AP.Disabled_Timestamp is null) 
             and 
             (AP.Archived_Timestamp is null)
             group by
             AP.ParameterID,
             STATIONS.Name ,
             SPG.Name,
             AP.NAME ")
    
    list_generate_data<<-dbGetQuery(sql_data_con,qr)
    #station_list_data<-list_generate_data$STATION_NAME
    
  })  
  
  ############## Station List  Craetion #######################
  
  output$hx_station_list_creation<-renderUI({
    per_list_data_creation()
    selectInput("selected_station","Station",choices = as.list(unique(list_generate_data$STATION_NAME)),width = "80%")
  })
  
  
  ############# Parameter Group creation ########################
  
  
  output$hx_parameter_group_creation<-renderUI({
    per_list_data_creation()
    parameter_group_list<-unique(list_generate_data[list_generate_data$STATION_NAME==input$selected_station,2])
    selectInput("selected_parameter_group","Parameter Group",choices = as.list(parameter_group_list),width = "90%")
  })
  
  
  ############### Parameter List Creation ############################  
  
  output$hx_parameter_creation<-renderUI({
    per_list_data_creation()
    parameter_name<-unique(list_generate_data[list_generate_data$PARAMETER_GROUP_NAME==input$selected_parameter_group,3])
    selectInput("selected_parameter","Parameter",choices = as.list(parameter_name),width = "90%")
  })
  
  
  
  ################ Valuebox Data Gen ###############################
  
  valuebox_data_gen<-eventReactive(input$top_get_data,{
    if( is.null(input$top_get_data)||input$top_get_data==0)
    {
      returnValue()
    }
    else
    {
      qr<-paste0("Select count(distinct ARD.assemblyid) as VEHICLE,
                   count(case when ARD.IsAcceptable = 1 then 1 else null end) as ACCEPTED,
                   count(case when ARD.IsAcceptable = 0 then 1 else null end) as REJECTED,
                   MAX(ARD.MINVALUE) as MINVALUE,
                   MAX(ARD.MAXVALUE) as MAXVALUE
                   from ASSEMBLY_RESULTS_DATA ARD
                   WITH (NOLOCK) 
                   INNER JOIN ASSEMBLY_PARAMETERS AP on ARD.ParameterID=AP.ParameterID
                   INNER JOIN STATION_PARAMETER_GROUPS SPG ON AP.GROUPID=SPG.GROUPID 
                   INNER JOIN STATIONS ON STATIONS.STATIONID=SPG.STATIONID 
                   INNER JOIN DEPARTMENTS ON DEPARTMENTS.DEPTID=STATIONS.DEPTID
                   INNER JOIN BUILDINGS ON BUILDINGS.BuildingID=DEPARTMENTS.BuildingID
                   INNER JOIN JD_SITES ON JD_SITES.SiteID=BUILDINGS.SiteID 
                   where convert(date,ARD.entrytimestamp) between '",input$selected_date_range[1],"' and '",input$selected_date_range[2],"'
                   and
                   AP.NAME='",input$selected_parameter,"'
                   and
                   SPG.NAME = '",input$selected_parameter_group,"'
                   and
                   STATIONS.Name ='",input$selected_station,"'
                   and
                   DEPARTMENTS.NAME='",input$selected_dept,"'")
      valuebox_data<<-dbGetQuery(sql_data_con,qr)
    }
  })
  
  valuebox4_data_gen<-eventReactive(input$top_get_data,{
    if( is.null(input$top_get_data)||input$top_get_data==0)
    {
      returnValue()
    }
    else
    {
      qr<-paste0("Select count(distinct overrideid) as COUNT from override_log OL
                 WITH (NOLOCK) 
                 INNER JOIN ASSEMBLY_PARAMETERS AP on OL.ParameterID=AP.ParameterID
                 INNER JOIN STATION_PARAMETER_GROUPS SPG ON AP.GROUPID=SPG.GROUPID 
                 INNER JOIN STATIONS ON STATIONS.STATIONID=SPG.STATIONID 
                 INNER JOIN DEPARTMENTS ON DEPARTMENTS.DEPTID=STATIONS.DEPTID
                 INNER JOIN BUILDINGS ON BUILDINGS.BuildingID=DEPARTMENTS.BuildingID
                 INNER JOIN JD_SITES ON JD_SITES.SiteID=BUILDINGS.SiteID 
                 where convert(date,OL.entrytimestamp) between '",input$selected_date_range[1],"' and '",input$selected_date_range[2],"'
                 and
                 AP.NAME='",input$selected_parameter,"'
                 and
                 SPG.NAME = '",input$selected_parameter_group,"'
                 and
                 STATIONS.Name ='",input$selected_station,"'
                 and
                 DEPARTMENTS.NAME='",input$selected_dept,"'")
      valuebox4_data<<-dbGetQuery(sql_data_con,qr)
    }
  })
  
  
  
  
  output$valuebox1<-renderValueBox({
    if(is.null(input$top_get_data)||input$top_get_data==0)
    {
      bs4ValueBox(value = 0, subtitle= "Vehicle Tested",status = "warning",width = 12,icon = "angle-double-up")
    }
    else
    {
      valuebox_data_gen()
      bs4ValueBox(value = valuebox_data$VEHICLE, subtitle= "Vehicle Tested",status = "warning",width = 12,icon = "angle-double-up")
    }
  }) 
  
  output$valuebox2<-renderValueBox({
    if(is.null(input$top_get_data)||input$top_get_data==0)
    {
      bs4ValueBox(value = 0,subtitle= "Under Specification",status = "success",width = 12,icon = "arrow-circle-up")
    }
    else
    {
      valuebox_data_gen()
      bs4ValueBox(value= valuebox_data$ACCEPTED,subtitle="Under Specification",status = "success",width = 12,icon = "arrow-circle-up")
    }
  })
  
  output$valuebox3<-renderValueBox({
    if(is.null(input$top_get_data)||input$top_get_data==0)
    {
      bs4ValueBox(value=0,subtitle="Over Specification",status = "warning",width = 12,icon = "arrow-circle-up")
    }
    else
    {
      valuebox_data_gen()
      bs4ValueBox(value=valuebox_data$REJECTED,subtitle="Over Specification",status = "warning",width = 12,icon = "arrow-circle-up")
    }
  })
  
  
  output$valuebox4<-renderValueBox({
    if(is.null(input$top_get_data)||input$top_get_data==0)
    {
      bs4ValueBox(value=0,subtitle="OverRide",status = "success",width = 12,icon = "exclamation")
    }
    else
    {
      valuebox4_data_gen()
      bs4ValueBox(value=valuebox4_data$COUNT,subtitle="OverRide",status = "success",width = 12,icon = "exclamation")
    }
  })
  
  
  
  output$valuebox5<-renderValueBox({
    if(is.null(input$top_get_data)||input$top_get_data==0)
    {
      bs4ValueBox(value=0,subtitle="Maximum Range",status = "warning",width = 12,icon = "search-plus")
    }
    else
    {
      valuebox_data_gen()
      bs4ValueBox(value=valuebox_data$MAXVALUE,subtitle="Maximum Range",status = "warning",width = 12,icon = "search-plus")
    }
  })
  
  
  output$valuebox6<-renderValueBox({
    if(is.null(input$top_get_data)||input$top_get_data==0)
    {
      bs4ValueBox(value=0,subtitle="Minimum Range",status = "success",width = 12,icon = "search-minus")
    }
    else
    {
      valuebox_data_gen()
      bs4ValueBox(value=valuebox_data$MINVALUE,subtitle="Minimum Range",status = "success",width = 12,icon = "search-minus")
    }
  })
  
  
  
  
  
  ############# Tourque performance Data Gen #################
  
  tourque_performance_plt_gen<-eventReactive(input$top_get_data,{
    if(is.null(input$top_get_data)||input$top_get_data==0)
    {
      returnValue()
    }
    else
    {
      qr<-paste0("Select 
          convert(date,ARD.EntryTimestamp) as DATE_HX,
                 ARD.DataValue,
                 ARD.MINVALUE as MINVALUE,
                 ARD.MAXVALUE as MAXVALUE
                 from ASSEMBLY_RESULTS_DATA ARD
                 WITH (NOLOCK) 
                 INNER JOIN ASSEMBLY_PARAMETERS AP on ARD.ParameterID=AP.ParameterID
                 INNER JOIN STATION_PARAMETER_GROUPS SPG ON AP.GROUPID=SPG.GROUPID 
                 INNER JOIN STATIONS ON STATIONS.STATIONID=SPG.STATIONID 
                 INNER JOIN DEPARTMENTS ON DEPARTMENTS.DEPTID=STATIONS.DEPTID
                 INNER JOIN BUILDINGS ON BUILDINGS.BuildingID=DEPARTMENTS.BuildingID
                 INNER JOIN JD_SITES ON JD_SITES.SiteID=BUILDINGS.SiteID 
                 where convert(date,ARD.entrytimestamp) between '",input$selected_date_range[1],"' and '",input$selected_date_range[2],"'
                 and
                 AP.NAME='",input$selected_parameter,"'
                 and
                 SPG.NAME = '",input$selected_parameter_group,"'
                 and
                 STATIONS.Name ='",input$selected_station,"'
                 and
                 DEPARTMENTS.NAME='",input$selected_dept,"'
                 and
                 DEPARTMENTS.archived_timestamp is null
                 and
                 SPG.archived_timestamp is null
                 and
                 AP.archived_timestamp is null and 
                 AP.disabled_timestamp is null and 
                 ARD.parameterchildname='Torque'")
      
      tourque_performance_data<-dbGetQuery(sql_data_con,qr)
      if(nrow(tourque_performance_data)==0)
      {
        returnValue()
        shinyalert::shinyalert("Harvester Quality System","There is no Tourque Data available for this above Selected Combinations",type = "warning")
      }
      else
      {
        plot_ly(data = tourque_performance_data,type = "box", x = ~DATE_HX , y = ~ as.numeric(DataValue),name = "Tourque Values")%>%
          add_lines(y=tourque_performance_data$MINVALUE, type='scatter', mode='lines',name="Min Limit")%>%
          add_lines(y=tourque_performance_data$MAXVALUE, type='scatter', mode='lines',name="Max Limit")%>%
          layout(
            legend = list(orientation = 'h',xanchor = "center",x = 0.7))%>%config(displayModeBar = F)
        # hcboxplot(x = as.numeric(tourque_performance_data$DataValue), var=tourque_performance_data$DATE,color="#367C2B",
        #           outliers = FALSE) %>% 
        #   hc_chart(type = "column")
        
      }
    }
  })
  
  
  
  output$tourque_performance<-renderPlotly({
    validate(need(input$top_get_data,""))
    input$top_get_data
    tourque_performance_plt_gen()
  })
  
  
  
  
  
  ############## Capabilities ####################  
  
  ############## dept list ###############  
  
  output$cap_dept_list_creation<-renderUI({
    qr<-paste0("Select distinct deptid,Name from departments where name not like '%Archive' and name not like '%Test%' and archived_timestamp is null 
               order by deptid")
    dept_list<-dbGetQuery(sql_data_con,qr)
    selectInput("selected_dept_cap","Department",choices = as.list(dept_list$Name),width = "75%")
  })
  
  ############## Other List gen #############
  
  cap_list_data_creation<-reactive({
    qr<-paste0("Select 
               STATIONS.Name  as STATION_NAME,
               SPG.Name as PARAMETER_GROUP_NAME,
               AP.NAME as PARAMETER_NAME,
               AP.ParameterID 
               FROM ASSEMBLY_PARAMETERS AP WITH (NOLOCK)
               inner join ASSEMBLY_RESULTS_DATA ARD
               on ARD.ParameterID=AP.ParameterID
               INNER JOIN STATION_PARAMETER_GROUPS SPG ON AP.GROUPID=SPG.GROUPID 
               INNER JOIN STATIONS ON STATIONS.STATIONID=SPG.STATIONID 
               INNER JOIN DEPARTMENTS ON DEPARTMENTS.DEPTID=STATIONS.DEPTID
               INNER JOIN BUILDINGS ON BUILDINGS.BuildingID=DEPARTMENTS.BuildingID
               INNER JOIN JD_SITES ON JD_SITES.SiteID=BUILDINGS.SiteID
               where
               (ARD.ParameterChildName = 'Torque')
               and
               (DEPARTMENTS.DeptID  in (select deptid from departments where 
               name ='",input$selected_dept_cap,"' and archived_timestamp is null)) 
               AND 
               (AP.DATATYPE=7) 
               AND 
               (AP.ENABLEPARAMETER=1)  
               AND 
               (AP.Disabled_Timestamp is null) 
               and 
               (AP.Archived_Timestamp is null)
               group by
               AP.ParameterID,
               STATIONS.Name ,
               SPG.Name,
               AP.NAME ")
    
    cap_list_generate_data<<-dbGetQuery(sql_data_con,qr)
    
  })
  
  
  
  ############## Station List  Craetion #######################
  
  output$cap_station_list_creation<-renderUI({
    # qr<-paste0("Select distinct name from stations where deptid = (select deptid from departments where name ='",input$selected_dept,"' and archived_timestamp is null )")
    # station_list<-dbGetQuery(sql_data_con,qr)
    
    cap_list_data_creation()
    selectInput("selected_station_cap","Station",choices = as.list(unique(cap_list_generate_data$STATION_NAME)),width = "90%")
  })  
  
  ############# Parameter Group creation ########################
  
  
  output$cap_parameter_group_creation<-renderUI({
    # qr<-paste0("Select name from dbo.station_parameter_groups where stationid=(Select stationid from stations 
    #             where name='",input$selected_station,"'
    #             and
    #            archived_timestamp is null)
    #            and archived_timestamp is null")
    # parameter_group<-dbGetQuery(sql_data_con,qr)
    cap_list_data_creation()
    parameter_group_list<-unique(cap_list_generate_data[cap_list_generate_data$STATION_NAME==input$selected_station_cap,2])
    selectInput("selected_parameter_group_cap","Parameter Group",choices = as.list(parameter_group_list),width = "95%")
  })
  
  
  
  ############### Parameter List Creation ############################  
  
  output$cap_parameter_creation<-renderUI({
    cap_list_data_creation()
    parameter_name<-unique(cap_list_generate_data[cap_list_generate_data$PARAMETER_GROUP_NAME==input$selected_parameter_group_cap,3])
    selectInput("selected_parameter_cap","Parameter",choices = as.list(parameter_name),width = "95%")
  })
  
  
  
  ############ Runner Plot Generation #############
  
  runner_plot_generation<-eventReactive(input$cap_top_get_data,{
    
    if(is.null(input$cap_top_get_data)||input$cap_top_get_data==0)
    {
      returnValue()
    }
    else
    {
      qr<-paste0("Select 
                 CONVERT(VARCHAR(5), ARD.EntryTimestamp, 114) as TIME,
                 ARD.DataValue,
                 ARD.MINVALUE as MINVALUE,
                 ARD.MAXVALUE as MAXVALUE,
                 ALS.ASSEMBLYNAME,
                 case
                 when ARD.DataValue>=ARD.MINVALUE and ARD.DataValue<=ARD.MAXVALUE  then 1 else 0
                 end 
                 as STATUS
                 from ASSEMBLY_RESULTS_DATA ARD
                 WITH (NOLOCK)
                 inner join ASSEMBLIES ALS on ARD.ASSEMBLYID=ALS.ASSEMBLYID 
                 INNER JOIN ASSEMBLY_PARAMETERS AP on ARD.ParameterID=AP.ParameterID
                 INNER JOIN STATION_PARAMETER_GROUPS SPG ON AP.GROUPID=SPG.GROUPID 
                 INNER JOIN STATIONS ON STATIONS.STATIONID=SPG.STATIONID 
                 INNER JOIN DEPARTMENTS ON DEPARTMENTS.DEPTID=STATIONS.DEPTID
                 INNER JOIN BUILDINGS ON BUILDINGS.BuildingID=DEPARTMENTS.BuildingID
                 INNER JOIN JD_SITES ON JD_SITES.SiteID=BUILDINGS.SiteID 
                 where convert(date,ARD.entrytimestamp)='",input$cap_selected_date_range,"'
                 and
                 AP.NAME='",input$selected_parameter_cap,"'
                 and
                 SPG.NAME = '",input$selected_parameter_group_cap,"'
                 and
                 STATIONS.Name ='",input$selected_station_cap,"'
                 and
                 DEPARTMENTS.NAME='",input$selected_dept_cap,"'
                 and
                 DEPARTMENTS.archived_timestamp is null
                 and
                 SPG.archived_timestamp is null
                 and
                 AP.archived_timestamp is null and 
                 AP.disabled_timestamp is null and 
                 ARD.parameterchildname='Torque' ")
      
      cap_data<<-dbGetQuery(sql_data_con,qr)
      
    }
    
  })
  
  
  output$runner_chart<-renderHighchart({
    validate(need(input$cap_top_get_data,""))
    input$cap_top_get_data
    runner_plot_generation()
    if(nrow(cap_data)==0)
    {
      returnValue()
      shinyalert::shinyalert("Harvester Quality System","There is no Tourque Data available for this above Selected Combinations",type = "warning")
    }
    else
    {
      hc<-highchart()%>%
        hc_chart(zoomType="x")%>%
        hc_tooltip(headerFormat='',shared=TRUE)%>%
        hc_xAxis(categories=cap_data$TIME)%>%
        hc_add_series(data = cap_data, hcaes(TIME, as.numeric(DataValue)),
                      name = "Tourque Value", 
                      type = "line",
                      color="#367C2B",
                      tooltip = list(pointFormat = "<b>TIME:</b> {point.TIME}<br/><b>Tourque Value:</b> {point.DataValue}<br/><b>Serial No:</b> {point.ASSEMBLYNAME}<br/>")
        )%>%
        hc_add_series(data = cap_data, hcaes(TIME, as.numeric(MINVALUE)), name = "Min Value",
                      tooltip = list(pointFormat = "<b>Min Limit:</b> {point.MINVALUE}<br/>"), type = "line",color="#FFDE00")%>%
        hc_add_series(data = cap_data, hcaes(TIME, as.numeric(MAXVALUE)), name = "Max Value",
                      tooltip = list(pointFormat = "<b>Max Limit:</b> {point.MAXVALUE}<br/>"),
                      type = "line",color="#FFDE00")
      
      hc
    }
  })
  
  ############# Density plot generation #####################  
  
  output$density_plot<-renderPlotly({
    validate(need(input$cap_top_get_data,""))
    input$cap_top_get_data
    runner_plot_generation()
    if(nrow(cap_data)==0)
    {
      returnValue()
    }
    else
    {
      plot_data<-cap_data[cap_data$STATUS==1,2]
      fit<-density(as.numeric(plot_data))
      ay <- list(
        tickfont = list(color = "red"),
        overlaying = "y",
        side = "right"
      )
      m <- list(
        l = 10,
        r = 50,
        b = 10,
        t = 10,
        pad = 4
      )
      plot_ly(x = as.numeric(plot_data), type = "histogram", name = "Tourque Data") %>%
        add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
        layout(yaxis2 = ay,autosize = TRUE,margin=m,
               legend = list(orientation = 'h')
        )%>%config(displayModeBar = F)
    }
  })
  
  
  
  
  ############ Performance Index generation ###################
  output$performance_index<-renderHighchart({
    validate(need(input$cap_top_get_data,""))
    input$cap_top_get_data
    runner_plot_generation()
    if(nrow(cap_data)==0)
    {
      returnValue()
    }
    else
    {
      fit_data<-as.numeric(cap_data[cap_data$STATUS==1,2])
      mean_fit<-mean(fit_data)
      USL<-max(as.numeric(cap_data$MAXVALUE))
      LSL<-max(as.numeric(cap_data$MINVALUE))
      std_dev<-sd(fit_data)
      
      cp<-(USL-LSL)/(6*std_dev)
      cpu<-(USL-mean_fit)/(3*std_dev)  
      cpl<-(mean_fit-LSL)/(3*std_dev)  
      cpk<-  min(cpu,cpl)
      
      hc<-highchart() %>% 
        hc_chart(type = "solidgauge") %>% 
        hc_tooltip(borderWidth = 0,backgroundColor = 'none',shadow = FALSE,style = list(fontSize = '16px'),
                   pointFormat = '{series.name}<br><span style="font-size:2em; color: {point.color}; font-weight: bold">{point.y}</span>',
                   positioner = JS("function (labelWidth, labelHeight) {return {x:100-labelWidth / 2,y: 180};}")) %>% 
        hc_pane(startAngle = 0,endAngle = 360,
                background = list(
                  list(outerRadius = '112%',innerRadius = '88%',backgroundColor = JS("Highcharts.Color('#367C2B').setOpacity(0).get()"),borderWidth =  0),
                  list(outerRadius = '87%',innerRadius = '63%',backgroundColor = JS("Highcharts.Color('#FFDE00').setOpacity(0).get()"),borderWidth = 0),
                  list(outerRadius = '62%',innerRadius =  '38%',backgroundColor = JS("Highcharts.Color('#27251F').setOpacity(0).get()"),borderWidth = 0),
                  list(outerRadius = '37%',innerRadius =  '13%',backgroundColor = JS("Highcharts.Color('#0CCDD6').setOpacity(0).get()"),borderWidth = 0)
                )) %>% 
        hc_yAxis(min = -10,max = 50,lineWidth = 0,tickPositions = list()) %>% 
        hc_plotOptions(solidgauge = list(borderWidth = '34px',dataLabels = list(enabled = FALSE),linecap = 'round',stickyTracking = FALSE)) %>% 
        hc_add_series(name = "Cp",borderColor ='#367C2B' ,data = list(list(color ='#367C2B' ,radius = "100%",innerRadius = "100%",y =round(cp,3) ))) %>% 
        hc_add_series(name = "CpU",borderColor = '#FFDE00',data = list(list(color = '#FFDE00',radius = "75%",innerRadius = "75%",y = round(cpu,3)))) %>% 
        hc_add_series(name = "CpL",borderColor = '#27251F',data = list(list(color = '#27251F',radius = "50%",innerRadius = "50%",y = round(cpl,3))))%>%
        hc_add_series(name = "CpK",borderColor = JS("Highcharts.getOptions().colors[3]"),data = list(list(color = JS("Highcharts.getOptions().colors[3]"),radius = "25%",innerRadius = "25%",y = round(cpk,3))))
      hc
    }
    
  })
  
  
  
  
  
  
  
  
  
  ############### Override performance ##############
  
  ################# Show UI ###################  
  
  observe({
    if(input$over_per_month_option==TRUE)
    {
      shinyjs::show("over_per_month_id")
      shinyjs::show("over_per_week_option_material_id")
    }
    else
    {
      shinyjs::hide("over_per_month_id")
      shinyjs::hide("over_per_week_option_material_id")
      shinyjs::hide("over_per_week_id")
    }
  })
  
  
  observe({
    if(input$over_per_week_option==TRUE)
    {
      shinyjs::show("over_per_week_id")
      shinyjs::disable("selected_month")
      shinyjs::disable("selected_year")
    }
    else
    {
      shinyjs::hide("over_per_week_id")
      shinyjs::enable("selected_month")
      shinyjs::enable("selected_year")
    }
  })
  
  
  
  ################### Top 10 Parameter Issue ################
  
  
  parameter_issue_plot_gen<-eventReactive(input$get_or_data,{
    if(is.null(input$get_or_data)||input$get_or_data==0)
    {
      returnValue()
    }
    else
    {
      if(input$over_per_month_option==FALSE)
      {
        qr<-paste0("
                    Select  top 10
                   OVERRIDE_LOG.parameterid,
                   ASSEMBLY_PARAMETERS.Name,
                   count(OVERRIDE_LOG.parameterid)as fail_count from OVERRIDE_LOG with(NOLOCK)
                   inner join
                   ASSEMBLY_PARAMETERS
                   on
                   OVERRIDE_LOG.parameterid=assembly_parameters.ParameterID
                   and assembly_parameters.disabled_timestamp is NUll
                   where
                   year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                   group by OVERRIDE_LOG.parameterid,ASSEMBLY_PARAMETERS.Name order by count(OVERRIDE_LOG.parameterid) desc")
      }
      else
      {
        if(input$over_per_week_option==TRUE)
        {
          qr<-paste0("Select  top 10
                      OVERRIDE_LOG.parameterid,
                     ASSEMBLY_PARAMETERS.Name,
                     count(OVERRIDE_LOG.parameterid)as fail_count from OVERRIDE_LOG with(NOLOCK)
                     inner join
                     ASSEMBLY_PARAMETERS
                     on
                     OVERRIDE_LOG.parameterid=assembly_parameters.ParameterID
                     and assembly_parameters.disabled_timestamp is NUll
                     where
                     convert(date,entrytimestamp) between '",input$selected_date[1],"' and '",input$selected_date[2],"'
                     group by OVERRIDE_LOG.parameterid,ASSEMBLY_PARAMETERS.Name order by count(OVERRIDE_LOG.parameterid) desc")
        }
        else
        {
          qr<-paste0("Select  top 10
                      OVERRIDE_LOG.parameterid,
                     ASSEMBLY_PARAMETERS.Name,
                     count(OVERRIDE_LOG.parameterid)as fail_count from OVERRIDE_LOG with(NOLOCK)
                     inner join
                     ASSEMBLY_PARAMETERS
                     on
                     OVERRIDE_LOG.parameterid=assembly_parameters.ParameterID
                     and assembly_parameters.disabled_timestamp is NUll
                     where
                     year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                     and month(OVERRIDE_LOG.entrytimestamp)='",input$selected_month,"'
                     group by OVERRIDE_LOG.parameterid,ASSEMBLY_PARAMETERS.Name order by count(OVERRIDE_LOG.parameterid) desc")
        }
      }
      parameter_issue_data<-dbGetQuery(sql_data_con,qr)
      if(nrow(parameter_issue_data)==0)
      {
        returnValue()
      }
      else
      {
        hc<-highchart() %>%
          hc_chart(type = "column") %>%
          hc_xAxis(categories = parameter_issue_data$Name,
                   tickmarkPlacement = "on",
                   title = list(enabled = FALSE),
                   labels=list(
                     style=list(
                       fontSize='10px'
                     ),
                     step=1
                   )
          ) %>%
          hc_yAxis(title = list(text = "Issue Count of Top 10 Parameter")) %>%
          hc_add_series(name = "Count of Issue", data =parameter_issue_data$fail_count,color='#20720D')
        hc
      }
    }
  })
  
  
  output$parameter_issue<-renderHighchart({
    validate(need(input$get_or_data,""))
    input$get_or_data
    parameter_issue_plot_gen()
  })
  
  
  
  ################## Top 10 Over Rides by Station #################
  
  overrides_by_station_plt<-eventReactive(input$get_or_data,{
    if(is.null(input$get_or_data)||input$get_or_data==0)
    {
      returnValue()
    }
    else
    {
      if(input$over_per_month_option==FALSE)
      {
        qr<-paste0("Select top 10
                    stations.name,
                   count(OVERRIDE_LOG.parameterid) as fail_count,
                   count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '7' and '15' then 1 else null end) as Shift1,
                   count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '16' and '23' then 1 else null end) as Shift2,
                   count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '0' and '6' then 1 else null end) as Shift3
                   from 
                   stations with(NOLOCK)
                   inner join
                   STATION_PARAMETER_GROUPS with(NOLOCK)
                   on 
                   STATIONS.stationid=station_parameter_groups.stationid
                   and STATIONS.archived_timestamp is null
                   inner join 
                   ASSEMBLY_PARAMETERS with(NOLOCK)
                   on
                   station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid
                   and station_parameter_groups.archived_timestamp is null
                   inner join
                   override_log with(NOLOCK)
                   on
                   override_log.parameterid=ASSEMBLY_PARAMETERS.parameterid
                   and assembly_parameters.disabled_timestamp is NUll
                   where 
                   year(override_log.entrytimestamp)='",input$selected_year,"'
                   group by stations.name
                   order by 
                   count(OVERRIDE_LOG.parameterid) desc
                   ")
      }
      else
      {
        if(input$over_per_week_option==TRUE)
        {
          qr<-paste0("Select top 10
                    stations.name,
                     count(OVERRIDE_LOG.parameterid) as fail_count,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '7' and '15' then 1 else null end) as Shift1,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '16' and '23' then 1 else null end) as Shift2,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '0' and '6' then 1 else null end) as Shift3
                     from 
                     stations with(NOLOCK)
                     inner join
                     STATION_PARAMETER_GROUPS with(NOLOCK)
                     on 
                     STATIONS.stationid=station_parameter_groups.stationid
                     and STATIONS.archived_timestamp is null
                     inner join 
                     ASSEMBLY_PARAMETERS with(NOLOCK)
                     on
                     station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid
                     and station_parameter_groups.archived_timestamp is null
                     inner join
                     override_log with(NOLOCK)
                     on
                     override_log.parameterid=ASSEMBLY_PARAMETERS.parameterid
                     and assembly_parameters.disabled_timestamp is NUll
                     where 
                     convert(date,override_log.entrytimestamp) between '",input$selected_date[1],"' and '",input$selected_date[2],"'
                     group by stations.name
                     order by 
                     count(OVERRIDE_LOG.parameterid) desc")
          
        }
        else
        {
          qr<-paste0("Select top 10
                    stations.name,
                     count(OVERRIDE_LOG.parameterid) as fail_count,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '7' and '15' then 1 else null end) as Shift1,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '16' and '23' then 1 else null end) as Shift2,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '0' and '6' then 1 else null end) as Shift3
                     from 
                     stations with(NOLOCK)
                     inner join
                     STATION_PARAMETER_GROUPS with(NOLOCK)
                     on 
                     STATIONS.stationid=station_parameter_groups.stationid
                     and STATIONS.archived_timestamp is null
                     inner join 
                     ASSEMBLY_PARAMETERS with(NOLOCK)
                     on
                     station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid
                     and station_parameter_groups.archived_timestamp is null
                     inner join
                     override_log with(NOLOCK)
                     on
                     override_log.parameterid=ASSEMBLY_PARAMETERS.parameterid
                     and assembly_parameters.disabled_timestamp is NUll
                     where 
                      year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                     and month(OVERRIDE_LOG.entrytimestamp)='",input$selected_month,"'
                     group by stations.name
                     order by 
                     count(OVERRIDE_LOG.parameterid) desc")
          
        }
      }
      override_station_data<-dbGetQuery(sql_data_con,qr)
      if(nrow(override_station_data)==0)
      {
        returnValue()
      }
      else
      {
        colors=c('#27251F','#367C2B','#FFDE00','#27251F','#367C2B','#FFDE00','#27251F')
        hc<-highchart() %>%
          hc_chart(type = "column") %>%
          hc_yAxis(title = list(text = "Override Counts of Top 10 Station")) %>%
          hc_xAxis(categories = override_station_data$name,
                   tickmarkPlacement = "on",
                   title = list(enabled = FALSE),
                   labels=list(
                     style=list(
                       fontSize='10px'
                     ),
                     step=1
                   )
          ) %>%
          hc_tooltip(formatter = JS("function () {
                            return '<b>' + this.x + '</b><br/>' +
                            this.series.name + ': ' + this.y + '<br/>' +
                            'Total: ' + this.point.stackTotal;
                            }")) %>%
          hc_plotOptions(column=list(stacking="normal"))
        
        for(i in 3:ncol(override_station_data)) 
        {
          hc <- hc_add_series(hc, override_station_data[, i], name = names(override_station_data)[i],color=colors[i]
          )
        }
        hc
      }
    }
  })
  
  
  output$overrides_by_station<-renderHighchart({
    validate(need(input$get_or_data,""))
    input$get_or_data
    overrides_by_station_plt()
  })
  
  
  
  
  
  ################# Top 10  overrides by Departments ###############  
  
  overrides_by_department_plot<-eventReactive(input$get_or_data,{
    if(is.null(input$get_or_data)||input$get_or_data==0)
    {
      returnValue()
    }
    else
    {
      if(input$over_per_month_option==FALSE)
      {
        qr<-paste0("Select top 10
                    departments.name,
                   count(OVERRIDE_LOG.parameterid) as fail_count,
                   count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '7' and '15' then 1 else null end) as Shift1,
                   count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '16' and '23' then 1 else null end) as Shift2,
                   count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '0' and '6' then 1 else null end) as Shift3
                   from 
                   departments
                   inner join
                   stations with(NOLOCK)
                   on
                   departments.deptid=stations.deptid
                   and departments.archived_timestamp is null
                   inner join
                   STATION_PARAMETER_GROUPS with(NOLOCK)
                   on 
                   STATIONS.stationid=station_parameter_groups.stationid
                   and STATIONS.archived_timestamp is null
                   inner join 
                   ASSEMBLY_PARAMETERS with(NOLOCK)
                   on
                   station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid
                   and station_parameter_groups.archived_timestamp is null
                   inner join
                   override_log with(NOLOCK)
                   on
                   override_log.parameterid=ASSEMBLY_PARAMETERS.parameterid
                   and assembly_parameters.disabled_timestamp is NUll
                   where 
                   year(override_log.entrytimestamp)='",input$selected_year,"'
                   group by departments.name
                   order by 
                   count(OVERRIDE_LOG.parameterid) desc")
        
      }
      else
      {
        if(input$over_per_week_option==TRUE)
        {
          qr<-paste0("Select top 10
                      departments.name,
                     count(OVERRIDE_LOG.parameterid) as fail_count,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '7' and '15' then 1 else null end) as Shift1,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '16' and '23' then 1 else null end) as Shift2,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '0' and '6' then 1 else null end) as Shift3
                     from 
                     departments with(NOLOCK)
                     inner join
                     stations with(NOLOCK)
                     on
                     departments.deptid=stations.deptid
                     and departments.archived_timestamp is null
                     inner join
                     STATION_PARAMETER_GROUPS with(NOLOCK)
                     on 
                     STATIONS.stationid=station_parameter_groups.stationid
                     and STATIONS.archived_timestamp is null
                     inner join 
                     ASSEMBLY_PARAMETERS with(NOLOCK)
                     on
                     station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid
                     and station_parameter_groups.archived_timestamp is null
                     inner join
                     override_log with(NOLOCK)
                     on
                     override_log.parameterid=ASSEMBLY_PARAMETERS.parameterid
                     and assembly_parameters.disabled_timestamp is NUll
                     where 
                     convert(date,override_log.entrytimestamp) between '",input$selected_date[1],"' and '",input$selected_date[2],"'
                     group by departments.name
                     order by 
                     count(OVERRIDE_LOG.parameterid) desc")
          
          
        }
        else
        {
          qr<-paste0("Select top 10
                      departments.name,
                     count(OVERRIDE_LOG.parameterid) as fail_count,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '7' and '15' then 1 else null end) as Shift1,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '16' and '23' then 1 else null end) as Shift2,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '0' and '6' then 1 else null end) as Shift3
                     from 
                     departments with(NOLOCK)
                     inner join
                     stations with(NOLOCK)
                     on
                     departments.deptid=stations.deptid
                     and departments.archived_timestamp is null
                     inner join
                     STATION_PARAMETER_GROUPS with(NOLOCK)
                     on 
                     STATIONS.stationid=station_parameter_groups.stationid
                     and STATIONS.archived_timestamp is null
                     inner join 
                     ASSEMBLY_PARAMETERS with(NOLOCK)
                     on
                     station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid
                     and station_parameter_groups.archived_timestamp is null
                     inner join
                     override_log with(NOLOCK)
                     on
                     override_log.parameterid=ASSEMBLY_PARAMETERS.parameterid
                     and assembly_parameters.disabled_timestamp is NUll
                     where 
                      year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                     and month(OVERRIDE_LOG.entrytimestamp)='",input$selected_month,"'
                     group by departments.name
                     order by 
                     count(OVERRIDE_LOG.parameterid) desc")
          
          
        }
      }
      override_dept_data<-dbGetQuery(sql_data_con,qr)
      if(nrow(override_dept_data)==0)
      {
        returnValue()
      }
      else
      {
        colors=c('#27251F','#367C2B','#FFDE00','#27251F','#367C2B','#FFDE00','#27251F')
        hc<-highchart() %>%
          hc_chart(type = "column") %>%
          hc_yAxis(title = list(text = "Override Counts of Top 10 Department")) %>%
          hc_xAxis(categories = override_dept_data$name,
                   tickmarkPlacement = "on",
                   title = list(enabled = FALSE),
                   labels=list(
                     style=list(
                       fontSize='10px'
                     ),
                     step=1
                   )
          ) %>%
          hc_tooltip(formatter = JS("function () {
                                    return '<b>' + this.x + '</b><br/>' +
                                    this.series.name + ': ' + this.y + '<br/>' +
                                    'Total: ' + this.point.stackTotal;
      }")) %>%
          hc_plotOptions(column=list(stacking="normal"))
        
        for(i in 3:ncol(override_dept_data)) 
        {
          hc <- hc_add_series(hc, override_dept_data[, i], name = names(override_dept_data)[i],color=colors[i]
          )
        }
        hc
      }
    }
  })
  
  
  output$overrides_by_department<-renderHighchart({
    validate(need(input$get_or_data,""))
    input$get_or_data
    overrides_by_department_plot()
  })
  
  
  
  
  ################ Top 10 Overrides by Person #############
  
  overrides_by_person_plot<-eventReactive(input$get_or_data,{
    if(is.null(input$get_or_data)||input$get_or_data==0)
    {
      returnValue()
    }
    else
    {
      if(input$over_per_month_option==FALSE)
      {
        qr<-paste0("Select top 10 username,count(username) as person_count from override_log
                   where 
                   year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                   group by 
                   override_log.username
                   order by count(username) desc ")
      }
      else
      {
        if(input$over_per_week_option==TRUE)
        {
          qr<-paste0("Select top 10 username,count(username) as person_count from override_log
                     where 
                     convert(date,override_log.entrytimestamp) between '",input$selected_date[1],"' and '",input$selected_date[2],"'
                     group by 
                     override_log.username
                     order by count(username) desc ")
        }
        else
        {
          qr<-paste0("Select top 10 username,count(username) as person_count from override_log
                     where 
                     year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                     and month(OVERRIDE_LOG.entrytimestamp)='",input$selected_month,"'
                     group by 
                     override_log.username
                     order by count(username) desc ")
        }
      }
      username_data<-dbGetQuery(sql_data_con,qr)
      if(nrow(username_data)==0)
      {
        returnValue()
      }
      else
      {
        hc<-highchart() %>%
          hc_chart(type = "column") %>%
          hc_xAxis(categories = username_data$username,
                   tickmarkPlacement = "on",
                   title = list(enabled = FALSE),
                   labels=list(
                     style=list(
                       fontSize='10px'
                     ),
                     step=1
                   )
          ) %>%
          hc_yAxis(title = list(text = "Top 10 User who Overrides")) %>%
          hc_add_series(name = "OverRide Count", data =username_data$person_count,color='#20720D')
        hc
      }
      
    }
  })
  
  
  output$overrides_by_person<-renderHighchart({
    validate(need(input$get_or_data,""))
    input$get_or_data
    overrides_by_person_plot()
  })
  
  
  
  
  
  
  
  
  ################ Top 10  Overrides Codes in Override ############
  
  overrides_by_oc_code_plot<-eventReactive(input$get_or_data,{
    if(is.null(input$get_or_data)||input$get_or_data==0)
    {
      returnValue()
    }
    else
    {
      if(input$over_per_month_option==FALSE)
      {
        qr<-paste0("Select top 10  manual_override_dept_codes.codeid,
                    manual_override_dept_codes.description,
                   count(override_log.codeid) as COUNT_FAIL
                   from 
                   manual_override_dept_codes with(NOLOCK)
                   inner join 
                   override_log
                   on
                   override_log.codeid=manual_override_dept_codes.codeid
                   where
                   year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                   group by
                   manual_override_dept_codes.codeid,
                   manual_override_dept_codes.description
                   order by 
                   count(override_log.codeid) desc")
      }
      else
      {
        if(input$over_per_week_option==TRUE)
        {
          qr<-paste0("Select top 10  manual_override_dept_codes.codeid,
                    manual_override_dept_codes.description,
                     count(override_log.codeid) as COUNT_FAIL
                     from 
                     manual_override_dept_codes with(NOLOCK)
                     inner join 
                     override_log
                     on
                     override_log.codeid=manual_override_dept_codes.codeid
                     where
                      convert(date,override_log.entrytimestamp) between '",input$selected_date[1],"' and '",input$selected_date[2],"'
                     group by
                     manual_override_dept_codes.codeid,
                     manual_override_dept_codes.description
                     order by 
                     count(override_log.codeid) desc")
        }
        else
        {
          qr<-paste0("Select top 10  manual_override_dept_codes.codeid,
                      manual_override_dept_codes.description,
                     count(override_log.codeid) as COUNT_FAIL
                     from 
                     manual_override_dept_codes with(NOLOCK)
                     inner join 
                     override_log
                     on
                     override_log.codeid=manual_override_dept_codes.codeid
                     where
                     year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                     and month(OVERRIDE_LOG.entrytimestamp)='",input$selected_month,"'
                     group by
                     manual_override_dept_codes.codeid,
                     manual_override_dept_codes.description
                     order by 
                     count(override_log.codeid) desc")
        }
      }
      oc_data<-dbGetQuery(sql_data_con,qr)
      if(nrow(username_data)==0)
      {
        returnValue()
      }
      else
      {
        hc<-highchart() %>%
          hc_chart(type = "column") %>%
          hc_xAxis(categories = oc_data$description,
                   tickmarkPlacement = "on",
                   title = list(enabled = FALSE),
                   labels=list(
                     style=list(
                       fontSize='10px'
                     ),
                     step=1
                   )
          ) %>%
          hc_yAxis(title = list(text = "Top 10 OverRide Code Count")) %>%
          hc_add_series(name = "OverRide Code Count", data =oc_data$COUNT_FAIL,color='#20720D')
        hc
      }
    }
  })
  
  
  output$overrides_by_oc_code<-renderHighchart({
    validate(need(input$get_or_data,""))
    input$get_or_data
    overrides_by_oc_code_plot()
  })
  
  
  
  ################ Top 10 Comments in Override #############
  
  overrides_by_comment_plot<-eventReactive(input$get_or_data,{
    if(is.null(input$get_or_data)||input$get_or_data==0)
    {
      returnValue()
    }
    else
    {
      if(input$over_per_month_option==FALSE)
      {
        
        qr<-paste0("Select top 10 comments, count(comments) as COUNT from override_log 
                    where 
                    year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                    group by
                   comments
                   order by 
                   count(comments) desc")
      }
      else
      {
        if(input$over_per_week_option==TRUE)
        {
          qr<-paste0("Select top 10 comments, count(comments) as COUNT from override_log 
                      where 
                      convert(date,override_log.entrytimestamp) between '",input$selected_date[1],"' and '",input$selected_date[2],"'
                      group by
                     comments
                     order by 
                     count(comments) desc")
        }
        else
        {
          qr<-paste0("Select top 10 comments, count(comments) as COUNT from override_log 
                      where 
                    year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                     and month(OVERRIDE_LOG.entrytimestamp)='",input$selected_month,"'
                     group by
                     comments
                     order by 
                     count(comments) desc")
        }
      }
      comment_data<-dbGetQuery(sql_data_con,qr)
      if(nrow(username_data)==0)
      {
        returnValue()
      }
      else
      {
        hc<-highchart() %>%
          hc_chart(type = "column") %>%
          hc_xAxis(categories = comment_data$comments,
                   tickmarkPlacement = "on",
                   title = list(enabled = FALSE),
                   labels=list(
                     style=list(
                       fontSize='10px'
                     ),
                     step=1
                   )
          ) %>%
          hc_yAxis(title = list(text = "Top 10 OverRide Comment Count")) %>%
          hc_add_series(name = "OverRide Comment Count", data =comment_data$COUNT,color='#20720D')
        hc
      }
    }
  })
  
  
  output$overrides_by_comment<-renderHighchart({
    validate(need(input$get_or_data,""))
    input$get_or_data
    overrides_by_comment_plot()
  })  
  
  
  ############################################### Report #####################################################
  
  output$FlexD <- downloadHandler(
    filename = function(){
      paste('SPC Report-', Sys.time(), '.html', sep = '')
    },
    content = function(file) {
      params <- list(histogram = chart, normal_dist = FD, saved_control_chart = saved_control_chart, 
                     QQ_Plot = QQ_Plot, Box_chart = Box_chart, pc_plot = pc_plot, interpretation_table = interpretation_table)
      src <- normalizePath('FlexD.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'FlexD.Rmd', overwrite = TRUE)
      out <- rmarkdown::render('FlexD.Rmd', output_format = flexdashboard::flex_dashboard())
      file.rename(out, file)
    }
  )
  
  output$Word <- downloadHandler(
    filename = function(){
      paste('Report-', Sys.time(), '.docx', sep = '')
    },
    content = function(file) {
      params <- list(histogram = chart, normal_dist = FD, saved_control_chart = saved_control_chart, 
                     QQ_Plot = QQ_Plot, Box_chart = Box_chart, pc_plot = pc_plot, interpretation_table = interpretation_table)
      src <- normalizePath('Word.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'Word.Rmd', overwrite = TRUE)
      out <- rmarkdown::render('Word.Rmd', "word_document")
      file.rename(out, file)
    }
  )
}