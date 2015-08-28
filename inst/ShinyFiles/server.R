require(shiny)
require(dplyr)
require(Hmisc)
require(scales)
require(tidyr)


shinyServer(function(input, output, session) {
      
      
      ###############################
      ## Data chooser
      ###############################
      
      # Return the requested dataset
      # Drop-down selection box for which data set
      output$choose_dataset <- renderUI({
            selectInput("dataset", "Choose a dataset to visualize:", 
                choices = names(dfLs))
          })
      
      
      
      ###############################
      ## Choose columns
      ###############################
      
      # Check boxes
      output$choose_columns <- renderUI({
            # If missing input, return to avoid error later in function
            if(is.null(input$dataset))
              return()
            
            # Get the data set with the appropriate name
            dat <- get(input$dataset)
            colnames <- names(dat)
            
            # Create the checkboxes and select them all by default
            checkboxGroupInput("columns", "Choose columns:", 
                choices  = colnames,
                selected = colnames[1:max(10,length(colnames))])
          })
      
      
      
      ###############################
      ## Pick single columns
      ###############################
      
     # Check boxes
      output$pick_a_column <- renderUI({
            # If missing input, return to avoid error later in function
            if(is.null(input$dataset))
              return()
            
            # Get the data set with the appropriate name
            dat <- get(input$dataset)
            
            # Remove any factor or character variables
            dat = dat[, !sapply(dat, is.factor)]
            dat = dat[, !sapply(dat, is.character)]
            
            colnames <- names(dat)
            
            selectInput("selectcol", "Choose a column (numeric only):", 
                choices = colnames)
            
          })
      
      
      
      ###############################
      ## Summary (Hmisc)
      ###############################
      
      ## Generate a summary of the dataset
      output$summary <- renderPrint({
            
            if(is.null(input$dataset))
              return()
            
            # Get the data set with the appropriate name
            dat <- get(input$dataset)
            
            describe(dat)
          })
      
      
      
      ###############################
      ## Table 
      ###############################
      
      # Filter data based on selections
      output$table <- renderDataTable({
            
            if(is.null(input$dataset))
              return()
            
            # Get the data set with the appropriate name
            dat <- get(input$dataset)
            
            if (is.null(input$columns) || !(input$columns %in% names(dat)))
              return()
            
            dat <- dat[, input$columns, drop = FALSE]
            
          })
      
      
      
      
      ###############################
      ## Heatmap plot
      ###############################
      
      heatSavePlot = reactive({
            
            ## Set up a default plot to return 
            defPlot = data.frame(Variable=c("a","b"), obs=1:10, Value=rnorm(10)) %>% mutate(obs=as.factor(obs)) 
            
            if(is.null(input$dataset))
              return(defPlot)
            
            # Get the data set with the appropriate name
            dat <-  get(input$dataset)
            
            if (is.null(input$columns) || !(input$columns %in% names(dat)))
              return(defPlot)
            if(is.null(input$headobs))
              return(defPlot)
            if(is.null(input$dummybox))
              return(defPlot)
            
            
            ## filter out the observations and others
            dat <- dat[, input$columns, drop = FALSE]
              
            datlen = length(dat[,1])
            
            faccols = names(dat[, sapply(dat, is.factor), drop=FALSE])
            
            ## Do dummy codes
            if (input$dummybox & length(faccols)>0){
              
              dummymat = NULL
              for (faccol in faccols){
                form = as.formula(paste("~",faccol, "-1"))
                dmat = as.data.frame(model.matrix(form, data=dat))
                names(dmat) = gsub(faccol, "", names(dmat))
                if(is.null(dummymat)) {
                  dummymat=dmat
                } else {
                  dummymat = cbind(dummymat,dmat)
                }
              }
              
              dat = cbind(dat, dummymat)
            } 
            
            ## Remove the factor columns
            dat = dat[, !sapply(dat, is.factor)]
            dat = as.data.frame(apply(dat, 2, rescale))
            
            if (datlen > input$headobs) {
              if (input$randbox) dat = dat %>% sample_n(input$headobs) %>% data.frame()
              else dat = dat %>% head(input$headobs) %>% data.frame()
            }
            
            dat$obs = factor(length(dat[,1]):1, levels=as.character(length(dat[,1]):1))
            plotDatLong = dat %>% gather(Variable, Value, -obs) %>% filter(!is.na(Value))
            
            plotDatLong$Value=round(plotDatLong$Value, digits=1)
            
            base_size <- 12
            p <- ggplot(plotDatLong, aes(x=Variable, y=obs, fill=Value)) + 
                geom_tile(colour = "white") + 
                scale_fill_gradient(low = "white", high = "steelblue") + 
                theme_gray(base_size = base_size) + 
                labs(x = "",y = "") + 
                scale_x_discrete(expand = c(0, 0)) +
                scale_y_discrete(expand = c(0, 0)) + 
                theme(
#                    legend.position = "top", 
                    axis.ticks = element_blank(), 
                    axis.text.x = element_text(size = base_size , angle = 45, hjust = 1,vjust=1, colour = "grey50"))
          })
      
      
      output$heatplot = renderPlot({
            print(heatSavePlot())
          },  width = 600, height = 700)
      
      
      
      
      ###############################
      ## Distributional plots
      ###############################
      
      
      densDat <- reactive({
            
            ## Set up a default plot to return 
            defPlot = data.frame(Value=1:10, obs=1:10, StdDeviations=0) 
            
            
            if(is.null(input$dataset))
              return(defPlot)
            
            # Get the data set with the appropriate name
            dat <- get(input$dataset)
            
            
            ## Check to make sure the selected column is set 
            if (is.null(input$selectcol) || !(input$selectcol %in% names(dat)))
              return(defPlot)
            ## Check if a sample of observations is set 
            if(is.null(input$distobs))
              return(defPlot)
            
            ## filter out the observations and others
            dat <- dat[, input$selectcol, drop = FALSE]
            names(dat)[1]="Value"
            
            ## get sample of observations
            datlen = length(dat[,1])
            
            if (datlen > input$distobs) {
              dat = dat %>% sample_n(input$distobs) %>% data.frame()
            }
            
            dat$obs = 1:length(dat[,1])
            dat = dat %>% mutate(StdDeviations=abs(scale(Value)[,1]))
          })
      
      
      rawSavePlot = reactive({
            g = ggplot(data=densDat(), aes(x=obs, y=Value, fill=StdDeviations)) +
                geom_point(shape=21, size=4, colour="gray50") +
                scale_fill_gradientn(limits = c(0,4),colours=c("darkblue", "orangered"), na.value = "red",name = "Standard\nDeviations" ) +
                xlab("Observation") +
                ylab("Value") +
                theme_bw()
          })
      
      
      output$rawplot = renderPlot({
            print(rawSavePlot())
          },  width = 675, height = 400)
      
      
      
      histSavePlot = reactive({
            if(is.null(input$binsize))
              return(1)
            
            g = ggplot(densDat(), aes(x=Value)) + 
                geom_histogram(aes(y=..density..), binwidth=input$binsize, color="black", fill="black", alpha=.7) +  
                geom_density(alpha=.2, fill="#FF6666")  +
                ylab("Density") +
                theme_bw()
          })
      
      
      output$histplot = renderPlot({
            print(histSavePlot())
          },  width = 600, height = 400)
      
      
      
      
      
      ###############################
      ## Saving the plots
      ###############################
      
      output$heatsave <- downloadHandler(
          filename = function() { 
            paste0(gsub("\\.","_",input$dataset),"_heatmap.png",sep="") 
          },
          content = function(file) {
            png(file, width=7, height = 8, units="in", res=300)
            print(heatSavePlot())
            dev.off()
          }
      )
      
      
      
      output$rawsave <- downloadHandler(
          
          filename = function() { 
            paste0(gsub("\\.","_",input$dataset),"_",gsub("\\.","_",input$selectcol),"_raw.png",sep="") 
          },
          content = function(file) {
            png(file, width=7, height = 5, units="in", res=300)
            print(rawSavePlot())
            dev.off()
          }
      )
      
      output$histsave <- downloadHandler(
          filename = function() { 
            paste0(gsub("\\.","_",input$dataset),"_",gsub("\\.","_",input$selectcol),"_hist.png",sep="") 
          },
          content = function(file) {
            png(file, width=7, height = 5, units="in", res=300)
            print(histSavePlot())
            dev.off()
          }
      )
      
      
      # Return to R session after closing browser window
      session$onSessionEnded(function() { stopApp() })
      
    })