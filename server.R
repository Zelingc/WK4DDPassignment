#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
shinyServer(function(input, output) { 
          mtcars$mpgsp <- ifelse(mtcars$mpg - 20 > 0, mtcars$mpg - 20, 0) 
          model1 <- lm(wt ~ mpg, data = mtcars) 
          model2 <- lm(wt ~ mpgsp + mpg, data = mtcars) 
          model1pred <- reactive({ 
                    mpgInput <- input$sliderMPG 
                    predict(model1, newdata = data.frame(mpg = mpgInput)) 
          }) 
          model2pred <- reactive({ 
                    mpgInput <- input$sliderMPG 
                    predict(model2, newdata =
                                      data.frame(mpg = mpgInput, 
                                                 mpgsp = ifelse(mpgInput - 20 > 0, mpgInput - 20, 0)))
          })
          output$plot1 <- renderPlot({ 
                    mpgInput <- input$sliderMPG 
                    plot(mtcars$mpg, mtcars$wt, xlab = "Miles Per Gallon", ylab = "Car Weight", bty = "n", pch = 16, 
                         xlim = c(10, 35), ylim = c(1, 6))
                    if(input$showModel1){ abline(model1, col = "yellow", lwd = 2) 
                    }
                    if(input$showModel2){ 
                              model2lines <- predict(model2, newdata = data.frame( 
                                        mpg = 10:35, mpgsp = ifelse(10:35 - 20 > 0, 10:35 - 20, 0)
                              )) 
                              lines(10:35, model2lines, col = "green", lwd = 2)
                    }
                    legend(5, 100, c("Model 1 Prediction", "Model 2 Prediction"), pch = 16, 
                           col = c("yellow", "green"), bty = "n", cex = 1.2) 
                    points(mpgInput, model1pred(), col = "yellow", pch = 16, cex = 2) 
                    points(mpgInput, model2pred(), col = "green", pch = 16, cex = 2)
          })
          output$pred1 <- renderText({ 
                    model1pred() }) 
          output$pred2 <- renderText({ 
                    model2pred()
          }) 
}) 