#server.R

#runGitHub( "shiny-test-repo", "diogoaurelio") 

library(shiny)

#For ggplot2 Graphics Library
library(ggplot2)

#fileUrl <- "http://github.com/diogoaurelio/intro-datascience/blob/master/kaggle-titanic-problem/data/train.csv"

trainData <- read.csv("./data/train.csv")

dataset <- trainData

shinyServer(
	function(input, output) {
		
		# output$oid1 <- renderPrint({ input$id1 })
		# output$oid2 <- renderPrint({ input$id2 })
		# output$odate <- renderPrint({ input$date })

		# output$inputValue <- renderPrint({ input$glucose })
		# output$prediction <- renderPrint({ diabetesRist(input$glucose) })

		# output$newHist <- renderPlot({
		# 	hist(galton$child, xlab='child height', col="lightblue", main="Histogram")
		# 	mu <- input$mu
		# 	lines(c(mu, mu), c(0,200), col="red", lwd=5)
		# 	mse <- mean((galton$child - mu)^2)
		# 	text(63, 150, paste("mu = ", mu))
		# 	text(63, 140, paste("MSE = ", round(mse,2)))
		# 	})


		dataset <- reactive(function() {
			trainData[sample(nrow(trainData), input$sampleSize), ]
			})

		output$plot <- reactivePlot(function() {

			p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
			
			if (input$color != 'None') 
				p <- p + aes_string(color=input$color)

			facets <- paste(input$facet_row, '~', input$facet_col)
			if (facets != '. ~ .')
				p <- p + facet_grid(facets)

			if (input$jitter)
				p <- p + geom_jitter()
			if (input$smooth)
				p <- p + geom_smooth()
			print(p)

			}, height=700)
	}
)