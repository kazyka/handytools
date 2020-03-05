#' Produces a plot of the color palette
#'
#' @param df Dataframe of dimension 10 x 6
#' @return Plot
#' @export

# devtools::check() for check
# devtools::document() for building documentation
# devtools::build() to build


plot_colors <- function(df) {

    # color_df <- df
    plot <- rect <- NULL

    plot(0,0, xlim = c(0, 1), ylim = c(0, 0.6), axes = F, xlab = "", ylab = "" )

    row <- 6

    for (j in seq(0.1,1.0,0.1)) {

        #current_hue <- hue_all[[j*10]]

        current_hue <- df[j*10, ]

        for (i in sort(rep((1:(row)/10), 1), decreasing=F) ) {


            rect( j-0.1 ,
                  sort(rep((0:(row - 1)/10), 1)[], decreasing=F)[i*10],
                  j,
                  sort(rep((1:(row)/10), 1), decreasing=F)[i*10],
                  border = "light gray" ,
                  col=as.character(c(current_hue[i*10])))
            #print(i*10)
        }
    }
}

