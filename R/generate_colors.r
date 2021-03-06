#' Produces a plot of the color palette
#'
#' @param empty
#' @return NULL
#'
#'
# devtools::check() for check
# devtools::document() for building documentation
# devtools::build() to build

generate_colors <- function(){
    col <- 10
    row <- 6

    rgb <- setNames <- plot <- rect <- NULL

    hue_blue_1 <- rgb(148/255, 182/255, 210/255, 1)
    hue_blue_2 <- rgb(234/255, 240/255, 246/255, 1)
    hue_blue_3 <- rgb(212/255, 226/255, 237/255, 1)
    hue_blue_4 <- rgb(191/255, 211/255, 228/255, 1)
    hue_blue_5 <- rgb(85/255, 139/255, 184/255, 1)
    hue_blue_6 <- rgb(53/255, 93/255, 126/255, 1)
    hue_blues <- c(hue_blue_1, hue_blue_2, hue_blue_3, hue_blue_4, hue_blue_5, hue_blue_6)

    hue_sand_1 <- rgb(165/255, 171/255, 129/255, 1)
    hue_sand_2 <- rgb(237/255, 238/255, 230/255, 1)
    hue_sand_3 <- rgb(219/255, 221/255, 205/255, 1)
    hue_sand_4 <- rgb(201/255, 205/255, 179/255, 1)
    hue_sand_5 <- rgb(129/255, 135/255, 90/255, 1)
    hue_sand_6 <- rgb(86/255, 90/255, 60/255, 1)
    hue_sands <- c(hue_sand_1, hue_sand_2, hue_sand_3, hue_sand_4, hue_sand_5, hue_sand_6)

    hue_red_1 <- rgb(221/255, 128/255, 71/255, 1)
    hue_red_2 <- rgb(248/255, 230/255, 218/255, 1)
    hue_red_3 <- rgb(241/255, 204/255, 181/255, 1)
    hue_red_4 <- rgb(235/255, 179/255, 145/255, 1)
    hue_red_5 <- rgb(185/255, 91/255, 34/255, 1)
    hue_red_6 <- rgb(123/255, 61/255, 23/255, 1)
    hue_reds <- c(hue_red_1, hue_red_2, hue_red_3, hue_red_4, hue_red_5, hue_red_6)

    hue_orange_1 <- rgb(216/255, 178/255, 92/255, 1)
    hue_orange_2 <- rgb(247/255, 240/255, 222/255, 1)
    hue_orange_3 <- rgb(239/255, 224/255, 190/255, 1)
    hue_orange_4 <- rgb(232/255, 209/255, 157/255, 1)
    hue_orange_5 <- rgb(186/255, 143/255, 45/255, 1)
    hue_orange_6 <- rgb(124/255, 95/255, 30/255, 1)
    hue_oranges <- c(hue_orange_1, hue_orange_2, hue_orange_3, hue_orange_4, hue_orange_5, hue_orange_6)

    hue_teal_1 <- rgb(123/255, 167/255, 157/255, 1)
    hue_teal_2 <- rgb(229/255, 237/255, 235/255, 1)
    hue_teal_3 <- rgb(202/255, 220/255, 216/255, 1)
    hue_teal_4 <- rgb(176/255, 202/255, 196/255, 1)
    hue_teal_5 <- rgb(87/255, 130/255, 121/255, 1)
    hue_teal_6 <- rgb(58/255, 87/255, 80/255, 1)
    hue_teals <- c(hue_teal_1, hue_teal_2, hue_teal_3, hue_teal_4, hue_teal_5, hue_teal_6)

    hue_dgray_1 <- rgb(119/255, 95/255, 85/255, 1)
    hue_dgray_2 <- rgb(229/255, 222/255, 219/255, 1)
    hue_dgray_3 <- rgb(204/255, 190/255, 184/255, 1)
    hue_dgray_4 <- rgb(178/255, 157/255, 148/255, 1)
    hue_dgray_5 <- rgb(89/255, 71/255, 64/255, 1)
    hue_dgray_6 <- rgb(60/255, 48/255, 42/255, 1)
    hue_dgrays <- c(hue_dgray_1, hue_dgray_2, hue_dgray_3, hue_dgray_4, hue_dgray_5, hue_dgray_6)

    hue_dyellow_1 <- rgb(235/255, 221/255, 195/255, 1)
    hue_dyellow_2 <- rgb(224/255, 203/255, 163/255, 1)
    hue_dyellow_3 <- rgb(208/255, 175/255, 114/255, 1)
    hue_dyellow_4 <- rgb(161/255, 124/255, 54/255, 1)
    hue_dyellow_5 <- rgb(81/255, 62/255, 27/255, 1)
    hue_dyellow_6 <- rgb(32/255, 25/255, 11/255, 1)
    hue_dyellows <- c(hue_dyellow_1, hue_dyellow_2, hue_dyellow_3, hue_dyellow_4, hue_dyellow_5, hue_dyellow_6)

    hue_gray_1 <- rgb(150/255, 140/255, 140/255, 1)
    hue_gray_2 <- rgb(234/255, 232/255, 232/255, 1)
    hue_gray_3 <- rgb(213/255, 209/255, 209/255, 1)
    hue_gray_4 <- rgb(192/255, 186/255, 186/255, 1)
    hue_gray_5 <- rgb(114/255, 104/255, 104/255, 1)
    hue_gray_6 <- rgb(76/255, 69/255, 69/255, 1)
    hue_grays <- c(hue_gray_1, hue_gray_2, hue_gray_3, hue_gray_4, hue_gray_5, hue_gray_6)

    hue_lgray_1 <- rgb(255/255, 255/255, 255/255, 1)
    hue_lgray_2 <- rgb(242/255, 242/255, 242/255, 1)
    hue_lgray_3 <- rgb(217/255, 217/255, 217/255, 1)
    hue_lgray_4 <- rgb(191/255, 191/255, 191/255, 1)
    hue_lgray_5 <- rgb(116/255, 116/255, 116/255, 1)
    hue_lgray_6 <- rgb(127/255, 127/255, 127/255, 1)
    hue_lgrays <- c(hue_lgray_1, hue_lgray_2, hue_lgray_3, hue_lgray_4, hue_lgray_5, hue_lgray_6)

    hue_black_1 <- rgb(0, 0, 0, 1)
    hue_black_2 <- rgb(127/255, 127/255, 127/255, 1)
    hue_black_3 <- rgb(89/255, 89/255, 89/255, 1)
    hue_black_4 <- rgb(64/255, 64/255, 64/255, 1)
    hue_black_5 <- rgb(38/255, 38/255, 38/255, 1)
    hue_black_6 <- rgb(13/255, 13/255, 13/255, 1)
    hue_blacks <- c(hue_black_1, hue_black_2, hue_black_3, hue_black_4, hue_black_5, hue_black_6)


    hue_all <- list(hue_blues, hue_sands, hue_reds, hue_oranges, hue_teals,
                    hue_dgrays, hue_dyellows, hue_grays, hue_lgrays, hue_blacks)

    hue_all_n <- c("hue_blues", "hue_sands", "hue_reds", "hue_oranges", "hue_teals",
                    "hue_dgrays", "hue_dyellows", "hue_grays", "hue_lgrays", "hue_blacks")

    color_options <- setNames(data.frame(matrix(ncol = 6, nrow = 1), stringsAsFactors=FALSE), c("hue_1", "hue_2", "hue_3",
                                                                        "hue_4", "hue_5", "hue_6"))
    for (i in 1:length(hue_all)) {
        color_options[i, ] <- t(hue_all[[i]])
    }

    rownames(color_options) <- hue_all_n


    #save(color_options, file = "R/color_options.rda")
    save(color_options, file = "data/color_options.rda")
}
