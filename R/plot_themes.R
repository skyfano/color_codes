## plot_themes.R | unikn
## spds | uni.kn |  2022 11 29
## ---------------------------

## Defining default themes for plotting.

## (A) Themes for base R plotting: -------

## ToDo.


## (B) ggplot2 themes: -------

## Check if ggplot2 is available:

# if (!require(ggplot2)) {
#
#   install.packages('ggplot2')   # installs ggplot2
#   library('ggplot2')            # loads ggplot2
#
# }

## Check if ggplot2 is loaded:

# if ("ggplot2" %in% (.packages())){
#
#   message("Package 'ggplot2' is loaded.")
#
# } # if ("ggplot2" %in% (.packages())) end.



# theme_unikn: Default theme for ggplot2: ------

# - Documentation: ----

#' Basic unikn theme (for ggplot2)
#'
#' \code{theme_unikn} provides a basic \bold{unikn} theme
#' to use in \bold{ggplot2} commands.
#'
#' The theme is lightweight and no-nonsense, but somewhat
#' opinionated (e.g., in using mostly grey scales to
#' allow emphasizing data points with color accents).
#'
#' @param col_title Color of title (text) elements (optional, numeric).
#' Default: \code{col_title = pal_seeblau[[4]]}.
#' Consider using \code{col_title = "black"} when data uses
#' \code{Seeblau} colors.
#'
#' @param base_size Base font size (optional, numeric).
#' Default: \code{base_size = 11}.
#'
#' @param base_family Base font family (optional, character).
#' Default: \code{base_family = ""}.
#' Options include \code{"mono"}, \code{"sans"} (default), and "serif".
#'
#' @param base_line_size Base line size (optional, numeric).
#' Default: \code{base_line_size = base_size/22}.
#'
#' @param base_rect_size Base rectangle size (optional, numeric).
#' Default: \code{base_rect_size = base_size/22}.
#'
#' @examples
#'
#' \donttest{
#'   # Plotting iris dataset (using ggplot2, theme_unikn, and unikn colors):
#'
#'   library('ggplot2')  # theme_unikn requires ggplot2
#'
#'   ggplot(datasets::iris) +
#'     geom_jitter(aes(x = Petal.Length, y = Petal.Width, color = Species), size = 3, alpha = 2/3) +
#'     scale_color_manual(values = usecol(pal = c(Pinky, Seeblau, Seegruen))) +
#'     labs(tag = "A", title = "Iris petals",
#'          caption = "Data from datasets::iris") +
#'     theme_unikn()
#' }
#'
#' @family plot functions
#'
#' @seealso
#' \code{\link{theme_grau}} for an alternative theme.
#'
#' @import ggplot2
#'
#' @export

# - Definition: ----

theme_unikn <- function(col_title = pal_seeblau[[4]],
                        # "black"
                        base_size = 11,
                        base_family = "",
                        base_line_size = base_size / 22,
                        base_rect_size = base_size / 22) {
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(
      #
      # titles:
      title = ggplot2::element_text(
        color = col_title,
        face = "bold",
        margin = ggplot2::margin(
          t = 10,
          r = 4,
          b = 4,
          l = 4,
          unit = "pt"
        )
      ),
      plot.subtitle = ggplot2::element_text(
        color = grey(.10, 1),
        face = "plain",
        hjust = 0,
        margin = ggplot2::margin(
          t = 2,
          r = 4,
          b = 8,
          l = 4,
          unit = "pt"
        )
      ),
      plot.caption = ggplot2::element_text(
        color =  grey(.20, 1),
        face = "plain",
        size = ggplot2::rel(.80),
        hjust = 1
      ),
      # axes:
      axis.line =  ggplot2::element_line(color = "black", size = ggplot2::rel(1)),
      axis.ticks = ggplot2::element_line(color = "black", size = ggplot2::rel(.90)),
      axis.title = ggplot2::element_text(color = grey(.10, 1)),
      axis.text =  ggplot2::element_text(color = grey(.20, 1), size = ggplot2::rel(.90)),
      # legend:
      legend.title = ggplot2::element_text(color = grey(.10, 1)),
      legend.text = ggplot2::element_text(color = grey(.20, 1), size = ggplot2::rel(.90)),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      # strip:
      # strip.background = ggplot2::element_rect(fill = pal_seeblau[[1]], color = pal_seeblau[[5]], size = ggplot2::rel(5/3)),
      strip.background = ggplot2::element_rect(
        fill = grey(.95, 1),
        color = grey(.05, 1),
        # light grey strip background
        size = ggplot2::rel(.90)
      ),
      strip.text = ggplot2::element_text(
        color = grey(0, 1),
        size = ggplot2::rel(1.0),
        margin = ggplot2::margin(
          t = 4,
          r = 4,
          b = 4,
          l = 4,
          unit = "pt"
        )
      ),
      # panel:
      # panel.border = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        fill = "transparent",
        color = grey(.05, 1),
        linetype = "solid",
        size = ggplot2::rel(.80)
      ),
      # panel.grid = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        color = grey(.75, 1),
        linetype = "solid",
        size = ggplot2::rel(.50)
      ),
      # panel.grid.minor = ggplot2::element_line(color = grey(.75, 1), linetype = "solid", size = ggplot2::rel(.40)), # "dotted"
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = grey(1, 1), color = NA),
      # "white" panel background
      # background:
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      complete = TRUE
    )
  
} # theme_unikn().



# theme_grau: Alternative theme for ggplot2: ------

# - Documentation: ----

#' Alternative theme (for ggplot2)
#'
#' \code{theme_grau} provides an alternative \bold{unikn} theme
#' to use in \bold{ggplot2} commands.
#'
#' \code{theme_grau} is no-nonsense, but fills
#' panel backgrounds in "grau" (specifically, pal_seeggrau[[1]]).
#'
#' This theme works well for dark colors and bright color accents,
#' but is of limited use with transparent colors.
#'
#' @param col_title Color of title (text) elements (optional, numeric).
#' Default: \code{col_title = grey(0, 1)} (i.e., "black").
#' Consider using \code{col_title = unikn::pal_seeblau[[4]]}.
#'
#' @param base_size Base font size (optional, numeric).
#' Default: \code{base_size = 11}.
#'
#' @param base_family Base font family (optional, character).
#' Default: \code{base_family = ""}.
#' Options include \code{"mono"}, \code{"sans"} (default), and "serif".
#'
#' @param base_line_size Base line size (optional, numeric).
#' Default: \code{base_line_size = base_size/22}.
#'
#' @param base_rect_size Base rectangle size (optional, numeric).
#' Default: \code{base_rect_size = base_size/22}.
#'
#' @examples
#'
#' \donttest{
#' # Plotting iris dataset (using ggplot2, theme_grau, and unikn colors):
#'
#' library('ggplot2')  # theme_unikn requires ggplot2
#'
#' ggplot(datasets::iris) +
#'   geom_jitter(aes(x = Sepal.Length, y = Sepal.Width, color = Species), size = 3, alpha = 2/3) +
#'   facet_wrap(~Species) +
#'   scale_color_manual(values = usecol(pal = c(Pinky, Seeblau, Seegruen))) +
#'   labs(tag = "B",
#'        title = "Iris sepals",
#'        caption = "Data from datasets::iris") +
#'   coord_fixed(ratio = 3/2) +
#'   theme_grau()
#'
#' }
#'
#' @family plot functions
#'
#' @seealso
#' \code{\link{theme_unikn}} for default theme.
#'
#' @import ggplot2
#'
#' @export

# - Definition: ----

theme_grau <- function(col_title = grey(0, 1),
                       # OR: "black"
                       base_size = 11,
                       base_family = "",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22) {
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(
      #
      # titles:
      title = ggplot2::element_text(
        color = col_title,
        face = "bold",
        margin = ggplot2::margin(
          t = 10,
          r = 4,
          b = 4,
          l = 4,
          unit = "pt"
        )
      ),
      plot.subtitle = ggplot2::element_text(
        color = grey(.10, 1),
        face = "plain",
        hjust = 0,
        margin = ggplot2::margin(
          t = 2,
          r = 4,
          b = 8,
          l = 4,
          unit = "pt"
        )
      ),
      plot.caption = ggplot2::element_text(
        color =  grey(.20, 1),
        face = "plain",
        size = ggplot2::rel(.80),
        hjust = 1
      ),
      # axes:
      axis.line =  ggplot2::element_line(color = "black", size = ggplot2::rel(1)),
      axis.ticks = ggplot2::element_line(color = "black", size = ggplot2::rel(.90)),
      axis.title = ggplot2::element_text(color = grey(.10, 1)),
      axis.text =  ggplot2::element_text(color = grey(.20, 1), size = ggplot2::rel(.90)),
      # legend:
      legend.title = ggplot2::element_text(color = pal_grau[[5]]),
      legend.text = ggplot2::element_text(color = grey(0, 1), size = ggplot2::rel(.90)),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      # strip:
      # strip.background = ggplot2::element_blank(),
      # strip.background = ggplot2::element_rect(fill = pal_seeblau[[1]], color = pal_seeblau[[5]], size = ggplot2::rel(5/3)),
      # strip.background = ggplot2::element_rect(fill = grey(.90, 1), color = grey(.90, 1), size = ggplot2::rel(6/3)),
      strip.background = ggplot2::element_rect(
        fill = "transparent",
        color = NA,
        size = ggplot2::rel(1.0)
      ),
      # transparent strip
      strip.text = ggplot2::element_text(
        color = grey(0, 1),
        size = ggplot2::rel(1.0),
        margin = ggplot2::margin(
          t = 5,
          r = 5,
          b = 5,
          l = 5,
          unit = "pt"
        )
      ),
      # larger than in theme_unikn()
      # panel:
      panel.border = ggplot2::element_blank(),
      # panel.border = ggplot2::element_rect(fill = "transparent", color = grey(.10, 1), linetype = "solid", size = ggplot2::rel(2/3)),
      # panel.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = pal_grau[[1]], color = pal_grau[[1]]),
      # light "grau" panel background
      # panel.grid = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        color = grey(1, 1),
        linetype = "solid",
        size = ggplot2::rel(.90)
      ),
      # "white" lines
      panel.grid.minor = ggplot2::element_blank(),
      # panel.grid.minor = ggplot2::element_line(color = grey(.95, 1), linetype = "solid", size = ggplot2::rel(2/3)),
      # background:
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      complete = TRUE
    )
  
} # theme_grau().



# theme_tueb_1: Default theme for ggplot2: ------

# - Documentation: ----

#' Primary theme for University of Tuebingen colours (for ggplot2)
#'
#' \code{theme_tueb_1} provides a basic \bold{colorCodes} theme
#' to use in \bold{ggplot2} commands.
#'
#' The theme is lightweight and no-nonsense, but somewhat
#' opinionated (e.g., in using mostly grey scales to
#' allow emphasizing data points with color accents).
#' 
#' @param binary A boolean.
#'   * `TRUE` (the default): discrete case.
#'   * `FALSE`: continuous case.
#'
#' @param col_title Color of title (text) elements (optional, numeric).
#' Default: \code{col_title = uni_tuebingen_1[[1]]}.
#'
#' @param base_size Base font size (optional, numeric).
#' Default: \code{base_size = 11}.
#'
#' @param base_family Base font family (optional, character).
#' Default: \code{base_family = ""}.
#' Options include \code{"mono"}, \code{"sans"} (default), and "serif".
#'
#' @param base_line_size Base line size (optional, numeric).
#' Default: \code{base_line_size = base_size/22}.
#'
#' @param base_rect_size Base rectangle size (optional, numeric).
#' Default: \code{base_rect_size = base_size/22}.
#'
#' @examples
#'
#' \donttest{
#'   # Plotting iris dataset (using ggplot2, theme_tueb_1, and unikn colors):
#'
#'   library('ggplot2')  # theme_tueb_1 requires ggplot2
#'
#'   ggplot(datasets::iris) +
#'     geom_jitter(aes(x = Petal.Length, y = Petal.Width, color = Species), size = 3, alpha = 2/3) +
#'     scale_color_manual(values = usecol(c(uni_tuebingen_1[1], uni_tuebingen_1[2], uni_tuebingen_1[3]))) +
#'     labs(tag = "A", title = "Iris petals",
#'          caption = "Data from datasets::iris") +
#'     theme_tueb_1()
#' }
#'
#' @family plot functions
#'
#' @seealso
#' \code{\link{theme_tueb_2}} for an alternative theme.
#'
#' @import ggplot2
#'
#' @export

# - Definition: ----

theme_tueb_1 <- function(binary = TRUE,
                         col_title = uni_tuebingen_1[[3]],
                         # "grey"
                         base_size = 11,
                         base_family = "",
                         base_line_size = base_size / 22,
                         base_rect_size = base_size / 22) {
  list(
    ggplot2::theme_bw(
      base_size = base_size,
      base_family = base_family,
      base_line_size = base_line_size,
      base_rect_size = base_rect_size
    ) %+replace%
      ggplot2::theme(
        title = ggplot2::element_text(
          color = col_title,
          face = "bold",
          margin = ggplot2::margin(
            t = 10,
            r = 4,
            b = 4,
            l = 4,
            unit = "pt"
          )
        ),
        plot.subtitle = ggplot2::element_text(
          color = grey(.10, 1),
          face = "plain",
          hjust = 0,
          margin = ggplot2::margin(
            t = 2,
            r = 4,
            b = 8,
            l = 4,
            unit = "pt"
          )
        ),
        plot.caption = ggplot2::element_text(
          color =  grey(.20, 1),
          face = "plain",
          size = ggplot2::rel(.80),
          hjust = 1
        ),
        # axes:
        axis.line =  ggplot2::element_line(color = "black", size = ggplot2::rel(1)),
        axis.ticks = ggplot2::element_line(color = "black", size = ggplot2::rel(.90)),
        axis.title = ggplot2::element_text(color = grey(.10, 1)),
        axis.text =  ggplot2::element_text(color = grey(.20, 1), size = ggplot2::rel(.90)),
        # legend:
        legend.title = ggplot2::element_text(color = grey(.10, 1)),
        legend.text = ggplot2::element_text(color = grey(.20, 1), size = ggplot2::rel(.90)),
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        # strip:
        # strip.background = ggplot2::element_rect(fill = pal_seeblau[[1]], color = pal_seeblau[[5]], size = ggplot2::rel(5/3)),
        strip.background = ggplot2::element_rect(
          fill = grey(.95, 1),
          color = grey(.05, 1),
          # light grey strip background
          size = ggplot2::rel(.90)
        ),
        strip.text = ggplot2::element_text(
          color = grey(0, 1),
          size = ggplot2::rel(1.0),
          margin = ggplot2::margin(
            t = 4,
            r = 4,
            b = 4,
            l = 4,
            unit = "pt"
          )
        ),
        # panel:
        # panel.border = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(
          fill = "transparent",
          color = grey(.05, 1),
          linetype = "solid",
          size = ggplot2::rel(.80)
        ),
        # panel.grid = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(
          color = grey(.75, 1),
          linetype = "solid",
          size = ggplot2::rel(.50)
        ),
        # panel.grid.minor = ggplot2::element_line(color = grey(.75, 1), linetype = "solid", size = ggplot2::rel(.40)), # "dotted"
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = grey(1, 1), color = NA),
        # "white" panel background
        # background:
        plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
        complete = TRUE
      ),
    
    if (binary == TRUE) {
      scale_color_manual(values = usecol(c(
        uni_tuebingen_1[1], uni_tuebingen_1[2], uni_tuebingen_1[3]
      )))
    } else {
      scale_color_gradientn(colors = usecol(c(
        uni_tuebingen_1[1], uni_tuebingen_1[2], uni_tuebingen_1[3]
      )))
    }
    
  )
}

# theme_tueb_2: Alternative theme for ggplot2: ------

# - Documentation: ----

#' Alternative theme (for ggplot2)
#'
#' \code{theme_tueb_2} provides an alternative \bold{Uni Tuebingen} theme
#' to use in \bold{ggplot2} commands.
#'
#' \code{theme_tueb_2} is no-nonsense, but fills
#' panel backgrounds in "grau" (specifically, pal_seeggrau[[1]]).
#'
#' This theme works well for dark colors and bright color accents,
#' but is of limited use with transparent colors.
#' 
#' @param binary A boolean.
#'   * `TRUE` (the default): discrete case.
#'   * `FALSE`: continuous case.
#'
#' @param col_title Color of title (text) elements (optional, numeric).
#' Default: \code{col_title = grey(0, 1)} (i.e., "black").
#'
#' @param base_size Base font size (optional, numeric).
#' Default: \code{base_size = 11}.
#'
#' @param base_family Base font family (optional, character).
#' Default: \code{base_family = ""}.
#' Options include \code{"mono"}, \code{"sans"} (default), and "serif".
#'
#' @param base_line_size Base line size (optional, numeric).
#' Default: \code{base_line_size = base_size/22}.
#'
#' @param base_rect_size Base rectangle size (optional, numeric).
#' Default: \code{base_rect_size = base_size/22}.
#'
#' @examples
#'
#' \donttest{
#'   # Plotting iris dataset (using ggplot2, theme_tueb_1, and unikn colors):
#'
#'   library('ggplot2')  # theme_tueb_1 requires ggplot2
#'
#'     n <- 12
#'     groups <- 1:n
#'     df <- data.frame()
#'     set.seed(3)
#'
#'     for (i in seq(1:30)){
#'         data = data.frame(matrix(0, n, 3))
#'         data[, 1] <- i
#'         data[, 2] <- sample(groups, nrow(data))
#'         data[, 3] <- prop.table(sample(c(rep(0, 100), c(1:n)), nrow(data)))
#'         df = rbind(df, data)}
#'
#'     names(df) <- c("X","Group","Y")
#'     df$Group <- as.factor(df$Group)
#'
#'     df <- df[c(2, 1, 3)]
#'     df <- df[order(df$X, df$Group) , ]
#'     rownames(df) <- NULL
#'     my_data <- df
#'
#'     ggplot(my_data, aes(x = X, y = Y, fill = Group)) +
#'         geom_area() +
#'         theme_tueb_2() +
#'         theme(legend.position = "none")
#' }
#'
#' @family plot functions
#'
#' @seealso
#' \code{\link{theme_tueb_1}} for an alternative theme.
#'
#' @import ggplot2
#'
#' @export


# - Definition: ----

theme_tueb_2 <- function(binary = TRUE,
                         col_title = uni_tuebingen_2[[1]],
                         base_size = 11,
                         base_family = "",
                         base_line_size = base_size / 22,
                         base_rect_size = base_size / 22) {
  list(
    ggplot2::theme_bw(
      base_size = base_size,
      base_family = base_family,
      base_line_size = base_line_size,
      base_rect_size = base_rect_size
    ) %+replace%
      ggplot2::theme(
        title = ggplot2::element_text(
          color = col_title,
          face = "bold",
          margin = ggplot2::margin(
            t = 10,
            r = 4,
            b = 4,
            l = 4,
            unit = "pt"
          )
        ),
        plot.subtitle = ggplot2::element_text(
          color = grey(.10, 1),
          face = "plain",
          hjust = 0,
          margin = ggplot2::margin(
            t = 2,
            r = 4,
            b = 8,
            l = 4,
            unit = "pt"
          )
        ),
        plot.caption = ggplot2::element_text(
          color =  grey(.20, 1),
          face = "plain",
          size = ggplot2::rel(.80),
          hjust = 1
        ),
        # axes:
        axis.line =  ggplot2::element_line(color = "black", size = ggplot2::rel(1)),
        axis.ticks = ggplot2::element_line(color = "black", size = ggplot2::rel(.90)),
        axis.title = ggplot2::element_text(color = grey(.10, 1)),
        axis.text =  ggplot2::element_text(color = grey(.20, 1), size = ggplot2::rel(.90)),
        # legend:
        legend.title = ggplot2::element_text(color = grey(.10, 1)),
        legend.text = ggplot2::element_text(color = grey(.20, 1), size = ggplot2::rel(.90)),
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        # strip:
        # strip.background = ggplot2::element_rect(fill = pal_seeblau[[1]], color = pal_seeblau[[5]], size = ggplot2::rel(5/3)),
        strip.background = ggplot2::element_rect(
          fill = grey(.95, 1),
          color = grey(.05, 1),
          # light grey strip background
          size = ggplot2::rel(.90)
        ),
        strip.text = ggplot2::element_text(
          color = grey(0, 1),
          size = ggplot2::rel(1.0),
          margin = ggplot2::margin(
            t = 4,
            r = 4,
            b = 4,
            l = 4,
            unit = "pt"
          )
        ),
        # panel:
        # panel.border = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(
          fill = "transparent",
          color = grey(.05, 1),
          linetype = "solid",
          size = ggplot2::rel(.80)
        ),
        # panel.grid = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(
          color = grey(.75, 1),
          linetype = "solid",
          size = ggplot2::rel(.50)
        ),
        # panel.grid.minor = ggplot2::element_line(color = grey(.75, 1), linetype = "solid", size = ggplot2::rel(.40)), # "dotted"
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = grey(1, 1), color = NA),
        # "white" panel background
        # background:
        plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
        complete = TRUE
      ),
    if (binary == TRUE) {
      scale_fill_manual(values = usecol(
        c(
          uni_tuebingen_2[1],
          uni_tuebingen_2[2],
          uni_tuebingen_2[3],
          uni_tuebingen_2[4],
          uni_tuebingen_2[5],
          uni_tuebingen_2[6],
          uni_tuebingen_2[7],
          uni_tuebingen_2[8],
          uni_tuebingen_2[9],
          uni_tuebingen_2[10],
          uni_tuebingen_2[11],
          uni_tuebingen_2[12]
        )
      ))
    } else {
      scale_color_gradientn(colors = usecol(
        c(
          uni_tuebingen_2[1],
          uni_tuebingen_2[2],
          uni_tuebingen_2[3],
          uni_tuebingen_2[4],
          uni_tuebingen_2[5],
          uni_tuebingen_2[6],
          uni_tuebingen_2[7],
          uni_tuebingen_2[8],
          uni_tuebingen_2[9],
          uni_tuebingen_2[10],
          uni_tuebingen_2[11],
          uni_tuebingen_2[12]
        )
      ))
    }
    
    
  )
  
} # theme_tueb_2().

###################################################################################################

# theme_tueb_1_disc: Discrete version for plotting: ------

# - Documentation: ----

#' Primary theme for University of Tuebingen colours (for ggplot2)
#'
#' \code{theme_tueb_1_disc} provides a basic \bold{colorCodes} theme
#' to use in \bold{ggplot2} commands.
#'
#' The theme is lightweight and no-nonsense, but somewhat
#' opinionated (e.g., in using mostly grey scales to
#' allow emphasizing data points with color accents).
#'
#' @param col_title Color of title (text) elements (optional, numeric).
#' Default: \code{col_title = uni_tuebingen_1[[1]]}.
#'
#' @param base_size Base font size (optional, numeric).
#' Default: \code{base_size = 11}.
#'
#' @param base_family Base font family (optional, character).
#' Default: \code{base_family = ""}.
#' Options include \code{"mono"}, \code{"sans"} (default), and "serif".
#'
#' @param base_line_size Base line size (optional, numeric).
#' Default: \code{base_line_size = base_size/22}.
#'
#' @param base_rect_size Base rectangle size (optional, numeric).
#' Default: \code{base_rect_size = base_size/22}.
#'
#' @examples
#'
#' \donttest{
#'   # Plotting iris dataset (using ggplot2, theme_tueb_1, and unikn colors):
#'
#'   library('ggplot2')  # theme_tueb_1 requires ggplot2
#'
#'   ggplot(datasets::iris) +
#'     geom_jitter(aes(x = Petal.Length, y = Petal.Width, color = Species), size = 3, alpha = 2/3) +
#'     labs(tag = "A", title = "Iris petals",
#'          caption = "Data from datasets::iris") +
#'     theme_tueb_1_disc()
#' }
#'
#' @family plot functions
#'
#' @seealso
#' \code{\link{theme_tueb_1_cont}} for an alternative theme.
#'
#' @import ggplot2
#'
#' @export

# - Definition: ----

theme_tueb_1_disc <- function(col_title = uni_tuebingen_1[[3]],
                              base_size = 11,
                              base_family = "",
                              base_line_size = base_size / 22,
                              base_rect_size = base_size / 22) {
  list(
    ggplot2::theme_bw(
      base_size = base_size,
      base_family = base_family,
      base_line_size = base_line_size,
      base_rect_size = base_rect_size
    ) %+replace%
      ggplot2::theme(
        title = ggplot2::element_text(
          color = col_title,
          face = "bold",
          margin = ggplot2::margin(
            t = 10,
            r = 4,
            b = 4,
            l = 4,
            unit = "pt"
          )
        ),
        plot.subtitle = ggplot2::element_text(
          color = grey(.10, 1),
          face = "plain",
          hjust = 0,
          margin = ggplot2::margin(
            t = 2,
            r = 4,
            b = 8,
            l = 4,
            unit = "pt"
          )
        ),
        plot.caption = ggplot2::element_text(
          color =  grey(.20, 1),
          face = "plain",
          size = ggplot2::rel(.80),
          hjust = 1
        ),
        # axes:
        axis.line =  ggplot2::element_line(color = "black", size = ggplot2::rel(1)),
        axis.ticks = ggplot2::element_line(color = "black", size = ggplot2::rel(.90)),
        axis.title = ggplot2::element_text(color = grey(.10, 1)),
        axis.text =  ggplot2::element_text(color = grey(.20, 1), size = ggplot2::rel(.90)),
        # legend:
        legend.title = ggplot2::element_text(color = grey(.10, 1)),
        legend.text = ggplot2::element_text(color = grey(.20, 1), size = ggplot2::rel(.90)),
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        # strip:
        # strip.background = ggplot2::element_rect(fill = pal_seeblau[[1]], color = pal_seeblau[[5]], size = ggplot2::rel(5/3)),
        strip.background = ggplot2::element_rect(
          fill = grey(.95, 1),
          color = grey(.05, 1),
          # light grey strip background
          size = ggplot2::rel(.90)
        ),
        strip.text = ggplot2::element_text(
          color = grey(0, 1),
          size = ggplot2::rel(1.0),
          margin = ggplot2::margin(
            t = 4,
            r = 4,
            b = 4,
            l = 4,
            unit = "pt"
          )
        ),
        # panel:
        # panel.border = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(
          fill = "transparent",
          color = grey(.05, 1),
          linetype = "solid",
          size = ggplot2::rel(.80)
        ),
        # panel.grid = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(
          color = grey(.75, 1),
          linetype = "solid",
          size = ggplot2::rel(.50)
        ),
        # panel.grid.minor = ggplot2::element_line(color = grey(.75, 1), linetype = "solid", size = ggplot2::rel(.40)), # "dotted"
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = grey(1, 1), color = NA),
        # "white" panel background
        # background:
        plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
        complete = TRUE
      ),
    scale_color_manual(values = usecol(
      c(uni_tuebingen_1[1], uni_tuebingen_1[2], uni_tuebingen_1[3])
    ))
  )
}

###################################################################################################

# theme_tueb_1_cont: Continous version for plotting: ------

# - Documentation: ----

#' Primary theme for University of Tuebingen colours (for ggplot2)
#'
#' \code{theme_tueb_1_disc} provides a basic \bold{colorCodes} theme
#' to use in \bold{ggplot2} commands.
#'
#' The theme is lightweight and no-nonsense, but somewhat
#' opinionated (e.g., in using mostly grey scales to
#' allow emphasizing data points with color accents).
#'
#' @param col_title Color of title (text) elements (optional, numeric).
#' Default: \code{col_title = uni_tuebingen_1[[1]]}.
#'
#' @param base_size Base font size (optional, numeric).
#' Default: \code{base_size = 11}.
#'
#' @param base_family Base font family (optional, character).
#' Default: \code{base_family = ""}.
#' Options include \code{"mono"}, \code{"sans"} (default), and "serif".
#'
#' @param base_line_size Base line size (optional, numeric).
#' Default: \code{base_line_size = base_size/22}.
#'
#' @param base_rect_size Base rectangle size (optional, numeric).
#' Default: \code{base_rect_size = base_size/22}.
#'
#' @examples
#'
#' \donttest{
#'   # Plotting iris dataset (using ggplot2, theme_tueb_1, and unikn colors):
#'
#'   library('ggplot2')  # theme_tueb_1 requires ggplot2
#'
#'     n <- 3
#'     groups <- 1:n
#'     df <- data.frame()
#'     set.seed(3)
#'
#'     for (i in seq(1:30)){
#'         data = data.frame(matrix(0, n, 3))
#'         data[, 1] <- i
#'         data[, 2] <- sample(groups, nrow(data))
#'         data[, 3] <- prop.table(sample(c(rep(0, 100), c(1:n)), nrow(data)))
#'         df = rbind(df, data)}
#'
#'     names(df) <- c("X","Group","Y")
#'     df$Group <- as.factor(df$Group)
#'
#'     df <- df[c(2, 1, 3)]
#'     df <- df[order(df$X, df$Group) , ]
#'     rownames(df) <- NULL
#'     my_data <- df
#'
#'     ggplot(my_data, aes(x = X, y = Y, fill = Group)) +
#'         geom_area() +
#'         theme_tueb_1_cont() +
#'         theme(legend.position = "none")
#' }
#' @family plot functions
#'
#' @seealso
#' \code{\link{theme_tueb_1_cont}} for an alternative theme.
#'
#' @import ggplot2
#'
#' @export

# - Definition: ----

theme_tueb_1_cont <- function(col_title = uni_tuebingen_1[[3]],
                              base_size = 11,
                              base_family = "",
                              base_line_size = base_size / 22,
                              base_rect_size = base_size / 22) {
  list(
    ggplot2::theme_bw(
      base_size = base_size,
      base_family = base_family,
      base_line_size = base_line_size,
      base_rect_size = base_rect_size
    ) %+replace%
      ggplot2::theme(
        title = ggplot2::element_text(
          color = col_title,
          face = "bold",
          margin = ggplot2::margin(
            t = 10,
            r = 4,
            b = 4,
            l = 4,
            unit = "pt"
          )
        ),
        plot.subtitle = ggplot2::element_text(
          color = grey(.10, 1),
          face = "plain",
          hjust = 0,
          margin = ggplot2::margin(
            t = 2,
            r = 4,
            b = 8,
            l = 4,
            unit = "pt"
          )
        ),
        plot.caption = ggplot2::element_text(
          color =  grey(.20, 1),
          face = "plain",
          size = ggplot2::rel(.80),
          hjust = 1
        ),
        # axes:
        axis.line =  ggplot2::element_line(color = "black", size = ggplot2::rel(1)),
        axis.ticks = ggplot2::element_line(color = "black", size = ggplot2::rel(.90)),
        axis.title = ggplot2::element_text(color = grey(.10, 1)),
        axis.text =  ggplot2::element_text(color = grey(.20, 1), size = ggplot2::rel(.90)),
        # legend:
        legend.title = ggplot2::element_text(color = grey(.10, 1)),
        legend.text = ggplot2::element_text(color = grey(.20, 1), size = ggplot2::rel(.90)),
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        # strip:
        # strip.background = ggplot2::element_rect(fill = pal_seeblau[[1]], color = pal_seeblau[[5]], size = ggplot2::rel(5/3)),
        strip.background = ggplot2::element_rect(
          fill = grey(.95, 1),
          color = grey(.05, 1),
          # light grey strip background
          size = ggplot2::rel(.90)
        ),
        strip.text = ggplot2::element_text(
          color = grey(0, 1),
          size = ggplot2::rel(1.0),
          margin = ggplot2::margin(
            t = 4,
            r = 4,
            b = 4,
            l = 4,
            unit = "pt"
          )
        ),
        # panel:
        # panel.border = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(
          fill = "transparent",
          color = grey(.05, 1),
          linetype = "solid",
          size = ggplot2::rel(.80)
        ),
        # panel.grid = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(
          color = grey(.75, 1),
          linetype = "solid",
          size = ggplot2::rel(.50)
        ),
        # panel.grid.minor = ggplot2::element_line(color = grey(.75, 1), linetype = "solid", size = ggplot2::rel(.40)), # "dotted"
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = grey(1, 1), color = NA),
        # "white" panel background
        # background:
        plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
        complete = TRUE
      ),
    scale_color_gradientn(colors = usecol(
      c(uni_tuebingen_1[1], uni_tuebingen_1[2], uni_tuebingen_1[3])
    ))
  )
}


###################################################################################################

# theme_tueb_2_disc: Discrete version for plotting for the second palette: ------

# - Documentation: ----

#' Primary theme for University of Tuebingen colours (for ggplot2)
#'
#' \code{theme_tueb_2_disc} provides a basic \bold{colorCodes} theme
#' to use in \bold{ggplot2} commands.
#'
#' The theme is lightweight and no-nonsense, but somewhat
#' opinionated (e.g., in using mostly grey scales to
#' allow emphasizing data points with color accents).
#'
#' @param col_title Color of title (text) elements (optional, numeric).
#' Default: \code{col_title = uni_tuebingen_1[[1]]}.
#'
#' @param base_size Base font size (optional, numeric).
#' Default: \code{base_size = 11}.
#'
#' @param base_family Base font family (optional, character).
#' Default: \code{base_family = ""}.
#' Options include \code{"mono"}, \code{"sans"} (default), and "serif".
#'
#' @param base_line_size Base line size (optional, numeric).
#' Default: \code{base_line_size = base_size/22}.
#'
#' @param base_rect_size Base rectangle size (optional, numeric).
#' Default: \code{base_rect_size = base_size/22}.
#'
#' @examples
#'
#' \donttest{
#'   # Plotting iris dataset (using ggplot2, theme_tueb_1, and unikn colors):
#'
#'   library('ggplot2')  # theme_tueb_1 requires ggplot2
#'
#'   ggplot(datasets::iris) +
#'     geom_jitter(aes(x = Petal.Length, y = Petal.Width, color = Species), size = 3, alpha = 2/3) +
#'     labs(tag = "A", title = "Iris petals",
#'          caption = "Data from datasets::iris") +
#'     theme_tueb_2_disc()
#' }
#'
#' @family plot functions
#'
#' @seealso
#' \code{\link{theme_tueb_1_cont}} for an alternative theme.
#'
#' @import ggplot2
#'
#' @export

# - Definition: ----

theme_tueb_2_disc <- function(col_title = uni_tuebingen_1[[3]],
                              base_size = 11,
                              base_family = "",
                              base_line_size = base_size / 22,
                              base_rect_size = base_size / 22) {
  list(
    ggplot2::theme_bw(
      base_size = base_size,
      base_family = base_family,
      base_line_size = base_line_size,
      base_rect_size = base_rect_size
    ) %+replace%
      ggplot2::theme(
        title = ggplot2::element_text(
          color = col_title,
          face = "bold",
          margin = ggplot2::margin(
            t = 10,
            r = 4,
            b = 4,
            l = 4,
            unit = "pt"
          )
        ),
        plot.subtitle = ggplot2::element_text(
          color = grey(.10, 1),
          face = "plain",
          hjust = 0,
          margin = ggplot2::margin(
            t = 2,
            r = 4,
            b = 8,
            l = 4,
            unit = "pt"
          )
        ),
        plot.caption = ggplot2::element_text(
          color =  grey(.20, 1),
          face = "plain",
          size = ggplot2::rel(.80),
          hjust = 1
        ),
        # axes:
        axis.line =  ggplot2::element_line(color = "black", size = ggplot2::rel(1)),
        axis.ticks = ggplot2::element_line(color = "black", size = ggplot2::rel(.90)),
        axis.title = ggplot2::element_text(color = grey(.10, 1)),
        axis.text =  ggplot2::element_text(color = grey(.20, 1), size = ggplot2::rel(.90)),
        # legend:
        legend.title = ggplot2::element_text(color = grey(.10, 1)),
        legend.text = ggplot2::element_text(color = grey(.20, 1), size = ggplot2::rel(.90)),
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        # strip:
        # strip.background = ggplot2::element_rect(fill = pal_seeblau[[1]], color = pal_seeblau[[5]], size = ggplot2::rel(5/3)),
        strip.background = ggplot2::element_rect(
          fill = grey(.95, 1),
          color = grey(.05, 1),
          # light grey strip background
          size = ggplot2::rel(.90)
        ),
        strip.text = ggplot2::element_text(
          color = grey(0, 1),
          size = ggplot2::rel(1.0),
          margin = ggplot2::margin(
            t = 4,
            r = 4,
            b = 4,
            l = 4,
            unit = "pt"
          )
        ),
        # panel:
        # panel.border = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(
          fill = "transparent",
          color = grey(.05, 1),
          linetype = "solid",
          size = ggplot2::rel(.80)
        ),
        # panel.grid = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(
          color = grey(.75, 1),
          linetype = "solid",
          size = ggplot2::rel(.50)
        ),
        # panel.grid.minor = ggplot2::element_line(color = grey(.75, 1), linetype = "solid", size = ggplot2::rel(.40)), # "dotted"
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = grey(1, 1), color = NA),
        # "white" panel background
        # background:
        plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
        complete = TRUE
      ),
    scale_fill_manual(values = usecol(
      c(
        uni_tuebingen_2[1],
        uni_tuebingen_2[2],
        uni_tuebingen_2[3],
        uni_tuebingen_2[4],
        uni_tuebingen_2[5],
        uni_tuebingen_2[6],
        uni_tuebingen_2[7],
        uni_tuebingen_2[8],
        uni_tuebingen_2[9],
        uni_tuebingen_2[10],
        uni_tuebingen_2[11],
        uni_tuebingen_2[12]
      )
    ))
  )
}




###################################################################################################

# theme_tueb_2_cont: Continuous version for plotting for the second palette: ------

# - Documentation: ----

#' Primary theme for University of Tuebingen colours (for ggplot2)
#'
#' \code{theme_tueb_2_cont} provides a basic \bold{colorCodes} theme
#' to use in \bold{ggplot2} commands.
#'
#' The theme is lightweight and no-nonsense, but somewhat
#' opinionated (e.g., in using mostly grey scales to
#' allow emphasizing data points with color accents).
#'
#' @param col_title Color of title (text) elements (optional, numeric).
#' Default: \code{col_title = uni_tuebingen_1[[1]]}.
#'
#' @param base_size Base font size (optional, numeric).
#' Default: \code{base_size = 11}.
#'
#' @param base_family Base font family (optional, character).
#' Default: \code{base_family = ""}.
#' Options include \code{"mono"}, \code{"sans"} (default), and "serif".
#'
#' @param base_line_size Base line size (optional, numeric).
#' Default: \code{base_line_size = base_size/22}.
#'
#' @param base_rect_size Base rectangle size (optional, numeric).
#' Default: \code{base_rect_size = base_size/22}.
#'
#' @examples
#'
#' \donttest{
#'   # Plotting iris dataset (using ggplot2, theme_tueb_1, and unikn colors):
#'
#'   library('ggplot2')  # theme_tueb_1 requires ggplot2
#'
#'     n <- 12
#'     groups <- 1:n
#'     df <- data.frame()
#'     set.seed(3)
#'
#'     for (i in seq(1:30)){
#'         data = data.frame(matrix(0, n, 3))
#'         data[, 1] <- i
#'         data[, 2] <- sample(groups, nrow(data))
#'         data[, 3] <- prop.table(sample(c(rep(0, 100), c(1:n)), nrow(data)))
#'         df = rbind(df, data)}
#'
#'     names(df) <- c("X","Group","Y")
#'     df$Group <- as.factor(df$Group)
#'
#'     df <- df[c(2, 1, 3)]
#'     df <- df[order(df$X, df$Group) , ]
#'     rownames(df) <- NULL
#'     my_data <- df
#'
#'     ggplot(my_data, aes(x = X, y = Y, fill = Group)) +
#'         geom_area() +
#'         theme_tueb_2_cont() +
#'         theme(legend.position = "none")
#' }
#' @family plot functions
#'
#' @seealso
#' \code{\link{theme_tueb_2_disc}} for an alternative theme.
#'
#' @import ggplot2
#'
#' @export

# - Definition: ----

theme_tueb_2_cont <- function(col_title = uni_tuebingen_1[[3]],
                              base_size = 11,
                              base_family = "",
                              base_line_size = base_size / 22,
                              base_rect_size = base_size / 22) {
  list(
    ggplot2::theme_bw(
      base_size = base_size,
      base_family = base_family,
      base_line_size = base_line_size,
      base_rect_size = base_rect_size
    ) %+replace%
      ggplot2::theme(
        title = ggplot2::element_text(
          color = col_title,
          face = "bold",
          margin = ggplot2::margin(
            t = 10,
            r = 4,
            b = 4,
            l = 4,
            unit = "pt"
          )
        ),
        plot.subtitle = ggplot2::element_text(
          color = grey(.10, 1),
          face = "plain",
          hjust = 0,
          margin = ggplot2::margin(
            t = 2,
            r = 4,
            b = 8,
            l = 4,
            unit = "pt"
          )
        ),
        plot.caption = ggplot2::element_text(
          color =  grey(.20, 1),
          face = "plain",
          size = ggplot2::rel(.80),
          hjust = 1
        ),
        # axes:
        axis.line =  ggplot2::element_line(color = "black", size = ggplot2::rel(1)),
        axis.ticks = ggplot2::element_line(color = "black", size = ggplot2::rel(.90)),
        axis.title = ggplot2::element_text(color = grey(.10, 1)),
        axis.text =  ggplot2::element_text(color = grey(.20, 1), size = ggplot2::rel(.90)),
        # legend:
        legend.title = ggplot2::element_text(color = grey(.10, 1)),
        legend.text = ggplot2::element_text(color = grey(.20, 1), size = ggplot2::rel(.90)),
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        # strip:
        # strip.background = ggplot2::element_rect(fill = pal_seeblau[[1]], color = pal_seeblau[[5]], size = ggplot2::rel(5/3)),
        strip.background = ggplot2::element_rect(
          fill = grey(.95, 1),
          color = grey(.05, 1),
          # light grey strip background
          size = ggplot2::rel(.90)
        ),
        strip.text = ggplot2::element_text(
          color = grey(0, 1),
          size = ggplot2::rel(1.0),
          margin = ggplot2::margin(
            t = 4,
            r = 4,
            b = 4,
            l = 4,
            unit = "pt"
          )
        ),
        # panel:
        # panel.border = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(
          fill = "transparent",
          color = grey(.05, 1),
          linetype = "solid",
          size = ggplot2::rel(.80)
        ),
        # panel.grid = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(
          color = grey(.75, 1),
          linetype = "solid",
          size = ggplot2::rel(.50)
        ),
        # panel.grid.minor = ggplot2::element_line(color = grey(.75, 1), linetype = "solid", size = ggplot2::rel(.40)), # "dotted"
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = grey(1, 1), color = NA),
        # "white" panel background
        # background:
        plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
        complete = TRUE
      ),
    scale_colour_gradientn(colors = usecol(
      c(
        uni_tuebingen_2[1],
        uni_tuebingen_2[2],
        uni_tuebingen_2[3],
        uni_tuebingen_2[4],
        uni_tuebingen_2[5],
        uni_tuebingen_2[6],
        uni_tuebingen_2[7],
        uni_tuebingen_2[8],
        uni_tuebingen_2[9],
        uni_tuebingen_2[10],
        uni_tuebingen_2[11],
        uni_tuebingen_2[12]
      )
    ))
  )
}