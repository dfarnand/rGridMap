# getStateHexPoly
# ' Internal function that creates and returns a data.frame with polygon
# ' points for the grid map

getStateLayout <- function() {
  # layout of state hexes
  data.frame(
    label =
      c(
        "AK", "ME", "WA", "MT", "ND", "MN", "WI", "MI", "NY",
        "MA", "RI", "OR", "NV", "CO", "NE", "MO", "KY", "WV",
        "VA", "MD", "DE", "AZ", "OK", "LA", "MS", "AL", "GA",
        "HI", "TX", "FL", "CA", "UT", "NM", "KS", "AR", "TN",
        "NC", "SC", "DC", "ID", "WY", "SD", "IA", "IL", "IN",
        "OH", "PA", "NJ", "CT", "VT", "NH"
      ),
    x_pos =
      c(
        1L, 12L, 2L, 3L, 4L, 5L, 6L, 8L, 10L, 11L, 12L, 2L, 3L,
        4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 4L, 5L, 6L, 7L, 8L,
        9L, 0L, 5L, 8L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L,
        2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 10L, 11L
      ),
    y_pos =
      c(
        7L, 7L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 3L, 3L, 3L,
        3L, 3L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 0L,
        0L, 0L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 4L, 4L, 4L,
        4L, 4L, 4L, 4L, 4L, 4L, 4L, 6L, 6L
      ),
    x_offset =
      c(
        0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
        0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
      ),
    stringsAsFactors = FALSE
  )
}

getNYSCountyLayout <- function() {
  data.frame(
    label = c(
      "SI", "NY", "BK", "BX", "QU",
      "NA", "SU", "RO", "WE", "OR", "PU", "CH", "CA", "AG", "ST", "CM",
      "TI", "BM", "SV", "UL", "DU", "ER", "WY", "YA", "SY", "YO", "CR",
      "DE", "GR", "CO", "GE", "LI", "OT", "SE", "OD", "CN", "SH", "AL",
      "RE", "NI", "OL", "MO", "WA", "CY", "MA", "MG", "SC", "SA", "OS",
      "ON", "OG", "FU", "WS", "JE", "LE", "HE", "WR", "SL", "HA", "ES",
      "FR", "CL"
    ), x_pos = c(
      9L, 10L, 11L, 9L, 10L, 11L, 12L, 9L, 10L,
      8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 1L, 2L, 3L,
      4L, 5L, 6L, 7L, 8L, 9L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L,
      1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 6L, 7L, 8L, 9L, 10L, 6L,
      7L, 8L, 9L, 8L, 9L, 10L, 8L, 9L
    ), y_pos = c(
      1L, 1L, 1L, 2L, 2L,
      2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
      6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 7L, 7L,
      7L, 7L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 9L, 9L, 9L, 9L, 9L,
      10L, 10L, 10L, 10L, 11L, 11L, 11L, 12L, 12L
    ), x_offset = c(
      0L,
      0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L,
      0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L
    )
  )
}

getOtherLayout <- function(path) {
  #' Loads layout info from a csv
  layout <- read.csv(layout)
  stopifnot(colnames(layout) == c("label", "x_pos", "y_pos", "x_offset"))
}

getStateHexPoly <- function(layout = "state", label_col = "state.abb") {
  #' Either specify "state" or "nyscounty",
  #' Or alternatively provide the path to a csv
  #' with columns "label", "x_pos", "y_pos", and "x_offset"

  # polygon points for one hex
  w <- round(sqrt(3) / 2, 2)
  hex <- data.frame(
    x = c(0, w, w, 0, -w, -w),
    y = c(1, 0.5, -0.5, -1, -0.5, 0.5),
    order = 1:6
  )

  # Load specified layout
  if (layout == "state") {
    layout <- getStateLayout()
  } else if (layout == "nyscounty") {
    layout <- getNYSCountyLayout()
  } else {
    layout <- getOtherLayout(layout)
  }


  # replicate hex for each state
  hexes <- data.frame(hex,
    state.abb = rep(layout$label, each = nrow(hex)),
    stringsAsFactors = F
  )

  hex_states <- merge(hexes, layout)

  # poly points for each state hex
  hex_states <- within(hex_states, {
    draw_x <- x + (2 * x_pos + x_offset) *  w
    draw_y <- y + y_pos * 1.5
  })

  # centroids for label points
  hex_states <- within(hex_states, {
    lab_x <- ifelse(order==1, draw_x - x, NA)
    lab_y <- ifelse(order==1, draw_y - y, NA)
  })

  # clean up data.frame
  hex_states <-
    hex_states[, c("label", "draw_x", "draw_y", "order", "lab_x", "lab_y")]

  # change state.abb col name
  if (label_col != "") {
    names(hex_states)[1] <- label_col
  }

  return(hex_states)
}
