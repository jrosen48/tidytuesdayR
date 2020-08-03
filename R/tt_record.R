#' @title Record and generate plot histories
#'
#' @description Record your past plots and generate a gif of the plots you've
#'    made in this r session. Common technique used by TidyTuesday-ers to show
#'    where they started vs their final plots
#'
#' @rdname Recording
#'
#' @param dir directory to save the intermediate plots in
#' @inheritParams ggplot2::ggsave
#'
#' @importFrom ggplot2 ggsave

#'
#' @examples
#'
#' if(require(ggplot2)){
#'   tt_record(dir = file.path(tempdir(),"recording"))
#'   ggplot(data.frame(x = 1, y = 1), aes(x=x, y=y)) + geom_point() + ylim(0,4)
#'   ggplot(data.frame(x = 1, y = 2), aes(x=x, y=y)) + geom_point() + ylim(0,4)
#'   ggplot(data.frame(x = 1, y = 3), aes(x=x, y=y)) + geom_point() + ylim(0,4)
#'   tt_playback(tempfile(fileext = ".gif"))
#' }
#'
#' @export

tt_record <- function(dir = NULL,
                      device = c("png", "jpeg", "bmp", "tiff", "emf", "svg", "eps"),
                      scale = 1,
                      width = NA,
                      height = NA,
                      units = c("in", "cm", "mm"),
                      dpi = 300,
                      limitsize = TRUE
){

  if(is.null(dir)){
    is_temp_dir <- TRUE
    dir <- tempdir()
  }else{
    is_temp_dir <- FALSE
  }

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  } else{
    if (length(list.files(dir, pattern = paste0("[.]", device, "$"))) > 1) {
      warning(
        "Writing to a folder that already exists. tt_playback may use more files than intended!"
      )
    }
  }

  device <- match.arg(device)
  units <- match.arg(units)

  TT_RECORDING_ENV$recording_dir <- dir
  TT_RECORDING_ENV$device <- device
  TT_RECORDING_ENV$is_temp_dir <- is_temp_dir

  ## create shim function

  TT_RECORDING_ENV$print.view_and_save_ggplot <-
    function(x,
             newpage = is.null(vp),
             vp = NULL,
             ...) {

      # View plot
      ggplot2:::print.ggplot(x, newpage = newpage, vp = vp, ...)
      time <- Sys.time()

      plot_file <- file.path(dir,paste0(format(time, "%Y_%m_%d_%H_%M_%OS6"),".",device))

      if(file.exists(plot_file)){
        paste
      }

      suppressMessages({
        ggplot2::ggsave(
          filename = plot_file,
          plot = x,
          scale = scale,
          width = width,
          height = height,
          units = units,
          dpi = dpi,
          limitsize = limitsize
        )
      })

      invisible(x)

    }

  registerS3method(
    genname = "print",
    class = "ggplot",
    method = "print.view_and_save_ggplot",
    envir = TT_RECORDING_ENV
  )
}


#' @describeIn Recording
#'
#' @param name name of gif
#' @param last_image_duration n units of frame_duration to show the last image for
#' @param frame_duration n seconds each plot should be shown
#' @param ... arguments passed to \code{\link[gifski]{gifski}}
#' @param playback Boolean, should the recording start playing after it is
#' turned into a gif? defaults to TRUE
#' @param stoprecording Boolean, should the plots stop being recorded?
#' defaults to TRUE.
#' @importFrom gifski gifski
#' @export

tt_playback <-
  function(name = NULL,
           last_image_duration = 12,
           frame_duration = .25,
           ...,
           playback = TRUE,
           stoprecording = TRUE) {

    records <- list.files(
      path    = TT_RECORDING_ENV$recording_dir,
      pattern = paste0("*.", TT_RECORDING_ENV$device, "$"),
      full.names = TRUE
    )

    stopifnot(last_image_duration > 0)

    records <- c(records[-length(records)],
                 rep(records[length(records)], times = last_image_duration))

    if (is.null(name)) {
      recording <- paste0(format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".gif")

      if (!TT_RECORDING_ENV$is_temp_dir) {
        recording <- file.path(TT_RECORDING_ENV$recording_dir, recording)
      }

    } else{
      recording <- name
    }

    gifski::gifski(records,
                   gif_file = recording,
                   delay = frame_duration,
                   ...)

    viewer <- getOption("viewer", utils::browseURL)

    if (is.function(viewer) && length(recording) && playback) {
      viewer(recording)
    }

    ## revert ggplot printing to standard printing
    if (stoprecording) {
      registerS3method(
        genname = "print",
        class = "ggplot",
        method = "print.ggplot",
        envir = getNamespace("ggplot2")
      )

    }


    invisible()

  }



TT_RECORDING_ENV <- new.env()
