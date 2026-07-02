list.files("R/", full.names = TRUE) |> sapply(source)

bfkr_interpret <- function(input_str) {
  ctx <- init()
  ctx <- load_ins(ctx, tokenize_bf(input_str))
  ctx <- eval(ctx)
  cat("\n---- END OF PROGRAM ----")
  dbg_print(ctx)
}

bfkr_repl <- function() {
  ctx <- init()
  cat("[q]uit, [r]egisters, [i]nstructions, [p]rint_all, [u]ndo\n")
  while (TRUE) {
    input_str <- readline(">>> ")
    if (input_str == "q") {
      cat("\n---- END OF PROGRAM ----")
      dbg_print(ctx)
      break
    } else if (input_str == "r") {
      dbg_print_reg(ctx)
    } else if (input_str == "m") {
      dbg_print_mem(ctx)
    } else if (input_str == "i") {
      dbg_print_ins(ctx)
    } else if (input_str == "p") {
      dbg_print(ctx)
    } else if (input_str == "u") {
      ctx <- load_history(ctx)
      dbg_print(ctx)
    } else {
      ins <- tokenize_bf(input_str)
      if (length(ins) == 0) {
        next
      }
      if (chk_bkt_map(ins) > 0) {
        while (TRUE) {
          ins <- c(ins, tokenize_bf(readline("... ")))
          map <- chk_bkt_map(ins)
          if (map == 0) {
            break
          }
          if (map < 0) {
            stop("ummapped brackets")
          }
        }
      } else if (chk_bkt_map(ins) < 0) {
        stop("ummapped brackets")
      }
      ctx <- save_history(ctx)
      ctx <- load_ins(ctx, ins)
      ctx <- eval(ctx)
      dbg_print(ctx)
    }
  }
}
