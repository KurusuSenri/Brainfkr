# print user friendly registers
dbg_print_reg <- function(ctx) {
  cat(sprintf("ptr: %03d val: %03d pc: %03d\n", ctx$ptr - 1, ctx$mem[ctx$ptr], ctx$pc - 1))
}

# print user friendly memory
dbg_print_mem <- function(ctx) {
  cells <- 8
  mem_len <- length(ctx$mem)
  num_row <- mem_len %/% cells
  cat("mem:\n")
  
  for (row in 1:num_row) {
    cat(sprintf("[%03d] ", (row - 1) * cells))
    
    # number area
    for (col in 1:cells) {
      idx <- (row - 1) * cells + col
      is_ptr <- (idx - 1) == ctx$ptr - 1L
      val_str <- sprintf("%03d", ctx$mem[idx])
      
      if (is_ptr) {
        cat(cli::bg_yellow(cli::col_black(val_str)))
      } else {
        cat(val_str)
      }
      cat(" ")
    }
    
    cat(" |")
    
    # ASCII area
    for (col in 1:cells) {
      idx <- (row - 1) * cells + col
      is_ptr <- (idx - 1) == ctx$ptr - 1L
      
      if (idx <= mem_len) {
        val <- ctx$mem[idx]
        char <- if (val >= 32 && val <= 126) intToUtf8(val) else "."
        
        if (is_ptr) {
          cat(cli::bg_yellow(cli::col_black(char)))
        } else {
          cat(char)
        }
      }
    }
    
    cat("|\n")
  }
}

# print instructions and jump table
dbg_print_ins <- function(ctx) {
  ins_vec <- ctx$ins
  len <- length(ins_vec)
  if(len == 0){
    cat("ins:\n")
    cat("jmp_tbl:\n")
    return()
  }
  
  cat("ins:\n")
  cat(ctx$ins)
  cat("\n")
  
  cat("jmp_tbl:\n")
  active_indices <- which(ctx$jmp_tbl != 0)
  for (i in active_indices) {
    if (i < ctx$jmp_tbl[i]) {
      cat(sprintf("[%03d]:[%03d]\n", i - 1, ctx$jmp_tbl[i] - 1))
    }
  }
}

# print all context
dbg_print <- function(ctx) {
  cat("\n")
  dbg_print_reg(ctx)
  dbg_print_mem(ctx)
  dbg_print_ins(ctx)
}
