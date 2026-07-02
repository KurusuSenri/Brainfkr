# initialize an empty context
init <- function() {
  ctx <- list(
    mem = integer(16),
    ptr = 1L,
    pc = 1L,
    rep = NA_integer_,
    ins = c(),
    jmp_tbl = integer(0),
    history = list()
  )
  return(ctx)
}

# append new instructions to context and generate new jump table
load_ins <- function(ctx, ins) {
  ins <- c(ctx$ins, ins)
  ctx$ins <- ins
  ctx$jmp_tbl <- gen_jmp_tbl(ins)
  return(ctx)
}

# save context history
save_history <- function(ctx) {
  snapshot <- ctx
  snapshot$history <- NULL
  ctx$history[[length(ctx$history) + 1]] <- snapshot
  return(ctx)
}

# load context history
load_history <- function(ctx) {
  all_history <- ctx$history
  if(length(all_history) == 0){
    return(ctx)
  }
  last_state <- all_history[[length(all_history)]]
  ctx <- last_state
  ctx$history <- all_history[-length(all_history)]
  return(ctx)
}

# evaluate context till the end of instructions
eval <- function(ctx) {
  ops <- list(
    "+" = op_add_blk,
    "-" = op_sub_blk,
    ">" = op_inc_ptr,
    "<" = op_dec_ptr,
    "," = op_get_chr,
    "." = op_put_chr,
    "[" = op_jmp_fwd,
    "]" = op_jmp_bkd
  )

  while (ctx$pc <= length(ctx$ins)) {
    token <- ctx$ins[ctx$pc]
    
    if (token %in% as.character(0:9)) {
      digit <- as.integer(token)
      ctx$rep <- ifelse(is.na(ctx$rep), digit, ctx$rep * 10L + digit)
      ctx$pc <- ctx$pc + 1L
      next
    }
    
    ctx$rep <- ifelse(is.na(ctx$rep), 1L, ctx$rep)
    ctx <- ops[[token]](ctx)
    ctx$rep <- NA_integer_
  }
  return(ctx)
}

# tokenize input string to brainfk instructions
tokenize_bf <- function(input_str) {
  valid_tokens <- c(">", "<", "+", "-", ".", ",", "[", "]", as.character(0:9))
  raw_vec <- strsplit(input_str, "")[[1]]
  clean_vec <- raw_vec[raw_vec %in% valid_tokens]
  return(clean_vec)
}

# check whether square brackets match
chk_bkt_map <- function(ins) {
  n <- 0
  for (tkn in ins) {
    if (tkn == "[") {
      n <- n + 1
    } else if (tkn == "]") {
      n <- n - 1
    }
  }
  return(n)
}

# generate square brackets mapping
gen_jmp_tbl <- function(code) {
  jmp_tbl <- integer(length(code))
  stack <- integer()
  for (i in 1:length(code)) {
    tkn <- code[[i]]
    if (tkn == "[") {
      stack <- c(stack, i)
    } else if (tkn == "]") {
      left_idx <- tail(stack, 1)
      stack <- head(stack, -1)
      jmp_tbl[left_idx] <- i
      jmp_tbl[i] <- left_idx
    }
  }
  return(jmp_tbl)
}

# print user friendly registers
dbg_print_reg <- function(ctx) {
  cat(sprintf("ptr: %04d val: %04d pc: %04d\n", ctx$ptr - 1, ctx$mem[ctx$ptr], ctx$pc - 1))
}

# print user friendly memory
dbg_print_mem <- function(ctx) {
  cells <- 8
  mem_len <- length(ctx$mem)
  num_row <- mem_len %/% cells
  cat("mem:\n")
  
  for (row in 1:num_row) {
    cat(sprintf("[%04d] ", (row - 1) * cells))
    
    # number area
    for (col in 1:cells) {
      idx <- (row - 1) * cells + col
      is_ptr <- (idx - 1) == ctx$ptr - 1L
      val_str <- sprintf("%04d", ctx$mem[idx])
      
      if (is_ptr) {
        cat(cli::bg_yellow(cli::col_black(val_str)))
      } else {
        cat(val_str)
      }
      cat(" ")
    }
    
    cat(" | ")
    
    # ASCII area
    for (col in 1:cells) {
      idx <- (row - 1) * cells + col
      is_ptr <- (idx - 1) == ctx$ptr - 1L
      
      if (idx <= mem_len) {
        val <- ctx$mem[idx]
        char <- if (val >= 32 && val <= 126) intToUtf8(val) else " "
        
        if (is_ptr) {
          cat(cli::bg_yellow(cli::col_black(char)))
        } else {
          cat(char)
        }
      }
    }
    
    cat("\n")
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
      cat(sprintf("[%04d]:[%04d]\n", i - 1, ctx$jmp_tbl[i] - 1))
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
