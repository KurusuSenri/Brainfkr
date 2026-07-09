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