# set ptr and automatically enlarge memory
set_ptr <- function(ctx, val) {
  ctx$ptr <- val
  if(ctx$ptr < 1) {
    stop(paste("negative index at PC"))
  }
  if (ctx$ptr > length(ctx$mem)) {
    target_len <- ceiling(ctx$ptr / 16L) * 16L
    ctx$mem <- c(ctx$mem, integer(target_len - length(ctx$mem)))
  }
  return(ctx)
}

# set mem block and automatically wrap
set_mem <- function(ctx, val) {
  ctx$mem[ctx$ptr] <- val %% 256
  return(ctx)
}

# increases memory pointer: moves the pointer to the right 1 block
op_inc_ptr <- function(ctx) {
  ctx <- set_ptr(ctx, ctx$ptr + ctx$rep)
  ctx$pc <- ctx$pc + 1L
  return(ctx)
}

# decreases memory pointer: moves the pointer to the left 1 block
op_dec_ptr <- function(ctx) {
  ctx <- set_ptr(ctx, ctx$ptr - ctx$rep)
  ctx$pc <- ctx$pc + 1L
  return(ctx)
}

# increases value stored at the block pointed to by the memory pointer
op_add_blk <- function(ctx) {
  mem_blk_val <- ctx$mem[ctx$ptr]
  ctx <- set_mem(ctx, mem_blk_val + ctx$rep)
  ctx$pc <- ctx$pc + 1L
  return(ctx)
}

# decreases value stored at the block pointed to by the memory pointer
op_sub_blk <- function(ctx) {
  mem_blk_val <- ctx$mem[ctx$ptr]
  ctx <- set_mem(ctx, mem_blk_val - ctx$rep)
  ctx$pc <- ctx$pc + 1L
  return(ctx)
}

# if block currently pointed to has zero value, jump forward to corresponding ]
op_jmp_fwd <- function(ctx) {
  if (ctx$mem[ctx$ptr] == 0) {
    ctx$pc <- ctx$jmp_tbl[ctx$pc]
  } else {
    ctx$pc <- ctx$pc + 1L
  }
  return(ctx)
}

# if block currently pointed to has non zero value, jump backward to corresponding [
op_jmp_bkd <- function(ctx) {
  if (ctx$mem[ctx$ptr] != 0) {
    ctx$pc <- ctx$jmp_tbl[ctx$pc]
  } else {
    ctx$pc <- ctx$pc + 1L
  }
  return(ctx)
}

# get 1 character
op_get_chr <- function(ctx) {
  chr <- readline("??? ")
  if (chr == "") {
    chr <- "\n"
  }
  chr_ascii <- utf8ToInt(chr)[[1]]
  ctx <- set_mem(ctx, chr_ascii)
  ctx$pc <- ctx$pc + 1L
  return(ctx)
}

# print 1 character to the console
op_put_chr <- function(ctx) {
  chr_ascii <- ctx$mem[ctx$ptr]
  chr <- intToUtf8(chr_ascii)
  cat(chr)
  ctx$pc <- ctx$pc + 1L
  return(ctx)
}
