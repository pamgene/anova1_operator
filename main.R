library(tercen)
library(dplyr)

do.anova = function(df){
  f.stat = NaN
  numdf = NaN
  dendf = NaN
  p.value = NaN
  r.squared = NaN
  adj.r.squared = NaN
  aLm = try(lm(.y ~ .group.colors, data=df), silent = TRUE)
  if(!inherits(aLm, 'try-error')) {
    p.value = (anova(aLm)$'Pr(>F)')[[1]]
    sm <- summary(aLm)
    f.stat <- sm$fstatistic[1]
    numdf <- sm$fstatistic[2]
    dendf <- sm$fstatistic[3]
    r.squared <- sm$r.squared
    adj.r.squared <- sm$adj.r.squared
  } 
  return (data.frame(.ri = df$.ri[1], .ci = df$.ci[1],
                     f.stat = c(f.stat),
                     numdf = c(numdf),
                     dendf = c(dendf),
                     p.value = c(p.value),
                     r.squared = c(r.squared),
                     adj.r.squared = c(adj.r.squared)))
}

ctx = tercenCtx()

if (length(ctx$colors) < 1) stop("A color factor is required.")

ctx %>% 
  select(.ci, .ri, .y) %>%
  mutate(.group.colors = do.call(function(...) paste(..., sep='.'), ctx$select(ctx$colors))) %>%
  group_by(.ci, .ri) %>%
  do(do.anova(.)) %>%
  ctx$addNamespace() %>%
  ctx$save()