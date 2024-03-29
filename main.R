library(tercen)
library(dplyr)

do.anova = function(df) {
  result   <- NULL
  pFactor1 <- NaN
  aLm = try(lm(.y ~ .group.colors, data=df), silent = TRUE)
  if (!inherits(aLm, 'try-error')) {
    pFactor1 <- (anova(aLm)$'Pr(>F)')[[1]]
  }
  
  if (is.numeric(df$.group.colors)) {
    slope <- intercept <- NaN
    if (!inherits(aLm, 'try-error')) {
      slope     <- as.vector(coefficients(aLm))[2]
      intercept <- as.vector(coefficients(aLm))[1]
    }
    result <- data.frame(.ri = df$.ri[1], .ci = df$.ci[1], pFactor1 = pFactor1, logPfactor1 = -log10(pFactor1), slope = slope, intercept = intercept)
  } else {
    result <- data.frame(.ri = df$.ri[1], .ci = df$.ci[1], pFactor1 = pFactor1, logPfactor1 = -log10(pFactor1))
  }
  result
}

ctx = tercenCtx()

if (length(ctx$colors) != 1) stop("Grouping for ANOVA1 must be defined using exactly one data color.")

groupingType = ifelse(is.null(ctx$op.value('Grouping Variable')), 'categorical', ctx$op.value('Grouping Variable'))

data <- ctx %>% 
  select(.ci, .ri, .y) %>%
  mutate(.group.colors = ctx$select(ctx$colors) %>% pull())

if (groupingType == 'categorical'){
  data <- data %>% mutate(.group.colors = as.factor(.group.colors))
} else {
  if (!is.numeric(data %>% pull(.group.colors))){
    stop("Grouping data can not be used as a contineous variable")
  }
}

data %>%
  group_by(.ci, .ri) %>%
  do(do.anova(.)) %>%
  ctx$addNamespace() %>%
  ctx$save()
