### simulate the estimations for 8 conditions

# participants per conditions
participants <- 60
noise <- 10

# Duration: short; Type: Reference; Direction: adapt-up 
short_reference_up <- rnorm(participants, mean = 52, sd=noise)

# Duration: short; Type: Reference; Direction: adapt-down 
short_reference_down <- rnorm(participants, mean = 26, sd=noise)

# Duration: short; Type: morph; Direction: adapt-up 
short_morph_up <- rnorm(participants, mean = 45, sd=noise)

# Duration: short; Type: morph; Direction: adapt-down 
short_morph_down <- rnorm(participants, mean = 32, sd=noise)

# Duration: long; Type: Reference; Direction: adapt-up 
long_reference_up <- rnorm(participants, mean = 52, sd=noise)

# Duration: long; Type: Reference; Direction: adapt-down 
long_reference_down <- rnorm(participants, mean = 26, sd=noise)

# Duration: long; Type: morph; Direction: adapt-up 
long_morph_up <- rnorm(participants, mean = 40, sd=noise)

# Duration: long; Type: morph; Direction: adapt-down 
long_morph_down <- rnorm(participants, mean = 37, sd=noise)

### create tags and conect the data

df_estimations <- data.frame(
  Estimation = c(short_reference_up, short_reference_down,
                 short_morph_up, short_morph_down,
                 long_reference_up, long_reference_down,
                 long_morph_up, long_morph_down),
  
  Duration = c(rep('Short',length(short_reference_up)*4),
               rep('Long',length(long_reference_up)*4)),
  
  Type = c(rep(c('Reference','Morph'), each = length(long_reference_up)*2)),
  
  Direction = c(rep('Adapt_up', length(long_reference_up)),
                rep('Adapt_down', length(long_reference_up)),
                rep('Adapt_up', length(long_reference_up)),
                rep('Adapt_down', length(long_reference_up)))
)

### Lets test dot-history hypothesis


wilcox.test(short_reference_up, short_morph_up)

wilcox.test(short_reference_down, short_morph_down)

wilcox.test(long_reference_up, long_morph_up)

wilcox.test(long_reference_down, long_morph_down)

## Alternative with linear regresion model

# convert to 0 and 1 the X variables

df_estimations$Duration_code <- ifelse(df_estimations$Duration == 'Short', 0,1)
df_estimations$Type_code <- ifelse(df_estimations$Type == 'Reference', 0,1)
df_estimations$Direction_code <- ifelse(df_estimations$Direction == 'Adapt_down', 0,1)

summary(lm(Estimation ~ Duration_code*Type_code*Direction_code, 
           data = df_estimations))

# summary(lm(Estimation ~ Type_code:Duration:Direction, 
#            data = df_estimations))

