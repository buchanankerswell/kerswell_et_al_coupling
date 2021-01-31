# Load Libraries
cat('Loading packages')
suppressMessages({
  lapply(
    c('readr', 'dplyr', 'purrr', 'tidyr'),
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )
  
  # Read data for antigorite stability depth thru time for cdf models
  cat('Read data for antigorite stability depth thru time for cdf models\n')
  antstab <- read_tsv('coupling_data/antstability.tsv')
  
  # Read Penniston-Dorland et al. (2015) dataset
  cat('Reading Penniston-Dorland et al. (2015) dataset\n')
  PD15 <- read_tsv('coupling_data/PD15.tsv')
  
  # Read model geotherms
  cat('Reading model geotherms\n')
  geotherms <- read_tsv('coupling_data/geotherms.tsv')
  
  # Reading data for 46km lithosphere models
  cat('Reading data for 46km lithosphere models\n')
  m.46km <- list.files('coupling_data/46km/', pattern="*.tsv") %>%
    map_df(~{
      fname <- paste0('coupling_data/46km/', .x)
      read_tsv(fname) %>% 
        mutate(model = fname %>% substr(20,24))
    })
  
  # Reading data for 62km lithosphere models
  cat('Reading data for 62km lithosphere models\n')
  m.62km <- list.files('coupling_data/62km/', pattern="*.tsv") %>%
    map_df(~{
      fname <- paste0('coupling_data/62km/', .x)
      read_tsv(fname) %>% 
        mutate(model = fname %>% substr(20,24))
    })
  
  # Reading data for 78km lithosphere models
  cat('Reading data for 78km lithosphere models\n')
  m.78km <- list.files('coupling_data/78km/', pattern="*.tsv") %>%
    map_df(~{
      fname <- paste0('coupling_data/78km/', .x)
      read_tsv(fname) %>% 
        mutate(model = fname %>% substr(20,24))
    })
  
  # Reading data for 94km lithosphere models
  cat('Reading data for 94km lithosphere models\n')
  m.94km <- list.files('coupling_data/94km/', pattern="*.tsv") %>%
    map_df(~{
      fname <- paste0('coupling_data/94km/', .x)
      read_tsv(fname) %>% 
        mutate(model = fname %>% substr(20,24))
    })
  
  # Combining data
  cat('Combining data\n')
  mods <- bind_rows(m.46km, m.62km, m.78km, m.94km) %>% 
    group_by(model) %>%
    nest(hf = c(x, qy, smooth, xTr, xCpl)) %>%
    ungroup() %>% 
    mutate(age = rep(c(rep(32.6,4), rep(55,4), rep(85,4), rep(110,4)), 4),
           cv = rep(rep(c(40,66,80,100),4), 4),
           .before = hf) %>% 
    rename(phi = tpara,
           zc = cd,
           z1100 = zL,
           tstep = frame) %>% 
    relocate(model, .before = phi) %>% 
    group_by(model)
})
# Print data
print(mods, n = nrow(mods))

# Clean up environment
rm(m.46km, m.62km, m.78km, m.94km)

# Save
cat('Saving data to: reduced_data.RData\n')
save.image(file = 'reduced_data.RData')
