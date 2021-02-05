source('functions.R')
suppressMessages({
  # Read data for antigorite stability depth thru time for cdf models
  cat('Read data for antigorite stability depth thru time for cdf models\n')
  antstab <- read_tsv('data/antstability.tsv')
  
  # Read Penniston-Dorland et al. (2015) dataset
  cat('Reading Penniston-Dorland et al. (2015) dataset\n')
  PD15 <- read_tsv('data/PD15.tsv')
  
  # Read model geotherms
  cat('Reading model geotherms\n')
  geotherms <- read_tsv('data/geotherms.tsv')
  
  # Reading data for 46km lithosphere models
  cat('Reading data for 46km lithosphere models\n')
  m.46km <- list.files('data/46km/', pattern="*.tsv") %>%
    map_df(~{
      fname <- paste0('data/46km/', .x)
      read_tsv(fname) %>% 
        mutate(model = fname %>% stringr::str_extract('cd.[0-9]{2}'))
    }) %>% 
    rename(xnorm = x,
           qs = qy,
           phi = tpara,
           zc = cd,
           z1100 = zL,
           x.trench = xTr,
           x.zc = xCpl,
           tstep = frame) %>% 
    mutate(x = (xnorm*(x.zc - x.trench))+x.trench, .before = qs)
  
  # Reading data for 62km lithosphere models
  cat('Reading data for 62km lithosphere models\n')
  m.62km <- list.files('data/62km/', pattern="*.tsv") %>%
    map_df(~{
      fname <- paste0('data/62km/', .x)
      read_tsv(fname) %>% 
        mutate(model = fname %>% stringr::str_extract('cd.[0-9]{2}'))
    }) %>% 
    rename(xnorm = x,
           qs = qy,
           phi = tpara,
           zc = cd,
           z1100 = zL,
           x.trench = xTr,
           x.zc = xCpl,
           tstep = frame) %>% 
    mutate(x = (xnorm*(x.zc - x.trench))+x.trench, .before = qs)
  
  # Reading data for 78km lithosphere models
  cat('Reading data for 78km lithosphere models\n')
  m.78km <- list.files('data/78km/', pattern="*.tsv") %>%
    map_df(~{
      fname <- paste0('data/78km/', .x)
      read_tsv(fname) %>% 
        mutate(model = fname %>% stringr::str_extract('cd.[0-9]{2}'))
    }) %>% 
    rename(xnorm = x,
           qs = qy,
           phi = tpara,
           zc = cd,
           z1100 = zL,
           x.trench = xTr,
           x.zc = xCpl,
           tstep = frame) %>% 
    mutate(x = (xnorm*(x.zc - x.trench))+x.trench, .before = qs)
  
  # Reading data for 94km lithosphere models
  cat('Reading data for 94km lithosphere models\n')
  m.94km <- list.files('data/94km/', pattern="*.tsv") %>%
    map_df(~{
      fname <- paste0('data/94km/', .x)
      read_tsv(fname) %>% 
        mutate(model = fname %>% stringr::str_extract('cd.[0-9]{2}'))
    }) %>% 
    rename(xnorm = x,
           qs = qy,
           phi = tpara,
           zc = cd,
           z1100 = zL,
           x.trench = xTr,
           x.zc = xCpl,
           tstep = frame) %>% 
    mutate(x = (xnorm*(x.zc - x.trench))+x.trench, .before = qs)
  
  # Combining data
  cat('Combining data\n')
  mods <- bind_rows(m.46km, m.62km, m.78km, m.94km) %>% 
    group_by(model) %>%
    nest(hf = c(x, xnorm, qs, smooth, x.trench, x.zc)) %>%
    ungroup() %>% 
    mutate(age = rep(c(rep(32.6,4), rep(55,4), rep(85,4), rep(110,4)), 4),
           cv = rep(rep(c(40,66,80,100),4), 4),
           .before = hf) %>% 
    relocate(model, .before = phi) %>% 
    group_by(model)
})

# Print data
print(mods, n = nrow(mods))

# Clean up environment
rm(m.46km, m.62km, m.78km, m.94km)
rm(list=lsf.str())

# Save
cat('Saving data to: data/data.RData\n')
save.image(file = 'data/data.RData')
