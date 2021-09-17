minage <- gsub('age','',female_imm$dimnames$age) %>% as.numeric() %>% min()
maxage <- gsub('age','',female_mat$dimnames$age) %>% as.numeric() %>% max()
minlength <- gsub('len','',female_imm$dimnames$length) %>% as.numeric() %>% min()
maxlength <- gsub('len','',female_mat$dimnames$length) %>% as.numeric() %>% max()


dl <- gsub('len','',female_mat$dimnames$length) %>% as.numeric() %>% diff() %>% median()
## Query length data to create catchdistribution components
ldist.aut.is <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'AUT',
                      age = mfdb_interval("all",c(minage,maxage),
                                       open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))


ldist.aut.gl <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      #data_source = 'greenland-ldist',
                      sampling_type = 'GGHL',
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))



## Age IGFS
aldist.aut.is <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'),
                    c(list(sampling_type = 'AUT',
                           data_source = 'iceland-aldist',
                           age = mfdb_interval('age',c(minage:maxage),open_ended = c('upper')),
                           length = mfdb_interval("len", 
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))

matp.aut.is <- 
  mfdb_sample_count(mdb, c('maturity_stage','age','length'),
                    append(defaults,
                           list(sampling_type='AUT',
                                sex = 'F',
                                #age=mfdb_group(mat_ages=minage:maxage),
                                length = mfdb_interval('len',
                                                       seq(minlength, maxlength, by = 2*dl),
                                                       open_ended = c('lower','upper')),              
                                maturity_stage = mfdb_group(femimm = 1, femmat = 2:5))))[[1]] %>% 
  mfdb::mfdb_concatenate_results(mfdb_sample_count(mdb, c('maturity_stage','age','length'),
                                                   append(defaults,
                                                          list(sampling_type='AUT',
                                                               sex = 'M',
                                                               #age=mfdb_group(mat_ages=minage:maxage),
                                                               length = mfdb_interval('len',
                                                                                      seq(minlength, maxlength, by = 2*dl),
                                                                                      open_ended = c('lower','upper')),              
                                                               maturity_stage = mfdb_group(malimm = 1, malmat = 2:5))))[[1]])

attributes(matp.aut.is)$age$all <- minage:maxage


ldist.lln.is <- 
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      data_source = 'iceland-ldist',
                      gear = c('LLN','HLN'),
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

ldist.lln.gl <- 
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      data_source = 'greenland-ldist',
                      gear = c('LLN','HLN'),
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

# ldist.lln.fo <- 
#   mfdb_sample_count(mdb, 
#                     c('age', 'length'), 
#                     c(list(
#                       sampling_type = 'SEA',
#                       data_source = 'faroes-ldist',
#                       gear = c('LLN','HLN'),
#                       age = mfdb_interval("all",c(minage,maxage),
#                                           open_ended = c("upper","lower")),
#                       length = mfdb_interval("len", 
#                                              seq(minlength, maxlength, by = dl),
#                                              open_ended = c("upper","lower"))),
#                       defaults))
# 



ldist.gil.is <- 
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      data_source = 'iceland-ldist',
                      gear = c('GIL'),
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

ldist.bmt.is <- 
  mfdb_sample_count(mdb,
                    c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      data_source = 'iceland-ldist',
                      gear=c('BMT','NPT','DSE','PSE','PGT','SHT'),
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

aldist.bmt.is <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'),
                    c(list(sampling_type = 'SEA',
                           data_source = 'iceland-aldist',
                           gear=c('BMT','NPT','DSE','PSE','PGT','SHT'),
                           age = mfdb_interval('age',c(minage:maxage),open_ended = c('upper')),
                           length = mfdb_interval("len", 
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))


ldist.bmt.gl <- 
  mfdb_sample_count(mdb,
                    c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      data_source = 'greenland-ldist',
                      gear=c('BMT','NPT','DSE','PSE','PGT','SHT'),
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

ldist.bmt.fo <- 
  mfdb_sample_count(mdb,
                    c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      data_source = 'faroes-ldist',
                      gear=c('BMT','NPT','DSE','PSE','PGT','SHT'),
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))
