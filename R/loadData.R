library(ibrlib)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(colorRamps)
library(grDevices)
library(colorspace)
library(wesanderson)
source('R/sql_queries.R')

mydb <- connect_local('r_user','ja.sam.r.user',)

# radiodata
tofactors <- c('idPNMain')
data.radio <- load_sql(mydb,query.radiodata,tofactors)

# askap and most data
tofactors <- c('idPNMain')
data.askap_most <- load_sql(mydb,query.askap_most,tofactors)

# positional offset data
tofactors <- c('idPNMain')
data.positional_offset <- load_sql(mydb,query.positional_offset,tofactors)

# radiodata 20cm 'frame'
tofactors <- c('idPNMain')
data.radio.20cmframe <- load_sql(mydb,query.radiodata.20cmframe,tofactors)

# spindex data
tofactors <- c('idPNMain')
data.spindex <- load_sql(mydb,query.results,tofactors)

# spindex data
tofactors <- c('idPNMain')
data.meta <- load_sql(mydb,query.meta,tofactors)

# classification data
tofactors <- c('idPNMain')
data.classification <- load_sql(mydb,query.class,tofactors)

# optical data
tofactors <- c('idPNMain')
data.optical.flags <- load_sql(mydb,query.optical,tofactors)

# optical data
tofactors <- c('idPNMain')
data.ir.flags <- load_sql(mydb,query.ir,tofactors)

# optical data
tofactors <- c('idPNMain')
data.allflags <- load_sql(mydb,query.allflags,tofactors)

# optical data
tofactors <- c('idPNMain')
data.mwa_localrms <- load_sql(mydb,query.mwa_rms,tofactors)

# distances to the center of the LMC
# RA_cent = 79.5082747
# DEC_cent = -68.9706213
tofactors <- c('idPNMain')
data.distances_to_centerLMC <- load_sql(mydb,query.distances_to_centerfield,tofactors)

kill_local()
