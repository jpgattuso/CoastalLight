library(CoastalLighT)
cl_DownloadData(month = 1)
gabes <- cl_GetData(lon = c(10, 14), lat = c(32.5, 36), 
                    dir = "./CoastalLight.d", month = 1)
