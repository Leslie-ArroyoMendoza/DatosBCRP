#' Esta funcion obtiene data diaria
#' @param codigo Codigo a descargar de la base del FRED
#' @param fecha.i fecha inicial "c(yyyy,m,d)"
#' @param fecha.f fecha inicial "c(yyyy,m,d)"
#' @return Dataframe con datos
#' @importFrom magrittr "%>%"
#' @export
###### FUNCION DE DATOS FREDR #########
######################################
datos_d_fred<-function(codigo,fecha.i,fecha.f){
  i<-zoo::as.Date(paste0(fecha.i[1],'-',fecha.i[2],'-',fecha.i[3]))
  f<-zoo::as.Date(paste0(fecha.f[1],'-',fecha.f[2],'-',fecha.f[3]))
  df<-fredr::fredr(series_id =codigo,observation_start = i,
            observation_end = f,frequency = 'd')[,c(1,3)]
  colnames(df)<-c('fecha',codigo) 
  return(df)
}


