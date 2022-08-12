#' Esta funcion obtiene data mensual
#' @param codigo Codigo a descargar de la base del FRED
#' @param fecha.i fecha inicial "c(yyyy,q)"
#' @param fecha.f fecha inicial "c(yyyy,q)"
#' @return Dataframe con datos
#' @importFrom magrittr "%>%"
#' @export
###### FUNCION DE DATOS FREDR #########
######################################
datos_m_fred<-function(codigo,fecha.i,fecha.f){
  fredr::fredr_set_key("9c280cfe7141267226bb073ff7bdcf50")
  i<-as.Date(paste0(fecha.i[1],'-',fecha.i[2],'-','01'))
  f<-as.Date(paste0(fecha.f[1],'-',fecha.f[2],'-','01'))
  df<-fredr::fredr(series_id =codigo,observation_start = i,
            observation_end = f,frequency = 'm')[,c(1,3)]
  colnames(df)<-c('fecha',codigo) 
  return(df)
}