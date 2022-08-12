#' Esta funcion obtiene data trimestral
#' @param codigo Codigo a descargar de la base de BCRP
#' @param fecha.i fecha inicial "c(yyyy,q)"
#' @param fecha.f fecha inicial "c(yyyy,q)"
#' @return Dataframe con datos
#' @importFrom magrittr "%>%"
#' @export
###### FUNCION DE DATOS BCRP #########
######################################
# fecha.i = c(yyyy,q)
# fecha.f = c(yyyy,q)
datos_trimestral<-function(codigo,fecha.i,fecha.f){
  base="https://estadisticas.bcrp.gob.pe/estadisticas/series/trimestrales/resultados/"
  fecha=paste0(fecha.i[1],'-',fecha.i[2],'/',fecha.f[1],'-',fecha.f[2])
  cadena=  paste0(base,codigo,"/html/", fecha,"/")
  a<-rvest::read_html(cadena)
  a<-rvest::html_nodes(a,"table")
  a<-rvest::html_table(a[[2]])
  colnames(a)<-c('fechas','x')
  a[a$x=='n.d.',2]<-NA
  #a<-na.omit(a)
  a<-tidyr::extract(a,fechas, into=c('T.','trim','y'), "(.{1})(.{1})(.{2})")
  a<-a %>%
    dplyr::mutate(trim=dplyr::recode(trim,'1'='03','2'='06','3'='09','4'='12'))%>%
    dplyr::mutate(day=dplyr::recode(trim,'03'='31','06'='30','09'='30','12'='31'))%>%
    dplyr::mutate(fecha=paste0(day,'/',trim,'/',y))%>%
    dplyr::mutate(fecha=zoo::as.Date(fecha,format = "%d/%m/%y"))%>%
    dplyr::select(-T.,-trim,-y,-day)
  colnames(a)[colnames(a)=="x"]<-codigo
  return(a)
}