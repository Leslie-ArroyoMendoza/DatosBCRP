#' Esta funcion obtiene data mensual
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
datos_mensual<-function(codigo,fecha.i,fecha.f){
  base="https://estadisticas.bcrp.gob.pe/estadisticas/series/mensuales/resultados/"
  fecha=paste0(fecha.i[1],'-',fecha.i[2],'/',fecha.f[1],'-',fecha.f[2])
  cadena= paste0(base,codigo,"/html/", fecha,"/")
  a<-rvest::read_html(cadena)
  a<-rvest::html_nodes(a,"table")
  a<-rvest::html_table(a[[2]])
  colnames(a)<-c('fechas','x')
  a[a$x=='n.d.',2]<-NA
  #a<-na.omit(a)
  index2<-a
  index2<-tidyr::extract(index2,fechas, into=c('m','y'), "(.{3})(.{2})")
  index2<-index2 %>%
    dplyr::mutate(m=dplyr::recode(m,'Ene'='01','Feb'='02','Mar'='03','Abr'='04',
                    'May'='05','Jun'='06','Jul'='07','Ago'='08',
                    'Sep'='09','Oct'='10','Nov'='11','Dic'='12'))
  index2$fecha<-apply(cbind(rep("01",dim(index2)[1]),index2[,c(1,2)]),1,paste0, collapse="/")
  index2<-index2[,!(names(index2) %in% c('m','y'))]
  index2$fecha<-zoo::as.Date(index2$fecha,format="%d/%m/%y")
  a<-index2
  colnames(a)[colnames(a)=="x"]<-codigo
  return(a)
}
