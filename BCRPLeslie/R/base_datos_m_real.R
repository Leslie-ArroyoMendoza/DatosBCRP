#' Esta funcion obtiene data trimestral
#' @param actvos conjunto  de Codigos a descargar de la base de BCRP
#' @param fecha.i fecha inicial "c(yyyy,q)"
#' @param fecha.f fecha inicial "c(yyyy,q)"
#' @return Dataframe con datos
#' @importFrom magrittr "%>%"
#' @export
###### FUNCION DE DATOS BCRP #########
######################################
base_datos_m_real<-function(activos,fecha.i,fecha.f){
  df1<-list()
  for( i in 1:length(activos)){
    df1[[i]]<-datos_mensual(activos[i],fecha.i,fecha.f)
    merge.df<-Reduce(function(...) merge(...,by='fecha',all=TRUE), df1)
  }
  
  #funcion para reemplazar n.d
  replace_nd<-function(x,na.rm=FALSE)replace(x,which(x=='n.d.'),NA)
  merge.df<-as.data.frame(apply(X=merge.df,MARGIN=2, replace_nd))
  merge.df<-merge.df %>% dplyr::mutate_at(tidyr::all_of(activos), as.numeric)
  merge.df$fecha<-as.Date(merge.df$fecha)
  return(merge.df)
}