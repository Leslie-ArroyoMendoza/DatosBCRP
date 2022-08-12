#' Esta funcion obtiene data diaria
#' @param activos conjunto de Codigos a descargar de la base del FRED
#' @param fecha.i fecha inicial "c(yyyy,m,d)"
#' @param fecha.f fecha inicial "c(yyyy,m,d)"
#' @return Dataframe con datos
#' @importFrom magrittr "%>%"
#' @export
###### FUNCION DE DATOS FREDR #########
######################################
base_datos_fred<-function(activos,fecha.i,fecha.f){
  df1<-list()
  for( i in 1:length(activos)){
    df1[[i]]<-datos_m_fred(activos[i],fecha.i,fecha.f)
    merge.df<-Reduce(function(...) merge(...,by='fecha',all=TRUE), df1)
  }
  #funcion para reemplazar n.d
  replace_nd<-function(x,na.rm=FALSE)replace(x,which(x=='n.d.'),NA)
  merge.df<-as.data.frame(apply(X=merge.df,MARGIN=2, replace_nd))
  #merge.df<-merge.df %>% as_tibble()%>% fill(all_of(activos), .direction="down") %>% as.data.frame()
  merge.df<-merge.df %>% dplyr::mutate_at(tidyr::all_of(activos), as.numeric)
  merge.df$fecha<-zoo::as.Date(merge.df$fecha)
  return(merge.df)
}