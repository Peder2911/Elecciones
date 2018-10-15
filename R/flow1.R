
# Votes ############################

pres <- read.csv('data/primera_vuelta_presidencial.csv',stringsAsFactors = FALSE)

adm <- st_read('geo/COL_adm1.shp')%>%
  mutate(name = unicodr::despanize(NAME_1))%>%
  select(name,geometry)

pst <- function(part,of){
  (part/of)*100
}

pres_grp <- pres%>%
  mutate(
    departamento = case_when(
          departamento == 'Bogota D.C.' ~ 'Cundinamarca',
          departamento == 'Norte De San' ~ 'Norte de Santander',
          departamento == 'San Andres' ~ 'San Andres y Providencia',
          departamento == 'Valle' ~ 'Valle del Cauca'),
    departamento = if_else(is.na(departamento),
                pres$departamento,
                departamento),
    departamento = unicodr::despanize(departamento))%>%
  group_by(departamento)%>%
  
  summarise(total = sum(votantes),
            pop = sum(total_votantes),
            duque = sum(iván.duque),
            petro = sum(gustavo.petro),
            fajardo = sum(sergio.fajardo),
            lleras = sum(germán.vargas.lleras),
            calle = sum(humberto.de.la.calle),
            morales = sum(viviane.morales),
            sarmiento = sum(jorge.antonio.trujillo.sarmiento),
            promotores = sum(promotores.voto.en.blanco),
            blank = sum(votos_en_blanco))%>%
  
  mutate(duque_pst = pst(duque,total),
         petro_pst = pst(petro,total),
         fajardo_pst = pst(fajardo,total),
         lleras_pst = pst(lleras,total),
         calle_pst = pst(calle,total),
         morales_pst = pst(morales,total),
         sarmiento_pst = pst(sarmiento,total),
         promotores_pst = pst(promotores,total),
         blank_pst = pst(blank,total),
         participation = pst(total,pop))%>%
  
  select(departamento,duque_pst,petro_pst,fajardo_pst,lleras_pst,
         calle_pst,morales_pst,sarmiento_pst,promotores_pst,blank_pst,
         participation)
  
geovote <- merge(adm,pres_grp,by.x = 'name',by.y = 'departamento')

ggplot(geovote)+
  geom_sf(aes(fill = duque_pst))

ggplot(geovote)+
  geom_sf(aes(fill = participation))+
  geom_sf(data = ged_co_sf_red,aes(size = best))

st_union(geovote[4,'geometry'],ged_co_sf_red[1,'geometry'])%>%
  ggplot()+
  geom_sf()

# ucdp stuff #######################

ged_col <- read.csv('data/ged181.csv')%>%
  filter(country == 'Colombia',
         year > 2002)%>%
  st_as_sf(coords = c('longitude','latitude'))%>%
  select(geometry,deaths = best)

st_crs(ged_col) <- st_crs(geovote)

ged_col_agg <- aggregate(ged_col,geovote,sum)%>%
  mutate(geometry = st_centroid(geometry))%>%
  filter(!is.na(deaths))
,size = 5
ggplot(geovote)+
  geom_sf(aes(fill = duque_pst))+
  geom_sf(data = ged_col_agg,
          aes(size = deaths),color = 'gray')
