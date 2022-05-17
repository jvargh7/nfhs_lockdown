sdist <- readxl::read_excel("data/NFHS5 Mapping.xlsx")

uw_mapping <- readxl::read_excel("C:/code/external/nfhs-unhealthy-weight/data/uw_mapping.xlsx",sheet="nfhs4 to nfhs5")


sdist_nfhs4 <- sdist %>% 
  left_join(uw_mapping %>% 
              dplyr::select(contains("nfhs5"),sdistri),
            by=c("nfhs5_statecode","nfhs5_factsheet_state","nfhs5_factsheet_district")) %>% 
  dplyr::select(sdist,sdistri) %>% 
  separate(col=sdistri,sep=";",into=c("V1","V2","V3"),fill = "right") %>% 
  pivot_longer(cols=contains("V"),names_to="V",values_to="sdistri") %>% 
  dplyr::filter(!is.na(sdistri)) %>% 
  dplyr::select(-V)

saveRDS(sdist_nfhs4,"data/sdist_nfhs4.RDS")


