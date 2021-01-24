
#-------------------------------------------------------------------
# Order country alphabetically or by latitude +
# define country regions
#-------------------------------------------------------------------

ctry_bylat <- c("Niger", "Mali", "Senegal","Burkina Faso", "Guinea", "Benin", "Nigeria", "Ethiopia", 
                 "Sierra Leone", "Ghana", "Cote D'Ivoire", "Liberia", "Cameroon", "Uganda", "Kenya", 
                 "Rwanda", "DR Congo", "Tanzania (Zanzibar)", "Tanzania (Mainland)", "Angola", "Malawi", 
                "Zambia", "Mozambique", "Zimbabwe", "Madagascar", "Myanmar", "Thailand", "Cambodia")

ctry_byaz <- c(rev(ctry_bylat[-c(1:3)]), "Cambodia", "Myanmar", "Thailand")


# northern hemisphere (Uganda and Kenya centroids are above equator but put them in southern 
# hemisphere region ecause their rainy season starts in September just like in southern Africa)
ctry_nhem <- c('Niger', 'Mali', 'Senegal', 'Burkina Faso', 'Guinea', 'Benin', 'Nigeria', 
                'Ethiopia', 'Sierra Leone', 'Ghana', "Cote D'Ivoire", 'Liberia', 'Cameroon', 
                'Myanmar', 'Thailand', 'Cambodia')

# Southern hemisphere
ctry_shem <- c('Uganda', 'Kenya', 'Rwanda', 'DR Congo', 'Tanzania (Zanzibar)', 'Tanzania (Mainland)', 
               'Angola', 'Malawi', 'Zambia', 'Mozambique', 'Zimbabwe', 'Madagascar')

# West Africa: Cameroon + any countries west of it
ctry_wafr <- c('Niger', 'Mali', 'Senegal', 'Burkina Faso', 'Guinea', 'Benin', 'Nigeria', 
               'Sierra Leone', 'Ghana', "Cote D'Ivoire", 'Liberia', 'Cameroon')

# East Africa: Rwanda + any countries east of it
ctry_eafr <- c('Ethiopia', 'Uganda', 'Kenya', 'Rwanda', 'Tanzania (Zanzibar)', 'Tanzania (Mainland)', 
               'Malawi', 'Mozambique', 'Zimbabwe', 'Madagascar')

# Central + Southern Africa: Rwanda + anythin south of it
ctry_csafr <- c('Rwanda', 'DR Congo', 'Tanzania (Zanzibar)', 'Tanzania (Mainland)', 
                'Angola', 'Malawi', 'Zambia', 'Mozambique', 'Zimbabwe', 'Madagascar')

# Southeast Asia
ctry_sea <- c("Myanmar", "Thailand", "Cambodia")
