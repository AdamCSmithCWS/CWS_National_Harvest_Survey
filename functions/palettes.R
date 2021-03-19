

my_col2 <-  scale_color_viridis_d(aesthetics = c("colour","fill"),
                                 option = "magma", begin = 0.3,end = 0.85,direction = -1)

my_col4 <-  scale_color_viridis_d(aesthetics = c("colour","fill"),
                                  option = "plasma",
                                  begin = 0.1,end = 0.9,direction = -1)



my_col_sim <-  scale_color_viridis_d(aesthetics = c("colour","fill"),
                                  option = "magma", begin = 0.2,end = 0.45,direction = -1)


my_col_diverge <- brewer.pal(11,"RdBu")[-c(5,7)] #removes the middle lightest colour leaves 4 red (1:5), 4 blue (6:10)

my_single_col <- brewer.pal(9,"Blues")[9]


library(viridis)
all_levels <- c("Species",
                "A",
                "I",
                "F",
                "M",
                "A-F",
                "A-M",
                "I-F",
                "I-M")
col_bas <- plasma(4,begin = 0.1,end = 0.9,direction = -1)
my_col_b <- c(my_single_col,
              col_bas[c(1,3)],
              col_bas[c(1,2)],
              col_bas)
names(my_col_b) <- all_levels


