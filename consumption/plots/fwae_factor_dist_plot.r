# import libraries
source('./consumption/import_libraries.r')
# source the theme
source('./consumption/plots/themes/custom_fwae_factor_line_theme.r')
# source function doing fwae conversion 
source('./consumption/data_proc/fcn/fcn_fwae_conv.r')


# Prepare df of FWAE factor ----------------------------------------------------
conv_fwae<- conv_fwae %>% 
            select(-one_of('compiled_from','region','species')) %>%
            group_by(prod_eng) %>%
            # calculate min,mean,max of conversion factors
            mutate(min_conv_fac = min(factor_cons_prod_to_fwae),
                   mean_conv_fac = mean(factor_cons_prod_to_fwae),
                   max_conv_fac = max(factor_cons_prod_to_fwae)) %>%

            select(-one_of(c('species','region','compiled_from','original_source',
                             'factor_cons_prod_to_fwae','comment'))) %>%
            distinct() %>%
            gather(lang, product, prod_eng:prod_esp) %>%
            distinct() %>%  # remove duplicate rows, when word is same for 2 languages 
            filter(lang == 'prod_eng') %>%
            select(-spec, -lang) %>% 
            ungroup 


conv_fwae <- conv_fwae %>%
            # rbind(conv_fwae, setNames(data.frame("Preservation", 1, 1.227634, 1.612903, "Unresolved"),names(conv_fwae))) %>%
            gather(key=key, value=x, -product, -factor_type) %>%
            
  
            group_by(product) %>%
            # calculate the PDF of the mode of the triangular distribution
            mutate(y = ifelse(key == 'mean_conv_fac' & min(x) != max(x), 2/(max(x)-min(x)),0)) %>%
            ungroup %>%

  
            group_by(factor_type) %>%
            mutate(temp_fac_max= max(y)) %>%
            ungroup() %>%
            group_by(product) %>%
            mutate(y = ifelse(key == 'mean_conv_fac' & min(x)==max(x), temp_fac_max, y)) %>%
            ungroup() %>%
  
            select(-temp_fac_max) %>%
  
            #mutate(y = ifelse(key == 'mean_conv_fac', 1, 0)) %>%
            mutate(x = as.numeric(x)) %>%
            mutate(product = stri_trans_totitle(product))


# read the code-assigned product list ------------------------------------------
consump_df <- read.csv('../output/consumption/hh_survey_consump_sep_prods_fwaes_uncert.csv',
                       stringsAsFactors=FALSE)

# calculate the number of product types from surveys ---------------------------------
product_type_count <- consump_df %>%  
                      group_by(product) %>% 
                      summarise(Unique_Elements = n())
rm(consump_df)



# extend color paletter 
getPalette = colorRampPalette(brewer.pal(8, "Set2"))



# Preparation plot -------------------------------------------------------------
conv_fwae_preprep <- conv_fwae %>% filter(factor_type == "Preprocessing")

preprep<- ggplot(conv_fwae_preprep, aes(y=y, x=x, color=product)) +
          geom_line(size= 0.8) + 
          scale_y_continuous(breaks=pretty_breaks()) +
          xlab("") + ylab("") + xlim(0,4.8) + 
          custom_factor_line_theme + 
          guides(col = guide_legend(ncol = 1)) +
          scale_colour_manual(values =getPalette(length(unique(conv_fwae_preprep$product)))) +
          theme(plot.margin=unit(c(6, -1, -1, 6), "mm"))

# Preservation plot -------------------------------------------------------------
conv_fwae_preserv <- conv_fwae %>% filter(factor_type == "Preservation")

preserv<- ggplot(conv_fwae_preserv, aes(y=y, x=x, color=product)) +
          geom_line(size= 0.8) + 
          scale_y_continuous(breaks=pretty_breaks()) +
          xlab("") + ylab("Probability density function") + xlim(0, 4.8) + 
          custom_factor_line_theme + 
          guides(col = guide_legend(ncol = 1)) +
          scale_colour_manual(values =getPalette(length(unique(conv_fwae_preserv$product)))) +
          theme(plot.margin=unit(c(1, -1, -1, 6), "mm"))


### Preparation plot -----------------------------------------------------------
# subset factors
conv_fwae_org <- conv_fwae %>% filter(factor_type == "Organism")
# make line plot
org<- ggplot( conv_fwae_org, aes(y=y, x=x, color=product)) +
              geom_line(size= 0.8) + geom_point()
              scale_y_continuous(breaks=pretty_breaks()) +
              xlab("Conversion factor") + ylab("") + xlim(0, 4.8) + 
              custom_factor_line_theme + 
              guides(col = guide_legend(ncol = 1)) + 
              scale_colour_manual(values = getPalette(length(unique(conv_fwae_org$product)))) +
              theme(plot.margin=unit(c(1, -1, 1, 6), "mm"))


# Arrange the plots in multiplot figure ----------------------------------------
plot_grid(preprep, preserv, org,  ncol=1, align="v", 
          label_size = 12, hjust = -0.5, vjust = 1, labels = c('A','B','C'))
          # labels = c('Preprocessing', 'Preservation','Organisms'))


### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/fwae_factors_dist_v2_pdf.png", 
       dpi=600, width=210, height=120, units='mm', type = "cairo-png")
dev.off()
