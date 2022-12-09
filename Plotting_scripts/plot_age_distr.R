##################################
# Plot the population density in Tanzania
#
# 23.04.2021
##################################

# <group poppercent="3.474714994" upperbound="1"/>
#       <group poppercent="12.76004028" upperbound="5"/>
#       <group poppercent="14.52151394" upperbound="10"/>
#       <group poppercent="12.75565434" upperbound="15"/>
#       <group poppercent="10.836323739999999" upperbound="20"/>
#       <group poppercent="8.393312454" upperbound="25"/>
#       <group poppercent="7.001421452" upperbound="30"/>
#       <group poppercent="5.800587654" upperbound="35"/>
#       <group poppercent="5.102136612" upperbound="40"/>
#       <group poppercent="4.182561874" upperbound="45"/>
#       <group poppercent="3.339409351" upperbound="50"/>
#       <group poppercent="2.986112356" upperbound="55"/>
#       <group poppercent="2.555766582" upperbound="60"/>
#       <group poppercent="2.332763433" upperbound="65"/>
#       <group poppercent="1.77400255" upperbound="70"/>
#       <group poppercent="1.008525491" upperbound="75"/>
#       <group poppercent="0.74167341" upperbound="80"/>
#       <group poppercent="0.271863401" upperbound="85"/>
#       <group poppercent="0.161614642" upperbound="90"/>
bins = c(1, seq(5, 90, by=5))
pop_p = c(3.474714994, 12.76004028, 14.52151394, 12.75565434, 10.836323739999999, 8.393312454, 7.001421452, 5.800587654,
          5.102136612, 4.182561874, 3.339409351, 2.986112356, 2.555766582, 2.332763433, 1.77400255, 1.008525491, 0.74167341, 0.271863401, 0.161614642)
pop_df = cbind.data.frame(bins, pop_p)
ggplot(pop_df, aes(x = bins, y = pop_p)) + 
    geom_col() + theme_bw(base_size=14) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = c(1, seq(10, 90, by=10))) +
    labs( x = "Age group (years)", y = "Percentage of the population (%)") 


