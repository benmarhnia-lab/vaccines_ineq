

* ssc install ineqdeco
 
global datain "D:\Vaccination - spatial analysis\1_Data"
global dataout "D:\Vaccination - spatial analysis\3_Results"


insheet using "$datain\data_for_stata_theil.csv", clear
drop v1

encode country, gen(n_country)

	foreach var in bcg mcv dtp opv fic e w {   
		
						ineqdeco `var', bygroup(n_country)

						return list , all
						*matrix theil   = r(ge1)
						*matrix within  = r(within_ge1)
						*matrix between = r(between_ge1) 
						
						putexcel set "$dataout\theil\Table_`var'", replace
						
						putexcel A1=("index")
						putexcel B1=("`var'")
						putexcel A2=("Theil")
						putexcel A3=("Within")
						putexcel A4=("Between")
						putexcel B2= matrix(r(ge1))
						putexcel B3= matrix(r(within_ge1))
						putexcel B4= matrix(r(between_ge1))
						
						
						putexcel A6=("Afghanistan")
						putexcel B6=matrix(r(ge1_1))
						
						putexcel A7=("Albania")
						putexcel B7=matrix(r(ge1_2))
						
						putexcel A8=("Angola")
						putexcel B8=matrix(r(ge1_3))
						
						putexcel A9=("Armenia")
						putexcel B9=matrix(r(ge1_4))				
						
						putexcel A10=("Bangladesh")
						putexcel B10=matrix(r(ge1_5))						
						
						putexcel A11=("Benin")
						putexcel B11=matrix(r(ge1_6))						

						putexcel A12=("Burundi")
						putexcel B12=matrix(r(ge1_7))

						putexcel A13=("Cambodia")
						putexcel B13=matrix(r(ge1_8))

						putexcel A14=("Cameroon")
						putexcel B14=matrix(r(ge1_9))
						
						putexcel A15=("Chad")
						putexcel B15=matrix(r(ge1_10))
						
						putexcel A16=("Egypt")
						putexcel B16=matrix(r(ge1_11))
						
						putexcel A17=("Ethiopia")
						putexcel B17=matrix(r(ge1_12))						
						
						putexcel A18=("Ghana")
						putexcel B18=matrix(r(ge1_13))						
						
						putexcel A19=("Guatemala")
						putexcel B19=matrix(r(ge1_14))						
						
						putexcel A20=("Guinea")
						putexcel B20=matrix(r(ge1_15))						
						
						putexcel A21=("Haiti")
						putexcel B21=matrix(r(ge1_16))						
						
						putexcel A22=("India")
						putexcel B22=matrix(r(ge1_17))						
						
						putexcel A23=("Indonesia")
						putexcel B23=matrix(r(ge1_18))						
												
						putexcel A24=("Jordan")
						putexcel B24=matrix(r(ge1_19))						
						
						putexcel A25=("Kenya")
						putexcel B25=matrix(r(ge1_20))						
						
						putexcel A26=("Lesotho")
						putexcel B26=matrix(r(ge1_21))						
												
						putexcel A27=("Liberia")
						putexcel B27=matrix(r(ge1_22))						
												
						putexcel A28=("Madagascar")
						putexcel B28=matrix(r(ge1_23))						
						
						putexcel A29=("Malawi")
						putexcel B29=matrix(r(ge1_24))						
						
						putexcel A30=("Maldives")
						putexcel B30=matrix(r(ge1_25))						
						
						putexcel A31=("Mali")
						putexcel B31=matrix(r(ge1_26))						
						
						putexcel A32=("Mauritania")
						putexcel B32=matrix(r(ge1_27))						
						
						putexcel A33=("Myanmar")
						putexcel B33=matrix(r(ge1_28))	
						
						putexcel A34=("Nepal")
						putexcel B34=matrix(r(ge1_29))						
						
						putexcel A35=("Nigeria")
						putexcel B35=matrix(r(ge1_30))						
						
						putexcel A36=("Pakistan")
						putexcel B36=matrix(r(ge1_31))						
						
						putexcel A37=("Philippines")
						putexcel B37=matrix(r(ge1_32))						
												
						putexcel A38=("Rwanda")
						putexcel B38=matrix(r(ge1_33))						
						
						putexcel A39=("Senegal")
						putexcel B39=matrix(r(ge1_34))						
						
						putexcel A40=("Sierra Leone")
						putexcel B40=matrix(r(ge1_35))						
						
						putexcel A41=("South Africa")
						putexcel B41=matrix(r(ge1_36))	
						
						putexcel A42=("Tajikistan")
						putexcel B42=matrix(r(ge1_37))													
						
						putexcel A43=("Tanzania")
						putexcel B43=matrix(r(ge1_38))						
						
						putexcel A44=("The Gambia")
						putexcel B44=matrix(r(ge1_39))						
						
						putexcel A45=("Timor-Leste")
						putexcel B45=matrix(r(ge1_40))						
												
						putexcel A46=("Uganda")
						putexcel B46=matrix(r(ge1_41))						
						
						putexcel A47=("Zambia")
						putexcel B47=matrix(r(ge1_42))						
						
						putexcel A48=("Zimbabwe")
						putexcel B48=matrix(r(ge1_43))						
												
						
	}
						
						
						