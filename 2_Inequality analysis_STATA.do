
* ssc install conindex


global datain "D:\Vaccination - spatial analysis\1_Data"
global dataout "D:\Vaccination - spatial analysis\3_Results"

************************************************************************************************************
*** National-level inequalities
************************************************************************************************************

insheet using "$datain\data_for_stata.csv", clear
		

		
		levels country, local(countries)  //create a list of country names     
			
		foreach name of local countries {      
		

				insheet using "$datain\data_for_stata.csv", clear

				keep if country == "`name'"
				
				drop v1
				destring fic, replace force	
				destring wealthscore, replace force	
				
				replace wealthscore = 1 if country == "Tajikistan" & wealth == "poorest"
				replace wealthscore = 2 if country == "Tajikistan" & wealth == "poorer"
				replace wealthscore = 3 if country == "Tajikistan" & wealth == "middle"
				replace wealthscore = 4 if country == "Tajikistan" & wealth == "richer"
				replace wealthscore = 5 if country == "Tajikistan" & wealth == "richest"

				
				conindex fic [aweight=wt], rankvar(wealthscore) wagstaff bounded limits(0 1) 
				
						return list , all 					
						putexcel set "$dataout\country\Table_`name'", replace
	
						putexcel A1=("country")
						putexcel B1=("index")
						putexcel C1=("obs")
						putexcel D1=("value")
						putexcel E1=("SE")
						
						putexcel A2=("`name'")
						putexcel B2=("wagstaff")
						putexcel C2= matrix(r(N))
						putexcel D2= matrix(r(CI))
						putexcel E2= matrix(r(CIse))
						
				conindex fic [aweight=wt], rankvar(wealthscore) erreygers bounded limits(0 1)	
				
						return list , all 
						matrix N = r(N)
						matrix index = r(CI)
						matrix SE = r(CIse)
											
						putexcel A3=("`name'")
						putexcel B3=("erreygers")
						putexcel C3= matrix(r(N))
						putexcel D3= matrix(r(CI))
						putexcel E3= matrix(r(CIse))

		
		}
		
		
************************************************************************************************************
*** Subnational-level inequalities
************************************************************************************************************


insheet using "$datain\data_for_stata.csv", clear

		drop if reg3 == "MLkidal"
		drop if reg3 == "ALkor"
		
		levels reg3, local(regions)  //create a list of country names     
			
		foreach name of local regions {      
		

				insheet using "$datain\data_for_stata.csv", clear

				keep if reg3 == "`name'"
				
				drop v1
				destring fic, replace force
				destring wealthscore, replace force	
				
				replace wealthscore = 1 if country == "Tajikistan" & wealth == "poorest"
				replace wealthscore = 2 if country == "Tajikistan" & wealth == "poorer"
				replace wealthscore = 3 if country == "Tajikistan" & wealth == "middle"
				replace wealthscore = 4 if country == "Tajikistan" & wealth == "richer"
				replace wealthscore = 5 if country == "Tajikistan" & wealth == "richest"
							
				conindex fic [aweight=wt], rankvar(wealthscore) wagstaff bounded limits(0 1) 
				
						return list , all 				
						putexcel set "$dataout\region\Table_`name'", replace
						
						putexcel A1=("region")
						putexcel B1=("index")
						putexcel C1=("obs")
						putexcel D1=("value")
						putexcel E1=("SE")
					
						putexcel A2=("`name'")
						putexcel B2=("wagstaff")
						putexcel C2= matrix(r(N))
						putexcel D2= matrix(r(CI))
						putexcel E2= matrix(r(CIse))
						
				conindex fic [aweight=wt], rankvar(wealthscore) erreygers bounded limits(0 1)	
													
						putexcel A3=("`name'")
						putexcel B3=("erreygers")
						putexcel C3= matrix( r(N))
						putexcel D3= matrix(r(CI))
						putexcel E3= matrix(r(CIse))
		
		}
		