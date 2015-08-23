lunch=read.csv("Lunch_Rsquare.csv")
lunch = lunch[,-1]
length(lunch[lunch>0.03753553])

lunch1=read.csv("Lunch_Rsquare1.csv")
lunch1 = lunch1[,-1]
length(lunch1[lunch1>0.0371239])

homeless=read.csv("Homeless_Rsquare.csv")
homeless=homeless[,-1]
length(homeless[homeless>0.03571468])

homeless1=read.csv("Homeless_Rsquare1.csv")
homeless1=homeless1[,-1]
length(homeless1[homeless1>0.03302692])


gender=read.csv("Gender_Rsquare.csv")
gender=gender[,-1]
length(gender[gender>0.006543204])

gender1=read.csv("Gender_Rsquare1.csv")
gender1=gender1[,-1]
length(gender1[gender1>0.009287218])

race=read.csv("Race_Rsquare.csv")
race=race[,-1]
length(race[race>0.0215214])

race1=read.csv("Race_Rsquare1.csv")
race1=race1[,-1]
length(race1[race1> 0.01746721])

country=read.csv("Country_Rsquare.csv")
country=country[,-1]
length(country[country>0.1046887])

country1=read.csv("Country_Rsquare1.csv")
country1=country1[,-1]
length(country1[country1> 0.1561849])

language=read.csv("Language_Rsquare.csv")
language=language[,-1]
length(language[language>0.04625235])

language1=read.csv("Language_Rsquare1.csv")
language1=language1[,-1]
length(language1[language1>0.05316651])

birth_country=read.csv("Birth_Country_Rsquare.csv")
birth_country=birth_country[,-1]
length(birth_country[birth_country>0.1046887])

birth_country1=read.csv("Birth_Country_Rsquare1.csv")
birth_country1=birth_country1[,-1]
length(birth_country1[birth_country1>0.1561849])

parent_language=read.csv("Parent_Language_Rsquare.csv")
parent_language=parent_language[,-1]
length(parent_language[parent_language>0.04327742])

parent_language1=read.csv("Parent_Language_Rsquare1.csv")
parent_language1=parent_language1[,-1]
length(parent_language1[parent_language1>0.04866746])



