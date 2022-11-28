import pandas as pd
from guesslang import Guess


guess = Guess()

scripts_to_guess = pd.read_csv("../../data/German/scripts-to-guess.csv")

guessed_script = {"site" :[], "language":[]}

for index, row in scripts_to_guess.iterrows():
    print (index)
    language = guess.language_name(row["content"])
    guessed_script["site"].append(row["site"])
    guessed_script["language"].append( language)

df_guessed_script = pd.DataFrame(guessed_script)
print(df_guessed_script)
df_guessed_script.to_csv("../../data/German/scripts_guessed.csv")