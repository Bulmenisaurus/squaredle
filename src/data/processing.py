import sys
import json

# Expects a list of words gathered from the puzzle in "puzzle-words.txt"
# Expects https://github.com/actiniumn404/Squaredle-Maker/blob/master/data/words.json in "words.txt"
# Will output a list of all common words (freq >3.3) puzzle-good.txt and uncommon to puzzle-bad.txt

with open('src/data/words.txt', 'r') as f:
    frequencyData = json.load(f)

with open('src/data/puzzle-words.txt', 'r') as f:
    words = json.load(f)

common_words = []
uncommon_words = []
for word in words:
    if word not in frequencyData:
        uncommon_words.append(word)
        continue
    
    if frequencyData[word] > 3.2:
        common_words.append(word)
    else:
        uncommon_words.append(word)

with open('src/data/puzzle-good.txt', 'w') as f:
    json.dump(common_words, f)

with open('src/data/puzzle-bad.txt', 'w') as f:
    json.dump(uncommon_words, f)