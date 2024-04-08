type SubmitWordResponse = 'invalidWord' | 'alreadyGuessed' | 'success';

export class WordManager {
    allWords: Set<string>;
    unlockedWords: Set<string>;
    constructor(words: string[]) {
        this.allWords = new Set(words);
        this.unlockedWords = new Set<string>();
    }

    trySubmitWords(word: string): SubmitWordResponse {
        if (!this.allWords.has(word)) {
            return 'invalidWord';
        }

        if (this.unlockedWords.has(word)) {
            return 'alreadyGuessed';
        }

        this.unlockedWords.add(word);
        console.log(`successfully guessed ${word}`);
        return 'success';
    }
}
