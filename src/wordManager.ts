type SubmitWordResponse = 'invalidWord' | 'alreadyGuessed' | 'success';

const wordHint = (word: string) => {
    // TODO: more?

    return word[0] + '*'.repeat(word.length - 1);
};

export class WordManager {
    allWords: Set<string>;
    unlockedWords: Set<string>;
    popupWordContainer: HTMLDivElement;
    wordElements: Map<string, HTMLSpanElement>;
    constructor(words: string[], popupWordContainer: HTMLDivElement) {
        this.allWords = new Set(words);
        this.unlockedWords = new Set<string>();
        this.popupWordContainer = popupWordContainer;
        this.wordElements = new Map();
        this.renderPopup();
    }

    trySubmitWords(word: string): SubmitWordResponse {
        if (!this.allWords.has(word)) {
            return 'invalidWord';
        }

        if (this.unlockedWords.has(word)) {
            return 'alreadyGuessed';
        }

        this.unlockedWords.add(word);
        const wordElement = this.wordElements.get(word);
        if (wordElement === undefined) {
            console.error(`Word ${word} has no corresponding HTML element`);
        } else {
            wordElement.innerText = word.toLowerCase();
        }
        return 'success';
    }

    renderPopup() {
        const longestWordLength = Math.max(...Array.from(this.allWords).map((w) => w.length));
        for (let i = 4; i <= longestWordLength; i++) {
            const heading = document.createElement('h2');
            heading.innerText = `${i} letters`;

            const words = Array.from(this.allWords)
                .filter((w) => w.length === i)
                .sort();
            const wordGroupContainer = document.createElement('div');
            wordGroupContainer.classList.add('word-group');
            wordGroupContainer.style.setProperty('--word-length', `${i + 2}ch`);

            for (const word of words) {
                const wordContainer = document.createElement('span');
                this.wordElements.set(word, wordContainer);
                if (this.unlockedWords.has(word)) {
                    wordContainer.innerText = word.toLowerCase();
                } else {
                    wordContainer.innerText = wordHint(word.toLowerCase());
                }

                wordGroupContainer.append(wordContainer);
            }

            this.popupWordContainer.append(heading, wordGroupContainer);
        }
    }
}
