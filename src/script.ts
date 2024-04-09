import { puzzle } from './puzzleData';
import { PuzzleLogic } from './puzzleLogic';
import { PuzzleRenderer } from './puzzleRenderer';
import { WordManager } from './wordManager';

const main = () => {
    const puzzleContainer = document.getElementById('puzzle-container')!;
    const lineContainer = document.getElementById('line-highlight') as any as SVGPathElement;

    const wordsFoundContainer = document.getElementById('words-found') as HTMLSpanElement;
    const wordsTotalContainer = document.getElementById('words-left') as HTMLSpanElement;

    const wordsPopup = document.getElementById('words-popup') as HTMLDivElement;
    const popupContent = document.getElementById('words-container') as HTMLDivElement;

    const openPopupButton = document.getElementById('word-opener') as HTMLButtonElement;
    const closePopupButton = document.getElementById('close') as HTMLButtonElement;

    let isPopupOpen = false;

    const openPopup = () => {
        wordsPopup.style.display = 'block';
        isPopupOpen = true;
    };
    const closePopup = () => {
        wordsPopup.style.display = 'none';
        isPopupOpen = false;
    };

    const puzzleRenderer = new PuzzleRenderer(puzzle, puzzleContainer, lineContainer);
    const puzzleLogic = new PuzzleLogic(puzzle);
    const wordManager = new WordManager(puzzle.words, popupContent);

    // update tiles
    const tileValues = puzzleLogic.getTileValues(wordManager);
    puzzleRenderer.drawTileValues(tileValues);

    // update title
    wordsFoundContainer.innerText = wordManager.unlockedWords.size.toString();
    wordsTotalContainer.innerText = wordManager.allWords.size.toString();

    window.addEventListener('keydown', (e) => {
        if (isPopupOpen) {
            if (e.key === 'Escape') {
                closePopup();
            }

            return;
        }

        const key = 'abcdefghijklmnopqrstuvwxyz'.includes(e.key) ? e.key.toUpperCase() : e.key;

        puzzleLogic.handleKey(key);
        const currentPath = puzzleLogic.getCurrentWordPath();

        if (currentPath !== null) {
            puzzleRenderer.renderWordLine(currentPath);
        } else {
            // if this path does not exist, do not record it
            puzzleLogic.undoKey(key);
        }
    });

    puzzleLogic.userFindWord.subscribe((word) => {
        const response = wordManager.trySubmitWords(word);

        if (response === 'success') {
            const tileValues = puzzleLogic.getTileValues(wordManager);
            puzzleRenderer.drawTileValues(tileValues);

            wordsFoundContainer.innerText = wordManager.unlockedWords.size.toString();
        }
    });

    openPopupButton.addEventListener('click', openPopup);

    closePopupButton.addEventListener('click', closePopup);
};

main();
