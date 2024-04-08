import { puzzle } from './puzzleData';
import { PuzzleLogic } from './puzzleLogic';
import { PuzzleRenderer } from './puzzleRenderer';
import { WordManager } from './wordManager';

const main = () => {
    const puzzleContainer = document.getElementById('puzzle-container')!;
    const lineContainer = document.getElementById('line-highlight') as any as SVGPathElement;

    const puzzleRenderer = new PuzzleRenderer(puzzle, puzzleContainer, lineContainer);
    const puzzleLogic = new PuzzleLogic(puzzle);
    const wordManager = new WordManager(puzzle.words);

    const tileValues = puzzleLogic.getTileValues(wordManager);
    puzzleRenderer.drawTileValues(tileValues);

    window.addEventListener('keydown', (e) => {
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
        console.log(`Listener userFoundWord ${word}`);
        const response = wordManager.trySubmitWords(word);
        console.log({ response });

        // if the user found a word, update the tile values
        if (response === 'success') {
            const tileValues = puzzleLogic.getTileValues(wordManager);
            puzzleRenderer.drawTileValues(tileValues);
        }
    });

    console.log(puzzleLogic.getTileValues(wordManager));
};

main();
