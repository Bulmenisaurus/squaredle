import { PuzzleLogic } from './puzzleLogic';
import { PuzzleRenderer } from './puzzleRenderer';
import { PuzzleData } from './types';

const main = () => {
    const puzzle: PuzzleData = {
        sideLength: 3,
        letters: 'namtirdgd'.toUpperCase(),
    };

    const puzzleContainer = document.getElementById('puzzle-container')!;
    const lineContainer = document.getElementById('line-highlight') as any as SVGPathElement;

    const puzzleRenderer = new PuzzleRenderer(puzzle, puzzleContainer, lineContainer);
    const puzzleLogic = new PuzzleLogic(puzzle);

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
};

main();
