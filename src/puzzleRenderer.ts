import { PolygonalChain, PuzzleData } from './types';

export class PuzzleRenderer {
    private puzzle: PuzzleData;
    private puzzleContainer: HTMLElement;
    private line: SVGPolylineElement;

    constructor(puzzleInfo: PuzzleData, container: HTMLElement, lineElement: SVGPolylineElement) {
        this.puzzle = puzzleInfo;
        this.puzzleContainer = container;
        this.line = lineElement;

        this.createLetterTiles();
    }

    private createLetterTiles() {
        for (const letter of this.puzzle.letters) {
            const letterElement = document.createElement('div');
            letterElement.innerText = letter;
            letterElement.classList.add('letter');

            this.puzzleContainer.append(letterElement);
        }
    }

    renderWordLine(word: PolygonalChain) {
        const formattedPoints = word
            .map(({ x, y }) => ({ x: x * 140 + 60, y: y * 140 + 60 }))
            .map(({ x, y }) => `${x.toFixed(1)}, ${y.toFixed(1)}`)
            .join(',');
        this.line.setAttribute('points', formattedPoints);
    }
}
