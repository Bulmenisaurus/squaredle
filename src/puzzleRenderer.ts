import { PolygonalChain, PuzzleData } from './types';

const circleToPath = (x: number, y: number, r: number): string => {
    return `M ${x} ${y}
m ${r}, 0
a ${r},${r} 0 1,1, ${-r * 2},0
a ${r},${r} 0 1,1 ${r * 2},0`;
};

export class PuzzleRenderer {
    private puzzle: PuzzleData;
    private puzzleContainer: HTMLElement;
    private line: SVGPathElement;

    constructor(puzzleInfo: PuzzleData, container: HTMLElement, lineElement: SVGPathElement) {
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
            .map(({ x, y }, idx) => {
                // for the first element, we need to move the path here and draw a circle
                if (idx === 0) {
                    const circle = circleToPath(x, y, 10);
                    return `${circle} M ${x.toFixed(1)} ${y.toFixed(1)}`;
                } else {
                    return `L ${x.toFixed(1)} ${y.toFixed(1)}`;
                }
            })
            .join(' ');
        this.line.setAttribute('d', formattedPoints);
    }
}
