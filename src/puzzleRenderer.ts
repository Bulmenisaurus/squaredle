import { TileInfo } from './puzzleLogic';
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
    letterTiles: HTMLDivElement[];

    constructor(puzzleInfo: PuzzleData, container: HTMLElement, lineElement: SVGPathElement) {
        this.puzzle = puzzleInfo;
        this.puzzleContainer = container;
        this.line = lineElement;

        this.letterTiles = this.createLetterTiles();
    }

    private createLetterTiles() {
        const tiles: HTMLDivElement[] = [];
        for (const letter of this.puzzle.letters) {
            const letterContainer = document.createElement('div');
            letterContainer.classList.add('letter-container');

            const letterElement = document.createElement('div');
            letterElement.innerText = letter;

            const startNumElement = document.createElement('div');
            startNumElement.classList.add('start-num');
            startNumElement.innerText = '?';

            const includeNumElement = document.createElement('div');
            includeNumElement.classList.add('include-num');
            includeNumElement.innerText = '?';

            letterContainer.append(letterElement, startNumElement, includeNumElement);
            this.puzzleContainer.append(letterContainer);
            tiles.push(letterContainer);
        }

        return tiles;
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

    drawTileValues(tileValues: TileInfo[]) {
        const formatNum = (n: number) => {
            if (n === 0) {
                return '';
            }

            if (n >= 10) {
                return '+';
            }

            return `${n}`;
        };

        for (let i = 0; i < tileValues.length; i++) {
            const tile = this.letterTiles[i];
            const start = tile.querySelector('.start-num') as HTMLElement;
            const contains = tile.querySelector('.include-num') as HTMLElement;

            start.innerText = formatNum(tileValues[i].starting);
            contains.innerText = formatNum(tileValues[i].containing);

            if (tileValues[i].starting === 0 && tileValues[i].containing === 0) {
                tile.classList.add('eliminated');
            }
        }
    }
}
