import { Coordinate, PuzzleData } from './types';

const toCoordinate = (index: number): Coordinate => {
    return { x: index % 3, y: Math.floor(index / 3) };
};

const toIdx = (coordinate: Coordinate): number => {
    return coordinate.y * 3 + coordinate.x;
};

const adjacent = (c: Coordinate) => {
    return [
        { x: c.x + 1, y: c.y }, // →
        { x: c.x, y: c.y - 1 }, // ↑
        { x: c.x - 1, y: c.y }, // ←
        { x: c.x, y: c.y + 1 }, // ↓
        { x: c.x + 1, y: c.y + 1 }, // ↘
        { x: c.x + 1, y: c.y - 1 }, // ↗
        { x: c.x - 1, y: c.y - 1 }, // ↖
        { x: c.x - 1, y: c.y + 1 }, // ↙
    ].filter(({ x, y }) => 0 <= x && x <= 2 && 0 <= y && y <= 2);
};

/**
 * Returns a list of indices to the word if it could be found, and null if it couldn't

 */
const findWord = (
    gridSize: number,
    grid: string,
    word: string,
    visited: number[]
): number[] | null => {
    // we have found the entire word, nothing else left
    if (word === '') {
        return visited;
    }

    // are just starting, the word could be anywhere
    if (visited.length === 0) {
        for (let i = 0; i < grid.length; i++) {
            if (grid[i] === word[0]) {
                const wordPath = findWord(gridSize, grid, word.slice(1), [i]);
                // we have found the word!
                if (wordPath !== null) {
                    return wordPath;
                }
                // otherwise, we have not found it. Maybe a different starting location will find it?
            }
        }
        // we have not found the word anywhere :(
        return null;
    } else {
        // we have already found a part of the word, time to look for the rest of it
        const lastLetterIdx = visited[visited.length - 1];
        for (const neighbor of adjacent(toCoordinate(lastLetterIdx))) {
            const neighborIdx = toIdx(neighbor);
            // have already visited this tile, ignore it
            if (visited.includes(neighborIdx)) {
                continue;
            }

            // we have not visited this tile, but it is wrong
            if (grid[neighborIdx] !== word[0]) {
                continue;
            }

            // Yay! Time to continue searching for the words
            const wordPath = findWord(gridSize, grid, word.slice(1), [...visited, neighborIdx]);
            // we have found the word!
            if (wordPath !== null) {
                return wordPath;
            }
        }
        return null;
    }
};

export class PuzzleLogic {
    puzzle: PuzzleData;
    letterBuffer: string[];
    constructor(puzzleInfo: PuzzleData) {
        this.puzzle = puzzleInfo;
        this.letterBuffer = [];
    }

    handleKey(key: string) {
        if (key === 'Escape') {
            console.log('clearing letter buffer');
            this.letterBuffer = [];
        }

        if (key === 'Backspace') {
            this.letterBuffer.pop();
        }

        if ('ABCDEFGHIJKLMNOPQRSTUVWXYZ'.includes(key.toUpperCase())) {
            this.letterBuffer.push(key.toUpperCase());
        }
    }

    undoKey(key: string) {
        const removedKey = this.letterBuffer.pop();
        console.assert(removedKey === key, 'Popped key does not match undo');
    }

    getCurrentWordPath(): Coordinate[] | null {
        const letterIndices = findWord(
            this.puzzle.sideLength,
            this.puzzle.letters,
            this.letterBuffer.join(''),
            []
        );

        if (letterIndices === null) {
            return null;
        } else {
            return letterIndices.map((c) => toCoordinate(c));
        }
    }
}