export interface PuzzleData {
    sideLength: number;
    letters: string;
    words: string[];
}

export interface Coordinate {
    x: number;
    y: number;
}

export type PolygonalChain = Coordinate[];
