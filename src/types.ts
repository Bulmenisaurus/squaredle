export interface PuzzleData {
    sideLength: number;
    letters: string;
}

export interface Coordinate {
    x: number;
    y: number;
}

export type PolygonalChain = Coordinate[];
