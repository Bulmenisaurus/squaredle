import { PuzzleData } from './types';

export const puzzle: PuzzleData = {
    sideLength: 3,
    letters: 'lytrotnem'.toUpperCase(),
    words: 'lone,lore,lyre,more,morn,mote,note,omen,rote,tern,tome,tone,tore,torn,tote,yore,loner,otter,tenor,toner,totem,rotten,lottery'
        .split(',')
        .map((w) => w.toUpperCase()),
};
