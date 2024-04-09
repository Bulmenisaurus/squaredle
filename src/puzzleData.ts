import { PuzzleData } from './types';

// export const puzzle: PuzzleData = {
//     sideLength: 4,
//     letters: 'xevdtrirenaoyral'.toUpperCase(),
//     words: [],
// };

// export const puzzle: PuzzleData = {
//     sideLength: 31,
//     letters: `oeonrsmctunrmrpunegyxolupetntmi
//     unenommeeetomuirucciaamericiumt
//     iiuiuuuhcolugliroodarmstadtiuml
//     nmmuiiinheilpromethiumhbsddnmna
//     oturomdenlfmupagsoaieytterbiumb
//     rihityaiehmmiralxovcaliforniumo
//     oompbdrktuiuudieeetstndfdumtblc
//     bseaborgiummoinulkoieiruyueaies
//     iumlueinuuulioremdcppenimorbrro
//     urslbnonmnieniooiutihuriiicinmi
//     eofaecroentgeniumuitnepnunuidtn
//     dipdrrmdiscmeumcnruspihmmmrpnaa
//     dmiiyabuhnomuicinrepocoauuyinmh
//     rizulymeitneriumipovarsenictoun
//     ilumlumzicueomnutcpiitpiuncpbin
//     eetoisstrontiumiuroaslhsabilrsz
//     inmruoaloyuelhuveutcpnoayuraaen
//     nroumnshsmnarmiobtatilrrlditccm
//     ohmuicnarftbuwnrihsiieupildilth
//     tanundmyttriumaeaesnlastatinean
//     psmoiaauhaimvumlnniiicnoouuunmd
//     yshurmunimulairftiuuieteanmmmlr
//     rieiiiouanmagnesiummgtraogiylou
//     kuulnblravovrfgcmmeouhucnsuusne
//     empdeirhhtrlsauaoerrouitfttcmio
//     iuiltnteocduohdsndhdlkoseaurfe
//     uuihenirolhcfpioyeiciiettnrlatm
//     muinaruuoemuilehnussrubidiumuht
//     muillaggmgeivnuimfdegmuibretimu
//     mumtuiimlueeuousahbismuthpebuup
//     itrmlciruirnemycadmiumeaeeiimum`
//         .replaceAll(' ', '')
//         .replace('\n', '')
//         .toUpperCase(),
//     words: [],
// };

export const puzzle: PuzzleData = {
    sideLength: 3,
    letters: 'lytrotnem'.toUpperCase(),
    words: 'lone,lore,lyre,more,morn,mote,note,omen,rote,tern,tome,tone,tore,torn,tote,yore,loner,otter,tenor,toner,totem,rotten,lottery'
        .split(',')
        .map((w) => w.toUpperCase()),
};
