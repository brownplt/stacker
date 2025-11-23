// Adapted from https://www.sitepoint.com/get-url-parameters-with-javascript/

const queryString = window.location.search;
const urlBase = window.location.href.replace(window.location.search, '');
const urlParams = new URLSearchParams(queryString);
export const syntaxAtURL = urlParams.get('syntax') || "";
export const randomSeedAtURL = urlParams.get('randomSeed') || "";
export const holeAtURL = urlParams.get('hole') || "";
export const nNextAtURL = parseInt(urlParams.get('nNext') || "-1");
export const programAtURL = urlParams.get('program') || "";
export const readOnlyMode = urlParams.get('readOnlyMode') !== null;
export const gcAtURL = urlParams.get('gc') !== null;
export const printTopLevelAtURL = urlParams.get('printOnRequest') === null;
export const make_url = (syntax, randomSeed, hole, nNext, program, readOnlyMode, gc, printTopLevel) => {
    const params = new URLSearchParams();
    params.set('syntax', syntax);
    params.set('randomSeed', randomSeed);
    params.set('hole', hole);
    params.set('nNext', nNext);
    params.set('program', program);
    if (readOnlyMode) {
        params.set('readOnlyMode', "");
    }
    if (gc) {
        params.set('gc', "");
    }
    if (!printTopLevel) {
        params.set('printOnRequest', "");
    }
    return `${urlBase}?${params.toString()}`;
};

// console.log(
// "syntaxAtURL", syntaxAtURL,
// "randomSeedAtURL", randomSeedAtURL,
// "nNextAtURL", nNextAtURL,
// "programAtURL", programAtURL,
// "readOnlyMode", readOnlyMode,
// "printTopLevel", printTopLevelAtURL,
// )
