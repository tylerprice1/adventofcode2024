/**
 * @typedef {{
 *   row: number;
 *   column: number;
 * }} Position
 *
 * @typedef {Position[]} Path
 */

/**
 * @param {string[]} grid
 * @returns {number}
 */
function part_1(grid) {
  const allPaths = new Array(grid.length).fill(0).flatMap((_, row) => {
    return new Array(grid[row].length).fill(0).flatMap((_, column) => {
      return _search(grid, { row, column });
    });
  });

  const paths = allPaths.map((path) => {
    return {
      path: path.map((p) => ({ ...p, char: grid[p.row][p.column] })),
      str: path
        .slice()
        .sort((a, b) => a.row - b.row || a.column - b.column)
        .map(({ row, column }) => `(${row},${column})`)
        .join(","),
    };
  });

  /** @type {Set<string>} */
  const pathStrSet = new Set();

  const uniquePaths = paths.reduce((unique, { path, str }) => {
    if (pathStrSet.has(str)) {
      return unique;
    }
    pathStrSet.add(str);

    return [...unique, path];
  }, /** @type {Path[]} */ ([]));

  return uniquePaths.length;

  /**
   * @param {string[]} grid
   * @param {Position} position
   * @param {Position} [delta]
   * @param {Path} [path]
   * @returns {Path[]}
   */
  function _search(grid, position, delta, path = []) {
    const str = path.map(({ row, column }) => grid[row][column]).join("");
    if (str === "XMAS" || str === "SAMX") {
      return [path];
    }

    const { row, column } = position;

    if (
      row < 0 ||
      row >= grid.length ||
      column < 0 ||
      column >= grid[row].length ||
      path.length === 4 ||
      !"XMAS".includes(grid[row][column])
    ) {
      return [];
    }

    const newPath = [...path, position];

    if (delta) {
      return _search(
        grid,
        { row: row + delta.row, column: column + delta.column },
        delta,
        newPath
      );
    }

    const newRows = new Array(3).fill(0).map((_, i) => i - 1);
    const newColumns = new Array(3).fill(0).map((_, i) => i - 1);

    return newRows.flatMap((r) =>
      newColumns.flatMap((c) =>
        r !== row || c !== column
          ? _search(
              grid,
              { row: row + r, column: column + c },
              { row: r, column: c },
              newPath
            )
          : []
      )
    );
  }
}

/**
 * @param {string[]} grid
 * @returns {number}
 */
function part_2(grid) {
  let count = 0;

  for (let r = 1; r < grid.length - 1; r += 1) {
    const row = grid[r];

    for (let c = 1; c < row.length - 1; c += 1) {
      const char = row[c];

      if (char === "A") {
        const topLeft = grid[r - 1][c - 1];
        const topRight = grid[r - 1][c + 1];
        const bottomRight = grid[r + 1][c + 1];
        const bottomLeft = grid[r + 1][c - 1];

        if (
          ((topLeft === "S" && bottomRight === "M") ||
            (topLeft === "M" && bottomRight === "S")) &&
          ((topRight === "S" && bottomLeft === "M") ||
            (topRight === "M" && bottomLeft === "S"))
        ) {
          count += 1;
        }
      }
    }
  }

  return count;
}

function main() {
  const test = `
    MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX
  `.split(/\s+/g);

  const puzzle = `
   SXMSMMXMASMSSSSXAMXMXAMXXAAXMAMSAMMXMAMXSAMXMXMMMMSMMXMXMASMMSMMSSSMAXXMSMSSXMASMXSSSXSMMSSSSSXMASXSAMXSMMSSMXXXXXMASXMXMSSSMMSSMSSSMMMMSSXM
SAMAMSMSMSXAAMAAXMSXMSSSXSXMSMMSASASXSMSMSSMSSMXAAMXMASXSAAMAXASAXMASXSASMASMMSMMAMXSAAXAAXAAAASAMAXAXASAAAAMMSMMSMMMASASAMXSAAAAMASMAASMMAS
SAMXSAAXAMMMMMMXAXSAMMAMAMXMAAXSXMASAAMSXAXAAAXSMSSSSXSAMXMMASMMMSXAMSAAXMAMMMAAMASAMXMMMSSMMMMMASMXMMXSMMSXMAAAAAAAXMASMASAMMSMMMAMMMXMAMAM
SAMXSMSMAMAAXSXSSXSAMMAMAMASXSMSXXXMMMMSMMSMMXMSAAAMXXMXMXXMXXXASAMXMSMSMMXXAXXXMAXAMSXXAAMXSAASMMMSXAAXMAXMSMSMMSXMXMMMSXMXSXMMMMXSXSMSSMMS
MSSXMAXMASMSSMAXMAMXMMASAXMXAMAMXMMMXMXMAMXXAXSMMMSMXMMAMMXMAMSMMMSSXSAXMSASXXSXMSSMMSAMMMSAMMMMAAAAMMMSSXMAMAAAAMMSXXAAXXSASAAAXMASAAAAMAXA
XAXASMXSAMAMAMMMMAMASMMMMSXMSMSMSAASXSXSAMAMSMSAXSAMXAAAXMAMAMAAMAAMAMAMAMASAASAMAAXAXAMASMMSSSSSMMSAMAAMAMAMSSMMSAMMSMXXMMASMSMMSAMXMMMSMMM
MMSMMMAMMSXSAMAASAMXMAAAXMAXMAXAMSMSAMASXSXAMASMMXAAXXXMXMMMASMSMMSSSMSMSMAMMMMAAXXMXMMMXMAAAAXAAMAMMMMASMMXXAAAAMAMMAMXMXAAMXMAAMXSASXMAXAX
XAAAAMMSAAASXMSASASXSSMMSMMMSAMSAMXMAMAMMAMSMAMMXSXMSMSASXMSXSAMXMAXMAXXXAXXXASXMASMXSAMSSMMSXMXMMAXXASAMAASMSSMMSSMMASAASMXSAMMMSMSXSASASXS
MSSMMSMMMMMMMMMAXAMAAXMASASASXMXMAMSAMXXSAMXMAMMXSMAAAMSMAASAMXMSXMXMAMSSMMMSASMMXXAASXMAAAMAAMSXSSSMMMSMMMSXAXXXAAXSAXAXMAASXSAMXAXXSAMXSAS
XAAASXXAAMAAXAMSMMMXMMMMXAMAXAMXAMMSXSXMXSSXSAXXASMMMSMAMMMMAMMAMXXXMASXMAAXMAXASMMMMSAMSSMMMXMAAAMAAMAMMMSMMMSMMMSAMAXSAMXMSAXXSMMMAXAMAMMM
MMSMSAXSSSMMMMAXAXMASAXSMSMXSAMXXXAMMMMMAMMMSMSMASXMXMMMMAXSMMSASMSSSXMASXMXMMMAXMAMSSMMMAMXXSSSSMSSMMASAMAAAAAAAAMMMXMAASAXMXXMXMASMMSMMSAS
AXAXMAMMAXAXAMSMSMMAXMXSAXAMSAMXMMMSAAAMAMSAMXAMAXASXMASMMMMMASAMAXAAXSMMMXAAXMXMMMSAMASXAAAAAAAXAXXASASMSSSMSSXMXSXXXMXAMXXSSMMAMMAXSMAXXMA
XSASMMMMAMMSXSAAMAASXSASXMAMMSMASAMXMXXXAXMASXXSSSMMXMASAAXAMMXXMSMMMMMAAAXSXXMSASXMASXMSAMMMMMMMMMSMMXSXXXMAXMMSMSMSMSAMASXSAAAASMMMAMMMSXM
XSAMMAXMMMMSMSMSMSMMAMASASXMAAXXMASAXXSMSSSMMAXAAMAAXXASMMSSSSMSAMASAAXMMMMMAAAAMMASXMXMAXXXAAAMXSASAXMMMSSMXMAXSASAAAXXSAMXMSMMXMAASAMSASAX
AMAMSXSXMASMAMAMMMAMMMXMXAMMXSSMXXMAMSAMXAAMASMMSMSASMXXAASXAAASASAMSSSSSXASMMXMASAMASMMMAMXSSXSAMXSAMSAMXAMASMMMAMSXSXXMAXAXMMSMXSMMMMASXSM
AMMMXAMASMMMAMAMSSMMXSASXMMSAMAMAMMMMXAMMXMAAXMXXAMXXAXXMMSMSMMSAMAXXXAAAMXAXMXMAMASAAAAAAMXMMXMMMXMMMSASXMMXAAAMXMXXMMMSSMMASAAMMMMASMMMMMM
MSSMSAAAXAASMSMSXAXMMSASAAAMASMMASAMMSMMMXXMSSSMMAMXMSMXSAMMASMMMMSSMMMMMXXSXXAMAXAMXSSMSMSAAAXMAMXAMXXMAAXAMSMSXMXMSMAMAMAMAMMMXAASAXMXXAAM
MAAXSAMXMSMSAMXSMXMXAMAMMMXMMMXSMSASAAMAMAXXAAAMSAMAMAAMMAMSAMMAAAXAAAXAXMMMMMMMSXMMAAAAAASMSMSMAMSAMXMXSSMXAMAMMMAXAXAXXXAMASXXSSMMMSSMSSMS
MMSMSXMAMXMMXMAMMSMMXXAMXXSSXSASASAMMXSAMSSMMSMMSMSMSASXXAMMMMSXMXSSSMSMSMAAAASMMASMMSAMMMMAMAMXXXASMMSAAAASMMAMAXSSSXSSMMASASMXAXAAAAAAAAAS
MMAAXAMXXSAMXSSMASMAMXSXSSMASXAMMMMMXMXAMAAXXAXXSXSASAMMMSSMMASMMMXAXXAAAMMMSASAXXMAAAAXMXMAMAMMMSMMXAMMSMMAAMMXSMMAXAMAMSAMAXAMMXSMMXSSMMMS
MSMSMMSAMXAMAMAMAXMASMMAMXMXMMSMSAMSXMSSMSSMMSXXMAMAMAMSAAAXMASASMMAMSMSMSMAMXSAMSMSMSSMSASASASAAXXXMMMMAAMSMMSAMASAMXMAMXMMXMSXSAXMAXMAMMAX
AAXXAAXAXMAMXXAMXXMASAMSMMMSMAXMMAMMAMAMMXAMAXSMMSMSMXMMMSSMXXSAMXMAMXAAAAAMXAMMMAAXAAXASXSAXXSMSMMXMXSSMSXAAAMASMMMSXMAMAXMXXXAAXSXMASAAMAM
SXXMMMSXMMSXMSSSMMMASXXAAAAAMAMMMSMXAMASMXAMXMASAAXMSMXMAXAAXMMMMASAMMSMSMSSMAXMXMXMMMMAMMMMMMXMXASAMAMAAXMMMMSAMMASAASASXSXSAMMMSMMXXMAMMAS
AMSXSAMXSAMXAAAMAMSXMASXXMMSXSAAAXASMSXAMMXXAAXMXSMMAXMXMSMSMXAXSXSASAAAAXAXMAMSMSMXMMXXXAAAXXAMXSMXSASMMMXXAXSXAMMXMAMXAAAAXAAXXXMASMXSAMAM
XMAXMASAMASXMMSMSMSASXXMASXMAMMMMSASAAAMXASXMSMSMMASMMMAMMAMASAMMASAMXSSMMMSMSXSAAAASAMXSMSSMSASMAAASXSAMAMSMMMXSSSSXSMSMMMMMXSMXMMASAAAAMXS
SAMXSAMXSMMXMAXXXASXMXAMXSAMAMXXXMXMXMMXMMAAAAAAASAMXMSASMAMXXXAMMSMMAMAMXXAMMAMSMMMSAAMMMMAMSAAXMMMSASXMAMAAAXAXAAMAAXAXMAXSXMMSAMXMMMMXMXM
MAXAMASXSAMXSASXMAMXSXMAMMAMMMXMMXXXSMSXSMSMMMSMMXXSAMSASXMXAMXMSAMXMXSAMSSMMMAMXSXMXMXSAMSAMXMAXMAXMMMMSSSSSMSXMMMMSXSSSSSSMXAAMXMXSASMMMMS
SXMASMMMSAXXMASAMXXAXASMXXMMXXMSAAMMMMAMSAXAAXXXXSMMAMMAMMSMSSXAMXMXXAXAMMAMXSAMMAMSAMASAMXASMMSMSMSSXAASAAXXAAMMXMAXMAMAMXAMSSMSAAAMAAMAAAX
MAMAMASASAMXMAMXMMMMSAMXXSSSSXAMXMMAAAASMAMSMSMSMMXXXMMMMASAAAMMMSSSMSMSMSMMMMASXAMMMMASAMSSMXAXXXMXMSMSMMMMSSMAAXMXXAXMXMSAXAAASMXXMMMSSMSS
MMMASMMAXXAMMMSMMMAASXMMMAAAMMAMXMMMMSMSMSMXXMASAAASMMXSMASMMXAAAAAXAMAMMMAAMMMMMXSXXMASAMMXAMXMMMMAMSXAAXMAXAXSSXSMXSXSMASXMMSMSASMXSMAXXAX
SASXSXMMMSXXMAAAASMMSASAAMMMMASMSSMSAXAXAAMMSSSMMMXSAMMMMXXAXXSSMXSMSMAXAMSMSASASAAXMAXSAMXMMMAAAAXAXXAXXMMXSAMXAASAMAAAMAMAMAMXMAMXAAMASXMM
AMSAMXMMASAMXSSSMSMXSAMXSXXMXSAAAAAAAMAMMMSAMXAMXMXSXMAMXASXMXMAAXMXMMXSMMMXSASASMAMSXAMAMMAMSSXSSSSSMMSAAAXAMXMMMMSAMXXMASXMASAMSMMAMMXXAAA
SXXAMXMMAXAAMAMAMXAAMAMXXMAMXXMMMMMMMMXMSAMASXMMAXMSASXXMAASMSXMSMXAXSASXSAAMXMXMASMXXXXAMAMXAXMXAXAAMAAXMMSAMMXSXXAXXMXXAXAMXSAMAAMMXMAXSMM
ASXSMXMMSSMXMAMAMMMASAMXMSMXXSXMAXAXMXAAAASAMAASASASXMAASXMXXAAXAXSMMMXMASMXXAMXSAXXSMMSMSSXMASMMSMXMMMXXMXMAXSAMXMMMAMXSSMSMMMMSSMMMAMSMXAA
MAMAAXXAMAMASMSSMMXMMASXMAMXMASASXSSMMSMSAMXSSMMMMAMMMMMMAASMSSMMXXXASAMXMMMSSSMMASAMAXMXAMXMSAMXMASMSSMSSSSMMMASMAAMAXAAAAXMAAAMMSXSAAAASXM
XXMAMMMXXAMAAAAAASXMMMMMSASXMASMMXXAXXXAMXMAMMMXXMXMAMXMSMMMAAAAXSASAXXMMAXXAAAAMAMXMSMSASMMXMAMXMAMAAAAMXAAAXSMSMMSMSSSMMMMSSSMSAAMSMSMXMAA
XXASASXASXMSSMMSMMAMASXXSAXXSAMASXSSMMMMMAMXMAMXMMAMXMAAXXXXMMXMMXAMMSSMSAMMMSMMMASAXMAMMXAXAMAMSMMXMXMMMMSMMXMAMMSMAMAXAMXMMMXAMMMXXAXMASXM
XMAXAAXMASAAXMAXAXXMAMAAMAMXMXMAMAAAMAAMSXSSSSMASAMMSAMXMASMXSXMXMXMMAXAAAMXAXAAMXSAMMSMXSAMASAMAAAMSASASAAAXMMAMSASMMAMMMAMAXMXMSMSMSMMMXAA
SXAXXMMXAMMSMMMMAXSMSXSAMAMXAAMSMMMMMSXMXMAXAAMASAAASASAMMMMAXMAMSAMXMMXMMMMXSSMMMSAMAMXAXAMXSXMSSMXSASAMSSSMMSMSMAMXMXMMSMSMSAMXAAXAAAMSSMM
AMXSSMXMXMSAMASMXMMAXAMXSXSMMMXMAMMXXXSSMSMMSMMMXXMASMSASXAMXSMMMMSAAXMMMAXMAXASAASXMXXMSSMMXSAXMAMXMMMAMAMAMAAMXMAMXMSSMSAXMAMXSMSMMMMMAASX
MAXAMXAMMMMASAXAMXMAMAMAAAMXSAMSSMAAXMASAAMMMMSSMXMAMMSAMMSSMMMSAAXXXXMASMSXMMAMMMMASMSMMAMXAXMMSAMXMXSAMXSAMSMMASASAMAAAMXMMSMMXAAXXAAMSXMM
XSMXXSXSAXSAMXSSXMASXXMXSXXAMAXAMXXMMMMMMMSAASAXAXMXSAMAMAXXMSASMSMASMMMXAAXXMAMSASXMAAMMAMMMSXXMAMSAMXMMXMXMMXSAXXMMSSMMMMSAAASMSMSSSSMMASA
SXSAAMMSAMMXMAMXASAMXSMMMAMSSMMSSMSMSAAMMXSMSMMXSAMAMXSAMXSMXMASAMXMMAMAMXMMXMSXSASMASMSSXMAAMXMSXMAMMAXMAMMMMXMMSMSAMXMAASMMSSMAAXXXAMAMAAS
SASMXMAMXMAXMAXMAMXSAMXAMAMAAMAMAXAAXSMSAASXXMXAXAMAXMMAXMAXAMAXMASXSSMAXSAMXAMXMMMXMAXAASMMSSSXSAMSMSSSSSXMAMXAAAXAMASXMXSAMXMASASMMXMAMMMA
MAMXMSSSSSSSSSSMSMMMXSAMXAMXMMASMMMSMXMXMASAMXMXSASXSMSAMSMXXXMXXASXMASAMSAMXSSSXAAXSMMMSMMAXAMASAMMAXAAXAASASXMMMSAMXSAXASMMAXMASXAXMSXSSSM
MAMAAXAAAAXAAAMAAAAAMMASMXMASMXSMMMAAXAMXAXAMAAXSAMXAASXMAMMSAMSMXSASXXXMMAXAXAMMMSXMXMSMMMSMAMAMAMMAMMMMSMMASXASAMXXXSAMXSASMSXXMMMMMMXMAAX
MAXXMMMMMMMMMMMSSSMSSXAAMXXMXMAMAAMMXXSSMMSAMXSAMXMMMMMXSXXASAMXMMSAMMMMSSSMXMMMASMMXAXMAXAMXXMXSAMXXAMAMXAMAMMMMMSMAMMASXSAMXMXAMAMXXXAMSMM
SSSMMAXAXXXSMSXMAAAAMMMMXASXMSSSMMSMSMMAAASXMXMXMAXAXSSMMSMASXMAMAMSMAAAXAXAAMASMSAXXMSSMMSMMMMXAASMSSMAMSAMMSMSAMAAXXMAMMSASAMMSSSSSSSMMAMX
XAAASXMMMAAMMSAMXMAMXMASMMSAAAAMMSAMAASMMMMMMASMSMSSXMAAAXMMMMSASASMSXSXMXMSASASXSXMSMAAAAMAAAMAMMMAAAXAMAMXAAMMASMSMXMAMAMMXAXAAAAAAAMSAAMX
MMMMMMASMSMXAMXMAMXXMSAXAASMMMMMMSASXXMAXXASMXMAMXAMASXMMSMAMASMSXSAMXXXMMXAXMAXAMMAAMSSMMSSMMMMSMXXMSMXMXSMMSMSAMXMASMMMSSSSSMMSSMMMMMAMSMM
ASXSMSASAAAMSSMXXSAAXMSMMMSXMXMAXSAMXMMSMSXSAXMMMMASXMASXXXAMASASMMXMAMAMSXMAXSMASASMXMAXMMMMSMSAMXSAAAASASAXAAMASXMMMAMXMAMAMMMAMMXXXXMAXXA
SXAAXMASMMMMMAXMASMSMAMXMASASXSMMMSMMSMMASASAMSMSAAXMMXSXASMSASMMMSMMASAMAMXMAMXXMAMXSXAMMAAAAMXAMAAMMSMMASMMMSMSXMAXSAMAMAMAMASAMMAMXSMXMAX
MMMMMMAMXXXASAMMAMAXMAMAMAXAMAXSXAXMAAAMSMXMAMAAAMMMSMAMMXMMMMSASAAMSASXSAMAMMSASMAMAMMMSSSMSSSSSMSMXMAAMXMAAXAXMASMMAMSXSXSXSMSASXXXASMSSSM
SAXMASMMXASXMAXMSSSXMSSSMSMSMMMMMMMSSSSMMMAXAMMSMXSAAMSSSSSMSSSXMMXXMASASXSXSAAAMMMMMSMXAAAAXXMAXAMASXSSMMMSMSMSMXMMAMXMASMMXSXMXMASMMMAAAAS
SMMMXMAASMMMXSMAXAMASAAAAXAMXSAXSAAMMAAMXXSSSSMXXAMMASXAAAAAXASAXMXMMMMMMMMXMXMSMXSAMAAMXSMMMSMAMXMASAMAXAXAAAAAAAXMAXXMAMASAMMXSAAXAAMMMXMM
MAAXAXMMSAAMAXMMMAMXMMXMMMASXXASMMXMMMMSXXMAXMXSMASMMSMMMSMMMSMMMMMMAAAAAAXAMXXAAASASMSMMMASAMMXSMSAMAMMMMSMSMSMSMSSXSXMASAMAMAXAMXMSMXSSMAX
XSXSSSMASXMMMMASMMXMAXXXXMSMASXMXMMSSMASMMMAMMASMAAAAXXXAXXXSXSXAAASXSSSSSMMSAMXMMSAMMAMXSAMAXXAXAXXMAMXAXAXAMXAAXXMMMAMAMXSMMSMMSMXMMSAXSXM
MMXAAMAMXXXMXMAMMXAAASMMAXXMAMMAXXAAAMXMAMXMMMAXMSSMMSXMMMMXMASXXXMMXAMMAMXSAMSAXXXMXSAMMMAMXASMMMMMSMMSSSMSAMXXXSAAAMAMASMSXXXAXAXAAXMMMMXM
XMASMMSAMXMXSMMSMSAXMAAXSSSMSSSMMMMSSMSSSMMSXMMSAAAAAXMAMASAMXMASMSSMSMMAMSXAMSMSMAMXMAXMMAMXMAXAMAMASAAAAMAMASAAXMAMSASMMMXMMSMMMSMSMMXMMAA
ASAXXAMAMASAMXMAMMMMMSAMXAAAAMAAXXAMAMAAXMAMAMSSMSSMMSAXSASASAMAMAAXAAXSXSSMMMXMAMAMMSMMSSMSMXMXXXASMSMMXSMMSAMMAMAXMSAMAXMMSAAAMXXAXAXAAXAS
MMAXMXXAXSMMXXSASXSAAXMMMMMMMSSMMMAMAMMXMMSSSMXSAAMAMXXXMXXMMMMSMMMMSSMMXXXAAAASASXMXAAAMAAAXAXXMSASXSASAAXAMXSXMMMMAMMSMMXAMXMMMMMMMSSSSSMM
MMSMMSMXMMASAASMSAMXSXMXASXSAXXAXMASMSXAMMAAXSSMMMSMXMASMSSSSXAAAAXMAMMSSSSSMSMSXSAMSSMMSMMMSASAAMAMASAMSSMXSMSMSAASXMAAXAMSSSSSSXSAAXXMAAAX
XAXAAMMMASAMMMMAMMMMAAMSMSASXMSMMMMAAMXXSSMMMXMAMXMMAXMASAAAXMSSXMSMASXAAMAAAXAMAMAMAAMMAXXXXAXMXMAMAMAMXXXXAAXXMSXMAMSSMMAXXAAAMAXMXXAMSMMM
MMSMMSXAMMMMXXMMMXMMMSMAAMAMAAAXXMMXAMXXMMAMMMSXMASMSSSXAMMMMMMXAASMASMMSMSMMMASMSSMSXMMAMMMMSMMMSASASMMMAMSMMMSMMMSMMXAMXSXMMMMMMMMAMSAMXAA
XXAAAXMSXSMSSXSAMXSAAAXXMMAMMSMSASMSAMXAXSAMAAAASMMMXMAMMSXXXSASMMMMAMAXAMXMAMXAXAXAXMMMXMAAAMAAAXASASXAMAMAMAAAAAAAMXMAMAXASASXAASMASAAMSSM
XSSSXSAXASAAXXMASASMSMSSXSXSXAMAAAASAMSMMSXSMSSMMMSSSSMXAAXMAMMSMAMASXSSXSXSSSMSMMSAMXSAMSSSSSMMSSMMAMMXXAXASMMXSMSMSASAMASMMXSSSXSSXSAMXAMA
XMAAAXAMMMMXSASAMAXXXMXMAMAMXXSMSXAMAAMAMXMSAMXAXSXAAMXMMSSMAMXXXAXAAAXMASXAAXAAAXMXMAMAMAAAAAMMXXXAAMMMSSSMSXSAAAAMSMSXMAXAMAMMMMMMMMAXMASM
MMMMMMXMXAXAXAMSSXMSSMAMAMAMMMXMMMXSMMMAMXAMSMMSMAMMMMAASAXXAMXMXSSMMMMMAMMMMMMXSXMAMXMMMSMXMASMMSMXMAXMAMAAXAAMMMMXSAMXMSSMMAMAAXAMXXXMMAMM
AAXMASAMXXSMXMAMXAMXASMSXSXSXXAAXSMXASXSSSSMAXXMMMASMSASMXXMXSXSAMXMASAXMAMSAAXSMMSASMXXAMXXXXAXAAXMXSMMSSMMMSMSSXXAMAMMMXAXSASXXSAMXXSAMAAX
SMXXAMASXMXXSXSXSMMSMMMMMMXMAMMSASAMXMAXAAXSSXSMAXAMXMMAMMSSXSAMSSXSASXMAMXXXXXAAXSASAAMSMSMMSMMSMMSAMXAAAASXMAAXAMXXSMMSMMMMASAASMSAMSAMXSM
XAAMXSMMAXSAMXMASMXXAAAXSSSMXSAMXXMAMMMMMMMAXAXSMMAMAMSASAXMASMSAMXSAMMSMXMXSSSSXMXMMMMMMAMAAAXMMXAMASMMSSMMAMMMSMXMSMAAXAAAMXMMMMXMAMSAMAXM
AMSSMAMSXMMAAMMAXMXMSSSMAAAAAXXSAMMSXAAAAMSAMSMSXMAXAXSAMASMMMXMAMAMAMMAMMMMAAMXXMXXMMSXMAMSSMSAMXXXAMXMAXASXMAMAMASASMMSSSSSMXXMMMSSMXAMXMX
SXAAMAMMMXSMMMSAMXSMAAMMMMMMMMMXAMXMSSSSSXMASMAMMMMXMMMXMASASMMMSMXMAMXAMAAMMSMMAMAMMAAAMSMMAAXMMSSMASMMAMMMMXAXASMSASMXAMAMAAMXXMASAMSXXXXX
MMSSMXSXXAXASASASASMMXMMXSXASASXSMXXAMXAAXSXMMSMSASASXSAMMSMMXMAXASXSSSSSSXSAMASXSASMMXSMAASMMSXAAAMXMASXSAAAXSXMSXSAMXASMAMXSAMXMXSAMAMMMMS
MAMXMASMMSSMMASMMASXSMSAASMXSXMAAMMMSSMSMMAMXXAXSASASASMSMMXSAMXSAAAAMXMAMAMASAXASAMXMAXMXMMAAXMMSSMXXXAASMMSMMAXXAMXSXMXSMSXMMMMSASAMMAAAAA
MASAMASXAAAAMAMMMSMAXAMMXMAXXMMSMMAAMXXAXMASXSXMMMMAMAMMAXXXMAMAMXMMMMAMAMMSMMASMMSMAXMMAMSXMMSMMAXAMXMMMMXXMASMMMXMAAAXAXXMMMAXAAAMXMXSSSSM
SASXMASMSMSMMMSXSAMXMSMSMSXAAMMMXSMMSASMXMAXMAXSAMMSMXMSASMMSSMMSSMMMSXSASXSASAMXAMSXMXMSMSASXAASMMMAAXXXXXXMAMMASAMXMXMMSXMAMSMSMSSMXXMAMAX
MASAMXSMMXXAMXSMSASAMXAAASMMMMAMMMXSMAMMAMMXSAMXAMAMAMAMAXAAAAMXAXAAMMMXMXXXAMXSASMMSAXXMASAMSSXMAXMSXSXXMASMAMSSSXSSSMAAMXMAXXAXAMAAASXMSSM
SAMXXMSASASASAMAXSMAMMMMSMAXMSMSAMSAMXMXAMMMMASMSMMSAMMMSSMMSSMMSSSMSXAXSSSMAMAMAMAAXSMXMXMXMXMAMMMXAASMSAMXMAMMAMAXAAXMMMXXMXMAMXMMSMMAXAMX
MAXMAXSAMASXMAXAXXXAXXAMAMMMMMXSASMMSMSSMXAASAMMMAAXASXAAMMMAAMXAAAAXMMSMAXSAMASASAMMXXMMMSMMAXAMXSMSXSAXXXMXMXMAMXMXMMXASMAMSMMMAXXXXSSMSSS
SAMASMMXMAMMAMXSSMSMXSASASMSXSAMXMMAMMXAASMXMXSXSMMSMMMMXMAMXAMMMSMMMMMAMMMSXMXMXSASAXMAMAAAMMSSXXMAXAMXMSMSAAMSSMAMMAXSAXSAMXAAMMXMXMXAAAAA
MASMMAAXMASAMXAMAAAMXXAMASMSAMXXXMMASMSMMMMAAAMMMMMAMXSSXSXSASAMXAAAAASMMXXMASASMSAMXSASMSSSMXAMXSMSMMMSXAAMMMMAAMASMMMAMMSMSMSMSSSXAMSMMMSM
SAMXSMMXMASASMSSSSSMMMMMXSAMSMMMSSSMMMAAMASMMAXAAASASXAXAAASAMXSSSSMSAMXSXAASAMXAMXMAMXMXXMAMMASASAAAAAXSMSMSAMXSMXMAAXMMXXXSAAAAAMXXXAMXMAX
MMSASAXXMASAAAAAAAMAAAXMAMXMMSAASASAXXMXSASXXMAMSXSASMSMMMMMAMMMAMXXMMXMAXMXXMXMSMAMMMAMXMMMXSSMAXXAXMSMMXXAMASAAAASXMMXMAMAMSMMMXMASMMSSSMS
SMXAXMMSMASMMMMSMSSSSSSMASXMAMMXSAMXMMSAMXSMMMSAAXMXMAMSXAXAMXMMMMSXSSMSAMSASXMAXSMSSSMSMSAXASAMXMXSAMXAXAMXSAMMSMXAMXSAMAMXXAXASXMMSAXXAAAA
MMMSSMAXAMSAXMXXAAAAAAAMASMSMSMAMAMXMAMASMMXSAMMAMXAMSMSSMXXMAXAXXXAAAXMAMMASXMAXAXAXAAAAMASMSXSXMAMMXSXMSSMMXMAXMMXMASAXXSMSXXXSAAXSXMMSMMM
SXAMAMXMMXMASMMMSMMMSMMMMMAAAAMASAMMXAMSMXAAMAMXAASMSMMMSMASMMSASMMMMMMMAMMXMAMXMMMSSSMSXSAMXMASMMMSMAMMMXAMXAMXSMSXMASMMSMMXSSMSMMMMAAXMXXX
AMSSMMXMSAMXMAAAAXXAAXXSXMSMSMSASASXMSSXAMMMSAMSMMMMAAAAMXXSAAMAMAAXMXSXXSMMSSMAAXAMXAAAAMXMAMXMAMXAMASASXXMSXSMAXSMMMSAMXAAAXMASAMASXMMMMAM
MXAMXMAMAASXSSMMSSMSSSMXAXMAMMMAXXMAAMAMXMAXXMMMXAAXSSMSXMAMMMMXMSMMMASXAMXMAAXSMMSSSMMMSMMMXXSSMMSXMASAMXAXAASMSMMAAAMXMSXMASMAMAMMSMMAMAAX
XMAMAAXXAXAMXAASXMAAMAMXMMXMXSMSMXMMMMASXMASXSAMMSSMXXAAXMAMMMMMMAMXMASMSMAMSSMXAAXXMAXAAXMASMMAXASMMMMXMSMMMSMAXASXMMSAXMAMSAMXSSMAXAXAMSSS
AXSSSSMMMMMXSXMMMMMMMAMMXSXMMXAMMSSMMSAMXMASMMAXXAMXMMSMXSAXAAAAMASAMMSAAXXMAAXSMMSASAMSSXSASASXMMSAMXASMAMXMAMMMXMAAASAMXSMXXXMAMMSMXXAMAMX
SXMAAMAASASXSAXSAAAXMMXSAMASAMXMAAAAAMAMXMAMAXAMAMXSAXAAMSAMSSSSSMSASAMMMSMXMAMXAXXAMXMAAAMASXMAAASAMMMMSMSSSMSMSSMMMMMSAMXAMXMMAXAAAMXSMMSS
AXMSMMMMSASASMMMSASMXSAMASAMMSXMMSSMMSXMXMMSSMMMXSAXSXMSMMXMAAMMAMXMMAMAXAMMSMSAMXMAMXMMSMMAMXSMMMSAMXMAMXAAAAXAAXASAMXAMAMAMSXMMMMSSXAMXXXA
MXAMASAAMMMMMMXXAMMAAMMMMMXSXMASAAMAMSXSAAAAAAXXAMXSMXXMXMMMMMMSSMSMSMSMSMSAASAXXAAAMAXAMASXSASXMASMMXMSSMMMMMMMMXXXAAMMSSSXMXSAASAAMMMSAMAM
XMAXAMMXMAAAAXMMMSMMMMAAXMASAAMMMXXSMSASMSMSSMMMSSSXMAMMAMMMSAMMXAXAAAAAMAMMSMMSXSMSSSMMSAMMSASMSAMXXAXMAMASXMSAXXSMSMXXAAXMAAXXAMMMMMXAAMAX
MSAMSMMSSSSSXSAAASASAXSXSMAXMMMXXXAXAMAMAXXMAAXAAXMASXMSSMSASMSMMMMSMXMSMAXXAAXMXXAAMAAAMMSAMAMXMASXSMMSAMASAASMSMSAAMXMMMMSMSSSSMSAASMSSSXS
XMAMMAXXMAMAXXMMXSAMAMMASMSSXXMASMAMAMSMMMASMMMMXMSAMXMAXMMASXAMASAMXXSXXMMMXSMMAMMMMSMMSASXMMMMSSMMMMAXXMXSMMMAAAMSMSAMAMAAXXAAMAMSMMAMXMAM
XXAMSAMMMSMMMMSMMMAMXMMAMAXMASMASMMMMMXAXXMMMMAXMAMASXMSSXMMMMMSAMMMSMSAXSASXXMMAXMAMMXMSAXXSAAMXMSASXSSSSMSAAMXMSMXXSMSAMSASMAMMAMMSMSMSMSM
SSMMXMAXAMAAASAAAXAMXSMMMXMMASMMMAXMMAXXMMSAMSAXSASAMXAAXXSAMXXMMXAXAAMAMMASAAXSAMSASMMMMSMXSSSSMMXSXAMAAMASMMSSMXASASASXMAXSAMMSAXMAXAAAAAM
XAMAMMSMSSSMXAXSMSMSXSAXMMMSASAMSAMXMASMAXSAMMAMSAMXSXSMSASASMMASMMSMSMXMMAMMSMMAMMASAAAAMMAMXMXAMMXMMMMMMMSMXMASMXMAMMMSMSXMAMAXXMSMSMSMMMX
SAMXSMAMXAAXSMMMAAXXASXMAAAMMMAMXMMMMAMMSMMXMXMXMSMMXAAXXMSAMXMAMAASXXMASMXMAAXXSMMSMMSMSMMASXMXAMXMASAXXMAMMXSAMMSMXMXAAXMASMMSSXMAXAAAXMSS
SAMXMMAXMSMMAXASMMSMXMMSSMSSXSSMMSAMMASAAXXAMASAAAMMMMMXXXMXXAMASMMMMMMASXSMSMXAMAXAAXXMXXSASAASMMMSAMMSMMMSAAMAMXASASMXSAMMMAAMAMSAMMSMSAAX
SMXASXSSMXXAXMXXAAXMAMXMAMAMXAXAASASXMSMMASMSASMSMMXSAMXMASMSMSXXMAXMSMASXXXMASMSXMMXMMMMAMAXMMMMAXMAMXAAMXMMXMAMXMSXXAXMXMMSMMMSAMXXXMASMSM
SMMXSAMXAMXSSMXSMMMSAMMSMMASMMMMMSMMXMSASMAAMAMMXAXAMAAMSAMAAAXAMMMMAAMMMMXAMXMXAMXSAXAAAXMASMSMSSMSAMSMSXXXSMSSSSXMMSMMMSMXAMXXMMXSAAMAMXAA
XXAMMXMMXMAMAXMAXSASXSAMAMSSMAAAASMASMSAMXXMAMMXSMMMSMXXMAXSMSMSMASXSMSAAXMXMAXXXSMSASMMMSAXXAAAMMAMMXAAMMMMMAXAAMXMAAAMAAMXAMXXXXASMSMAAXXS
AXSXXAXAAMASAMMSXMASXMAMSMAMXSSXXSMASXMXMASXSSMXXAAAAXMXXXMMSMAXMASAAXSMXSASXMXSMMAMAMXAASXMMSMMSMSMMSMXMAXAAXMMMMAMSSSMMSSSSMMMSMMSAMMSSSMM
SAMXSMSSXSXSXMXAMMAMMMXXXAMXAMAMSMMMSASXMXSAMXMASMMSSSMSAMXMAMSMSXMXMMXXASMXAMMXAMSMAMXMMSAXAAASAAMAMXSAAMSXMSSXAMXMMAAMXAAMMAAXMASMMMAAAAXM
MAXAAAAXMMMMAXXMSMASMMMAMAXMAMAMAAMXXAMAXXMMMSMAXAAXAAAXMMAMMSXMASAMXAXMASXXSMASXMMMASMMMMMMXXSMMXXAMASXSXXXASAMXMMAMXMXMMSMSSMMSXMAAMMSSMMS
SSMMMSMSMAAXAMXMAXASAAXXSXMXMSSSSSMMSSMMMMSMSAMMSMAMMMMMXXAMXMXXMASXXXSAMXMSMMMXMAXMAXMASXSAMSAMXSSXMAMSAMXAMMAMAMSXMMSXXXMMXAXASXSXMSAMAAXA
MXAAAAXSMSSSMSXMAMSSMSAAMMMMXAAAAXAXAMASMXAXSAMXAXXXXASMMMXSAAXMXXMMMMAXXSXAXXMASMMMSSSMSMMAXSAMXXASAMXMMXAMASXMAXASMASXMMMXSMMMSASAXMMSSMSM
MSXMSMXMXXMAMMASXSXMAMMXMAAASMSMMMSMMSAXASXMXMMSXSMMSMSAXAASMMMMSMMAAXMMMMMMSAMXXMASAAAAXAMXMMAMXSAMXMMAMMSSMXASMSAAMASASMMASAMAMAMMXMAMMAXX
MAAMAMSMSSSMMSAMMMAMXMXAMSSXSXAAXXXAMMMSXXMASMMSAXXAAXSAMSMXAXSAXASMXSAAXMAXMXSXMASMMSMMMSMMMXMMMXAMXMAAMAAMXMMMMMXMAMSAMAMMSAMMMMAXAMAXMSMM
MMSMASXAAMAXMMMSASMMXMSXXAXXMXMXMMXMMAAXXAMXMAASASXSXXMAMXASMMMXXXAAXSMMSSMSAMMMMSXSAXMMAXAXXASASMAXSMSSSMMSSSMXAXASXMMAMXSXSAMXMSMSMSAXSAAA
XAAXXMMMMSSMXAXXMAAASMMSMMSSMSSMASASXMXSXXMXSMMMAMXAMSSMMXMAMAMAMMMXMSAMXAASXSAAMXAMXSAMASAMSMSASXMMXAAMAAXAAAXSXSASXMMAMMAASXMAXAAAXAASXMXS
MSSSSXMXAAAMSMSMMSMMMAAAXAXAAAXAXSASAMXSXSAMXAXMXMXMMXASXXSMSAMAXXAXSXMSSMMMASXSSMSMMSMAMMXMMAMAMMSMMMMSSMMMMMMXMSAMAMSMSAMXMASMSMSMSMSMXXSM
AXAAXMASMXAMAAAXMAAXSMMMMASMMMSMAMAMASXMASXXXXXMASXXSSMMMAMAXXXMXMMSXAXMASMMAMMMMAMMMMXMMMMSMAMAMAAMAAMMAMMSAXXAMMAMSMAAMXXAMXMXMAXAMXAAAMAM
SMMMMAAXMASMMSMSSSSXSMAMMMMAMXMMAMAMMSAMAMMMMSMSMSXMAMAAMAMMMSMXAAMXXAMSMMXMAXAAMAMMAMAAAAMXSMXMMSSSMSMXAMASASXSSSMMASXSSSSSSSMASMMAMSMSMMMS
XAAXSMMSSXMAAXAAAXMAMMSAAASAMMAXXMASAXXMASAAXXMAAXXSSMSMSMSAXMAMSMSAXMMASMASXSSSSMXSASXSSMSAMXMSXMAXAAXSSMMSAMAMMMSMASXMAAXXAMSASASXMXAXXXXS
XSXXAMAMXAMMMMSMMMXMAAXMSMSMSMSXSMXMMSXMXSASXMSMSMMMMAAAAAXXSMSMAAMMMXSASMXSMAMAMAXMASXAXAXMMAMAAMAMMMMMMAMSMMAMAAMMAMAMMAMMXMMXSAMXSMMMXSAS
MXMMMSMMMSMSAMMAXAMXMXSAXXMMAMXASAAAAAAMXSXMAXSXAMSAMSMMMSMMMAMAMXMMAMMAXMXXAAMAMAXMAMMMMSMMSXSMSMAXAXSXSAMXASXSMSSMMSXMXMMMAMMMXMMMSAAAMMAM
AXAAXAAAAAXSASXSMSAAXMMMMASXXXMAMSSMMXSMAMAMXMSAMXAXMXXMAMXAMMMAXAXMSSSSSMXASXMSMSAMAMAAAAAXXXXAMMSMMMAASMXSAMXXXMXAASAMXMASAMAMMXXXSSMSMMXM
MMMMMSSMXXXMMMAXAXSMMMAAMAMXMASAMXMASAMMAXSASXSXXAMMMMSAMXSMSASMSSSMAXAAAASXMSXMAMMSASXMSSSMMMMMMAXASAMXMAASXSMMMMMMMXASASASMSMSAMSMXXMAMSAM
XMXSAXMAMSMSSMMMSMAXASASXMXSAAXAMSAAMAMMAMXAMAMMMSXASAMAMMSAMASXAMAMMSMMMMMAAMMMAMXMMSXAXXXAAAAAMAMAMXSAMXMXAXMAAAMAXMXMAMASAAXXXAMMAMMSXSAS
XAAAXSAASAAAMASAMXSMXSAXASXMMMMAMAMSSSMMSMMAMXMASAAASXSAMMMSMAMMSSSMAAXAXXSMMMASAMAMXSMMMXSSSSSMSAMAMXSAMMXMAMXMMMSASAAMXMAMMMSMMMASXSAMXSSM
XMAMMMXMSMMMSAMMSXMSMMMSAMXMAMSAMASXAXMAXAMXMAMMMMMMMASASMAMMXMXAMAMSSSMSMMXXSMSASASAMAMMAMMMAAASMMSSMXXMXAASMMSAMXAMXAMXMAMSASAMXAXXMAXASMS
SSMMSXMXXXSXMXXMAMMASAAMAMAMAXMAMXSMAMMASXMSSXSASXSXXXMMAMAMSMSMMSMMAAXXAAASMMMSASAMMSAMMASAMMMMMAAMAXSMXXASXAASAMMMMXMXMXSAMASASMAMXMSMMMAM
MAAASASXMMAAXSMSAMMAXXMMAXXMSXSAMMMMMXMASAXXAASAMAMXMSMXAMAXAAXMXAXMMMMSSSMSAAXMAMXMMSAXSASASXMAMMMSAMXAMSMMMMMSAMAMAMSAMXXMMXMAMMMAAAAAXMAM
XSMMMXMAAASMMSAMAXMSMSMSMSAXAASASAAAXXMASMSMMMMMMAMASASMSXSAMXMXSSSXSAAAMXMMMMSMAMSMASAMMXSXMASXSAXMXMAXMAAXXSAMXSASAMMASASXSSMMMMXSMSSSXSSS
MXASXSSSMXAMAMXMAMSMAAAAASMMMXMASMSMSMXXSAMXAXMASXSMSAMXXAMXMASAAAAASMSXMASXMASXAAAMMXMXXAMASAMXSAMSASASXXMMXMASXMMSASXSMASAXSXXAXXXMAMXAXAM
MSAMAAAAXSMXSXXXXXAAMMMMMMSSMMMXMMMMMMMMMXMSMSSMAMAMMMMAMMMXMAMMXMMMMXMAMAXAMASXSSSSXSXSMXSAMASXMAMMXMASMMSAXSAMMSMSAMXAMMMMMMSSSSMMSMMXSMSA
MMMMMMMMMASAMXAMXSMSSXXXMMMMAASASAXAAAMAAAXAAMAMMSSMAAMXSASMMSSMXMSXMSSSMSSMMXSMMAAMAAAXXAMAMXMAMMMXSMMMAXSAMSASAAMMAMXMASAXAAAXXAMAAAXMXAXA
XAXXAXXXXAMMXSMAMXAAAMAMAAAMSMSASMSSSSSMSSSMSMXSAMXSXSAASMSAMAAXXMASMAAAAXAMAASMMMMMMMSMMSMSMSSSMAAMXAAXSMMAMXAMMMMXAMXXASXSMSXSSXMMXMMAXMXX
SMSMXMMMMMSXMASXAMMMSAXMSSSXMXMAMXMMMMAAAAAAXMXXAMAMMMMXSXSAMSSMXSASMAXMMMAXMMSAMXXAXAAAXAAMAMAASMSSSXMXMAMSMMXMMSSSMSXMASMXMAAMMMSSMMSSMMSM
AXAXAXAAAMAAAMXSMMSAXXXMAMMMXXMXMAAAAMAMMSMMMMMSMMAMXAXXMAMAMXXAAMASXMSSXSAMMXMMMMXMMSSSMMSMAMXMASAAAXSASMMMAXSAAAAAAMXSAMXMMMMMAAAAAAAAAAAA
MSMMMSXMXXAXMSAXMASMXSMMASMMXXMAMXXSSSMSXXXXAAAAXSAMSSSMMXMSAMXMMMAMAMAMXSXMMASXMMAMMXMAMXMXXMAXMMMSMMMAXMASAMSAMMSMMMAMASMMMSMSMMXSMMXMMSSS
MAXAMSMSSXSAAMASMXSAASXSASAAAXSSMSMMAAAXXXXSSMXSMSMMMXAXAMXXASXMAMASAMAXMMAMSMSAMSSSMASAMMMXMXXSAMAAAXXXMMXMXMXAMXAAAMASAAAMXAAXAXXMXSMSMAAX
SASXAXASAAMXMASMXAMMMSAMXSMMXXAAASMMSMMMMXXMAXAMMXMAXSMMMMMSXMASASMXAMMMAMAMSAMXMAMAXASXSAMXMAXMAMSSSMMXMXXMXMSSMXMMASAXMSXMMMMMSMXXAXAAAMMM
MASXMMMMMMMAMSMMXXSMAMAMMXMAXMMMMMAMASXAMXSSSMMSASXSXMMXAAXSMSAMXMASXMSASMASMAMAMXSAMXSMXASAMSMSAMAMAASAMSXMAAAXMASMASAXMMMMASAMAASMMMSMSAMS
MAMMXAXAXXSAMAAAAMXMASAMSAMXSSXAAAMMAMSASAMXAAAAXMAMXAASXSMXAMSSXMASAASAMSXMMAMXSAMXSXSASXMASAASMMAMSAMASAASMSMXSASMAMAMAAMSASASMSXAAAXSXMSX
MMSASMSMSASXSSSMMAMSXSASMXMXSAMSSSXMXXSXMXSSSMMSSMXMAMASAAMMXMMSXMSSMMMSMXXMAXSAMXXSAMXXSSSSMMXMXSSMXMSMMMXMMAMXMXSMASMSMXMMASMMXMMSMSSMMSSX
  `.split(/\s+/g);

  console.log(part_1(test));
  console.log(part_1(puzzle));

  console.log(part_2(test));
  console.log(part_2(puzzle));
}

main();