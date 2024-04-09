"use strict";
(() => {
  // src/puzzleData.ts
  var puzzle = {
    sideLength: 3,
    letters: "lytrotnem".toUpperCase(),
    words: "lone,lore,lyre,more,morn,mote,note,omen,rote,tern,tome,tone,tore,torn,tote,yore,loner,otter,tenor,toner,totem,rotten,lottery".split(",").map((w) => w.toUpperCase())
  };

  // src/monomitter.ts
  function monomitter(emitLatestOnSubscribe = false) {
    const callbacks = /* @__PURE__ */ new Set();
    let valueBeenSet = false;
    let latestValue = void 0;
    function publish(value) {
      valueBeenSet = true;
      latestValue = value;
      callbacks.forEach((callback) => callback(value));
    }
    function subscribe(callback) {
      callbacks.add(callback);
      if (emitLatestOnSubscribe && valueBeenSet) {
        callback(latestValue);
      }
      return {
        unsubscribe() {
          callbacks.delete(callback);
        }
      };
    }
    function clear() {
      callbacks.clear();
    }
    return {
      publish,
      subscribe,
      clear
    };
  }

  // src/puzzleLogic.ts
  var toCoordinate = (index, sideLength) => {
    return { x: index % sideLength, y: Math.floor(index / sideLength) };
  };
  var toIdx = (coordinate, sideLength) => {
    return coordinate.y * sideLength + coordinate.x;
  };
  var adjacent = (c, sideLength) => {
    return [
      { x: c.x + 1, y: c.y },
      // →
      { x: c.x, y: c.y - 1 },
      // ↑
      { x: c.x - 1, y: c.y },
      // ←
      { x: c.x, y: c.y + 1 },
      // ↓
      { x: c.x + 1, y: c.y + 1 },
      // ↘
      { x: c.x + 1, y: c.y - 1 },
      // ↗
      { x: c.x - 1, y: c.y - 1 },
      // ↖
      { x: c.x - 1, y: c.y + 1 }
      // ↙
    ].filter(({ x, y }) => 0 <= x && x < sideLength && 0 <= y && y < sideLength);
  };
  var findWord = (gridSize, grid, word, visited) => {
    if (word === "") {
      return visited;
    }
    if (visited.length === 0) {
      for (let i = 0; i < grid.length; i++) {
        if (grid[i] === word[0]) {
          const wordPath = findWord(gridSize, grid, word.slice(1), [i]);
          if (wordPath !== null) {
            return wordPath;
          }
        }
      }
      return null;
    } else {
      const lastLetterIdx = visited[visited.length - 1];
      for (const neighbor of adjacent(toCoordinate(lastLetterIdx, gridSize), gridSize)) {
        const neighborIdx = toIdx(neighbor, gridSize);
        if (visited.includes(neighborIdx)) {
          continue;
        }
        if (grid[neighborIdx] !== word[0]) {
          continue;
        }
        const wordPath = findWord(gridSize, grid, word.slice(1), [...visited, neighborIdx]);
        if (wordPath !== null) {
          return wordPath;
        }
      }
      return null;
    }
  };
  var PuzzleLogic = class {
    constructor(puzzleInfo) {
      this.puzzle = puzzleInfo;
      this.letterBuffer = [];
      this.userFindWord = monomitter();
    }
    handleKey(key) {
      if (key === "Escape") {
        this.letterBuffer = [];
      }
      if (key === "Backspace") {
        this.letterBuffer.pop();
      }
      if (key === "Enter") {
        this.userFindWord.publish(this.letterBuffer.join(""));
        this.letterBuffer = [];
      }
      if ("ABCDEFGHIJKLMNOPQRSTUVWXYZ".includes(key.toUpperCase())) {
        this.letterBuffer.push(key.toUpperCase());
      }
    }
    undoKey(key) {
      const removedKey = this.letterBuffer.pop();
      console.assert(removedKey === key, "Popped key does not match undo");
    }
    getCurrentWordPath() {
      const letterIndices = findWord(
        this.puzzle.sideLength,
        this.puzzle.letters,
        this.letterBuffer.join(""),
        []
      );
      if (letterIndices === null) {
        return null;
      } else {
        return letterIndices.map((c) => toCoordinate(c, this.puzzle.sideLength));
      }
    }
    getTileValues(words) {
      const values = [];
      for (let i = 0; i < this.puzzle.sideLength ** 2; i++) {
        values.push({ containing: 0, starting: 0 });
      }
      for (const word of words.allWords) {
        if (words.unlockedWords.has(word)) {
          continue;
        }
        const wordPosition = findWord(this.puzzle.sideLength, this.puzzle.letters, word, []);
        if (wordPosition === null) {
          throw new Error(`Could not find word ${word} on the grid`);
        }
        values[wordPosition[0]].starting++;
        for (const letter of wordPosition) {
          values[letter].containing++;
        }
      }
      return values;
    }
  };

  // src/puzzleRenderer.ts
  var circleToPath = (x, y, r) => {
    return `M ${x} ${y}
m ${r}, 0
a ${r},${r} 0 1,1, ${-r * 2},0
a ${r},${r} 0 1,1 ${r * 2},0`;
  };
  var PuzzleRenderer = class {
    constructor(puzzleInfo, container, lineElement) {
      this.puzzle = puzzleInfo;
      this.puzzleContainer = container;
      this.line = lineElement;
      this.letterTiles = this.createLetterTiles();
    }
    createLetterTiles() {
      const tiles = [];
      for (const letter of this.puzzle.letters) {
        const letterContainer = document.createElement("div");
        letterContainer.classList.add("letter-container");
        const letterElement = document.createElement("div");
        letterElement.innerText = letter;
        const startNumElement = document.createElement("div");
        startNumElement.classList.add("start-num");
        startNumElement.innerText = "?";
        const includeNumElement = document.createElement("div");
        includeNumElement.classList.add("include-num");
        includeNumElement.innerText = "?";
        letterContainer.append(letterElement, startNumElement, includeNumElement);
        this.puzzleContainer.append(letterContainer);
        tiles.push(letterContainer);
      }
      return tiles;
    }
    renderWordLine(word) {
      const gapSize = 400 * (5 / 100);
      const nonGap = 400 - (this.puzzle.sideLength - 1) * gapSize;
      const tileSize = nonGap / this.puzzle.sideLength;
      const centerDist = tileSize + gapSize;
      const offset = tileSize / 2;
      console.log({});
      const formattedPoints = word.map(({ x, y }) => ({ x: x * centerDist + offset, y: y * centerDist + offset })).map(({ x, y }, idx) => {
        if (idx === 0) {
          const circle = circleToPath(x, y, 10);
          return `${circle} M ${x.toFixed(1)} ${y.toFixed(1)}`;
        } else {
          return `L ${x.toFixed(1)} ${y.toFixed(1)}`;
        }
      }).join(" ");
      this.line.setAttribute("d", formattedPoints);
    }
    drawTileValues(tileValues) {
      const formatNum = (n) => {
        if (n === 0) {
          return "";
        }
        if (n >= 10) {
          return "+";
        }
        return `${n}`;
      };
      for (let i = 0; i < tileValues.length; i++) {
        const tile = this.letterTiles[i];
        const start = tile.querySelector(".start-num");
        const contains = tile.querySelector(".include-num");
        start.innerText = formatNum(tileValues[i].starting);
        contains.innerText = formatNum(tileValues[i].containing);
        if (tileValues[i].starting === 0 && tileValues[i].containing === 0) {
          tile.classList.add("eliminated");
        }
      }
    }
  };

  // src/wordManager.ts
  var wordHint = (word) => {
    return word[0] + "*".repeat(word.length - 1);
  };
  var WordManager = class {
    constructor(words, popupWordContainer) {
      this.allWords = new Set(words);
      this.unlockedWords = this.loadLocalStorage();
      this.popupWordContainer = popupWordContainer;
      this.wordElements = /* @__PURE__ */ new Map();
      this.renderPopup();
    }
    loadLocalStorage() {
      const saved = localStorage.getItem("words");
      if (saved !== null) {
        return new Set(saved.split(","));
      } else {
        return /* @__PURE__ */ new Set();
      }
    }
    saveToLocalStorage(words) {
      localStorage.setItem("words", Array.from(words).join(","));
    }
    trySubmitWords(word) {
      if (!this.allWords.has(word)) {
        return "invalidWord";
      }
      if (this.unlockedWords.has(word)) {
        return "alreadyGuessed";
      }
      this.unlockedWords.add(word);
      this.saveToLocalStorage(this.unlockedWords);
      const wordElement = this.wordElements.get(word);
      if (wordElement === void 0) {
        console.error(`Word ${word} has no corresponding HTML element`);
      } else {
        wordElement.innerText = word.toLowerCase();
      }
      return "success";
    }
    renderPopup() {
      const longestWordLength = Math.max(...Array.from(this.allWords).map((w) => w.length));
      for (let i = 4; i <= longestWordLength; i++) {
        const heading = document.createElement("h2");
        heading.innerText = `${i} letters`;
        const words = Array.from(this.allWords).filter((w) => w.length === i).sort();
        const wordGroupContainer = document.createElement("div");
        wordGroupContainer.classList.add("word-group");
        wordGroupContainer.style.setProperty("--word-length", `${i + 2}ch`);
        for (const word of words) {
          const wordContainer = document.createElement("span");
          this.wordElements.set(word, wordContainer);
          if (this.unlockedWords.has(word)) {
            wordContainer.innerText = word.toLowerCase();
          } else {
            wordContainer.innerText = wordHint(word.toLowerCase());
          }
          wordGroupContainer.append(wordContainer);
        }
        this.popupWordContainer.append(heading, wordGroupContainer);
      }
    }
  };

  // src/script.ts
  var main = () => {
    const puzzleContainer = document.getElementById("puzzle-container");
    const lineContainer = document.getElementById("line-highlight");
    const wordsFoundContainer = document.getElementById("words-found");
    const wordsTotalContainer = document.getElementById("words-left");
    const wordsPopup = document.getElementById("words-popup");
    const popupContent = document.getElementById("words-container");
    const openPopupButton = document.getElementById("word-opener");
    const closePopupButton = document.getElementById("close");
    let isPopupOpen = false;
    const openPopup = () => {
      wordsPopup.style.display = "block";
      isPopupOpen = true;
    };
    const closePopup = () => {
      wordsPopup.style.display = "none";
      isPopupOpen = false;
    };
    const puzzleRenderer = new PuzzleRenderer(puzzle, puzzleContainer, lineContainer);
    const puzzleLogic = new PuzzleLogic(puzzle);
    const wordManager = new WordManager(puzzle.words, popupContent);
    const tileValues = puzzleLogic.getTileValues(wordManager);
    puzzleRenderer.drawTileValues(tileValues);
    wordsFoundContainer.innerText = wordManager.unlockedWords.size.toString();
    wordsTotalContainer.innerText = wordManager.allWords.size.toString();
    window.addEventListener("keydown", (e) => {
      if (isPopupOpen) {
        if (e.key === "Escape") {
          closePopup();
        }
        return;
      }
      const key = "abcdefghijklmnopqrstuvwxyz".includes(e.key) ? e.key.toUpperCase() : e.key;
      puzzleLogic.handleKey(key);
      const currentPath = puzzleLogic.getCurrentWordPath();
      if (currentPath !== null) {
        puzzleRenderer.renderWordLine(currentPath);
      } else {
        puzzleLogic.undoKey(key);
      }
    });
    puzzleLogic.userFindWord.subscribe((word) => {
      const response = wordManager.trySubmitWords(word);
      if (response === "success") {
        const tileValues2 = puzzleLogic.getTileValues(wordManager);
        puzzleRenderer.drawTileValues(tileValues2);
        wordsFoundContainer.innerText = wordManager.unlockedWords.size.toString();
      }
    });
    openPopupButton.addEventListener("click", openPopup);
    closePopupButton.addEventListener("click", closePopup);
  };
  main();
})();
