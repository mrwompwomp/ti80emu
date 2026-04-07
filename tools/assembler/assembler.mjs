import { promises as fs } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";

function hex4(value) {
  return (value >>> 0).toString(16).toUpperCase().padStart(4, "0");
}

function splitSignedOffset(value) {
  const match = /^([^+-]+)([+-].+)?$/.exec(value);
  if (!match) throw new Error(`bad value ${value}`);
  return {
    base: match[1],
    offsetText: match[2] ?? ""
  };
}

function parseSignedHexOffset(offsetText) {
  if (!offsetText) return 0;
  const sign = offsetText[0] === "+" ? 1 : -1;
  return sign * parseHex(offsetText.slice(1), `unknown name ${offsetText.slice(1)}`);
}

function maybeLookup(symbols, key) {
  return symbols.has(key) ? symbols.get(key) : key;
}

function parseHex(value, errorMessage = `unknown name ${value}`) {
  if (!/^[0-9A-Fa-f]+$/.test(value)) throw new Error(errorMessage);
  return Number.parseInt(value, 16);
}

function addr(symbols, value) {
  return parseHex(maybeLookup(symbols, value), `unknown name ${value}`);
}

function reg(symbols, value) {
  const { base, offsetText } = splitSignedOffset(value);
  const lookedUp = maybeLookup(symbols, base);
  const parsedBase = base.startsWith("J")
    ? 0x200 + parseHex(base.slice(1), `unknown name ${base}`)
    : parseHex(lookedUp, `unknown name ${base}`);
  const offset = parseSignedHexOffset(offsetText);
  return ((parsedBase & 0x1f0) | ((parsedBase + offset) & 0x00f)) & 0x1ff;
}

function loc(symbols, value) {
  if (value === "A") return 0x0ff;
  if (value === "DP") return 0x11c;
  if (value === "I") return 0x100;
  if (value === "SP") return 0x118;
  if (value === "(I)") return 0x1ff;
  if (value.startsWith("(") && value.endsWith(")")) return reg(symbols, value.slice(1, -1));
  throw new Error(`bad register ${value}`);
}

function flag(symbols, value) {
  return maybeLookup(symbols, value);
}

function val(symbols, value) {
  const { base, offsetText } = splitSignedOffset(value);
  const lookedUp = maybeLookup(symbols, base);
  return parseHex(lookedUp, `unknown name ${base}`) + parseSignedHexOffset(offsetText);
}

function valBaseText(symbols, value) {
  return maybeLookup(symbols, splitSignedOffset(value).base);
}

function valLE(symbols, value) {
  const resolved = val(symbols, value);
  return (((resolved << 8) & 0xff00) | (resolved >> 8)) & 0xffff;
}

function cmp(value) {
  if (value === "=") return 0x0400;
  if (value === "≠") return 0x0600;
  if (value === "<") return 0x0c00;
  if (value === "≥") return 0x0e00;
  throw new Error(`unknown comparison ${value}`);
}

function size(value) {
  if (value.length === 1) return 0x0100;
  if (value.length === 2) return 0x0000;
  throw new Error(`bad size for ${value}`);
}

function yx(value) {
  return (((value << 4) & 0x00f0) | ((value >> 4) & 0x000f)) & 0x00ff;
}

function jo(tokens) {
  if (tokens[0] === "CALL" || tokens[0] === "JUMP") return 0x0200;
  if (tokens[0] === "IF" && tokens[1] === "NO" && tokens[2] === "CARRY:") return 0x0400;
  if (tokens[0] === "IF" && tokens[1] === "CARRY:") return 0x0600;
  return 0;
}

function jc(symbols, tokens) {
  if (tokens[0] === "CALL") return 0x8000 + addr(symbols, tokens[1]);
  if (tokens[0] === "JUMP") return addr(symbols, tokens[1]);
  if (tokens[0] === "IF" && tokens[1] === "NO" && tokens[2] === "CARRY:") return jc(symbols, tokens.slice(3));
  if (tokens[0] === "IF" && tokens[1] === "CARRY:") return jc(symbols, tokens.slice(2));
  throw new Error(`cannot compute branch target for ${tokens.join(" ")}`);
}

function translate(version, value) {
  if (version === 4) return value;
  let a = parseHex(value, `unknown name ${value}`);

  function fail(message) {
    throw new Error(message);
  }

  function walk(input) {
    if (input < 0x04c2) return input;
    if (input < 0x04c6) fail(`address ${hex4(input)} not in ROM 3.0`);
    if (input < 0x0dc2) return input - 0x4;
    if (input < 0x0dc8) fail(`address ${hex4(input)} not in ROM 3.0`);
    if (input < 0x13cf) return input - 0x0a;
    if (input < 0x179c) return input - 0x07;
    if (input < 0x179d) fail(`address ${hex4(input)} not in ROM 3.0`);
    if (input < 0x17a1) return input - 0x08;
    if (input < 0x17a3) fail(`address ${hex4(input)} not known in ROM 3.0`);
    if (input < 0x17a8) return input - 0x09;
    if (input < 0x17a9) fail(`address ${hex4(input)} not in ROM 3.0`);
    if (input < 0x17b2) return input - 0x0a;
    if (input < 0x17b3) fail(`address ${hex4(input)} not in ROM 3.0`);
    if (input < 0x17b5) return input - 0x0b;
    if (input < 0x17b6) fail(`address ${hex4(input)} not in ROM 3.0`);
    if (input < 0x17b7) return input - 0x0c;
    if (input < 0x17b8) fail(`address ${hex4(input)} not in ROM 3.0`);
    if (input < 0x17cc) return input - 0x0d;
    if (input < 0x17cd) fail(`address ${hex4(input)} not in ROM 3.0`);
    if (input < 0x17d0) return input - 0x0e;
    if (input < 0x17d1) fail(`address ${hex4(input)} not in ROM 3.0`);
    if (input < 0x17d8) return input - 0x0f;
    if (input < 0x17d9) fail(`address ${hex4(input)} not in ROM 3.0`);
    if (input < 0x17da) return input - 0x10;
    if (input < 0x17db) fail(`address ${hex4(input)} not in ROM 3.0`);
    if (input < 0x17dc) return input - 0x11;
    if (input < 0x17de) fail(`address ${hex4(input)} not in ROM 3.0`);
    if (input < 0x17e3) return input - 0x13;
    if (input < 0x17e4) fail(`address ${hex4(input)} not in ROM 3.0`);
    if (input < 0x17e8) return input - 0x14;
    if (input < 0x17e9) fail(`address ${hex4(input)} not in ROM 3.0`);
    if (input < 0x17ed) return input - 0x15;
    if (input < 0x17ee) fail(`address ${hex4(input)} not in ROM 3.0`);
    if (input < 0x1fff) return input - 0x16;
    if (input < 0x4000) fail(`address ${hex4(input)} not in ROM`);
    if (input < 0x4ead) return input;
    if (input < 0x7cc5) return input + 0x0d;
    if (input < 0x7ff1) return input + 0x0e;
    if (input < 0x7ff2) fail(`address ${hex4(input)} not in ROM 3.0`);
    if (input < 0x7fff) return input - 0x3145;
    if (input < 0x8000) return input;
    return walk(input - 0x8000) + 0x8000;
  }

  a = walk(a);
  return hex4(a);
}

function tokenizeSource(source) {
  return source
    .replace(/\r/g, "")
    .split("\n")
    .map((line) => {
      const trimmed = line.trim();
      return trimmed ? trimmed.split(/\s+/u) : [];
    });
}

async function preprocessLines(lines, includeBaseDir) {
  const result = [];
  for (const line of lines) {
    if (line[0] === "INCLUDE") {
      const includePath = path.resolve(includeBaseDir, line.slice(1).join(" "));
      const source = await fs.readFile(includePath, "utf8");
      const nested = await preprocessLines(tokenizeSource(source), path.dirname(includePath));
      result.push(...nested);
    } else {
      result.push(line);
    }
  }
  return result;
}

function normalizeDbLines(lines) {
  const normalized = [];
  for (const line of lines) {
    if (line[0] === "DB" && normalized.at(-1)?.[0] === "DB") {
      normalized[normalized.length - 1] = ["DB", `${normalized.at(-1)[1]},${line.slice(1).join("")}`];
    } else {
      normalized.push(line);
    }
  }
  return normalized;
}

function parseOperandList(tokens) {
  const text = tokens.join("");
  if (!text) return [];
  return text.split(",").filter((part) => part.length > 0);
}

function resolveValueLength(symbols, value) {
  const resolved = valBaseText(symbols, value);
  if (/^[0-9A-Fa-f]{1,4}$/.test(resolved)) return resolved.length;
  return 4;
}

function testMSB(state, value, warnings) {
  if ((value & 0x0f0) === 0x0f0) return;
  const page = value >> 8;
  if (state.i === 2 && page !== 2) {
    warnings.push(`${hex4(state.current)}: ambiguous register index (MSB used by CPU unknown)`);
    return;
  }
  if (state.i !== 2 && page === state.i) {
    throw new Error(`${hex4(state.current)}: bad register index (CPU would use opposite MSB)`);
  }
}

function formatWords(words) {
  if (words.length === 1) return `${hex4(words[0])}     `;
  if (words.length === 2) return `${hex4(words[0])} ${hex4(words[1])}`;
  throw new Error(`unsupported word count ${words.length}`);
}

function formatListingLine(address, words, tokens) {
  return `${hex4(address)}: ${formatWords(words)} ${tokens.join(" ")}`;
}

function initialAddressState() {
  return { orig: null, current: null, i: 2 };
}

function requireAddress(state) {
  if (state.orig == null || state.current == null) throw new Error("no base address specified");
}

function advance(state, words) {
  requireAddress(state);
  state.orig += words;
  state.current += words;
}

function collectSymbols(version, lines, seedSymbols) {
  const symbols = new Map(seedSymbols);
  const state = initialAddressState();

  for (const line of lines) {
    if (!line.length) continue;
    if (line[0].startsWith("#")) continue;
    if (line[0] === "ORG") {
      const value = addr(symbols, line[1]);
      state.orig = value;
      state.current = value;
      continue;
    }
    if (line[0] === "RELOCATE") {
      requireAddress(state);
      state.current = addr(symbols, line[1]);
      continue;
    }
    if (line[0] === "END" && line[1] === "RELOCATION") {
      requireAddress(state);
      state.current = state.orig;
      continue;
    }
    if (line[0] === ":") {
      requireAddress(state);
      symbols.set(line[1], hex4(state.current));
      continue;
    }
    if (line[0] === "NAME") {
      symbols.set(line[2], line[1]);
      continue;
    }
    if (line[0] === "ROM" && line[1] === "NAME") {
      symbols.set(line[3], translate(version, line[2]));
      continue;
    }

    advance(state, instructionLength(line, symbols));
  }

  return symbols;
}

function mapsEqual(a, b) {
  if (a.size !== b.size) return false;
  for (const [key, value] of a) {
    if (b.get(key) !== value) return false;
  }
  return true;
}

function resolveSymbols(version, lines) {
  let symbols = new Map();
  for (let iteration = 0; iteration < 16; iteration += 1) {
    const next = collectSymbols(version, lines, symbols);
    if (mapsEqual(symbols, next)) return next;
    symbols = next;
  }
  return symbols;
}

function instructionLength(tokens, symbols) {
  const op = tokens[0];
  if (["ADD", "DADD"].includes(op)) return tokens[1].endsWith(";") ? 2 : 1;
  if (["ADD.B", "ADD.N", "DADD.B", "DADD.N", "SUB.B", "SUB.N", "DSUB.B", "DSUB.N"].includes(op)) return tokens[1].endsWith(";") ? 2 : 1;
  if (op === "CALL") return 2;
  if (["CLEAR", "NOT.B", "NOT.N", "NOP", "READD", "READD.B", "READD.N", "READU", "READU.B", "READU.N", "REP", "RET", "SCANKEYS", "SET", "SHR", "TOGGLE", "WRITED", "WRITED.B", "WRITED.N", "WRITEU", "WRITEU.B", "WRITEU.N"].includes(op)) return 1;
  if (["IF", "IF.B", "IF.N"].includes(op)) return 2;
  if (op === "JUMP") return ["(104)", "(JUMP_TARGET)"].includes(tokens[1]) ? 1 : 2;
  if (op === "LOAD") {
    if (tokens[1] === "(I+),BYTE") return 2;
    if (tokens[1].startsWith("(I+),")) return resolveValueLength(symbols, tokens[1].slice(5)) === 4 ? 2 : 1;
    if (tokens[1].startsWith("I,")) return tokens[1].endsWith(";") ? 2 : 1;
    return 1;
  }
  if (["LOAD.B", "LOAD.N", "OR.B", "OR.N", "XCHG.B", "XCHG.N", "XOR.B", "XOR.N"].includes(op)) return 1;
  if (op === "DB") return Math.ceil(parseOperandList(tokens.slice(1)).length / 2);
  if (["DW", "DWLE"].includes(op)) return parseOperandList(tokens.slice(1)).length;
  throw new Error(`unknown instruction: ${op}`);
}

function emitDbLines(state, opcode, items, symbols) {
  const lines = [];
  for (let index = 0; index < items.length; index += 4) {
    const chunk = items.slice(index, index + 4);
    const firstWord = ((val(symbols, chunk[0]) << 8) | (chunk[1] ? (val(symbols, chunk[1]) & 0xff) : 0)) & 0xffff;
    const words = [firstWord];
    if (chunk[2]) {
      words.push(((val(symbols, chunk[2]) << 8) | (chunk[3] ? (val(symbols, chunk[3]) & 0xff) : 0)) & 0xffff);
    }
    lines.push(formatListingLine(state.current, words, [opcode, chunk.join(",")]));
    advance(state, words.length);
  }
  return lines;
}

function emitDwLines(state, opcode, items, symbols, littleEndian) {
  const lines = [];
  for (let index = 0; index < items.length; index += 2) {
    const chunk = items.slice(index, index + 2);
    const words = chunk.map((item) => (littleEndian ? valLE(symbols, item) : val(symbols, item)) & 0xffff);
    lines.push(formatListingLine(state.current, words, [opcode, chunk.join(",")]));
    advance(state, words.length);
  }
  return lines;
}

function emitInstruction(tokens, state, symbols, warnings) {
  const op = tokens[0];
  const joined = tokens.join(" ");
  const line = (words) => {
    const result = formatListingLine(state.current, words, tokens);
    if (op === "LOAD" && tokens[1].startsWith("I,")) state.i = reg(symbols, tokens[1].replace(/^I,/, "").replace(/;$/, "")) >> 8;
    if (op === "JUMP") state.i = 2;
    advance(state, words.length);
    return [result];
  };

  if (op === "ADD" || op === "DADD") {
    const operand = tokens[1];
    const branch = operand.endsWith(";");
    const text = branch ? operand.slice(0, -1) : operand;
    const split = text.indexOf(",");
    const u = text.slice(0, split);
    const v = text.slice(split + 1);
    let word;
    if (u === "(I)") word = (op === "DADD" ? 0xe800 : 0xe000) | size(valBaseText(symbols, v)) | (val(symbols, v) & 0x00ff);
    else if (operand[0] === "A") word = (op === "DADD" ? 0xf800 : 0xf000) | size(valBaseText(symbols, v)) | (val(symbols, v) & 0x00ff);
    else {
      const target = loc(symbols, u);
      word = (op === "DADD" ? 0xc800 : 0xc000)
        | ((target << 4) & 0x01f0)
        | ((target << 7) & 0x1000)
        | (val(symbols, v) & 0x00ff);
    }
    return line(branch ? [word + jo(tokens.slice(2)), jc(symbols, tokens.slice(2))] : [word]);
  }

  if (["ADD.B", "ADD.N", "DADD.B", "DADD.N", "SUB.B", "SUB.N", "DSUB.B", "DSUB.N"].includes(op)) {
    const operand = tokens[1];
    const branch = operand.endsWith(";");
    const text = branch ? operand.slice(0, -1) : operand;
    const regText = text.split(",")[1];
    const registerValue = loc(symbols, regText);
    testMSB(state, registerValue, warnings);
    const opcodeBase = {
      "ADD.B": 0x8000,
      "ADD.N": 0x8100,
      "DADD.B": 0x8800,
      "DADD.N": 0x8900,
      "SUB.B": 0x9000,
      "SUB.N": 0x9100,
      "DSUB.B": 0x9800,
      "DSUB.N": 0x9900
    }[op];
    const word = opcodeBase + yx(registerValue);
    return line(branch ? [word + jo(tokens.slice(2)), jc(symbols, tokens.slice(2))] : [word]);
  }

  if (op === "CALL") return line([0xd000 + jo(tokens), jc(symbols, tokens)]);

  if (["CLEAR", "SET", "TOGGLE"].includes(op)) {
    const flagText = flag(symbols, tokens[1]);
    const dot = flagText.indexOf(".");
    const registerText = flagText.slice(0, dot);
    const bitText = flagText.slice(dot + 1);
    const registerValue = loc(symbols, registerText);
    const base = { TOGGLE: 0x2000, CLEAR: 0x2800, SET: 0x3000 }[op];
    const bit = parseHex(bitText, `unknown name ${bitText}`);
    return line([base | yx(registerValue) | ((registerValue << 1) & 0x0200) | ((bit << 9) & 0x0400) | ((bit << 8) & 0x0100)]);
  }

  if (op === "DB") return emitDbLines(state, "DB", parseOperandList(tokens.slice(1)), symbols);
  if (op === "DW") return emitDwLines(state, "DW", parseOperandList(tokens.slice(1)), symbols, false);
  if (op === "DWLE") return emitDwLines(state, "DWLE", parseOperandList(tokens.slice(1)), symbols, true);

  if (op === "IF" && tokens[1] === "CLEAR") {
    const flagText = flag(symbols, tokens[2].slice(0, -1));
    const dot = flagText.indexOf(".");
    const registerText = flagText.slice(0, dot);
    const bitText = flagText.slice(dot + 1);
    const registerValue = loc(symbols, registerText);
    const bit = parseHex(bitText, `unknown name ${bitText}`);
    const word = 0xa000 | yx(registerValue) | ((registerValue << 1) & 0x0200) | ((bit << 9) & 0x0400) | ((bit << 8) & 0x0100);
    return line([word, jc(symbols, tokens.slice(3))]);
  }

  if (op === "IF" && tokens[1] === "SET") {
    const flagText = flag(symbols, tokens[2].slice(0, -1));
    const dot = flagText.indexOf(".");
    const registerText = flagText.slice(0, dot);
    const bitText = flagText.slice(dot + 1);
    const registerValue = loc(symbols, registerText);
    const bit = parseHex(bitText, `unknown name ${bitText}`);
    const word = 0xa800 | yx(registerValue) | ((registerValue << 1) & 0x0200) | ((bit << 9) & 0x0400) | ((bit << 8) & 0x0100);
    return line([word, jc(symbols, tokens.slice(3))]);
  }

  if (op === "IF" && tokens[1].startsWith("(I+)")) {
    const expression = tokens[1];
    const operatorIndex = expression.search(/[=≠<≥]/u);
    const left = expression.slice(0, operatorIndex);
    const operator = expression[operatorIndex];
    const rightWithColon = expression.slice(operatorIndex + 1);
    const right = rightWithColon.slice(0, rightWithColon.indexOf(":"));
    return line([0x6000 + cmp(operator) + size(valBaseText(symbols, right)) + val(symbols, right), jc(symbols, tokens.slice(2))]);
  }

  if (op === "IF" && tokens[1].startsWith("A")) {
    const expression = tokens[1];
    const operatorIndex = expression.search(/[=≠<≥]/u);
    const rightWithColon = expression.slice(operatorIndex + 1);
    const right = rightWithColon.slice(0, rightWithColon.indexOf(":"));
    return line([0x7000 + cmp(expression[operatorIndex]) + size(valBaseText(symbols, right)) + val(symbols, right), jc(symbols, tokens.slice(2))]);
  }

  if (op === "IF") {
    const expression = tokens[1];
    const operatorIndex = expression.search(/[=≠<≥]/u);
    const left = expression.slice(0, operatorIndex);
    const operator = expression[operatorIndex];
    const rightWithColon = expression.slice(operatorIndex + 1);
    const right = rightWithColon.slice(0, rightWithColon.indexOf(":"));
    const target = loc(symbols, left);
    return line([0x4000 | cmp(operator) | ((target << 4) & 0x01f0) | ((target << 7) & 0x1000) | (val(symbols, right) & 0x00ff), jc(symbols, tokens.slice(2))]);
  }

  if (op === "IF.B" || op === "IF.N") {
    const expression = tokens[1];
    const operatorIndex = expression.search(/[=≠<≥]/u);
    const rightWithColon = expression.slice(operatorIndex + 1);
    const right = rightWithColon.slice(0, rightWithColon.indexOf(":"));
    const registerValue = loc(symbols, right);
    testMSB(state, registerValue, warnings);
    const base = op === "IF.B" ? 0xb000 : 0xb100;
    return line([base + cmp(expression[operatorIndex]) + yx(registerValue), jc(symbols, tokens.slice(2))]);
  }

  if (op === "JUMP" && (tokens[1] === "(104)" || tokens[1] === "(JUMP_TARGET)")) return line([0x0080]);
  if (op === "JUMP") return line([0xd000 + jo(tokens), jc(symbols, tokens)]);

  if (op === "LOAD" && tokens[1] === "(I+),BYTE") {
    const target = tokens[2];
    const value = tokens[3];
    if (target === "PTR") return line([0x0c00 | ((val(symbols, value) << 1) & 0x00fe), 0x0c00 | ((val(symbols, value) >> 7) & 0x00ff)]);
    if (target === "PTR+1") return line([0x0c00 | ((val(symbols, value) << 1) & 0x00fe) | 1, 0x0c00 | ((val(symbols, value) >> 7) & 0x00ff)]);
    if (target === "LPTR" || target === "LPTR+1" || target === "HPTR") {
      const word = target === "HPTR"
        ? (0x0c00 | ((val(symbols, value) >> 7) & 0x00ff))
        : (0x0c00 | ((val(symbols, value) << 1) & 0x00fe) | (target === "LPTR+1" ? 1 : 0));
      const result = formatListingLine(state.current, [word], tokens);
      advance(state, 2);
      return [result];
    }
  }

  if (op === "LOAD" && tokens[1].startsWith("(I+),")) {
    const valueText = tokens[1].slice(5);
    const resolvedLength = resolveValueLength(symbols, valueText);
    const first = resolvedLength === 4 ? (0x0c00 | (val(symbols, valueText) & 0x00ff)) : (0x0c00 + size(valBaseText(symbols, valueText)) + (val(symbols, valueText) & 0x00ff));
    if (resolvedLength === 4) return line([first, 0x0c00 | ((val(symbols, valueText) >> 8) & 0x00ff)]);
    return line([first]);
  }

  if (op === "LOAD" && tokens[1].startsWith("A,")) {
    const source = tokens[1].slice(2);
    return line([0x1c00 + size(valBaseText(symbols, source)) + (val(symbols, source) & 0x00ff)]);
  }

  if (op === "LOAD" && tokens[1].startsWith("I,")) {
    const target = tokens[1].slice(2);
    if (target.endsWith(";")) {
      const registerValue = reg(symbols, target.slice(0, -1));
      return line([0x0a00 + registerValue, jc(symbols, tokens.slice(2))]);
    }
    return line([0x0800 + reg(symbols, target)]);
  }

  if (op === "LOAD") {
    const split = tokens[1].indexOf(",");
    const left = tokens[1].slice(0, split);
    const right = tokens[1].slice(split + 1);
    const target = loc(symbols, left);
    return line([0x0e00 | ((target << 4) & 0x01f0) | ((target << 7) & 0x1000) | (val(symbols, right) & 0x00ff)]);
  }

  if (op === "LOAD.B" || op === "LOAD.N") {
    const nibbleMode = op.endsWith(".N");
    if (tokens[1].startsWith("A,") && tokens[1].slice(2) !== "(I)") {
      const registerValue = loc(symbols, tokens[1].slice(2));
      const base = nibbleMode ? 0x1100 : 0x1000;
      return line([base | yx(registerValue) | ((registerValue << 1) & 0x0200)]);
    }
    if (tokens[1].startsWith("(I),")) {
      const registerValue = loc(symbols, tokens[1].slice(4));
      testMSB(state, registerValue, warnings);
      const base = nibbleMode ? 0x0500 : 0x0400;
      return line([base | yx(registerValue)]);
    }
    if (tokens[1].endsWith(",A")) {
      const registerValue = loc(symbols, tokens[1].slice(0, -2));
      const base = nibbleMode ? 0x1900 : 0x1800;
      return line([base | yx(registerValue) | ((registerValue << 1) & 0x0200)]);
    }
    const registerValue = loc(symbols, tokens[1].slice(0, -4));
    testMSB(state, registerValue, warnings);
    const base = nibbleMode ? 0x0300 : 0x0200;
    return line([base | yx(registerValue)]);
  }

  if (op === "NOP") return line([0xd000]);
  if (op === "NOT.B" || op === "NOT.N") {
    const registerValue = loc(symbols, tokens[1]);
    const base = op === "NOT.N" ? 0x3900 : 0x3800;
    return line([base | yx(registerValue) | ((registerValue << 1) & 0x0200)]);
  }

  if (op === "OR.B" || op === "OR.N" || op === "XOR.B" || op === "XOR.N") {
    const registerValue = loc(symbols, tokens[1].slice(4));
    testMSB(state, registerValue, warnings);
    const base = { "XOR.B": 0x3c00, "XOR.N": 0x3d00, "OR.B": 0x3e00, "OR.N": 0x3f00 }[op];
    return line([base + yx(registerValue)]);
  }

  if (["READD", "READD.B", "READD.N"].includes(op)) return line([op === "READD.N" ? 0x0105 : 0x0005]);
  if (["READU", "READU.B", "READU.N"].includes(op)) return line([op === "READU.N" ? 0x0104 : 0x0004]);
  if (op === "REP") return line([0x0f00 + parseHex(tokens[1], `unknown name ${tokens[1]}`) - 1]);
  if (op === "RET") return line([0x0040]);
  if (op === "SCANKEYS") return line([0x0020]);
  if (op === "SHR" && tokens[1] === "A") return line([0x0018]);
  if (["WRITED", "WRITED.B", "WRITED.N"].includes(op)) return line([op === "WRITED.N" ? 0x0107 : 0x0007]);
  if (["WRITEU", "WRITEU.B", "WRITEU.N"].includes(op)) return line([op === "WRITEU.N" ? 0x0106 : 0x0006]);

  if (op === "XCHG.B" || op === "XCHG.N") {
    const nibbleMode = op.endsWith(".N");
    if (tokens[1].startsWith("A,")) {
      const registerValue = loc(symbols, tokens[1].slice(2));
      return line([(nibbleMode ? 0x1500 : 0x1400) | yx(registerValue) | ((registerValue << 1) & 0x0200)]);
    }
    const registerValue = loc(symbols, tokens[1].slice(4));
    testMSB(state, registerValue, warnings);
    return line([(nibbleMode ? 0x0700 : 0x0600) | yx(registerValue)]);
  }

  throw new Error(`${hex4(state.current)}: unknown instruction: ${joined}`);
}

export function assembleTokenLines(version, tokenLines) {
  const lines = normalizeDbLines(tokenLines);
  const symbols = resolveSymbols(version, lines);
  const warnings = [];
  const output = [];
  const state = initialAddressState();

  for (const line of lines) {
    if (!line.length) continue;
    if (line[0].startsWith("#")) continue;
    if (line[0] === "ORG") {
      const value = addr(symbols, line[1]);
      state.orig = value;
      state.current = value;
      continue;
    }
    if (line[0] === "RELOCATE") {
      requireAddress(state);
      state.current = addr(symbols, line[1]);
      continue;
    }
    if (line[0] === "END" && line[1] === "RELOCATION") {
      requireAddress(state);
      state.current = state.orig;
      continue;
    }
    if (line[0] === ":") {
      requireAddress(state);
      output.push(`${hex4(state.current)}(${line[1]}):`);
      continue;
    }
    if (line[0] === "NAME") continue;
    if (line[0] === "ROM" && line[1] === "NAME") continue;
    requireAddress(state);
    output.push(...emitInstruction(line, state, symbols, warnings));
  }

  return {
    lines: output,
    warnings,
    symbols
  };
}

export async function assembleSource(version, source, options = {}) {
  const includeBaseDir = options.includeBaseDir ?? process.cwd();
  const preprocessed = await preprocessLines(tokenizeSource(source), includeBaseDir);
  return assembleTokenLines(version, preprocessed);
}

export async function assembleFile(version, fileBase) {
  const asmPath = `${fileBase}.asm`;
  const source = await fs.readFile(asmPath, "utf8");
  const result = await assembleSource(version, source, { includeBaseDir: path.dirname(asmPath) });
  const outputPath = `${fileBase}.80z`;
  await fs.writeFile(outputPath, `${result.lines.join("\n")}\n`, "utf8");
  return { ...result, outputPath };
}

async function cli(argv) {
  if ((argv.length === 1 && !argv[0].startsWith("-")) || (argv.length === 2 && ["-3", "-4"].includes(argv[0]))) {
    const version = argv.length === 2 ? Number(argv[0].slice(1)) : 4;
    const fileBase = argv.at(-1);
    const result = await assembleFile(version, fileBase);
    for (const warning of result.warnings) process.stderr.write(`${warning}\n`);
    return;
  }

  const execName = path.basename(process.argv[1] ?? "assembler.mjs");
  process.stdout.write(`Usage: ${execName} [-3|-4] file\n`);
  process.exit(argv.length ? 1 : 0);
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  cli(process.argv.slice(2)).catch((error) => {
    process.stderr.write(`${error.message}\n`);
    process.exit(1);
  });
}
