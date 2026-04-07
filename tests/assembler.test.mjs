import assert from "node:assert/strict";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import test from "node:test";

import { assembleSource } from "../tools/assembler/assembler.mjs";

const execFileAsync = promisify(execFile);
const rootDir = process.cwd();
const fixtureDir = path.join(rootDir, "tests", "fixtures", "assembler");
const haskellAssembler = path.join(rootDir, "tools", "assembler", "Assembler.hs");
const runghcBinary = process.env.RUNGHC || path.join(os.homedir(), ".ghcup", "bin", "runghc");

async function assembleFixtureWithJs(name, version = 4) {
  const source = await readFile(path.join(fixtureDir, `${name}.asm`), "utf8");
  return assembleSource(version, source, { includeBaseDir: fixtureDir });
}

async function assembleFixtureWithHaskell(name, version = 4) {
  const tmpDir = await mkdtemp(path.join(os.tmpdir(), "ti80-assembler-"));
  const basePath = path.join(tmpDir, name);
  try {
    const source = await readFile(path.join(fixtureDir, `${name}.asm`), "utf8");
    await writeFile(`${basePath}.asm`, source, "utf8");
    const args = version === 4 ? [haskellAssembler, basePath] : [haskellAssembler, `-${version}`, basePath];
    await execFileAsync(runghcBinary, args, { cwd: rootDir });
    const output = await readFile(`${basePath}.80z`, "utf8");
    return output.replace(/\r/g, "").trimEnd().split("\n");
  } finally {
    await rm(tmpDir, { recursive: true, force: true });
  }
}

test("JS assembler matches the documented basic control listing", async () => {
  const result = await assembleFixtureWithJs("basic_control");
  const expected = await readFile(path.join(fixtureDir, "basic_control.expected.80z"), "utf8");
  assert.equal(result.lines.join("\n"), expected.trimEnd());
  assert.deepEqual(result.warnings, []);
});

test("JS assembler matches the documented DB/DW chunking listing", async () => {
  const result = await assembleFixtureWithJs("data_directives");
  const expected = await readFile(path.join(fixtureDir, "data_directives.expected.80z"), "utf8");
  assert.equal(result.lines.join("\n"), expected.trimEnd());
  assert.deepEqual(result.warnings, []);
});

// `version` is for the ROM version compat
for (const fixture of [
  { name: "basic_control", version: 4 },
  { name: "arithmetic_compare", version: 4 },
  { name: "data_directives", version: 4 },
  { name: "i_forms", version: 4 },
  { name: "relocation_and_names", version: 3 }
]) {
  test(`JS assembler matches Haskell for ${fixture.name} (v${fixture.version}.x)`, async () => {
    const [jsResult, hsLines] = await Promise.all([
      assembleFixtureWithJs(fixture.name, fixture.version),
      assembleFixtureWithHaskell(fixture.name, fixture.version)
    ]);
    assert.equal(jsResult.lines.join("\n"), hsLines.join("\n"));
  });
}
