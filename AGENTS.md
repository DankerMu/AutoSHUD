# Repository Guidelines

## Project Structure & Module Organization

- `Step*_*.R` and `GetReady.R` are the main pipeline scripts (preprocess → subset → build model → run SHUD → analyze).
- `Rfunction/` contains reusable helpers (e.g., GDAL wrappers, project parsing, forcing readers).
- `SubScript/` contains optional sub-workflows (soil/landcover/forcing variants and analysis helpers).
- `Example/` contains a working sample project (`*.autoshud.txt`) and small GIS inputs.
- `Table/` contains lookup tables (e.g., landcover classes/colors).

Most outputs are written under the `dir.out` configured in your `*.autoshud.txt` project file (created folders include `predata/`, `fig/`, `modelin/`, `modelout/`, etc.).

## Build, Test, and Development Commands

Open `AutoSHUD.Rproj` in RStudio, or run scripts from the repo root with `Rscript`:

- `Rscript Step1_RawDataProcessng.R Example/9035800.autoshud.txt` — preprocess boundary/DEM/streams.
- `Rscript Step2_DataSubset.R Example/9035800.autoshud.txt` — derive soil/landuse/forcing inputs.
- `Rscript Step3_BuidModel.R Example/9035800.autoshud.txt` — generate SHUD mesh + model inputs.
- `Rscript Step4_SHUD.R Example/9035800.autoshud.txt` — clone/build/run SHUD (requires `git`, `make`, a C/C++ toolchain, and Sundials; you may need to change the clone URL to HTTPS if you don’t use SSH).

System dependencies: GDAL command-line tools must be on `PATH` (`gdalwarp`, `gdal_merge.py`).

## Coding Style & Naming Conventions

- Indentation: 2 spaces; no tabs.
- Keep scripts config-driven via `*.autoshud.txt` (avoid hard-coded absolute paths in code).
- Naming: pipeline scripts use `StepN_*.R`; reusable helpers belong in `Rfunction/`; optional modules belong in `SubScript/`.

## Testing Guidelines

There is no formal automated test suite. Minimum validation for changes:

- Run Steps 1–3 on `Example/9035800.autoshud.txt` and confirm outputs are created under `dir.out` and figures under `*/fig/`.
- For GIS/forcing changes, note the dataset/config used and a couple of key output files in your PR.

## Commit & Pull Request Guidelines

- Commit messages in this repo are short and free-form (often `update ...`). Keep commits small and prefer “area + intent”, e.g., `Step2: fix CMFD RH handling`.
- PRs should include: purpose, the config file used (`*.autoshud.txt`), and any new R/system dependencies. Add screenshots when plots/figures change.
- Do not commit large rasters or model outputs; extend `.gitignore` for generated folders if needed.

## Configuration & Security Tips

Project files (`*.autoshud.txt`) frequently contain local filesystem paths. Prefer relative paths for examples and avoid committing sensitive paths, credentials, or private data locations.

