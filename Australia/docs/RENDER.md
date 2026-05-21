# Rendering the working paper

The headline paper is authored in markdown (`wp_draft.md`) and rendered
to PDF / docx / html via Quarto for distribution. The companion paper
in [`../../LIVES/docs/`](../../LIVES/docs/) uses the same setup.

## Quick start

```bash
cd Australia/docs/

# Stopgap: docx from plain pandoc, no Quarto required
make docx-pandoc

# Full pipeline: Quarto-rendered PDF / docx / html
make pdf
make docx
make html
```

## What lives where

| File                  | Role                                                              |
|-----------------------|-------------------------------------------------------------------|
| `wp_draft.md`         | Single source of truth for the prose — edit here.                |
| `wp_draft.qmd`        | Quarto wrapper: YAML metadata (title, author, abstract, bib, format options) and `{{< include _wp_body.md >}}`. |
| `_wp_body.md`         | Build artefact, generated from `wp_draft.md` by `make body`. Gitignored — do not edit. |
| `wp_references.bib`   | BibTeX bibliography for the paper.                               |
| `Makefile`            | Render targets.                                                  |

The editing flow is therefore: edit `wp_draft.md` → run `make pdf` (or
`make docx-pandoc` if Quarto isn't installed yet). The Makefile
regenerates `_wp_body.md` automatically before each render.

## Prerequisites

### For `make docx-pandoc` (the no-LaTeX path)

```bash
brew install pandoc   # macOS; see pandoc.org for other platforms
```

Pandoc alone produces a reasonable Word document with a table of
contents and numbered sections from the raw markdown. Useful as a
first pass for shareable review copies before the Quarto setup is in
place.

### For `make pdf` / `make docx` / `make html`

You need **Quarto** and (for PDF only) a **TeX distribution**.

```bash
# Quarto: official installer
brew install --cask quarto       # macOS
# or download from https://quarto.org/docs/get-started/

# TeX (only for PDF):
quarto install tinytex           # smallest, self-managed
# or, for a full system TeX:
brew install --cask mactex       # macOS full distribution (~4 GB)
```

Verify with:

```bash
quarto --version
quarto check
```

`quarto check` will tell you whether your TeX install is wired up
correctly for PDF rendering.

## Citations

The `.bib` file uses keys of the form `surname1_surname2_year`
(e.g. `williams_2010`, `muellbauer_williams_2012`). Inline citations
in the body markdown are currently in name-and-year prose form
("Williams (2010, 2012)" etc.) which Quarto+pandoc render as plain
text without consulting the `.bib`. A future polishing pass can
convert these to Quarto's `[@williams_2010]` form so that BibTeX
takes over the citation formatting; this is straightforward but
mechanical and is not blocking submission.

## Submission to the RBA RDP series

The RBA RDP house style is a Word-document template with a specific
cover page, header / footer styling, and front-matter conventions.
The Quarto-rendered docx (`make docx`) is the entry point for that
conversion: open the rendered file in Word and apply the RBA RDP
template's styles to each section heading. The actual RBA template
file is not in this repository (the bank distributes it directly to
authors at submission); when it arrives the same `wp_draft.md` source
can be retargeted by editing `wp_draft.qmd`'s `format: docx` block to
reference the RBA template:

```yaml
format:
  docx:
    reference-doc: rba_rdp_template.docx
```

That is the final-mile substitution; the underlying prose, tables,
figures, citations, and bibliography do not need to change.

## Troubleshooting

- **`make pdf` fails with "xelatex not found"** — run `quarto install
  tinytex`, or install MacTeX/TeXLive and re-run `quarto check`.
- **`make docx-pandoc` fails on table syntax** — pandoc's default
  table parser is strict. If a table chokes, run `make docx`
  instead; Quarto's pipeline is more forgiving.
- **`{{< include _wp_body.md >}}` not resolved** — that's a Quarto
  shortcode, not raw markdown. Run via `quarto render`, not
  `pandoc` directly, when using `wp_draft.qmd`.
- **Bibliography not appearing** — make sure `wp_references.bib`
  is in the same directory as `wp_draft.qmd`, and that the
  citations in the body have been migrated to `[@key]` form (a
  pure-prose draft renders cleanly but without an automatic
  bibliography section).
