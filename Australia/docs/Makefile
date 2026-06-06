# Render the working paper to PDF / docx / html.
#
# - The single source of truth is wp_draft.md.
# - wp_draft.qmd is a thin Quarto wrapper supplying YAML metadata
#   (title, author, abstract, bibliography, format options) and
#   `{{< include _wp_body.md >}}` for the body.
# - _wp_body.md is an auto-generated build artefact: it is
#   wp_draft.md with the leading title / draft-tag / author /
#   JEL / keywords preamble stripped (those live in the YAML).
# - Final RBA RDP styling (template substitution, RBA cover page,
#   typography) is a submission-time task; this Makefile produces
#   a generic academic-style PDF / docx in the meantime.
#
# Targets:
#   make pdf           Render wp_draft.md to wp_draft.pdf via pandoc + Typst
#                       (single ~40 MB engine, ~10x faster than LaTeX; the
#                       default PDF target — works on a fresh machine with
#                       `brew install pandoc typst`)
#   make pdf-quarto    Alternative: render wp_draft.qmd via Quarto + LaTeX
#                       (needs quarto + tinytex/MacTeX; larger install
#                       footprint; useful if you want the BibTeX auto-cite
#                       pipeline)
#   make docx          Render wp_draft.qmd to wp_draft.docx (Quarto)
#   make html          Render wp_draft.qmd to wp_draft.html (Quarto)
#   make docx-pandoc   Stopgap: produce wp_draft.docx via plain pandoc
#                       (no Quarto needed)
#   make clean         Remove generated artefacts
#
# See RENDER.md for prerequisites.

PANDOC ?= pandoc
QUARTO ?= quarto
RM     ?= rm -f

SRC_MD       = wp_draft.md
BODY_MD      = _wp_body.md
QMD          = wp_draft.qmd
BIB          = wp_references.bib

PDF_OUT      = wp_draft.pdf
DOCX_OUT     = wp_draft.docx
HTML_OUT     = wp_draft.html
DOCX_PANDOC  = wp_draft_pandoc.docx

# Typst PDF metadata
TITLE        = Australian Household Consumption, Wealth and Credit Conditions: An Updated Single-Equation LIVES Estimate
AUTHOR       = David Stephan
DATE         = June 2026

.PHONY: pdf pdf-quarto docx html docx-pandoc clean help body

help:
	@echo "Targets:"
	@echo "  make pdf            Render to PDF via pandoc + Typst (default; needs pandoc + typst)"
	@echo "  make pdf-quarto     Render to PDF via Quarto + LaTeX (needs quarto + tinytex)"
	@echo "  make docx           Render to docx via Quarto"
	@echo "  make html           Render to html via Quarto"
	@echo "  make docx-pandoc    Render to docx via plain pandoc (no Quarto needed)"
	@echo "  make clean          Remove generated files"
	@echo ""
	@echo "See RENDER.md for setup."

# ----------------------------------------------------------------------------
# Body partial: wp_draft.md minus the leading preamble (title + draft tag
# + author + JEL + keywords + first ---), which moves into Quarto YAML.
# Stops stripping at the first line starting "## Abstract".
# ----------------------------------------------------------------------------
$(BODY_MD): $(SRC_MD)
	@echo "[make] Building $(BODY_MD) from $(SRC_MD)"
	@awk '/^## 1\. Introduction$$/{found=1} found' $(SRC_MD) > $(BODY_MD)

body: $(BODY_MD)

# ----------------------------------------------------------------------------
# Default PDF: pandoc + Typst (lightweight, fast, no LaTeX dependency)
# ----------------------------------------------------------------------------
$(PDF_OUT): $(SRC_MD)
	$(PANDOC) $(SRC_MD) -o $(PDF_OUT) \
	    --from gfm \
	    --pdf-engine typst \
	    --toc \
	    --toc-depth 3 \
	    --number-sections \
	    -V title="$(TITLE)" \
	    -V author="$(AUTHOR)" \
	    -V date="$(DATE)" \
	    -V papersize=a4 \
	    -V margin-left=2cm \
	    -V margin-right=2cm \
	    -V margin-top=2.5cm \
	    -V margin-bottom=2.5cm \
	    -V fontsize=11pt

pdf: $(PDF_OUT)

# ----------------------------------------------------------------------------
# Quarto rendering targets (alternative to default pandoc+Typst path)
# ----------------------------------------------------------------------------
pdf-quarto: $(QMD) $(BODY_MD) $(BIB)
	$(QUARTO) render $(QMD) --to pdf

$(DOCX_OUT): $(QMD) $(BODY_MD) $(BIB)
	$(QUARTO) render $(QMD) --to docx

docx: $(DOCX_OUT)

$(HTML_OUT): $(QMD) $(BODY_MD) $(BIB)
	$(QUARTO) render $(QMD) --to html

html: $(HTML_OUT)

# ----------------------------------------------------------------------------
# No-Quarto stopgap: plain pandoc from wp_draft.md to docx. This skips the
# YAML metadata wrapping and just produces a Word document from the raw
# markdown. Useful for reviewer-friendly markup before Quarto is installed.
# ----------------------------------------------------------------------------
$(DOCX_PANDOC): $(SRC_MD)
	$(PANDOC) $(SRC_MD) -o $(DOCX_PANDOC) \
	    --from gfm \
	    --to docx \
	    --toc \
	    --number-sections

docx-pandoc: $(DOCX_PANDOC)

# ----------------------------------------------------------------------------
clean:
	$(RM) $(BODY_MD) $(PDF_OUT) $(DOCX_OUT) $(HTML_OUT) $(DOCX_PANDOC)
