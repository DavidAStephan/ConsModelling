# Render the companion paper to PDF / docx / html. Mirrors the
# headline paper's setup in Australia/docs/Makefile. See ../../Australia/docs/RENDER.md
# for setup notes.
#
# Default PDF path: pandoc + Typst (no LaTeX dependency). Alternative
# Quarto path retained as `make pdf-quarto`.

PANDOC ?= pandoc
QUARTO ?= quarto
RM     ?= rm -f

SRC_MD       = companion_paper_draft.md
BODY_MD      = _companion_body.md
QMD          = companion_paper_draft.qmd

PDF_OUT      = companion_paper_draft.pdf
DOCX_OUT     = companion_paper_draft.docx
HTML_OUT     = companion_paper_draft.html
DOCX_PANDOC  = companion_paper_draft_pandoc.docx

TITLE        = A Multi-Equation LIVES System for Australia: Joint Identification of Credit Conditions and Williams' Calibrations Tested
AUTHOR       = David Stephan
DATE         = May 2026

.PHONY: pdf pdf-quarto docx html docx-pandoc clean help body

help:
	@echo "Targets:"
	@echo "  make pdf            Render to PDF via pandoc + Typst (default)"
	@echo "  make pdf-quarto     Render to PDF via Quarto + LaTeX"
	@echo "  make docx           Render to docx via Quarto"
	@echo "  make html           Render to html via Quarto"
	@echo "  make docx-pandoc    Render to docx via plain pandoc"
	@echo "  make clean          Remove generated files"

$(BODY_MD): $(SRC_MD)
	@echo "[make] Building $(BODY_MD) from $(SRC_MD)"
	@awk '/^## 1\. Introduction$$/{found=1} found' $(SRC_MD) > $(BODY_MD)

body: $(BODY_MD)

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

pdf-quarto: $(QMD) $(BODY_MD)
	$(QUARTO) render $(QMD) --to pdf

$(DOCX_OUT): $(QMD) $(BODY_MD)
	$(QUARTO) render $(QMD) --to docx

docx: $(DOCX_OUT)

$(HTML_OUT): $(QMD) $(BODY_MD)
	$(QUARTO) render $(QMD) --to html

html: $(HTML_OUT)

$(DOCX_PANDOC): $(SRC_MD)
	$(PANDOC) $(SRC_MD) -o $(DOCX_PANDOC) \
	    --from gfm \
	    --to docx \
	    --toc \
	    --number-sections

docx-pandoc: $(DOCX_PANDOC)

clean:
	$(RM) $(BODY_MD) $(PDF_OUT) $(DOCX_OUT) $(HTML_OUT) $(DOCX_PANDOC)
