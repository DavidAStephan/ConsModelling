# Render the companion paper to PDF / docx / html. Mirrors the
# headline paper's setup in Australia/docs/Makefile. See ../../Australia/docs/RENDER.md
# for setup notes; the same prerequisites (Quarto + TinyTeX/MacTeX)
# apply.

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

.PHONY: pdf docx html docx-pandoc clean help body

help:
	@echo "Targets:"
	@echo "  make pdf            Render via Quarto to PDF (needs quarto + tinytex)"
	@echo "  make docx           Render via Quarto to docx"
	@echo "  make html           Render via Quarto to html"
	@echo "  make docx-pandoc    Render to docx via plain pandoc"
	@echo "  make clean          Remove generated files"

$(BODY_MD): $(SRC_MD)
	@echo "[make] Building $(BODY_MD) from $(SRC_MD)"
	@awk '/^## 1\. Introduction$$/{found=1} found' $(SRC_MD) > $(BODY_MD)

body: $(BODY_MD)

$(PDF_OUT): $(QMD) $(BODY_MD)
	$(QUARTO) render $(QMD) --to pdf

pdf: $(PDF_OUT)

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
