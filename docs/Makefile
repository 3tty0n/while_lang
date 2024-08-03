APP = text_japanese

$(APP).pdf: $(APP).tex
	latexmk $<

clean:
	latexmk -C $(APP)
