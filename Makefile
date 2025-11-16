index.html: index_template.html publications.bib build_index.py
	python3 build_index.py publications.bib index_template.html index.html
