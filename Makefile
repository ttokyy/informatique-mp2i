clean:
	for extension in aux log gz toc ;\
	do  \
		for file in $$(find . -type f -name "*.$${extension}") ;\
		do  \
			rm $${file} ;\
		done ;\
	done;
	for folder in $$(find . -type d -name "_minted*") ;\
	do  \
		rm -r $${folder} ;\
	done ;\


.PHONY: clean
