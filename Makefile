
transpiler : main.rkt parse.rkt c.rkt python.rkt
	raco exe -o transpiler main.rkt

clean :
	rm -rfv transpiler.* compiled *.bak