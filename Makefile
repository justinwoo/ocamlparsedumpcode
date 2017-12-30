default:
	jbuilder build main.exe
	_build/default/main.exe
install:
	opam install core angstrom
