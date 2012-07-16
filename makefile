all:	
	ghc Main.hs -O2 -o lifter
	ghc Installer.lhs -O2 -o installer
