import fontforge, psMat
font = fontforge.open("tenevil-font.svg")
font.encoding = "UnicodeFull"
glyphs = font.selection.all()
glyphmove = psMat.translate(0,-300)
font.transform(glyphmove)
font.autoWidth(150)
font.addLookup('gen_kern', 'gpos_pair', (), [['liga', [['latn', ['dflt']]]]])
font.addLookupSubtable('gen_kern','gen_kern_subt')
font.autoKern('gen_kern_subt',150,touch=1)
glyphs = font.selection.select(("ranges",None),0xf0080,0xf00ff)
font.autoWidth(50)
font.generate("tenevil-font.otf")
