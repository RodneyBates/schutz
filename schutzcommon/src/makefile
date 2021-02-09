
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2021, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File makefile, subdirectory schutzcommon/src, Schutz Semantic editor.

# Generate files derived/MessageCodes.[im]3 from MessageCodes.[im]30, by scanning  
# many source files for references to message codes.   
# Generate file derived/Version.i3 from Version.i30. 

SED = sed 
AWK = awk
SORT = sort
DIFF = diff
GREP = grep
DATE = date
CPP  = /lib/cpp -P

# Files to scan: 
ERRORFILES = \
  Assertions.m3 \
  AssertUnsafe.m3 \
  Coroutine.m3 \
  EstBuild.m3 \
  EstHs.m3 \
  EstUtil.m3 \
  FsTreeUtils.m3\
  LangUtil.m3 \
  LangUtilLo.m3 \
  LbeStd.m3 \
  LineMarks.m3 \
  Marks.m3 \
  MergeTxt.m3 \
  ModHs.m3 \
  ParseHs.m3 \
  ParseTrv.m3 \
  Parser.m3 \
  PortTypes.m3 \
  SharedStrings.m3 \
  Strings.m3 \
  TravUtil.m3 \
  WriteTrv.m3 \
  LdlSemantics.m3 \
  AstView.m3 \
  LangMap.m3 \
  TextIntSymbolTable.m3 \
  TokRelation.m3 \
  LRTable.m3 \
  GenConstEst.m3 \
  Files.m3 \
  TreeBrowse.m3 \
  PaintHs.m3 \
  GrammarGen.m3 \
  Layout.m3 \
  ../../edit/src/Ui.m3 \
  ../../edit/src/Display.m3 \
  ../../edit/src/TempMark.m3 \
  ../../edit/src/TextEdit.m3 \
  ../../edit/src/EditWindow.m3 \
  ../../edit/src/Selection.m3 \
  ../../devel/src/LexTable.m3 \
  ../../lalr/src/Automaton.m3 \
  ../../lalr/src/Debug.m3 \
  ../../lalr/src/Check.m3 \
  ../../lalr/src/Gen.m3 \
  ../../lalr/src/LALRLookahead.m3 \
  ../../lalr/src/LRUtils.m3 \
  ../../lalr/src/Reduce.m3 \
  ../../ldl0/src/Ldl0FsTrees.m3 \
  ../../ldl0/src/Ldl0Semantics.m3 \
  ../../ldl0/src/Ldl0Tok.m3 \
  ../../ldl0/src/ManualAsTrees.m3 \
  ../../ldl0/src/Ldl0Scanner.m3 \
  ../../ldl1/src/Ldl1FsTrees.m3 \
  ../../ldl1/src/Ldl1Semantics.m3 \
  ../../ldl1/src/Ldl1Tok.m3 \
  ../../ldl1/src/Ldl1Scanner.m3 \
  ../../m3/src/M3Scanner.m3 \
  ../../m3/src/M3Tok.m3 \

   

SEDRULES1 = -n \
  -e 's/^.*AFT *\. *\([IWEFA][a-zA-Z0-9_]*\).*/\1/p'

SEDRULES1a = \
  -e 's/^I__*\(.*\)/\1/' \
  -e 's/^W__*\(.*\)/\1/' \
  -e 's/^E__*\(.*\)/\1/' \
  -e 's/^F__*\(.*\)/\1/' \
  -e 's/^A__*\(.*\)/\1/' 

SEDRULES1b = \
  -e 's/^[^_]*$$//' \
  -e 's/^\([^_]*\)_\(.*\)/\1/'

SEDRULES1c = \
  -e 's/^\([^_]*\)_\(.*\)/\2/'

SEDRULES2 = \
  -e 's/\([[:lower:][:digit:]]\)\([[:upper:]]\)/\1 \2/g' \
  -e 's/_/ /g' \
  -e 'y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/'

AWKRULES2c = \
  '{ getline trailer < "../derived/codes2c"; print $$0 , trailer } '

AWKRULES3 = \
  '{ getline trailer < "../derived/codes2d"; print $$0 , trailer } '

SEDRULES4 = \
  -e 's/^/      , /'

SEDRULES5 = \
  -e 's/^\([a-zA-Z0-9_]*\) \(.*\)/      | T . \1 => RETURN "\2" ; /'

SEDRULES6 = -n \
  -e 's/^\(I[a-zA-Z0-9_]*\)/      | T . \1 => RETURN KindTyp . MkInformation ; /p' \
  -e 's/^\(W[a-zA-Z0-9_]*\)/      | T . \1 => RETURN KindTyp . MkWarning ; /p' \
  -e 's/^\(E[a-zA-Z0-9_]*\)/      | T . \1 => RETURN KindTyp . MkError ; /p' \
  -e 's/^\(F[a-zA-Z0-9_]*\)/      | T . \1 => RETURN KindTyp . MkFatal ; /p' \
  -e 's/^\(A[a-zA-Z0-9_]*\)/      | T . \1 => RETURN KindTyp . MkAssert ; /p'

all : mkderived ../derived/MessageCodes.i3 ../derived/MessageCodes.m3 \
      ForceVersion ../derived/Version.i3

mkderived:
	sh -c "if test ! -d ../derived; then mkdir ../derived; else true; fi"

ForceVersion : 
	rm -f ../derived/Version.i3

../derived/Version.i3 : Version.i30
	$(SED) -e "s/%DATESTRING%/`$(DATE)`/" Version.i30 > ../derived/Version.i3

../derived/codes1 ../derived/codes2 ../derived/codes3 ../derived/codes4 ../derived/codes5 \
../derived/codes6 : $(ERRORFILES) 
	$(SED) $(SEDRULES1) $(ERRORFILES) | $(SORT) - > ../derived/codes0 #List of codes
	$(SORT) -u ../derived/codes0 > ../derived/codes1           #Codes with no duplicates 
	if ! $(DIFF) ../derived/codes0 ../derived/codes1 | $(GREP) "^< *" | $(SED) -e 's/^> *//' > ../derived/duplicatecodes ;\
	then echo "******** Duplicate message codes exist.  See file ../derived/duplicatecodes. **********XS" ;\
	fi
	$(SED) $(SEDRULES1a) ../derived/codes1  > ../derived/codes1a # Kind removed 
	$(SED) $(SEDRULES1b) ../derived/codes1a > ../derived/codes1b # Prefixes of texts 
	$(SED) $(SEDRULES1c) ../derived/codes1a > ../derived/codes1c # Suffixes of texts 
	$(SED) $(SEDRULES2)  ../derived/codes1c > ../derived/codes2c # Suffix texts with blanks  
	$(AWK) $(AWKRULES2c) ../derived/codes1b > ../derived/codes2d # Rejoined texts 
	$(AWK) $(AWKRULES3)  ../derived/codes1  > ../derived/codes3  # Code-text pairs
	$(SED) $(SEDRULES4)  ../derived/codes1  > ../derived/codes4  # Codes as enumeration literals 
	$(SED) $(SEDRULES5)  ../derived/codes3  > ../derived/codes5  # CASE alts for PROCEDURE Text 
	$(SED) $(SEDRULES6)  ../derived/codes1  > ../derived/codes6  # CASE alts for PROCEDURE Kind 

../derived/MessageCodes.i3 : MessageCodes.i30 ../derived/codes4
	$(CPP) < MessageCodes.i30 > ../derived/MessageCodes.i3

../derived/MessageCodes.m3 : MessageCodes.m30 ../derived/codes5 ../derived/codes6 
	$(CPP) < MessageCodes.m30 > ../derived/MessageCodes.m3


