
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001 .. 2007, from Cocktail, lalr, Default.md, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(* Compute default states *) 

INTERFACE Default 

; IMPORT LRTable 

; TYPE Public 
    = OBJECT 
      METHODS 
        Init ( Gram : LRTable . GrammarTyp ) : T 
                  (* Erzeugen / Initialisieren der Datenstruktur zur 
                     Berechnug der Defaultzustaende. *) 
      END (* OBJECT *) 

; TYPE T <: Public 

; VAR NoDefault : BOOLEAN := FALSE 
  (* Setting this true will prevent the use of default rows in table 
     compression, giving bigger LALR tables with faster access. *) 

; PROCEDURE New ( Gram : LRTable . GrammarTyp ) : T 
  (* Equivalent to NEW ( T ) . Init ( Gram ) *) 

; PROCEDURE PutInDefaultList 
    ( Self : T 
    ; ReadState : LRTable . StateNoTyp 
    ; READONLY TableLine : LRTable . StateNoArrayRefTyp 
    ) 
  (* Eintragen der in TableLine enthaltenen Zeile in die Datenstruktur *) 

; PROCEDURE ComputeDefaults ( Self : T ) 
  (* Berechnung der variablen Defaultzustaende und entfernen, der dadurch 
  in der Tabelle (Datenstruktur) ueberfluessigen Eintraege *) 

; PROCEDURE GetNextStateNo 
    ( Self : T ; State : LRTable . StateNoTyp ) : LRTable . StateNoTyp 
  (* Funktion zum weiterschalten des Zustandes nach interner Strategie *) 

; PROCEDURE GetTSortState 
    ( Self : T ; StateNo : LRTable . StateNoTyp ) : LRTable . StateNoTyp 
  (* Funktion zum weiterschalten des Zustandes nach interner Strategie *) 

; PROCEDURE GetNSortState 
    ( Self : T ; StateNo : LRTable . StateNoTyp ) : LRTable . StateNoTyp 
  (* Funktion zum weiterschalten des Zustandes nach interner Strategie *) 

; PROCEDURE ConstructDefaultedRow 
    ( Self : T 
    ; ReadState : LRTable . StateNoTyp 
    ; VAR TableLine : LRTable . StateNoArrayRefTyp 
      (* ^Preallocated by caller, but filled in entirely by this procedure. *) 
    ; VAR DefaultStateNo : LRTable . StateNoTyp 
    ) 
  (* Auslesen der durch 'ReadState' bezeichneten Zeile aus der 
     Datenstruktur *) 

; END Default 
. 

