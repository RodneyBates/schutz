
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Ascii 

(* Names for nonprintable Ascii characters. *) 

; CONST null = '\000' 
; CONST soh = '\001' 
; CONST stx = '\002' 
; CONST etx = '\003' 
; CONST eot = '\004' 
; CONST enq = '\005' 
; CONST ack = '\006' 
; CONST bel = '\007' 
; CONST bs = '\010' 
; CONST ht = '\011' 
; CONST lf = '\012' 
; CONST vt = '\013' 
; CONST ff = '\014' 
; CONST cr = '\015' 
; CONST so = '\016' 
; CONST si = '\017' 
; CONST dle = '\020' 
; CONST dc1 = '\021' 
; CONST dc2 = '\022' 
; CONST dc3 = '\023' 
; CONST dc4 = '\024' 
; CONST nak = '\025' 
; CONST syn = '\026' 
; CONST etb = '\027' 
; CONST can = '\030' 
; CONST em = '\031' 
; CONST sub = '\032' 
; CONST esc = '\033' 
; CONST fs = '\034' 
; CONST gs = '\035' 
; CONST rs = '\036' 
; CONST us = '\037' 
; CONST FirstPrintable = '\040' 
(* REVIEW: Would it be of any use to give the printable chars identifiers? 
   Or would that be like alphabetizing the roman numerals? *) 
; CONST LastPrintable = '\176' 
; CONST del = '\177' 

; END Ascii 
. 
