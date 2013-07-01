      * /* OMGWTF2 contest entry */
      * 
      * /* Copyright 2013 by Charles Anthony */
      * 
      * /*
      *     This program is free software: you can redistribute it and/or modify
      *     it under the terms of the GNU General Public License as published by
      *     the Free Software Foundation, either version 3 of the License, or
      *     (at your option) any later version.
      * 
      *     This program is distributed in the hope that it will be useful,
      *     but WITHOUT ANY WARRANTY; without even the implied warranty of
      *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      *     GNU General Public License for more details.
      * 
      *     You should have received a copy of the GNU General Public License
      *     along with this program.  If not, see
      * <http://www.gnu.org/licenses/>.
      * */

      ******************************************************************
      ******************************************************************
      ******************************************************************
      ***
      *** omgwtf2
      ***
      ******************************************************************
      ******************************************************************
      ******************************************************************

       identification division.

       program-id.  omgwtf2.

       environment division.

       input-output section.

      * "Executive Decision Maker.atr" from:
      *
      * http://www.atarimania.com/game-atari-400-800-xl-xe-executive-decision-maker_20061.html

       file-control.
           select atr-fd assign "Executive Decision Maker.atr"
             organization is record sequential.

       data division.

       file section.

       fd atr-fd is global.

       01 atr-file is global.
           05 atr-header.
               10 atr-header-wMagic usage binary-short unsigned.
               10 atr-header-wPars usage binary-short unsigned.
               10 atr-header-wSecSize usage binary-short unsigned.
               10 atr-header-btParsHigh usage binary-char unsigned.
               10 atr-header-dwCRC usage binary-long unsigned.
               10 atr-header-dwUnused usage binary-long unsigned.
               10 atr-header-btFlags usage binary-char unsigned.
           05 atr-sectors occurs 720 times.
               10 atr-sectors-bytes occurs 128 times.
                   15 atr-sectors-byte usage binary-char unsigned.

       working-storage section.

       copy "constants.cpy".

       01 nv is global usage binary-short unsigned.

      * struct variableTable
      *   {
      *     char * name;
      *     int type; // 0: scalar, 1: string
      *     int dim;
      *     double val; // scalar
      *     char * sval; // string
      *   };

       01 variableTableStruct is global.
           05 vts occurs 256 times.
               10 vts-name usage pointer.
               10 vts-type usage binary-long signed. *> 0: scalar, 1: string
               10 vts-dim usage binary-long signed.
               10 vts-val usage computational-2.     *> scalar
               10 vts-sval usage pointer.            *> string

       01 lineNumberTable is global value NULL.
           05 lineNumberRows occurs H"10000" times.
               10 lineNumberRow usage pointer.

       01 lineStart is global usage pointer.
       01 lineNext is global usage pointer.
       01 tokenEnd is global usage pointer.

       01 trapJmp is global pic x(512).
       01 lexLinePtr is global usage pointer.

       01 lexState is global usage binary-long.

       01 returnStack is global.
           05 returnStackRows occurs 16 times.
               10 rs-tokenEnd usage pointer.
               10 rs-lineNext usage pointer.
               10 rs-lineStart usage pointer.

       01 rsp is global usage binary-long signed value 1.

       01 trapLine is global usage binary-long signed.
       01 printChannel is global usage binary-long signed.

      **
      ** graphics state variables
      **

       01 posCol is global usage binary-long signed.
       01 posRow is global usage binary-long signed.
       01 gInit is global usage binary-long signed.
       01 gMode is global usage binary-long signed.
       01 stdscr is global usage pointer.
      *01 gWin is global usage pointer.
      *01 tWin is global usage pointer.
       01 lastKBChar is global usage binary-char unsigned.
       01 leftMargin is global usage binary-char unsigned value zero.

       01 forData is global.
           05 fd-indexVarNum usage binary-long signed.
           05 fd-limit usage computational-2.
           05 fd-step usage computational-2.
           05 fd-tokenEnd usage is pointer.
           05 fd-lineNext usage is pointer.
           05 fd-lineStart usage is pointer.

       01 substrBuffer is global.
           05 substrBufferChars occurs 257 times.
               10 substrBufferChar usage is binary-char unsigned.

       01 programEnd is global usage pointer.

       01 yylval is global usage binary-double unsigned.
       01 currentLineNumber is global usage binary-long signed.

       01 programFile is global usage pointer.

       01 vnt is global usage binary-short unsigned.
       01 vvt is global usage binary-short unsigned.
       01 stmtab is global usage binary-short unsigned.
       01 stmcur is global usage binary-short unsigned.
       01 vvte is global usage binary-short unsigned.
       01 vvtl is global usage binary-short unsigned.
       01 codeLen is global usage binary-short unsigned.

       01 linePtr is global usage pointer.

       01 firstLineNumber is global usage binary-short unsigned.
       01 lastLineNumber is global usage binary-short unsigned.

       01 iseed is global usage binary-long signed.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      **
      ** main
      **

       procedure division.

       call "omgwtf2_main"

       exit program.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      **
      ** Common utility routines
      **  fatal
      **  fatalN
      **

      ******************************************************************
      **
      ** fatalN
      **

       identification division.

       program-id. fatal.

       environment division.

       data division.

       working-storage section.

       linkage section.
       01 desc usage is pointer.

       procedure division using by value desc.

       call "endwin"
       call "printf" using "Fatal error: %s" & x"0a", by value desc
       stop run.

       end program fatal.

      ******************************************************************
      **
      ** fatalN
      **

       identification division.

       program-id. fatalN.

       environment division.

       data division.

       working-storage section.

       linkage section.

       01 desc usage is pointer.
       01 n usage is binary-long signed.

       procedure division using by value desc, by value n.

       call "endwin"
       call "printf" using "Fatal error: %s %d (0x%02x)" & x"0a",
                            by value desc, n, n
       stop run.

       end program fatalN.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      **
      ** readAtrFile: Extract a file from an Atari disk image file
      **
      **
      **
      ** Format of ATR files:
      ** 
      ** 
      ** http://www.atarimax.com/jindroush.atari.org/afmtatr.html
      ** 
      ** DWORD - 32bit unsigned long (little endian) 
      ** WORD - 16bit unsigned short (little endian) 
      ** BYTE - 8bit unsigned char
      ** Header
      ** 
      ** 16 bytes long. 
      **  
      ** Type Name    Description
      ** WORD  wMagic  $0296 (sum of 'NICKATARI')
      ** WORD  wPars   size of this disk image, in paragraphs (size/$10)
      ** WORD  wSecSize        sector size. ($80 or $100) bytes/sector
      ** BYTE  btParsHigh      high part of size, in paragraphs (added by REV
      **00)
      ** DWORD         dwCRC   32bit CRC of file (added by APE?)
      ** DWORD         dwUnused        unused
      ** BYTE  btFlags         bit 0 (ReadOnly) (added by APE?)
      ** 
      ** Body
      ** 
      ** Then there are continuous sectors. Some ATR files are incorrect - if 
      ** sector size is > $80 first three sectors should be $80 long. But, few 
      ** files have these sectors $100 long.


      ******************************************************************
      **
      ** readAtrFile
      **

       identification division.

       program-id. readAtrFile.

       environment division.

       data division.

       working-storage section.

       01 diskSize usage binary-long unsigned.

       linkage section.

       procedure division.

       if function length (atr-header) not equal to 16 then
           display "struct header wrong size"
           stop run
       end-if.

       open input atr-fd.
       read atr-fd record into atr-file.
       close atr-fd.

       if atr-header-wMagic not equal to H"0296" then
           display "ATR header magic value wrong"
           stop run
       end-if.

       compute diskSize equal ((atr-header-btParsHigh * H"10000") +
                               atr-header-wPars) * 16.

       if diskSize + 16 not equal to 92176 then
           display "diskSize value wrong"
           stop run
       end-if.

       goback.

       end program readAtrFile.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      **
      ** readBasicFile: Locate a file in the disk image and extract
      **
      ** disk organization:
      **    http://www.atariarchives.org/iad/chapter2.php

       identification division.

       program-id. readBasicFile.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".

      * Number of sectors of directory entries
       01 numDirSectors constant as 8.

      * First directory sector (the above web page is 1-based sector #s)
       01 startDirSector constant as H"169".

      * Number of directory entries in a directory sector
       01 numDirEntries constant as 8.

      * primary file name length
       01 pnameLen constant as 8.

      * file name extension length
       01 extLen constant as 3.

      * entry deleted flag bit
       01 flagDeleted constant as H"80".

      * entry in use flag bit
       01 flagInUse constant as H"40".

      * number of byte bytes in a data sector
       01 numDataBytes constant as 125.

      * mask out file number from next sector number field
       01 nextMask constant as H"3".

       01 i usage binary-long unsigned.
       01 j usage binary-long unsigned.
       01 entryNum usage binary-long.
       01 found usage binary-long.
       01 fileSize usage binary-long.
       01 cnt usage binary-short unsigned.
       01 ssn usage binary-short unsigned.
       01 sn usage binary-short unsigned.
       01 programFileUsed usage binary-long unsigned.
       01 used usage binary-long unsigned.
       01 pfp usage pointer.

      * struct dirSector
      *   {
      *     struct dirEntry entries [numDirEntries];
      *   };

        01 dirSector based.
            05 aDirSector occurs numDirEntries times.
                10  dirEntry-flag usage binary-char signed.
                10  dirEntry-count usage binary-short unsigned.
                10  dirEntry-ssn usage binary-short unsigned.
                10  dirEntry-pname pic x(pnameLen).
                10  dirEntry-ext pic x(extLen).

      * struct dataSector
      *   {
      *     byte data [numDataBytes];
      *     // Figure 2-3 in the documentation is wrong; the text is
      *     // correct
      *     byte nextSectorNumHi; // File number encoded in high six bits
      *     byte nextSectorNumLo;
      *     byte numBytesUsed;
      *   } __attribute__((packed));
 
       01 dataSector based.
           02 dataSector-data occurs numDataBytes times.
               04 dataSector-bytes usage binary-char unsigned.
           02 dataSector-nextSectorNumHi usage binary-char unsigned.
           02 dataSector-nextSectorNumLo usage binary-char unsigned.
           02 dataSector-numBytesUsed usage binary-char unsigned.

       01 pfp-data based.
           02 pfpDataSector-data occurs numDataBytes times.
               04 pfpDataSector-bytes usage binary-char unsigned.

       linkage section.

       procedure division.

       perform with test before
         varying i from 0 by 1 until i 
         is greater than or equal to numDirSectors

           set address of dirSector to address of
             atr-sectors(startDirSector + i)

           perform with test before 
             varying entryNum from 1 by 1 until entryNum 
             is greater than numDirEntries

               if dirEntry-flag(entryNum) equal to 66 and
                  dirEntry-pname(entryNum) equal to "DECISION" and
                  dirEntry-ext(entryNum) equal to "BAS" then
                   move 1 to found
                   exit perform
               end-if

           end-perform

           if found not equal zero then
             exit perform
           end-if

       end-perform.

       if found equal zero then
         display "program file not found"
         stop run
       end-if
          
      * unsigned int fileSize = entry -> count * numDataBytes;

       compute fileSize equal dirEntry-count(entryNum) * 
             numDataBytes.

       allocate (fileSize) characters returning programFile.
       move dirEntry-count(entryNum) to cnt
       move dirEntry-ssn(entryNum) to ssn
       move zero to programFileUsed
       move programFile to pfp

       move ssn to sn

       perform varying i from 0 by 1 until i 
         is greater than or equal to cnt

           set address of dataSector to address of
             atr-sectors(sn)

           set address of pfp-data to pfp

           compute used equal programFileUsed +  dataSector-numBytesUsed
           if used greater than fileSize then
             call "fatal" using "overran programFile"
           end-if

           perform varying j from 1 by 1 until 
             j greater than numDataBytes
               move dataSector-data(j) to pfpDataSector-data(j)
           end-perform

           set pfp up by dataSector-numBytesUsed
           add dataSector-numBytesUsed to programFileUsed
           compute sn equal 
             function mod (dataSector-nextSectorNumHi, 4) *
             256 + dataSector-nextSectorNumLo

       end-perform.

       end program readBasicFile.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      **
      ** analyzeBasicFile: Extract symbol table and line number
      ** information from an Atari BASIC save file
      **

       identification division.

       program-id. analyzeBasicFile.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".
       copy "casts.cpy".

       01 i usage binary-long unsigned.
       01 j usage binary-long unsigned.
       01 lineNumber usage binary-short unsigned.
       01 lineLength usage binary-char unsigned.
       01 wcor usage binary-short unsigned.
       01 name usage pointer.
       01 char usage binary-char unsigned.

      * Documentation at:
      *   http://www.atarimax.com/jindroush.atari.org/afmtbas.html
      * Code borrowed from:
      *   http://www.atarimax.com/jindroush.atari.org/data/asoft/chkbas_src.zip

      * struct header
      *   {
      *     word lomem;
      *     word vnt;
      *     word vnte;
      *     word vvt;
      *     word stmtab;
      *     word stmcur;
      *     word starp;
      *   } __attribute__((packed));

       01 header based.
           05 hdr-lomem usage binary-short unsigned.
           05 hdr-vnt usage binary-short unsigned.
           05 hdr-vnte usage binary-short unsigned.
           05 hdr-vvt usage binary-short unsigned.
           05 hdr-stmtab usage binary-short unsigned.
           05 hdr-stmcur usage binary-short unsigned.
           05 hdr-starp usage binary-short unsigned.

      * struct vvtEntry
      *   {
      *     byte btType;
      *     byte btNumber;
      *     byte data [6];
      *   };

       01 vvtEntryStruct based.
           05 vvtTable occurs 256 times.
               10 btType usage binary-char unsigned.
               10 btNumber usage binary-char unsigned.
               10 btDataArray occurs 6 times.
                   20 btData usage binary-char unsigned.
       01 vvtEntry usage pointer.

       01 len usage binary-long signed.
       01 name-array based.
           05 name-array-chars occurs 256 times.
               10 name-array-char usage binary-char unsigned.

       01 str usage pointer.
       01 str-array based.
           05 str-array-chars occurs 256 times.
               10 str-array-char usage binary-char unsigned.

       linkage section.

       procedure division.

       set address of header to programFile

      *    display "lomem " hdr-lomem
      *    display "vnt " hdr-vnt
      *    display "vnte " hdr-vnte
      *    display "vvt " hdr-vvt
      *    display "stmtab " hdr-stmtab
      *    display "stmcur " hdr-stmcur
      *    display "starp " hdr-starp

       compute wcor equal hdr-vnt - hdr-lomem - H"0e"
       compute vnt equal hdr-vnt - wcor
       compute vvt equal hdr-vvt - wcor
       compute stmtab equal hdr-stmtab - wcor
       compute stmcur equal hdr-stmcur - wcor
       compute vvte equal stmtab - 1
       compute vvtl equal vvte - vvt + 1
       compute nv equal vvtl / 8
       compute codeLen equal stmcur - stmtab

      *    display "Constants & pointers:"
      *    display "Start of Name Table      (VNT)   : " vnt
      *    display "Start of Variable Table  (VVT)   : " vvt
      *    display "End of Variable Table    (VVTE)  : " vvte
      *    display "Length of Variable Table (VVTL)  : " vvtl

      *    display "Number of Variables      (NV)    : " nv

      *    display "Start of Code            (STMTAB): " stmtab
      *    display "Length of Code                   : " codeLen
      *    display "Current command          (STMCUR): " stmcur


       move programFile to name
       set name up by vnt
       set address of name-array to name

       perform varying i from 1 by 1 until i 
         is greater than nv

           move 1 to len
           perform with test before until
             name-array-char(len) greater than 127
               add 1 to len
           end-perform

           allocate (len + 1) characters returning str
           set address of str-array to str

           perform varying j from 1 by 1 until 
             j greater than len
               move name-array-chars(j) to char
               if char greater than 127 then
                 subtract 128 from char
               end-if
               move char to str-array-char(j)
           end-perform
           move H"00" to str-array-char(len + 1)
           move str to vts-name(i)
           set name up by len
           set address of name-array to name

       end-perform

       move programFile to vvtEntry
       set vvtEntry up by vvt              
       set address of vvtEntryStruct to vvtEntry

       perform varying i from 1 by 1 until i 
         is greater than nv

           move btType(i) to vts-type(i)
           move 0 to vts-dim(i)
           move 0 to vts-val(i)
           move NULL to vts-sval(i)
       end-perform

       move zero to firstLineNumber
       move zero to lastLineNumber

       move programFile to linePtr
       set linePtr up by stmtab

       move programFile to programEnd
       set programEnd up by stmtab
       set programEnd up by codeLen

       perform until linePtr greater than or equal to
         programEnd

           move linePtr to lineStart

           set address of wordCast to linePtr
           move wordCasted to lineNumber
           set linePtr up by length of wordCast

           set address of byteCast to linePtr
           move byteCasted to lineLength
           set linePtr up by length of byteCast

           move lineStart to lineNext
           set lineNext up by lineLength

           if firstLineNumber equal to zero then
               move lineNumber to firstLineNumber 
           end-if
           move lineNumber to lastLineNumber

           set lineNumberRow(lineNumber) to lineStart
           move lineNext to linePtr

       end-perform

       goback.

       end program analyzeBasicFile.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      **
      ** Support routines for executing Atari BASIC program
      **

      ******************************************************************
      **
      ** parseNCONST
      **

       identification division.

       program-id. parseNCONST.

       environment division.

       data division.

       working-storage section.

       copy "casts.cpy".

       01 exp usage is binary-char unsigned.
       01 pom usage is binary-char unsigned.
       01 poml usage is binary-char unsigned.
       01 pomh usage is binary-char unsigned.
       01 num usage is binary-char unsigned.
       01 iexp usage is binary-long signed.
       01 i usage is binary-long signed.
       01 ptr usage is pointer.

       linkage section.
       01 linePtr usage is pointer.
       01 res usage is computational-2.

       procedure division using by reference linePtr, by reference res.

       move zero to res

       set address of byteCast to linePtr
       move byteCasted to exp
       set linePtr up by length of byteCast

       if exp equal zero then
           move zero to iexp
           set linePtr up by length of byteCast
           set linePtr up by length of byteCast
           set linePtr up by length of byteCast
           set linePtr up by length of byteCast
           set linePtr up by length of byteCast
       else
           compute iexp equal (function integer(exp) - 68) * 2
           perform varying i from 1 by 1 until i greater than 5
               
               set address of byteCast to linePtr
               move byteCasted to pom
               set linePtr up by length of byteCast

               compute pomh equal pom / 16
               compute poml equal function mod (pom, 16)

               compute num equal pomh * 10 + poml
               compute res equal res * 100
               add num to res

           end-perform
       end-if

       compute res equal res * function exp10 (iexp)

       goback.

       end program parseNCONST.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      **
      ** Graphics/text emulation
      **

      ******************************************************************
      **
      ** clearWindows
      **

       identification division.

       program-id. clearWindows.

       environment division.

       data division.

       working-storage section.

       procedure division.

       call "erase"
       call "refresh"
      *if tWin not equal null then
      *    call "werase" using by value tWin
      *    call "wrefresh" using by value tWin
      *end-if
      *if gWin not equal null then
      *    call "werase" using by value gWin
      *    call "wrefresh" using by value gWin
      *end-if
       move 0 to posCol
       move 0 to posRow
      *move 2 to leftMargin
       goback.

       end program clearWindows.

      ******************************************************************
      **
      ** setGraphicsMode
      **

       identification division.

       program-id. setGraphicsMode.

       environment division.

       data division.

       working-storage section.

       linkage section.

       01 vmode usage is computational-2.

       procedure division using by reference vmode.

       if gInit equal zero then
           call "initscr"
           call "getStdscr" using by reference stdscr
           call "halfdelay" using by value 1
           call "echo"
           call "nonl"
           call "intrflush" using by value stdscr, by value 0
           call "keypad" using by value stdscr, by value 1
      *    if vmode equal 2 then
      *        call "newwin" using 10, 20, 0, 10 returning gWin
      *        call "newwin" using 4, 40, 10, 0 returning tWin
      *    else
               call "wresize" using by value stdscr, 24, 40
      *        if gWin not equal null then
      *          call "delwin" using by value gWin
      *          move null to gWin
      *        end-if
      *        if tWin not equal null then
      *          call "delwin" using by value tWin
      *          move null to tWin
      *        end-if
      *    end-if
           move 1 to gInit
       end-if

       move vmode to gMode
       call "clearWindows"

       goback.

       end program setGraphicsMode.

      ******************************************************************
      **
      ** setPosition
      **

       identification division.

       program-id. setPosition.

       environment division.

       data division.

       working-storage section.

       01 c usage binary-long signed.

       linkage section.

       01 vcol usage is computational-2.
       01 vrow usage is computational-2.

       procedure division using by reference vcol, by reference vrow.

       move vcol to posCol
       move vrow to posRow

       compute c equal posCol + leftMargin
      *if gMode equal zero then
           call "move" using posRow, c
      *else
      *    call "wmove" using by value gWin, posRow, c
      *    call "wmove" using by value tWin, posRow, c
      *end-if

       goback.

       end program setPosition.

      ******************************************************************
      **
      ** setChannel
      **

       identification division.

       program-id. setChannel.

       environment division.

       data division.

       working-storage section.

       linkage section.

       01 channel usage is computational-2.

       procedure division using by reference channel.

       move channel to printChannel

       goback.

       end program setChannel.

      ******************************************************************
      **
      ** printChar
      **

       identification division.

       program-id. printChar.

       environment division.

       data division.

       working-storage section.

       01 c usage is binary-char unsigned.
       01 c0 usage is binary-char unsigned.
       01 invert usage is binary-long signed.
       01 w usage is pointer.
       01 a_reverse usage is binary-long signed value H"20000000".
       01 qm usage is binary-char signed value 63. *> '?'

       linkage section.

       01 char usage is binary-char unsigned.

       procedure division using by value char.

       move char to c
       move char to c0

       if c greater than 127 then
           subtract 128 from c
           move 1 to invert
       else
           move 0 to invert
       end-if

       if c equal to H"12" then
           move 45 to c *> '-'
       end-if

       if gInit not equal zero then

           if c0 equal to H"fd" then *> ATASCII bell
               call "beep"
               goback
           end-if

           if c0 equal to H"7d" then *> ATASCII clear screen
               call "clearWindows"
               goback
           end-if

      *    if gMode equal zero then
               move stdscr to w
      *    else
      *        if printChannel equal 6 then
      *            move gWin to w
      *        else
      *            move tWin to w
      *        end-if
      *    end-if

           if invert not equal zero then
               call "attrset" using by value a_reverse
           end-if

           *> toupper c
      *    if w equal to gwin then
      *        if c greater than 96 and c less than 123 then
      *            subtract 32 from c
      *        end-if
      *    end-if

           if (c greater than 31 and less than 127) or
              c equal to 10 then
               call "wechochar" using by value w, by value c
           else
               call "wechochar" using by value w, by value qm
           end-if
           call "wrefresh" using by value w
       else
           if c equal H"fd" then *> ATASCII bell
             goback
           end-if
           if (c greater than 31 and less than 127) or
              c equal to 10 then
               call "printf" using "%c", by value c
           else
               call "printf" using "<%02x>", by value c
           end-if
       end-if 

       goback.

       end program printChar.

      ******************************************************************
      **
      ** printNL
      **

       identification division.

       program-id. printNL.

       environment division.

       data division.

       working-storage section.

       procedure division.

       call "printChar" using by value 10
       move zero to printChannel

       goback.

       end program printNL.

      ******************************************************************
      **
      ** printExp
      **

       identification division.

       program-id. printExp.

       environment division.

       data division.

       working-storage section.

       01 sbuf.
           05 sbuf-chars occurs 256 times.
             10 sbuf-char usage binary-char unsigned.

       linkage section.

       01 exp usage is computational-2.

       procedure division using by reference exp.

       call "formatExp" using by reference exp, by reference sbuf
       call "printSExp" using by reference sbuf

       goback.

       end program printExp.

      ******************************************************************
      **
      ** printSExp
      **

       identification division.

       program-id. printSExp.

       environment division.

       data division.

       working-storage section.

       01 l usage binary-long signed.
       01 i usage binary-long signed.

       copy "casts.cpy".

       linkage section.

       01 exp usage is pointer.

       procedure division using by value exp.

       set address of byteCast to exp
       move byteCasted to l

       set address of countedString to exp

       perform varying i from 1 by 1 until i greater than l
         call "printChar" using by value cs-char(i)
       end-perform

       goback.

       end program printSExp.

      ******************************************************************
      **
      ** doInput
      **

       identification division.

       program-id. doInput.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".
       copy "casts.cpy".

       01 maxlen usage is binary-long signed.
       01 len usage is binary-long signed.
       01 i usage is binary-long signed.
       01 char usage is binary-long signed.
       01 buf pic x(255) usage is display.
       01 cbuf pic x usage is display.
       01 zbuf usage is binary-char unsigned value zero.

       01 frozen usage is binary-long signed value 0.
       01 hell usage is binary-long signed value 1.

       01 sval usage is pointer.

       linkage section.

       01 varNum usage is binary-long signed.

       procedure division using by value varNum.
      
       if varNum less than zero or greater than or equal to nv then
           call "fatalN" using "varNum out of range",
                               by value varNum
       end-if

      * varNum is 1-based
       add 1 to varNum

       if vts-type(varNum) not equal to vtTypeString then
           call "fatal" using "doInput expected a string variable"
       end-if

       if vts-sval(varNum) equal NULL then
           call "fatal" using "unallocated string"
       end-if

       move vts-dim(varNum) to maxLen

       call "nocbreak"
       call "nodelay" using by value stdscr, by value 0
       call "echo"
       call "printChar" using by value '?'

       move vts-sval(varNum) to sval
       set sval up by 1

       perform varying i from 1 by 1 until hell equal frozen

           call "getch" returning char
           if char equal -1 or equal 10 then
               exit perform
           end-if

           if i less than or equal maxLen then
               set address of svalCast to sval
               move char to svalCasted
               set sval up by 1
           end-if
       end-perform

       move vts-sval(varNum) to sval
       set address of svalCast to sval
       subtract 1 from i
       move i to svalCasted

       goback.

       end program doInput.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      **
      ** Variable access
      **

      ******************************************************************
      **
      ** getVarValue
      **

       identification division.

       program-id. getVarValue.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".
       copy "casts.cpy".

       01 addr usage is binary-long signed.
       01 i usage is binary-long signed.

       linkage section.

       01 varNum usage is binary-long signed.
       01 res usage is computational-2.

       procedure division using by value varNum, by reference res.

       if varNum less than zero or greater than or equal to nv then
         call "fatalN" using "varNum out of range", by value varNum
       end-if

      * varNum is 1-based
       add 1 to varNum

       if vts-type(varNum) not equal to vtTypeScalar then
         call "fatal" using "getVarValue expected a scalar variable"
       end-if

       move vts-val(varNum) to res

       goback.

       end program getVarValue.

      ******************************************************************
      **
      ** getSVarValue
      **

       identification division.

       program-id. getSVarValue.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".
       copy "casts.cpy".

       01 addr usage is binary-long signed.
       01 i usage is binary-long signed.

       linkage section.

       01 varNum usage is binary-long signed.
       01 res usage is pointer.

       procedure division using by value varNum, by reference res.

       if varNum less than zero or greater than or equal to nv then
         call "fatalN" using "varNum out of range", by value varNum
       end-if

      * varNum is 1-based
       add 1 to varNum

       if vts-type(varNum) not equal to vtTypeString then
         call "fatal" using "getSVarValue expected a string variable"
       end-if

       move vts-sval(varNum) to res

       goback.

       end program getSVarValue.

      ******************************************************************
      **
      ** getSVarValue1
      **

       identification division.

       program-id. getSVarValue1.

       environment division.

       data division.

       working-storage section.

       copy "casts.cpy".

       01 addr usage is binary-long signed.
       01 i usage is binary-long signed.

       linkage section.

       01 varNum usage is binary-long signed.
       01 sub usage is computational-2.
       01 res usage is pointer.

       procedure division using by value varNum, by reference sub,
                                by reference res.

       call "fatal" using "getSVarValue1 failed".

       end program getSVarValue1.

      ******************************************************************
      **
      ** getSVarValue2
      **

       identification division.

       program-id. getSVarValue2.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".
       copy "casts.cpy".

       01 addr usage is binary-long signed.
       01 isub1 usage is binary-long signed.
       01 isub2 usage is binary-long signed.
       01 len usage is binary-long signed.
       01 slen usage is binary-long signed.
       01 i usage is binary-long signed.
       01 j usage is binary-long signed.
       01 sval usage is pointer.

       linkage section.

       01 varNum usage is binary-long signed.
       01 sub1 usage is computational-2.
       01 sub2 usage is computational-2.
       01 res usage is pointer.

       procedure division using by value varNum, by reference sub1,
                                by reference sub2, by reference res.

           if varNum less than zero or greater than or equal to nv then
               call "fatalN" using "varNum out of range",
                                    by value varNum
           end-if

      * varNum is 1-based
           add 1 to varNum
 
           if vts-type(varNum) not equal to vtTypeString then
               call "fatal" using
                 "getSVarValue2 expected a string variable"
           end-if

           if vts-sval(varNum) equal to NULL then
               call "fatal" using "unallocated string"
           end-if

           move sub1 to isub1
           move sub2 to isub2

           move vts-sval(varNum) to sval
           set address of svalCast to sval
           move svalCasted to len

           if isub1 less than 1 or isub1 greater than len then
               call "fireTrap"
               call "fatalN" using "string subscript 1 out of range",
                                   by value isub1
           end-if

           if isub2 less than 1 or isub2 greater than len then
               call "fireTrap"
               call "fatalN" using "string subscript 2 out of range",
                                   by value isub2
           end-if

           if isub1 greater than isub2 then
               call "fireTrap"
               call "fatal" using "string subscript 2 > subscript1"
           end-if

           compute slen equal isub2 - isub1 + 1

           if slen > 256 then
             call "fireTrap"
             call "fatalN" using "improbable... substring too long",
                                   by value slen
           end-if

           move vts-sval(varNum) to sval
           set sval up by isub1
           move slen to substrBufferChar(1)

           perform varying i from 1 by 1 until i greater than slen
               set address of svalCast to sval
               compute j equal i + 1
               move svalCasted to substrBufferChar(j)
               set sval up by 1
           end-perform

           set res to address of substrBuffer

           goback.

       end program getSVarValue2.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      ***
      *** Math
      ***

      ******************************************************************
      **
      ** evalOp
      **

       identification division.

       program-id. evalOp.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".
       copy "casts.cpy".

       01 l usage binary-long signed.

       linkage section.

       01 exp1 usage is computational-2.
       01 op usage is binary-long signed.
       01 exp2 usage is computational-2.
       01 res usage is computational-2.

       procedure division using by reference exp1, by value op,
                                by reference exp2, by reference res.

       if op equal opPlus then
         compute res equal exp1 + exp2
         goback
       end-if

       if op equal opMinus1 then
         compute res equal exp1 - exp2
         goback
       end-if

       if op equal opNE then
          if exp1 not equal exp2 then
            move 1 to res
          else
            move 0 to res
          end-if
         goback
       end-if

       if op equal opEQ1 then
          if exp1 equal exp2 then
            move 1 to res
          else
            move 0 to res
          end-if
         goback
       end-if

       if op equal opMinus2 then
         compute res equal 0 - exp2
         goback
       end-if

       if op equal opTimes then
         compute res equal exp1 * exp2
         goback
       end-if

       if op equal opDiv then
         if exp2 equal 0 then
           call "fatal" using "div by 0"
         end-if
         compute res equal exp1 / exp2
         goback
       end-if

       if op equal opOr then
          if exp1 not equal 0 or exp2 not equal 0 then
            move 1 to res
          else
            move 0 to res
          end-if
         goback
       end-if

       call "fatalN" using "evalOp failed", by value op.

       end program evalOp.

      ******************************************************************
      **
      ** updateLastKBChar
      **

       identification division.

       program-id. updateLastKBChar.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".
       copy "casts.cpy".

       01 char usage is binary-long signed.

       procedure division.

       call "halfdelay" using by value 1
       call "noecho"

       call "getch" returning char

       if char not equal -1 then
           move char to lastKBChar 
      *    // For some reason, ATASCII escape is 28
           if lastKBChar equal 27 then
             move 28 to lastKBChar
           end-if
       end-if

       goback.

       end program updateLastKBChar.

      ******************************************************************
      **
      ** randu
      **

       identification division.

       program-id. randu.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".
       copy "casts.cpy".

       01 IMAX usage binary-long signed value 2147483647.
       01 fimax usage computational-1.
       01 XMAX_INV usage computational-1.

       linkage section.

       01 iseed usage binary-long signed.
       01 res usage computational-2.

       procedure division using by reference iseed, by reference res.

       move IMAX to fimax
       compute XMAX_INV equal 1.0 / fimax
       compute iseed equal iseed * 65539
       if iseed less than zero then
           compute iseed equal iseed + IMAX + 1
       end-if

       compute res equal iseed * XMAX_INV

       goback.

       end program randu.

      ******************************************************************
      **
      ** evalFunc
      **

       identification division.

       program-id. evalFunc.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".
       copy "casts.cpy".

       01 addr usage is binary-long signed.
       01 i usage is binary-long signed.

       linkage section.

       01 func usage is binary-long signed.
       01 arg usage is computational-2.
       01 res usage is computational-2.

       procedure division using by value func, by reference arg,
                                by reference res.

       if func equal opPeek then

           move arg to addr
           if addr equal H"d01f" then *> Console switches
               call "updateLastKBChar"
               if lastKBChar not equal 255
                   move 6.0 to res *> indicate that start is pressed XXX
                   goback
               end-if
               move 7.0 to res *> no keys pressed
               goback
           end-if

           if addr equal 764 then *> keypress
              call "updateLastKBChar"
              move lastKBChar to res
              goback
           end-if

           call "fatalN" using "peek failed", by value addr

       end-if

       if func equal opInt then
         move arg to i
         move i to res
         goback
       end-if

       if func equal opRnd
         *> XXX
         *> I can't get randu to work; the computes are done in decimal
         *> arithmeic, which seems to be an epic fail.
         *> call "randu" using by reference iseed, by reference res
         compute res equal function random ()
         goback
       end-if

       call "fatalN" using "evalFunc failed", by value func.

       end program evalFunc.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      **
      ** String manipulation
      **

      ******************************************************************
      **
      ** evalSOP
      **

       identification division.

       program-id. evalSOp.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".
       copy "casts.cpy".

       linkage section.

       01 exp1 usage is binary-long signed.
       01 op usage is binary-long signed.
       01 exp2 usage is pointer.
       01 res usage is computational-2.

       procedure division using by value exp1, by value op,
                                by value exp2, by reference res.

       call "fatal" using "evalSOp failed"

       goback.

       end program evalSOp.

      ******************************************************************
      **
      ** evalSFunc
      **

       identification division.

       program-id. evalSFunc.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".
       copy "casts.cpy".

       linkage section.

       01 func usage is binary-long signed.
       01 arg usage is pointer.
       01 res usage is computational-2.

       procedure division using by value func, by value arg,
                                by reference res.
       if func equal opLen then
         set address of byteCast to arg
         move byteCasted to res
         goback
       end-if

       call "fatalN" using "evalSFunc failed", by value func.

       end program evalSFunc.

      ******************************************************************
      **
      ** stringEQ
      *

       identification division.

       program-id. stringEq.

       environment division.

       data division.

       working-storage section.

       01 i usage binary-char signed.
       01 l1 usage binary-char signed.
       01 l2 usage binary-char signed.

       copy "casts.cpy".

       linkage section.

       01 str1 usage is pointer.
       01 str2 usage is pointer.
       01 res usage is computational-2.

       procedure division using by value str1, by value str2,
                                by reference res.

       set address of byteCast1 to str1
       set address of byteCast2 to str2
       move byteCasted1 to l1
       move byteCasted2 to l2

       if l1 not equal l2 then
         move 0 to res
         goback
       end-if

       set str1 up by 1
       set str2 up by 1
       call "strncasecmp" using by value str1,
           by value str2 by value l1 returning i
       if i not equal 0 then
         move 0 to res
       else
         move 1 to res
       end-if

       goback.

       end program stringEq.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      **
      ** Array allocation
      **

      ******************************************************************
      **
      ** dimSVar
      **

       identification division.

       program-id. dimSVar.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".
       copy "casts.cpy".

       01 addr usage is binary-long signed.
       01 idim usage is binary-long signed.

       linkage section.

       01 varNum usage is binary-long signed.
       01 dim usage is computational-2.

       procedure division using by value varNum, by reference dim.

      * varNum is 1-based
       add 1 to varNum

       if varNum less than 1 or greater than nv then
         call "fatalN" using "varNum out of range ", by value varNum
       end-if

       if vts-type(varNum) not equal to vtTypeString then
         call "fatal" using "dimSVar expected a string variable "
       end-if

       move dim to idim
       if idim less than 1 or dim greater than 256 then
         call "fatalN" using "dim out of range", by value dim
       end-if

       move idim to vts-dim(varNum)
       if vts-sval(varNum) not equal null then
         free vts-sval(varNum)
       end-if

       add 1 to dim
       allocate (dim) characters returning vts-sval(varNum)
       goback.

       end program dimSVar.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      **
      ** Memory functions
      **

      ******************************************************************
      **
      ** doPoke
      **

       identification division.

       program-id. doPoke.

       environment division.

       data division.

       working-storage section.

       01 iaddr usage is binary-long signed.
       01 ival usage is binary-long signed.

       linkage section.

       01 addr usage is computational-2.
       01 val usage is computational-2.

       procedure division using by reference addr, by reference val.
      
       move addr to iaddr
       move val to ival

       if iaddr equal 752 then
         goback
       end-if

       if iaddr equal 764 then
         move ival to lastKBChar
         goback
       end-if

       if iaddr equal 82 then
         goback
       end-if

       goback.
       end program doPoke.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      **
      ** Flow control
      **

      ******************************************************************
      **
      ** doIfLine
      **

       identification division.

       program-id. doIfLine.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".

       01 lineNum usage is binary-long signed.

       linkage section.

       01 exp1 usage is computational-2.
       01 exp2 usage is computational-2.

       procedure division using by reference exp1, by reference exp2.
      
       if exp1 not equal to 0.0 then

      * consume tokens until end
           move exp2 to lineNum

           if lineNum less than 1 or lineNum greater than H"10000" then
             call "fatalN" using "line number out of range",
                                 by value lineNum
           end-if

           if lineNumberRow (lineNum) equal NULL then
             call "fatalN" using "no such line number",
                                 by value lineNum
           end-if

           move lineNumberRow(lineNum) to lexLinePtr
           move startOfLine to lexState

      * Forces YYACCEPT, causing parser restart
           move 1 to return-code
           goback
       end-if

       move 0 to return-code

       goback.

       end program doIfLine.

      ******************************************************************
      **
      ** doIfThen
      **

       identification division.

       program-id. doIfThen.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".

       01 tok usage is binary-long signed.
       01 frozen usage is binary-long signed value 0.
       01 hell usage is binary-long signed value 1.

       linkage section.

       01 exp usage is computational-2.

       procedure division using by reference exp.
      
       if exp equal to 0.0 then
      * consume tokens until end
           perform until hell equal frozen
               call "yylex" returning tok
               if tok equal tokEOF then
                 call "fatal" using "ran off end looking for END"
               end-if            
               if tok equal tokEnd then
                 exit perform
               end-if
           end-perform
      * consume the end statement
           perform until hell equal frozen
               call "yylex" returning tok
               if tok equal tokEOF then
                 call "fatal" using "ran off end looking for END"
               end-if            
               if tok equal tokEOT then
                 exit perform
               end-if
           end-perform
      * Forces YYACCEPT, causing parser restart
           move 1 to return-code
           goback
       end-if
       move 0 to return-code

       goback.

       end program doIfThen.

      ******************************************************************
      **
      ** doGosub
      **

       identification division.

       program-id. doGosub.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".

       01 lineNum usage is binary-long signed.

       linkage section.

       01 exp usage is computational-2.

       procedure division using by reference exp.
      
       if rsp greater than 16 then
         call "fatal" using "return stack overflow"
       end-if

       move exp to lineNum

       if lineNum less than 1 or lineNum greater than H"10000" then
         call "fatalN" using "line number out of range",
                             by value lineNum
       end-if

       if lineNumberRow (lineNum) equal NULL then
         call "fatalN" using "no such line number",
                             by value lineNum
       end-if

       move tokenEnd to rs-tokenEnd(rsp)
       move lineNext to rs-lineNext(rsp)
       move lineStart to rs-lineStart(rsp)
       add 1 to rsp

       move lineNumberRow(lineNum) to lexLinePtr
       move startOfLine to lexState
       goback.

       end program doGosub.

      ******************************************************************
      **
      ** doReturn
      **

       identification division.

       program-id. doReturn.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".

       procedure division.
      
       if rsp less than or equal to 1 then
         call "fatal" using "return stack underflow"
       end-if

       subtract 1 from rsp
       move inOperands to lexState
       move rs-tokenEnd(rsp) to tokenEnd
       move tokenEnd to lexLinePtr
       move rs-lineNext(rsp) to lineNext
       move rs-lineStart(rsp) to lineStart

       goback.

       end program doReturn.

      ******************************************************************
      **
      ** doPop
      **

       identification division.

       program-id. doPop.

       environment division.

       data division.

       working-storage section.

       procedure division.
      
       if rsp less than or equal to 1 then
         call "fatal" using "return stack underflow"
       end-if
       subtract 1 from rsp

       goback.

       end program doPop.

      ******************************************************************
      **
      ** doGoto
      **

       identification division.

       program-id. doGoto.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".

       01 lineNum usage is binary-long signed.

       linkage section.

       01 exp usage is computational-2.

       procedure division using by reference exp.

       move exp to lineNum

       if lineNum less than 1 or lineNum greater than H"10000" then
         call "fatalN" using "line number out of range",
                             by value lineNum
       end-if

       if lineNumberRow (lineNum) equal NULL then
         call "fatalN" using "no such line number",
                             by value lineNum
       end-if

       move lineNumberRow(lineNum) to lexLinePtr
       move startOfLine to lexState.

       goback.

       end program doGoto.

      ******************************************************************
      **
      ** doTrap
      **

       identification division.

       program-id. doTrap.

       environment division.

       data division.

       working-storage section.

       01 lineNum usage is binary-long signed.

       linkage section.

       01 exp usage is computational-2.

       procedure division using by reference exp.

       move exp to lineNum
       if lineNum equal zero then
         move zero to trapLine 
         goback
       end-if

       if lineNum less than 1 or lineNum greater than H"10000" then
         call "fatalN" using "line number out of range",
                              by value lineNum
       end-if

       if lineNumberRow (lineNum) equal NULL then
         call "fatalN" using "no such line number",
                              by value lineNum
       end-if

       move lineNum to trapLine.

       goback.

       end program doTrap.

      ******************************************************************
      **
      ** fireTrap
      **

       identification division.

       program-id. fireTrap.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".

       01 lineNum usage is binary-long signed.

       procedure division.

       if trapLine not equal zero then
           if trapLine less than 1 or
             trapLine greater than H"10000" then
               call "fatalN" using "line number out of range",
                                   by value trapLine
           end-if

           if lineNumberRow (trapLine) equal NULL then
               call "fatalN" using "no such line number",
                                   by value trapLine
           end-if

           move lineNumberRow(trapLine) to lexLinePtr
           move startOfLine to lexState
           call "dolongjmp" using trapJmp, 1 *> 1 should be the Atari errno
       end-if

       goback.

       end program fireTrap.

      ******************************************************************
      **
      ** doEnd
      **

       identification division.

       program-id. doEnd.

       environment division.

       data division.

       working-storage section.

       procedure division.
      
       call "erase"
       call "refresh"
       call "endwin"
       move 1 to return-code.
       stop run.

       end program doEnd.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      **
      ** FOR/NEXT
      **

      ******************************************************************
      **
      ** doFor
      **

       identification division.

       program-id. doFor.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".

       01 varNumActual usage is binary-long signed.

       linkage section.

       01 varNum usage is binary-long signed.
       01 vinitial usage is computational-2.
       01 vfinal usage is computational-2.
       01 vstep usage is computational-2.

       procedure division using by value varNum, by reference vinitial,
                                by reference vfinal, by reference vstep.
      
       if varNum less than zero or greater than or equal to nv then
         call "fatalN" using "varNum out of range",
                             by value varNum
       end-if

       move varNum to varNumActual
      * varNum is 1-based
       add 1 to varNum

       if vts-type(varNum) not equal to vtTypeScalar then
         call "fatal" using "doFor expected a scalar variable"
       end-if

       move vinitial to vts-val(varNum)
       move varNumActual to fd-indexVarNum
       move vfinal to fd-limit
       move vstep to fd-step
       move tokenEnd to fd-tokenEnd
       move lineNext to fd-lineNext
       move lineStart to fd-lineStart

       goback.

       end program doFor.

      ******************************************************************
      **
      ** doNext
      **

       identification division.

       program-id. doNext.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".

       01 done usage is binary-long signed.

       linkage section.

       01 varNum usage is binary-long signed.

       procedure division using by value varNum.
      
       if varNum not equal fd-indexVarNum then
         call "fatal" using "next wrong variable"
       end-if
      * varNum is 1-based
       add 1 to varNum 
       add fd-step to vts-val(varNum)
       if fd-step less than zero then
         if vts-val(varNum) less than fd-limit then
           move 1 to done
         else
           move 0 to done
         end-if
       else
         if vts-val(varNum) greater than fd-limit then
           move 1 to done
         else
           move 0 to done
       end-if
       if done equal 0 then
         move inOperands to lexState
         move fd-tokenEnd to tokenEnd
         move fd-tokenEnd to lineNext
         move fd-lineStart to lineStart
      *  Forces YYACCEPT, causing parser restart
         move 1 to return-code
         goback
       end-if
       move 0 to return-code
       goback.

       end program doNext.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      **
      ** Let
      **

      ******************************************************************
      **
      ** doLet
      **

       identification division.

       program-id. doLet.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".

       linkage section.

       01 varNum usage is binary-long signed.
       01 val usage is computational-2.

       procedure division using by value  varNum, by reference val.
      
       if varNum less than zero or greater than or equal to nv then
         call "fatalN" using "varNum out of range",
                             by value varNum
       end-if

      * varNum is 1-based
       add 1 to varNum

       if vts-type(varNum) not equal to vtTypeScalar then
         call "fatal" using "doLet expected a scalar variable"
       end-if

       move val to vts-val(varNum)

       goback.

       end program doLet.

      ******************************************************************
      **
      ** doSLet
      **

       identification division.

       program-id. doSLet.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".

       01 i usage is binary-char unsigned.
       01 l usage is binary-char unsigned.
     
       01 destPtr usage is pointer.
       01 srcPtr usage is pointer.

       copy "casts.cpy".

       linkage section.

       01 varNum usage is binary-long signed.
       01 val usage is pointer.

       procedure division using by value varNum, by value val.

       if varNum less than zero or greater than or equal to nv then
         call "fatalN" using "varNum out of range",
                             by value varNum
       end-if

      * varNum is 1-based
       add 1 to varNum

       if vts-type(varNum) not equal to vtTypeString then
         call "fatal" using "doSLet expected a string variable"
       end-if

       move val to srcPtr
       set address of byteCast to srcPtr
       move byteCasted to l

       if l greater than vts-dim(varNum) then
         move vts-dim(varNum) to l
       end-if

       move vts-sval(varNum) to destPtr

       perform varying i from 1 by 1 until i greater than l + 1
         set address of byteCast to srcPtr
         set address of svalCast to destPtr
         move byteCasted to svalCasted
         set destPtr up by 1
         set srcPtr up by 1
       end-perform

       move vts-sval(varNum) to destPtr
       set address of svalCast to destPtr
       move l to svalCasted
       goback.

       end program doSLet.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      **
      ** Parser/interpreter
      **

      ******************************************************************
      **
      ** yylex
      **

       identification division.

       program-id. yylex.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".
       copy "casts.cpy".

       01 lineNumber usage binary-short unsigned.
       01 lineLength usage binary-char unsigned.
       01 tokenLen usage binary-char unsigned.
       01 token usage binary-char unsigned.
       01 len usage binary-char unsigned.
       01 op usage binary-char unsigned.
       01 iop usage binary-long unsigned.
       01 val usage computational-2.

       linkage section.

       procedure division.

       if lexState equal atEnd then
           move 0 to return-code
           goback
       end-if

       if lexState equal atEnd
           move tokEOF to return-code
           goback
       end-if
      
       if lexLinePtr greater than programEnd then
           move atEnd to lexState
           move tokEOT to return-code
           goback
       end-if
      
       if lexState equal startOfLine then
           move lexLinePtr to lineStart
           set address of wordCast to lexLinePtr
           move wordCasted to lineNumber
           set lexLinePtr up by length of wordCast
           move lineNumber to currentLineNumber
           set address of byteCast to lexLinePtr
           move byteCasted to lineLength
           set lexLinePtr up by length of byteCast
           move lineStart to lineNext
           set lineNext up by lineLength
           set address of intCast to address of yylval
           move lineNumber to intCasted
           move inToken to lexState
       end-if

       if lexState equal inToken then
           if lexLinePtr greater than lineNext then
               move startOfLine to lexState
               move tokEOT to return-code
           goback
           end-if
           set address of byteCast to lexLinePtr
           move byteCasted to tokenLen
           set lexLinePtr up by length of byteCast
           set address of byteCast to lexLinePtr
           move byteCasted to token
           set lexLinePtr up by length of byteCast
           move lineStart to tokenEnd
           set tokenEnd up by tokenLen
           move inOperands to lexState
      *      Special cases
      *      Discard every thing after REM
           if token equal tokREM then
               move tokenEnd to lexLinePtr
           end-if
           if token equal tokREM then
               move tokREM2 to token
           end-if
           move token to return-code
           goback
       end-if
       if lexState equal inOperands then
           if lexLinePtr greater than or equal lineNext then
               move startOfLine to lexState
               move tokEOT to return-code
               goback
           end-if
           if lexLinePtr greater than or equal tokenEnd then
               move inToken to lexState
               move tokEOT to return-code
               goback
           end-if
           set address of byteCast to lexLinePtr
           move byteCasted to op
           set lexLinePtr up by length of byteCast
      *       Special cases
           if op equal opNCONST then
               call "parseNCONST" using by reference lexLinePtr, 
                                        by reference val
           *>set address of doubleCast to address of yylval
           *>move val to doubleCasted
               call "setlvalD" using by reference val
           end-if
           if op equal opSCONST then
           *>set address of ptrCast to address of yylval
           *>move lexLinePtr to ptrCasted
               call "setlvalS" using by value lexLinePtr
               set address of byteCast to lexLinePtr
               move byteCasted to len
               set lexLinePtr up by length of byteCast
               set lexLinePtr up by len
           end-if
           if op greater than 127 then
               subtract 128 from op
           *> set address of intCast to address of yylval
           *> move op to intCasted
               call "setlvalI" using by value op
               compute iop equal op + 1
               if vts-type(iop) equal 0 then
                   move opVAR to op
               else
                   move opSVAR to op
               end-if
           end-if
           move op to iop
           add H"100" to iop
           move iop to return-code
           goback
       end-if

       call "fatal" using "yylex fell through"
       move tokEOF to return-code

       goback.

       end program yylex.

      ******************************************************************
      **
      ** yyerror
      **

       identification division.

       program-id. yyerror.

       environment division.

       data division.

       linkage section.

       01 msg usage is pointer.

       procedure division using msg.

       call "fatal" using msg
       stop run.

       end program yyerror.

      ******************************************************************
      **
      ** interpret
      **

       identification division.

       program-id. interpret.

       environment division.

       data division.

       working-storage section.

       copy "constants.cpy".

       01 frozen usage is binary-long signed value 0.
       01 hell usage is binary-long signed value 1.

       procedure division.

       move zero to gInit
       move programFile to lexLinePtr
       set lexLinePtr up by stmtab

       move startOfLine to lexState              

       call "setjmp" using trapJmp
       perform until hell equal frozen
           call "yyparse"
       end-perform.

       end program interpret.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      **
      ** omgwtf2_main
      **

       identification division.

       program-id. omgwtf2_main.

       environment division.

       data division.

       working-storage section.

       01 foo pic x(5) value x"04" & "abcd".

       procedure division.

      *call "setYYDEBUG"

       move 12345 to iseed
       move 255 to lastKBChar

       call "readAtrFile"
       call "readBasicFile"
       call "analyzeBasicFile"
       call "interpret"
       stop run.

       end program omgwtf2_main.

       end program omgwtf2.
