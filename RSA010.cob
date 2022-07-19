000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.  RSA010.                                             RSA010
000300 ENVIRONMENT DIVISION.                                            RSA010
000400 CONFIGURATION SECTION.                                           RSA010
000500 SOURCE-COMPUTER. PC-MICROFOCUS.                                  RSA010
000600 OBJECT-COMPUTER. PC-MICROFOCUS.                                  RSA010
000700 SPECIAL-NAMES.                                                   RSA010
000800      C01 IS SAUTP                                                RSA010
000900      CSP IS SAUT0                                                RSA010
001000      DECIMAL-POINT IS COMMA.                                     RSA010
001100 INPUT-OUTPUT SECTION.                                            RSA010
001200 FILE-CONTROL.                                                    RSA010
001300      SELECT     EW-FICHIER    ASSIGN      AS-EW                  RSA010
001400      FILE STATUS IS                   1-EW00-STATUS              RSA010
001500                                       VSAM-STATUS.               RSA010
001600      SELECT YX-FICHIER      ASSIGN AS-SOYX                       D01YX
001700             ORGANIZATION    LINE SEQUENTIAL                      D01YX
001800             FILE STATUS     1-YX00-STATUS.                       D01YX
001900 DATA DIVISION.                                                   RSA010
002000 FILE SECTION.                                                    RSA010
002100 FD                 EW-FICHIER                                    RSA010
002200      BLOCK              00000 RECORDS.                           RSA010
002300 01                 EW00.                                         RSA010
002400      10       FILLER         PICTURE  X(133).                    RSA010
002500 FD                 YX-FICHIER                                    RSA010
002600      BLOCK              00000 RECORDS.                           RSA010
002700 01                 YX00.                                         RSA010
002800      10            YX00-ZX80   PICTURE  X(80).                   RSA010
002900 WORKING-STORAGE SECTION.                                         RSA010
003000          EXEC SQL INCLUDE SQLCA         END-EXEC.                7AAAAA
003100          EXEC SQL BEGIN DECLARE SECTION END-EXEC.                7RS999
003200 01                 RS01.                                         RSA010
003300      10            RS01-NONER  PICTURE  XX.                      RSA010
003400      10            RS01-NORERD PICTURE  S9(6)                    RSA010
003500                    COMPUTATIONAL-3.                              RSA010
003600      10            RS01-ZREPR  PICTURE  X(8).                    RSA010
003700      10            RS01-NOELL  PICTURE  S999                     RSA010
003800                    COMPUTATIONAL-3.                              RSA010
003900      10            RS01-DIELL  PICTURE  X(8).                    RSA010
004000 01                 RS02.                                         RSA010
004100      10            RS02-NORER  PICTURE  X(6).                    RSA010
004200      10            RS02-CDRES  PICTURE  X.                       RSA010
004300      10            RS02-LNRENR PICTURE  X(20).                   RSA010
004400      10            RS02-LNREPR PICTURE  X(12).                   RSA010
004500      10            RS02-DTRENR PICTURE  X(8).                    RSA010
004600      10            RS02-LNRENF PICTURE  X(20).                   RSA010
004700      10            RS02-CDRECR PICTURE  X(3).                    RSA010
004800      10            RS02-CDREXR PICTURE  X.                       RSA010
004900      10            RS02-CTREF  PICTURE  XX.                      RSA010
005000      10            RS02-CEREF  PICTURE  X.                       RSA010
005100      10            RS02-DBREF  PICTURE  X(8).                    RSA010
005200      10            RS02-DMREN  PICTURE  X(8).                    RSA010
005300 01                 RS03.                                         RSA010
005400      10            RS03-NORER  PICTURE  X(6).                    RSA010
005500      10            RS03-XCSEQ  PICTURE  S9(9)                    RSA010
005600                    COMPUTATIONAL-3.                              RSA010
005700      10            RS03-LRRE1R PICTURE  X(36).                   RSA010
005800      10            RS03-LRRE2R PICTURE  X(32).                   RSA010
005900      10            RS03-LVRER  PICTURE  X(31).                   RSA010
006000      10            RS03-CPRENR PICTURE  X(5).                    RSA010
006100      10            RS03-CDREPR PICTURE  XXX.                     RSA010
006200      10            RS03-DMREN  PICTURE  X(8).                    RSA010
006300 01                 RS04.                                         RSA010
006400      10            RS04-NOREN.                                   RSA010
006500      11            RS04-CNREN  PICTURE  XX.                      RSA010
006600      11            RS04-NORER  PICTURE  X(6).                    RSA010
006700      11            RS04-IDRPL  PICTURE  X(5).                    RSA010
006800      11            RS04-IDRPLG                                   RSA010
006900                    REDEFINES            RS04-IDRPL.              RSA010
007000      12            RS04-NIREG  PICTURE  XX.                      RSA010
007100      12            RS04-CDRETP PICTURE  X.                       RSA010
007200      12            RS04-NIPOL  PICTURE  XX.                      RSA010
007300      10            RS04-CDRES  PICTURE  X.                       RSA010
007400      10            RS04-NOCRA  PICTURE  XX.                      RSA010
007500      10            RS04-NOTIE  PICTURE  X(6).                    RSA010
007600      10            RS04-CDPAD  PICTURE  X.                       RSA010
007700      10            RS04-CDREA  PICTURE  X.                       RSA010
007800      10            RS04-DMREA  PICTURE  X(8).                    RSA010
007900      10            RS04-DFREN  PICTURE  X(8).                    RSA010
008000      10            RS04-NOSIN  PICTURE  X(10).                   RSA010
008100      10            RS04-DTREAC PICTURE  X(8).                    RSA010
008200      10            RS04-DTREC  PICTURE  X(8).                    RSA010
008300      10            RS04-CTRETC PICTURE  X(8).                    RSA010
008400      10            RS04-CDPAF  PICTURE  X.                       RSA010
008500      10            RS04-CTPAT  PICTURE  X.                       RSA010
008600      10            RS04-CQRER  PICTURE  X.                       RSA010
008700      10            RS04-CERERC PICTURE  X.                       RSA010
008800      10            RS04-NOREG  PICTURE  XXX.                     RSA010
008900      10            RS04-CDREF  PICTURE  X.                       RSA010
009000      10            RS04-CDREG  PICTURE  X.                       RSA010
009100      10            RS04-LIRE30 PICTURE  X(30).                   RSA010
009200      10            RS04-DMRET  PICTURE  X(8).                    RSA010
009300      10            RS04-DDREN  PICTURE  X(8).                    RSA010
009400      10            RS04-MTREA  PICTURE  S9(09)V99                RSA010
009500                    COMPUTATIONAL-3.                              RSA010
009600      10            RS04-MORER  PICTURE  S9(09)V99                RSA010
009700                    COMPUTATIONAL-3.                              RSA010
009800      10            RS04-LIRER  PICTURE  X(30).                   RSA010
009900      10            RS04-MTPARV PICTURE  S9(09)V99                RSA010
010000                    COMPUTATIONAL-3.                              RSA010
010100      10            RS04-CDRELK PICTURE  X(3).                    RSA010
010200      10            RS04-MKREND PICTURE  S9(09)V99                RSA010
010300                    COMPUTATIONAL-3.                              RSA010
010400      10            RS04-LIREK  PICTURE  X(30).                   RSA010
010500      10            RS04-CSREE  PICTURE  X.                       RSA010
010600      10            RS04-MTRER  PICTURE  S9(09)V99                RSA010
010700                    COMPUTATIONAL-3.                              RSA010
010800      10            RS04-DTPAID PICTURE  X(8).                    RSA010
010900      10            RS04-DHPAID PICTURE  X(8).                    RSA010
011000      10            RS04-DTPARP PICTURE  X(8).                    RSA010
011100      10            RS04-LIPAOD PICTURE  X(31).                   RSA010
011200      10            RS04-MCPAA  PICTURE  S9(13)V99                RSA010
011300                    COMPUTATIONAL-3.                              RSA010
011400      10            RS04-MCPAP  PICTURE  S9(13)V99                RSA010
011500                    COMPUTATIONAL-3.                              RSA010
011600      10            RS04-MKREP  PICTURE  S9(09)V99                RSA010
011700                    COMPUTATIONAL-3.                              RSA010
011800      10            RS04-CDRER  PICTURE  X.                       RSA010
011900      10            RS04-NOPAMD PICTURE  S99                      RSA010
012000                    COMPUTATIONAL-3.                              RSA010
012100      10            RS04-NOPAID PICTURE  S999                     RSA010
012200                    COMPUTATIONAL-3.                              RSA010
012300      10            RS04-MKREC  PICTURE  S9(09)V99                RSA010
012400                    COMPUTATIONAL-3.                              RSA010
012500      10            RS04-CEREK  PICTURE  X.                       RSA010
012600      10            RS04-DTPAIS PICTURE  X(8).                    RSA010
012700      10            RS04-NODEP  PICTURE  XX.                      RSA010
012800      10            RS04-CDREV  PICTURE  X.                       RSA010
012900      10            RS04-LIPA2  PICTURE  X(31).                   RSA010
013000      10            RS04-CDREK  PICTURE  XXX.                     RSA010
013100      10            RS04-DTREC1 PICTURE  X(8).                    RSA010
013200      10            RS04-CTREU  PICTURE  X.                       RSA010
013300      10            RS04-DFREL  PICTURE  X(8).                    RSA010
013400      10            RS04-LIR30A PICTURE  X(30).                   RSA010
013500      10            RS04-CCGT1  PICTURE  X.                       RSA010
013600      10            RS04-CDGT   PICTURE  X(5).                    RSA010
013700      10            RS04-CRCGJ  PICTURE  X.                       RSA010
013800      10            RS04-CNCOB  PICTURE  XXX.                     RSA010
013900      10            RS04-CRCGE  PICTURE  X.                       RSA010
014000      10            RS04-CCREN  PICTURE  XXX.                     RSA010
014100      10            RS04-CRCGD  PICTURE  X.                       RSA010
014200      10            RS04-CCCON  PICTURE  XX.                      RSA010
014300      10            RS04-CDSIN  PICTURE  X.                       RSA010
014400      10            RS04-CDEXCS PICTURE  X.                       RSA010
014500      10            RS04-NOSINL PICTURE  X(10).                   RSA010
014600      10            RS04-NOSINS PICTURE  X(10).                   RSA010
014700      10            RS04-CCFRN  PICTURE  X.                       RSA010
014800      10            RS04-CCFRA  PICTURE  X.                       RSA010
014900      10            RS04-MTREB  PICTURE  S9(09)V99                RSA010
015000                    COMPUTATIONAL-3.                              RSA010
015100      10            RS04-LIFOE  PICTURE  X(10).                   RSA010
015200      10            RS04-NIREGS PICTURE  XX.                      RSA010
015300      10            RS04-CDRES3 PICTURE  X.                       RSA010
015400      10            RS04-NOCRA3 PICTURE  XX.                      RSA010
015500      10            RS04-CDCAT  PICTURE  X.                       RSA010
015600      10            RS04-CDRVA  PICTURE  X.                       RSA010
015700      10            RS04-CDFISC PICTURE  X.                       RSA010
015800      10            RS04-CDRESO PICTURE  X.                       RSA010
015900      10            RS04-CCRER  PICTURE  XXX.                     RSA010
016000      10            RS04-CTRTCA PICTURE  X(8).                    RSA010
016100      10            RS04-CDTV   PICTURE  S999                     RSA010
016200                    COMPUTATIONAL-3.                              RSA010
016300      10            RS04-REFAV  PICTURE  X(14).                   RSA010
016400      10            RS04-CTRTPM PICTURE  X(8).                    RSA010
016500      10            RS04-INDRVA PICTURE  XX.                      RSA010
016600 01                 RS42.                                         RSA010
016700      10            RS42-ZIN42.                                   RSA010
016800      11            RS42-ZCLET  PICTURE  X(10).                   RSA010
016900      11            RS42-FILLER                                   RSA010
017000                    REDEFINES            RS42-ZCLET.              RSA010
017100      12            RS42-CDRES  PICTURE  X.                       RSA010
017200      12            RS42-CDRETS PICTURE  X.                       RSA010
017300      12            RS42-NORETS PICTURE  X(8).                    RSA010
017400      11            RS42-NIRET  PICTURE  X(14).                   RSA010
017500      10            RS42-ZTA100 PICTURE  X(100).                  RSA010
017600          EXEC SQL END   DECLARE SECTION END-EXEC.                7RTAAA
017700 77               W-WA00-NONER    PIC 99.                         7WA010
017800 01               W-WW00-ZRECOR                                   7WA100
017900                  PICTURE X(4).                                   7WA100
018000 01               W-WA00-ABRS27.                                  7WA820
018100    05            W-WA00-CDMON                                    7WA840
018200                  PICTURE XXX.                                    7WA840
018300 01               W-WW00-NORER                                    7WW100
018400                  PICTURE X(6).                                   7WW100
018500 01               W-WW00-LIDEV                                    7WW998
018600                  PICTURE X(6).                                   7WW998
018700 77                 XA00-XRC      VALUE ZERO                      7XA015
018800                  PICTURE 9(4).                                   7XA015
018900 77                 XA00-8TMES    VALUE ZERO                      7XA055
019000                  PICTURE X(01).                                  7XA055
019100 77                 XAIN-XDATRT                                   7XA110
019200                  PICTURE X(8).                                   7XA110
019300 77                 XAIN-XHETRT                                   7XA120
019400                  PICTURE X(6).                                   7XA120
019500 77                 XAED-XDATRT                                   7XA160
019600                  PICTURE X(10).                                  7XA160
019700 77                 XAED-XHETRT                                   7XA170
019800                  PICTURE X(8).                                   7XA170
019900 77                 XA30-ENVNAM   PIC X(080).                     7XA310
020000 77                 XA30-ENVVAL   PIC X(080).                     7XA320
020100 01                 XA60.                                         7XA610
020200   05               XA60-L1.                                      7XA620
020300     10             FILLER PIC X  VALUE 'F'.                      7XA625
020400     10             XA60-XCDFSF   VALUE SPACE                     7XA630
020500                  PICTURE X(4).                                   7XA630
020600     10             FILLER PIC X  VALUE ':'.                      7XA635
020700     10             XA60-XLISUI   VALUE SPACE                     7XA640
020800                  PICTURE X(30).                                  7XA640
020900   05               XA60-ZX67A    VALUE SPACE                     7XA650
021000                  PICTURE X(67).                                  7XA650
021100   05               XA60-ZX67B    VALUE SPACE                     7XA660
021200                  PICTURE X(67).                                  7XA660
021300   05               XA60-ZX67C    VALUE SPACE                     7XA670
021400                  PICTURE X(67).                                  7XA670
021500 01                 XA80.                                         7XA810
021600   05               FILLER        PIC X(012) VALUE 'ERREUR I-O S'.7XA815
021700   05               FILLER        PIC X(011) VALUE 'UR FICHIER '. 7XA816
021800   05               XA80-XCOSD                                    7XA820
021900                  PICTURE XX.                                     7XA820
022000   05               FILLER        PIC X(010) VALUE ', STATUS: '.  7XA825
022100   05               XA80-STATUS.                                  7XA830
022200    07              XA80-STATUS1  PIC X.                          7XA831
022300    07              XA80-STATUS2  PIC 99 COMP-X.                  7XA832
022400    07              FILLER        PIC X(003) VALUE SPACE.         7XA833
022500   05               FILLER        PIC X(012) VALUE ', ENREG. TRA'.7XA835
022600   05               FILLER        PIC X(006) VALUE 'ITES: '.      7XA836
022700   05               XA80-XQNENR                                   7XA840
022800                  PICTURE Z(8)9.                                  7XA840
022900 01                 XA81-STATUS.                                  7XA900
023000    07              XA81-STATUS1  PIC X.                          7XA905
023100    07              FILLER        PIC X      VALUE '/'.           7XA910
023200    07              XA81-STATUS2  PIC 999.                        7XA915
023300          EXEC SQL BEGIN DECLARE SECTION END-EXEC.                7XBBB0
023400 01               USERID          PIC X(20).                      7XBCC2
023500          EXEC SQL END   DECLARE SECTION END-EXEC.                7XBZZ0
023600 77                 RS01-XROWID                                   7XB001
023700                  PICTURE X(18).                                  7XB001
023800 77                 RS02-XROWID                                   7XB002
023900                  PICTURE X(18).                                  7XB002
024000 77                 RS03-XROWID                                   7XB003
024100                  PICTURE X(18).                                  7XB003
024200 77                 RS04-XROWID                                   7XB004
024300                  PICTURE X(18).                                  7XB004
024400 77                 RS05-XROWID                                   7XB005
024500                  PICTURE X(18).                                  7XB005
024600 77                 RS06-XROWID                                   7XB006
024700                  PICTURE X(18).                                  7XB006
024800 77                 RS07-XROWID                                   7XB007
024900                  PICTURE X(18).                                  7XB007
025000 77                 RS08-XROWID                                   7XB008
025100                  PICTURE X(18).                                  7XB008
025200 77                 RS09-XROWID                                   7XB009
025300                  PICTURE X(18).                                  7XB009
025400 77                 RS10-XROWID                                   7XB010
025500                  PICTURE X(18).                                  7XB010
025600 77                 RS11-XROWID                                   7XB011
025700                  PICTURE X(18).                                  7XB011
025800 77                 RS12-XROWID                                   7XB012
025900                  PICTURE X(18).                                  7XB012
026000 77                 RS13-XROWID                                   7XB013
026100                  PICTURE X(18).                                  7XB013
026200 77                 RS14-XROWID                                   7XB014
026300                  PICTURE X(18).                                  7XB014
026400 77                 RS15-XROWID                                   7XB015
026500                  PICTURE X(18).                                  7XB015
026600 77                 RS16-XROWID                                   7XB016
026700                  PICTURE X(18).                                  7XB016
026800 77                 RS17-XROWID                                   7XB017
026900                  PICTURE X(18).                                  7XB017
027000 77                 RS18-XROWID                                   7XB018
027100                  PICTURE X(18).                                  7XB018
027200 77                 RS19-XROWID                                   7XB019
027300                  PICTURE X(18).                                  7XB019
027400 77                 RS20-XROWID                                   7XB020
027500                  PICTURE X(18).                                  7XB020
027600 77                 RS22-XROWID                                   7XB022
027700                  PICTURE X(18).                                  7XB022
027800 77                 RS23-XROWID                                   7XB023
027900                  PICTURE X(18).                                  7XB023
028000 77                 RS24-XROWID                                   7XB024
028100                  PICTURE X(18).                                  7XB024
028200 77                 RS26-XROWID                                   7XB026
028300                  PICTURE X(18).                                  7XB026
028400 77                 RS27-XROWID                                   7XB027
028500                  PICTURE X(18).                                  7XB027
028600 77                 RS28-XROWID                                   7XB028
028700                  PICTURE X(18).                                  7XB028
028800 77                 RS29-XROWID                                   7XB029
028900                  PICTURE X(18).                                  7XB029
029000 77                 RS30-XROWID                                   7XB030
029100                  PICTURE X(18).                                  7XB030
029200 77                 RS31-XROWID                                   7XB031
029300                  PICTURE X(18).                                  7XB031
029400 77                 RS32-XROWID                                   7XB032
029500                  PICTURE X(18).                                  7XB032
029600 77                 RS41-XROWID                                   7XB041
029700                  PICTURE X(18).                                  7XB041
029800 77                 RS42-XROWID                                   7XB042
029900                  PICTURE X(18).                                  7XB042
030000 77                 RS61-XROWID                                   7XB061
030100                  PICTURE X(18).                                  7XB061
030200 77                 RS62-XROWID                                   7XB062
030300                  PICTURE X(18).                                  7XB062
030400 77                 RS63-XROWID                                   7XB063
030500                  PICTURE X(18).                                  7XB063
030600 77                 RS64-XROWID                                   7XB064
030700                  PICTURE X(18).                                  7XB064
030800 77                 RS69-XROWID                                   7XB069
030900                  PICTURE X(18).                                  7XB069
031000 77                 RS70-XROWID                                   7XB070
031100                  PICTURE X(18).                                  7XB070
031200 77                 RS71-XROWID                                   7XB071
031300                  PICTURE X(18).                                  7XB071
031400 01                 XO00.                                         7XO-A0
031500   05               XO00-XORACN   PIC X(001) VALUE ZERO.          7XO-B2
031600   05               XO00-XORATR   PIC X(001) VALUE ZERO.          7XO-C2
031700   05               XO00-XORACO   PIC X(001) VALUE ZERO.          7XO-D2
031800   05               XO00-XORAER.                                  7XO-EE
031900     10             FILLER        PIC X(012) VALUE 'ERREUR ORACL'.7XO-EG
032000     10             FILLER        PIC X(007) VALUE 'E ORA-0'.     7XO-EI
032100     10             XO00-XORARC   PIC 9(004).                     7XO-EK
032200     10             FILLER        PIC X(005) VALUE ' EN F'.       7XO-EM
032300     10             XO00-XCDFSF                                   7XO-EO
032400                  PICTURE X(4).                                   7XO-EO
032500     10             FILLER        PIC X(008) VALUE ' ACCES:'.     7XO-EQ
032600     10             XO00-XORATY   PIC X(008).                     7XO-ES
032700     10             FILLER        PIC X(008) VALUE ' TABLE:'.     7XO-EU
032800     10             XO00-XORATA                                   7XO-EW
032900                  PICTURE X(10).                                  7XO-EW
033000   05               XO00-XORAE2.                                  7XO-FE
033100     10             FILLER        PIC X(012) VALUE 'DEBUT SEGT: '.7XO-FG
033200     10             XO00-XORACL.                                  7XO-FI
033300       15           XO00-XORAC1   PIC X(001) OCCURS 055.          7XO-FK
033400                  EXEC SQL BEGIN DECLARE SECTION         END-EXEC.7XO-01
033500 77                 XO00-XORARB   PIC X(008) VALUE SPACE.         7XO-15
033600                  EXEC SQL END   DECLARE SECTION         END-EXEC.7XO-89
033700 77                 XOAA-RS01-CF  PIC X(001)     VALUE ZERO.      7XOAA0
033800 77                 XOAA-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOAA1
033900 77                 XOBB-RS42-CF  PIC X(001)     VALUE ZERO.      7XOBB0
034000 77                 XOBB-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOBB1
034100 77                 XOCC-RS02-CF  PIC X(001)     VALUE ZERO.      7XOCC0
034200 77                 XOCC-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOCC1
034300 77                 XODD-RS03-CF  PIC X(001)     VALUE ZERO.      7XODD0
034400 77                 XODD-COUNT    PIC S9(9) COMP VALUE ZERO.      7XODD1
034500 77                 XOEE-RS04-CF  PIC X(001)     VALUE ZERO.      7XOEE0
034600 77                 XOEE-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOEE1
034700          EXEC SQL BEGIN DECLARE SECTION END-EXEC.                7XPZZ5
034800 77                 XP00-XROWID                                   7XP001
034900                  PICTURE X(18).                                  7XP001
035000 77                 XP00-XCSEQ                                    7XP002
035100                  PICTURE S9(9)                                   7XP002
035200                    COMPUTATIONAL-3.                              7XP002
035300 77                 XP00-XCOUNT   PIC S9(9) COMP-3.               7XP003
035400          EXEC SQL END   DECLARE SECTION END-EXEC.                7XP999
035500 01                 CODE-ABORT    PIC X(2).                       7ZA999
035600 01   DEBUT-WSS.                                                  RSA010
035700      05   FILLER PICTURE X(7) VALUE 'WORKING'.                   RSA010
035800      05   IK     PICTURE X.                                      RSA010
035900 01  CONSTANTES-PAC.                                              RSA010
036000     05  FILLER  PICTURE X(60)   VALUE                            RSA010
036100               '6078HRSA06/09/04RSA010LEBLANC 15:25:42RSA010  UNIXRSA010
036200-    '06/09/2004'.                                                RSA010
036300 01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     RSA010
036400     05  NUGNA   PICTURE X(5).                                    RSA010
036500     05  APPLI   PICTURE X(3).                                    RSA010
036600     05  DATGN   PICTURE X(8).                                    RSA010
036700     05  PROGR   PICTURE X(6).                                    RSA010
036800     05  CODUTI  PICTURE X(8).                                    RSA010
036900     05  TIMGN   PICTURE X(8).                                    RSA010
037000     05  PROGE   PICTURE X(8).                                    RSA010
037100     05  COBASE  PICTURE X(4).                                    RSA010
037200     05  DATGNC  PICTURE X(10).                                   RSA010
037300 01  DATCE.                                                       RSA010
037400   05  CENTUR   PICTURE XX   VALUE '19'.                          RSA010
037500   05  DATOR.                                                     RSA010
037600     10  DATOA  PICTURE XX.                                       RSA010
037700     10  DATOM  PICTURE XX.                                       RSA010
037800     10  DATOJ  PICTURE XX.                                       RSA010
037900 01  DAT6.                                                        RSA010
038000      10 DAT61   PICTURE XX.                                      RSA010
038100      10 DAT62   PICTURE XX.                                      RSA010
038200      10 DAT63   PICTURE XX.                                      RSA010
038300 01  DAT8.                                                        RSA010
038400      10 DAT81   PICTURE XX.                                      RSA010
038500      10 DAT8S1  PICTURE X.                                       RSA010
038600      10 DAT82   PICTURE XX.                                      RSA010
038700      10 DAT8S2  PICTURE X.                                       RSA010
038800      10 DAT83   PICTURE XX.                                      RSA010
038900 01  DAT8E    REDEFINES    DAT8.                                  RSA010
039000      10 DAT81E  PICTURE X(4).                                    RSA010
039100      10 DAT82E  PICTURE XX.                                      RSA010
039200      10 DAT83E  PICTURE XX.                                      RSA010
039300 01  DAT6C.                                                       RSA010
039400      10  DAT61C PICTURE XX.                                      RSA010
039500      10  DAT62C PICTURE XX.                                      RSA010
039600      10  DAT63C.                                                 RSA010
039700       15 DAT63CC PICTURE XX.                                     RSA010
039800       15 DAT64C  PICTURE XX.                                     RSA010
039900 01  DAT8C.                                                       RSA010
040000      10  DAT81C  PICTURE XX.                                     RSA010
040100      10  DAT8S1C PICTURE X   VALUE '/'.                          RSA010
040200      10  DAT82C  PICTURE XX.                                     RSA010
040300      10  DAT8S2C PICTURE X   VALUE '/'.                          RSA010
040400      10  DAT83C.                                                 RSA010
040500       15 DAT83CC PICTURE XX.                                     RSA010
040600       15 DAT84C  PICTURE XX.                                     RSA010
040700 01  TIMCO.                                                       RSA010
040800   05  TIMCOH   PICTURE XX.                                       RSA010
040900   05  TIMCOM   PICTURE XX.                                       RSA010
041000   05  TIMCOS   PICTURE XX.                                       RSA010
041100   05  TIMOC.                                                     RSA010
041200    10 TIMCOC   PICTURE XX.                                       RSA010
041300 01  TIMDAY.                                                      RSA010
041400   05  TIMHOU   PICTURE XX.                                       RSA010
041500   05  TIMS1    PICTURE X  VALUE ':'.                             RSA010
041600   05  TIMMIN   PICTURE XX.                                       RSA010
041700   05  TIMS2    PICTURE X  VALUE ':'.                             RSA010
041800   05  TIMSEC   PICTURE XX.                                       RSA010
041900 01  DATSEP     PICTURE X VALUE '/'.                              RSA010
042000 01   VARIABLES-CONDITIONNELLES.                                  RSA010
042100      05                  FT      PICTURE X VALUE '0'.            RSA010
042200 01   INDICES  COMPUTATIONAL  SYNC.                               RSA010
042300      05          TALLI   PICTURE S9(4) VALUE  ZERO.              RSA010
042400      05        J00       PICTURE S9(4) VALUE +1.                 RSA010
042500      05        J01       PICTURE S9(4) VALUE +1.                 RSA010
042600      05           IXO00L PICTURE S9(4) VALUE  ZERO.              RSA010
042700      05           IXO00R PICTURE S9(4) VALUE  ZERO.              RSA010
042800      05           IXO00M PICTURE S9(4) VALUE +0055.              RSA010
042900      05           J99OVR PICTURE S9(4) VALUE  ZERO.              P000
043000 01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   RSA010
043100      05       5-RS00-CPTENR PICTURE S9(9) VALUE ZERO.            RSA010
043200      05       5-YX00-CPTENR PICTURE S9(9) VALUE ZERO.            RSA010
043300 01  ZONES-STATUS.                                                RSA010
043400      05   VSAM-STATUS.                                           RSA010
043500        10 VSAM-RCODE   PICTURE S9(4) COMP VALUE ZERO.            RSA010
043600        10 VSAM-FCODE   PICTURE S9(4) COMP VALUE ZERO.            RSA010
043700        10 VSAM-FBCODE  PICTURE S9(4) COMP VALUE ZERO.            RSA010
043800      05          1-EW00-STATUS PICTURE XX VALUE ZERO.            RSA010
043900      05          1-YX00-STATUS PICTURE XX VALUE ZERO.            RSA010
044000 01   CAT-TAB.                                                    RSA010
044100      05  FILLER         PICTURE X(100) VALUE SPACES.             RSA010
044200      05  FILLER         PICTURE X(100) VALUE SPACES.             RSA010
044300 01   CAT-TAB-R  REDEFINES CAT-TAB.                               RSA010
044400      05  CAT    PICTURE XX       OCCURS 0100.                    RSA010
044500 01   ST-TA.                                                      RSA010
044600      05  ST-ABS      PICTURE X  VALUE SPACE.                     RSA010
044700      05  ST-T.                                                   RSA010
044800      07  ST-TT    OCCURS 40.                                     RSA010
044900        10  ST-ST     PICTURE XX.                                 RSA010
045000        10  ST-LI     PICTURE 99.                                 RSA010
045100        10  ST-SA     PICTURE 99.                                 RSA010
045200 01  CONTENU-DES-CATEGORIES.                                      RSA010
045300     05          TS-2-BB.                                         RSA010
045400     10         ABS-2-BB PICTURE X VALUE  '*'.                    RSA010
045500      10  FILLER    PICTURE       X(24) VALUE                     RSA010
045600     '010101000201020302030401'.                                  RSA010
045700     05          TS-2-CC.                                         RSA010
045800     10         ABS-2-CC PICTURE X VALUE  ' '.                    RSA010
045900      10  FILLER    PICTURE       X(24) VALUE                     RSA010
046000     '000502000601040701050801'.                                  RSA010
046100     05          TS-2-DD.                                         RSA010
046200     10         ABS-2-DD PICTURE X VALUE  ' '.                    RSA010
046300      10  FILLER    PICTURE     X(42) VALUE                       RSA010
046400     '000902001001061101071202081301091401101501'.                RSA010
046500      10  FILLER    PICTURE     X(42) VALUE                       RSA010
046600     '111601121701131801141901152001162101172201'.                RSA010
046700      10  FILLER    PICTURE     X(42) VALUE                       RSA010
046800     '182301192401202501212601222701232801242901'.                RSA010
046900      10  FILLER    PICTURE     X(42) VALUE                       RSA010
047000     '253001263101273201283301293401303501313601'.                RSA010
047100      10  FILLER    PICTURE     X(42) VALUE                       RSA010
047200     '323701333801343901354001364101374201384301'.                RSA010
047300      10  FILLER    PICTURE       X(18) VALUE                     RSA010
047400     '394401404501414601'.                                        RSA010
047500 01   TAILLES-DES-CATEGORIES   COMPUTATIONAL-3.                   RSA010
047600      05          2-BB-NL PICTURE S99 VALUE +04.                  RSA010
047700      05          2-CC-NL PICTURE S99 VALUE +05.                  RSA010
047800      05          2-DD-NL PICTURE S99 VALUE +40.                  RSA010
047900 01  COMPTEURS-ET-VARIABLES-EDITION.                              RSA010
048000      05         COMPTEURS     COMPUTATIONAL-3.                   RSA010
048100      10       5-EW00-2CLM PICTURE S999 VALUE +60.                RSA010
048200      10       5-EW00-2CE  PICTURE S9(9) VALUE ZERO.              RSA010
048300      10       5-EW00-2CL  PICTURE S999 VALUE +60.                RSA010
048400      10       5-EW00-2CL1 PICTURE S999 VALUE +60.                RSA010
048500      10       5-EW00-2CP  PICTURE S9(7) VALUE ZERO.              RSA010
048600      05       5-EW00-2DP  PICTURE X    VALUE '1'.                RSA010
048700      05         ST-SLS.                                          RSA010
048800      10         STX      PICTURE XX.                             RSA010
048900      10         ST9  REDEFINES STX PICTURE 99.                   RSA010
049000      10         J02      PICTURE 99.                             RSA010
049100      10         SAUT     PICTURE 99.                             RSA010
049200      05         CATX     PICTURE XX  VALUE SPACE.                RSA010
049300 01  LIBELLES.                                                    RSA010
049400      05            2-LIB.                                        RSA010
049500          10        2-LIB01.                                      RSA010
049600              15  FILLER  PICTURE X(44) VALUE                     RSA010
049700     'GROUPAMA - RENTES                           '.              RSA010
049800              15  FILLER  PICTURE X(44) VALUE                     RSA010
049900     '        LISTE DES RENTES PAR RENTIER        '.              RSA010
050000              15  FILLER  PICTURE X(44) VALUE                     RSA010
050100     '          DATE : 99/99/99      PAGE :    1  '.              RSA010
050200          10        2-LIB02.                                      RSA010
050300              15  FILLER  PICTURE X(44) VALUE                     RSA010
050400     '                                            '.              RSA010
050500              15  FILLER  PICTURE X(44) VALUE                     RSA010
050600     '        ============================        '.              RSA010
050700              15  FILLER  PICTURE X(44) VALUE                     RSA010
050800     '                                            '.              RSA010
050900          10        2-LIB03.                                      RSA010
051000              15  FILLER  PICTURE X(44) VALUE                     RSA010
051100     '    RENTIER  NO  999999         CDRECR : XXX'.              RSA010
051200              15  FILLER  PICTURE X(44) VALUE                     RSA010
051300     '    CDREXR : X     DMRENR : 99999999    CERE'.              RSA010
051400              15  FILLER  PICTURE X(44) VALUE                     RSA010
051500     'F :     CTREF : XX   DBREF : 99999999       '.              RSA010
051600          10        2-LIB04.                                      RSA010
051700              15  FILLER  PICTURE X(44) VALUE                     RSA010
051800     '    ===================         INDEX : XXXX'.              RSA010
051900              15  FILLER  PICTURE X(44) VALUE                     RSA010
052000     'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.              RSA010
052100              15  FILLER  PICTURE X(44) VALUE                     RSA010
052200     'X,XXXXXXX,XXXXXXXXXXXXXXXXXXXXXXXXXXX       '.              RSA010
052300          10        2-LIB05.                                      RSA010
052400              15  FILLER  PICTURE X(44) VALUE                     RSA010
052500     '      * HISTORIQUE  (RS03)                  '.              RSA010
052600              15  FILLER  PICTURE X(44) VALUE                     RSA010
052700     '                                            '.              RSA010
052800              15  FILLER  PICTURE X(44) VALUE                     RSA010
052900     '                                            '.              RSA010
053000          10        2-LIB06.                                      RSA010
053100              15  FILLER  PICTURE X(44) VALUE                     RSA010
053200     '        ----------                          '.              RSA010
053300              15  FILLER  PICTURE X(44) VALUE                     RSA010
053400     '                                            '.              RSA010
053500              15  FILLER  PICTURE X(44) VALUE                     RSA010
053600     '                                            '.              RSA010
053700          10        2-LIB07.                                      RSA010
053800              15  FILLER  PICTURE X(44) VALUE                     RSA010
053900     '               +  ADRESSE :  XXXXXXXXXXXXXXX'.              RSA010
054000              15  FILLER  PICTURE X(44) VALUE                     RSA010
054100     'XXXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXX'.              RSA010
054200              15  FILLER  PICTURE X(44) VALUE                     RSA010
054300     'XXXXXXXXXX     DATE MAJ : 99999999          '.              RSA010
054400          10        2-LIB08.                                      RSA010
054500              15  FILLER  PICTURE X(44) VALUE                     RSA010
054600     '                             99999  XXXXXXXX'.              RSA010
054700              15  FILLER  PICTURE X(44) VALUE                     RSA010
054800     'XXXXXXXXXXXXXXXXXXXXXXX    PAYS : XXX       '.              RSA010
054900              15  FILLER  PICTURE X(44) VALUE                     RSA010
055000     '                                            '.              RSA010
055100          10        2-LIB09.                                      RSA010
055200              15  FILLER  PICTURE X(44) VALUE                     RSA010
055300     '      * RENTES      (RS04)                  '.              RSA010
055400              15  FILLER  PICTURE X(44) VALUE                     RSA010
055500     '                                            '.              RSA010
055600              15  FILLER  PICTURE X(44) VALUE                     RSA010
055700     '                                            '.              RSA010
055800          10        2-LIB10.                                      RSA010
055900              15  FILLER  PICTURE X(44) VALUE                     RSA010
056000     '        ------                              '.              RSA010
056100              15  FILLER  PICTURE X(44) VALUE                     RSA010
056200     '                                            '.              RSA010
056300              15  FILLER  PICTURE X(44) VALUE                     RSA010
056400     '                                            '.              RSA010
056500          10        2-LIB11.                                      RSA010
056600              15  FILLER  PICTURE X(44) VALUE                     RSA010
056700     '               +  NO RENTE (NOREN) : XXXXXXX'.              RSA010
056800              15  FILLER  PICTURE X(44) VALUE                     RSA010
056900     'XXXXXX    INDEX (ZINO41,ZIN042) : XXXXXXXXXX'.              RSA010
057000              15  FILLER  PICTURE X(44) VALUE                     RSA010
057100     'XXXXXX,XXXXXXXXXXXXXXXXXXXX                 '.              RSA010
057200          10        2-LIB12.                                      RSA010
057300              15  FILLER  PICTURE X(44) VALUE                     RSA010
057400     '                  CODE ACTIVITE (CDREA) : X '.              RSA010
057500              15  FILLER  PICTURE X(44) VALUE                     RSA010
057600     '                                 DATE MAJ AC'.              RSA010
057700              15  FILLER  PICTURE X(44) VALUE                     RSA010
057800     'T. (DMREA) : 99999999                       '.              RSA010
057900          10        2-LIB13.                                      RSA010
058000              15  FILLER  PICTURE X(44) VALUE                     RSA010
058100     '                  DATE CONSTIT. (DTREC) : 99'.              RSA010
058200              15  FILLER  PICTURE X(44) VALUE                     RSA010
058300     '999999                           DATE ACCID.'.              RSA010
058400              15  FILLER  PICTURE X(44) VALUE                     RSA010
058500     ' ENTRAINANT RENTE (DTREAC) : 99999999       '.              RSA010
058600          10        2-LIB14.                                      RSA010
058700              15  FILLER  PICTURE X(44) VALUE                     RSA010
058800     '                  NO SINISTRE (NOSIN)   : XX'.              RSA010
058900              15  FILLER  PICTURE X(44) VALUE                     RSA010
059000     'XXXXXXXX                         DATE CLOTUR'.              RSA010
059100              15  FILLER  PICTURE X(44) VALUE                     RSA010
059200     'E RENTE (DFREN) : 99999999                  '.              RSA010
059300          10        2-LIB15.                                      RSA010
059400              15  FILLER  PICTURE X(44) VALUE                     RSA010
059500     '                  FREQUENCE PAIEM (CDPAF): X'.              RSA010
059600              15  FILLER  PICTURE X(44) VALUE                     RSA010
059700     '                                 TYPE TERME '.              RSA010
059800              15  FILLER  PICTURE X(44) VALUE                     RSA010
059900     'PAIEM. (CTPAT) : X                          '.              RSA010
060000          10        2-LIB16.                                      RSA010
060100              15  FILLER  PICTURE X(44) VALUE                     RSA010
060200     '                  TABLE CAPITALIS. (CTRETC):'.              RSA010
060300              15  FILLER  PICTURE X(44) VALUE                     RSA010
060400     ' XXXXXXXX                        CODE QUALIT'.              RSA010
060500              15  FILLER  PICTURE X(44) VALUE                     RSA010
060600     'E RENTIER (CQRER) : X                       '.              RSA010
060700          10        2-LIB17.                                      RSA010
060800              15  FILLER  PICTURE X(44) VALUE                     RSA010
060900     '                  NO GESTIONNAIRE (NOREG) : '.              RSA010
061000              15  FILLER  PICTURE X(44) VALUE                     RSA010
061100     'XXX                              SITUATION R'.              RSA010
061200              15  FILLER  PICTURE X(44) VALUE                     RSA010
061300     'ECOURS RESTE (CERERC) : X                   '.              RSA010
061400          10        2-LIB18.                                      RSA010
061500              15  FILLER  PICTURE X(44) VALUE                     RSA010
061600     '                  CODE GESTION RENTE (CDREG)'.              RSA010
061700              15  FILLER  PICTURE X(44) VALUE                     RSA010
061800     ' : X                             CODE CREAT.'.              RSA010
061900              15  FILLER  PICTURE X(44) VALUE                     RSA010
062000     ' FICT./NORM. (CDREF) : X                    '.              RSA010
062100          10        2-LIB19.                                      RSA010
062200              15  FILLER  PICTURE X(44) VALUE                     RSA010
062300     '                  DATE MAJ TIERS (DMRET) : 9'.              RSA010
062400              15  FILLER  PICTURE X(44) VALUE                     RSA010
062500     '9999999                          (LIRE 30 ):'.              RSA010
062600              15  FILLER  PICTURE X(44) VALUE                     RSA010
062700     ' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX             '.              RSA010
062800          10        2-LIB20.                                      RSA010
062900              15  FILLER  PICTURE X(44) VALUE                     RSA010
063000     '                  MONTANT ANNUEL (MTREA) : -'.              RSA010
063100              15  FILLER  PICTURE X(44) VALUE                     RSA010
063200     '9999999999,99                    DATE DEPART'.              RSA010
063300              15  FILLER  PICTURE X(44) VALUE                     RSA010
063400     ' RENTE (DDREN) : 99999999                   '.              RSA010
063500          10        2-LIB21.                                      RSA010
063600              15  FILLER  PICTURE X(44) VALUE                     RSA010
063700     '                  COMMENTAIRE REGUL (LIRER):'.              RSA010
063800              15  FILLER  PICTURE X(44) VALUE                     RSA010
063900     ' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  MONTANT REG'.              RSA010
064000              15  FILLER  PICTURE X(44) VALUE                     RSA010
064100     'UL ARRERAGES (MORER) : -9999999999,99       '.              RSA010
064200          10        2-LIB22.                                      RSA010
064300              15  FILLER  PICTURE X(44) VALUE                     RSA010
064400     '                  CODE MVT CAPITAL (CDRELK):'.              RSA010
064500              15  FILLER  PICTURE X(44) VALUE                     RSA010
064600     ' XXX                             REPORT REVA'.              RSA010
064700              15  FILLER  PICTURE X(44) VALUE                     RSA010
064800     'LO. A PAYER (MTPARV) : -9999999999,99       '.              RSA010
064900          10        2-LIB23.                                      RSA010
065000              15  FILLER  PICTURE X(44) VALUE                     RSA010
065100     '                  COMMENTAIRE CAP (LIREK)  :'.              RSA010
065200              15  FILLER  PICTURE X(44) VALUE                     RSA010
065300     ' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  DERNIER MVT'.              RSA010
065400              15  FILLER  PICTURE X(44) VALUE                     RSA010
065500     ' CAPITAL (MKREND) :    -9999999999,99       '.              RSA010
065600          10        2-LIB24.                                      RSA010
065700              15  FILLER  PICTURE X(44) VALUE                     RSA010
065800     '                  MONTANT RACHAT (MTRER)   :'.              RSA010
065900              15  FILLER  PICTURE X(44) VALUE                     RSA010
066000     ' -9999999999,99                  CODE SENS E'.              RSA010
066100              15  FILLER  PICTURE X(44) VALUE                     RSA010
066200     'CR. COMPT.(CSREE) :     X                   '.              RSA010
066300          10        2-LIB25.                                      RSA010
066400              15  FILLER  PICTURE X(44) VALUE                     RSA010
066500     '                  DATE DER.ECHEANCE(DHPAID):'.              RSA010
066600              15  FILLER  PICTURE X(44) VALUE                     RSA010
066700     ' 99999999                        DATE DERNIE'.              RSA010
066800              15  FILLER  PICTURE X(44) VALUE                     RSA010
066900     'R PAIEM. (DTPAID) : 99999999                '.              RSA010
067000          10        2-LIB26.                                      RSA010
067100              15  FILLER  PICTURE X(44) VALUE                     RSA010
067200     '                  REF.DER.ORD.PAIEM(LIPAOD):'.              RSA010
067300              15  FILLER  PICTURE X(44) VALUE                     RSA010
067400     ' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  DATE REPRIS'.              RSA010
067500              15  FILLER  PICTURE X(44) VALUE                     RSA010
067600     'E PAIEM. (DTPARP) : 99999999                '.              RSA010
067700          10        2-LIB27.                                      RSA010
067800              15  FILLER  PICTURE X(44) VALUE                     RSA010
067900     '                  CUMUL PAIEM. (MCPAP)     :'.              RSA010
068000              15  FILLER  PICTURE X(44) VALUE                     RSA010
068100     ' -99999999999999,99              MT CUMULE P'.              RSA010
068200              15  FILLER  PICTURE X(44) VALUE                     RSA010
068300     'AIEM.ARCH.(MCPAA) : -99999999999999,99      '.              RSA010
068400          10        2-LIB28.                                      RSA010
068500              15  FILLER  PICTURE X(44) VALUE                     RSA010
068600     '                  DER.NO MODE PAIEM(NOPAMD):'.              RSA010
068700              15  FILLER  PICTURE X(44) VALUE                     RSA010
068800     ' 99                              MONTANT PRO'.              RSA010
068900              15  FILLER  PICTURE X(44) VALUE                     RSA010
069000     'V. MATH. (MKREP)  : -9999999999,99          '.              RSA010
069100          10        2-LIB29.                                      RSA010
069200              15  FILLER  PICTURE X(44) VALUE                     RSA010
069300     '                  MONT.CUMULE CAP. (MKREC) :'.              RSA010
069400              15  FILLER  PICTURE X(44) VALUE                     RSA010
069500     ' -9999999999,99                  CODE REASSU'.              RSA010
069600              15  FILLER  PICTURE X(44) VALUE                     RSA010
069700     'RANCE (CDRER)     : X                       '.              RSA010
069800          10        2-LIB30.                                      RSA010
069900              15  FILLER  PICTURE X(44) VALUE                     RSA010
070000     '                  CODE EXIST. DERNIER MVT DE'.              RSA010
070100              15  FILLER  PICTURE X(44) VALUE                     RSA010
070200     ' CAPITAL (CEREK): X              DERNIER NO '.              RSA010
070300              15  FILLER  PICTURE X(44) VALUE                     RSA010
070400     'PAIEM.(NOPAID)    : XXX                     '.              RSA010
070500          10        2-LIB31.                                      RSA010
070600              15  FILLER  PICTURE X(44) VALUE                     RSA010
070700     '                  DATE DE DERNIER PAIEMENT R'.              RSA010
070800              15  FILLER  PICTURE X(44) VALUE                     RSA010
070900     'EPAIE.-REIMP. (DTPAIS):XXXXXXXX  NUMERO DE D'.              RSA010
071000              15  FILLER  PICTURE X(44) VALUE                     RSA010
071100     'EPARTEMENT (NODEP): XX                      '.              RSA010
071200          10        2-LIB32.                                      RSA010
071300              15  FILLER  PICTURE X(44) VALUE                     RSA010
071400     '                  RENTE CONST/REVISEE (CDREV'.              RSA010
071500              15  FILLER  PICTURE X(44) VALUE                     RSA010
071600     ') : X                            REF ORD PAI'.              RSA010
071700              15  FILLER  PICTURE X(44) VALUE                     RSA010
071800     'EM (LIPA2): XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX '.              RSA010
071900          10        2-LIB33.                                      RSA010
072000              15  FILLER  PICTURE X(44) VALUE                     RSA010
072100     '                  DERN MVT CAPITAL REST(CDRE'.              RSA010
072200              15  FILLER  PICTURE X(44) VALUE                     RSA010
072300     'K): XXX                          DATE 1ERE C'.              RSA010
072400              15  FILLER  PICTURE X(44) VALUE                     RSA010
072500     'ONSTITUT. (DTREC1): XXXXXXXX                '.              RSA010
072600          10        2-LIB34.                                      RSA010
072700              15  FILLER  PICTURE X(44) VALUE                     RSA010
072800     '                  TYPE DE DUREE RENTE (CTREU'.              RSA010
072900              15  FILLER  PICTURE X(44) VALUE                     RSA010
073000     ') : X                            DATE LIMITE'.              RSA010
073100              15  FILLER  PICTURE X(44) VALUE                     RSA010
073200     ' RENTE (DFREL)    : XXXXXXXX                '.              RSA010
073300          10        2-LIB35.                                      RSA010
073400              15  FILLER  PICTURE X(44) VALUE                     RSA010
073500     '                  LIBELLE-2 (LIR30A): XXXXXX'.              RSA010
073600              15  FILLER  PICTURE X(44) VALUE                     RSA010
073700     'XXXXXXXXXXXXXXXXXXXXXXXX         CATEG. ASSU'.              RSA010
073800              15  FILLER  PICTURE X(44) VALUE                     RSA010
073900     'RANCE GT (CCGT1)  : X                       '.              RSA010
074000          10        2-LIB36.                                      RSA010
074100              15  FILLER  PICTURE X(44) VALUE                     RSA010
074200     '                  CODE GARANTIE TARIF. (CDGT'.              RSA010
074300              15  FILLER  PICTURE X(44) VALUE                     RSA010
074400     ') : XXXXX                        CRP GARANTI'.              RSA010
074500              15  FILLER  PICTURE X(44) VALUE                     RSA010
074600     'E TARIFABLE(CRCGJ): X                       '.              RSA010
074700          10        2-LIB37.                                      RSA010
074800              15  FILLER  PICTURE X(44) VALUE                     RSA010
074900     '                  CODE B10 (CNCOB)          '.              RSA010
075000              15  FILLER  PICTURE X(44) VALUE                     RSA010
075100     '  : XXX                          CRP SOUS-CA'.              RSA010
075200              15  FILLER  PICTURE X(44) VALUE                     RSA010
075300     'T. COMPTAB.(CRCGE): X                       '.              RSA010
075400          10        2-LIB38.                                      RSA010
075500              15  FILLER  PICTURE X(44) VALUE                     RSA010
075600     '                  SOUS-CAT REASSURANCE (CCRE'.              RSA010
075700              15  FILLER  PICTURE X(44) VALUE                     RSA010
075800     'N): XXX                          CRP SOUS-CA'.              RSA010
075900              15  FILLER  PICTURE X(44) VALUE                     RSA010
076000     'T. REASSUR.(CRCGD): X                       '.              RSA010
076100          10        2-LIB39.                                      RSA010
076200              15  FILLER  PICTURE X(44) VALUE                     RSA010
076300     '                  CODE CATEGORIE A1 (CCCON) '.              RSA010
076400              15  FILLER  PICTURE X(44) VALUE                     RSA010
076500     '  : XX                           CODE AVIS D'.              RSA010
076600              15  FILLER  PICTURE X(44) VALUE                     RSA010
076700     'E SINISTRE (CDSIN): X                       '.              RSA010
076800          10        2-LIB40.                                      RSA010
076900              15  FILLER  PICTURE X(44) VALUE                     RSA010
077000     '                  CODE EXCED. SINISTRE(CDEXC'.              RSA010
077100              15  FILLER  PICTURE X(44) VALUE                     RSA010
077200     'S): X                            NUMERO SINI'.              RSA010
077300              15  FILLER  PICTURE X(44) VALUE                     RSA010
077400     'STRE LEGAL(NOSINL): XXXXXXXXXX              '.              RSA010
077500          10        2-LIB41.                                      RSA010
077600              15  FILLER  PICTURE X(44) VALUE                     RSA010
077700     '                  NUMERO SIN. SOUSCRIP(NOSIN'.              RSA010
077800              15  FILLER  PICTURE X(44) VALUE                     RSA010
077900     'S): XXXXXXXXXX                   CODE FONDS '.              RSA010
078000              15  FILLER  PICTURE X(44) VALUE                     RSA010
078100     'DE REVALO. (CCFRN): X                       '.              RSA010
078200          10        2-LIB42.                                      RSA010
078300              15  FILLER  PICTURE X(44) VALUE                     RSA010
078400     '                  CODE FONDS REVAL NON CCMA('.              RSA010
078500              15  FILLER  PICTURE X(44) VALUE                     RSA010
078600     'CCFRA) : X                       MT REMBOURS'.              RSA010
078700              15  FILLER  PICTURE X(44) VALUE                     RSA010
078800     'EMT  FONDS (MTREB): 999999999,99            '.              RSA010
078900          10        2-LIB43.                                      RSA010
079000              15  FILLER  PICTURE X(44) VALUE                     RSA010
079100     '                  REF. FONDS EXTERNE (LIFOE)'.              RSA010
079200              15  FILLER  PICTURE X(44) VALUE                     RSA010
079300     '  : XXXXXXXXXX                   RANG DS LE '.              RSA010
079400              15  FILLER  PICTURE X(44) VALUE                     RSA010
079500     'SINISTRE (NIREGS) : 99                      '.              RSA010
079600          10        2-LIB44.                                      RSA010
079700              15  FILLER  PICTURE X(44) VALUE                     RSA010
079800     '                  CODE SOCIETE (CDRES3)     '.              RSA010
079900              15  FILLER  PICTURE X(44) VALUE                     RSA010
080000     '  : X                            NUMERO DE G'.              RSA010
080100              15  FILLER  PICTURE X(44) VALUE                     RSA010
080200     'ROUPAMA (NOCRA3)  : 99                      '.              RSA010
080300          10        2-LIB45.                                      RSA010
080400              15  FILLER  PICTURE X(44) VALUE                     RSA010
080500     '                  CATEGORIE DE LA RENTE(CDCA'.              RSA010
080600              15  FILLER  PICTURE X(44) VALUE                     RSA010
080700     'T): X                            CODE REVALO'.              RSA010
080800              15  FILLER  PICTURE X(44) VALUE                     RSA010
080900     'RISATION (CDRVA)  : X                       '.              RSA010
081000          10        2-LIB46.                                      RSA010
081100              15  FILLER  PICTURE X(44) VALUE                     RSA010
081200     '                  CODE DEVISE (CDMON)       '.              RSA010
081300              15  FILLER  PICTURE X(44) VALUE                     RSA010
081400     '  : XXX                                     '.              RSA010
081500              15  FILLER  PICTURE X(44) VALUE                     RSA010
081600     '                                            '.              RSA010
081700      05            2-LIB-R REDEFINES 2-LIB.                      RSA010
081800           10   1-LI00-2 OCCURS  046.                             RSA010
081900            15  FILLER        PICTURE  X(00132).                  RSA010
082000 01            6-EW00.                                            RSA010
082100     05        6-EW00-2.                                          RSA010
082200      10       6-EW200-SAUT   PICTURE  X.                         RSA010
082300      10       6-EW200        PICTURE  X(132).                    RSA010
082400      10       6-EW201    REDEFINES   6-EW200.                    RSA010
082500        15     FILLER         PICTURE  X(105).                    RSA010
082600        15     6-EW201-DJREN  PICTURE X(8).                       RSA010
082700        15     FILLER         PICTURE  X(012).                    RSA010
082800        15     6-EW201-ZNOPAG PICTURE ZZZZ9.                      RSA010
082900        15     FILLER         PICTURE  X(002).                    RSA010
083000      10       6-EW202    REDEFINES   6-EW200.                    RSA010
083100        15     FILLER         PICTURE  X(017).                    RSA010
083200        15     6-EW202-NORER  PICTURE 9(6).                       RSA010
083300        15     FILLER         PICTURE  X(018).                    RSA010
083400        15     6-EW202-CDRECR PICTURE X(3).                       RSA010
083500        15     FILLER         PICTURE  X(013).                    RSA010
083600        15     6-EW202-CDREXR PICTURE X.                          RSA010
083700        15     FILLER         PICTURE  X(014).                    RSA010
083800        15     6-EW202-DMREN  PICTURE X(10).                      RSA010
083900        15     FILLER         PICTURE  X(010).                    RSA010
084000        15     6-EW202-CEREF  PICTURE X.                          RSA010
084100        15     FILLER         PICTURE  X(011).                    RSA010
084200        15     6-EW202-CTREF  PICTURE XX.                         RSA010
084300        15     FILLER         PICTURE  X(011).                    RSA010
084400        15     6-EW202-DBREF  PICTURE X(10).                      RSA010
084500        15     FILLER         PICTURE  X(005).                    RSA010
084600      10       6-EW203    REDEFINES   6-EW200.                    RSA010
084700        15     FILLER         PICTURE  X(040).                    RSA010
084800        15     6-EW203-CDRES  PICTURE X.                          RSA010
084900        15     6-EW203-LNRENR PICTURE X(20).                      RSA010
085000        15     6-EW203-LNREPR PICTURE X(12).                      RSA010
085100        15     6-EW203-DTRENR PICTURE X(10).                      RSA010
085200        15     6-EW203-NORER1 PICTURE 9(6).                       RSA010
085300        15     FILLER         PICTURE  X(001).                    RSA010
085400        15     6-EW203-CDRES2 PICTURE X.                          RSA010
085500        15     6-EW203-NORER2 PICTURE 9(6).                       RSA010
085600        15     FILLER         PICTURE  X(001).                    RSA010
085700        15     6-EW203-CDRES3 PICTURE X.                          RSA010
085800        15     6-EW203-LNRENF PICTURE X(20).                      RSA010
085900        15     6-EW203-NORER3 PICTURE 9(6).                       RSA010
086000        15     FILLER         PICTURE  X(007).                    RSA010
086100      10       6-EW204    REDEFINES   6-EW200.                    RSA010
086200        15     FILLER         PICTURE  X(029).                    RSA010
086300        15     6-EW204-LRRE1R PICTURE X(36).                      RSA010
086400        15     FILLER         PICTURE  X(001).                    RSA010
086500        15     6-EW204-LRRE2R PICTURE X(32).                      RSA010
086600        15     FILLER         PICTURE  X(016).                    RSA010
086700        15     6-EW204-DMREN  PICTURE X(10).                      RSA010
086800        15     FILLER         PICTURE  X(008).                    RSA010
086900      10       6-EW205    REDEFINES   6-EW200.                    RSA010
087000        15     FILLER         PICTURE  X(029).                    RSA010
087100        15     6-EW205-CPRENR PICTURE 9(5)                        RSA010
087200                                                    BLANK ZERO.   RSA010
087300        15     FILLER         PICTURE  X(002).                    RSA010
087400        15     6-EW205-LVRER  PICTURE X(31).                      RSA010
087500        15     FILLER         PICTURE  X(011).                    RSA010
087600        15     6-EW205-CDREPR PICTURE XXX.                        RSA010
087700        15     FILLER         PICTURE  X(051).                    RSA010
087800      10       6-EW206    REDEFINES   6-EW200.                    RSA010
087900        15     FILLER         PICTURE  X(037).                    RSA010
088000        15     6-EW206-NOREN  PICTURE X(13).                      RSA010
088100        15     FILLER         PICTURE  X(028).                    RSA010
088200        15     6-EW206-CDRES  PICTURE X.                          RSA010
088300        15     6-EW206-NOCRA  PICTURE 99                          RSA010
088400                                                    BLANK ZERO.   RSA010
088500        15     6-EW206-NOREN1 PICTURE X(13).                      RSA010
088600        15     FILLER         PICTURE  X(001).                    RSA010
088700        15     6-EW206-NOTIE  PICTURE 9(6)                        RSA010
088800                                                    BLANK ZERO.   RSA010
088900        15     6-EW206-CDPAD  PICTURE X.                          RSA010
089000        15     6-EW206-NOREN2 PICTURE X(13).                      RSA010
089100        15     FILLER         PICTURE  X(017).                    RSA010
089200      10       6-EW207    REDEFINES   6-EW200.                    RSA010
089300        15     FILLER         PICTURE  X(042).                    RSA010
089400        15     6-EW207-CDREA  PICTURE X.                          RSA010
089500        15     FILLER         PICTURE  X(058).                    RSA010
089600        15     6-EW207-DMREA  PICTURE X(10).                      RSA010
089700        15     FILLER         PICTURE  X(021).                    RSA010
089800      10       6-EW208    REDEFINES   6-EW200.                    RSA010
089900        15     FILLER         PICTURE  X(042).                    RSA010
090000        15     6-EW208-DTREC  PICTURE X(10).                      RSA010
090100        15     FILLER         PICTURE  X(065).                    RSA010
090200        15     6-EW208-DTREAC PICTURE X(10).                      RSA010
090300        15     FILLER         PICTURE  X(005).                    RSA010
090400      10       6-EW209    REDEFINES   6-EW200.                    RSA010
090500        15     FILLER         PICTURE  X(042).                    RSA010
090600        15     6-EW209-NOSIN  PICTURE X(10).                      RSA010
090700        15     FILLER         PICTURE  X(054).                    RSA010
090800        15     6-EW209-DFREN  PICTURE X(10).                      RSA010
090900        15     FILLER         PICTURE  X(016).                    RSA010
091000      10       6-EW210    REDEFINES   6-EW200.                    RSA010
091100        15     FILLER         PICTURE  X(043).                    RSA010
091200        15     6-EW210-CDPAF  PICTURE X.                          RSA010
091300        15     FILLER         PICTURE  X(061).                    RSA010
091400        15     6-EW210-CTPAT  PICTURE X.                          RSA010
091500        15     FILLER         PICTURE  X(026).                    RSA010
091600      10       6-EW211    REDEFINES   6-EW200.                    RSA010
091700        15     FILLER         PICTURE  X(045).                    RSA010
091800        15     6-EW211-CTRETC PICTURE X(8).                       RSA010
091900        15     FILLER         PICTURE  X(055).                    RSA010
092000        15     6-EW211-CQRER  PICTURE X.                          RSA010
092100        15     FILLER         PICTURE  X(023).                    RSA010
092200      10       6-EW212    REDEFINES   6-EW200.                    RSA010
092300        15     FILLER         PICTURE  X(044).                    RSA010
092400        15     6-EW212-NOREG  PICTURE XXX.                        RSA010
092500        15     FILLER         PICTURE  X(065).                    RSA010
092600        15     6-EW212-CERERC PICTURE X.                          RSA010
092700        15     FILLER         PICTURE  X(019).                    RSA010
092800      10       6-EW213    REDEFINES   6-EW200.                    RSA010
092900        15     FILLER         PICTURE  X(047).                    RSA010
093000        15     6-EW213-CDREG  PICTURE X.                          RSA010
093100        15     FILLER         PICTURE  X(063).                    RSA010
093200        15     6-EW213-CDREF  PICTURE X.                          RSA010
093300        15     FILLER         PICTURE  X(020).                    RSA010
093400      10       6-EW214    REDEFINES   6-EW200.                    RSA010
093500        15     FILLER         PICTURE  X(043).                    RSA010
093600        15     6-EW214-DMRET  PICTURE X(10).                      RSA010
093700        15     FILLER         PICTURE  X(036).                    RSA010
093800        15     6-EW214-LIRE30 PICTURE X(30).                      RSA010
093900        15     FILLER         PICTURE  X(013).                    RSA010
094000      10       6-EW215    REDEFINES   6-EW200.                    RSA010
094100        15     FILLER         PICTURE  X(043).                    RSA010
094200        15     6-EW215-MTREA  PICTURE ----B---B--9,99             RSA010
094300                                                    BLANK ZERO.   RSA010
094400        15     FILLER         PICTURE  X(047).                    RSA010
094500        15     6-EW215-DDREN  PICTURE X(10).                      RSA010
094600        15     FILLER         PICTURE  X(017).                    RSA010
094700      10       6-EW216    REDEFINES   6-EW200.                    RSA010
094800        15     FILLER         PICTURE  X(045).                    RSA010
094900        15     6-EW216-LIRER  PICTURE X(30).                      RSA010
095000        15     FILLER         PICTURE  X(036).                    RSA010
095100        15     6-EW216-MORER  PICTURE ----B---B--9,99             RSA010
095200                                                    BLANK ZERO.   RSA010
095300        15     FILLER         PICTURE  X(006).                    RSA010
095400      10       6-EW217    REDEFINES   6-EW200.                    RSA010
095500        15     FILLER         PICTURE  X(045).                    RSA010
095600        15     6-EW217-CDRELK PICTURE X(3).                       RSA010
095700        15     FILLER         PICTURE  X(063).                    RSA010
095800        15     6-EW217-MTPARV PICTURE ----B---B--9,99.            RSA010
095900        15     FILLER         PICTURE  X(006).                    RSA010
096000      10       6-EW218    REDEFINES   6-EW200.                    RSA010
096100        15     FILLER         PICTURE  X(045).                    RSA010
096200        15     6-EW218-LIREK  PICTURE X(30).                      RSA010
096300        15     FILLER         PICTURE  X(036).                    RSA010
096400        15     6-EW218-MKREND PICTURE ----B---B--9,99             RSA010
096500                                                    BLANK ZERO.   RSA010
096600        15     FILLER         PICTURE  X(006).                    RSA010
096700      10       6-EW219    REDEFINES   6-EW200.                    RSA010
096800        15     FILLER         PICTURE  X(045).                    RSA010
096900        15     6-EW219-MTRER  PICTURE ZZZBZZZBZZ9,99.             RSA010
097000        15     FILLER         PICTURE  X(053).                    RSA010
097100        15     6-EW219-CSREE  PICTURE X.                          RSA010
097200        15     FILLER         PICTURE  X(019).                    RSA010
097300      10       6-EW220    REDEFINES   6-EW200.                    RSA010
097400        15     FILLER         PICTURE  X(045).                    RSA010
097500        15     6-EW220-DHPAID PICTURE X(10).                      RSA010
097600        15     FILLER         PICTURE  X(053).                    RSA010
097700        15     6-EW220-DTPAID PICTURE X(10).                      RSA010
097800        15     FILLER         PICTURE  X(014).                    RSA010
097900      10       6-EW221    REDEFINES   6-EW200.                    RSA010
098000        15     FILLER         PICTURE  X(045).                    RSA010
098100        15     6-EW221-LIPAOD PICTURE X(31).                      RSA010
098200        15     FILLER         PICTURE  X(032).                    RSA010
098300        15     6-EW221-DTPARP PICTURE X(10).                      RSA010
098400        15     FILLER         PICTURE  X(014).                    RSA010
098500      10       6-EW222    REDEFINES   6-EW200.                    RSA010
098600        15     FILLER         PICTURE  X(045).                    RSA010
098700        15     6-EW222-MCPAP  PICTURE -B---B---B---B--9,99.       RSA010
098800        15     FILLER         PICTURE  X(043).                    RSA010
098900        15     6-EW222-MCPAA  PICTURE --B---B---B---B--9,99.      RSA010
099000        15     FILLER         PICTURE  X(003).                    RSA010
099100      10       6-EW223    REDEFINES   6-EW200.                    RSA010
099200        15     FILLER         PICTURE  X(045).                    RSA010
099300        15     6-EW223-NOPAMD PICTURE 99.                         RSA010
099400        15     FILLER         PICTURE  X(061).                    RSA010
099500        15     6-EW223-MKREP  PICTURE ----B---B--9,99             RSA010
099600                                                    BLANK ZERO.   RSA010
099700        15     FILLER         PICTURE  X(009).                    RSA010
099800      10       6-EW224    REDEFINES   6-EW200.                    RSA010
099900        15     FILLER         PICTURE  X(045).                    RSA010
100000        15     6-EW224-MKREC  PICTURE ----B---B--9,99             RSA010
100100                                                    BLANK ZERO.   RSA010
100200        15     FILLER         PICTURE  X(048).                    RSA010
100300        15     6-EW224-CDRER  PICTURE X.                          RSA010
100400        15     FILLER         PICTURE  X(023).                    RSA010
100500      10       6-EW225    REDEFINES   6-EW200.                    RSA010
100600        15     FILLER         PICTURE  X(062).                    RSA010
100700        15     6-EW225-CEREK  PICTURE X.                          RSA010
100800        15     FILLER         PICTURE  X(045).                    RSA010
100900        15     6-EW225-NOPAID PICTURE 999.                        RSA010
101000        15     FILLER         PICTURE  X(021).                    RSA010
101100      10       6-EW226    REDEFINES   6-EW200.                    RSA010
101200        15     FILLER         PICTURE  X(067).                    RSA010
101300        15     6-EW226-DTPAIS PICTURE X(10).                      RSA010
101400        15     FILLER         PICTURE  X(031).                    RSA010
101500        15     6-EW226-NODEP  PICTURE XX.                         RSA010
101600        15     FILLER         PICTURE  X(022).                    RSA010
101700      10       6-EW227    REDEFINES   6-EW200.                    RSA010
101800        15     FILLER         PICTURE  X(048).                    RSA010
101900        15     6-EW227-CDREV  PICTURE X.                          RSA010
102000        15     FILLER         PICTURE  X(051).                    RSA010
102100        15     6-EW227-LIPA2  PICTURE X(31).                      RSA010
102200        15     FILLER         PICTURE  X(001).                    RSA010
102300      10       6-EW228    REDEFINES   6-EW200.                    RSA010
102400        15     FILLER         PICTURE  X(048).                    RSA010
102500        15     6-EW228-CDREK  PICTURE XXX.                        RSA010
102600        15     FILLER         PICTURE  X(057).                    RSA010
102700        15     6-EW228-DTREC1 PICTURE X(10).                      RSA010
102800        15     FILLER         PICTURE  X(014).                    RSA010
102900      10       6-EW229    REDEFINES   6-EW200.                    RSA010
103000        15     FILLER         PICTURE  X(048).                    RSA010
103100        15     6-EW229-CTREU  PICTURE X.                          RSA010
103200        15     FILLER         PICTURE  X(059).                    RSA010
103300        15     6-EW229-DFREL  PICTURE X(10).                      RSA010
103400        15     FILLER         PICTURE  X(014).                    RSA010
103500      10       6-EW230    REDEFINES   6-EW200.                    RSA010
103600        15     FILLER         PICTURE  X(038).                    RSA010
103700        15     6-EW230-LIR30A PICTURE X(30).                      RSA010
103800        15     FILLER         PICTURE  X(040).                    RSA010
103900        15     6-EW230-CCGT1  PICTURE X.                          RSA010
104000        15     FILLER         PICTURE  X(023).                    RSA010
104100      10       6-EW231    REDEFINES   6-EW200.                    RSA010
104200        15     FILLER         PICTURE  X(048).                    RSA010
104300        15     6-EW231-CDGT   PICTURE X(5).                       RSA010
104400        15     FILLER         PICTURE  X(055).                    RSA010
104500        15     6-EW231-CRCGJ  PICTURE X.                          RSA010
104600        15     FILLER         PICTURE  X(023).                    RSA010
104700      10       6-EW232    REDEFINES   6-EW200.                    RSA010
104800        15     FILLER         PICTURE  X(048).                    RSA010
104900        15     6-EW232-CNCOB  PICTURE XXX.                        RSA010
105000        15     FILLER         PICTURE  X(057).                    RSA010
105100        15     6-EW232-CRCGE  PICTURE X.                          RSA010
105200        15     FILLER         PICTURE  X(023).                    RSA010
105300      10       6-EW233    REDEFINES   6-EW200.                    RSA010
105400        15     FILLER         PICTURE  X(048).                    RSA010
105500        15     6-EW233-CCREN  PICTURE XXX.                        RSA010
105600        15     FILLER         PICTURE  X(057).                    RSA010
105700        15     6-EW233-CRCGD  PICTURE X.                          RSA010
105800        15     FILLER         PICTURE  X(023).                    RSA010
105900      10       6-EW234    REDEFINES   6-EW200.                    RSA010
106000        15     FILLER         PICTURE  X(048).                    RSA010
106100        15     6-EW234-CCCON  PICTURE XX.                         RSA010
106200        15     FILLER         PICTURE  X(058).                    RSA010
106300        15     6-EW234-CDSIN  PICTURE X.                          RSA010
106400        15     FILLER         PICTURE  X(023).                    RSA010
106500      10       6-EW235    REDEFINES   6-EW200.                    RSA010
106600        15     FILLER         PICTURE  X(048).                    RSA010
106700        15     6-EW235-CDEXCS PICTURE X.                          RSA010
106800        15     FILLER         PICTURE  X(059).                    RSA010
106900        15     6-EW235-NOSINL PICTURE X(10).                      RSA010
107000        15     FILLER         PICTURE  X(014).                    RSA010
107100      10       6-EW236    REDEFINES   6-EW200.                    RSA010
107200        15     FILLER         PICTURE  X(048).                    RSA010
107300        15     6-EW236-NOSINS PICTURE X(10).                      RSA010
107400        15     FILLER         PICTURE  X(050).                    RSA010
107500        15     6-EW236-CCFRN  PICTURE X.                          RSA010
107600        15     FILLER         PICTURE  X(023).                    RSA010
107700      10       6-EW237    REDEFINES   6-EW200.                    RSA010
107800        15     FILLER         PICTURE  X(053).                    RSA010
107900        15     6-EW237-CCFRA  PICTURE X.                          RSA010
108000        15     FILLER         PICTURE  X(054).                    RSA010
108100        15     6-EW237-MTREB  PICTURE ZZZBZZZBZZ9,99.             RSA010
108200        15     FILLER         PICTURE  X(010).                    RSA010
108300      10       6-EW238    REDEFINES   6-EW200.                    RSA010
108400        15     FILLER         PICTURE  X(048).                    RSA010
108500        15     6-EW238-LIFOE  PICTURE X(10).                      RSA010
108600        15     FILLER         PICTURE  X(050).                    RSA010
108700        15     6-EW238-NIREGS PICTURE 99                          RSA010
108800                                                    BLANK ZERO.   RSA010
108900        15     FILLER         PICTURE  X(022).                    RSA010
109000      10       6-EW239    REDEFINES   6-EW200.                    RSA010
109100        15     FILLER         PICTURE  X(048).                    RSA010
109200        15     6-EW239-CDRES3 PICTURE X.                          RSA010
109300        15     FILLER         PICTURE  X(059).                    RSA010
109400        15     6-EW239-NOCRA3 PICTURE 99                          RSA010
109500                                                    BLANK ZERO.   RSA010
109600        15     FILLER         PICTURE  X(022).                    RSA010
109700      10       6-EW240    REDEFINES   6-EW200.                    RSA010
109800        15     FILLER         PICTURE  X(048).                    RSA010
109900        15     6-EW240-CDCAT  PICTURE X.                          RSA010
110000        15     FILLER         PICTURE  X(059).                    RSA010
110100        15     6-EW240-CDRVA  PICTURE X.                          RSA010
110200        15     FILLER         PICTURE  X(023).                    RSA010
110300      10       6-EW241    REDEFINES   6-EW200.                    RSA010
110400        15     FILLER         PICTURE  X(048).                    RSA010
110500        15     6-EW241-CDMON  PICTURE XXX.                        RSA010
110600        15     FILLER         PICTURE  X(081).                    RSA010
110700 01   ZONES-UTILISATEUR PICTURE X.                                RSA010
110800          EXEC SQL BEGIN DECLARE SECTION END-EXEC.                7I2010
110900 01               C-0203.                                         7I2110
111000   05             C-0203-NORER                                    7I2120
111100                  PICTURE X(6).                                   7I2120
111200   05             C-0203-XCSEQ                                    7I2130
111300                  PICTURE S9(9)                                   7I2130
111400                    COMPUTATIONAL-3.                              7I2130
111500 01               C-0204.                                         7I2140
111600   05             C-0204-NORER                                    7I2150
111700                  PICTURE X(6).                                   7I2150
111800   05             C-0204-CNREN                                    7I2160
111900                  PICTURE XX.                                     7I2160
112000   05             C-0204-IDRPL                                    7I2170
112100                  PICTURE X(5).                                   7I2170
112200          EXEC SQL END   DECLARE SECTION END-EXEC.                7I2990
112300 01               D-0203.                                         7I3110
112400   05             D-0203-NORER   VALUE '!'                        7I3120
112500                  PICTURE X(6).                                   7I3120
112600   05             D-0203-XCSEQ   VALUE ZERO                       7I3130
112700                  PICTURE S9(9)                                   7I3130
112800                    COMPUTATIONAL-3.                              7I3130
112900 01               D-0204.                                         7I3140
113000   05             D-0204-NORER   VALUE '!'                        7I3150
113100                  PICTURE X(6).                                   7I3150
113200   05             D-0204-CNREN   VALUE '!'                        7I3160
113300                  PICTURE XX.                                     7I3160
113400   05             D-0204-IDRPL   VALUE '!'                        7I3170
113500                  PICTURE X(5).                                   7I3170
113600 PROCEDURE DIVISION.                                              RSA010
113700 DECLARATIVES.                                                    RSA010
113800 SECEW SECTION.                                                   RSA010
113900     USE AFTER ERROR PROCEDURE ON   EW-FICHIER.                   RSA010
114000 F0AEW. DISPLAY  'STATUS : EW     = '  1-EW00-STATUS.             RSA010
114100 F0AEW-A. GO TO  F0A90.                                           RSA010
114200 SECYX SECTION.                                                   P000
114300     USE AFTER ERROR PROCEDURE                                    P100
114400      ON YX-FICHIER.                                              P110
114500 F0AYX.                                                           P120
114600     MOVE        'YX' TO XA80-XCOSD                               P200
114700     MOVE        1-YX00-STATUS TO XA80-STATUS                     P210
114800     MOVE        5-YX00-CPTENR TO XA80-XQNENR                     P220
114900     GO TO F0A90.                                                 P500
115000 F0AEW-FN. EXIT.                                                  P500
115100 F0A90.                                                           P100
115200     MOVE        '0A90' TO XA60-XCDFSF                            P110
115300     MOVE        'ERREUR I-O' TO XA60-XLISUI.                     P120
115400           IF    XA80-STATUS1 = '9'                               P130
115500     MOVE        XA80-STATUS1 TO XA81-STATUS1                     P132
115600     MOVE        XA80-STATUS2 TO XA81-STATUS2                     P133
115700     MOVE        XA81-STATUS TO XA80-STATUS.                      P135
115800     MOVE        XA80 TO XA60-ZX67A                               P140
115900     MOVE        SPACE TO XA60-ZX67B.                             P150
116000     PERFORM     F9900 THRU F9900-FN.                             P210
116100 F0A90-FN. EXIT.                                                  P210
116200 END DECLARATIVES.                                                RSA010
116300 SEC00 SECTION.                                                   RSA010
116400 F0B.           EXIT.                                             P000
116500 F0BBA.                                                           P000
116600     MOVE        'GCA_DATCE' TO XA30-ENVNAM                       P050
116700     PERFORM     F99VE THRU F99VE-FN.                             P060
116800           IF    IK = ZERO                                        P070
116900     MOVE        XA30-ENVVAL TO DATCE                             P070
117000           ELSE                                                   P100
117100     MOVE FUNCTION CURRENT-DATE (1:8)                             P100
117200       TO DATCE.                                                  P110
117300     ACCEPT TIMCO FROM TIME.                                      P120
117400     MOVE        DATCE TO XAIN-XDATRT                             P210
117500     MOVE        TIMCO TO XAIN-XHETRT.                            P220
117600     MOVE        DATCE                                            P310
117700     TO DAT8E DAT6C                                               P310
117800     MOVE DAT83E TO DAT61C  MOVE DAT81E TO DAT63C                 P310
117900     MOVE DAT82E TO DAT62C                                        P310
118000     MOVE   DAT6C TO  XAED-XDATRT                                 P310
118100     MOVE        XAED-XDATRT                                      P315
118200     TO DAT8E DAT6C                                               P315
118300     MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 P315
118400     MOVE DAT63C TO DAT83C                                        P315
118500     MOVE   DAT8C TO XAED-XDATRT                                  P315
118600     MOVE        TIMCO                                            P320
118700     TO DAT8E DAT6C                                               P320
118800     MOVE DAT61C TO TIMHOU  MOVE DAT62C TO TIMMIN                 P320
118900     MOVE DAT82E TO TIMSEC                                        P320
119000     MOVE TIMDAY TO   XAED-XHETRT.                                P320
119100 F0BBA-FN. EXIT.                                                  P320
119200 F0BBE.                                                           P000
119300     PERFORM     F98-D THRU F98-D-FN.                             P100
119400 F0BBE-FN. EXIT.                                                  P100
119500 F0BCA.         EXIT.                                             P000
119600 F0BCI.                                                           P000
119700     MOVE        'GCA_OUSR' TO XA30-ENVNAM                        P100
119800     PERFORM     F99VE THRU F99VE-FN.                             P110
119900           IF    IK = ZERO                                        P200
120000     MOVE        XA30-ENVVAL TO USERID                            P200
120100           ELSE                                                   P210
120200     MOVE        '0BCE' TO XA60-XCDFSF                            P210
120300     MOVE        'ERREUR SUR VARIABLE GCA_OUSR  ' TO              P230
120400     XA60-XLISUI                                                  P235
120500     MOVE                         'VARIABLE GCA_OUSR   ABSENTE A TP240
120600-                'ORT' TO XA60-ZX67A                              P245
120700     MOVE        SPACE TO XA60-ZX67B                              P250
120800     PERFORM     F9900 THRU F9900-FN.                             P260
120900 F0BCI-FN. EXIT.                                                  P260
121000 F0BCO.                                                           P000
121100     MOVE        'GCA_OTRC' TO XA30-ENVNAM                        P100
121200     PERFORM     F99VE THRU F99VE-FN.                             P110
121300           IF    IK = ZERO                                        P200
121400           AND   XA30-ENVVAL = 'ON'                               P210
121500     MOVE        '1' TO XO00-XORATR.                              P200
121600 F0BCO-FN. EXIT.                                                  P200
121700 F0BCU.                                                           P000
121800     MOVE        'GCA_8TMES' TO XA30-ENVNAM                       P100
121900     PERFORM     F99VE THRU F99VE-FN.                             P110
122000           IF    IK = ZERO                                        P200
122100           AND   XA30-ENVVAL = '1'                                P210
122200     MOVE        XA30-ENVVAL TO XA00-8TMES                        P200
122300           ELSE                                                   P220
122400     MOVE        ZERO TO XA00-8TMES.                              P220
122500 F0BCU-FN. EXIT.                                                  P220
122600 F0BCY.                                                           P000
122700     MOVE        'GCA_ORBS' TO XA30-ENVNAM                        P100
122800     PERFORM     F99VE THRU F99VE-FN.                             P110
122900           IF    IK = ZERO                                        P200
123000     MOVE        XA30-ENVVAL TO XO00-XORARB.                      P200
123100 F0BCY-FN. EXIT.                                                  P200
123200 F0BDA.                                                           P000
123300     PERFORM     F99SX THRU F99SX-FN.                             P100
123400 F0BDA-FN. EXIT.                                                  P100
123500 F0BEA.         EXIT.                                             P000
123600 F0BEA-FN. EXIT.                                                  P000
123700 F0BCA-FN. EXIT.                                                  P000
123800 F0B-FN.   EXIT.                                                  P000
123900 F0C.           EXIT.                                             P000
124000 F0CCI.                                                           P000
124100     PERFORM     F95-WORK-CN THRU F95-WORK-CN-FN                  P100
124200     MOVE        '1' TO XO00-XORACN.                              P110
124300 F0CCI-FN. EXIT.                                                  P110
124400 F0CCO.    IF    XO00-XORATR = '1'                                P000
124500           NEXT SENTENCE ELSE GO TO     F0CCO-FN.                 P000
124600     PERFORM     F95-WORK-TR THRU F95-WORK-TR-FN                  P100
124700     DISPLAY     'TRACE ORACLE IS ON'.                            P110
124800 F0CCO-FN. EXIT.                                                  P110
124900 F0CCY.    IF    XO00-XORARB NOT = SPACE                          P000
125000           NEXT SENTENCE ELSE GO TO     F0CCY-FN.                 P000
125100     PERFORM     F95-WORK-RBS THRU F95-WORK-RBS-FN                P100
125200     DISPLAY     'ROLLBACK SEGMENT : '                            P110
125300     XO00-XORARB.                                                 P120
125400 F0CCY-FN. EXIT.                                                  P120
125500 F0C-FN.   EXIT.                                                  P120
125600 F0EEW.                                                           P000
125700     INSPECT     2-LIB REPLACING ALL '"'                          P100
125800                       BY ''''.                                   P120
125900 F0EEW-FN. EXIT.                                                  P120
126000 F01.      EXIT.                                                  RSA010
126100 F01AA.                                                           P010
126200     PERFORM     F95-RS01-NO THRU F95-RS01-NO-FN.                 P100
126300           IF    IK = ZERO                                        P110
126400     MOVE        RS01-NONER TO W-WA00-NONER                       P110
126500     DISPLAY     'No de NER lu : ' W-WA00-NONER                   P120
126600           ELSE                                                   P130
126700     PERFORM     F99OR THRU F99OR-FN.                             P130
126800 F01AA-FN. EXIT.                                                  P130
126900 F01EW.    OPEN OUTPUT                   EW-FICHIER.              RSA010
127000        IF        1-EW00-STATUS  NOT  = ZERO                      RSA010
127100             AND  1-EW00-STATUS  NOT  = '97'                      RSA010
127200        PERFORM    F0AEW                                          RSA010
127300        PERFORM    F0A90         THRU F0A90-FN.                   RSA010
127400 F01EW-FN. EXIT.                                                  RSA010
127500 F01YX.    OPEN OUTPUT                   YX-FICHIER.              RSA010
127600        IF        1-YX00-STATUS  NOT  = ZERO                      RSA010
127700             AND  1-YX00-STATUS  NOT  = '97'                      RSA010
127800        PERFORM    F0AYX                                          RSA010
127900        PERFORM    F0A90         THRU F0A90-FN.                   RSA010
128000 F01YX-FN. EXIT.                                                  RSA010
128100 F01-FN.   EXIT.                                                  RSA010
128200 F03CA.                                                           P000
128300     MOVE        SPACE TO RS42-ZIN42                              P100
128400     MOVE        'C' TO RS42-CDRES                                P120
128500     MOVE        '1' TO RS42-CDRETS                               P160
128600     MOVE        'RG' TO RS42-NORETS                              P180
128700     MOVE        '000002' TO RS42-NIRET                           P190
128800     PERFORM     F95-RS42-FU THRU F95-RS42-FU-FN.                 P200
128900           IF    IK = '0'                                         P220
129000     MOVE        RS42-ZTA100 TO W-WA00-ABRS27                     P220
129100           ELSE                                                   P240
129200     DISPLAY     'PAS DE TABLE C1RG 000002 '                      P240
129300     'ABORT PROGRAMME'                                            P250
129400     PERFORM     F9900 THRU F9900-FN.                             P260
129500 F03CA-FN. EXIT.                                                  P260
129600 F03CB.                                                           P000
129700           IF    W-WA00-CDMON = 'FRF'                             P100
129800     MOVE        'FRANCS' TO W-WW00-LIDEV.                        P100
129900           IF    W-WA00-CDMON = 'EUR'                             P120
130000     MOVE        'EUROS ' TO W-WW00-LIDEV.                        P120
130100 F03CB-FN. EXIT.                                                  P120
130200*          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            RSA010
130300 F05.           EXIT.                                             RSA010
130400 F20.      IF FT =            ALL '1'                             RSA010
130500           NEXT SENTENCE ELSE GO TO     F20-FN.                   RSA010
130600 F20AD.                                                           P000
130700     PERFORM     F0BBA THRU F0BBA-FN.                             P100
130800 F20AD-FN. EXIT.                                                  P100
130900 F20EW.    CLOSE    EW-FICHIER.                                   RSA010
131000 F20EW-FN. EXIT.                                                  RSA010
131100 F20YX.    CLOSE    YX-FICHIER.                                   RSA010
131200 F20YX-FN. EXIT.                                                  RSA010
131300 F2080.    IF    XO00-XORACN = '1'                                P000
131400           NEXT SENTENCE ELSE GO TO     F2080-FN.                 P000
131500     PERFORM     F95-WORK-CO THRU F95-WORK-CO-FN.                 P100
131600 F2080-FN. EXIT.                                                  P100
131700 F2085.    IF    XO00-XORACN = '1'                                P000
131800           NEXT SENTENCE ELSE GO TO     F2085-FN.                 P000
131900     PERFORM     F95-WORK-RR THRU F95-WORK-RR-FN                  P900
132000     MOVE        ZERO TO XO00-XORACN.                             P910
132100 F2085-FN. EXIT.                                                  P910
132200 F2090.                                                           P000
132300     PERFORM     F98-Z THRU F98-Z-FN.                             P100
132400 F2090-FN. EXIT.                                                  P100
132500 F2095.                                                           P000
132600     MOVE        XA00-XRC TO RETURN-CODE.                         P100
132700 F2095-FN. EXIT.                                                  P100
132800 F2099.     GOBACK.                                               RSA010
132900 F2099-FN. EXIT.                                                  RSA010
133000 F20-FN.   EXIT.                                                  RSA010
133100 F50.           EXIT.                                             P000
133200 F50BB.                                                           P000
133300     ACCEPT      W-WW00-NORER                                     P100
133400     MOVE        W-WW00-NORER TO RS02-NORER                       P110
133500     PERFORM     F95-RS02-FA THRU F95-RS02-FA-FN.                 P120
133600 F50FF.                                                           P000
133700     PERFORM     F95-RS03-FV THRU F95-RS03-FV-FN.                 P100
133800 F50FF-FN. EXIT.                                                  P100
133900 F50HH.    IF    IK = '0'                                         P000
134000           NEXT SENTENCE ELSE GO TO     F50HH-FN.                 P000
134100     MOVE        'RS03' TO W-WW00-ZRECOR                          P100
134200     PERFORM     F82 THRU F82-FN                                  P120
134300     PERFORM     F95-RS03-FV THRU F95-RS03-FV-FN.                 P140
134400 F50HH-900. GO TO F50HH.                                          P140
134500 F50HH-FN. EXIT.                                                  P140
134600 F50JJ.                                                           P000
134700     PERFORM     F95-RS04-FV THRU F95-RS04-FV-FN.                 P100
134800 F50JJ-FN. EXIT.                                                  P100
134900 F50LL.    IF    IK = '0'                                         P000
135000           NEXT SENTENCE ELSE GO TO     F50LL-FN.                 P000
135100     MOVE        'RS04' TO W-WW00-ZRECOR                          P010
135200     PERFORM     F82 THRU F82-FN                                  P120
135300     PERFORM     F95-RS04-FV THRU F95-RS04-FV-FN.                 P140
135400 F50LL-900. GO TO F50LL.                                          P140
135500 F50LL-FN. EXIT.                                                  P140
135600 F50BB-FN. EXIT.                                                  P140
135700 F50RR.                                                           P000
135800     MOVE                     ALL '1' TO FT GO TO F20.            P100
135900 F50RR-FN. EXIT.                                                  P100
136000 F50-FN.   EXIT.                                                  P100
136100 F82.           EXIT.                                             RSA010
136200 F82BB.                                                           RSA010
136300           IF    W-WW00-ZRECOR = 'RS02'                           RSA010
136400                 OR 5-EW00-2CL NOT < 5-EW00-2CLM                  RSA010
136500           MOVE                  01 TO 5-EW00-2CL                 RSA010
136600           ADD      2-BB-NL TO         5-EW00-2CL                 RSA010
136700           MOVE      'BB' TO CAT (J00) ADD 1 TO J00.              RSA010
136800 F82BB-FN. EXIT.                                                  RSA010
136900 F82CC.                                                           RSA010
137000           IF    W-WW00-ZRECOR = 'RS03'                           RSA010
137100           ADD      2-CC-NL TO         5-EW00-2CL                 RSA010
137200           MOVE      'CC' TO CAT (J00) ADD 1 TO J00.              RSA010
137300 F82CC-FN. EXIT.                                                  RSA010
137400 F82DD.                                                           RSA010
137500           IF    W-WW00-ZRECOR = 'RS04'                           RSA010
137600           ADD      2-DD-NL TO         5-EW00-2CL                 RSA010
137700           MOVE      'DD' TO CAT (J00) ADD 1 TO J00.              RSA010
137800 F82DD-FN. EXIT.                                                  RSA010
137900 F82ZZ.    MOVE 1 TO J00.                                         RSA010
138000 F82ZZ-005. MOVE CAT (J00) TO CATX. IF CATX = '  '                RSA010
138100           MOVE 1 TO J00 MOVE SPACE TO CAT-TAB                    RSA010
138200           GO TO     F8299-FN.  MOVE 0 TO J01.                    RSA010
138300           IF CATX                 = 'BB'                         RSA010
138400           MOVE  TS-2-BB TO ST-TA GO TO F82ZZ-009.                RSA010
138500           IF CATX                 = 'CC'                         RSA010
138600           MOVE  TS-2-CC TO ST-TA GO TO F82ZZ-009.                RSA010
138700           IF CATX                 = 'DD'                         RSA010
138800           MOVE  TS-2-DD TO ST-TA GO TO F82ZZ-009.                RSA010
138900 F82ZZ-009. ADD 1 TO J01.                                         RSA010
139000 F82ZZ-010. MOVE ST-TT (J01) TO ST-SLS.                           RSA010
139100           IF  ST-SLS = SPACE                                     RSA010
139200           ADD 1 TO J00           GO TO F82ZZ-005.                RSA010
139300          IF J02 = '00' MOVE SPACE TO 6-EW200 ELSE                RSA010
139400           MOVE 1-LI00-2 (J02)     TO 6-EW200.                    RSA010
139500           IF ST-ABS NOT = ' ' AND SAUT = '01'                    RSA010
139600           ADD         1            TO 5-EW00-2CP.                RSA010
139700 F82ZZ-FN. EXIT.                                                  RSA010
139800 F8200.                                                           RSA010
139900           IF STX = '00'          GO TO F8299.                    RSA010
140000           GO TO                        F8201                     RSA010
140100                                        F8202                     RSA010
140200                                        F8203                     RSA010
140300                                        F8204                     RSA010
140400                                        F8205                     RSA010
140500                                        F8206                     RSA010
140600                                        F8207                     RSA010
140700                                        F8208                     RSA010
140800                                        F8209                     RSA010
140900                                        F8210                     RSA010
141000                                        F8211                     RSA010
141100                                        F8212                     RSA010
141200                                        F8213                     RSA010
141300                                        F8214                     RSA010
141400                                        F8215                     RSA010
141500                                        F8216                     RSA010
141600                                        F8217                     RSA010
141700                                        F8218                     RSA010
141800                                        F8219                     RSA010
141900                                        F8220                     RSA010
142000                                        F8221                     RSA010
142100                                        F8222                     RSA010
142200                                        F8223                     RSA010
142300                                        F8224                     RSA010
142400                                        F8225                     RSA010
142500                                        F8226                     RSA010
142600                                        F8227                     RSA010
142700                                        F8228                     RSA010
142800                                        F8229                     RSA010
142900                                        F8230                     RSA010
143000                                        F8231                     RSA010
143100                                        F8232                     RSA010
143200                                        F8233                     RSA010
143300                                        F8234                     RSA010
143400                                        F8235                     RSA010
143500                                        F8236                     RSA010
143600                                        F8237                     RSA010
143700                                        F8238                     RSA010
143800                                        F8239                     RSA010
143900                                        F8240                     RSA010
144000                                        F8241                     RSA010
144100           DEPENDING ON ST9.                                      RSA010
144200 F8200-FN. EXIT.                                                  RSA010
144300 F8201.                                                           RSA010
144400           MOVE  DATOR           TO   6-EW201-DJREN.              RSA010
144500           MOVE   5-EW00-2CP     TO   6-EW201-ZNOPAG.             RSA010
144600 F8201-99. GO TO  F8299.                                          RSA010
144700 F8201-FN. EXIT.                                                  RSA010
144800 F8202.                                                           RSA010
144900           MOVE     RS02-NORER   TO   6-EW202-NORER.              RSA010
145000           MOVE     RS02-CDRECR  TO   6-EW202-CDRECR.             RSA010
145100           MOVE     RS02-CDREXR  TO   6-EW202-CDREXR.             RSA010
145200           MOVE     RS02-DMREN   TO   6-EW202-DMREN.              RSA010
145300           MOVE     RS02-CEREF   TO   6-EW202-CEREF.              RSA010
145400           MOVE     RS02-CTREF   TO   6-EW202-CTREF.              RSA010
145500           MOVE     RS02-DBREF   TO   6-EW202-DBREF.              RSA010
145600 F8202-99. GO TO  F8299.                                          RSA010
145700 F8202-FN. EXIT.                                                  RSA010
145800 F8203.                                                           RSA010
145900           MOVE     RS02-CDRES   TO   6-EW203-CDRES.              RSA010
146000           MOVE     RS02-LNRENR  TO   6-EW203-LNRENR.             RSA010
146100           MOVE     RS02-LNREPR  TO   6-EW203-LNREPR.             RSA010
146200           MOVE     RS02-DTRENR  TO   6-EW203-DTRENR.             RSA010
146300           MOVE     RS02-NORER   TO   6-EW203-NORER1.             RSA010
146400           MOVE     RS02-CDRES   TO   6-EW203-CDRES2.             RSA010
146500           MOVE     RS02-NORER   TO   6-EW203-NORER2.             RSA010
146600           MOVE     RS02-CDRES   TO   6-EW203-CDRES3.             RSA010
146700           MOVE     RS02-LNRENF  TO   6-EW203-LNRENF.             RSA010
146800           MOVE     RS02-NORER   TO   6-EW203-NORER3.             RSA010
146900 F8203-99. GO TO  F8299.                                          RSA010
147000 F8203-FN. EXIT.                                                  RSA010
147100 F8204.                                                           RSA010
147200           MOVE     RS03-LRRE1R  TO   6-EW204-LRRE1R.             RSA010
147300           MOVE     RS03-LRRE2R  TO   6-EW204-LRRE2R.             RSA010
147400           MOVE     RS03-DMREN   TO   6-EW204-DMREN.              RSA010
147500 F8204-99. GO TO  F8299.                                          RSA010
147600 F8204-FN. EXIT.                                                  RSA010
147700 F8205.                                                           RSA010
147800           MOVE     RS03-CPRENR  TO   6-EW205-CPRENR.             RSA010
147900           MOVE     RS03-LVRER   TO   6-EW205-LVRER.              RSA010
148000           MOVE     RS03-CDREPR  TO   6-EW205-CDREPR.             RSA010
148100 F8205-99. GO TO  F8299.                                          RSA010
148200 F8205-FN. EXIT.                                                  RSA010
148300 F8206.                                                           RSA010
148400           MOVE     RS04-NOREN   TO   6-EW206-NOREN.              RSA010
148500           MOVE     RS04-CDRES   TO   6-EW206-CDRES.              RSA010
148600           MOVE     RS04-NOCRA   TO   6-EW206-NOCRA.              RSA010
148700           MOVE     RS04-NOREN   TO   6-EW206-NOREN1.             RSA010
148800           MOVE     RS04-NOTIE   TO   6-EW206-NOTIE.              RSA010
148900           MOVE     RS04-CDPAD   TO   6-EW206-CDPAD.              RSA010
149000           MOVE     RS04-NOREN   TO   6-EW206-NOREN2.             RSA010
149100 F8206-99. GO TO  F8299.                                          RSA010
149200 F8206-FN. EXIT.                                                  RSA010
149300 F8207.                                                           RSA010
149400           MOVE     RS04-CDREA   TO   6-EW207-CDREA.              RSA010
149500           MOVE     RS04-DMREA   TO   6-EW207-DMREA.              RSA010
149600 F8207-99. GO TO  F8299.                                          RSA010
149700 F8207-FN. EXIT.                                                  RSA010
149800 F8208.                                                           RSA010
149900           MOVE     RS04-DTREC   TO   6-EW208-DTREC.              RSA010
150000           MOVE     RS04-DTREAC  TO   6-EW208-DTREAC.             RSA010
150100 F8208-99. GO TO  F8299.                                          RSA010
150200 F8208-FN. EXIT.                                                  RSA010
150300 F8209.                                                           RSA010
150400           MOVE     RS04-NOSIN   TO   6-EW209-NOSIN.              RSA010
150500           MOVE     RS04-DFREN   TO   6-EW209-DFREN.              RSA010
150600 F8209-99. GO TO  F8299.                                          RSA010
150700 F8209-FN. EXIT.                                                  RSA010
150800 F8210.                                                           RSA010
150900           MOVE     RS04-CDPAF   TO   6-EW210-CDPAF.              RSA010
151000           MOVE     RS04-CTPAT   TO   6-EW210-CTPAT.              RSA010
151100 F8210-99. GO TO  F8299.                                          RSA010
151200 F8210-FN. EXIT.                                                  RSA010
151300 F8211.                                                           RSA010
151400           MOVE     RS04-CTRETC  TO   6-EW211-CTRETC.             RSA010
151500           MOVE     RS04-CQRER   TO   6-EW211-CQRER.              RSA010
151600 F8211-99. GO TO  F8299.                                          RSA010
151700 F8211-FN. EXIT.                                                  RSA010
151800 F8212.                                                           RSA010
151900           MOVE     RS04-NOREG   TO   6-EW212-NOREG.              RSA010
152000           MOVE     RS04-CERERC  TO   6-EW212-CERERC.             RSA010
152100 F8212-99. GO TO  F8299.                                          RSA010
152200 F8212-FN. EXIT.                                                  RSA010
152300 F8213.                                                           RSA010
152400           MOVE     RS04-CDREG   TO   6-EW213-CDREG.              RSA010
152500           MOVE     RS04-CDREF   TO   6-EW213-CDREF.              RSA010
152600 F8213-99. GO TO  F8299.                                          RSA010
152700 F8213-FN. EXIT.                                                  RSA010
152800 F8214.                                                           RSA010
152900           MOVE     RS04-DMRET   TO   6-EW214-DMRET.              RSA010
153000           MOVE     RS04-LIRE30  TO   6-EW214-LIRE30.             RSA010
153100 F8214-99. GO TO  F8299.                                          RSA010
153200 F8214-FN. EXIT.                                                  RSA010
153300 F8215.                                                           RSA010
153400           MOVE     RS04-MTREA   TO   6-EW215-MTREA.              RSA010
153500           MOVE     RS04-DDREN   TO   6-EW215-DDREN.              RSA010
153600 F8215-99. GO TO  F8299.                                          RSA010
153700 F8215-FN. EXIT.                                                  RSA010
153800 F8216.                                                           RSA010
153900           MOVE     RS04-LIRER   TO   6-EW216-LIRER.              RSA010
154000           MOVE     RS04-MORER   TO   6-EW216-MORER.              RSA010
154100 F8216-99. GO TO  F8299.                                          RSA010
154200 F8216-FN. EXIT.                                                  RSA010
154300 F8217.                                                           RSA010
154400           MOVE     RS04-CDRELK  TO   6-EW217-CDRELK.             RSA010
154500           MOVE     RS04-MTPARV  TO   6-EW217-MTPARV.             RSA010
154600 F8217-99. GO TO  F8299.                                          RSA010
154700 F8217-FN. EXIT.                                                  RSA010
154800 F8218.                                                           RSA010
154900           MOVE     RS04-LIREK   TO   6-EW218-LIREK.              RSA010
155000           MOVE     RS04-MKREND  TO   6-EW218-MKREND.             RSA010
155100 F8218-99. GO TO  F8299.                                          RSA010
155200 F8218-FN. EXIT.                                                  RSA010
155300 F8219.                                                           RSA010
155400           MOVE     RS04-MTRER   TO   6-EW219-MTRER.              RSA010
155500           MOVE     RS04-CSREE   TO   6-EW219-CSREE.              RSA010
155600 F8219-99. GO TO  F8299.                                          RSA010
155700 F8219-FN. EXIT.                                                  RSA010
155800 F8220.                                                           RSA010
155900           MOVE     RS04-DHPAID  TO   6-EW220-DHPAID.             RSA010
156000           MOVE     RS04-DTPAID  TO   6-EW220-DTPAID.             RSA010
156100 F8220-99. GO TO  F8299.                                          RSA010
156200 F8220-FN. EXIT.                                                  RSA010
156300 F8221.                                                           RSA010
156400           MOVE     RS04-LIPAOD  TO   6-EW221-LIPAOD.             RSA010
156500           MOVE     RS04-DTPARP  TO   6-EW221-DTPARP.             RSA010
156600 F8221-99. GO TO  F8299.                                          RSA010
156700 F8221-FN. EXIT.                                                  RSA010
156800 F8222.                                                           RSA010
156900           MOVE     RS04-MCPAP   TO   6-EW222-MCPAP.              RSA010
157000           MOVE     RS04-MCPAA   TO   6-EW222-MCPAA.              RSA010
157100 F8222-99. GO TO  F8299.                                          RSA010
157200 F8222-FN. EXIT.                                                  RSA010
157300 F8223.                                                           RSA010
157400           MOVE     RS04-NOPAMD  TO   6-EW223-NOPAMD.             RSA010
157500           MOVE     RS04-MKREP   TO   6-EW223-MKREP.              RSA010
157600 F8223-99. GO TO  F8299.                                          RSA010
157700 F8223-FN. EXIT.                                                  RSA010
157800 F8224.                                                           RSA010
157900           MOVE     RS04-MKREC   TO   6-EW224-MKREC.              RSA010
158000           MOVE     RS04-CDRER   TO   6-EW224-CDRER.              RSA010
158100 F8224-99. GO TO  F8299.                                          RSA010
158200 F8224-FN. EXIT.                                                  RSA010
158300 F8225.                                                           RSA010
158400           MOVE     RS04-CEREK   TO   6-EW225-CEREK.              RSA010
158500           MOVE     RS04-NOPAID  TO   6-EW225-NOPAID.             RSA010
158600 F8225-99. GO TO  F8299.                                          RSA010
158700 F8225-FN. EXIT.                                                  RSA010
158800 F8226.                                                           RSA010
158900           MOVE     RS04-DTPAIS  TO   6-EW226-DTPAIS.             RSA010
159000           MOVE     RS04-NODEP   TO   6-EW226-NODEP.              RSA010
159100 F8226-99. GO TO  F8299.                                          RSA010
159200 F8226-FN. EXIT.                                                  RSA010
159300 F8227.                                                           RSA010
159400           MOVE     RS04-CDREV   TO   6-EW227-CDREV.              RSA010
159500           MOVE     RS04-LIPA2   TO   6-EW227-LIPA2.              RSA010
159600 F8227-99. GO TO  F8299.                                          RSA010
159700 F8227-FN. EXIT.                                                  RSA010
159800 F8228.                                                           RSA010
159900           MOVE     RS04-CDREK   TO   6-EW228-CDREK.              RSA010
160000           MOVE     RS04-DTREC1  TO   6-EW228-DTREC1.             RSA010
160100 F8228-99. GO TO  F8299.                                          RSA010
160200 F8228-FN. EXIT.                                                  RSA010
160300 F8229.                                                           RSA010
160400           MOVE     RS04-CTREU   TO   6-EW229-CTREU.              RSA010
160500           MOVE     RS04-DFREL   TO   6-EW229-DFREL.              RSA010
160600 F8229-99. GO TO  F8299.                                          RSA010
160700 F8229-FN. EXIT.                                                  RSA010
160800 F8230.                                                           RSA010
160900           MOVE     RS04-LIR30A  TO   6-EW230-LIR30A.             RSA010
161000           MOVE     RS04-CCGT1   TO   6-EW230-CCGT1.              RSA010
161100 F8230-99. GO TO  F8299.                                          RSA010
161200 F8230-FN. EXIT.                                                  RSA010
161300 F8231.                                                           RSA010
161400           MOVE     RS04-CDGT    TO   6-EW231-CDGT.               RSA010
161500           MOVE     RS04-CRCGJ   TO   6-EW231-CRCGJ.              RSA010
161600 F8231-99. GO TO  F8299.                                          RSA010
161700 F8231-FN. EXIT.                                                  RSA010
161800 F8232.                                                           RSA010
161900           MOVE     RS04-CNCOB   TO   6-EW232-CNCOB.              RSA010
162000           MOVE     RS04-CRCGE   TO   6-EW232-CRCGE.              RSA010
162100 F8232-99. GO TO  F8299.                                          RSA010
162200 F8232-FN. EXIT.                                                  RSA010
162300 F8233.                                                           RSA010
162400           MOVE     RS04-CCREN   TO   6-EW233-CCREN.              RSA010
162500           MOVE     RS04-CRCGD   TO   6-EW233-CRCGD.              RSA010
162600 F8233-99. GO TO  F8299.                                          RSA010
162700 F8233-FN. EXIT.                                                  RSA010
162800 F8234.                                                           RSA010
162900           MOVE     RS04-CCCON   TO   6-EW234-CCCON.              RSA010
163000           MOVE     RS04-CDSIN   TO   6-EW234-CDSIN.              RSA010
163100 F8234-99. GO TO  F8299.                                          RSA010
163200 F8234-FN. EXIT.                                                  RSA010
163300 F8235.                                                           RSA010
163400           MOVE     RS04-CDEXCS  TO   6-EW235-CDEXCS.             RSA010
163500           MOVE     RS04-NOSINL  TO   6-EW235-NOSINL.             RSA010
163600 F8235-99. GO TO  F8299.                                          RSA010
163700 F8235-FN. EXIT.                                                  RSA010
163800 F8236.                                                           RSA010
163900           MOVE     RS04-NOSINS  TO   6-EW236-NOSINS.             RSA010
164000           MOVE     RS04-CCFRN   TO   6-EW236-CCFRN.              RSA010
164100 F8236-99. GO TO  F8299.                                          RSA010
164200 F8236-FN. EXIT.                                                  RSA010
164300 F8237.                                                           RSA010
164400           MOVE     RS04-CCFRA   TO   6-EW237-CCFRA.              RSA010
164500           MOVE     RS04-MTREB   TO   6-EW237-MTREB.              RSA010
164600 F8237-99. GO TO  F8299.                                          RSA010
164700 F8237-FN. EXIT.                                                  RSA010
164800 F8238.                                                           RSA010
164900           MOVE     RS04-LIFOE   TO   6-EW238-LIFOE.              RSA010
165000           MOVE     RS04-NIREGS  TO   6-EW238-NIREGS.             RSA010
165100 F8238-99. GO TO  F8299.                                          RSA010
165200 F8238-FN. EXIT.                                                  RSA010
165300 F8239.                                                           RSA010
165400           MOVE     RS04-CDRES3  TO   6-EW239-CDRES3.             RSA010
165500           MOVE     RS04-NOCRA3  TO   6-EW239-NOCRA3.             RSA010
165600 F8239-99. GO TO  F8299.                                          RSA010
165700 F8239-FN. EXIT.                                                  RSA010
165800 F8240.                                                           RSA010
165900           MOVE     RS04-CDCAT   TO   6-EW240-CDCAT.              RSA010
166000           MOVE     RS04-CDRVA   TO   6-EW240-CDRVA.              RSA010
166100 F8240-99. GO TO  F8299.                                          RSA010
166200 F8240-FN. EXIT.                                                  RSA010
166300 F8241.                                                           RSA010
166400           MOVE   W-WA00-CDMON   TO   6-EW241-CDMON.              RSA010
166500 F8241-99. GO TO  F8299.                                          RSA010
166600 F8241-FN. EXIT.                                                  RSA010
166700 F8299.    MOVE     6-EW00     TO   EW00.                         RSA010
166800           IF ST-ABS = ' '        GO TO F8299-10.                 RSA010
166900           MOVE ' ' TO ST-ABS.                                    RSA010
167000           IF SAUT = '01' MOVE 1    TO 5-EW00-2CL1                RSA010
167100           WRITE EW00    AFTER   ADVANCING SAUTP                  RSA010
167200           GO TO  F8299-20.                                       RSA010
167300           SUBTRACT 5-EW00-2CL1  FROM SAUT.                       RSA010
167400 F8299-10. IF SAUT = '00'                                         RSA010
167500           WRITE EW00    AFTER   ADVANCING SAUT0 ELSE             RSA010
167600           WRITE EW00    AFTER   ADVANCING SAUT                   RSA010
167700           ADD  SAUT                TO 5-EW00-2CL1.               RSA010
167800 F8299-20. ADD 1 TO 5-EW00-2CE.   GO TO F82ZZ-009.                RSA010
167900 F8299-FN. EXIT.                                                  RSA010
168000 F82-FN.   EXIT.                                                  RSA010
168100 F90.      EXIT.                                                  RSA010
168200 F90YU.                                                           P000
168300         GO TO     F90YU-FN.                                      P100
168400 F90YX.                                                           RSA010
168500           WRITE    YX00.                                         RSA010
168600 F90YX-99. ADD 1 TO 5-YX00-CPTENR.                                RSA010
168700 F90YX-FN. EXIT.                                                  RSA010
168800 F90YU-FN. EXIT.                                                  RSA010
168900 F90YZ.         EXIT.                                             P000
169000 F90YZ-FN. EXIT.                                                  P000
169100 F90-FN.   EXIT.                                                  P000
169200 F9099-ITER-FN.  GO TO F05.                                       RSA010
169300 F95-A.                                                           P000
169400     EXEC SQL    WHENEVER SQLWARNING CONTINUE          END-EXEC.  P100
169500     EXEC SQL    WHENEVER NOT FOUND  CONTINUE          END-EXEC.  P200
169600     EXEC SQL    WHENEVER SQLERROR   GO TO F99OR       END-EXEC.  P300
169700 F95-A-FN. EXIT.                                                  P300
169800 F95AA.         EXIT.                                             P000
169900 F95-RS01-NO.                                                     P100
170000     MOVE        'SELECT' TO XO00-XORATY                          P101
170100     MOVE        '95AA' TO XO00-XCDFSF                            P102
170200     MOVE        'RS01' TO XO00-XORATA                            P103
170300     MOVE        RS01 TO XO00-XORACL                              P104
170400     MOVE        ZERO TO XOAA-RS01-CF                             P105
170500     EXEC SQL                                                     P108
170600                 SELECT  NONER                                    P110
170700                   INTO :RS01-NONER                               P210
170800                   FROM  RS01                          END-EXEC.  P300
170900     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
171000           IF    IK = ZERO                                        P485
171100     MOVE        '1' TO XOAA-RS01-CF                              P485
171200     ADD         1 TO XOAA-COUNT                                  P490
171300     MOVE        XP00-XROWID TO RS01-XROWID.                      P495
171400 F95-RS01-NO-FN. EXIT.                                            P499
171500 F95AA-FN. EXIT.                                                  P499
171600 F95BB.         EXIT.                                             P000
171700 F95-RS42-FU.                                                     P100
171800     MOVE        'SELECT' TO XO00-XORATY                          P101
171900     MOVE        '95BB' TO XO00-XCDFSF                            P102
172000     MOVE        'RS42' TO XO00-XORATA                            P103
172100     MOVE        RS42 TO XO00-XORACL                              P104
172200     MOVE        ZERO TO XOBB-RS42-CF                             P105
172300     EXEC SQL                                                     P108
172400                 SELECT  ROWID,                                   P110
172500                         ZCLET                                    P111
172600                      ,  NIRET                                    P112
172700                      ,  ZTA100                                   P113
172800                   INTO :XP00-XROWID,                             P210
172900                        :RS42-ZCLET                               P211
173000                      , :RS42-NIRET                               P212
173100                      , :RS42-ZTA100                              P213
173200                   FROM  RS42                                     P300
173300                  WHERE                                           P310
173400                         ZCLET  =  :RS42-ZCLET                    P311
173500                    AND  NIRET  =  :RS42-NIRET                    P312
173600                    AND  ROWNUM = 1                    END-EXEC.  P410
173700     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
173800           IF    IK = ZERO                                        P485
173900     MOVE        '1' TO XOBB-RS42-CF                              P485
174000     ADD         1 TO XOBB-COUNT                                  P490
174100     MOVE        XP00-XROWID TO RS42-XROWID.                      P495
174200 F95-RS42-FU-FN. EXIT.                                            P499
174300 F95BB-FN. EXIT.                                                  P499
174400 F95CC.         EXIT.                                             P000
174500 F95-RS02-FA.                                                     P100
174600     MOVE        'SELECT' TO XO00-XORATY                          P101
174700     MOVE        '95CC' TO XO00-XCDFSF                            P102
174800     MOVE        'RS02' TO XO00-XORATA                            P103
174900     MOVE        RS02 TO XO00-XORACL                              P104
175000     MOVE        ZERO TO XOCC-RS02-CF                             P105
175100     EXEC SQL                                                     P108
175200                 SELECT  ROWID,                                   P110
175300                         NORER                                    P111
175400                      ,  nvl(CDRES ,' ')                          P112
175500                      ,  nvl(LNRENR,' ')                          P113
175600                      ,  nvl(LNREPR,' ')                          P114
175700                      ,  nvl(DTRENR,' ')                          P115
175800                      ,  nvl(LNRENF,' ')                          P116
175900                      ,  nvl(CDRECR,' ')                          P117
176000                      ,  nvl(CDREXR,' ')                          P118
176100                      ,  nvl(CTREF ,' ')                          P119
176200                      ,  nvl(CEREF ,' ')                          P120
176300                      ,  nvl(DBREF ,' ')                          P121
176400                      ,  nvl(DMREN ,' ')                          P122
176500                   INTO :XP00-XROWID,                             P210
176600                        :RS02-NORER                               P211
176700                      , :RS02-CDRES                               P212
176800                      , :RS02-LNRENR                              P213
176900                      , :RS02-LNREPR                              P214
177000                      , :RS02-DTRENR                              P215
177100                      , :RS02-LNRENF                              P216
177200                      , :RS02-CDRECR                              P217
177300                      , :RS02-CDREXR                              P218
177400                      , :RS02-CTREF                               P219
177500                      , :RS02-CEREF                               P220
177600                      , :RS02-DBREF                               P221
177700                      , :RS02-DMREN                               P222
177800                   FROM  RS02                                     P300
177900                  WHERE                                           P310
178000                         NORER  = :RS02-NORER          END-EXEC.  P311
178100     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
178200           IF    IK = ZERO                                        P485
178300     MOVE        '1' TO XOCC-RS02-CF                              P485
178400     ADD         1 TO XOCC-COUNT                                  P490
178500     MOVE        XP00-XROWID TO RS02-XROWID.                      P495
178600 F95-RS02-FA-FN. EXIT.                                            P499
178700 F95CC-FN. EXIT.                                                  P499
178800 F95DD.         EXIT.                                             P000
178900 F95-RS03-FV.                                                     P100
179000     MOVE        'SELECT' TO XO00-XORATY                          P101
179100     MOVE        '95DD' TO XO00-XCDFSF                            P102
179200     MOVE        'RS03' TO XO00-XORATA                            P103
179300     MOVE        RS03 TO XO00-XORACL                              P104
179400     MOVE        ZERO TO XODD-RS03-CF                             P105
179500     EXEC SQL                                                     P108
179600                 SELECT  ROWID,                                   P110
179700                         NORER                                    P111
179800                      ,  XCSEQ                                    P112
179900                      ,  nvl(LRRE1R,' ')                          P113
180000                      ,  nvl(LRRE2R,' ')                          P114
180100                      ,  nvl(LVRER ,' ')                          P115
180200                      ,  nvl(CPRENR,' ')                          P116
180300                      ,  nvl(CDREPR,' ')                          P117
180400                      ,  nvl(DMREN ,' ')                          P118
180500                   INTO :XP00-XROWID,                             P210
180600                        :RS03-NORER                               P211
180700                      , :RS03-XCSEQ                               P212
180800                      , :RS03-LRRE1R                              P213
180900                      , :RS03-LRRE2R                              P214
181000                      , :RS03-LVRER                               P215
181100                      , :RS03-CPRENR                              P216
181200                      , :RS03-CDREPR                              P217
181300                      , :RS03-DMREN                               P218
181400                   FROM  RS03                                     P300
181500                  WHERE                                           P310
181600                         NORER  = :C-0203-NORER                   P311
181700                    AND                                           P400
181800                         XCSEQ                                    P401
181900                      = ( SELECT MIN (                            P420
182000                         XCSEQ                                    P421
182100                                     )                            P440
182200                   FROM  RS03                                     P442
182300                  WHERE                                           P444
182400                         NORER  = :C-0203-NORER                   P445
182500                    AND (XCSEQ  > :C-0203-XCSEQ                   P446
182600                        )                                         P447
182700                        )                              END-EXEC.  P475
182800     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
182900           IF    IK = ZERO                                        P485
183000     MOVE        '1' TO XODD-RS03-CF                              P485
183100     ADD         1 TO XODD-COUNT                                  P490
183200     MOVE        XP00-XROWID TO RS03-XROWID.                      P495
183300 F95-RS03-FV-FN. EXIT.                                            P499
183400 F95DD-FN. EXIT.                                                  P499
183500 F95EE.         EXIT.                                             P000
183600 F95-RS04-FV.                                                     P100
183700     MOVE        'SELECT' TO XO00-XORATY                          P101
183800     MOVE        '95EE' TO XO00-XCDFSF                            P102
183900     MOVE        'RS04' TO XO00-XORATA                            P103
184000     MOVE        RS04 TO XO00-XORACL                              P104
184100     MOVE        ZERO TO XOEE-RS04-CF                             P105
184200     EXEC SQL                                                     P108
184300                 SELECT  ROWID,                                   P110
184400                         CNREN                                    P111
184500                      ,  NORER                                    P112
184600                      ,  IDRPL                                    P113
184700                      ,  nvl(CDRES ,' ')                          P114
184800                      ,  nvl(NOCRA ,' ')                          P115
184900                      ,  nvl(NOTIE ,' ')                          P116
185000                      ,  nvl(CDPAD ,' ')                          P117
185100                      ,  nvl(CDREA ,' ')                          P118
185200                      ,  nvl(DMREA ,' ')                          P119
185300                      ,  nvl(DFREN ,' ')                          P120
185400                      ,  nvl(NOSIN ,' ')                          P121
185500                      ,  nvl(DTREAC,' ')                          P122
185600                      ,  nvl(DTREC ,' ')                          P123
185700                      ,  nvl(CTRETC,' ')                          P124
185800                      ,  nvl(CDPAF ,' ')                          P125
185900                      ,  nvl(CTPAT ,' ')                          P126
186000                      ,  nvl(CQRER ,' ')                          P127
186100                      ,  nvl(CERERC,' ')                          P128
186200                      ,  nvl(NOREG ,' ')                          P129
186300                      ,  nvl(CDREF ,' ')                          P130
186400                      ,  nvl(CDREG ,' ')                          P131
186500                      ,  nvl(LIRE30,' ')                          P132
186600                      ,  nvl(DMRET ,' ')                          P133
186700                      ,  nvl(DDREN ,' ')                          P134
186800                      ,  nvl(MTREA , 0)                           P135
186900                      ,  nvl(MORER , 0)                           P136
187000                      ,  nvl(LIRER ,' ')                          P137
187100                      ,  nvl(MTPARV, 0)                           P138
187200                      ,  nvl(CDRELK,' ')                          P139
187300                      ,  nvl(MKREND, 0)                           P140
187400                      ,  nvl(LIREK ,' ')                          P141
187500                      ,  nvl(CSREE ,' ')                          P142
187600                      ,  nvl(MTRER , 0)                           P143
187700                      ,  nvl(DTPAID,' ')                          P144
187800                      ,  nvl(DHPAID,' ')                          P145
187900                      ,  nvl(DTPARP,' ')                          P146
188000                      ,  nvl(LIPAOD,' ')                          P147
188100                      ,  nvl(MCPAA , 0)                           P148
188200                      ,  nvl(MCPAP , 0)                           P149
188300                      ,  nvl(MKREP , 0)                           P150
188400                      ,  nvl(CDRER ,' ')                          P151
188500                      ,  nvl(NOPAMD, 0)                           P152
188600                      ,  nvl(NOPAID, 0)                           P153
188700                      ,  nvl(MKREC , 0)                           P154
188800                      ,  nvl(CEREK ,' ')                          P155
188900                      ,  nvl(DTPAIS,' ')                          P156
189000                      ,  nvl(NODEP ,' ')                          P157
189100                      ,  nvl(CDREV ,' ')                          P158
189200                      ,  nvl(LIPA2 ,' ')                          P159
189300                      ,  nvl(CDREK ,' ')                          P160
189400                      ,  nvl(DTREC1,' ')                          P161
189500                      ,  nvl(CTREU ,' ')                          P162
189600                      ,  nvl(DFREL ,' ')                          P163
189700                      ,  nvl(LIR30A,' ')                          P164
189800                      ,  nvl(CCGT1 ,' ')                          P165
189900                      ,  nvl(CDGT  ,' ')                          P166
190000                      ,  nvl(CRCGJ ,' ')                          P167
190100                      ,  nvl(CNCOB ,' ')                          P168
190200                      ,  nvl(CRCGE ,' ')                          P169
190300                      ,  nvl(CCREN ,' ')                          P170
190400                      ,  nvl(CRCGD ,' ')                          P171
190500                      ,  nvl(CCCON ,' ')                          P172
190600                      ,  nvl(CDSIN ,' ')                          P173
190700                      ,  nvl(CDEXCS,' ')                          P174
190800                      ,  nvl(NOSINL,' ')                          P175
190900                      ,  nvl(NOSINS,' ')                          P176
191000                      ,  nvl(CCFRN ,' ')                          P177
191100                      ,  nvl(CCFRA ,' ')                          P178
191200                      ,  nvl(MTREB , 0)                           P179
191300                      ,  nvl(LIFOE ,' ')                          P180
191400                      ,  nvl(NIREGS,' ')                          P181
191500                      ,  nvl(CDRES3,' ')                          P182
191600                      ,  nvl(NOCRA3,' ')                          P183
191700                      ,  nvl(CDCAT ,' ')                          P184
191800                      ,  nvl(CDRVA ,' ')                          P185
191900                      ,  nvl(CDFISC,' ')                          P186
192000                      ,  nvl(CDRESO,' ')                          P187
192100                      ,  nvl(CCRER ,' ')                          P188
192200                      ,  nvl(CTRTCA,' ')                          P189
192300                   INTO :XP00-XROWID,                             P210
192400                        :RS04-CNREN                               P211
192500                      , :RS04-NORER                               P212
192600                      , :RS04-IDRPL                               P213
192700                      , :RS04-CDRES                               P214
192800                      , :RS04-NOCRA                               P215
192900                      , :RS04-NOTIE                               P216
193000                      , :RS04-CDPAD                               P217
193100                      , :RS04-CDREA                               P218
193200                      , :RS04-DMREA                               P219
193300                      , :RS04-DFREN                               P220
193400                      , :RS04-NOSIN                               P221
193500                      , :RS04-DTREAC                              P222
193600                      , :RS04-DTREC                               P223
193700                      , :RS04-CTRETC                              P224
193800                      , :RS04-CDPAF                               P225
193900                      , :RS04-CTPAT                               P226
194000                      , :RS04-CQRER                               P227
194100                      , :RS04-CERERC                              P228
194200                      , :RS04-NOREG                               P229
194300                      , :RS04-CDREF                               P230
194400                      , :RS04-CDREG                               P231
194500                      , :RS04-LIRE30                              P232
194600                      , :RS04-DMRET                               P233
194700                      , :RS04-DDREN                               P234
194800                      , :RS04-MTREA                               P235
194900                      , :RS04-MORER                               P236
195000                      , :RS04-LIRER                               P237
195100                      , :RS04-MTPARV                              P238
195200                      , :RS04-CDRELK                              P239
195300                      , :RS04-MKREND                              P240
195400                      , :RS04-LIREK                               P241
195500                      , :RS04-CSREE                               P242
195600                      , :RS04-MTRER                               P243
195700                      , :RS04-DTPAID                              P244
195800                      , :RS04-DHPAID                              P245
195900                      , :RS04-DTPARP                              P246
196000                      , :RS04-LIPAOD                              P247
196100                      , :RS04-MCPAA                               P248
196200                      , :RS04-MCPAP                               P249
196300                      , :RS04-MKREP                               P250
196400                      , :RS04-CDRER                               P251
196500                      , :RS04-NOPAMD                              P252
196600                      , :RS04-NOPAID                              P253
196700                      , :RS04-MKREC                               P254
196800                      , :RS04-CEREK                               P255
196900                      , :RS04-DTPAIS                              P256
197000                      , :RS04-NODEP                               P257
197100                      , :RS04-CDREV                               P258
197200                      , :RS04-LIPA2                               P259
197300                      , :RS04-CDREK                               P260
197400                      , :RS04-DTREC1                              P261
197500                      , :RS04-CTREU                               P262
197600                      , :RS04-DFREL                               P263
197700                      , :RS04-LIR30A                              P264
197800                      , :RS04-CCGT1                               P265
197900                      , :RS04-CDGT                                P266
198000                      , :RS04-CRCGJ                               P267
198100                      , :RS04-CNCOB                               P268
198200                      , :RS04-CRCGE                               P269
198300                      , :RS04-CCREN                               P270
198400                      , :RS04-CRCGD                               P271
198500                      , :RS04-CCCON                               P272
198600                      , :RS04-CDSIN                               P273
198700                      , :RS04-CDEXCS                              P274
198800                      , :RS04-NOSINL                              P275
198900                      , :RS04-NOSINS                              P276
199000                      , :RS04-CCFRN                               P277
199100                      , :RS04-CCFRA                               P278
199200                      , :RS04-MTREB                               P279
199300                      , :RS04-LIFOE                               P280
199400                      , :RS04-NIREGS                              P281
199500                      , :RS04-CDRES3                              P282
199600                      , :RS04-NOCRA3                              P283
199700                      , :RS04-CDCAT                               P284
199800                      , :RS04-CDRVA                               P285
199900                      , :RS04-CDFISC                              P286
200000                      , :RS04-CDRESO                              P287
200100                      , :RS04-CCRER                               P288
200200                      , :RS04-CTRTCA                              P289
200300                   FROM  RS04                                     P300
200400                  WHERE                                           P310
200500                         NORER  = :C-0204-NORER                   P311
200600                    AND                                           P400
200700                         CNREN                                    P401
200800                     ||  IDRPL                                    P402
200900                      = ( SELECT MIN (                            P420
201000                         CNREN                                    P421
201100                     ||  IDRPL                                    P422
201200                                     )                            P440
201300                   FROM  RS04                                     P442
201400                  WHERE                                           P444
201500                         NORER  = :C-0204-NORER                   P445
201600                    AND (CNREN  > :C-0204-CNREN                   P446
201700                     OR (CNREN  = :C-0204-CNREN                   P447
201800                    AND (IDRPL  > :C-0204-IDRPL                   P448
201900                        )))                                       P449
202000                        )                              END-EXEC.  P475
202100     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
202200           IF    IK = ZERO                                        P485
202300     MOVE        '1' TO XOEE-RS04-CF                              P485
202400     ADD         1 TO XOEE-COUNT                                  P490
202500     MOVE        XP00-XROWID TO RS04-XROWID.                      P495
202600 F95-RS04-FV-FN. EXIT.                                            P499
202700 F95EE-FN. EXIT.                                                  P499
202800 F95I2.                                                           P000
202900           IF    XO00-XORATA = 'RS02'                             P005
203000     MOVE        D-0203 TO C-0203                                 P010
203100     MOVE        RS02-NORER TO C-0203-NORER.                      P015
203200           IF    XO00-XORATA = 'RS03'                             P020
203300     MOVE        D-0203 TO C-0203                                 P025
203400     MOVE        RS03-NORER TO C-0203-NORER                       P030
203500     MOVE        RS03-XCSEQ TO C-0203-XCSEQ.                      P035
203600           IF    XO00-XORATA = 'RS02'                             P040
203700     MOVE        D-0204 TO C-0204                                 P045
203800     MOVE        RS02-NORER TO C-0204-NORER.                      P050
203900           IF    XO00-XORATA = 'RS04'                             P055
204000     MOVE        D-0204 TO C-0204                                 P060
204100     MOVE        RS04-NORER TO C-0204-NORER                       P065
204200     MOVE        RS04-CNREN TO C-0204-CNREN                       P070
204300     MOVE        RS04-IDRPL TO C-0204-IDRPL.                      P075
204400 F95I2-FN. EXIT.                                                  P075
204500 F95I3.                                                           P000
204600           IF    XO00-XORATA = 'RS03'                             P005
204700     MOVE        C-0203-NORER TO RS03-NORER                       P010
204800     PERFORM     F95I4 THRU F95I4-FN                              P015
204900     MOVE        XP00-XCSEQ TO RS03-XCSEQ                         P020
205000     MOVE        XP00-XCSEQ TO RS03-XCSEQ.                        P040
205100 F95I3-FN. EXIT.                                                  P040
205200 F95I4.                                                           P000
205300     EXEC SQL    SELECT   RS_SET.NEXTVAL                          P100
205400                   INTO  :XP00-XCSEQ                              P110
205500                   FROM   DUAL                         END-EXEC.  P120
205600 F95I4-FN. EXIT.                                                  P120
205700 F9590.         EXIT.                                             P000
205800 F95-WORK-CN.                                                     P100
205900     MOVE        '9590' TO XO00-XCDFSF                            P110
206000     MOVE        'CONNECT' TO XO00-XORATY                         P120
206100     MOVE        SPACE TO XO00-XORATA                             P130
206200     MOVE        USERID TO XO00-XORACL                            P140
206300     EXEC SQL    CONNECT :USERID                       END-EXEC.  P150
206400 F95-WORK-CN-FN.                                                  P199
206500 F9590-FN. EXIT.                                                  P199
206600 F9591.         EXIT.                                             P000
206700 F95-WORK-RBS.                                                    P100
206800     MOVE        '9591' TO XO00-XCDFSF                            P110
206900     MOVE        'RB SEGMT' TO XO00-XORATY                        P120
207000     MOVE        SPACE TO XO00-XORATA                             P130
207100     MOVE        XO00-XORARB TO XO00-XORACL                       P140
207200     EXEC SQL    SET TRANSACTION USE                              P150
207300                 ROLLBACK SEGMENT :XO00-XORARB         END-EXEC.  P160
207400 F95-WORK-RBS-FN.                                                 P199
207500 F9591-FN. EXIT.                                                  P199
207600 F9592.         EXIT.                                             P000
207700 F95-WORK-CO.                                                     P100
207800     MOVE        '9592' TO XO00-XCDFSF                            P110
207900     MOVE        'COMMIT' TO XO00-XORATY                          P120
208000     MOVE        SPACE TO XO00-XORATA                             P130
208100     MOVE        SPACE TO XO00-XORACL                             P140
208200     EXEC SQL    COMMIT WORK                           END-EXEC.  P150
208300     MOVE        '1' TO XO00-XORACO.                              P155
208400           IF    XO00-XORARB NOT = SPACE                          P160
208500     PERFORM     F95-WORK-RBS THRU F95-WORK-RBS-FN.               P165
208600 F95-WORK-CO-FN.                                                  P199
208700 F9592-FN. EXIT.                                                  P199
208800 F9594.         EXIT.                                             P000
208900 F95-WORK-RO.                                                     P100
209000     MOVE        '9594' TO XO00-XCDFSF                            P110
209100     MOVE        'ROLLBACK' TO XO00-XORATY                        P120
209200     MOVE        SPACE TO XO00-XORATA                             P130
209300     MOVE        SPACE TO XO00-XORACL                             P140
209400     EXEC SQL    ROLLBACK WORK                         END-EXEC.  P150
209500 F95-WORK-RO-FN.                                                  P199
209600 F9594-FN. EXIT.                                                  P199
209700 F9596.         EXIT.                                             P000
209800 F95-WORK-RR.                                                     P100
209900     MOVE        '9596' TO XO00-XCDFSF                            P110
210000     MOVE        'RELEASE' TO XO00-XORATY                         P120
210100     MOVE        SPACE TO XO00-XORATA                             P130
210200     MOVE        SPACE TO XO00-XORACL                             P140
210300     EXEC SQL    ROLLBACK WORK RELEASE                 END-EXEC.  P150
210400 F95-WORK-RR-FN.                                                  P199
210500 F9596-FN. EXIT.                                                  P199
210600 F9598.         EXIT.                                             P000
210700 F95-WORK-TR.                                                     P100
210800     MOVE        '9598' TO XO00-XCDFSF                            P110
210900     MOVE        'TRACE' TO XO00-XORATY                           P120
211000     MOVE        SPACE TO XO00-XORATA                             P130
211100     MOVE        SPACE TO XO00-XORACL                             P140
211200     EXEC SQL    ALTER SESSION SET SQL_TRACE TRUE      END-EXEC.  P150
211300 F95-WORK-TR-FN.                                                  P199
211400 F9598-FN. EXIT.                                                  P199
211500 F9599.         EXIT.                                             P000
211600 F95-WORK-OK.                                                     P100
211700           IF    SQLCODE = ZERO                                   P110
211800     MOVE        ZERO TO IK                                       P110
211900     PERFORM     F95I2 THRU F95I2-FN                              P115
212000           ELSE                                                   P120
212100     MOVE        '1' TO IK.                                       P120
212200     MOVE        SQLCODE TO XO00-XORARC.                          P130
212300 F95-WORK-OK-FN.                                                  P199
212400 F9599-FN. EXIT.                                                  P199
212500 F98.           EXIT.                                             P000
212600 F98-D.         EXIT.                                             P000
212700 F98-H.                                                           P000
212800     DISPLAY     '------------------------------'                 P010
212900     '------------------------------'                             P020
213000     '-------------'                                              P030
213100     DISPLAY     '- Programme ' PROGE ' : '                       P100
213200     'LISTE '.                                                    P110
213300     DISPLAY     '-   version ' NUGNA ' generee '                 P200
213400     'le ' DATGN ' a ' TIMGN ' en '                               P210
213500     'bibliotheque ' APPLI.                                       P220
213600     DISPLAY     '-   debut : ' XAED-XDATRT ' '                   P300
213700     XAED-XHETRT.                                                 P310
213800     DISPLAY     '------------------------------'                 P500
213900     '------------------------------'                             P520
214000     '-------------'.                                             P530
214100 F98-H-FN. EXIT.                                                  P530
214200 F98-L.    IF    DATCE = XA30-ENVVAL                              P000
214300           NEXT SENTENCE ELSE GO TO     F98-L-FN.                 P000
214400     DISPLAY                      '- Attention ! La date du jour aP100
214500-                                 ' ete forcee par la variable GCAP110
214600-                '_DATCE'.                                        P120
214700     DISPLAY     '------------------------------'                 P500
214800     '------------------------------'                             P520
214900     '-------------'.                                             P530
215000 F98-L-FN. EXIT.                                                  P530
215100 F98-D-FN. EXIT.                                                  P530
215200 F98-Z.                                                           P000
215300     DISPLAY     '-'.                                             P010
215400 F98AA.                                                           P000
215500     MOVE        XOAA-COUNT TO XA80-XQNENR.                       P100
215600     DISPLAY     '- oracle F95AA ' XA80-XQNENR                    P200
215700     ' enregistrement  lu '                                       P210
215800     ' dans RS01'.                                                P220
215900 F98AA-FN. EXIT.                                                  P220
216000 F98BB.                                                           P000
216100     MOVE        XOBB-COUNT TO XA80-XQNENR.                       P100
216200     DISPLAY     '- oracle F95BB ' XA80-XQNENR                    P200
216300     ' enregistrements lus'                                       P210
216400     ' dans RS42'.                                                P220
216500 F98BB-FN. EXIT.                                                  P220
216600 F98CC.                                                           P000
216700     MOVE        XOCC-COUNT TO XA80-XQNENR.                       P100
216800     DISPLAY     '- oracle F95CC ' XA80-XQNENR                    P200
216900     ' enregistrements lus'                                       P210
217000     ' dans RS02'.                                                P220
217100 F98CC-FN. EXIT.                                                  P220
217200 F98DD.                                                           P000
217300     MOVE        XODD-COUNT TO XA80-XQNENR.                       P100
217400     DISPLAY     '- oracle F95DD ' XA80-XQNENR                    P200
217500     ' enregistrements lus'                                       P210
217600     ' dans RS03'.                                                P220
217700 F98DD-FN. EXIT.                                                  P220
217800 F98EE.                                                           P000
217900     MOVE        XOEE-COUNT TO XA80-XQNENR.                       P100
218000     DISPLAY     '- oracle F95EE ' XA80-XQNENR                    P200
218100     ' enregistrements lus'                                       P210
218200     ' dans RS04'.                                                P220
218300 F98EE-FN. EXIT.                                                  P220
218400 F98YX.                                                           P000
218500     MOVE        5-YX00-CPTENR TO XA80-XQNENR.                    P100
218600     DISPLAY     '- fichier ' 'YX   '                             P200
218700     XA80-XQNENR ' enregistrements'                               P210
218800     ' assignation: ' 'YX'                                        P220
218900     ' ouverture: ' 'O'.                                          P230
219000 F98YX-FN. EXIT.                                                  P230
219100 F9899.                                                           P000
219200     DISPLAY     '-     fin : ' XAED-XDATRT ' '                   P300
219300     XAED-XHETRT.                                                 P310
219400     DISPLAY     '------------------------------'                 P500
219500     '------------------------------'                             P520
219600     '-------------'.                                             P530
219700 F9899-FN. EXIT.                                                  P530
219800 F98-Z-FN. EXIT.                                                  P530
219900 F98-FN.   EXIT.                                                  P530
220000 F99OR.                                                           P000
220100     PERFORM     F0BBA THRU F0BBA-FN.                             P100
220200 F99OV.                                                           P000
220300     MOVE        001                      TO J99OVR.              P000
220400                              GO TO     F99OV-B.                  P000
220500 F99OV-A.                                                         P000
220600     ADD         1                        TO J99OVR.              P000
220700 F99OV-B.                                                         P000
220800     IF          IXO00M                   <  J99OVR               P000
220900                              GO TO     F99OV-FN.                 P000
221000           IF    XO00-XORAC1 (J99OVR) < SPACE                     P100
221100           OR    XO00-XORAC1 (J99OVR) > 'Z'                       P110
221200     MOVE        '.' TO XO00-XORAC1 (J99OVR).                     P100
221300 F99OV-900. GO TO F99OV-A.                                        P100
221400 F99OV-FN. EXIT.                                                  P100
221500 F99OZ.                                                           P000
221600     MOVE        '99OR' TO XA60-XCDFSF                            P100
221700     MOVE        'ERREUR ORACLE' TO XA60-XLISUI                   P110
221800     MOVE        XO00-XORAER TO XA60-ZX67A                        P130
221900     MOVE        XO00-XORAE2 TO XA60-ZX67B                        P140
222000     MOVE        SQLERRMC TO XA60-ZX67C.                          P150
222100     PERFORM     F9900 THRU F9900-FN.                             P210
222200 F99OZ-FN. EXIT.                                                  P210
222300 F99OR-FN. EXIT.                                                  P210
222400 F99SW.         EXIT.                                             P000
222500 F99SW-FN. EXIT.                                                  P000
222600 F99SX.         EXIT.                                             P000
222700 F99SX-FN. EXIT.                                                  P000
222800 F99VE.                                                           P000
222900     MOVE        ZERO TO IK                                       P100
223000     DISPLAY     XA30-ENVNAM                                      P110
223100     UPON ENVIRONMENT-NAME                                        P120
223200     ACCEPT      XA30-ENVVAL                                      P200
223300     FROM ENVIRONMENT-VALUE                                       P210
223400     ON EXCEPTION                                                 P220
223500     MOVE        '1' TO IK                                        P230
223600     MOVE        SPACE TO XA30-ENVVAL.                            P240
223700 F99VE-FN. EXIT.                                                  P240
223800 F99VI.                                                           P000
223900     MOVE        ZERO TO IK                                       P100
224000     DISPLAY     XA30-ENVNAM                                      P110
224100     UPON ENVIRONMENT-NAME                                        P120
224200     DISPLAY     XA30-ENVVAL                                      P200
224300     UPON ENVIRONMENT-VALUE                                       P210
224400     ON EXCEPTION                                                 P220
224500     MOVE        '1' TO IK.                                       P230
224600 F99VO.    IF    IK = ZERO                                        P000
224700           NEXT SENTENCE ELSE GO TO     F99VO-FN.                 P000
224800     MOVE        SPACE TO YX00                                    P100
224900     STRING      'setenv ' DELIMITED BY SIZE                      P110
225000     XA30-ENVNAM DELIMITED BY SPACE                               P115
225100     ' ' DELIMITED BY SIZE                                        P120
225200     XA30-ENVVAL DELIMITED BY SPACE                               P125
225300     INTO YX00 ON OVERFLOW                                        P130
225400     MOVE        '1' TO IK.                                       P150
225500     PERFORM     F90YX THRU F90YX-FN.                             P200
225600 F99VO-FN. EXIT.                                                  P200
225700 F99VI-FN. EXIT.                                                  P200
225800 F9900.         EXIT.                                             P000
225900 F9910.                                                           P000
226000     DISPLAY     '***'                                            P100
226100     DISPLAY     '***' XA60-L1                                    P200
226200     DISPLAY     '***' XA60-ZX67A.                                P300
226300           IF    XA60-ZX67B NOT = SPACE                           P400
226400     DISPLAY     '***' XA60-ZX67B.                                P400
226500           IF    XA60-ZX67C NOT = SPACE                           P500
226600     DISPLAY     '***' XA60-ZX67C                                 P500
226700     DISPLAY     '***'.                                           P600
226800 F9910-FN. EXIT.                                                  P600
226900 F9920.                                                           P000
227000     PERFORM     F98-Z THRU F98-Z-FN.                             P100
227100 F9950.    IF    XO00-XORACN NOT = ZERO                           P000
227200           NEXT SENTENCE ELSE GO TO     F9950-FN.                 P000
227300     MOVE        ZERO TO XO00-XORACN                              P100
227400     PERFORM     F95-WORK-RR THRU F95-WORK-RR-FN.                 P110
227500 F9950-FN. EXIT.                                                  P110
227600 F9980.                                                           P000
227700     MOVE        4 TO XA00-XRC.                                   P100
227800           IF    XO00-XORACO NOT = ZERO                           P500
227900     MOVE        8 TO XA00-XRC.                                   P500
228000 F9980-FN. EXIT.                                                  P500
228100 F9990.                                                           P000
228200     MOVE        XA00-XRC TO RETURN-CODE                          P100
228300     STOP RUN.                                                    P500
228400 F9990-FN. EXIT.                                                  P500
228500 F9920-FN. EXIT.                                                  P500
228600 F9900-FN. EXIT.                                                  P500
228700 F9999.                                                           P000
228800     DISPLAY     '******* ABORT PROVOQUE *******'                 P100
228900     ' ' XA60-XCDFSF                                              P150
229000     PERFORM     F9920 THRU F9920-FN.                             P200
229100 F9999-FN. EXIT.                                                  P200
