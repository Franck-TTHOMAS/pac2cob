000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.  RSA030.                                             RSA030
000300 ENVIRONMENT DIVISION.                                            RSA030
000400 CONFIGURATION SECTION.                                           RSA030
000500 SOURCE-COMPUTER. PC-MICROFOCUS.                                  RSA030
000600 OBJECT-COMPUTER. PC-MICROFOCUS.                                  RSA030
000700 SPECIAL-NAMES.                                                   RSA030
000800      C01 IS SAUTP                                                RSA030
000900      CSP IS SAUT0                                                RSA030
001000      DECIMAL-POINT IS COMMA.                                     RSA030
001100 INPUT-OUTPUT SECTION.                                            RSA030
001200 FILE-CONTROL.                                                    RSA030
001300      SELECT     EW-FICHIER    ASSIGN      AS-EW                  RSA030
001400      FILE STATUS IS                   1-EW00-STATUS              RSA030
001500                                       VSAM-STATUS.               RSA030
001600      SELECT YX-FICHIER      ASSIGN AS-SOYX                       D01YX
001700             ORGANIZATION    LINE SEQUENTIAL                      D01YX
001800             FILE STATUS     1-YX00-STATUS.                       D01YX
001900 DATA DIVISION.                                                   RSA030
002000 FILE SECTION.                                                    RSA030
002100 FD                 EW-FICHIER                                    RSA030
002200      BLOCK              00000 RECORDS.                           RSA030
002300 01                 EW00.                                         RSA030
002400      10       FILLER         PICTURE  X(133).                    RSA030
002500 FD                 YX-FICHIER                                    RSA030
002600      BLOCK              00000 RECORDS.                           RSA030
002700 01                 YX00.                                         RSA030
002800      10            YX00-ZX80   PICTURE  X(80).                   RSA030
002900 WORKING-STORAGE SECTION.                                         RSA030
003000          EXEC SQL INCLUDE SQLCA         END-EXEC.                7AAAAA
003100          EXEC SQL BEGIN DECLARE SECTION END-EXEC.                7RS999
003200 01                 RS01.                                         RSA030
003300      10            RS01-NONER  PICTURE  XX.                      RSA030
003400      10            RS01-NORERD PICTURE  S9(6)                    RSA030
003500                    COMPUTATIONAL-3.                              RSA030
003600      10            RS01-ZREPR  PICTURE  X(8).                    RSA030
003700      10            RS01-NOELL  PICTURE  S999                     RSA030
003800                    COMPUTATIONAL-3.                              RSA030
003900      10            RS01-DIELL  PICTURE  X(8).                    RSA030
004000 01                 RS02.                                         RSA030
004100      10            RS02-NORER  PICTURE  X(6).                    RSA030
004200      10            RS02-CDRES  PICTURE  X.                       RSA030
004300      10            RS02-LNRENR PICTURE  X(20).                   RSA030
004400      10            RS02-LNREPR PICTURE  X(12).                   RSA030
004500      10            RS02-DTRENR PICTURE  X(8).                    RSA030
004600      10            RS02-LNRENF PICTURE  X(20).                   RSA030
004700      10            RS02-CDRECR PICTURE  X(3).                    RSA030
004800      10            RS02-CDREXR PICTURE  X.                       RSA030
004900      10            RS02-CTREF  PICTURE  XX.                      RSA030
005000      10            RS02-CEREF  PICTURE  X.                       RSA030
005100      10            RS02-DBREF  PICTURE  X(8).                    RSA030
005200      10            RS02-DMREN  PICTURE  X(8).                    RSA030
005300 01                 RS03.                                         RSA030
005400      10            RS03-NORER  PICTURE  X(6).                    RSA030
005500      10            RS03-XCSEQ  PICTURE  S9(9)                    RSA030
005600                    COMPUTATIONAL-3.                              RSA030
005700      10            RS03-LRRE1R PICTURE  X(36).                   RSA030
005800      10            RS03-LRRE2R PICTURE  X(32).                   RSA030
005900      10            RS03-LVRER  PICTURE  X(31).                   RSA030
006000      10            RS03-CPRENR PICTURE  X(5).                    RSA030
006100      10            RS03-CDREPR PICTURE  XXX.                     RSA030
006200      10            RS03-DMREN  PICTURE  X(8).                    RSA030
006300 01                 RS04.                                         RSA030
006400      10            RS04-NOREN.                                   RSA030
006500      11            RS04-CNREN  PICTURE  XX.                      RSA030
006600      11            RS04-NORER  PICTURE  X(6).                    RSA030
006700      11            RS04-IDRPL  PICTURE  X(5).                    RSA030
006800      11            RS04-IDRPLG                                   RSA030
006900                    REDEFINES            RS04-IDRPL.              RSA030
007000      12            RS04-NIREG  PICTURE  XX.                      RSA030
007100      12            RS04-CDRETP PICTURE  X.                       RSA030
007200      12            RS04-NIPOL  PICTURE  XX.                      RSA030
007300      10            RS04-CDRES  PICTURE  X.                       RSA030
007400      10            RS04-NOCRA  PICTURE  XX.                      RSA030
007500      10            RS04-NOTIE  PICTURE  X(6).                    RSA030
007600      10            RS04-CDPAD  PICTURE  X.                       RSA030
007700      10            RS04-CDREA  PICTURE  X.                       RSA030
007800      10            RS04-DMREA  PICTURE  X(8).                    RSA030
007900      10            RS04-DFREN  PICTURE  X(8).                    RSA030
008000      10            RS04-NOSIN  PICTURE  X(10).                   RSA030
008100      10            RS04-DTREAC PICTURE  X(8).                    RSA030
008200      10            RS04-DTREC  PICTURE  X(8).                    RSA030
008300      10            RS04-CTRETC PICTURE  X(8).                    RSA030
008400      10            RS04-CDPAF  PICTURE  X.                       RSA030
008500      10            RS04-CTPAT  PICTURE  X.                       RSA030
008600      10            RS04-CQRER  PICTURE  X.                       RSA030
008700      10            RS04-CERERC PICTURE  X.                       RSA030
008800      10            RS04-NOREG  PICTURE  XXX.                     RSA030
008900      10            RS04-CDREF  PICTURE  X.                       RSA030
009000      10            RS04-CDREG  PICTURE  X.                       RSA030
009100      10            RS04-LIRE30 PICTURE  X(30).                   RSA030
009200      10            RS04-DMRET  PICTURE  X(8).                    RSA030
009300      10            RS04-DDREN  PICTURE  X(8).                    RSA030
009400      10            RS04-MTREA  PICTURE  S9(09)V99                RSA030
009500                    COMPUTATIONAL-3.                              RSA030
009600      10            RS04-MORER  PICTURE  S9(09)V99                RSA030
009700                    COMPUTATIONAL-3.                              RSA030
009800      10            RS04-LIRER  PICTURE  X(30).                   RSA030
009900      10            RS04-MTPARV PICTURE  S9(09)V99                RSA030
010000                    COMPUTATIONAL-3.                              RSA030
010100      10            RS04-CDRELK PICTURE  X(3).                    RSA030
010200      10            RS04-MKREND PICTURE  S9(09)V99                RSA030
010300                    COMPUTATIONAL-3.                              RSA030
010400      10            RS04-LIREK  PICTURE  X(30).                   RSA030
010500      10            RS04-CSREE  PICTURE  X.                       RSA030
010600      10            RS04-MTRER  PICTURE  S9(09)V99                RSA030
010700                    COMPUTATIONAL-3.                              RSA030
010800      10            RS04-DTPAID PICTURE  X(8).                    RSA030
010900      10            RS04-DHPAID PICTURE  X(8).                    RSA030
011000      10            RS04-DTPARP PICTURE  X(8).                    RSA030
011100      10            RS04-LIPAOD PICTURE  X(31).                   RSA030
011200      10            RS04-MCPAA  PICTURE  S9(13)V99                RSA030
011300                    COMPUTATIONAL-3.                              RSA030
011400      10            RS04-MCPAP  PICTURE  S9(13)V99                RSA030
011500                    COMPUTATIONAL-3.                              RSA030
011600      10            RS04-MKREP  PICTURE  S9(09)V99                RSA030
011700                    COMPUTATIONAL-3.                              RSA030
011800      10            RS04-CDRER  PICTURE  X.                       RSA030
011900      10            RS04-NOPAMD PICTURE  S99                      RSA030
012000                    COMPUTATIONAL-3.                              RSA030
012100      10            RS04-NOPAID PICTURE  S999                     RSA030
012200                    COMPUTATIONAL-3.                              RSA030
012300      10            RS04-MKREC  PICTURE  S9(09)V99                RSA030
012400                    COMPUTATIONAL-3.                              RSA030
012500      10            RS04-CEREK  PICTURE  X.                       RSA030
012600      10            RS04-DTPAIS PICTURE  X(8).                    RSA030
012700      10            RS04-NODEP  PICTURE  XX.                      RSA030
012800      10            RS04-CDREV  PICTURE  X.                       RSA030
012900      10            RS04-LIPA2  PICTURE  X(31).                   RSA030
013000      10            RS04-CDREK  PICTURE  XXX.                     RSA030
013100      10            RS04-DTREC1 PICTURE  X(8).                    RSA030
013200      10            RS04-CTREU  PICTURE  X.                       RSA030
013300      10            RS04-DFREL  PICTURE  X(8).                    RSA030
013400      10            RS04-LIR30A PICTURE  X(30).                   RSA030
013500      10            RS04-CCGT1  PICTURE  X.                       RSA030
013600      10            RS04-CDGT   PICTURE  X(5).                    RSA030
013700      10            RS04-CRCGJ  PICTURE  X.                       RSA030
013800      10            RS04-CNCOB  PICTURE  XXX.                     RSA030
013900      10            RS04-CRCGE  PICTURE  X.                       RSA030
014000      10            RS04-CCREN  PICTURE  XXX.                     RSA030
014100      10            RS04-CRCGD  PICTURE  X.                       RSA030
014200      10            RS04-CCCON  PICTURE  XX.                      RSA030
014300      10            RS04-CDSIN  PICTURE  X.                       RSA030
014400      10            RS04-CDEXCS PICTURE  X.                       RSA030
014500      10            RS04-NOSINL PICTURE  X(10).                   RSA030
014600      10            RS04-NOSINS PICTURE  X(10).                   RSA030
014700      10            RS04-CCFRN  PICTURE  X.                       RSA030
014800      10            RS04-CCFRA  PICTURE  X.                       RSA030
014900      10            RS04-MTREB  PICTURE  S9(09)V99                RSA030
015000                    COMPUTATIONAL-3.                              RSA030
015100      10            RS04-LIFOE  PICTURE  X(10).                   RSA030
015200      10            RS04-NIREGS PICTURE  XX.                      RSA030
015300      10            RS04-CDRES3 PICTURE  X.                       RSA030
015400      10            RS04-NOCRA3 PICTURE  XX.                      RSA030
015500      10            RS04-CDCAT  PICTURE  X.                       RSA030
015600      10            RS04-CDRVA  PICTURE  X.                       RSA030
015700      10            RS04-CDFISC PICTURE  X.                       RSA030
015800      10            RS04-CDRESO PICTURE  X.                       RSA030
015900      10            RS04-CCRER  PICTURE  XXX.                     RSA030
016000      10            RS04-CTRTCA PICTURE  X(8).                    RSA030
016100      10            RS04-CDTV   PICTURE  S999                     RSA030
016200                    COMPUTATIONAL-3.                              RSA030
016300      10            RS04-REFAV  PICTURE  X(14).                   RSA030
016400      10            RS04-CTRTPM PICTURE  X(8).                    RSA030
016500      10            RS04-INDRVA PICTURE  XX.                      RSA030
016600 01                 RS05.                                         RSA030
016700      10            RS05-CNREN  PICTURE  XX.                      RSA030
016800      10            RS05-NORER  PICTURE  X(6).                    RSA030
016900      10            RS05-IDRPL  PICTURE  X(5).                    RSA030
017000      10            RS05-IDRPLG                                   RSA030
017100                    REDEFINES            RS05-IDRPL.              RSA030
017200      11            RS05-NIREG  PICTURE  XX.                      RSA030
017300      11            RS05-CDRETP PICTURE  X.                       RSA030
017400      11            RS05-NIPOL  PICTURE  XX.                      RSA030
017500      10            RS05-NOPAM  PICTURE  S99                      RSA030
017600                    COMPUTATIONAL-3.                              RSA030
017700      10            RS05-DMRENB PICTURE  X(8).                    RSA030
017800      10            RS05-NCRENR PICTURE  X(24).                   RSA030
017900      10            RS05-NCREER PICTURE  X(5).                    RSA030
018000      10            RS05-NCREGR PICTURE  X(5).                    RSA030
018100      10            RS05-NCREBR PICTURE  X(11).                   RSA030
018200      10            RS05-NCRECR PICTURE  XXX.                     RSA030
018300      10            RS05-CDPAMR PICTURE  X.                       RSA030
018400 01                 RS06.                                         RSA030
018500      10            RS06-CNREN  PICTURE  XX.                      RSA030
018600      10            RS06-NORER  PICTURE  X(6).                    RSA030
018700      10            RS06-IDRPL  PICTURE  X(5).                    RSA030
018800      10            RS06-IDRPLG                                   RSA030
018900                    REDEFINES            RS06-IDRPL.              RSA030
019000      11            RS06-NIREG  PICTURE  XX.                      RSA030
019100      11            RS06-CDRETP PICTURE  X.                       RSA030
019200      11            RS06-NIPOL  PICTURE  XX.                      RSA030
019300      10            RS06-XCSEQ  PICTURE  S9(9)                    RSA030
019400                    COMPUTATIONAL-3.                              RSA030
019500      10            RS06-DMREN  PICTURE  X(8).                    RSA030
019600      10            RS06-DDREN  PICTURE  X(8).                    RSA030
019700      10            RS06-MTREA  PICTURE  S9(09)V99                RSA030
019800                    COMPUTATIONAL-3.                              RSA030
019900      10            RS06-DDREJ  PICTURE  X(8).                    RSA030
020000      10            RS06-DTREC  PICTURE  X(8).                    RSA030
020100      10            RS06-TXREI  PICTURE  S9999V99                 RSA030
020200                    COMPUTATIONAL-3.                              RSA030
020300      10            RS06-MTREAO PICTURE  S9(09)V99                RSA030
020400                    COMPUTATIONAL-3.                              RSA030
020500      10            RS06-BTREAT PICTURE  S9(09)V99                RSA030
020600                    COMPUTATIONAL-3.                              RSA030
020700      10            RS06-TXREAS PICTURE  S99V99                   RSA030
020800                    COMPUTATIONAL-3.                              RSA030
020900 01                 RS07.                                         RSA030
021000      10            RS07-CNREN  PICTURE  XX.                      RSA030
021100      10            RS07-NORER  PICTURE  X(6).                    RSA030
021200      10            RS07-IDRPL  PICTURE  X(5).                    RSA030
021300      10            RS07-IDRPLG                                   RSA030
021400                    REDEFINES            RS07-IDRPL.              RSA030
021500      11            RS07-NIREG  PICTURE  XX.                      RSA030
021600      11            RS07-CDRETP PICTURE  X.                       RSA030
021700      11            RS07-NIPOL  PICTURE  XX.                      RSA030
021800      10            RS07-XCSEQ  PICTURE  S9(9)                    RSA030
021900                    COMPUTATIONAL-3.                              RSA030
022000      10            RS07-DMREN  PICTURE  X(8).                    RSA030
022100      10            RS07-MKREN  PICTURE  S9(09)V99                RSA030
022200                    COMPUTATIONAL-3.                              RSA030
022300      10            RS07-LIREK  PICTURE  X(30).                   RSA030
022400      10            RS07-CSREE  PICTURE  X.                       RSA030
022500      10            RS07-MOREC  PICTURE  S9(09)V99                RSA030
022600                    COMPUTATIONAL-3.                              RSA030
022700      10            RS07-TXCAD  PICTURE  S9(5)V9(5)               RSA030
022800                    COMPUTATIONAL-3.                              RSA030
022900      10            RS07-CDTIPS PICTURE  X(6).                    RSA030
023000      10            RS07-CDREGR PICTURE  XX.                      RSA030
023100 01                 RS08.                                         RSA030
023200      10            RS08-CNREN  PICTURE  XX.                      RSA030
023300      10            RS08-NORER  PICTURE  X(6).                    RSA030
023400      10            RS08-IDRPL  PICTURE  X(5).                    RSA030
023500      10            RS08-IDRPLG                                   RSA030
023600                    REDEFINES            RS08-IDRPL.              RSA030
023700      11            RS08-NIREG  PICTURE  XX.                      RSA030
023800      11            RS08-CDRETP PICTURE  X.                       RSA030
023900      11            RS08-NIPOL  PICTURE  XX.                      RSA030
024000      10            RS08-DDREJ  PICTURE  X(8).                    RSA030
024100      10            RS08-CDREJ  PICTURE  XXX.                     RSA030
024200      10            RS08-CTRER  PICTURE  X.                       RSA030
024300      10            RS08-CTREA  PICTURE  X.                       RSA030
024400      10            RS08-TXREI  PICTURE  S9999V99                 RSA030
024500                    COMPUTATIONAL-3.                              RSA030
024600      10            RS08-MTRES  PICTURE  S9(09)V99                RSA030
024700                    COMPUTATIONAL-3.                              RSA030
024800      10            RS08-MOREC  PICTURE  S9(09)V99                RSA030
024900                    COMPUTATIONAL-3.                              RSA030
025000      10            RS08-CERETP PICTURE  X.                       RSA030
025100 01                 RS09.                                         RSA030
025200      10            RS09-CNREN  PICTURE  XX.                      RSA030
025300      10            RS09-NORER  PICTURE  X(6).                    RSA030
025400      10            RS09-IDRPL  PICTURE  X(5).                    RSA030
025500      10            RS09-IDRPLG                                   RSA030
025600                    REDEFINES            RS09-IDRPL.              RSA030
025700      11            RS09-NIREG  PICTURE  XX.                      RSA030
025800      11            RS09-CDRETP PICTURE  X.                       RSA030
025900      11            RS09-NIPOL  PICTURE  XX.                      RSA030
026000      10            RS09-TXREO  PICTURE  S9999V99                 RSA030
026100                    COMPUTATIONAL-3.                              RSA030
026200      10            RS09-CEREO  PICTURE  X.                       RSA030
026300      10            RS09-MOREC  PICTURE  S9(09)V99                RSA030
026400                    COMPUTATIONAL-3.                              RSA030
026500      10            RS09-LNRENO PICTURE  X(20).                   RSA030
026600      10            RS09-LNREPO PICTURE  X(12).                   RSA030
026700      10            RS09-CDREXO PICTURE  X.                       RSA030
026800      10            RS09-DTRENO PICTURE  X(8).                    RSA030
026900      10            RS09-NORENR PICTURE  X(13).                   RSA030
027000 01                 RS10.                                         RSA030
027100      10            RS10-CNREN  PICTURE  XX.                      RSA030
027200      10            RS10-NORER  PICTURE  X(6).                    RSA030
027300      10            RS10-IDRPL  PICTURE  X(5).                    RSA030
027400      10            RS10-IDRPLG                                   RSA030
027500                    REDEFINES            RS10-IDRPL.              RSA030
027600      11            RS10-NIREG  PICTURE  XX.                      RSA030
027700      11            RS10-CDRETP PICTURE  X.                       RSA030
027800      11            RS10-NIPOL  PICTURE  XX.                      RSA030
027900      10            RS10-CTRER  PICTURE  X.                       RSA030
028000      10            RS10-CTREA  PICTURE  X.                       RSA030
028100      10            RS10-MOREC  PICTURE  S9(09)V99                RSA030
028200                    COMPUTATIONAL-3.                              RSA030
028300      10            RS10-CTREEX PICTURE  X.                       RSA030
028400      10            RS10-DTREO  PICTURE  X(8).                    RSA030
028500      10            RS10-MTPAC  PICTURE  S9(09)V99                RSA030
028600                    COMPUTATIONAL-3.                              RSA030
028700 01                 RS11.                                         RSA030
028800      10            RS11-CNREN  PICTURE  XX.                      RSA030
028900      10            RS11-NORER  PICTURE  X(6).                    RSA030
029000      10            RS11-IDRPL  PICTURE  X(5).                    RSA030
029100      10            RS11-IDRPLG                                   RSA030
029200                    REDEFINES            RS11-IDRPL.              RSA030
029300      11            RS11-NIREG  PICTURE  XX.                      RSA030
029400      11            RS11-CDRETP PICTURE  X.                       RSA030
029500      11            RS11-NIPOL  PICTURE  XX.                      RSA030
029600      10            RS11-CTRER  PICTURE  X.                       RSA030
029700      10            RS11-CTREA  PICTURE  X.                       RSA030
029800      10            RS11-LIPOT  PICTURE  X(24).                   RSA030
029900      10            RS11-LIPOTB PICTURE  X(24).                   RSA030
030000      10            RS11-TXPOO  PICTURE  S999                     RSA030
030100                    COMPUTATIONAL-3.                              RSA030
030200      10            RS11-MTPOO  PICTURE  S9(09)V99                RSA030
030300                    COMPUTATIONAL-3.                              RSA030
030400      10            RS11-MTRECR PICTURE  S9(09)V99                RSA030
030500                    COMPUTATIONAL-3.                              RSA030
030600      10            RS11-CDTIPS PICTURE  X(6).                    RSA030
030700      10            RS11-TXCAD  PICTURE  S9(5)V9(5)               RSA030
030800                    COMPUTATIONAL-3.                              RSA030
030900      10            RS11-CDREGR PICTURE  XX.                      RSA030
031000      10            RS11-TXREI  PICTURE  S9999V99                 RSA030
031100                    COMPUTATIONAL-3.                              RSA030
031200 01                 RS12.                                         RSA030
031300      10            RS12-CNREN  PICTURE  XX.                      RSA030
031400      10            RS12-NORER  PICTURE  X(6).                    RSA030
031500      10            RS12-IDRPL  PICTURE  X(5).                    RSA030
031600      10            RS12-IDRPLG                                   RSA030
031700                    REDEFINES            RS12-IDRPL.              RSA030
031800      11            RS12-NIREG  PICTURE  XX.                      RSA030
031900      11            RS12-CDRETP PICTURE  X.                       RSA030
032000      11            RS12-NIPOL  PICTURE  XX.                      RSA030
032100      10            RS12-DDREJ  PICTURE  X(8).                    RSA030
032200      10            RS12-CDREJ  PICTURE  XXX.                     RSA030
032300      10            RS12-MOREC  PICTURE  S9(09)V99                RSA030
032400                    COMPUTATIONAL-3.                              RSA030
032500      10            RS12-DTREO  PICTURE  X(8).                    RSA030
032600      10            RS12-MTPAC  PICTURE  S9(09)V99                RSA030
032700                    COMPUTATIONAL-3.                              RSA030
032800      10            RS12-LNRENS PICTURE  X(20).                   RSA030
032900      10            RS12-NORES  PICTURE  X(6).                    RSA030
033000      10            RS12-LNRENA PICTURE  X(20).                   RSA030
033100      10            RS12-CTRESS PICTURE  X.                       RSA030
033200      10            RS12-TXREI  PICTURE  S9999V99                 RSA030
033300                    COMPUTATIONAL-3.                              RSA030
033400      10            RS12-CNREK  PICTURE  XXX.                     RSA030
033500      10            RS12-MKREL  PICTURE  S9(09)V99                RSA030
033600                    COMPUTATIONAL-3.                              RSA030
033700      10            RS12-CCREM  PICTURE  X.                       RSA030
033800      10            RS12-CDREB  PICTURE  X.                       RSA030
033900      10            RS12-CDREPS PICTURE  XXX.                     RSA030
034000      10            RS12-CNREOS PICTURE  X.                       RSA030
034100      10            RS12-CTREA  PICTURE  X.                       RSA030
034200 01                 RS13.                                         RSA030
034300      10            RS13-CNREN  PICTURE  XX.                      RSA030
034400      10            RS13-NORER  PICTURE  X(6).                    RSA030
034500      10            RS13-IDRPL  PICTURE  X(5).                    RSA030
034600      10            RS13-IDRPLG                                   RSA030
034700                    REDEFINES            RS13-IDRPL.              RSA030
034800      11            RS13-NIREG  PICTURE  XX.                      RSA030
034900      11            RS13-CDRETP PICTURE  X.                       RSA030
035000      11            RS13-NIPOL  PICTURE  XX.                      RSA030
035100      10            RS13-DDREJ  PICTURE  X(8).                    RSA030
035200      10            RS13-CDREJ  PICTURE  XXX.                     RSA030
035300      10            RS13-MOREC  PICTURE  S9(09)V99                RSA030
035400                    COMPUTATIONAL-3.                              RSA030
035500      10            RS13-DTREO  PICTURE  X(8).                    RSA030
035600      10            RS13-MTPAC  PICTURE  S9(09)V99                RSA030
035700                    COMPUTATIONAL-3.                              RSA030
035800      10            RS13-LNRENS PICTURE  X(20).                   RSA030
035900      10            RS13-NORES  PICTURE  X(6).                    RSA030
036000      10            RS13-LNRENA PICTURE  X(20).                   RSA030
036100      10            RS13-CTREDC PICTURE  X.                       RSA030
036200      10            RS13-CTREJ  PICTURE  X.                       RSA030
036300      10            RS13-TXREI  PICTURE  S9999V99                 RSA030
036400                    COMPUTATIONAL-3.                              RSA030
036500      10            RS13-MTREAO PICTURE  S9(09)V99                RSA030
036600                    COMPUTATIONAL-3.                              RSA030
036700      10            RS13-KMREDC PICTURE  S9999V9(5)               RSA030
036800                    COMPUTATIONAL-3.                              RSA030
036900      10            RS13-CNREK  PICTURE  XXX.                     RSA030
037000      10            RS13-MKREL  PICTURE  S9(09)V99                RSA030
037100                    COMPUTATIONAL-3.                              RSA030
037200      10            RS13-CCREM  PICTURE  X.                       RSA030
037300      10            RS13-CDREB  PICTURE  X.                       RSA030
037400      10            RS13-CDREAT PICTURE  X.                       RSA030
037500      10            RS13-DTRENV PICTURE  X(8).                    RSA030
037600      10            RS13-CDREX  PICTURE  X.                       RSA030
037700 01                 RS14.                                         RSA030
037800      10            RS14-CNREN  PICTURE  XX.                      RSA030
037900      10            RS14-NORER  PICTURE  X(6).                    RSA030
038000      10            RS14-IDRPL  PICTURE  X(5).                    RSA030
038100      10            RS14-IDRPLG                                   RSA030
038200                    REDEFINES            RS14-IDRPL.              RSA030
038300      11            RS14-NIREG  PICTURE  XX.                      RSA030
038400      11            RS14-CDRETP PICTURE  X.                       RSA030
038500      11            RS14-NIPOL  PICTURE  XX.                      RSA030
038600      10            RS14-NOCRM  PICTURE  XX.                      RSA030
038700      10            RS14-NOREMS PICTURE  X(15).                   RSA030
038800      10            RS14-CDRETX PICTURE  XXX.                     RSA030
038900      10            RS14-TXREAS PICTURE  S99V99                   RSA030
039000                    COMPUTATIONAL-3.                              RSA030
039100      10            RS14-NORENR PICTURE  X(13).                   RSA030
039200      10            RS14-BTREAT PICTURE  S9(09)V99                RSA030
039300                    COMPUTATIONAL-3.                              RSA030
039400      10            RS14-TXREI  PICTURE  S9999V99                 RSA030
039500                    COMPUTATIONAL-3.                              RSA030
039600 01                 RS15.                                         RSA030
039700      10            RS15-CNREN  PICTURE  XX.                      RSA030
039800      10            RS15-NORER  PICTURE  X(6).                    RSA030
039900      10            RS15-IDRPL  PICTURE  X(5).                    RSA030
040000      10            RS15-IDRPLG                                   RSA030
040100                    REDEFINES            RS15-IDRPL.              RSA030
040200      11            RS15-NIREG  PICTURE  XX.                      RSA030
040300      11            RS15-CDRETP PICTURE  X.                       RSA030
040400      11            RS15-NIPOL  PICTURE  XX.                      RSA030
040500      10            RS15-PEREA  PICTURE  S9(4)                    RSA030
040600                    COMPUTATIONAL-3.                              RSA030
040700      10            RS15-MTRECR PICTURE  S9(09)V99                RSA030
040800                    COMPUTATIONAL-3.                              RSA030
040900      10            RS15-NORENR PICTURE  X(13).                   RSA030
041000      10            RS15-TXREI  PICTURE  S9999V99                 RSA030
041100                    COMPUTATIONAL-3.                              RSA030
041200 01                 RS16.                                         RSA030
041300      10            RS16-CNREN  PICTURE  XX.                      RSA030
041400      10            RS16-NORER  PICTURE  X(6).                    RSA030
041500      10            RS16-IDRPL  PICTURE  X(5).                    RSA030
041600      10            RS16-IDRPLG                                   RSA030
041700                    REDEFINES            RS16-IDRPL.              RSA030
041800      11            RS16-NIREG  PICTURE  XX.                      RSA030
041900      11            RS16-CDRETP PICTURE  X.                       RSA030
042000      11            RS16-NIPOL  PICTURE  XX.                      RSA030
042100      10            RS16-XCSEQ  PICTURE  S9(9)                    RSA030
042200                    COMPUTATIONAL-3.                              RSA030
042300      10            RS16-DMREN  PICTURE  X(8).                    RSA030
042400      10            RS16-NOTIE  PICTURE  X(6).                    RSA030
042500 01                 RS17.                                         RSA030
042600      10            RS17-CNREN  PICTURE  XX.                      RSA030
042700      10            RS17-NORER  PICTURE  X(6).                    RSA030
042800      10            RS17-IDRPL  PICTURE  X(5).                    RSA030
042900      10            RS17-IDRPLG                                   RSA030
043000                    REDEFINES            RS17-IDRPL.              RSA030
043100      11            RS17-NIREG  PICTURE  XX.                      RSA030
043200      11            RS17-CDRETP PICTURE  X.                       RSA030
043300      11            RS17-NIPOL  PICTURE  XX.                      RSA030
043400      10            RS17-NORCS  PICTURE  S99                      RSA030
043500                    COMPUTATIONAL-3.                              RSA030
043600      10            RS17-DMREN  PICTURE  X(8).                    RSA030
043700      10            RS17-LIRCS  PICTURE  X(40).                   RSA030
043800      10            RS17-LIRCSB PICTURE  X(40).                   RSA030
043900      10            RS17-CERCS  PICTURE  X.                       RSA030
044000      10            RS17-DDRCS  PICTURE  X(8).                    RSA030
044100      10            RS17-CTRCK  PICTURE  X.                       RSA030
044200      10            RS17-MKRCL  PICTURE  S9(09)V99                RSA030
044300                    COMPUTATIONAL-3.                              RSA030
044400      10            RS17-DHRCS  PICTURE  X(8).                    RSA030
044500      10            RS17-MORCS  PICTURE  S9(09)V99                RSA030
044600                    COMPUTATIONAL-3.                              RSA030
044700      10            RS17-CTRCB  PICTURE  X.                       RSA030
044800      10            RS17-MTRCA  PICTURE  S9(09)V99                RSA030
044900                    COMPUTATIONAL-3.                              RSA030
045000      10            RS17-MTRCG  PICTURE  S9(09)V99                RSA030
045100                    COMPUTATIONAL-3.                              RSA030
045200      10            RS17-MCRCCR PICTURE  S9(9)V99                 RSA030
045300                    COMPUTATIONAL-3.                              RSA030
045400      10            RS17-LNRCA  PICTURE  X(32).                   RSA030
045500      10            RS17-LRRE1  PICTURE  X(36).                   RSA030
045600      10            RS17-LRRE2  PICTURE  X(32).                   RSA030
045700      10            RS17-LVRE   PICTURE  X(31).                   RSA030
045800      10            RS17-CPREN  PICTURE  X(5).                    RSA030
045900      10            RS17-CDREP  PICTURE  XXX.                     RSA030
046000      10            RS17-MTRCB  PICTURE  S9(09)V99                RSA030
046100                    COMPUTATIONAL-3.                              RSA030
046200      10            RS17-DTRCB  PICTURE  X(8).                    RSA030
046300 01                 RS18.                                         RSA030
046400      10            RS18-CNREN  PICTURE  XX.                      RSA030
046500      10            RS18-NORER  PICTURE  X(6).                    RSA030
046600      10            RS18-IDRPL  PICTURE  X(5).                    RSA030
046700      10            RS18-IDRPLG                                   RSA030
046800                    REDEFINES            RS18-IDRPL.              RSA030
046900      11            RS18-NIREG  PICTURE  XX.                      RSA030
047000      11            RS18-CDRETP PICTURE  X.                       RSA030
047100      11            RS18-NIPOL  PICTURE  XX.                      RSA030
047200      10            RS18-NORCS  PICTURE  S99                      RSA030
047300                    COMPUTATIONAL-3.                              RSA030
047400      10            RS18-XCSEQ  PICTURE  S9(9)                    RSA030
047500                    COMPUTATIONAL-3.                              RSA030
047600      10            RS18-DTRCB  PICTURE  X(8).                    RSA030
047700      10            RS18-MTRCBH PICTURE  S9(09)V99                RSA030
047800                    COMPUTATIONAL-3.                              RSA030
047900      10            RS18-DMREN  PICTURE  X(8).                    RSA030
048000 01                 RS19.                                         RSA030
048100      10            RS19-CNREN  PICTURE  XX.                      RSA030
048200      10            RS19-NORER  PICTURE  X(6).                    RSA030
048300      10            RS19-IDRPL  PICTURE  X(5).                    RSA030
048400      10            RS19-IDRPLG                                   RSA030
048500                    REDEFINES            RS19-IDRPL.              RSA030
048600      11            RS19-NIREG  PICTURE  XX.                      RSA030
048700      11            RS19-CDRETP PICTURE  X.                       RSA030
048800      11            RS19-NIPOL  PICTURE  XX.                      RSA030
048900      10            RS19-NOPAI  PICTURE  S999                     RSA030
049000                    COMPUTATIONAL-3.                              RSA030
049100      10            RS19-DHPAI  PICTURE  X(8).                    RSA030
049200      10            RS19-DTPAI  PICTURE  X(8).                    RSA030
049300      10            RS19-MTPAI  PICTURE  S9(09)V99                RSA030
049400                    COMPUTATIONAL-3.                              RSA030
049500      10            RS19-MORER  PICTURE  S9(09)V99                RSA030
049600                    COMPUTATIONAL-3.                              RSA030
049700      10            RS19-CDPAD  PICTURE  X.                       RSA030
049800      10            RS19-NOTIE  PICTURE  X(6).                    RSA030
049900      10            RS19-NOPAM  PICTURE  S99                      RSA030
050000                    COMPUTATIONAL-3.                              RSA030
050100      10            RS19-CDPAM  PICTURE  X.                       RSA030
050200      10            RS19-DTPAIR PICTURE  X(8).                    RSA030
050300      10            RS19-CDPAS  PICTURE  X.                       RSA030
050400      10            RS19-CDPAP  PICTURE  X.                       RSA030
050500      10            RS19-NOPAF  PICTURE  X(6).                    RSA030
050600      10            RS19-CDPAI  PICTURE  X.                       RSA030
050700      10            RS19-CDTAXE PICTURE  X.                       RSA030
050800      10            RS19-CDCAL  PICTURE  X.                       RSA030
050900      10            RS19-MTREF  PICTURE  S9(09)V99                RSA030
051000                    COMPUTATIONAL-3.                              RSA030
051100      10            RS19-DDPAI  PICTURE  X(8).                    RSA030
051200 01                 RS20.                                         RSA030
051300      10            RS20-CNREN  PICTURE  XX.                      RSA030
051400      10            RS20-NORER  PICTURE  X(6).                    RSA030
051500      10            RS20-IDRPL  PICTURE  X(5).                    RSA030
051600      10            RS20-IDRPLG                                   RSA030
051700                    REDEFINES            RS20-IDRPL.              RSA030
051800      11            RS20-NIREG  PICTURE  XX.                      RSA030
051900      11            RS20-CDRETP PICTURE  X.                       RSA030
052000      11            RS20-NIPOL  PICTURE  XX.                      RSA030
052100      10            RS20-CTREA  PICTURE  X.                       RSA030
052200      10            RS20-TXREI  PICTURE  S9999V99                 RSA030
052300                    COMPUTATIONAL-3.                              RSA030
052400      10            RS20-DTREO  PICTURE  X(8).                    RSA030
052500      10            RS20-MTREAO PICTURE  S9(09)V99                RSA030
052600                    COMPUTATIONAL-3.                              RSA030
052700      10            RS20-MTREAT PICTURE  S9(09)V99                RSA030
052800                    COMPUTATIONAL-3.                              RSA030
052900      10            RS20-MOREC  PICTURE  S9(09)V99                RSA030
053000                    COMPUTATIONAL-3.                              RSA030
053100 01                 RS22.                                         RSA030
053200      10            RS22-NOREN.                                   RSA030
053300      11            RS22-CNREN  PICTURE  XX.                      RSA030
053400      11            RS22-NORER  PICTURE  X(6).                    RSA030
053500      11            RS22-IDRPL  PICTURE  X(5).                    RSA030
053600      10            RS22-IDRPLG                                   RSA030
053700                    REDEFINES            RS22-NOREN.              RSA030
053800      11            RS22-FILLER PICTURE  X(8).                    RSA030
053900      11            RS22-NIREG  PICTURE  XX.                      RSA030
054000      11            RS22-CDRETP PICTURE  X.                       RSA030
054100      11            RS22-NIPOL  PICTURE  XX.                      RSA030
054200      10            RS22-XCSEQ  PICTURE  S9(9)                    RSA030
054300                    COMPUTATIONAL-3.                              RSA030
054400      10            RS22-DMREN  PICTURE  X(8).                    RSA030
054500      10            RS22-DHPAI  PICTURE  X(8).                    RSA030
054600      10            RS22-MTPAIO PICTURE  S9(09)V99                RSA030
054700                    COMPUTATIONAL-3.                              RSA030
054800      10            RS22-LIPAO  PICTURE  X(31).                   RSA030
054900      10            RS22-NOTIE  PICTURE  X(6).                    RSA030
055000      10            RS22-CTPAO  PICTURE  X.                       RSA030
055100      10            RS22-NOPAI  PICTURE  S999                     RSA030
055200                    COMPUTATIONAL-3.                              RSA030
055300      10            RS22-LIPA2  PICTURE  X(31).                   RSA030
055400      10            RS22-CDTAXE PICTURE  X.                       RSA030
055500      10            RS22-CDRES  PICTURE  X.                       RSA030
055600 01                 RS23.                                         RSA030
055700      10            RS23-NOREN.                                   RSA030
055800      11            RS23-CNREN  PICTURE  XX.                      RSA030
055900      11            RS23-NORER  PICTURE  X(6).                    RSA030
056000      11            RS23-IDRPL  PICTURE  X(5).                    RSA030
056100      11            RS23-IDRPLG                                   RSA030
056200                    REDEFINES            RS23-IDRPL.              RSA030
056300      12            RS23-NIREG  PICTURE  XX.                      RSA030
056400      12            RS23-CDRETP PICTURE  X.                       RSA030
056500      12            RS23-NIPOL  PICTURE  XX.                      RSA030
056600      10            RS23-XCSEQ  PICTURE  S9(9)                    RSA030
056700                    COMPUTATIONAL-3.                              RSA030
056800      10            RS23-MTREB  PICTURE  S9(09)V99                RSA030
056900                    COMPUTATIONAL-3.                              RSA030
057000      10            RS23-PXREF  PICTURE  S9(4)                    RSA030
057100                    COMPUTATIONAL-3.                              RSA030
057200      10            RS23-NOCRA  PICTURE  XX.                      RSA030
057300      10            RS23-NOSIN  PICTURE  X(10).                   RSA030
057400      10            RS23-NIREGS PICTURE  XX.                      RSA030
057500      10            RS23-ZN04   PICTURE  S9(4)                    RSA030
057600                    COMPUTATIONAL-3.                              RSA030
057700      10            RS23-CTRS23 PICTURE  X.                       RSA030
057800      10            RS23-CDSIN  PICTURE  X.                       RSA030
057900      10            RS23-CDEXCS PICTURE  X.                       RSA030
058000      10            RS23-CNRENA PICTURE  XX.                      RSA030
058100 01                 RS24.                                         RSA030
058200      10            RS24-CNREN  PICTURE  XX.                      RSA030
058300      10            RS24-NORER  PICTURE  X(6).                    RSA030
058400      10            RS24-IDRPL  PICTURE  X(5).                    RSA030
058500      10            RS24-IDRPLG                                   RSA030
058600                    REDEFINES            RS24-IDRPL.              RSA030
058700      11            RS24-NIREG  PICTURE  XX.                      RSA030
058800      11            RS24-CDRETP PICTURE  X.                       RSA030
058900      11            RS24-NIPOL  PICTURE  XX.                      RSA030
059000      10            RS24-MKREP  PICTURE  S9(09)V99                RSA030
059100                    COMPUTATIONAL-3.                              RSA030
059200      10            RS24-MKREF  PICTURE  S9(09)V99                RSA030
059300                    COMPUTATIONAL-3.                              RSA030
059400      10            RS24-DERET  PICTURE  X(8).                    RSA030
059500      10            RS24-ZX03   PICTURE  X(03).                   RSA030
059600      10            RS24-CTRS24 PICTURE  X.                       RSA030
059700      10            RS24-CDREA  PICTURE  X.                       RSA030
059800      10            RS24-DHPAID PICTURE  X(8).                    RSA030
059900      10            RS24-MTARA  PICTURE  S9(09)V99                RSA030
060000                    COMPUTATIONAL-3.                              RSA030
060100      10            RS24-MTARO  PICTURE  S9(09)V99                RSA030
060200                    COMPUTATIONAL-3.                              RSA030
060300      10            RS24-MKREP1 PICTURE  S9(09)V99                RSA030
060400                    COMPUTATIONAL-3.                              RSA030
060500      10            RS24-MKREP2 PICTURE  S9(09)V99                RSA030
060600                    COMPUTATIONAL-3.                              RSA030
060700      10            RS24-MTFRAA PICTURE  S9(09)V99                RSA030
060800                    COMPUTATIONAL-3.                              RSA030
060900      10            RS24-MVREF  PICTURE  S9(5)V9(5)               RSA030
061000                    COMPUTATIONAL-3.                              RSA030
061100      10            RS24-CTRETC PICTURE  X(8).                    RSA030
061200      10            RS24-CDTV   PICTURE  S999                     RSA030
061300                    COMPUTATIONAL-3.                              RSA030
061400      10            RS24-DTREO  PICTURE  X(8).                    RSA030
061500      10            RS24-TXREI  PICTURE  S9999V99                 RSA030
061600                    COMPUTATIONAL-3.                              RSA030
061700      10            RS24-PARER  PICTURE  S999                     RSA030
061800                    COMPUTATIONAL-3.                              RSA030
061900      10            RS24-CDUREE PICTURE  S999                     RSA030
062000                    COMPUTATIONAL-3.                              RSA030
062100      10            RS24-PARERI PICTURE  S999                     RSA030
062200                    COMPUTATIONAL-3.                              RSA030
062300 01                 RS26.                                         RSA030
062400      10            RS26-CNREN  PICTURE  XX.                      RSA030
062500      10            RS26-NORER  PICTURE  X(6).                    RSA030
062600      10            RS26-IDRPL  PICTURE  X(5).                    RSA030
062700      10            RS26-IDRPLG                                   RSA030
062800                    REDEFINES            RS26-IDRPL.              RSA030
062900      11            RS26-NIREG  PICTURE  XX.                      RSA030
063000      11            RS26-CDRETP PICTURE  X.                       RSA030
063100      11            RS26-NIPOL  PICTURE  XX.                      RSA030
063200      10            RS26-XCSEQ  PICTURE  S9(9)                    RSA030
063300                    COMPUTATIONAL-3.                              RSA030
063400      10            RS26-CDTAXE PICTURE  X.                       RSA030
063500      10            RS26-CSTAXE PICTURE  X.                       RSA030
063600      10            RS26-TXBTAX PICTURE  S999V99                  RSA030
063700                    COMPUTATIONAL-3.                              RSA030
063800      10            RS26-DMTAXE PICTURE  X(8).                    RSA030
063900      10            RS26-DDTAXE PICTURE  X(8).                    RSA030
064000      10            RS26-DFTAXE PICTURE  X(8).                    RSA030
064100      10            RS26-MORTAX PICTURE  S9(09)V99                RSA030
064200                    COMPUTATIONAL-3.                              RSA030
064300      10            RS26-NOTIE  PICTURE  X(6).                    RSA030
064400      10            RS26-CDCAL  PICTURE  X.                       RSA030
064500 01                 RS27.                                         RSA030
064600      10            RS27-CNREN  PICTURE  XX.                      RSA030
064700      10            RS27-NORER  PICTURE  X(6).                    RSA030
064800      10            RS27-IDRPL  PICTURE  X(5).                    RSA030
064900      10            RS27-IDRPLG                                   RSA030
065000                    REDEFINES            RS27-IDRPL.              RSA030
065100      11            RS27-NIREG  PICTURE  XX.                      RSA030
065200      11            RS27-CDRETP PICTURE  X.                       RSA030
065300      11            RS27-NIPOL  PICTURE  XX.                      RSA030
065400      10            RS27-CTRS27 PICTURE  X.                       RSA030
065500      10            RS27-CDGARA PICTURE  XX.                      RSA030
065600      10            RS27-DTSOU  PICTURE  X(4).                    RSA030
065700      10            RS27-NVERS  PICTURE  XXX.                     RSA030
065800      10            RS27-CDPEAS PICTURE  X(8).                    RSA030
065900      10            RS27-CDMOD  PICTURE  XX.                      RSA030
066000      10            RS27-CDECSY PICTURE  XX.                      RSA030
066100      10            RS27-DTDSI  PICTURE  X(8).                    RSA030
066200      10            RS27-TYPMAL PICTURE  XX.                      RSA030
066300      10            RS27-MTRAD  PICTURE  S9(09)V99                RSA030
066400                    COMPUTATIONAL-3.                              RSA030
066500      10            RS27-CCONT  PICTURE  X(12).                   RSA030
066600      10            RS27-CDPESO PICTURE  X(8).                    RSA030
066700      10            RS27-CPROD  PICTURE  X(10).                   RSA030
066800 01                 RS28.                                         RSA030
066900      10            RS28-CNREN  PICTURE  XX.                      RSA030
067000      10            RS28-NORER  PICTURE  X(6).                    RSA030
067100      10            RS28-IDRPL  PICTURE  X(5).                    RSA030
067200      10            RS28-IDRPLG                                   RSA030
067300                    REDEFINES            RS28-IDRPL.              RSA030
067400      11            RS28-NIREG  PICTURE  XX.                      RSA030
067500      11            RS28-CDRETP PICTURE  X.                       RSA030
067600      11            RS28-NIPOL  PICTURE  XX.                      RSA030
067700      10            RS28-XCSEQ  PICTURE  S9(9)                    RSA030
067800                    COMPUTATIONAL-3.                              RSA030
067900      10            RS28-XCSEQF PICTURE  S9(9)                    RSA030
068000                    COMPUTATIONAL-3.                              RSA030
068100      10            RS28-NOPAI  PICTURE  S999                     RSA030
068200                    COMPUTATIONAL-3.                              RSA030
068300      10            RS28-DHPAI  PICTURE  X(8).                    RSA030
068400      10            RS28-DTPAI  PICTURE  X(8).                    RSA030
068500      10            RS28-MTPAI  PICTURE  S9(09)V99                RSA030
068600                    COMPUTATIONAL-3.                              RSA030
068700      10            RS28-MORER  PICTURE  S9(09)V99                RSA030
068800                    COMPUTATIONAL-3.                              RSA030
068900      10            RS28-CDPAD  PICTURE  X.                       RSA030
069000      10            RS28-NOTIE  PICTURE  X(6).                    RSA030
069100      10            RS28-NOPAM  PICTURE  S99                      RSA030
069200                    COMPUTATIONAL-3.                              RSA030
069300      10            RS28-CDPAM  PICTURE  X.                       RSA030
069400      10            RS28-DTPAIR PICTURE  X(8).                    RSA030
069500      10            RS28-CDPAS  PICTURE  X.                       RSA030
069600      10            RS28-CDPAP  PICTURE  X.                       RSA030
069700      10            RS28-NOPAF  PICTURE  X(6).                    RSA030
069800      10            RS28-CDPAI  PICTURE  X.                       RSA030
069900      10            RS28-CDTAXE PICTURE  X.                       RSA030
070000      10            RS28-CDCAL  PICTURE  X.                       RSA030
070100      10            RS28-MTREF  PICTURE  S9(09)V99                RSA030
070200                    COMPUTATIONAL-3.                              RSA030
070300      10            RS28-DDPAI  PICTURE  X(8).                    RSA030
070400 01                 RS29.                                         RSA030
070500      10            RS29-CNREN  PICTURE  XX.                      RSA030
070600      10            RS29-NORER  PICTURE  X(6).                    RSA030
070700      10            RS29-IDRPL  PICTURE  X(5).                    RSA030
070800      10            RS29-IDRPLG                                   RSA030
070900                    REDEFINES            RS29-IDRPL.              RSA030
071000      11            RS29-NIREG  PICTURE  XX.                      RSA030
071100      11            RS29-CDRETP PICTURE  X.                       RSA030
071200      11            RS29-NIPOL  PICTURE  XX.                      RSA030
071300      10            RS29-CTRS27 PICTURE  X.                       RSA030
071400      10            RS29-PMBASE PICTURE  S9(09)V99                RSA030
071500                    COMPUTATIONAL-3.                              RSA030
071600      10            RS29-TXCAD  PICTURE  S9(5)V9(5)               RSA030
071700                    COMPUTATIONAL-3.                              RSA030
071800      10            RS29-BTPOO1 PICTURE  S9(09)V99                RSA030
071900                    COMPUTATIONAL-3.                              RSA030
072000      10            RS29-MVREF  PICTURE  S9(5)V9(5)               RSA030
072100                    COMPUTATIONAL-3.                              RSA030
072200      10            RS29-DERET  PICTURE  X(8).                    RSA030
072300      10            RS29-CDTIPS PICTURE  X(6).                    RSA030
072400      10            RS29-CDREGR PICTURE  XX.                      RSA030
072500      10            RS29-MTANNU PICTURE  S9(09)V99                RSA030
072600                    COMPUTATIONAL-3.                              RSA030
072700 01                 RS30.                                         RSA030
072800      10            RS30-NONER  PICTURE  XX.                      RSA030
072900      10            RS30-NOTIED PICTURE  S9(6)                    RSA030
073000                    COMPUTATIONAL-3.                              RSA030
073100 01                 RS31.                                         RSA030
073200      10            RS31-NOTIE  PICTURE  X(6).                    RSA030
073300      10            RS31-CDRES  PICTURE  X.                       RSA030
073400      10            RS31-LNTIE  PICTURE  X(30).                   RSA030
073500      10            RS31-CDRECT PICTURE  X(3).                    RSA030
073600 01                 RS32.                                         RSA030
073700      10            RS32-NOTIE  PICTURE  X(6).                    RSA030
073800      10            RS32-XCSEQ  PICTURE  S9(9)                    RSA030
073900                    COMPUTATIONAL-3.                              RSA030
074000      10            RS32-DMTIE  PICTURE  X(8).                    RSA030
074100      10            RS32-LRRE1T PICTURE  X(36).                   RSA030
074200      10            RS32-LRRE2T PICTURE  X(32).                   RSA030
074300      10            RS32-LVRET  PICTURE  X(31).                   RSA030
074400      10            RS32-CPRENT PICTURE  X(5).                    RSA030
074500      10            RS32-CDREPT PICTURE  XXX.                     RSA030
074600      10            RS32-NCRENT PICTURE  X(24).                   RSA030
074700      10            RS32-NCREET PICTURE  X(5).                    RSA030
074800      10            RS32-NCREGT PICTURE  X(5).                    RSA030
074900      10            RS32-NCREBT PICTURE  X(11).                   RSA030
075000      10            RS32-NCRECT PICTURE  XXX.                     RSA030
075100      10            RS32-CDPAMT PICTURE  X.                       RSA030
075200      10            RS32-NOPAM  PICTURE  S99                      RSA030
075300                    COMPUTATIONAL-3.                              RSA030
075400      10            RS32-DMTIEB PICTURE  X(8).                    RSA030
075500 01                 RS41.                                         RSA030
075600      10            RS41-ZCLET  PICTURE  X(10).                   RSA030
075700      10            RS41-FILLER                                   RSA030
075800                    REDEFINES            RS41-ZCLET.              RSA030
075900      11            RS41-CDRES  PICTURE  X.                       RSA030
076000      11            RS41-CDRETS PICTURE  X.                       RSA030
076100      11            RS41-NORETS PICTURE  X(8).                    RSA030
076200      10            RS41-ZLI30  PICTURE  X(30).                   RSA030
076300      10            RS41-ZN01   PICTURE  S9                       RSA030
076400                    COMPUTATIONAL-3.                              RSA030
076500      10            RS41-ZN02   PICTURE  S99                      RSA030
076600                    COMPUTATIONAL-3.                              RSA030
076700      10            RS41-TXTECH PICTURE  S9(3)V9(5)               RSA030
076800                    COMPUTATIONAL-3.                              RSA030
076900      10            RS41-CDMORT PICTURE  X(8).                    RSA030
077000      10            RS41-TAIPOS PICTURE  S9(2)                    RSA030
077100                    COMPUTATIONAL-3.                              RSA030
077200 01                 RS42.                                         RSA030
077300      10            RS42-ZIN42.                                   RSA030
077400      11            RS42-ZCLET  PICTURE  X(10).                   RSA030
077500      11            RS42-FILLER                                   RSA030
077600                    REDEFINES            RS42-ZCLET.              RSA030
077700      12            RS42-CDRES  PICTURE  X.                       RSA030
077800      12            RS42-CDRETS PICTURE  X.                       RSA030
077900      12            RS42-NORETS PICTURE  X(8).                    RSA030
078000      11            RS42-NIRET  PICTURE  X(14).                   RSA030
078100      10            RS42-ZTA100 PICTURE  X(100).                  RSA030
078200 01                 RS61.                                         RSA030
078300      10            RS61-CDRES  PICTURE  X.                       RSA030
078400      10            RS61-XCSEQ  PICTURE  S9(9)                    RSA030
078500                    COMPUTATIONAL-3.                              RSA030
078600      10            RS61-ZBAT   PICTURE  X.                       RSA030
078700      10            RS61-ZDTRTA PICTURE  XXX.                     RSA030
078800      10            RS61-DERETA PICTURE  X(8).                    RSA030
078900      10            RS61-MTREA1 PICTURE  S9(09)V99                RSA030
079000                    COMPUTATIONAL-3.                              RSA030
079100      10            RS61-MTREA2 PICTURE  S9(09)V99                RSA030
079200                    COMPUTATIONAL-3.                              RSA030
079300      10            RS61-MTREA3 PICTURE  S9(09)V99                RSA030
079400                    COMPUTATIONAL-3.                              RSA030
079500      10            RS61-ZDTRTD PICTURE  XXX.                     RSA030
079600      10            RS61-DERETD PICTURE  X(8).                    RSA030
079700      10            RS61-KMREDC PICTURE  S9999V9(5)               RSA030
079800                    COMPUTATIONAL-3.                              RSA030
079900      10            RS61-ZDTRTR PICTURE  XXX.                     RSA030
080000      10            RS61-DERETR PICTURE  X(8).                    RSA030
080100      10            RS61-MTREAL PICTURE  S9(09)V99                RSA030
080200                    COMPUTATIONAL-3.                              RSA030
080300      10            RS61-ZDTRTP PICTURE  XXX.                     RSA030
080400      10            RS61-MMPAI  PICTURE  S9(9)V99                 RSA030
080500                    COMPUTATIONAL-3.                              RSA030
080600      10            RS61-MXPAI  PICTURE  S9(9)V99                 RSA030
080700                    COMPUTATIONAL-3.                              RSA030
080800      10            RS61-DHPAIT PICTURE  X(8).                    RSA030
080900      10            RS61-ZDTRTS PICTURE  XXX.                     RSA030
081000      10            RS61-DHPAIS PICTURE  X(8).                    RSA030
081100      10            RS61-TXREAS PICTURE  S99V99                   RSA030
081200                    COMPUTATIONAL-3.                              RSA030
081300      10            RS61-ZDTRTF PICTURE  XXX.                     RSA030
081400      10            RS61-CNREN  PICTURE  XX.                      RSA030
081500      10            RS61-Z10CR  PICTURE  X(20).                   RSA030
081600      10            RS61-DGREJ  PICTURE  X(8).                    RSA030
081700      10            RS61-CNPAI  PICTURE  X.                       RSA030
081800      10            RS61-ZPARST PICTURE  X(285).                  RSA030
081900      10            RS61-MCPAV  PICTURE  S9(09)V99                RSA030
082000                    COMPUTATIONAL-3.                              RSA030
082100      10            RS61-MCPAM  PICTURE  S9(09)V99                RSA030
082200                    COMPUTATIONAL-3.                              RSA030
082300      10            RS61-MCPAMI PICTURE  S9(09)V99                RSA030
082400                    COMPUTATIONAL-3.                              RSA030
082500      10            RS61-ZPARTX PICTURE  X(285).                  RSA030
082600      10            RS61-MTPAIS PICTURE  S9(9)V99                 RSA030
082700                    COMPUTATIONAL-3.                              RSA030
082800      10            RS61-DTTTD  PICTURE  X(8).                    RSA030
082900 01                 RS62.                                         RSA030
083000      10            RS62-CDRES  PICTURE  X.                       RSA030
083100      10            RS62-XCSEQ  PICTURE  S9(9)                    RSA030
083200                    COMPUTATIONAL-3.                              RSA030
083300      10            RS62-ZDTRTE PICTURE  XXX.                     RSA030
083400      10            RS62-ZDTRTI PICTURE  XXX.                     RSA030
083500      10            RS62-ZDTRTO PICTURE  XXX.                     RSA030
083600      10            RS62-DERET  PICTURE  X(8).                    RSA030
083700      10            RS62-CSELS3 PICTURE  X.                       RSA030
083800      10            RS62-CETATA PICTURE  X.                       RSA030
083900      10            RS62-CETATB PICTURE  X.                       RSA030
084000      10            RS62-CETATC PICTURE  X.                       RSA030
084100      10            RS62-CETATD PICTURE  X.                       RSA030
084200      10            RS62-CETATE PICTURE  X.                       RSA030
084300      10            RS62-CETATF PICTURE  X.                       RSA030
084400      10            RS62-CETATG PICTURE  X.                       RSA030
084500      10            RS62-CETATH PICTURE  X.                       RSA030
084600      10            RS62-CTRS60 PICTURE  X.                       RSA030
084700      10            RS62-DTRET  PICTURE  X(8).                    RSA030
084800 01                 RS63.                                         RSA030
084900      10            RS63-CDRES  PICTURE  X.                       RSA030
085000      10            RS63-XCSEQ  PICTURE  S9(9)                    RSA030
085100                    COMPUTATIONAL-3.                              RSA030
085200      10            RS63-DK00.                                    RSA030
085300      11            RS63-NOREN  PICTURE  X(13).                   RSA030
085400      11            RS63-NOSIN  PICTURE  X(10).                   RSA030
085500      11            RS63-NIREGS PICTURE  XX.                      RSA030
085600      11            RS63-NOCRA  PICTURE  XX.                      RSA030
085700      11            RS63-CDSIN  PICTURE  X.                       RSA030
085800      11            RS63-DMRENS PICTURE  X(8).                    RSA030
085900      11            RS63-NOSIN1 PICTURE  X(10).                   RSA030
086000      11            RS63-NIREG1 PICTURE  XX.                      RSA030
086100      11            RS63-NOCRA1 PICTURE  XX.                      RSA030
086200      11            RS63-DMREN1 PICTURE  X(8).                    RSA030
086300      11            RS63-CDRES2 PICTURE  X.                       RSA030
086400      11            RS63-NONER  PICTURE  XX.                      RSA030
086500      11            RS63-ZX01   PICTURE  X.                       RSA030
086600      11            RS63-ZX35   PICTURE  X(35).                   RSA030
086700      11            RS63-ZX35A  PICTURE  X(35).                   RSA030
086800      11            RS63-CDEXCS PICTURE  X.                       RSA030
086900      11            RS63-ZX03   PICTURE  X(03).                   RSA030
087000      11            RS63-CDRES3 PICTURE  X.                       RSA030
087100      11            RS63-XCSEQF PICTURE  S9(9)                    RSA030
087200                    COMPUTATIONAL-3.                              RSA030
087300 01                 RS64.                                         RSA030
087400      10            RS64-DTRET  PICTURE  X(8).                    RSA030
087500      10            RS64-ZNUSIM PICTURE  XXX.                     RSA030
087600      10            RS64-SIMRG  PICTURE  X(5).                    RSA030
087700      10            RS64-CDSIT  PICTURE  X.                       RSA030
087800      10            RS64-ZEDDET PICTURE  XXX.                     RSA030
087900      10            RS64-ZEDREC PICTURE  XXX.                     RSA030
088000      10            RS64-ZENVFC PICTURE  XXX.                     RSA030
088100      10            RS64-ZIDINF PICTURE  X(8).                    RSA030
088200      10            RS64-ZDTRS  PICTURE  X.                       RSA030
088300      10            RS64-DTPER  PICTURE  X(8).                    RSA030
088400 01                 RS70.                                         RSA030
088500      10            RS70-CDRES  PICTURE  X.                       RSA030
088600      10            RS70-ZBAT   PICTURE  X.                       RSA030
088700      10            RS70-DTRET  PICTURE  X(8).                    RSA030
088800      10            RS70-CEFOR  PICTURE  X.                       RSA030
088900      10            RS70-DHPAIT PICTURE  X(8).                    RSA030
089000      10            RS70-DHPAIS PICTURE  X(8).                    RSA030
089100      10            RS70-DERETD PICTURE  X(8).                    RSA030
089200      10            RS70-DERETA PICTURE  X(8).                    RSA030
089300      10            RS70-MMPAI  PICTURE  S9(9)V99                 RSA030
089400                    COMPUTATIONAL-3.                              RSA030
089500      10            RS70-MXPAI  PICTURE  S9(9)V99                 RSA030
089600                    COMPUTATIONAL-3.                              RSA030
089700      10            RS70-NPREC  PICTURE  X(7).                    RSA030
089800      10            RS70-DTRETP PICTURE  X(8).                    RSA030
089900      10            RS70-ZDTRTE PICTURE  XXX.                     RSA030
090000      10            RS70-ZDTRTI PICTURE  XXX.                     RSA030
090100      10            RS70-ZDTRTO PICTURE  XXX.                     RSA030
090200      10            RS70-DERET  PICTURE  X(8).                    RSA030
090300      10            RS70-CSELS3 PICTURE  X.                       RSA030
090400 01                 RS71.                                         RSA030
090500      10            RS71-CDRES  PICTURE  X.                       RSA030
090600      10            RS71-XCSEQ  PICTURE  S9(9)                    RSA030
090700                    COMPUTATIONAL-3.                              RSA030
090800      10            RS71-NOREN  PICTURE  X(13).                   RSA030
090900      10            RS71-NOPAI  PICTURE  S999                     RSA030
091000                    COMPUTATIONAL-3.                              RSA030
091100      10            RS71-MTPAIM PICTURE  S9(09)V99                RSA030
091200                    COMPUTATIONAL-3.                              RSA030
091300      10            RS71-CDPAP  PICTURE  X.                       RSA030
091400      10            RS71-NOPAF  PICTURE  X(6).                    RSA030
091500      10            RS71-DTPAI  PICTURE  X(8).                    RSA030
091600      10            RS71-DTPAIR PICTURE  X(8).                    RSA030
091700      10            RS71-CDTAXE PICTURE  X.                       RSA030
091800          EXEC SQL END   DECLARE SECTION END-EXEC.                7RTAAA
091900 77               W-WA00-NONER    PIC 99.                         7WA010
092000 01               W-WW00-ZRECOR                                   7WA100
092100                  PICTURE X(4).                                   7WA100
092200 01               DB-RECORD-NAME PIC X(4) REDEFINES W-WW00-ZRECOR.7WA110
092300 01               W-WA00-ABRS27.                                  7WA820
092400    05            W-WA00-CDMON                                    7WA840
092500                  PICTURE XXX.                                    7WA840
092600 01               W-WW00-NORER                                    7WW100
092700                  PICTURE X(6).                                   7WW100
092800 01               W-WW00-LIDEV                                    7WW998
092900                  PICTURE X(6).                                   7WW998
093000 77                 XA00-XRC      VALUE ZERO                      7XA015
093100                  PICTURE 9(4).                                   7XA015
093200 77                 XA00-8TMES    VALUE ZERO                      7XA055
093300                  PICTURE X(01).                                  7XA055
093400 77                 XAIN-XDATRT                                   7XA110
093500                  PICTURE X(8).                                   7XA110
093600 77                 XAIN-XHETRT                                   7XA120
093700                  PICTURE X(6).                                   7XA120
093800 77                 XAED-XDATRT                                   7XA160
093900                  PICTURE X(10).                                  7XA160
094000 77                 XAED-XHETRT                                   7XA170
094100                  PICTURE X(8).                                   7XA170
094200 77                 XA30-ENVNAM   PIC X(080).                     7XA310
094300 77                 XA30-ENVVAL   PIC X(080).                     7XA320
094400 01                 XA60.                                         7XA610
094500   05               XA60-L1.                                      7XA620
094600     10             FILLER PIC X  VALUE 'F'.                      7XA625
094700     10             XA60-XCDFSF   VALUE SPACE                     7XA630
094800                  PICTURE X(4).                                   7XA630
094900     10             FILLER PIC X  VALUE ':'.                      7XA635
095000     10             XA60-XLISUI   VALUE SPACE                     7XA640
095100                  PICTURE X(30).                                  7XA640
095200   05               XA60-ZX67A    VALUE SPACE                     7XA650
095300                  PICTURE X(67).                                  7XA650
095400   05               XA60-ZX67B    VALUE SPACE                     7XA660
095500                  PICTURE X(67).                                  7XA660
095600   05               XA60-ZX67C    VALUE SPACE                     7XA670
095700                  PICTURE X(67).                                  7XA670
095800 01                 XA80.                                         7XA810
095900   05               FILLER        PIC X(012) VALUE 'ERREUR I-O S'.7XA815
096000   05               FILLER        PIC X(011) VALUE 'UR FICHIER '. 7XA816
096100   05               XA80-XCOSD                                    7XA820
096200                  PICTURE XX.                                     7XA820
096300   05               FILLER        PIC X(010) VALUE ', STATUS: '.  7XA825
096400   05               XA80-STATUS.                                  7XA830
096500    07              XA80-STATUS1  PIC X.                          7XA831
096600    07              XA80-STATUS2  PIC 99 COMP-X.                  7XA832
096700    07              FILLER        PIC X(003) VALUE SPACE.         7XA833
096800   05               FILLER        PIC X(012) VALUE ', ENREG. TRA'.7XA835
096900   05               FILLER        PIC X(006) VALUE 'ITES: '.      7XA836
097000   05               XA80-XQNENR                                   7XA840
097100                  PICTURE Z(8)9.                                  7XA840
097200 01                 XA81-STATUS.                                  7XA900
097300    07              XA81-STATUS1  PIC X.                          7XA905
097400    07              FILLER        PIC X      VALUE '/'.           7XA910
097500    07              XA81-STATUS2  PIC 999.                        7XA915
097600          EXEC SQL BEGIN DECLARE SECTION END-EXEC.                7XBBB0
097700 01               USERID          PIC X(20).                      7XBCC2
097800          EXEC SQL END   DECLARE SECTION END-EXEC.                7XBZZ0
097900 77                 RS01-XROWID                                   7XB001
098000                  PICTURE X(18).                                  7XB001
098100 77                 RS02-XROWID                                   7XB002
098200                  PICTURE X(18).                                  7XB002
098300 77                 RS03-XROWID                                   7XB003
098400                  PICTURE X(18).                                  7XB003
098500 77                 RS04-XROWID                                   7XB004
098600                  PICTURE X(18).                                  7XB004
098700 77                 RS05-XROWID                                   7XB005
098800                  PICTURE X(18).                                  7XB005
098900 77                 RS06-XROWID                                   7XB006
099000                  PICTURE X(18).                                  7XB006
099100 77                 RS07-XROWID                                   7XB007
099200                  PICTURE X(18).                                  7XB007
099300 77                 RS08-XROWID                                   7XB008
099400                  PICTURE X(18).                                  7XB008
099500 77                 RS09-XROWID                                   7XB009
099600                  PICTURE X(18).                                  7XB009
099700 77                 RS10-XROWID                                   7XB010
099800                  PICTURE X(18).                                  7XB010
099900 77                 RS11-XROWID                                   7XB011
100000                  PICTURE X(18).                                  7XB011
100100 77                 RS12-XROWID                                   7XB012
100200                  PICTURE X(18).                                  7XB012
100300 77                 RS13-XROWID                                   7XB013
100400                  PICTURE X(18).                                  7XB013
100500 77                 RS14-XROWID                                   7XB014
100600                  PICTURE X(18).                                  7XB014
100700 77                 RS15-XROWID                                   7XB015
100800                  PICTURE X(18).                                  7XB015
100900 77                 RS16-XROWID                                   7XB016
101000                  PICTURE X(18).                                  7XB016
101100 77                 RS17-XROWID                                   7XB017
101200                  PICTURE X(18).                                  7XB017
101300 77                 RS18-XROWID                                   7XB018
101400                  PICTURE X(18).                                  7XB018
101500 77                 RS19-XROWID                                   7XB019
101600                  PICTURE X(18).                                  7XB019
101700 77                 RS20-XROWID                                   7XB020
101800                  PICTURE X(18).                                  7XB020
101900 77                 RS22-XROWID                                   7XB022
102000                  PICTURE X(18).                                  7XB022
102100 77                 RS23-XROWID                                   7XB023
102200                  PICTURE X(18).                                  7XB023
102300 77                 RS24-XROWID                                   7XB024
102400                  PICTURE X(18).                                  7XB024
102500 77                 RS26-XROWID                                   7XB026
102600                  PICTURE X(18).                                  7XB026
102700 77                 RS27-XROWID                                   7XB027
102800                  PICTURE X(18).                                  7XB027
102900 77                 RS28-XROWID                                   7XB028
103000                  PICTURE X(18).                                  7XB028
103100 77                 RS29-XROWID                                   7XB029
103200                  PICTURE X(18).                                  7XB029
103300 77                 RS30-XROWID                                   7XB030
103400                  PICTURE X(18).                                  7XB030
103500 77                 RS31-XROWID                                   7XB031
103600                  PICTURE X(18).                                  7XB031
103700 77                 RS32-XROWID                                   7XB032
103800                  PICTURE X(18).                                  7XB032
103900 77                 RS41-XROWID                                   7XB041
104000                  PICTURE X(18).                                  7XB041
104100 77                 RS42-XROWID                                   7XB042
104200                  PICTURE X(18).                                  7XB042
104300 77                 RS61-XROWID                                   7XB061
104400                  PICTURE X(18).                                  7XB061
104500 77                 RS62-XROWID                                   7XB062
104600                  PICTURE X(18).                                  7XB062
104700 77                 RS63-XROWID                                   7XB063
104800                  PICTURE X(18).                                  7XB063
104900 77                 RS64-XROWID                                   7XB064
105000                  PICTURE X(18).                                  7XB064
105100 77                 RS69-XROWID                                   7XB069
105200                  PICTURE X(18).                                  7XB069
105300 77                 RS70-XROWID                                   7XB070
105400                  PICTURE X(18).                                  7XB070
105500 77                 RS71-XROWID                                   7XB071
105600                  PICTURE X(18).                                  7XB071
105700 01                 XO00.                                         7XO-A0
105800   05               XO00-XORACN   PIC X(001) VALUE ZERO.          7XO-B2
105900   05               XO00-XORATR   PIC X(001) VALUE ZERO.          7XO-C2
106000   05               XO00-XORACO   PIC X(001) VALUE ZERO.          7XO-D2
106100   05               XO00-XORAER.                                  7XO-EE
106200     10             FILLER        PIC X(012) VALUE 'ERREUR ORACL'.7XO-EG
106300     10             FILLER        PIC X(007) VALUE 'E ORA-0'.     7XO-EI
106400     10             XO00-XORARC   PIC 9(004).                     7XO-EK
106500     10             FILLER        PIC X(005) VALUE ' EN F'.       7XO-EM
106600     10             XO00-XCDFSF                                   7XO-EO
106700                  PICTURE X(4).                                   7XO-EO
106800     10             FILLER        PIC X(008) VALUE ' ACCES:'.     7XO-EQ
106900     10             XO00-XORATY   PIC X(008).                     7XO-ES
107000     10             FILLER        PIC X(008) VALUE ' TABLE:'.     7XO-EU
107100     10             XO00-XORATA                                   7XO-EW
107200                  PICTURE X(10).                                  7XO-EW
107300   05               XO00-XORAE2.                                  7XO-FE
107400     10             FILLER        PIC X(012) VALUE 'DEBUT SEGT: '.7XO-FG
107500     10             XO00-XORACL.                                  7XO-FI
107600       15           XO00-XORAC1   PIC X(001) OCCURS 055.          7XO-FK
107700                  EXEC SQL BEGIN DECLARE SECTION         END-EXEC.7XO-01
107800 77                 XO00-XORARB   PIC X(008) VALUE SPACE.         7XO-15
107900                  EXEC SQL END   DECLARE SECTION         END-EXEC.7XO-89
108000 77                 XOAA-RS01-CF  PIC X(001)     VALUE ZERO.      7XOAA0
108100 77                 XOAA-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOAA1
108200 77                 XOBB-RS42-CF  PIC X(001)     VALUE ZERO.      7XOBB0
108300 77                 XOBB-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOBB1
108400 77                 XOCC-RS02-CF  PIC X(001)     VALUE ZERO.      7XOCC0
108500 77                 XOCC-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOCC1
108600 77                 XODD-RS04-CF  PIC X(001)     VALUE ZERO.      7XODD0
108700 77                 XODD-COUNT    PIC S9(9) COMP VALUE ZERO.      7XODD1
108800 77                 XOEE-RS05-CF  PIC X(001)     VALUE ZERO.      7XOEE0
108900 77                 XOEE-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOEE1
109000 77                 XOFF-RS06-CF  PIC X(001)     VALUE ZERO.      7XOFF0
109100 77                 XOFF-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOFF1
109200 77                 XOFF-RS06-OPE PIC X(001)     VALUE ZERO.      7XOFF2
109300 77                 XOGG-RS07-CF  PIC X(001)     VALUE ZERO.      7XOGG0
109400 77                 XOGG-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOGG1
109500 77                 XOH1-RS08-CF  PIC X(001)     VALUE ZERO.      7XOH10
109600 77                 XOH1-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOH11
109700 77                 XOH2-RS09-CF  PIC X(001)     VALUE ZERO.      7XOH20
109800 77                 XOH2-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOH21
109900 77                 XOH3-RS10-CF  PIC X(001)     VALUE ZERO.      7XOH30
110000 77                 XOH3-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOH31
110100 77                 XOH4-RS11-CF  PIC X(001)     VALUE ZERO.      7XOH40
110200 77                 XOH4-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOH41
110300 77                 XOH5-RS12-CF  PIC X(001)     VALUE ZERO.      7XOH50
110400 77                 XOH5-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOH51
110500 77                 XOH6-RS13-CF  PIC X(001)     VALUE ZERO.      7XOH60
110600 77                 XOH6-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOH61
110700 77                 XOH7-RS14-CF  PIC X(001)     VALUE ZERO.      7XOH70
110800 77                 XOH7-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOH71
110900 77                 XOH8-RS15-CF  PIC X(001)     VALUE ZERO.      7XOH80
111000 77                 XOH8-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOH81
111100 77                 XOH9-RS20-CF  PIC X(001)     VALUE ZERO.      7XOH90
111200 77                 XOH9-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOH91
111300 77                 XOPP-RS16-CF  PIC X(001)     VALUE ZERO.      7XOPP0
111400 77                 XOPP-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOPP1
111500 77                 XOQQ-RS17-CF  PIC X(001)     VALUE ZERO.      7XOQQ0
111600 77                 XOQQ-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOQQ1
111700 77                 XORR-RS18-CF  PIC X(001)     VALUE ZERO.      7XORR0
111800 77                 XORR-COUNT    PIC S9(9) COMP VALUE ZERO.      7XORR1
111900 77                 XOSS-RS19-CF  PIC X(001)     VALUE ZERO.      7XOSS0
112000 77                 XOSS-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOSS1
112100 77                 XOTT-RS22-CF  PIC X(001)     VALUE ZERO.      7XOTT0
112200 77                 XOTT-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOTT1
112300 77                 XOUU-RS23-CF  PIC X(001)     VALUE ZERO.      7XOUU0
112400 77                 XOUU-COUNT    PIC S9(9) COMP VALUE ZERO.      7XOUU1
112500          EXEC SQL BEGIN DECLARE SECTION END-EXEC.                7XPZZ5
112600 77                 XP00-XROWID                                   7XP001
112700                  PICTURE X(18).                                  7XP001
112800 77                 XP00-XCSEQ                                    7XP002
112900                  PICTURE S9(9)                                   7XP002
113000                    COMPUTATIONAL-3.                              7XP002
113100 77                 XP00-XCOUNT   PIC S9(9) COMP-3.               7XP003
113200          EXEC SQL END   DECLARE SECTION END-EXEC.                7XP999
113300 01                 CODE-ABORT    PIC X(2).                       7ZA999
113400 01   DEBUT-WSS.                                                  RSA030
113500      05   FILLER PICTURE X(7) VALUE 'WORKING'.                   RSA030
113600      05   IK     PICTURE X.                                      RSA030
113700 01  CONSTANTES-PAC.                                              RSA030
113800     05  FILLER  PICTURE X(60)   VALUE                            RSA030
113900               '6078HRSA06/09/04RSA030LEBLANC 15:27:44RSA030  UNIXRSA030
114000-    '06/09/2004'.                                                RSA030
114100 01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     RSA030
114200     05  NUGNA   PICTURE X(5).                                    RSA030
114300     05  APPLI   PICTURE X(3).                                    RSA030
114400     05  DATGN   PICTURE X(8).                                    RSA030
114500     05  PROGR   PICTURE X(6).                                    RSA030
114600     05  CODUTI  PICTURE X(8).                                    RSA030
114700     05  TIMGN   PICTURE X(8).                                    RSA030
114800     05  PROGE   PICTURE X(8).                                    RSA030
114900     05  COBASE  PICTURE X(4).                                    RSA030
115000     05  DATGNC  PICTURE X(10).                                   RSA030
115100 01  DATCE.                                                       RSA030
115200   05  CENTUR   PICTURE XX   VALUE '19'.                          RSA030
115300   05  DATOR.                                                     RSA030
115400     10  DATOA  PICTURE XX.                                       RSA030
115500     10  DATOM  PICTURE XX.                                       RSA030
115600     10  DATOJ  PICTURE XX.                                       RSA030
115700 01  DAT6.                                                        RSA030
115800      10 DAT61   PICTURE XX.                                      RSA030
115900      10 DAT62   PICTURE XX.                                      RSA030
116000      10 DAT63   PICTURE XX.                                      RSA030
116100 01  DAT8.                                                        RSA030
116200      10 DAT81   PICTURE XX.                                      RSA030
116300      10 DAT8S1  PICTURE X.                                       RSA030
116400      10 DAT82   PICTURE XX.                                      RSA030
116500      10 DAT8S2  PICTURE X.                                       RSA030
116600      10 DAT83   PICTURE XX.                                      RSA030
116700 01  DAT8E    REDEFINES    DAT8.                                  RSA030
116800      10 DAT81E  PICTURE X(4).                                    RSA030
116900      10 DAT82E  PICTURE XX.                                      RSA030
117000      10 DAT83E  PICTURE XX.                                      RSA030
117100 01  DAT6C.                                                       RSA030
117200      10  DAT61C PICTURE XX.                                      RSA030
117300      10  DAT62C PICTURE XX.                                      RSA030
117400      10  DAT63C.                                                 RSA030
117500       15 DAT63CC PICTURE XX.                                     RSA030
117600       15 DAT64C  PICTURE XX.                                     RSA030
117700 01  DAT8C.                                                       RSA030
117800      10  DAT81C  PICTURE XX.                                     RSA030
117900      10  DAT8S1C PICTURE X   VALUE '/'.                          RSA030
118000      10  DAT82C  PICTURE XX.                                     RSA030
118100      10  DAT8S2C PICTURE X   VALUE '/'.                          RSA030
118200      10  DAT83C.                                                 RSA030
118300       15 DAT83CC PICTURE XX.                                     RSA030
118400       15 DAT84C  PICTURE XX.                                     RSA030
118500 01  TIMCO.                                                       RSA030
118600   05  TIMCOH   PICTURE XX.                                       RSA030
118700   05  TIMCOM   PICTURE XX.                                       RSA030
118800   05  TIMCOS   PICTURE XX.                                       RSA030
118900   05  TIMOC.                                                     RSA030
119000    10 TIMCOC   PICTURE XX.                                       RSA030
119100 01  TIMDAY.                                                      RSA030
119200   05  TIMHOU   PICTURE XX.                                       RSA030
119300   05  TIMS1    PICTURE X  VALUE ':'.                             RSA030
119400   05  TIMMIN   PICTURE XX.                                       RSA030
119500   05  TIMS2    PICTURE X  VALUE ':'.                             RSA030
119600   05  TIMSEC   PICTURE XX.                                       RSA030
119700 01  DATSEP     PICTURE X VALUE '/'.                              RSA030
119800 01   VARIABLES-CONDITIONNELLES.                                  RSA030
119900      05                  FT      PICTURE X VALUE '0'.            RSA030
120000 01   INDICES  COMPUTATIONAL  SYNC.                               RSA030
120100      05          TALLI   PICTURE S9(4) VALUE  ZERO.              RSA030
120200      05        J00       PICTURE S9(4) VALUE +1.                 RSA030
120300      05        J01       PICTURE S9(4) VALUE +1.                 RSA030
120400      05           IXO00L PICTURE S9(4) VALUE  ZERO.              RSA030
120500      05           IXO00R PICTURE S9(4) VALUE  ZERO.              RSA030
120600      05           IXO00M PICTURE S9(4) VALUE +0055.              RSA030
120700      05           J99OVR PICTURE S9(4) VALUE  ZERO.              P000
120800 01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   RSA030
120900      05       5-RS00-CPTENR PICTURE S9(9) VALUE ZERO.            RSA030
121000      05       5-YX00-CPTENR PICTURE S9(9) VALUE ZERO.            RSA030
121100 01  ZONES-STATUS.                                                RSA030
121200      05   VSAM-STATUS.                                           RSA030
121300        10 VSAM-RCODE   PICTURE S9(4) COMP VALUE ZERO.            RSA030
121400        10 VSAM-FCODE   PICTURE S9(4) COMP VALUE ZERO.            RSA030
121500        10 VSAM-FBCODE  PICTURE S9(4) COMP VALUE ZERO.            RSA030
121600      05          1-EW00-STATUS PICTURE XX VALUE ZERO.            RSA030
121700      05          1-YX00-STATUS PICTURE XX VALUE ZERO.            RSA030
121800 01   CAT-TAB.                                                    RSA030
121900      05  FILLER         PICTURE X(100) VALUE SPACES.             RSA030
122000      05  FILLER         PICTURE X(100) VALUE SPACES.             RSA030
122100 01   CAT-TAB-R  REDEFINES CAT-TAB.                               RSA030
122200      05  CAT    PICTURE XX       OCCURS 0100.                    RSA030
122300 01   ST-TA.                                                      RSA030
122400      05  ST-ABS      PICTURE X  VALUE SPACE.                     RSA030
122500      05  ST-T.                                                   RSA030
122600      07  ST-TT    OCCURS 40.                                     RSA030
122700        10  ST-ST     PICTURE XX.                                 RSA030
122800        10  ST-LI     PICTURE 99.                                 RSA030
122900        10  ST-SA     PICTURE 99.                                 RSA030
123000 01  CONTENU-DES-CATEGORIES.                                      RSA030
123100     05          TS-4-BB.                                         RSA030
123200     10         ABS-4-BB PICTURE X VALUE  '*'.                    RSA030
123300      10  FILLER    PICTURE       X(30) VALUE                     RSA030
123400     '000101010201000301020402000501'.                            RSA030
123500     05          TS-4-CC.                                         RSA030
123600     10         ABS-4-CC PICTURE X VALUE  ' '.                    RSA030
123700      10  FILLER    PICTURE       X(30) VALUE                     RSA030
123800     '000602000701030802040901051001'.                            RSA030
123900     05          TS-4-DD.                                         RSA030
124000     10         ABS-4-DD PICTURE X VALUE  ' '.                    RSA030
124100      10  FILLER    PICTURE       X(24) VALUE                     RSA030
124200     '001102001201061302071401'.                                  RSA030
124300     05          TS-4-EE.                                         RSA030
124400     10         ABS-4-EE PICTURE X VALUE  ' '.                    RSA030
124500      10  FILLER    PICTURE       X(24) VALUE                     RSA030
124600     '001502001601081702091801'.                                  RSA030
124700     05          TS-4-FF.                                         RSA030
124800     10         ABS-4-FF PICTURE X VALUE  ' '.                    RSA030
124900      10  FILLER    PICTURE       X(30) VALUE                     RSA030
125000     '001902002001102102112201122301'.                            RSA030
125100     05          TS-4-GG.                                         RSA030
125200     10         ABS-4-GG PICTURE X VALUE  ' '.                    RSA030
125300      10  FILLER    PICTURE       X(30) VALUE                     RSA030
125400     '002402002501132602142701152801'.                            RSA030
125500     05          TS-4-HH.                                         RSA030
125600     10         ABS-4-HH PICTURE X VALUE  ' '.                    RSA030
125700      10  FILLER    PICTURE       X(30) VALUE                     RSA030
125800     '002902003001163102173201183301'.                            RSA030
125900     05          TS-4-II.                                         RSA030
126000     10         ABS-4-II PICTURE X VALUE  ' '.                    RSA030
126100      10  FILLER    PICTURE       X(24) VALUE                     RSA030
126200     '003402003501193602203701'.                                  RSA030
126300     05          TS-4-JJ.                                         RSA030
126400     10         ABS-4-JJ PICTURE X VALUE  ' '.                    RSA030
126500      10  FILLER    PICTURE     X(42) VALUE                       RSA030
126600     '003802003901214002224101234201244301254401'.                RSA030
126700      10  FILLER    PICTURE       X(06) VALUE                     RSA030
126800     '264501'.                                                    RSA030
126900     05          TS-4-KK.                                         RSA030
127000     10         ABS-4-KK PICTURE X VALUE  ' '.                    RSA030
127100      10  FILLER    PICTURE     X(42) VALUE                       RSA030
127200     '004602004701274802284901295001305101315201'.                RSA030
127300      10  FILLER    PICTURE       X(12) VALUE                     RSA030
127400     '325301335401'.                                              RSA030
127500     05          TS-4-LL.                                         RSA030
127600     10         ABS-4-LL PICTURE X VALUE  ' '.                    RSA030
127700      10  FILLER    PICTURE       X(24) VALUE                     RSA030
127800     '005502005601345702355801'.                                  RSA030
127900     05          TS-4-MM.                                         RSA030
128000     10         ABS-4-MM PICTURE X VALUE  ' '.                    RSA030
128100      10  FILLER    PICTURE       X(18) VALUE                     RSA030
128200     '005902006001366102'.                                        RSA030
128300     05          TS-4-NN.                                         RSA030
128400     10         ABS-4-NN PICTURE X VALUE  ' '.                    RSA030
128500      10  FILLER    PICTURE       X(18) VALUE                     RSA030
128600     '006202006301376402'.                                        RSA030
128700     05          TS-4-OO.                                         RSA030
128800     10         ABS-4-OO PICTURE X VALUE  ' '.                    RSA030
128900      10  FILLER    PICTURE     X(42) VALUE                       RSA030
129000     '006502006601386702396801406901417001427101'.                RSA030
129100      10  FILLER    PICTURE       X(24) VALUE                     RSA030
129200     '437201447301457401467501'.                                  RSA030
129300     05          TS-4-PP.                                         RSA030
129400     10         ABS-4-PP PICTURE X VALUE  ' '.                    RSA030
129500      10  FILLER    PICTURE       X(18) VALUE                     RSA030
129600     '007602007701477802'.                                        RSA030
129700     05          TS-4-RR.                                         RSA030
129800     10         ABS-4-RR PICTURE X VALUE  ' '.                    RSA030
129900      10  FILLER    PICTURE     X(42) VALUE                       RSA030
130000     '007902008001488102498201508301518401528501'.                RSA030
130100     05          TS-4-TT.                                         RSA030
130200     10         ABS-4-TT PICTURE X VALUE  ' '.                    RSA030
130300      10  FILLER    PICTURE       X(36) VALUE                     RSA030
130400     '008602008701538802548901559001569101'.                      RSA030
130500     05          TS-4-VV.                                         RSA030
130600     10         ABS-4-VV PICTURE X VALUE  ' '.                    RSA030
130700      10  FILLER    PICTURE       X(18) VALUE                     RSA030
130800     '009202009301579402'.                                        RSA030
130900     05          TS-4-WW.                                         RSA030
131000     10         ABS-4-WW PICTURE X VALUE  ' '.                    RSA030
131100      10  FILLER    PICTURE       X(30) VALUE                     RSA030
131200     '009502009601589702599801609901'.                            RSA030
131300 01   TAILLES-DES-CATEGORIES   COMPUTATIONAL-3.                   RSA030
131400      05          4-BB-NL PICTURE S99 VALUE +05.                  RSA030
131500      05          4-CC-NL PICTURE S99 VALUE +07.                  RSA030
131600      05          4-DD-NL PICTURE S99 VALUE +06.                  RSA030
131700      05          4-EE-NL PICTURE S99 VALUE +06.                  RSA030
131800      05          4-FF-NL PICTURE S99 VALUE +07.                  RSA030
131900      05          4-GG-NL PICTURE S99 VALUE +07.                  RSA030
132000      05          4-HH-NL PICTURE S99 VALUE +07.                  RSA030
132100      05          4-II-NL PICTURE S99 VALUE +06.                  RSA030
132200      05          4-JJ-NL PICTURE S99 VALUE +10.                  RSA030
132300      05          4-KK-NL PICTURE S99 VALUE +11.                  RSA030
132400      05          4-LL-NL PICTURE S99 VALUE +06.                  RSA030
132500      05          4-MM-NL PICTURE S99 VALUE +05.                  RSA030
132600      05          4-NN-NL PICTURE S99 VALUE +05.                  RSA030
132700      05          4-OO-NL PICTURE S99 VALUE +13.                  RSA030
132800      05          4-PP-NL PICTURE S99 VALUE +05.                  RSA030
132900      05          4-RR-NL PICTURE S99 VALUE +09.                  RSA030
133000      05          4-TT-NL PICTURE S99 VALUE +08.                  RSA030
133100      05          4-VV-NL PICTURE S99 VALUE +05.                  RSA030
133200      05          4-WW-NL PICTURE S99 VALUE +07.                  RSA030
133300 01  COMPTEURS-ET-VARIABLES-EDITION.                              RSA030
133400      05         COMPTEURS     COMPUTATIONAL-3.                   RSA030
133500      10       5-EW00-4CLM PICTURE S999 VALUE +60.                RSA030
133600      10       5-EW00-4CE  PICTURE S9(9) VALUE ZERO.              RSA030
133700      10       5-EW00-4CL  PICTURE S999 VALUE +60.                RSA030
133800      10       5-EW00-4CL1 PICTURE S999 VALUE +60.                RSA030
133900      10       5-EW00-4CP  PICTURE S9(7) VALUE ZERO.              RSA030
134000      05       5-EW00-4DP  PICTURE X    VALUE '1'.                RSA030
134100      05         ST-SLS.                                          RSA030
134200      10         STX      PICTURE XX.                             RSA030
134300      10         ST9  REDEFINES STX PICTURE 99.                   RSA030
134400      10         J02      PICTURE 99.                             RSA030
134500      10         SAUT     PICTURE 99.                             RSA030
134600      05         CATX     PICTURE XX  VALUE SPACE.                RSA030
134700 01  LIBELLES.                                                    RSA030
134800      05            4-LIB.                                        RSA030
134900          10        4-LIB01.                                      RSA030
135000              15  FILLER  PICTURE X(44) VALUE                     RSA030
135100     '                                            '.              RSA030
135200              15  FILLER  PICTURE X(44) VALUE                     RSA030
135300     '                                            '.              RSA030
135400              15  FILLER  PICTURE X(44) VALUE                     RSA030
135500     '                                            '.              RSA030
135600          10        4-LIB02.                                      RSA030
135700              15  FILLER  PICTURE X(44) VALUE                     RSA030
135800     'GROUPAMA - RENTES                           '.              RSA030
135900              15  FILLER  PICTURE X(44) VALUE                     RSA030
136000     '                   RENTES                   '.              RSA030
136100              15  FILLER  PICTURE X(44) VALUE                     RSA030
136200     '          DATE : 99/99/99      PAGE :    1  '.              RSA030
136300          10        4-LIB03.                                      RSA030
136400              15  FILLER  PICTURE X(44) VALUE                     RSA030
136500     '                                            '.              RSA030
136600              15  FILLER  PICTURE X(44) VALUE                     RSA030
136700     '                   ======                   '.              RSA030
136800              15  FILLER  PICTURE X(44) VALUE                     RSA030
136900     '                                            '.              RSA030
137000          10        4-LIB04.                                      RSA030
137100              15  FILLER  PICTURE X(44) VALUE                     RSA030
137200     '    RENTE NO XXXXXXXXXXXXX                  '.              RSA030
137300              15  FILLER  PICTURE X(44) VALUE                     RSA030
137400     '                                            '.              RSA030
137500              15  FILLER  PICTURE X(44) VALUE                     RSA030
137600     '                                            '.              RSA030
137700          10        4-LIB05.                                      RSA030
137800              15  FILLER  PICTURE X(44) VALUE                     RSA030
137900     '    ======================                  '.              RSA030
138000              15  FILLER  PICTURE X(44) VALUE                     RSA030
138100     '                                            '.              RSA030
138200              15  FILLER  PICTURE X(44) VALUE                     RSA030
138300     '                                            '.              RSA030
138400          10        4-LIB06.                                      RSA030
138500              15  FILLER  PICTURE X(44) VALUE                     RSA030
138600     '       * REFERENCES BANCAIRES DU RENTIER (RS'.              RSA030
138700              15  FILLER  PICTURE X(44) VALUE                     RSA030
138800     '05)                                         '.              RSA030
138900              15  FILLER  PICTURE X(44) VALUE                     RSA030
139000     '                                            '.              RSA030
139100          10        4-LIB07.                                      RSA030
139200              15  FILLER  PICTURE X(44) VALUE                     RSA030
139300     '         -----------------------------------'.              RSA030
139400              15  FILLER  PICTURE X(44) VALUE                     RSA030
139500     '---                                         '.              RSA030
139600              15  FILLER  PICTURE X(44) VALUE                     RSA030
139700     '                                            '.              RSA030
139800          10        4-LIB08.                                      RSA030
139900              15  FILLER  PICTURE X(44) VALUE                     RSA030
140000     '            + NO ORD. MODE PAIEM. (NOPAM) : '.              RSA030
140100              15  FILLER  PICTURE X(44) VALUE                     RSA030
140200     'XX   MODE PAIEM. (CDPAMR) : X    DATE MAJ SI'.              RSA030
140300              15  FILLER  PICTURE X(44) VALUE                     RSA030
140400     'T. BANC. (DMRENB) : 99999999                '.              RSA030
140500          10        4-LIB09.                                      RSA030
140600              15  FILLER  PICTURE X(44) VALUE                     RSA030
140700     '              DOMICILIATION BANC. (NCRENR): '.              RSA030
140800              15  FILLER  PICTURE X(44) VALUE                     RSA030
140900     'XXXXXXXXXXXXXXXXXXXXXXXX         CODE ETABLI'.              RSA030
141000              15  FILLER  PICTURE X(44) VALUE                     RSA030
141100     'SSEMENT (NCREER)  : XXXXX                   '.              RSA030
141200          10        4-LIB10.                                      RSA030
141300              15  FILLER  PICTURE X(44) VALUE                     RSA030
141400     '              CODE GUICHET (NCREGR) : XXXXX '.              RSA030
141500              15  FILLER  PICTURE X(44) VALUE                     RSA030
141600     '     NUMERO COMPTE (NCREBR): XXXXXXXXXXX    '.              RSA030
141700              15  FILLER  PICTURE X(44) VALUE                     RSA030
141800     'CLE COMPTE BANC. (NCRECR) : XXX             '.              RSA030
141900          10        4-LIB11.                                      RSA030
142000              15  FILLER  PICTURE X(44) VALUE                     RSA030
142100     '       * HISTORIQUE MONTANTS ANNUELS (RS06) '.              RSA030
142200              15  FILLER  PICTURE X(44) VALUE                     RSA030
142300     '                                            '.              RSA030
142400              15  FILLER  PICTURE X(44) VALUE                     RSA030
142500     '                                            '.              RSA030
142600          10        4-LIB12.                                      RSA030
142700              15  FILLER  PICTURE X(44) VALUE                     RSA030
142800     '         ---------------------------------- '.              RSA030
142900              15  FILLER  PICTURE X(44) VALUE                     RSA030
143000     '                                            '.              RSA030
143100              15  FILLER  PICTURE X(44) VALUE                     RSA030
143200     '                                            '.              RSA030
143300          10        4-LIB13.                                      RSA030
143400              15  FILLER  PICTURE X(44) VALUE                     RSA030
143500     '            + DATE MAJ (DMREN) : 99999999   '.              RSA030
143600              15  FILLER  PICTURE X(44) VALUE                     RSA030
143700     ' DATE DEPART RENTE (DDREN) : 99999999    MON'.              RSA030
143800              15  FILLER  PICTURE X(44) VALUE                     RSA030
143900     'TANT ANNUEL RENTE (MTREA): -9999999999,99   '.              RSA030
144000          10        4-LIB14.                                      RSA030
144100              15  FILLER  PICTURE X(44) VALUE                     RSA030
144200     '              DATE ACCORD (DDREJ) : 99999999'.              RSA030
144300              15  FILLER  PICTURE X(44) VALUE                     RSA030
144400     '   DATE CONSTIT. RENTE (DTREC) : 99999999   '.              RSA030
144500              15  FILLER  PICTURE X(44) VALUE                     RSA030
144600     ' TAUX INVALID. RENTIER (TXREI) : 999,99     '.              RSA030
144700          10        4-LIB15.                                      RSA030
144800              15  FILLER  PICTURE X(44) VALUE                     RSA030
144900     '       * HISTORIQUE MOUVEMENTS CAPITAUX (RS0'.              RSA030
145000              15  FILLER  PICTURE X(44) VALUE                     RSA030
145100     '7)                                          '.              RSA030
145200              15  FILLER  PICTURE X(44) VALUE                     RSA030
145300     '                                            '.              RSA030
145400          10        4-LIB16.                                      RSA030
145500              15  FILLER  PICTURE X(44) VALUE                     RSA030
145600     '         -----------------------------------'.              RSA030
145700              15  FILLER  PICTURE X(44) VALUE                     RSA030
145800     '--                                          '.              RSA030
145900              15  FILLER  PICTURE X(44) VALUE                     RSA030
146000     '                                            '.              RSA030
146100          10        4-LIB17.                                      RSA030
146200              15  FILLER  PICTURE X(44) VALUE                     RSA030
146300     '            + DATE MAJ (DMREN) : 99999999   '.              RSA030
146400              15  FILLER  PICTURE X(44) VALUE                     RSA030
146500     ' MONTANT CAPITAL (MKREN) : -9999999999,99   '.              RSA030
146600              15  FILLER  PICTURE X(44) VALUE                     RSA030
146700     'CODE SENS ECR. COMPT. (CSREE) : X           '.              RSA030
146800          10        4-LIB18.                                      RSA030
146900              15  FILLER  PICTURE X(44) VALUE                     RSA030
147000     '              MONTANT ARRERAGE A CONSTITUTIO'.              RSA030
147100              15  FILLER  PICTURE X(44) VALUE                     RSA030
147200     'N (MOREC) : 99999999999,99    COMMENTAIRES ('.              RSA030
147300              15  FILLER  PICTURE X(44) VALUE                     RSA030
147400     'LIREK): XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX      '.              RSA030
147500          10        4-LIB19.                                      RSA030
147600              15  FILLER  PICTURE X(44) VALUE                     RSA030
147700     '       * RENTE AT VIAGER (RS08)             '.              RSA030
147800              15  FILLER  PICTURE X(44) VALUE                     RSA030
147900     '                                            '.              RSA030
148000              15  FILLER  PICTURE X(44) VALUE                     RSA030
148100     '                                            '.              RSA030
148200          10        4-LIB20.                                      RSA030
148300              15  FILLER  PICTURE X(44) VALUE                     RSA030
148400     '         ----------------------             '.              RSA030
148500              15  FILLER  PICTURE X(44) VALUE                     RSA030
148600     '                                            '.              RSA030
148700              15  FILLER  PICTURE X(44) VALUE                     RSA030
148800     '                                            '.              RSA030
148900          10        4-LIB21.                                      RSA030
149000              15  FILLER  PICTURE X(44) VALUE                     RSA030
149100     '            + DATE ACCORD (DDREJ) : 99999999'.              RSA030
149200              15  FILLER  PICTURE X(44) VALUE                     RSA030
149300     '    CODE SIEGE TRIBUNAL (CDREJ) : XXX    COD'.              RSA030
149400              15  FILLER  PICTURE X(44) VALUE                     RSA030
149500     'E TYPE RENTIER (CTRER) : X                  '.              RSA030
149600          10        4-LIB22.                                      RSA030
149700              15  FILLER  PICTURE X(44) VALUE                     RSA030
149800     '              CODE TYPE ACCID. (CTREA) : X  '.              RSA030
149900              15  FILLER  PICTURE X(44) VALUE                     RSA030
150000     '    TAUX INVALIDITE (TXREI) : 999,99     COD'.              RSA030
150100              15  FILLER  PICTURE X(44) VALUE                     RSA030
150200     'E EXIST. TIERCE PERS. (CERETP) : X          '.              RSA030
150300          10        4-LIB23.                                      RSA030
150400              15  FILLER  PICTURE X(44) VALUE                     RSA030
150500     '              MONTANT SALAIRE REGLT (MTRES) '.              RSA030
150600              15  FILLER  PICTURE X(44) VALUE                     RSA030
150700     ': -9999999999,99      MONTANT ARRERAGES A CO'.              RSA030
150800              15  FILLER  PICTURE X(44) VALUE                     RSA030
150900     'NSTITUTION (MOREC) : 9999999999,99          '.              RSA030
151000          10        4-LIB24.                                      RSA030
151100              15  FILLER  PICTURE X(44) VALUE                     RSA030
151200     '       * RENTE AT ORPHELIN (RS09)           '.              RSA030
151300              15  FILLER  PICTURE X(44) VALUE                     RSA030
151400     '                                            '.              RSA030
151500              15  FILLER  PICTURE X(44) VALUE                     RSA030
151600     '                                            '.              RSA030
151700          10        4-LIB25.                                      RSA030
151800              15  FILLER  PICTURE X(44) VALUE                     RSA030
151900     '         ------------------------           '.              RSA030
152000              15  FILLER  PICTURE X(44) VALUE                     RSA030
152100     '                                            '.              RSA030
152200              15  FILLER  PICTURE X(44) VALUE                     RSA030
152300     '                                            '.              RSA030
152400          10        4-LIB26.                                      RSA030
152500              15  FILLER  PICTURE X(44) VALUE                     RSA030
152600     '            + TAUX RENTE ORPHELIN (TXREO) : '.              RSA030
152700              15  FILLER  PICTURE X(44) VALUE                     RSA030
152800     '999,99    CODE SITUAT. ORPHELIN (CEREO) : X '.              RSA030
152900              15  FILLER  PICTURE X(44) VALUE                     RSA030
153000     '   MONT. ARRERAGE (MOREC) : 99999999999,99  '.              RSA030
153100          10        4-LIB27.                                      RSA030
153200              15  FILLER  PICTURE X(44) VALUE                     RSA030
153300     '              NOM ORPHELIN (LNRENO) : XXXXXX'.              RSA030
153400              15  FILLER  PICTURE X(44) VALUE                     RSA030
153500     'XXXXXXXXXXXXXX   PRENOM (LNREPO) : XXXXXXXXX'.              RSA030
153600              15  FILLER  PICTURE X(44) VALUE                     RSA030
153700     'XXX    SEXE (CDREXO) : X                    '.              RSA030
153800          10        4-LIB28.                                      RSA030
153900              15  FILLER  PICTURE X(44) VALUE                     RSA030
154000     '              DATE NAISSANCE (DTRENO) : 9999'.              RSA030
154100              15  FILLER  PICTURE X(44) VALUE                     RSA030
154200     '9999             NO RENTE REP. (NORENR) : XX'.              RSA030
154300              15  FILLER  PICTURE X(44) VALUE                     RSA030
154400     'XXXXXXXXXXX                                 '.              RSA030
154500          10        4-LIB29.                                      RSA030
154600              15  FILLER  PICTURE X(44) VALUE                     RSA030
154700     '       * RENTE AAEXA (RS10)                 '.              RSA030
154800              15  FILLER  PICTURE X(44) VALUE                     RSA030
154900     '                                            '.              RSA030
155000              15  FILLER  PICTURE X(44) VALUE                     RSA030
155100     '                                            '.              RSA030
155200          10        4-LIB30.                                      RSA030
155300              15  FILLER  PICTURE X(44) VALUE                     RSA030
155400     '         ------------------                 '.              RSA030
155500              15  FILLER  PICTURE X(44) VALUE                     RSA030
155600     '                                            '.              RSA030
155700              15  FILLER  PICTURE X(44) VALUE                     RSA030
155800     '                                            '.              RSA030
155900          10        4-LIB31.                                      RSA030
156000              15  FILLER  PICTURE X(44) VALUE                     RSA030
156100     '            + CODE TYPE RENTIER (CTRER) : X '.              RSA030
156200              15  FILLER  PICTURE X(44) VALUE                     RSA030
156300     '   CODE TYPE ACCID. (CTREA) : X    MONTANT A'.              RSA030
156400              15  FILLER  PICTURE X(44) VALUE                     RSA030
156500     'RRERAGES (MOREC) : 99999999999,99           '.              RSA030
156600          10        4-LIB32.                                      RSA030
156700              15  FILLER  PICTURE X(44) VALUE                     RSA030
156800     '              CODE COMPL. RENTE AAEXA (CTREE'.              RSA030
156900              15  FILLER  PICTURE X(44) VALUE                     RSA030
157000     'X) : X     DATE DEPART SINISTRES (DTREO) : 9'.              RSA030
157100              15  FILLER  PICTURE X(44) VALUE                     RSA030
157200     '9999999                                     '.              RSA030
157300          10        4-LIB33.                                      RSA030
157400              15  FILLER  PICTURE X(44) VALUE                     RSA030
157500     '              MONT. PAIEM. SIN. (MTPAC) : 99'.              RSA030
157600              15  FILLER  PICTURE X(44) VALUE                     RSA030
157700     '999999999,99                                '.              RSA030
157800              15  FILLER  PICTURE X(44) VALUE                     RSA030
157900     '                                            '.              RSA030
158000          10        4-LIB34.                                      RSA030
158100              15  FILLER  PICTURE X(44) VALUE                     RSA030
158200     '       * PROTHESES (RS11)                   '.              RSA030
158300              15  FILLER  PICTURE X(44) VALUE                     RSA030
158400     '                                            '.              RSA030
158500              15  FILLER  PICTURE X(44) VALUE                     RSA030
158600     '                                            '.              RSA030
158700          10        4-LIB35.                                      RSA030
158800              15  FILLER  PICTURE X(44) VALUE                     RSA030
158900     '         ----------------                   '.              RSA030
159000              15  FILLER  PICTURE X(44) VALUE                     RSA030
159100     '                                            '.              RSA030
159200              15  FILLER  PICTURE X(44) VALUE                     RSA030
159300     '                                            '.              RSA030
159400          10        4-LIB36.                                      RSA030
159500              15  FILLER  PICTURE X(44) VALUE                     RSA030
159600     '         + TYPE RENTIER(CTRER): X TYPE ACCID'.              RSA030
159700              15  FILLER  PICTURE X(44) VALUE                     RSA030
159800     '(CTREA): X TAUX COUT PROTH(TXPOO): 999 LIB P'.              RSA030
159900              15  FILLER  PICTURE X(44) VALUE                     RSA030
160000     'ROTH(LIPOT): XXXXXXXXXXXXXXXXXXXXXXXX       '.              RSA030
160100          10        4-LIB37.                                      RSA030
160200              15  FILLER  PICTURE X(44) VALUE                     RSA030
160300     '         LIB PROTH(LIPOTB): XXXXXXXXXXXXXXXX'.              RSA030
160400              15  FILLER  PICTURE X(44) VALUE                     RSA030
160500     'XXXXXXXX MT COUT PROT(MTPOO):  999999999,99 '.              RSA030
160600              15  FILLER  PICTURE X(44) VALUE                     RSA030
160700     ' MT ANN CR(MTRECR):   999999999,99          '.              RSA030
160800          10        4-LIB38.                                      RSA030
160900              15  FILLER  PICTURE X(44) VALUE                     RSA030
161000     '       * RENTE SS (RS12)                    '.              RSA030
161100              15  FILLER  PICTURE X(44) VALUE                     RSA030
161200     '                                            '.              RSA030
161300              15  FILLER  PICTURE X(44) VALUE                     RSA030
161400     '                                            '.              RSA030
161500          10        4-LIB39.                                      RSA030
161600              15  FILLER  PICTURE X(44) VALUE                     RSA030
161700     '         ---------------                    '.              RSA030
161800              15  FILLER  PICTURE X(44) VALUE                     RSA030
161900     '                                            '.              RSA030
162000              15  FILLER  PICTURE X(44) VALUE                     RSA030
162100     '                                            '.              RSA030
162200          10        4-LIB40.                                      RSA030
162300              15  FILLER  PICTURE X(44) VALUE                     RSA030
162400     '            + DATE ACCORD (DDREJ) : 99999999'.              RSA030
162500              15  FILLER  PICTURE X(44) VALUE                     RSA030
162600     '    CODE SIEGE TRIBUNAL (CDREJ) : XXX       '.              RSA030
162700              15  FILLER  PICTURE X(44) VALUE                     RSA030
162800     '  MONT. ARRERAGES (MOREC) : 99999999999,99  '.              RSA030
162900          10        4-LIB41.                                      RSA030
163000              15  FILLER  PICTURE X(44) VALUE                     RSA030
163100     '              DATE DEP.SIN.(DTREO): 99999999'.              RSA030
163200              15  FILLER  PICTURE X(44) VALUE                     RSA030
163300     '    MT PAIEM. SIN. (MTPAC) : 99999999999,99 '.              RSA030
163400              15  FILLER  PICTURE X(44) VALUE                     RSA030
163500     '  NOM ASSURE (LNRENS): XXXXXXXXXXXXXXXXXXXX '.              RSA030
163600          10        4-LIB42.                                      RSA030
163700              15  FILLER  PICTURE X(44) VALUE                     RSA030
163800     '              NO SOUSCRIPT. (NORES): XXXXXX '.              RSA030
163900              15  FILLER  PICTURE X(44) VALUE                     RSA030
164000     '    NOM ADVERS. (LNRENA) : XXXXXXXXXXXXXXXXX'.              RSA030
164100              15  FILLER  PICTURE X(44) VALUE                     RSA030
164200     'XXX   TX INVALID. RENTIER (TXREI) : 999,99  '.              RSA030
164300          10        4-LIB43.                                      RSA030
164400              15  FILLER  PICTURE X(44) VALUE                     RSA030
164500     '              NAT. CAP. (CNREK) : XXX       '.              RSA030
164600              15  FILLER  PICTURE X(44) VALUE                     RSA030
164700     '    MT CAP. LIM. (MKREL) : 99999999999,99   '.              RSA030
164800              15  FILLER  PICTURE X(44) VALUE                     RSA030
164900     '  CATEG. MINISTERE (CCREM) : X              '.              RSA030
165000          10        4-LIB44.                                      RSA030
165100              15  FILLER  PICTURE X(44) VALUE                     RSA030
165200     '              CODE PARTENAIRE(CDREB): X     '.              RSA030
165300              15  FILLER  PICTURE X(44) VALUE                     RSA030
165400     '    CODE TYPE RENTE SS (CTRESS): X          '.              RSA030
165500              15  FILLER  PICTURE X(44) VALUE                     RSA030
165600     '  PROTOCOLE OS (CDREPS) : X                 '.              RSA030
165700          10        4-LIB45.                                      RSA030
165800              15  FILLER  PICTURE X(44) VALUE                     RSA030
165900     '              NATURE ORG. SOC.(CNREOS): X   '.              RSA030
166000              15  FILLER  PICTURE X(44) VALUE                     RSA030
166100     '                                            '.              RSA030
166200              15  FILLER  PICTURE X(44) VALUE                     RSA030
166300     '                                            '.              RSA030
166400          10        4-LIB46.                                      RSA030
166500              15  FILLER  PICTURE X(44) VALUE                     RSA030
166600     '       * RENTES DROIT COMMUN (RS13)         '.              RSA030
166700              15  FILLER  PICTURE X(44) VALUE                     RSA030
166800     '                                            '.              RSA030
166900              15  FILLER  PICTURE X(44) VALUE                     RSA030
167000     '                                            '.              RSA030
167100          10        4-LIB47.                                      RSA030
167200              15  FILLER  PICTURE X(44) VALUE                     RSA030
167300     '         --------------------------         '.              RSA030
167400              15  FILLER  PICTURE X(44) VALUE                     RSA030
167500     '                                            '.              RSA030
167600              15  FILLER  PICTURE X(44) VALUE                     RSA030
167700     '                                            '.              RSA030
167800          10        4-LIB48.                                      RSA030
167900              15  FILLER  PICTURE X(44) VALUE                     RSA030
168000     '            + DATE ACCORD (DDREJ) : 99999999'.              RSA030
168100              15  FILLER  PICTURE X(44) VALUE                     RSA030
168200     '    CODE SIEGE TRIBUNAL (CDREJ) : XXX       '.              RSA030
168300              15  FILLER  PICTURE X(44) VALUE                     RSA030
168400     '  MONT. ARRERAGES (MOREC) : 99999999999,99  '.              RSA030
168500          10        4-LIB49.                                      RSA030
168600              15  FILLER  PICTURE X(44) VALUE                     RSA030
168700     '              DATE DEP.SIN.(DTREO): 99999999'.              RSA030
168800              15  FILLER  PICTURE X(44) VALUE                     RSA030
168900     '    MT PAIEM. SIN. (MTPAC) : 99999999999,99 '.              RSA030
169000              15  FILLER  PICTURE X(44) VALUE                     RSA030
169100     '  NOM ASSURE (LNRENS): XXXXXXXXXXXXXXXXXXXX '.              RSA030
169200          10        4-LIB50.                                      RSA030
169300              15  FILLER  PICTURE X(44) VALUE                     RSA030
169400     '              NO SOUSCRIPT. (NORES) : XXXXXX'.              RSA030
169500              15  FILLER  PICTURE X(44) VALUE                     RSA030
169600     '    NOM ADVERS. (LNRENA) : XXXXXXXXXXXXXXXXX'.              RSA030
169700              15  FILLER  PICTURE X(44) VALUE                     RSA030
169800     'XXX   TX INVALID. RENTIER (TXREI) : 999,99  '.              RSA030
169900          10        4-LIB51.                                      RSA030
170000              15  FILLER  PICTURE X(44) VALUE                     RSA030
170100     '              NAT. CAP. (CNREK) : XXX       '.              RSA030
170200              15  FILLER  PICTURE X(44) VALUE                     RSA030
170300     '    MT CAP. LIM. (MKREL) : 99999999999,99   '.              RSA030
170400              15  FILLER  PICTURE X(44) VALUE                     RSA030
170500     '  MONT. ANNUEL (MTREAO) : -9999999999,99    '.              RSA030
170600          10        4-LIB52.                                      RSA030
170700              15  FILLER  PICTURE X(44) VALUE                     RSA030
170800     '              TYPE MAJ DRT COM. (CTREDC) : X'.              RSA030
170900              15  FILLER  PICTURE X(44) VALUE                     RSA030
171000     '    CATEG. MINISTERE (CCREM): X             '.              RSA030
171100              15  FILLER  PICTURE X(44) VALUE                     RSA030
171200     '  CODE PARTNRE (CDREB) : X                  '.              RSA030
171300          10        4-LIB53.                                      RSA030
171400              15  FILLER  PICTURE X(44) VALUE                     RSA030
171500     '              TYPE TERME JUGEMENT (CTREJ): X'.              RSA030
171600              15  FILLER  PICTURE X(44) VALUE                     RSA030
171700     '                                            '.              RSA030
171800              15  FILLER  PICTURE X(44) VALUE                     RSA030
171900     '  COEF. REVALO. (KMREDC): 999,99999         '.              RSA030
172000          10        4-LIB54.                                      RSA030
172100              15  FILLER  PICTURE X(44) VALUE                     RSA030
172200     '              APPL. TAB. CAP.(CDREAT): X    '.              RSA030
172300              15  FILLER  PICTURE X(44) VALUE                     RSA030
172400     '    DATE NAIS. VICTIME (DTRENV): 99999999   '.              RSA030
172500              15  FILLER  PICTURE X(44) VALUE                     RSA030
172600     '  SEXE (CDREX) : X                          '.              RSA030
172700          10        4-LIB55.                                      RSA030
172800              15  FILLER  PICTURE X(44) VALUE                     RSA030
172900     '       * COTISATIONS AS (RS14)              '.              RSA030
173000              15  FILLER  PICTURE X(44) VALUE                     RSA030
173100     '                                            '.              RSA030
173200              15  FILLER  PICTURE X(44) VALUE                     RSA030
173300     '                                            '.              RSA030
173400          10        4-LIB56.                                      RSA030
173500              15  FILLER  PICTURE X(44) VALUE                     RSA030
173600     '         ---------------------              '.              RSA030
173700              15  FILLER  PICTURE X(44) VALUE                     RSA030
173800     '                                            '.              RSA030
173900              15  FILLER  PICTURE X(44) VALUE                     RSA030
174000     '                                            '.              RSA030
174100          10        4-LIB57.                                      RSA030
174200              15  FILLER  PICTURE X(44) VALUE                     RSA030
174300     '            + NO CAISSE REG. MSA (NOCRM) : 9'.              RSA030
174400              15  FILLER  PICTURE X(44) VALUE                     RSA030
174500     '9   NO IMMAT. RENTIER (MOREMS): 999999999999'.              RSA030
174600              15  FILLER  PICTURE X(44) VALUE                     RSA030
174700     '999  CODE TX ASA (CDRETX): XXX              '.              RSA030
174800          10        4-LIB58.                                      RSA030
174900              15  FILLER  PICTURE X(44) VALUE                     RSA030
175000     '              TAUX ASA (TXREAS) : 99,99     '.              RSA030
175100              15  FILLER  PICTURE X(44) VALUE                     RSA030
175200     '    NO RENTE REF. (NORENR): XXXXXXXXXXXXX   '.              RSA030
175300              15  FILLER  PICTURE X(44) VALUE                     RSA030
175400     '                                            '.              RSA030
175500          10        4-LIB59.                                      RSA030
175600              15  FILLER  PICTURE X(44) VALUE                     RSA030
175700     '       * COTISATIONS AMEXA (RS15)           '.              RSA030
175800              15  FILLER  PICTURE X(44) VALUE                     RSA030
175900     '                                            '.              RSA030
176000              15  FILLER  PICTURE X(44) VALUE                     RSA030
176100     '                                            '.              RSA030
176200          10        4-LIB60.                                      RSA030
176300              15  FILLER  PICTURE X(44) VALUE                     RSA030
176400     '         ------------------------           '.              RSA030
176500              15  FILLER  PICTURE X(44) VALUE                     RSA030
176600     '                                            '.              RSA030
176700              15  FILLER  PICTURE X(44) VALUE                     RSA030
176800     '                                            '.              RSA030
176900          10        4-LIB61.                                      RSA030
177000              15  FILLER  PICTURE X(44) VALUE                     RSA030
177100     '            + ANNEE REF. (PEREA) : XXXX     '.              RSA030
177200              15  FILLER  PICTURE X(44) VALUE                     RSA030
177300     '    MONTANT ANNUITE (MTRECR): 99999999999,99'.              RSA030
177400              15  FILLER  PICTURE X(44) VALUE                     RSA030
177500     '  NO RENTE REF (NORENR): XXXXXXXXXXXXX      '.              RSA030
177600          10        4-LIB62.                                      RSA030
177700              15  FILLER  PICTURE X(44) VALUE                     RSA030
177800     '       * HISTORIQUE DESTINATAIRE RENTE (RS16'.              RSA030
177900              15  FILLER  PICTURE X(44) VALUE                     RSA030
178000     ')                                           '.              RSA030
178100              15  FILLER  PICTURE X(44) VALUE                     RSA030
178200     '                                            '.              RSA030
178300          10        4-LIB63.                                      RSA030
178400              15  FILLER  PICTURE X(44) VALUE                     RSA030
178500     '         -----------------------------------'.              RSA030
178600              15  FILLER  PICTURE X(44) VALUE                     RSA030
178700     '-                                           '.              RSA030
178800              15  FILLER  PICTURE X(44) VALUE                     RSA030
178900     '                                            '.              RSA030
179000          10        4-LIB64.                                      RSA030
179100              15  FILLER  PICTURE X(44) VALUE                     RSA030
179200     '            + DATE MAJ (DMREN) : 99999999   '.              RSA030
179300              15  FILLER  PICTURE X(44) VALUE                     RSA030
179400     '    NO TIERS (NOTIE) : 999999               '.              RSA030
179500              15  FILLER  PICTURE X(44) VALUE                     RSA030
179600     '                                            '.              RSA030
179700          10        4-LIB65.                                      RSA030
179800              15  FILLER  PICTURE X(44) VALUE                     RSA030
179900     '       * RECOURS (RS17)                     '.              RSA030
180000              15  FILLER  PICTURE X(44) VALUE                     RSA030
180100     '                                            '.              RSA030
180200              15  FILLER  PICTURE X(44) VALUE                     RSA030
180300     '                                            '.              RSA030
180400          10        4-LIB66.                                      RSA030
180500              15  FILLER  PICTURE X(44) VALUE                     RSA030
180600     '         --------------                     '.              RSA030
180700              15  FILLER  PICTURE X(44) VALUE                     RSA030
180800     '                                            '.              RSA030
180900              15  FILLER  PICTURE X(44) VALUE                     RSA030
181000     '                                            '.              RSA030
181100          10        4-LIB67.                                      RSA030
181200              15  FILLER  PICTURE X(44) VALUE                     RSA030
181300     '            + NO RECOURS (NOCRS): 99        '.              RSA030
181400              15  FILLER  PICTURE X(44) VALUE                     RSA030
181500     '   DATE MAJ (DMREN) : 99999999              '.              RSA030
181600              15  FILLER  PICTURE X(44) VALUE                     RSA030
181700     '  ETAT AVANCEMENT (CERCS) : X               '.              RSA030
181800          10        4-LIB68.                                      RSA030
181900              15  FILLER  PICTURE X(44) VALUE                     RSA030
182000     '              REF. RECOURS (LIRCS): XXXXXXXX'.              RSA030
182100              15  FILLER  PICTURE X(44) VALUE                     RSA030
182200     'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX            '.              RSA030
182300              15  FILLER  PICTURE X(44) VALUE                     RSA030
182400     '  DATE DEPART RECOURS (DDRCS) : 99999999    '.              RSA030
182500          10        4-LIB69.                                      RSA030
182600              15  FILLER  PICTURE X(44) VALUE                     RSA030
182700     '              NAT. CAPIT. (CTRCK) : X       '.              RSA030
182800              15  FILLER  PICTURE X(44) VALUE                     RSA030
182900     '   MONTANT CAP. LIM.: 99999999999,99        '.              RSA030
183000              15  FILLER  PICTURE X(44) VALUE                     RSA030
183100     '  DATE ECHEANCE (DHRCS) : 99999999          '.              RSA030
183200          10        4-LIB70.                                      RSA030
183300              15  FILLER  PICTURE X(44) VALUE                     RSA030
183400     '              TYPE REMB.(CTRCB)   : X       '.              RSA030
183500              15  FILLER  PICTURE X(44) VALUE                     RSA030
183600     '   MT RAPPEL (MTRCA): 99999999999,99        '.              RSA030
183700              15  FILLER  PICTURE X(44) VALUE                     RSA030
183800     '  MT REGUL. (MTRCG) : -9999999999,99        '.              RSA030
183900          10        4-LIB71.                                      RSA030
184000              15  FILLER  PICTURE X(44) VALUE                     RSA030
184100     '              MT TOT. VERSE(MCRCCR): 9999999'.              RSA030
184200              15  FILLER  PICTURE X(44) VALUE                     RSA030
184300     '9999,99   DATE REMB.(DTRCB): 99999999       '.              RSA030
184400              15  FILLER  PICTURE X(44) VALUE                     RSA030
184500     '  MT REMB. (MTRCB)  : 99999999999,99        '.              RSA030
184600          10        4-LIB72.                                      RSA030
184700              15  FILLER  PICTURE X(44) VALUE                     RSA030
184800     '              CIE ASSURANCE (LNRCA): XXXXXXX'.              RSA030
184900              15  FILLER  PICTURE X(44) VALUE                     RSA030
185000     'XXXXXXXXXXXXXXXXXXXXXXXXX                   '.              RSA030
185100              15  FILLER  PICTURE X(44) VALUE                     RSA030
185200     '  MT ECHEANCE (MORCS): 99999999999,99       '.              RSA030
185300          10        4-LIB73.                                      RSA030
185400              15  FILLER  PICTURE X(44) VALUE                     RSA030
185500     '              ADRESSE (LRRE1,LRRE2): XXXXXXX'.              RSA030
185600              15  FILLER  PICTURE X(44) VALUE                     RSA030
185700     'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX, XXXXXXXXXXXXX'.              RSA030
185800              15  FILLER  PICTURE X(44) VALUE                     RSA030
185900     'XXXXXXXXXXXXXXXXXXX                         '.              RSA030
186000          10        4-LIB74.                                      RSA030
186100              15  FILLER  PICTURE X(44) VALUE                     RSA030
186200     '              VILLE (LVRE)         : XXXXXXX'.              RSA030
186300              15  FILLER  PICTURE X(44) VALUE                     RSA030
186400     'XXXXXXXXXXXXXXXXXXXXXXXX                    '.              RSA030
186500              15  FILLER  PICTURE X(44) VALUE                     RSA030
186600     '  CODE POSTAL (CPREN) : XXXXX               '.              RSA030
186700          10        4-LIB75.                                      RSA030
186800              15  FILLER  PICTURE X(44) VALUE                     RSA030
186900     '              REF.RECOURS (LIRCSB) : XXXXXXX'.              RSA030
187000              15  FILLER  PICTURE X(44) VALUE                     RSA030
187100     'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX           '.              RSA030
187200              15  FILLER  PICTURE X(44) VALUE                     RSA030
187300     '  PAYS        (CDREP) : XXX                 '.              RSA030
187400          10        4-LIB76.                                      RSA030
187500              15  FILLER  PICTURE X(44) VALUE                     RSA030
187600     '                 + REMBOURSEMENTS RECOURS (R'.              RSA030
187700              15  FILLER  PICTURE X(44) VALUE                     RSA030
187800     'S18)                                        '.              RSA030
187900              15  FILLER  PICTURE X(44) VALUE                     RSA030
188000     '                                            '.              RSA030
188100          10        4-LIB77.                                      RSA030
188200              15  FILLER  PICTURE X(44) VALUE                     RSA030
188300     '                   -------------------------'.              RSA030
188400              15  FILLER  PICTURE X(44) VALUE                     RSA030
188500     '----                                        '.              RSA030
188600              15  FILLER  PICTURE X(44) VALUE                     RSA030
188700     '                                            '.              RSA030
188800          10        4-LIB78.                                      RSA030
188900              15  FILLER  PICTURE X(44) VALUE                     RSA030
189000     '                    + DATE REMB.(DTRCB) : 99'.              RSA030
189100              15  FILLER  PICTURE X(44) VALUE                     RSA030
189200     '999999    MT REMB. (MTRCBH): 99999999999,99 '.              RSA030
189300              15  FILLER  PICTURE X(44) VALUE                     RSA030
189400     '  DATE MAJ (DMREN) : 99999999               '.              RSA030
189500          10        4-LIB79.                                      RSA030
189600              15  FILLER  PICTURE X(44) VALUE                     RSA030
189700     '       * PAIEMENTS (RS19)                   '.              RSA030
189800              15  FILLER  PICTURE X(44) VALUE                     RSA030
189900     '                                            '.              RSA030
190000              15  FILLER  PICTURE X(44) VALUE                     RSA030
190100     '                                            '.              RSA030
190200          10        4-LIB80.                                      RSA030
190300              15  FILLER  PICTURE X(44) VALUE                     RSA030
190400     '         ----------------                   '.              RSA030
190500              15  FILLER  PICTURE X(44) VALUE                     RSA030
190600     '                                            '.              RSA030
190700              15  FILLER  PICTURE X(44) VALUE                     RSA030
190800     '                                            '.              RSA030
190900          10        4-LIB81.                                      RSA030
191000              15  FILLER  PICTURE X(44) VALUE                     RSA030
191100     '            + NO PAIEM. (NOPAI) : 999       '.              RSA030
191200              15  FILLER  PICTURE X(44) VALUE                     RSA030
191300     '   DATE ECH. PAIEM.(DHPAI) : 99999999       '.              RSA030
191400              15  FILLER  PICTURE X(44) VALUE                     RSA030
191500     '  DATE PAIEM (DTPAI) : 99999999             '.              RSA030
191600          10        4-LIB82.                                      RSA030
191700              15  FILLER  PICTURE X(44) VALUE                     RSA030
191800     '              MT TOT. PAIEM. (MTPAI): -99999'.              RSA030
191900              15  FILLER  PICTURE X(44) VALUE                     RSA030
192000     '99999,99  MT REGUL. ARRERAGES (MORER) : -999'.              RSA030
192100              15  FILLER  PICTURE X(44) VALUE                     RSA030
192200     '9999999,99                                  '.              RSA030
192300          10        4-LIB83.                                      RSA030
192400              15  FILLER  PICTURE X(44) VALUE                     RSA030
192500     '              CODE DEST. (CDPAD): X         '.              RSA030
192600              15  FILLER  PICTURE X(44) VALUE                     RSA030
192700     '   NO TIERS (NOTIE) : 999999                '.              RSA030
192800              15  FILLER  PICTURE X(44) VALUE                     RSA030
192900     '  NO ORD.MODE PAIEM.(NOPAM): 99             '.              RSA030
193000          10        4-LIB84.                                      RSA030
193100              15  FILLER  PICTURE X(44) VALUE                     RSA030
193200     '              CODE MODE PAIEM(CDPAM): X     '.              RSA030
193300              15  FILLER  PICTURE X(44) VALUE                     RSA030
193400     '   DATE REPAIEM.(DTPAIR) : 99999999         '.              RSA030
193500              15  FILLER  PICTURE X(44) VALUE                     RSA030
193600     '  CODE SITUAT.PAIEM.(CDPAS) : X             '.              RSA030
193700          10        4-LIB85.                                      RSA030
193800              15  FILLER  PICTURE X(44) VALUE                     RSA030
193900     '              MOTIF REIMPUT.(CDPAP) : X     '.              RSA030
194000              15  FILLER  PICTURE X(44) VALUE                     RSA030
194100     '   NO FEUILLET (NOPAF) : XXXXXX             '.              RSA030
194200              15  FILLER  PICTURE X(44) VALUE                     RSA030
194300     '  CODE PAYE REIMPUTE (CDPAI): X             '.              RSA030
194400          10        4-LIB86.                                      RSA030
194500              15  FILLER  PICTURE X(44) VALUE                     RSA030
194600     '       * ORDRES DE PAIEMENTS (RS22)         '.              RSA030
194700              15  FILLER  PICTURE X(44) VALUE                     RSA030
194800     '                                            '.              RSA030
194900              15  FILLER  PICTURE X(44) VALUE                     RSA030
195000     '                                            '.              RSA030
195100          10        4-LIB87.                                      RSA030
195200              15  FILLER  PICTURE X(44) VALUE                     RSA030
195300     '         --------------------------         '.              RSA030
195400              15  FILLER  PICTURE X(44) VALUE                     RSA030
195500     '                                            '.              RSA030
195600              15  FILLER  PICTURE X(44) VALUE                     RSA030
195700     '                                            '.              RSA030
195800          10        4-LIB88.                                      RSA030
195900              15  FILLER  PICTURE X(44) VALUE                     RSA030
196000     '            + DATE MAJ (DMREN) : 99999999   '.              RSA030
196100              15  FILLER  PICTURE X(44) VALUE                     RSA030
196200     '   DATE ECH. PAIEM.(DHPAI) : 99999999       '.              RSA030
196300              15  FILLER  PICTURE X(44) VALUE                     RSA030
196400     '  MT ORD.PAIEM (MTPAIO) : -9999999999,99    '.              RSA030
196500          10        4-LIB89.                                      RSA030
196600              15  FILLER  PICTURE X(44) VALUE                     RSA030
196700     '              RET. ORD. PAIEM.(LIPAO): XXXXX'.              RSA030
196800              15  FILLER  PICTURE X(44) VALUE                     RSA030
196900     'XXXXXXXXXXXXXXXXXXXXXXXXXX                  '.              RSA030
197000              15  FILLER  PICTURE X(44) VALUE                     RSA030
197100     '  NO TIERS (NOTIE) : 999999                 '.              RSA030
197200          10        4-LIB90.                                      RSA030
197300              15  FILLER  PICTURE X(44) VALUE                     RSA030
197400     '              TYPE PAIEM.(CTPAO) : X        '.              RSA030
197500              15  FILLER  PICTURE X(44) VALUE                     RSA030
197600     '   NO PAIEM. RENTE (NOPAI) : 999            '.              RSA030
197700              15  FILLER  PICTURE X(44) VALUE                     RSA030
197800     '  NO RENTE (NOREN) : XXXXXXXXXXXXX          '.              RSA030
197900          10        4-LIB91.                                      RSA030
198000              15  FILLER  PICTURE X(44) VALUE                     RSA030
198100     '              REF. ORDRE PAIEMENT (LIPA2) : '.              RSA030
198200              15  FILLER  PICTURE X(44) VALUE                     RSA030
198300     'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX             '.              RSA030
198400              15  FILLER  PICTURE X(44) VALUE                     RSA030
198500     '                                            '.              RSA030
198600          10        4-LIB92.                                      RSA030
198700              15  FILLER  PICTURE X(44) VALUE                     RSA030
198800     '       * REMBOURSEMENT CCR (RS23)           '.              RSA030
198900              15  FILLER  PICTURE X(44) VALUE                     RSA030
199000     '                                            '.              RSA030
199100              15  FILLER  PICTURE X(44) VALUE                     RSA030
199200     '                                            '.              RSA030
199300          10        4-LIB93.                                      RSA030
199400              15  FILLER  PICTURE X(44) VALUE                     RSA030
199500     '         ------------------------           '.              RSA030
199600              15  FILLER  PICTURE X(44) VALUE                     RSA030
199700     '                                            '.              RSA030
199800              15  FILLER  PICTURE X(44) VALUE                     RSA030
199900     '                                            '.              RSA030
200000          10        4-LIB94.                                      RSA030
200100              15  FILLER  PICTURE X(44) VALUE                     RSA030
200200     '            + MT REMB. FONDS (MTREB): 999999'.              RSA030
200300              15  FILLER  PICTURE X(44) VALUE                     RSA030
200400     '99999,99   EX.REMB.(PXREF): 9999            '.              RSA030
200500              15  FILLER  PICTURE X(44) VALUE                     RSA030
200600     '                                            '.              RSA030
200700          10        4-LIB95.                                      RSA030
200800              15  FILLER  PICTURE X(44) VALUE                     RSA030
200900     '       * AUTRES RENTES  (RS20)              '.              RSA030
201000              15  FILLER  PICTURE X(44) VALUE                     RSA030
201100     '                                            '.              RSA030
201200              15  FILLER  PICTURE X(44) VALUE                     RSA030
201300     '                                            '.              RSA030
201400          10        4-LIB96.                                      RSA030
201500              15  FILLER  PICTURE X(44) VALUE                     RSA030
201600     '         ------------------------           '.              RSA030
201700              15  FILLER  PICTURE X(44) VALUE                     RSA030
201800     '                                            '.              RSA030
201900              15  FILLER  PICTURE X(44) VALUE                     RSA030
202000     '                                            '.              RSA030
202100          10        4-LIB97.                                      RSA030
202200              15  FILLER  PICTURE X(44) VALUE                     RSA030
202300     '            + CODE TYPE ACCIDENT (CTREA) : X'.              RSA030
202400              15  FILLER  PICTURE X(44) VALUE                     RSA030
202500     '           TX INVAL. RENTIER(TXREI): 999,99 '.              RSA030
202600              15  FILLER  PICTURE X(44) VALUE                     RSA030
202700     '  DATE DEP. SIN. (DTREO) : 99999999         '.              RSA030
202800          10        4-LIB98.                                      RSA030
202900              15  FILLER  PICTURE X(44) VALUE                     RSA030
203000     '              MT ANNUEL ORIG (MTREAO) : 9999'.              RSA030
203100              15  FILLER  PICTURE X(44) VALUE                     RSA030
203200     '99999,99   MT SOUSCRIT(MTREAT): 999999999,99'.              RSA030
203300              15  FILLER  PICTURE X(44) VALUE                     RSA030
203400     '                                            '.              RSA030
203500          10        4-LIB99.                                      RSA030
203600              15  FILLER  PICTURE X(44) VALUE                     RSA030
203700     '              MONT. ARRERAGES (MOREC) : 9999'.              RSA030
203800              15  FILLER  PICTURE X(44) VALUE                     RSA030
203900     '99999,99                                    '.              RSA030
204000              15  FILLER  PICTURE X(44) VALUE                     RSA030
204100     '                                            '.              RSA030
204200      05            4-LIB-R REDEFINES 4-LIB.                      RSA030
204300           10   1-LI00-4 OCCURS  099.                             RSA030
204400            15  FILLER        PICTURE  X(00132).                  RSA030
204500 01            6-EW00.                                            RSA030
204600     05        6-EW00-4.                                          RSA030
204700      10       6-EW400-SAUT   PICTURE  X.                         RSA030
204800      10       6-EW400        PICTURE  X(132).                    RSA030
204900      10       6-EW401    REDEFINES   6-EW400.                    RSA030
205000        15     FILLER         PICTURE  X(105).                    RSA030
205100        15     6-EW401-DJREN  PICTURE X(8).                       RSA030
205200        15     FILLER         PICTURE  X(012).                    RSA030
205300        15     6-EW401-ZNOPAG PICTURE ZZZZ9.                      RSA030
205400        15     FILLER         PICTURE  X(002).                    RSA030
205500      10       6-EW402    REDEFINES   6-EW400.                    RSA030
205600        15     FILLER         PICTURE  X(013).                    RSA030
205700        15     6-EW402-NOREN  PICTURE X(13).                      RSA030
205800        15     FILLER         PICTURE  X(106).                    RSA030
205900      10       6-EW403    REDEFINES   6-EW400.                    RSA030
206000        15     FILLER         PICTURE  X(044).                    RSA030
206100        15     6-EW403-NOPAM  PICTURE 99.                         RSA030
206200        15     FILLER         PICTURE  X(026).                    RSA030
206300        15     6-EW403-CDPAMR PICTURE X.                          RSA030
206400        15     FILLER         PICTURE  X(035).                    RSA030
206500        15     6-EW403-DMRENB PICTURE X(10).                      RSA030
206600        15     FILLER         PICTURE  X(014).                    RSA030
206700      10       6-EW404    REDEFINES   6-EW400.                    RSA030
206800        15     FILLER         PICTURE  X(044).                    RSA030
206900        15     6-EW404-NCRENR PICTURE X(24).                      RSA030
207000        15     FILLER         PICTURE  X(040).                    RSA030
207100        15     6-EW404-NCREER PICTURE X(5).                       RSA030
207200        15     FILLER         PICTURE  X(019).                    RSA030
207300      10       6-EW405    REDEFINES   6-EW400.                    RSA030
207400        15     FILLER         PICTURE  X(038).                    RSA030
207500        15     6-EW405-NCREGR PICTURE X(5).                       RSA030
207600        15     FILLER         PICTURE  X(030).                    RSA030
207700        15     6-EW405-NCREBR PICTURE X(11).                      RSA030
207800        15     FILLER         PICTURE  X(032).                    RSA030
207900        15     6-EW405-NCRECR PICTURE XXX.                        RSA030
208000        15     FILLER         PICTURE  X(013).                    RSA030
208100      10       6-EW406    REDEFINES   6-EW400.                    RSA030
208200        15     FILLER         PICTURE  X(033).                    RSA030
208300        15     6-EW406-DMREN  PICTURE X(10).                      RSA030
208400        15     FILLER         PICTURE  X(030).                    RSA030
208500        15     6-EW406-DDREN  PICTURE X(10).                      RSA030
208600        15     FILLER         PICTURE  X(032).                    RSA030
208700        15     6-EW406-MTREA  PICTURE ----B---B--9,99             RSA030
208800                                                    BLANK ZERO.   RSA030
208900        15     FILLER         PICTURE  X(002).                    RSA030
209000      10       6-EW407    REDEFINES   6-EW400.                    RSA030
209100        15     FILLER         PICTURE  X(036).                    RSA030
209200        15     6-EW407-DDREJ  PICTURE X(10).                      RSA030
209300        15     FILLER         PICTURE  X(031).                    RSA030
209400        15     6-EW407-DTREC  PICTURE X(10).                      RSA030
209500        15     FILLER         PICTURE  X(034).                    RSA030
209600        15     6-EW407-TXREI  PICTURE Z99,99.                     RSA030
209700        15     FILLER         PICTURE  X(005).                    RSA030
209800      10       6-EW408    REDEFINES   6-EW400.                    RSA030
209900        15     FILLER         PICTURE  X(033).                    RSA030
210000        15     6-EW408-DMREN  PICTURE X(10).                      RSA030
210100        15     FILLER         PICTURE  X(028).                    RSA030
210200        15     6-EW408-MKREN  PICTURE ----B---B--9,99             RSA030
210300                                                    BLANK ZERO.   RSA030
210400        15     FILLER         PICTURE  X(034).                    RSA030
210500        15     6-EW408-CSREE  PICTURE X.                          RSA030
210600        15     FILLER         PICTURE  X(011).                    RSA030
210700      10       6-EW409    REDEFINES   6-EW400.                    RSA030
210800        15     FILLER         PICTURE  X(056).                    RSA030
210900        15     6-EW409-MOREC  PICTURE ----B---B--9,99.            RSA030
211000        15     FILLER         PICTURE  X(025).                    RSA030
211100        15     6-EW409-LIREK  PICTURE X(30).                      RSA030
211200        15     FILLER         PICTURE  X(006).                    RSA030
211300      10       6-EW410    REDEFINES   6-EW400.                    RSA030
211400        15     FILLER         PICTURE  X(036).                    RSA030
211500        15     6-EW410-DDREJ  PICTURE X(10).                      RSA030
211600        15     FILLER         PICTURE  X(032).                    RSA030
211700        15     6-EW410-CDREJ  PICTURE XXX.                        RSA030
211800        15     FILLER         PICTURE  X(032).                    RSA030
211900        15     6-EW410-CTRER  PICTURE X.                          RSA030
212000        15     FILLER         PICTURE  X(018).                    RSA030
212100      10       6-EW411    REDEFINES   6-EW400.                    RSA030
212200        15     FILLER         PICTURE  X(041).                    RSA030
212300        15     6-EW411-CTREA  PICTURE X.                          RSA030
212400        15     FILLER         PICTURE  X(032).                    RSA030
212500        15     6-EW411-TXREI  PICTURE Z99,99.                     RSA030
212600        15     FILLER         PICTURE  X(041).                    RSA030
212700        15     6-EW411-CERETP PICTURE X.                          RSA030
212800        15     FILLER         PICTURE  X(010).                    RSA030
212900      10       6-EW412    REDEFINES   6-EW400.                    RSA030
213000        15     FILLER         PICTURE  X(046).                    RSA030
213100        15     6-EW412-MTRES  PICTURE ZZZBZZZBZZ9,99.             RSA030
213200        15     FILLER         PICTURE  X(049).                    RSA030
213300        15     6-EW412-MOREC  PICTURE ----B---B--9,99.            RSA030
213400        15     FILLER         PICTURE  X(008).                    RSA030
213500      10       6-EW413    REDEFINES   6-EW400.                    RSA030
213600        15     FILLER         PICTURE  X(044).                    RSA030
213700        15     6-EW413-TXREO  PICTURE Z99,99.                     RSA030
213800        15     FILLER         PICTURE  X(036).                    RSA030
213900        15     6-EW413-CEREO  PICTURE X.                          RSA030
214000        15     FILLER         PICTURE  X(029).                    RSA030
214100        15     6-EW413-MOREC  PICTURE ----B---B--9,99.            RSA030
214200        15     FILLER         PICTURE  X(001).                    RSA030
214300      10       6-EW414    REDEFINES   6-EW400.                    RSA030
214400        15     FILLER         PICTURE  X(038).                    RSA030
214500        15     6-EW414-LNRENO PICTURE X(20).                      RSA030
214600        15     FILLER         PICTURE  X(021).                    RSA030
214700        15     6-EW414-LNREPO PICTURE X(12).                      RSA030
214800        15     FILLER         PICTURE  X(020).                    RSA030
214900        15     6-EW414-CDREXO PICTURE X.                          RSA030
215000        15     FILLER         PICTURE  X(020).                    RSA030
215100      10       6-EW415    REDEFINES   6-EW400.                    RSA030
215200        15     FILLER         PICTURE  X(040).                    RSA030
215300        15     6-EW415-DTRENO PICTURE X(10).                      RSA030
215400        15     FILLER         PICTURE  X(036).                    RSA030
215500        15     6-EW415-NORENR PICTURE X(13).                      RSA030
215600        15     FILLER         PICTURE  X(033).                    RSA030
215700      10       6-EW416    REDEFINES   6-EW400.                    RSA030
215800        15     FILLER         PICTURE  X(042).                    RSA030
215900        15     6-EW416-CTRER  PICTURE X.                          RSA030
216000        15     FILLER         PICTURE  X(031).                    RSA030
216100        15     6-EW416-CTREA  PICTURE X.                          RSA030
216200        15     FILLER         PICTURE  X(032).                    RSA030
216300        15     6-EW416-MOREC  PICTURE ----B---B--9,99.            RSA030
216400        15     FILLER         PICTURE  X(010).                    RSA030
216500      10       6-EW417    REDEFINES   6-EW400.                    RSA030
216600        15     FILLER         PICTURE  X(049).                    RSA030
216700        15     6-EW417-CTREEX PICTURE X.                          RSA030
216800        15     FILLER         PICTURE  X(037).                    RSA030
216900        15     6-EW417-DTREO  PICTURE X(10).                      RSA030
217000        15     FILLER         PICTURE  X(035).                    RSA030
217100      10       6-EW418    REDEFINES   6-EW400.                    RSA030
217200        15     FILLER         PICTURE  X(042).                    RSA030
217300        15     6-EW418-MTPAC  PICTURE ZZZBZZZBZZ9,99.             RSA030
217400        15     FILLER         PICTURE  X(076).                    RSA030
217500      10       6-EW419    REDEFINES   6-EW400.                    RSA030
217600        15     FILLER         PICTURE  X(032).                    RSA030
217700        15     6-EW419-CTRER  PICTURE X.                          RSA030
217800        15     FILLER         PICTURE  X(020).                    RSA030
217900        15     6-EW419-CTREA  PICTURE X.                          RSA030
218000        15     FILLER         PICTURE  X(025).                    RSA030
218100        15     6-EW419-TXPOO  PICTURE 999.                        RSA030
218200        15     FILLER         PICTURE  X(019).                    RSA030
218300        15     6-EW419-LIPOT  PICTURE X(24).                      RSA030
218400        15     FILLER         PICTURE  X(007).                    RSA030
218500      10       6-EW420    REDEFINES   6-EW400.                    RSA030
218600        15     FILLER         PICTURE  X(028).                    RSA030
218700        15     6-EW420-LIPOTB PICTURE X(24).                      RSA030
218800        15     FILLER         PICTURE  X(023).                    RSA030
218900        15     6-EW420-MTPOO  PICTURE ZZZBZZZBZZ9,99.             RSA030
219000        15     FILLER         PICTURE  X(021).                    RSA030
219100        15     6-EW420-MTRECR PICTURE ZZZBZZZBZZ9,99.             RSA030
219200        15     FILLER         PICTURE  X(008).                    RSA030
219300      10       6-EW421    REDEFINES   6-EW400.                    RSA030
219400        15     FILLER         PICTURE  X(036).                    RSA030
219500        15     6-EW421-DDREJ  PICTURE X(10).                      RSA030
219600        15     FILLER         PICTURE  X(032).                    RSA030
219700        15     6-EW421-CDREJ  PICTURE XXX.                        RSA030
219800        15     FILLER         PICTURE  X(035).                    RSA030
219900        15     6-EW421-MOREC  PICTURE ----B---B--9,99.            RSA030
220000        15     FILLER         PICTURE  X(001).                    RSA030
220100      10       6-EW422    REDEFINES   6-EW400.                    RSA030
220200        15     FILLER         PICTURE  X(036).                    RSA030
220300        15     6-EW422-DTREO  PICTURE X(10).                      RSA030
220400        15     FILLER         PICTURE  X(027).                    RSA030
220500        15     6-EW422-MTPAC  PICTURE ZZZBZZZBZZ9,99.             RSA030
220600        15     FILLER         PICTURE  X(024).                    RSA030
220700        15     6-EW422-LNRENS PICTURE X(20).                      RSA030
220800        15     FILLER         PICTURE  X(001).                    RSA030
220900      10       6-EW423    REDEFINES   6-EW400.                    RSA030
221000        15     FILLER         PICTURE  X(037).                    RSA030
221100        15     6-EW423-NORES  PICTURE X(6).                       RSA030
221200        15     FILLER         PICTURE  X(028).                    RSA030
221300        15     6-EW423-LNRENA PICTURE X(20).                      RSA030
221400        15     FILLER         PICTURE  X(033).                    RSA030
221500        15     6-EW423-TXREI  PICTURE Z99,99.                     RSA030
221600        15     FILLER         PICTURE  X(002).                    RSA030
221700      10       6-EW424    REDEFINES   6-EW400.                    RSA030
221800        15     FILLER         PICTURE  X(034).                    RSA030
221900        15     6-EW424-CNREK  PICTURE XXX.                        RSA030
222000        15     FILLER         PICTURE  X(034).                    RSA030
222100        15     6-EW424-MKREL  PICTURE ZZZBZZZBZZ9,99.             RSA030
222200        15     FILLER         PICTURE  X(032).                    RSA030
222300        15     6-EW424-CCREM  PICTURE X.                          RSA030
222400        15     FILLER         PICTURE  X(014).                    RSA030
222500      10       6-EW425    REDEFINES   6-EW400.                    RSA030
222600        15     FILLER         PICTURE  X(038).                    RSA030
222700        15     6-EW425-CDREB  PICTURE X.                          RSA030
222800        15     FILLER         PICTURE  X(038).                    RSA030
222900        15     6-EW425-CTRESS PICTURE X.                          RSA030
223000        15     FILLER         PICTURE  X(036).                    RSA030
223100        15     6-EW425-CDREPS PICTURE XXX.                        RSA030
223200        15     FILLER         PICTURE  X(015).                    RSA030
223300      10       6-EW426    REDEFINES   6-EW400.                    RSA030
223400        15     FILLER         PICTURE  X(040).                    RSA030
223500        15     6-EW426-CNREOS PICTURE X.                          RSA030
223600        15     FILLER         PICTURE  X(091).                    RSA030
223700      10       6-EW427    REDEFINES   6-EW400.                    RSA030
223800        15     FILLER         PICTURE  X(036).                    RSA030
223900        15     6-EW427-DDREJ  PICTURE X(10).                      RSA030
224000        15     FILLER         PICTURE  X(032).                    RSA030
224100        15     6-EW427-CDREJ  PICTURE XXX.                        RSA030
224200        15     FILLER         PICTURE  X(035).                    RSA030
224300        15     6-EW427-MOREC  PICTURE ----B---B--9,99.            RSA030
224400        15     FILLER         PICTURE  X(001).                    RSA030
224500      10       6-EW428    REDEFINES   6-EW400.                    RSA030
224600        15     FILLER         PICTURE  X(036).                    RSA030
224700        15     6-EW428-DTREO  PICTURE X(10).                      RSA030
224800        15     FILLER         PICTURE  X(027).                    RSA030
224900        15     6-EW428-MTPAC  PICTURE ZZZBZZZBZZ9,99.             RSA030
225000        15     FILLER         PICTURE  X(024).                    RSA030
225100        15     6-EW428-LNRENS PICTURE X(20).                      RSA030
225200        15     FILLER         PICTURE  X(001).                    RSA030
225300      10       6-EW429    REDEFINES   6-EW400.                    RSA030
225400        15     FILLER         PICTURE  X(038).                    RSA030
225500        15     6-EW429-NORES  PICTURE X(6).                       RSA030
225600        15     FILLER         PICTURE  X(027).                    RSA030
225700        15     6-EW429-LNRENA PICTURE X(20).                      RSA030
225800        15     FILLER         PICTURE  X(033).                    RSA030
225900        15     6-EW429-TXREI  PICTURE Z99,99.                     RSA030
226000        15     FILLER         PICTURE  X(002).                    RSA030
226100      10       6-EW430    REDEFINES   6-EW400.                    RSA030
226200        15     FILLER         PICTURE  X(034).                    RSA030
226300        15     6-EW430-CNREK  PICTURE XXX.                        RSA030
226400        15     FILLER         PICTURE  X(034).                    RSA030
226500        15     6-EW430-MKREL  PICTURE ZZZBZZZBZZ9,99.             RSA030
226600        15     FILLER         PICTURE  X(029).                    RSA030
226700        15     6-EW430-MTREAO PICTURE ----B---B--9,99             RSA030
226800                                                    BLANK ZERO.   RSA030
226900        15     FILLER         PICTURE  X(003).                    RSA030
227000      10       6-EW431    REDEFINES   6-EW400.                    RSA030
227100        15     FILLER         PICTURE  X(043).                    RSA030
227200        15     6-EW431-CTREDC PICTURE X.                          RSA030
227300        15     FILLER         PICTURE  X(030).                    RSA030
227400        15     6-EW431-CCREM  PICTURE X.                          RSA030
227500        15     FILLER         PICTURE  X(038).                    RSA030
227600        15     6-EW431-CDREB  PICTURE X.                          RSA030
227700        15     FILLER         PICTURE  X(018).                    RSA030
227800      10       6-EW432    REDEFINES   6-EW400.                    RSA030
227900        15     FILLER         PICTURE  X(043).                    RSA030
228000        15     6-EW432-CTREJ  PICTURE X.                          RSA030
228100        15     FILLER         PICTURE  X(070).                    RSA030
228200        15     6-EW432-KMREDC PICTURE ZZZ9,9(5).                  RSA030
228300        15     FILLER         PICTURE  X(008).                    RSA030
228400      10       6-EW433    REDEFINES   6-EW400.                    RSA030
228500        15     FILLER         PICTURE  X(039).                    RSA030
228600        15     6-EW433-CDREAT PICTURE X.                          RSA030
228700        15     FILLER         PICTURE  X(037).                    RSA030
228800        15     6-EW433-DTRENV PICTURE X(10).                      RSA030
228900        15     FILLER         PICTURE  X(018).                    RSA030
229000        15     6-EW433-CDREX  PICTURE X.                          RSA030
229100        15     FILLER         PICTURE  X(026).                    RSA030
229200      10       6-EW434    REDEFINES   6-EW400.                    RSA030
229300        15     FILLER         PICTURE  X(043).                    RSA030
229400        15     6-EW434-NOCRM  PICTURE 99.                         RSA030
229500        15     FILLER         PICTURE  X(031).                    RSA030
229600        15     6-EW434-NOREMS PICTURE X(15).                      RSA030
229700        15     FILLER         PICTURE  X(024).                    RSA030
229800        15     6-EW434-CDRETX PICTURE XXX.                        RSA030
229900        15     FILLER         PICTURE  X(014).                    RSA030
230000      10       6-EW435    REDEFINES   6-EW400.                    RSA030
230100        15     FILLER         PICTURE  X(034).                    RSA030
230200        15     6-EW435-TXREAS PICTURE 99,99.                      RSA030
230300        15     FILLER         PICTURE  X(033).                    RSA030
230400        15     6-EW435-NORENR PICTURE X(13).                      RSA030
230500        15     FILLER         PICTURE  X(047).                    RSA030
230600      10       6-EW436    REDEFINES   6-EW400.                    RSA030
230700        15     FILLER         PICTURE  X(035).                    RSA030
230800        15     6-EW436-PEREA  PICTURE 9(4).                       RSA030
230900        15     FILLER         PICTURE  X(035).                    RSA030
231000        15     6-EW436-MTRECR PICTURE ZZZBZZZBZZ9,99.             RSA030
231100        15     FILLER         PICTURE  X(025).                    RSA030
231200        15     6-EW436-NORENR PICTURE X(13).                      RSA030
231300        15     FILLER         PICTURE  X(006).                    RSA030
231400      10       6-EW437    REDEFINES   6-EW400.                    RSA030
231500        15     FILLER         PICTURE  X(033).                    RSA030
231600        15     6-EW437-DMREN  PICTURE X(10).                      RSA030
231700        15     FILLER         PICTURE  X(024).                    RSA030
231800        15     6-EW437-NOTIE  PICTURE 9(6)                        RSA030
231900                                                    BLANK ZERO.   RSA030
232000        15     FILLER         PICTURE  X(059).                    RSA030
232100      10       6-EW438    REDEFINES   6-EW400.                    RSA030
232200        15     FILLER         PICTURE  X(034).                    RSA030
232300        15     6-EW438-NORCS  PICTURE 99.                         RSA030
232400        15     FILLER         PICTURE  X(030).                    RSA030
232500        15     6-EW438-DMREN  PICTURE X(10).                      RSA030
232600        15     FILLER         PICTURE  X(040).                    RSA030
232700        15     6-EW438-CERCS  PICTURE X.                          RSA030
232800        15     FILLER         PICTURE  X(015).                    RSA030
232900      10       6-EW439    REDEFINES   6-EW400.                    RSA030
233000        15     FILLER         PICTURE  X(036).                    RSA030
233100        15     6-EW439-LIRCS  PICTURE X(40).                      RSA030
233200        15     FILLER         PICTURE  X(044).                    RSA030
233300        15     6-EW439-DDRCS  PICTURE X(10).                      RSA030
233400        15     FILLER         PICTURE  X(002).                    RSA030
233500      10       6-EW440    REDEFINES   6-EW400.                    RSA030
233600        15     FILLER         PICTURE  X(036).                    RSA030
233700        15     6-EW440-CTRCK  PICTURE X.                          RSA030
233800        15     FILLER         PICTURE  X(029).                    RSA030
233900        15     6-EW440-MKRCL  PICTURE ZZZBZZZBZZ9,99.             RSA030
234000        15     FILLER         PICTURE  X(034).                    RSA030
234100        15     6-EW440-DHRCS  PICTURE X(10).                      RSA030
234200        15     FILLER         PICTURE  X(008).                    RSA030
234300      10       6-EW441    REDEFINES   6-EW400.                    RSA030
234400        15     FILLER         PICTURE  X(036).                    RSA030
234500        15     6-EW441-CTRCB  PICTURE X.                          RSA030
234600        15     FILLER         PICTURE  X(029).                    RSA030
234700        15     6-EW441-MTRCA  PICTURE ZZZBZZZBZZ9,99.             RSA030
234800        15     FILLER         PICTURE  X(030).                    RSA030
234900        15     6-EW441-MTRCG  PICTURE ----B---B--9,99.            RSA030
235000        15     FILLER         PICTURE  X(007).                    RSA030
235100      10       6-EW442    REDEFINES   6-EW400.                    RSA030
235200        15     FILLER         PICTURE  X(037).                    RSA030
235300        15     6-EW442-MCRCCR PICTURE ---B---B--9,99.             RSA030
235400        15     FILLER         PICTURE  X(022).                    RSA030
235500        15     6-EW442-DTRCB  PICTURE X(10).                      RSA030
235600        15     FILLER         PICTURE  X(027).                    RSA030
235700        15     6-EW442-MTRCB  PICTURE ----B---B--9,99.            RSA030
235800        15     FILLER         PICTURE  X(007).                    RSA030
235900      10       6-EW443    REDEFINES   6-EW400.                    RSA030
236000        15     FILLER         PICTURE  X(037).                    RSA030
236100        15     6-EW443-LNRCA  PICTURE X(32).                      RSA030
236200        15     FILLER         PICTURE  X(042).                    RSA030
236300        15     6-EW443-MORCS  PICTURE ZZZBZZZBZZ9,99.             RSA030
236400        15     FILLER         PICTURE  X(007).                    RSA030
236500      10       6-EW444    REDEFINES   6-EW400.                    RSA030
236600        15     FILLER         PICTURE  X(037).                    RSA030
236700        15     6-EW444-LRRE1  PICTURE X(36).                      RSA030
236800        15     FILLER         PICTURE  X(002).                    RSA030
236900        15     6-EW444-LRRE2  PICTURE X(32).                      RSA030
237000        15     FILLER         PICTURE  X(025).                    RSA030
237100      10       6-EW445    REDEFINES   6-EW400.                    RSA030
237200        15     FILLER         PICTURE  X(037).                    RSA030
237300        15     6-EW445-LVRE   PICTURE X(31).                      RSA030
237400        15     FILLER         PICTURE  X(044).                    RSA030
237500        15     6-EW445-CPREN  PICTURE 9(5)                        RSA030
237600                                                    BLANK ZERO.   RSA030
237700        15     FILLER         PICTURE  X(015).                    RSA030
237800      10       6-EW446    REDEFINES   6-EW400.                    RSA030
237900        15     FILLER         PICTURE  X(037).                    RSA030
238000        15     6-EW446-LIRCSB PICTURE X(40).                      RSA030
238100        15     FILLER         PICTURE  X(035).                    RSA030
238200        15     6-EW446-CDREP  PICTURE XXX.                        RSA030
238300        15     FILLER         PICTURE  X(017).                    RSA030
238400      10       6-EW447    REDEFINES   6-EW400.                    RSA030
238500        15     FILLER         PICTURE  X(042).                    RSA030
238600        15     6-EW447-DTRCB  PICTURE X(10).                      RSA030
238700        15     FILLER         PICTURE  X(021).                    RSA030
238800        15     6-EW447-MTRCBH PICTURE ----B---B--9,99.            RSA030
238900        15     FILLER         PICTURE  X(021).                    RSA030
239000        15     6-EW447-DMREN  PICTURE X(10).                      RSA030
239100        15     FILLER         PICTURE  X(013).                    RSA030
239200      10       6-EW448    REDEFINES   6-EW400.                    RSA030
239300        15     FILLER         PICTURE  X(034).                    RSA030
239400        15     6-EW448-NOPAI  PICTURE 999.                        RSA030
239500        15     FILLER         PICTURE  X(036).                    RSA030
239600        15     6-EW448-DHPAI  PICTURE X(10).                      RSA030
239700        15     FILLER         PICTURE  X(028).                    RSA030
239800        15     6-EW448-DTPAI  PICTURE X(10).                      RSA030
239900        15     FILLER         PICTURE  X(011).                    RSA030
240000      10       6-EW449    REDEFINES   6-EW400.                    RSA030
240100        15     FILLER         PICTURE  X(038).                    RSA030
240200        15     6-EW449-MTPAI  PICTURE ----B---B--9,99.            RSA030
240300        15     FILLER         PICTURE  X(031).                    RSA030
240400        15     6-EW449-MORER  PICTURE ----B---B--9,99             RSA030
240500                                                    BLANK ZERO.   RSA030
240600        15     FILLER         PICTURE  X(033).                    RSA030
240700      10       6-EW450    REDEFINES   6-EW400.                    RSA030
240800        15     FILLER         PICTURE  X(034).                    RSA030
240900        15     6-EW450-CDPAD  PICTURE X.                          RSA030
241000        15     FILLER         PICTURE  X(031).                    RSA030
241100        15     6-EW450-NOTIE  PICTURE 9(6)                        RSA030
241200                                                    BLANK ZERO.   RSA030
241300        15     FILLER         PICTURE  X(045).                    RSA030
241400        15     6-EW450-NOPAM  PICTURE 99.                         RSA030
241500        15     FILLER         PICTURE  X(013).                    RSA030
241600      10       6-EW451    REDEFINES   6-EW400.                    RSA030
241700        15     FILLER         PICTURE  X(038).                    RSA030
241800        15     6-EW451-CDPAM  PICTURE X.                          RSA030
241900        15     FILLER         PICTURE  X(032).                    RSA030
242000        15     6-EW451-DTPAIR PICTURE X(10).                      RSA030
242100        15     FILLER         PICTURE  X(037).                    RSA030
242200        15     6-EW451-CDPAS  PICTURE X.                          RSA030
242300        15     FILLER         PICTURE  X(013).                    RSA030
242400      10       6-EW452    REDEFINES   6-EW400.                    RSA030
242500        15     FILLER         PICTURE  X(038).                    RSA030
242600        15     6-EW452-CDPAP  PICTURE X.                          RSA030
242700        15     FILLER         PICTURE  X(030).                    RSA030
242800        15     6-EW452-NOPAF  PICTURE X(6).                       RSA030
242900        15     FILLER         PICTURE  X(043).                    RSA030
243000        15     6-EW452-CDPAI  PICTURE X.                          RSA030
243100        15     FILLER         PICTURE  X(013).                    RSA030
243200      10       6-EW453    REDEFINES   6-EW400.                    RSA030
243300        15     FILLER         PICTURE  X(033).                    RSA030
243400        15     6-EW453-DMREN  PICTURE X(10).                      RSA030
243500        15     FILLER         PICTURE  X(030).                    RSA030
243600        15     6-EW453-DHPAI  PICTURE X(10).                      RSA030
243700        15     FILLER         PICTURE  X(031).                    RSA030
243800        15     6-EW453-MTPAIO PICTURE ----B---B--9,99.            RSA030
243900        15     FILLER         PICTURE  X(003).                    RSA030
244000      10       6-EW454    REDEFINES   6-EW400.                    RSA030
244100        15     FILLER         PICTURE  X(039).                    RSA030
244200        15     6-EW454-LIPAO  PICTURE X(31).                      RSA030
244300        15     FILLER         PICTURE  X(039).                    RSA030
244400        15     6-EW454-NOTIE  PICTURE 9(6)                        RSA030
244500                                                    BLANK ZERO.   RSA030
244600        15     FILLER         PICTURE  X(017).                    RSA030
244700      10       6-EW455    REDEFINES   6-EW400.                    RSA030
244800        15     FILLER         PICTURE  X(035).                    RSA030
244900        15     6-EW455-CTPAO  PICTURE X.                          RSA030
245000        15     FILLER         PICTURE  X(037).                    RSA030
245100        15     6-EW455-NOPAI  PICTURE 999.                        RSA030
245200        15     FILLER         PICTURE  X(033).                    RSA030
245300        15     6-EW455-NOREN  PICTURE X(13).                      RSA030
245400        15     FILLER         PICTURE  X(010).                    RSA030
245500      10       6-EW456    REDEFINES   6-EW400.                    RSA030
245600        15     FILLER         PICTURE  X(044).                    RSA030
245700        15     6-EW456-LIPA2  PICTURE X(31).                      RSA030
245800        15     FILLER         PICTURE  X(057).                    RSA030
245900      10       6-EW457    REDEFINES   6-EW400.                    RSA030
246000        15     FILLER         PICTURE  X(038).                    RSA030
246100        15     6-EW457-MTREB  PICTURE ZZZBZZZBZZ9,99.             RSA030
246200        15     FILLER         PICTURE  X(020).                    RSA030
246300        15     6-EW457-PXREF  PICTURE 9(4).                       RSA030
246400        15     FILLER         PICTURE  X(056).                    RSA030
246500      10       6-EW458    REDEFINES   6-EW400.                    RSA030
246600        15     FILLER         PICTURE  X(043).                    RSA030
246700        15     6-EW458-CTREA  PICTURE X.                          RSA030
246800        15     FILLER         PICTURE  X(037).                    RSA030
246900        15     6-EW458-TXREI  PICTURE Z99,99.                     RSA030
247000        15     FILLER         PICTURE  X(028).                    RSA030
247100        15     6-EW458-DTREO  PICTURE X(10).                      RSA030
247200        15     FILLER         PICTURE  X(007).                    RSA030
247300      10       6-EW459    REDEFINES   6-EW400.                    RSA030
247400        15     FILLER         PICTURE  X(040).                    RSA030
247500        15     6-EW459-MTREAO PICTURE ----B---B--9,99             RSA030
247600                                                    BLANK ZERO.   RSA030
247700        15     FILLER         PICTURE  X(021).                    RSA030
247800        15     6-EW459-MTREAT PICTURE ZZZBZZZBZZ9,99              RSA030
247900                                                    BLANK ZERO.   RSA030
248000        15     FILLER         PICTURE  X(042).                    RSA030
248100      10       6-EW460    REDEFINES   6-EW400.                    RSA030
248200        15     FILLER         PICTURE  X(040).                    RSA030
248300        15     6-EW460-MOREC  PICTURE ----B---B--9,99.            RSA030
248400        15     FILLER         PICTURE  X(077).                    RSA030
248500 01   ZONES-UTILISATEUR PICTURE X.                                RSA030
248600          EXEC SQL BEGIN DECLARE SECTION END-EXEC.                7I2010
248700 01               C-0204.                                         7I2110
248800   05             C-0204-NORER                                    7I2120
248900                  PICTURE X(6).                                   7I2120
249000   05             C-0204-CNREN                                    7I2130
249100                  PICTURE XX.                                     7I2130
249200   05             C-0204-IDRPL                                    7I2140
249300                  PICTURE X(5).                                   7I2140
249400 01               C-0405.                                         7I2150
249500   05             C-0405-CNREN                                    7I2160
249600                  PICTURE XX.                                     7I2160
249700   05             C-0405-IDRPL                                    7I2170
249800                  PICTURE X(5).                                   7I2170
249900   05             C-0405-NORER                                    7I2180
250000                  PICTURE X(6).                                   7I2180
250100   05             C-0405-NOPAM                                    7I2190
250200                  PICTURE S99                                     7I2190
250300                    COMPUTATIONAL-3.                              7I2190
250400 01               C-0406.                                         7I2200
250500   05             C-0406-CNREN                                    7I2210
250600                  PICTURE XX.                                     7I2210
250700   05             C-0406-IDRPL                                    7I2220
250800                  PICTURE X(5).                                   7I2220
250900   05             C-0406-NORER                                    7I2230
251000                  PICTURE X(6).                                   7I2230
251100   05             C-0406-DDREN                                    7I2235
251200                  PICTURE X(8).                                   7I2235
251300   05             C-0406-XCSEQ                                    7I2240
251400                  PICTURE S9(9)                                   7I2240
251500                    COMPUTATIONAL-3.                              7I2240
251600 01               C-0407.                                         7I2250
251700   05             C-0407-CNREN                                    7I2260
251800                  PICTURE XX.                                     7I2260
251900   05             C-0407-IDRPL                                    7I2270
252000                  PICTURE X(5).                                   7I2270
252100   05             C-0407-NORER                                    7I2280
252200                  PICTURE X(6).                                   7I2280
252300   05             C-0407-XCSEQ                                    7I2290
252400                  PICTURE S9(9)                                   7I2290
252500                    COMPUTATIONAL-3.                              7I2290
252600 01               C-0416.                                         7I2300
252700   05             C-0416-CNREN                                    7I2310
252800                  PICTURE XX.                                     7I2310
252900   05             C-0416-IDRPL                                    7I2320
253000                  PICTURE X(5).                                   7I2320
253100   05             C-0416-NORER                                    7I2330
253200                  PICTURE X(6).                                   7I2330
253300   05             C-0416-XCSEQ                                    7I2340
253400                  PICTURE S9(9)                                   7I2340
253500                    COMPUTATIONAL-3.                              7I2340
253600 01               C-0417.                                         7I2350
253700   05             C-0417-CNREN                                    7I2360
253800                  PICTURE XX.                                     7I2360
253900   05             C-0417-IDRPL                                    7I2370
254000                  PICTURE X(5).                                   7I2370
254100   05             C-0417-NORER                                    7I2380
254200                  PICTURE X(6).                                   7I2380
254300   05             C-0417-NORCS                                    7I2390
254400                  PICTURE S99                                     7I2390
254500                    COMPUTATIONAL-3.                              7I2390
254600 01               C-0419.                                         7I2400
254700   05             C-0419-CNREN                                    7I2410
254800                  PICTURE XX.                                     7I2410
254900   05             C-0419-IDRPL                                    7I2420
255000                  PICTURE X(5).                                   7I2420
255100   05             C-0419-NORER                                    7I2430
255200                  PICTURE X(6).                                   7I2430
255300   05             C-0419-DTPAI                                    7I2440
255400                  PICTURE X(8).                                   7I2440
255500   05             C-0419-DTPAIR                                   7I2450
255600                  PICTURE X(8).                                   7I2450
255700   05             C-0419-NOPAI                                    7I2460
255800                  PICTURE S999                                    7I2460
255900                    COMPUTATIONAL-3.                              7I2460
256000 01               C-0422.                                         7I2470
256100   05             C-0422-CNREN                                    7I2480
256200                  PICTURE XX.                                     7I2480
256300   05             C-0422-IDRPL                                    7I2490
256400                  PICTURE X(5).                                   7I2490
256500   05             C-0422-NORER                                    7I2500
256600                  PICTURE X(6).                                   7I2500
256700   05             C-0422-XCSEQ                                    7I2510
256800                  PICTURE S9(9)                                   7I2510
256900                    COMPUTATIONAL-3.                              7I2510
257000 01               C-0423.                                         7I2520
257100   05             C-0423-CNREN                                    7I2530
257200                  PICTURE XX.                                     7I2530
257300   05             C-0423-IDRPL                                    7I2540
257400                  PICTURE X(5).                                   7I2540
257500   05             C-0423-NORER                                    7I2550
257600                  PICTURE X(6).                                   7I2550
257700   05             C-0423-XCSEQ                                    7I2560
257800                  PICTURE S9(9)                                   7I2560
257900                    COMPUTATIONAL-3.                              7I2560
258000 01               C-04XX.                                         7I2570
258100   05             C-04XX-CNREN                                    7I2580
258200                  PICTURE XX.                                     7I2580
258300   05             C-04XX-IDRPL                                    7I2590
258400                  PICTURE X(5).                                   7I2590
258500   05             C-04XX-NORER                                    7I2600
258600                  PICTURE X(6).                                   7I2600
258700 01               C-1718.                                         7I2610
258800   05             C-1718-CNREN                                    7I2620
258900                  PICTURE XX.                                     7I2620
259000   05             C-1718-IDRPL                                    7I2630
259100                  PICTURE X(5).                                   7I2630
259200   05             C-1718-NORCS                                    7I2640
259300                  PICTURE S99                                     7I2640
259400                    COMPUTATIONAL-3.                              7I2640
259500   05             C-1718-NORER                                    7I2650
259600                  PICTURE X(6).                                   7I2650
259700   05             C-1718-XCSEQ                                    7I2660
259800                  PICTURE S9(9)                                   7I2660
259900                    COMPUTATIONAL-3.                              7I2660
260000          EXEC SQL END   DECLARE SECTION END-EXEC.                7I2990
260100 01               D-0204.                                         7I3110
260200   05             D-0204-NORER   VALUE '!'                        7I3120
260300                  PICTURE X(6).                                   7I3120
260400   05             D-0204-CNREN   VALUE '!'                        7I3130
260500                  PICTURE XX.                                     7I3130
260600   05             D-0204-IDRPL   VALUE '!'                        7I3140
260700                  PICTURE X(5).                                   7I3140
260800 01               D-0405.                                         7I3150
260900   05             D-0405-CNREN   VALUE '!'                        7I3160
261000                  PICTURE XX.                                     7I3160
261100   05             D-0405-IDRPL   VALUE '!'                        7I3170
261200                  PICTURE X(5).                                   7I3170
261300   05             D-0405-NORER   VALUE '!'                        7I3180
261400                  PICTURE X(6).                                   7I3180
261500   05             D-0405-NOPAM   VALUE ZERO                       7I3190
261600                  PICTURE S99                                     7I3190
261700                    COMPUTATIONAL-3.                              7I3190
261800 01               D-0406.                                         7I3200
261900   05             D-0406-CNREN   VALUE '!'                        7I3210
262000                  PICTURE XX.                                     7I3210
262100   05             D-0406-IDRPL   VALUE '!'                        7I3220
262200                  PICTURE X(5).                                   7I3220
262300   05             D-0406-NORER   VALUE '!'                        7I3230
262400                  PICTURE X(6).                                   7I3230
262500   05             D-0406-DDREN   VALUE '!'                        7I3235
262600                  PICTURE X(8).                                   7I3235
262700   05             D-0406-XCSEQ   VALUE ZERO                       7I3240
262800                  PICTURE S9(9)                                   7I3240
262900                    COMPUTATIONAL-3.                              7I3240
263000 01               D-0407.                                         7I3250
263100   05             D-0407-CNREN   VALUE '!'                        7I3260
263200                  PICTURE XX.                                     7I3260
263300   05             D-0407-IDRPL   VALUE '!'                        7I3270
263400                  PICTURE X(5).                                   7I3270
263500   05             D-0407-NORER   VALUE '!'                        7I3280
263600                  PICTURE X(6).                                   7I3280
263700   05             D-0407-XCSEQ   VALUE ZERO                       7I3290
263800                  PICTURE S9(9)                                   7I3290
263900                    COMPUTATIONAL-3.                              7I3290
264000 01               D-0416.                                         7I3300
264100   05             D-0416-CNREN   VALUE '!'                        7I3310
264200                  PICTURE XX.                                     7I3310
264300   05             D-0416-IDRPL   VALUE '!'                        7I3320
264400                  PICTURE X(5).                                   7I3320
264500   05             D-0416-NORER   VALUE '!'                        7I3330
264600                  PICTURE X(6).                                   7I3330
264700   05             D-0416-XCSEQ   VALUE ZERO                       7I3340
264800                  PICTURE S9(9)                                   7I3340
264900                    COMPUTATIONAL-3.                              7I3340
265000 01               D-0417.                                         7I3350
265100   05             D-0417-CNREN   VALUE '!'                        7I3360
265200                  PICTURE XX.                                     7I3360
265300   05             D-0417-IDRPL   VALUE '!'                        7I3370
265400                  PICTURE X(5).                                   7I3370
265500   05             D-0417-NORER   VALUE '!'                        7I3380
265600                  PICTURE X(6).                                   7I3380
265700   05             D-0417-NORCS   VALUE ZERO                       7I3390
265800                  PICTURE S99                                     7I3390
265900                    COMPUTATIONAL-3.                              7I3390
266000 01               D-0419.                                         7I3400
266100   05             D-0419-CNREN   VALUE '!'                        7I3410
266200                  PICTURE XX.                                     7I3410
266300   05             D-0419-IDRPL   VALUE '!'                        7I3420
266400                  PICTURE X(5).                                   7I3420
266500   05             D-0419-NORER   VALUE '!'                        7I3430
266600                  PICTURE X(6).                                   7I3430
266700   05             D-0419-DTPAI   VALUE '!'                        7I3440
266800                  PICTURE X(8).                                   7I3440
266900   05             D-0419-DTPAIR  VALUE '!'                        7I3450
267000                  PICTURE X(8).                                   7I3450
267100   05             D-0419-NOPAI   VALUE ZERO                       7I3460
267200                  PICTURE S999                                    7I3460
267300                    COMPUTATIONAL-3.                              7I3460
267400 01               D-0422.                                         7I3470
267500   05             D-0422-CNREN   VALUE '!'                        7I3480
267600                  PICTURE XX.                                     7I3480
267700   05             D-0422-IDRPL   VALUE '!'                        7I3490
267800                  PICTURE X(5).                                   7I3490
267900   05             D-0422-NORER   VALUE '!'                        7I3500
268000                  PICTURE X(6).                                   7I3500
268100   05             D-0422-XCSEQ   VALUE ZERO                       7I3510
268200                  PICTURE S9(9)                                   7I3510
268300                    COMPUTATIONAL-3.                              7I3510
268400 01               D-0423.                                         7I3520
268500   05             D-0423-CNREN   VALUE '!'                        7I3530
268600                  PICTURE XX.                                     7I3530
268700   05             D-0423-IDRPL   VALUE '!'                        7I3540
268800                  PICTURE X(5).                                   7I3540
268900   05             D-0423-NORER   VALUE '!'                        7I3550
269000                  PICTURE X(6).                                   7I3550
269100   05             D-0423-XCSEQ   VALUE ZERO                       7I3560
269200                  PICTURE S9(9)                                   7I3560
269300                    COMPUTATIONAL-3.                              7I3560
269400 01               D-04XX.                                         7I3570
269500   05             D-04XX-CNREN   VALUE '!'                        7I3580
269600                  PICTURE XX.                                     7I3580
269700   05             D-04XX-IDRPL   VALUE '!'                        7I3590
269800                  PICTURE X(5).                                   7I3590
269900   05             D-04XX-NORER   VALUE '!'                        7I3600
270000                  PICTURE X(6).                                   7I3600
270100 01               D-1718.                                         7I3610
270200   05             D-1718-CNREN   VALUE '!'                        7I3620
270300                  PICTURE XX.                                     7I3620
270400   05             D-1718-IDRPL   VALUE '!'                        7I3630
270500                  PICTURE X(5).                                   7I3630
270600   05             D-1718-NORCS   VALUE ZERO                       7I3640
270700                  PICTURE S99                                     7I3640
270800                    COMPUTATIONAL-3.                              7I3640
270900   05             D-1718-NORER   VALUE '!'                        7I3650
271000                  PICTURE X(6).                                   7I3650
271100   05             D-1718-XCSEQ   VALUE ZERO                       7I3660
271200                  PICTURE S9(9)                                   7I3660
271300                    COMPUTATIONAL-3.                              7I3660
271400 PROCEDURE DIVISION.                                              RSA030
271500 DECLARATIVES.                                                    RSA030
271600 SECEW SECTION.                                                   RSA030
271700     USE AFTER ERROR PROCEDURE ON   EW-FICHIER.                   RSA030
271800 F0AEW. DISPLAY  'STATUS : EW     = '  1-EW00-STATUS.             RSA030
271900 F0AEW-A. GO TO  F0A90.                                           RSA030
272000 SECYX SECTION.                                                   P000
272100     USE AFTER ERROR PROCEDURE                                    P100
272200      ON YX-FICHIER.                                              P110
272300 F0AYX.                                                           P120
272400     MOVE        'YX' TO XA80-XCOSD                               P200
272500     MOVE        1-YX00-STATUS TO XA80-STATUS                     P210
272600     MOVE        5-YX00-CPTENR TO XA80-XQNENR                     P220
272700     GO TO F0A90.                                                 P500
272800 F0AEW-FN. EXIT.                                                  P500
272900 F0A90.                                                           P100
273000     MOVE        '0A90' TO XA60-XCDFSF                            P110
273100     MOVE        'ERREUR I-O' TO XA60-XLISUI.                     P120
273200           IF    XA80-STATUS1 = '9'                               P130
273300     MOVE        XA80-STATUS1 TO XA81-STATUS1                     P132
273400     MOVE        XA80-STATUS2 TO XA81-STATUS2                     P133
273500     MOVE        XA81-STATUS TO XA80-STATUS.                      P135
273600     MOVE        XA80 TO XA60-ZX67A                               P140
273700     MOVE        SPACE TO XA60-ZX67B.                             P150
273800     PERFORM     F9900 THRU F9900-FN.                             P210
273900 F0A90-FN. EXIT.                                                  P210
274000 END DECLARATIVES.                                                RSA030
274100 SEC00 SECTION.                                                   RSA030
274200 F0B.           EXIT.                                             P000
274300 F0BBA.                                                           P000
274400     MOVE        'GCA_DATCE' TO XA30-ENVNAM                       P050
274500     PERFORM     F99VE THRU F99VE-FN.                             P060
274600           IF    IK = ZERO                                        P070
274700     MOVE        XA30-ENVVAL TO DATCE                             P070
274800           ELSE                                                   P100
274900     MOVE FUNCTION CURRENT-DATE (1:8)                             P100
275000       TO DATCE.                                                  P110
275100     ACCEPT TIMCO FROM TIME.                                      P120
275200     MOVE        DATCE TO XAIN-XDATRT                             P210
275300     MOVE        TIMCO TO XAIN-XHETRT.                            P220
275400     MOVE        DATCE                                            P310
275500     TO DAT8E DAT6C                                               P310
275600     MOVE DAT83E TO DAT61C  MOVE DAT81E TO DAT63C                 P310
275700     MOVE DAT82E TO DAT62C                                        P310
275800     MOVE   DAT6C TO  XAED-XDATRT                                 P310
275900     MOVE        XAED-XDATRT                                      P315
276000     TO DAT8E DAT6C                                               P315
276100     MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 P315
276200     MOVE DAT63C TO DAT83C                                        P315
276300     MOVE   DAT8C TO XAED-XDATRT                                  P315
276400     MOVE        TIMCO                                            P320
276500     TO DAT8E DAT6C                                               P320
276600     MOVE DAT61C TO TIMHOU  MOVE DAT62C TO TIMMIN                 P320
276700     MOVE DAT82E TO TIMSEC                                        P320
276800     MOVE TIMDAY TO   XAED-XHETRT.                                P320
276900 F0BBA-FN. EXIT.                                                  P320
277000 F0BBE.                                                           P000
277100     PERFORM     F98-D THRU F98-D-FN.                             P100
277200 F0BBE-FN. EXIT.                                                  P100
277300 F0BCA.         EXIT.                                             P000
277400 F0BCI.                                                           P000
277500     MOVE        'GCA_OUSR' TO XA30-ENVNAM                        P100
277600     PERFORM     F99VE THRU F99VE-FN.                             P110
277700           IF    IK = ZERO                                        P200
277800     MOVE        XA30-ENVVAL TO USERID                            P200
277900           ELSE                                                   P210
278000     MOVE        '0BCE' TO XA60-XCDFSF                            P210
278100     MOVE        'ERREUR SUR VARIABLE GCA_OUSR  ' TO              P230
278200     XA60-XLISUI                                                  P235
278300     MOVE                         'VARIABLE GCA_OUSR   ABSENTE A TP240
278400-                'ORT' TO XA60-ZX67A                              P245
278500     MOVE        SPACE TO XA60-ZX67B                              P250
278600     PERFORM     F9900 THRU F9900-FN.                             P260
278700 F0BCI-FN. EXIT.                                                  P260
278800 F0BCO.                                                           P000
278900     MOVE        'GCA_OTRC' TO XA30-ENVNAM                        P100
279000     PERFORM     F99VE THRU F99VE-FN.                             P110
279100           IF    IK = ZERO                                        P200
279200           AND   XA30-ENVVAL = 'ON'                               P210
279300     MOVE        '1' TO XO00-XORATR.                              P200
279400 F0BCO-FN. EXIT.                                                  P200
279500 F0BCU.                                                           P000
279600     MOVE        'GCA_8TMES' TO XA30-ENVNAM                       P100
279700     PERFORM     F99VE THRU F99VE-FN.                             P110
279800           IF    IK = ZERO                                        P200
279900           AND   XA30-ENVVAL = '1'                                P210
280000     MOVE        XA30-ENVVAL TO XA00-8TMES                        P200
280100           ELSE                                                   P220
280200     MOVE        ZERO TO XA00-8TMES.                              P220
280300 F0BCU-FN. EXIT.                                                  P220
280400 F0BCY.                                                           P000
280500     MOVE        'GCA_ORBS' TO XA30-ENVNAM                        P100
280600     PERFORM     F99VE THRU F99VE-FN.                             P110
280700           IF    IK = ZERO                                        P200
280800     MOVE        XA30-ENVVAL TO XO00-XORARB.                      P200
280900 F0BCY-FN. EXIT.                                                  P200
281000 F0BDA.                                                           P000
281100     PERFORM     F99SX THRU F99SX-FN.                             P100
281200 F0BDA-FN. EXIT.                                                  P100
281300 F0BEA.         EXIT.                                             P000
281400 F0BEA-FN. EXIT.                                                  P000
281500 F0BCA-FN. EXIT.                                                  P000
281600 F0B-FN.   EXIT.                                                  P000
281700 F0C.           EXIT.                                             P000
281800 F0CCI.                                                           P000
281900     PERFORM     F95-WORK-CN THRU F95-WORK-CN-FN                  P100
282000     MOVE        '1' TO XO00-XORACN.                              P110
282100 F0CCI-FN. EXIT.                                                  P110
282200 F0CCO.    IF    XO00-XORATR = '1'                                P000
282300           NEXT SENTENCE ELSE GO TO     F0CCO-FN.                 P000
282400     PERFORM     F95-WORK-TR THRU F95-WORK-TR-FN                  P100
282500     DISPLAY     'TRACE ORACLE IS ON'.                            P110
282600 F0CCO-FN. EXIT.                                                  P110
282700 F0CCY.    IF    XO00-XORARB NOT = SPACE                          P000
282800           NEXT SENTENCE ELSE GO TO     F0CCY-FN.                 P000
282900     PERFORM     F95-WORK-RBS THRU F95-WORK-RBS-FN                P100
283000     DISPLAY     'ROLLBACK SEGMENT : '                            P110
283100     XO00-XORARB.                                                 P120
283200 F0CCY-FN. EXIT.                                                  P120
283300 F0C-FN.   EXIT.                                                  P120
283400 F0EEW.                                                           P000
283500     INSPECT     4-LIB REPLACING ALL '"'                          P100
283600                       BY ''''.                                   P120
283700 F0EEW-FN. EXIT.                                                  P120
283800 F01.      EXIT.                                                  RSA030
283900 F01AA.                                                           P010
284000     PERFORM     F95-RS01-NO THRU F95-RS01-NO-FN.                 P100
284100           IF    IK = ZERO                                        P110
284200     MOVE        RS01-NONER TO W-WA00-NONER                       P110
284300     DISPLAY     'No de NER lu : ' W-WA00-NONER                   P120
284400           ELSE                                                   P130
284500     PERFORM     F99OR THRU F99OR-FN.                             P130
284600 F01AA-FN. EXIT.                                                  P130
284700 F01EW.    OPEN OUTPUT                   EW-FICHIER.              RSA030
284800        IF        1-EW00-STATUS  NOT  = ZERO                      RSA030
284900             AND  1-EW00-STATUS  NOT  = '97'                      RSA030
285000        PERFORM    F0AEW                                          RSA030
285100        PERFORM    F0A90         THRU F0A90-FN.                   RSA030
285200 F01EW-FN. EXIT.                                                  RSA030
285300 F01YX.    OPEN OUTPUT                   YX-FICHIER.              RSA030
285400        IF        1-YX00-STATUS  NOT  = ZERO                      RSA030
285500             AND  1-YX00-STATUS  NOT  = '97'                      RSA030
285600        PERFORM    F0AYX                                          RSA030
285700        PERFORM    F0A90         THRU F0A90-FN.                   RSA030
285800 F01YX-FN. EXIT.                                                  RSA030
285900 F01-FN.   EXIT.                                                  RSA030
286000 F03CA.                                                           P000
286100     MOVE        SPACE TO RS42-ZIN42                              P100
286200     MOVE        'C' TO RS42-CDRES                                P120
286300     MOVE        '1' TO RS42-CDRETS                               P160
286400     MOVE        'RG' TO RS42-NORETS                              P180
286500     MOVE        '000002' TO RS42-NIRET                           P190
286600     PERFORM     F95-RS42-FU THRU F95-RS42-FU-FN.                 P200
286700           IF    IK = '0'                                         P220
286800     MOVE        RS42-ZTA100 TO W-WA00-ABRS27                     P220
286900           ELSE                                                   P240
287000     DISPLAY     'PAS DE TABLE C1RG 000002 '                      P240
287100     'ABORT PROGRAMME'                                            P250
287200     PERFORM     F9900 THRU F9900-FN.                             P260
287300 F03CA-FN. EXIT.                                                  P260
287400 F03CB.                                                           P000
287500           IF    W-WA00-CDMON = 'FRF'                             P100
287600     MOVE        'FRANCS' TO W-WW00-LIDEV.                        P100
287700           IF    W-WA00-CDMON = 'EUR'                             P120
287800     MOVE        'EUROS ' TO W-WW00-LIDEV.                        P120
287900 F03CB-FN. EXIT.                                                  P120
288000*          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            RSA030
288100 F05.           EXIT.                                             RSA030
288200 F20.      IF FT =            ALL '1'                             RSA030
288300           NEXT SENTENCE ELSE GO TO     F20-FN.                   RSA030
288400 F20AD.                                                           P000
288500     PERFORM     F0BBA THRU F0BBA-FN.                             P100
288600 F20AD-FN. EXIT.                                                  P100
288700 F20EW.    CLOSE    EW-FICHIER.                                   RSA030
288800 F20EW-FN. EXIT.                                                  RSA030
288900 F20YX.    CLOSE    YX-FICHIER.                                   RSA030
289000 F20YX-FN. EXIT.                                                  RSA030
289100 F2080.    IF    XO00-XORACN = '1'                                P000
289200           NEXT SENTENCE ELSE GO TO     F2080-FN.                 P000
289300     PERFORM     F95-WORK-CO THRU F95-WORK-CO-FN.                 P100
289400 F2080-FN. EXIT.                                                  P100
289500 F2085.                                                           PFF0
289600     PERFORM     F95-RS06-FV-CL THRU F95-RS06-FV-CL-FN            PFF0
289700           IF    XO00-XORACN = '1'                                P000
289800     PERFORM     F95-WORK-RR THRU F95-WORK-RR-FN                  P900
289900     MOVE        ZERO TO XO00-XORACN.                             P910
290000 F2085-FN. EXIT.                                                  P910
290100 F2090.                                                           P000
290200     PERFORM     F98-Z THRU F98-Z-FN.                             P100
290300 F2090-FN. EXIT.                                                  P100
290400 F2095.                                                           P000
290500     MOVE        XA00-XRC TO RETURN-CODE.                         P100
290600 F2095-FN. EXIT.                                                  P100
290700 F2099.     GOBACK.                                               RSA030
290800 F2099-FN. EXIT.                                                  RSA030
290900 F20-FN.   EXIT.                                                  RSA030
291000 F50.           EXIT.                                             P000
291100 F50BB.                                                           P000
291200     MOVE        '0' TO IK                                        P100
291300     ACCEPT      W-WW00-NORER                                     P120
291400     MOVE        W-WW00-NORER TO RS02-NORER                       P140
291500     PERFORM     F95-RS02-FA THRU F95-RS02-FA-FN.                 P160
291600 F50DD.    IF    IK = '0'                                         P000
291700           NEXT SENTENCE ELSE GO TO     F50DD-FN.                 P000
291800     PERFORM     F95-RS04-FV THRU F95-RS04-FV-FN.                 P100
291900           IF    IK = '0'                                         P140
292000     MOVE        'RS04' TO DB-RECORD-NAME                         P140
292100     PERFORM     F84 THRU F84-FN                                  P145
292200           ELSE                                                   P160
292300     MOVE        '0' TO IK                                        P160
292400         GO TO     F50DD-FN.                                      P180
292500 F50EE.    IF    IK = '0'                                         P000
292600           NEXT SENTENCE ELSE GO TO     F50EE-FN.                 P000
292700     PERFORM     F95-RS05-FV THRU F95-RS05-FV-FN.                 P100
292800           IF    IK = '0'                                         P140
292900     MOVE        'RS05' TO DB-RECORD-NAME                         P140
293000     PERFORM     F84 THRU F84-FN                                  P145
293100           ELSE                                                   P160
293200     MOVE        '0' TO IK                                        P160
293300         GO TO     F50EE-FN.                                      P180
293400 F50EE-900. GO TO F50EE.                                          P180
293500 F50EE-FN. EXIT.                                                  P180
293600 F50FF.    IF    IK = '0'                                         P000
293700           NEXT SENTENCE ELSE GO TO     F50FF-FN.                 P000
293800     PERFORM     F95-RS06-FV THRU F95-RS06-FV-FN.                 P100
293900           IF    IK = '0'                                         P140
294000     MOVE        'RS06' TO DB-RECORD-NAME                         P140
294100     PERFORM     F84 THRU F84-FN                                  P145
294200           ELSE                                                   P160
294300     MOVE        '0' TO IK                                        P160
294400     PERFORM     F95-RS06-FV-CL THRU F95-RS06-FV-CL-FN            P170
294500         GO TO     F50FF-FN.                                      P180
294600 F50FF-900. GO TO F50FF.                                          P180
294700 F50FF-FN. EXIT.                                                  P180
294800 F50GG.    IF    IK = '0'                                         P000
294900           NEXT SENTENCE ELSE GO TO     F50GG-FN.                 P000
295000     PERFORM     F95-RS07-FV THRU F95-RS07-FV-FN.                 P100
295100           IF    IK = '0'                                         P140
295200     MOVE        'RS07' TO DB-RECORD-NAME                         P140
295300     PERFORM     F84 THRU F84-FN                                  P145
295400           ELSE                                                   P160
295500     MOVE        '0' TO IK                                        P160
295600         GO TO     F50GG-FN.                                      P180
295700 F50GG-900. GO TO F50GG.                                          P180
295800 F50GG-FN. EXIT.                                                  P180
295900 F50II.                                                           P000
296000     PERFORM     F95-RS08-FV THRU F95-RS08-FV-FN.                 P100
296100           IF    IK = '0'                                         P110
296200     MOVE        'RS08' TO W-WW00-ZRECOR                          P110
296300     PERFORM     F84 THRU F84-FN                                  P120
296400         GO TO     F50II-FN.                                      P130
296500     PERFORM     F95-RS09-FV THRU F95-RS09-FV-FN.                 P200
296600           IF    IK = '0'                                         P210
296700     MOVE        'RS09' TO W-WW00-ZRECOR                          P210
296800     PERFORM     F84 THRU F84-FN                                  P220
296900         GO TO     F50II-FN.                                      P230
297000     PERFORM     F95-RS10-FV THRU F95-RS10-FV-FN.                 P300
297100           IF    IK = '0'                                         P310
297200     MOVE        'RS10' TO W-WW00-ZRECOR                          P310
297300     PERFORM     F84 THRU F84-FN                                  P320
297400         GO TO     F50II-FN.                                      P330
297500     PERFORM     F95-RS11-FV THRU F95-RS11-FV-FN.                 P400
297600           IF    IK = '0'                                         P410
297700     MOVE        'RS11' TO W-WW00-ZRECOR                          P410
297800     PERFORM     F84 THRU F84-FN                                  P420
297900         GO TO     F50II-FN.                                      P430
298000     PERFORM     F95-RS12-FV THRU F95-RS12-FV-FN.                 P500
298100           IF    IK = '0'                                         P510
298200     MOVE        'RS12' TO W-WW00-ZRECOR                          P510
298300     PERFORM     F84 THRU F84-FN                                  P520
298400         GO TO     F50II-FN.                                      P530
298500     PERFORM     F95-RS13-FV THRU F95-RS13-FV-FN.                 P600
298600           IF    IK = '0'                                         P610
298700     MOVE        'RS13' TO W-WW00-ZRECOR                          P610
298800     PERFORM     F84 THRU F84-FN                                  P620
298900         GO TO     F50II-FN.                                      P630
299000     PERFORM     F95-RS14-FV THRU F95-RS14-FV-FN.                 P700
299100           IF    IK = '0'                                         P710
299200     MOVE        'RS14' TO W-WW00-ZRECOR                          P710
299300     PERFORM     F84 THRU F84-FN                                  P720
299400         GO TO     F50II-FN.                                      P730
299500     PERFORM     F95-RS15-FV THRU F95-RS15-FV-FN.                 P800
299600           IF    IK = '0'                                         P810
299700     MOVE        'RS15' TO W-WW00-ZRECOR                          P810
299800     PERFORM     F84 THRU F84-FN                                  P820
299900         GO TO     F50II-FN.                                      P830
300000     PERFORM     F95-RS20-FV THRU F95-RS20-FV-FN.                 P900
300100           IF    IK = '0'                                         P910
300200     MOVE        'RS20' TO W-WW00-ZRECOR                          P910
300300     PERFORM     F84 THRU F84-FN                                  P920
300400           ELSE                                                   P930
300500     MOVE        '0' TO IK.                                       P930
300600 F50II-FN. EXIT.                                                  P930
300700 F50PP.    IF    IK = '0'                                         P000
300800           NEXT SENTENCE ELSE GO TO     F50PP-FN.                 P000
300900     PERFORM     F95-RS16-FV THRU F95-RS16-FV-FN.                 P100
301000           IF    IK = '0'                                         P140
301100     MOVE        'RS16' TO DB-RECORD-NAME                         P140
301200     PERFORM     F84 THRU F84-FN                                  P145
301300           ELSE                                                   P160
301400     MOVE        '0' TO IK                                        P160
301500         GO TO     F50PP-FN.                                      P180
301600 F50PP-900. GO TO F50PP.                                          P180
301700 F50PP-FN. EXIT.                                                  P180
301800 F50QQ.    IF    IK = '0'                                         P000
301900           NEXT SENTENCE ELSE GO TO     F50QQ-FN.                 P000
302000     PERFORM     F95-RS17-FV THRU F95-RS17-FV-FN.                 P100
302100           IF    IK = '0'                                         P140
302200     MOVE        'RS17' TO DB-RECORD-NAME                         P140
302300     PERFORM     F84 THRU F84-FN                                  P145
302400           ELSE                                                   P160
302500     MOVE        '0' TO IK                                        P160
302600         GO TO     F50QQ-FN.                                      P180
302700 F50RR.    IF    IK = '0'                                         P000
302800           NEXT SENTENCE ELSE GO TO     F50RR-FN.                 P000
302900     PERFORM     F95-RS18-FV THRU F95-RS18-FV-FN.                 P100
303000           IF    IK = '0'                                         P140
303100     MOVE        'RS18' TO DB-RECORD-NAME                         P140
303200     PERFORM     F84 THRU F84-FN                                  P145
303300           ELSE                                                   P160
303400     MOVE        '0' TO IK                                        P160
303500         GO TO     F50RR-FN.                                      P180
303600 F50RR-900. GO TO F50RR.                                          P180
303700 F50RR-FN. EXIT.                                                  P180
303800 F50QQ-900. GO TO F50QQ.                                          P180
303900 F50QQ-FN. EXIT.                                                  P180
304000 F50SS.    IF    IK = '0'                                         P000
304100           NEXT SENTENCE ELSE GO TO     F50SS-FN.                 P000
304200     PERFORM     F95-RS23-FV THRU F95-RS23-FV-FN.                 P100
304300           IF    IK = '0'                                         P140
304400     MOVE        'RS23' TO DB-RECORD-NAME                         P140
304500     PERFORM     F84 THRU F84-FN                                  P145
304600           ELSE                                                   P160
304700     MOVE        '0' TO IK                                        P160
304800         GO TO     F50SS-FN.                                      P180
304900 F50SS-900. GO TO F50SS.                                          P180
305000 F50SS-FN. EXIT.                                                  P180
305100 F50TT.    IF    IK = '0'                                         P000
305200           NEXT SENTENCE ELSE GO TO     F50TT-FN.                 P000
305300     PERFORM     F95-RS19-FV THRU F95-RS19-FV-FN.                 P100
305400           IF    IK = '0'                                         P140
305500     MOVE        'RS19' TO DB-RECORD-NAME                         P140
305600     PERFORM     F84 THRU F84-FN                                  P145
305700           ELSE                                                   P160
305800     MOVE        '0' TO IK                                        P160
305900         GO TO     F50TT-FN.                                      P180
306000 F50TT-900. GO TO F50TT.                                          P180
306100 F50TT-FN. EXIT.                                                  P180
306200 F50UU.    IF    IK = '0'                                         P000
306300           NEXT SENTENCE ELSE GO TO     F50UU-FN.                 P000
306400     PERFORM     F95-RS22-FV THRU F95-RS22-FV-FN.                 P100
306500           IF    IK = '0'                                         P140
306600     MOVE        'RS22' TO DB-RECORD-NAME                         P140
306700     PERFORM     F84 THRU F84-FN                                  P145
306800           ELSE                                                   P160
306900     MOVE        '0' TO IK                                        P160
307000         GO TO     F50UU-FN.                                      P180
307100 F50UU-900. GO TO F50UU.                                          P180
307200 F50UU-FN. EXIT.                                                  P180
307300 F50DD-900. GO TO F50DD.                                          P180
307400 F50DD-FN. EXIT.                                                  P180
307500 F50BB-FN. EXIT.                                                  P180
307600 F50XX.                                                           P000
307700     MOVE                     ALL '1' TO FT GO TO F20.            P100
307800 F50XX-FN. EXIT.                                                  P100
307900 F50-FN.   EXIT.                                                  P100
308000 F84.           EXIT.                                             RSA030
308100 F84BB.                                                           RSA030
308200           IF    DB-RECORD-NAME = 'RS04'                          RSA030
308300                 OR 5-EW00-4CL NOT < 5-EW00-4CLM                  RSA030
308400           MOVE                  01 TO 5-EW00-4CL                 RSA030
308500           ADD      4-BB-NL TO         5-EW00-4CL                 RSA030
308600           MOVE      'BB' TO CAT (J00) ADD 1 TO J00.              RSA030
308700 F84BB-FN. EXIT.                                                  RSA030
308800 F84CC.                                                           RSA030
308900           IF    DB-RECORD-NAME = 'RS05'                          RSA030
309000           ADD      4-CC-NL TO         5-EW00-4CL                 RSA030
309100           MOVE      'CC' TO CAT (J00) ADD 1 TO J00.              RSA030
309200 F84CC-FN. EXIT.                                                  RSA030
309300 F84DD.                                                           RSA030
309400           IF    DB-RECORD-NAME = 'RS06'                          RSA030
309500           ADD      4-DD-NL TO         5-EW00-4CL                 RSA030
309600           MOVE      'DD' TO CAT (J00) ADD 1 TO J00.              RSA030
309700 F84DD-FN. EXIT.                                                  RSA030
309800 F84EE.                                                           RSA030
309900           IF    DB-RECORD-NAME = 'RS07'                          RSA030
310000           ADD      4-EE-NL TO         5-EW00-4CL                 RSA030
310100           MOVE      'EE' TO CAT (J00) ADD 1 TO J00.              RSA030
310200 F84EE-FN. EXIT.                                                  RSA030
310300 F84FF.                                                           RSA030
310400           IF    DB-RECORD-NAME = 'RS08'                          RSA030
310500           ADD      4-FF-NL TO         5-EW00-4CL                 RSA030
310600           MOVE      'FF' TO CAT (J00) ADD 1 TO J00.              RSA030
310700 F84FF-FN. EXIT.                                                  RSA030
310800 F84GG.                                                           RSA030
310900           IF    DB-RECORD-NAME = 'RS09'                          RSA030
311000           ADD      4-GG-NL TO         5-EW00-4CL                 RSA030
311100           MOVE      'GG' TO CAT (J00) ADD 1 TO J00.              RSA030
311200 F84GG-FN. EXIT.                                                  RSA030
311300 F84HH.                                                           RSA030
311400           IF    DB-RECORD-NAME = 'RS10'                          RSA030
311500           ADD      4-HH-NL TO         5-EW00-4CL                 RSA030
311600           MOVE      'HH' TO CAT (J00) ADD 1 TO J00.              RSA030
311700 F84HH-FN. EXIT.                                                  RSA030
311800 F84II.                                                           RSA030
311900           IF    DB-RECORD-NAME = 'RS11'                          RSA030
312000           ADD      4-II-NL TO         5-EW00-4CL                 RSA030
312100           MOVE      'II' TO CAT (J00) ADD 1 TO J00.              RSA030
312200 F84II-FN. EXIT.                                                  RSA030
312300 F84JJ.                                                           RSA030
312400           IF    DB-RECORD-NAME = 'RS12'                          RSA030
312500           ADD      4-JJ-NL TO         5-EW00-4CL                 RSA030
312600           MOVE      'JJ' TO CAT (J00) ADD 1 TO J00.              RSA030
312700 F84JJ-FN. EXIT.                                                  RSA030
312800 F84KK.                                                           RSA030
312900           IF    DB-RECORD-NAME = 'RS13'                          RSA030
313000           ADD      4-KK-NL TO         5-EW00-4CL                 RSA030
313100           MOVE      'KK' TO CAT (J00) ADD 1 TO J00.              RSA030
313200 F84KK-FN. EXIT.                                                  RSA030
313300 F84LL.                                                           RSA030
313400           IF    DB-RECORD-NAME = 'RS14'                          RSA030
313500           ADD      4-LL-NL TO         5-EW00-4CL                 RSA030
313600           MOVE      'LL' TO CAT (J00) ADD 1 TO J00.              RSA030
313700 F84LL-FN. EXIT.                                                  RSA030
313800 F84MM.                                                           RSA030
313900           IF    DB-RECORD-NAME = 'RS15'                          RSA030
314000           ADD      4-MM-NL TO         5-EW00-4CL                 RSA030
314100           MOVE      'MM' TO CAT (J00) ADD 1 TO J00.              RSA030
314200 F84MM-FN. EXIT.                                                  RSA030
314300 F84NN.                                                           RSA030
314400           IF    DB-RECORD-NAME = 'RS16'                          RSA030
314500           ADD      4-NN-NL TO         5-EW00-4CL                 RSA030
314600           MOVE      'NN' TO CAT (J00) ADD 1 TO J00.              RSA030
314700 F84NN-FN. EXIT.                                                  RSA030
314800 F84OO.                                                           RSA030
314900           IF    DB-RECORD-NAME = 'RS17'                          RSA030
315000           ADD      4-OO-NL TO         5-EW00-4CL                 RSA030
315100           MOVE      'OO' TO CAT (J00) ADD 1 TO J00.              RSA030
315200 F84OO-FN. EXIT.                                                  RSA030
315300 F84PP.                                                           RSA030
315400           IF    DB-RECORD-NAME = 'RS18'                          RSA030
315500           ADD      4-PP-NL TO         5-EW00-4CL                 RSA030
315600           MOVE      'PP' TO CAT (J00) ADD 1 TO J00.              RSA030
315700 F84PP-FN. EXIT.                                                  RSA030
315800 F84RR.                                                           RSA030
315900           IF    DB-RECORD-NAME = 'RS19'                          RSA030
316000           ADD      4-RR-NL TO         5-EW00-4CL                 RSA030
316100           MOVE      'RR' TO CAT (J00) ADD 1 TO J00.              RSA030
316200 F84RR-FN. EXIT.                                                  RSA030
316300 F84TT.                                                           RSA030
316400           IF    DB-RECORD-NAME = 'RS22'                          RSA030
316500           ADD      4-TT-NL TO         5-EW00-4CL                 RSA030
316600           MOVE      'TT' TO CAT (J00) ADD 1 TO J00.              RSA030
316700 F84TT-FN. EXIT.                                                  RSA030
316800 F84VV.                                                           RSA030
316900           IF    DB-RECORD-NAME = 'RS23'                          RSA030
317000           ADD      4-VV-NL TO         5-EW00-4CL                 RSA030
317100           MOVE      'VV' TO CAT (J00) ADD 1 TO J00.              RSA030
317200 F84VV-FN. EXIT.                                                  RSA030
317300 F84WW.                                                           RSA030
317400           IF    DB-RECORD-NAME = 'RS20'                          RSA030
317500           ADD      4-WW-NL TO         5-EW00-4CL                 RSA030
317600           MOVE      'WW' TO CAT (J00) ADD 1 TO J00.              RSA030
317700 F84WW-FN. EXIT.                                                  RSA030
317800 F84ZZ.    MOVE 1 TO J00.                                         RSA030
317900 F84ZZ-005. MOVE CAT (J00) TO CATX. IF CATX = '  '                RSA030
318000           MOVE 1 TO J00 MOVE SPACE TO CAT-TAB                    RSA030
318100           GO TO     F8499-FN.  MOVE 0 TO J01.                    RSA030
318200           IF CATX                 = 'BB'                         RSA030
318300           MOVE  TS-4-BB TO ST-TA GO TO F84ZZ-009.                RSA030
318400           IF CATX                 = 'CC'                         RSA030
318500           MOVE  TS-4-CC TO ST-TA GO TO F84ZZ-009.                RSA030
318600           IF CATX                 = 'DD'                         RSA030
318700           MOVE  TS-4-DD TO ST-TA GO TO F84ZZ-009.                RSA030
318800           IF CATX                 = 'EE'                         RSA030
318900           MOVE  TS-4-EE TO ST-TA GO TO F84ZZ-009.                RSA030
319000           IF CATX                 = 'FF'                         RSA030
319100           MOVE  TS-4-FF TO ST-TA GO TO F84ZZ-009.                RSA030
319200           IF CATX                 = 'GG'                         RSA030
319300           MOVE  TS-4-GG TO ST-TA GO TO F84ZZ-009.                RSA030
319400           IF CATX                 = 'HH'                         RSA030
319500           MOVE  TS-4-HH TO ST-TA GO TO F84ZZ-009.                RSA030
319600           IF CATX                 = 'II'                         RSA030
319700           MOVE  TS-4-II TO ST-TA GO TO F84ZZ-009.                RSA030
319800           IF CATX                 = 'JJ'                         RSA030
319900           MOVE  TS-4-JJ TO ST-TA GO TO F84ZZ-009.                RSA030
320000           IF CATX                 = 'KK'                         RSA030
320100           MOVE  TS-4-KK TO ST-TA GO TO F84ZZ-009.                RSA030
320200           IF CATX                 = 'LL'                         RSA030
320300           MOVE  TS-4-LL TO ST-TA GO TO F84ZZ-009.                RSA030
320400           IF CATX                 = 'MM'                         RSA030
320500           MOVE  TS-4-MM TO ST-TA GO TO F84ZZ-009.                RSA030
320600           IF CATX                 = 'NN'                         RSA030
320700           MOVE  TS-4-NN TO ST-TA GO TO F84ZZ-009.                RSA030
320800           IF CATX                 = 'OO'                         RSA030
320900           MOVE  TS-4-OO TO ST-TA GO TO F84ZZ-009.                RSA030
321000           IF CATX                 = 'PP'                         RSA030
321100           MOVE  TS-4-PP TO ST-TA GO TO F84ZZ-009.                RSA030
321200           IF CATX                 = 'RR'                         RSA030
321300           MOVE  TS-4-RR TO ST-TA GO TO F84ZZ-009.                RSA030
321400           IF CATX                 = 'TT'                         RSA030
321500           MOVE  TS-4-TT TO ST-TA GO TO F84ZZ-009.                RSA030
321600           IF CATX                 = 'VV'                         RSA030
321700           MOVE  TS-4-VV TO ST-TA GO TO F84ZZ-009.                RSA030
321800           IF CATX                 = 'WW'                         RSA030
321900           MOVE  TS-4-WW TO ST-TA GO TO F84ZZ-009.                RSA030
322000 F84ZZ-009. ADD 1 TO J01.                                         RSA030
322100 F84ZZ-010. MOVE ST-TT (J01) TO ST-SLS.                           RSA030
322200           IF  ST-SLS = SPACE                                     RSA030
322300           ADD 1 TO J00           GO TO F84ZZ-005.                RSA030
322400          IF J02 = '00' MOVE SPACE TO 6-EW400 ELSE                RSA030
322500           MOVE 1-LI00-4 (J02)     TO 6-EW400.                    RSA030
322600           IF ST-ABS NOT = ' ' AND SAUT = '01'                    RSA030
322700           ADD         1            TO 5-EW00-4CP.                RSA030
322800 F84ZZ-FN. EXIT.                                                  RSA030
322900 F8400.                                                           RSA030
323000           IF STX = '00'          GO TO F8499.                    RSA030
323100           GO TO                        F8401                     RSA030
323200                                        F8402                     RSA030
323300                                        F8403                     RSA030
323400                                        F8404                     RSA030
323500                                        F8405                     RSA030
323600                                        F8406                     RSA030
323700                                        F8407                     RSA030
323800                                        F8408                     RSA030
323900                                        F8409                     RSA030
324000                                        F8410                     RSA030
324100                                        F8411                     RSA030
324200                                        F8412                     RSA030
324300                                        F8413                     RSA030
324400                                        F8414                     RSA030
324500                                        F8415                     RSA030
324600                                        F8416                     RSA030
324700                                        F8417                     RSA030
324800                                        F8418                     RSA030
324900                                        F8419                     RSA030
325000                                        F8420                     RSA030
325100                                        F8421                     RSA030
325200                                        F8422                     RSA030
325300                                        F8423                     RSA030
325400                                        F8424                     RSA030
325500                                        F8425                     RSA030
325600                                        F8426                     RSA030
325700                                        F8427                     RSA030
325800                                        F8428                     RSA030
325900                                        F8429                     RSA030
326000                                        F8430                     RSA030
326100                                        F8431                     RSA030
326200                                        F8432                     RSA030
326300                                        F8433                     RSA030
326400                                        F8434                     RSA030
326500                                        F8435                     RSA030
326600                                        F8436                     RSA030
326700                                        F8437                     RSA030
326800                                        F8438                     RSA030
326900                                        F8439                     RSA030
327000                                        F8440                     RSA030
327100                                        F8441                     RSA030
327200                                        F8442                     RSA030
327300                                        F8443                     RSA030
327400                                        F8444                     RSA030
327500                                        F8445                     RSA030
327600                                        F8446                     RSA030
327700                                        F8447                     RSA030
327800                                        F8448                     RSA030
327900                                        F8449                     RSA030
328000                                        F8450                     RSA030
328100                                        F8451                     RSA030
328200                                        F8452                     RSA030
328300                                        F8453                     RSA030
328400                                        F8454                     RSA030
328500                                        F8455                     RSA030
328600                                        F8456                     RSA030
328700                                        F8457                     RSA030
328800                                        F8458                     RSA030
328900                                        F8459                     RSA030
329000                                        F8460                     RSA030
329100           DEPENDING ON ST9.                                      RSA030
329200 F8400-FN. EXIT.                                                  RSA030
329300 F8401.                                                           RSA030
329400           MOVE  DATOR           TO   6-EW401-DJREN.              RSA030
329500           MOVE   5-EW00-4CP     TO   6-EW401-ZNOPAG.             RSA030
329600 F8401-99. GO TO  F8499.                                          RSA030
329700 F8401-FN. EXIT.                                                  RSA030
329800 F8402.                                                           RSA030
329900           MOVE     RS04-NOREN   TO   6-EW402-NOREN.              RSA030
330000 F8402-99. GO TO  F8499.                                          RSA030
330100 F8402-FN. EXIT.                                                  RSA030
330200 F8403.                                                           RSA030
330300           MOVE     RS05-NOPAM   TO   6-EW403-NOPAM.              RSA030
330400           MOVE     RS05-CDPAMR  TO   6-EW403-CDPAMR.             RSA030
330500           MOVE     RS05-DMRENB  TO   6-EW403-DMRENB.             RSA030
330600 F8403-99. GO TO  F8499.                                          RSA030
330700 F8403-FN. EXIT.                                                  RSA030
330800 F8404.                                                           RSA030
330900           MOVE     RS05-NCRENR  TO   6-EW404-NCRENR.             RSA030
331000           MOVE     RS05-NCREER  TO   6-EW404-NCREER.             RSA030
331100 F8404-99. GO TO  F8499.                                          RSA030
331200 F8404-FN. EXIT.                                                  RSA030
331300 F8405.                                                           RSA030
331400           MOVE     RS05-NCREGR  TO   6-EW405-NCREGR.             RSA030
331500           MOVE     RS05-NCREBR  TO   6-EW405-NCREBR.             RSA030
331600           MOVE     RS05-NCRECR  TO   6-EW405-NCRECR.             RSA030
331700 F8405-99. GO TO  F8499.                                          RSA030
331800 F8405-FN. EXIT.                                                  RSA030
331900 F8406.                                                           RSA030
332000           MOVE     RS06-DMREN   TO   6-EW406-DMREN.              RSA030
332100           MOVE     RS06-DDREN   TO   6-EW406-DDREN.              RSA030
332200           MOVE     RS06-MTREA   TO   6-EW406-MTREA.              RSA030
332300 F8406-99. GO TO  F8499.                                          RSA030
332400 F8406-FN. EXIT.                                                  RSA030
332500 F8407.                                                           RSA030
332600           MOVE     RS06-DDREJ   TO   6-EW407-DDREJ.              RSA030
332700           MOVE     RS06-DTREC   TO   6-EW407-DTREC.              RSA030
332800           MOVE     RS06-TXREI   TO   6-EW407-TXREI.              RSA030
332900 F8407-99. GO TO  F8499.                                          RSA030
333000 F8407-FN. EXIT.                                                  RSA030
333100 F8408.                                                           RSA030
333200           MOVE     RS07-DMREN   TO   6-EW408-DMREN.              RSA030
333300           MOVE     RS07-MKREN   TO   6-EW408-MKREN.              RSA030
333400           MOVE     RS07-CSREE   TO   6-EW408-CSREE.              RSA030
333500 F8408-99. GO TO  F8499.                                          RSA030
333600 F8408-FN. EXIT.                                                  RSA030
333700 F8409.                                                           RSA030
333800           MOVE     RS07-MOREC   TO   6-EW409-MOREC.              RSA030
333900           MOVE     RS07-LIREK   TO   6-EW409-LIREK.              RSA030
334000 F8409-99. GO TO  F8499.                                          RSA030
334100 F8409-FN. EXIT.                                                  RSA030
334200 F8410.                                                           RSA030
334300           MOVE     RS08-DDREJ   TO   6-EW410-DDREJ.              RSA030
334400           MOVE     RS08-CDREJ   TO   6-EW410-CDREJ.              RSA030
334500           MOVE     RS08-CTRER   TO   6-EW410-CTRER.              RSA030
334600 F8410-99. GO TO  F8499.                                          RSA030
334700 F8410-FN. EXIT.                                                  RSA030
334800 F8411.                                                           RSA030
334900           MOVE     RS08-CTREA   TO   6-EW411-CTREA.              RSA030
335000           MOVE     RS08-TXREI   TO   6-EW411-TXREI.              RSA030
335100           MOVE     RS08-CERETP  TO   6-EW411-CERETP.             RSA030
335200 F8411-99. GO TO  F8499.                                          RSA030
335300 F8411-FN. EXIT.                                                  RSA030
335400 F8412.                                                           RSA030
335500           MOVE     RS08-MTRES   TO   6-EW412-MTRES.              RSA030
335600           MOVE     RS08-MOREC   TO   6-EW412-MOREC.              RSA030
335700 F8412-99. GO TO  F8499.                                          RSA030
335800 F8412-FN. EXIT.                                                  RSA030
335900 F8413.                                                           RSA030
336000           MOVE     RS09-TXREO   TO   6-EW413-TXREO.              RSA030
336100           MOVE     RS09-CEREO   TO   6-EW413-CEREO.              RSA030
336200           MOVE     RS09-MOREC   TO   6-EW413-MOREC.              RSA030
336300 F8413-99. GO TO  F8499.                                          RSA030
336400 F8413-FN. EXIT.                                                  RSA030
336500 F8414.                                                           RSA030
336600           MOVE     RS09-LNRENO  TO   6-EW414-LNRENO.             RSA030
336700           MOVE     RS09-LNREPO  TO   6-EW414-LNREPO.             RSA030
336800           MOVE     RS09-CDREXO  TO   6-EW414-CDREXO.             RSA030
336900 F8414-99. GO TO  F8499.                                          RSA030
337000 F8414-FN. EXIT.                                                  RSA030
337100 F8415.                                                           RSA030
337200           MOVE     RS09-DTRENO  TO   6-EW415-DTRENO.             RSA030
337300           MOVE     RS09-NORENR  TO   6-EW415-NORENR.             RSA030
337400 F8415-99. GO TO  F8499.                                          RSA030
337500 F8415-FN. EXIT.                                                  RSA030
337600 F8416.                                                           RSA030
337700           MOVE     RS10-CTRER   TO   6-EW416-CTRER.              RSA030
337800           MOVE     RS10-CTREA   TO   6-EW416-CTREA.              RSA030
337900           MOVE     RS10-MOREC   TO   6-EW416-MOREC.              RSA030
338000 F8416-99. GO TO  F8499.                                          RSA030
338100 F8416-FN. EXIT.                                                  RSA030
338200 F8417.                                                           RSA030
338300           MOVE     RS10-CTREEX  TO   6-EW417-CTREEX.             RSA030
338400           MOVE     RS10-DTREO   TO   6-EW417-DTREO.              RSA030
338500 F8417-99. GO TO  F8499.                                          RSA030
338600 F8417-FN. EXIT.                                                  RSA030
338700 F8418.                                                           RSA030
338800           MOVE     RS10-MTPAC   TO   6-EW418-MTPAC.              RSA030
338900 F8418-99. GO TO  F8499.                                          RSA030
339000 F8418-FN. EXIT.                                                  RSA030
339100 F8419.                                                           RSA030
339200           MOVE     RS11-CTRER   TO   6-EW419-CTRER.              RSA030
339300           MOVE     RS11-CTREA   TO   6-EW419-CTREA.              RSA030
339400           MOVE     RS11-TXPOO   TO   6-EW419-TXPOO.              RSA030
339500           MOVE     RS11-LIPOT   TO   6-EW419-LIPOT.              RSA030
339600 F8419-99. GO TO  F8499.                                          RSA030
339700 F8419-FN. EXIT.                                                  RSA030
339800 F8420.                                                           RSA030
339900           MOVE     RS11-LIPOTB  TO   6-EW420-LIPOTB.             RSA030
340000           MOVE     RS11-MTPOO   TO   6-EW420-MTPOO.              RSA030
340100           MOVE     RS11-MTRECR  TO   6-EW420-MTRECR.             RSA030
340200 F8420-99. GO TO  F8499.                                          RSA030
340300 F8420-FN. EXIT.                                                  RSA030
340400 F8421.                                                           RSA030
340500           MOVE     RS12-DDREJ   TO   6-EW421-DDREJ.              RSA030
340600           MOVE     RS12-CDREJ   TO   6-EW421-CDREJ.              RSA030
340700           MOVE     RS12-MOREC   TO   6-EW421-MOREC.              RSA030
340800 F8421-99. GO TO  F8499.                                          RSA030
340900 F8421-FN. EXIT.                                                  RSA030
341000 F8422.                                                           RSA030
341100           MOVE     RS12-DTREO   TO   6-EW422-DTREO.              RSA030
341200           MOVE     RS12-MTPAC   TO   6-EW422-MTPAC.              RSA030
341300           MOVE     RS12-LNRENS  TO   6-EW422-LNRENS.             RSA030
341400 F8422-99. GO TO  F8499.                                          RSA030
341500 F8422-FN. EXIT.                                                  RSA030
341600 F8423.                                                           RSA030
341700           MOVE     RS12-NORES   TO   6-EW423-NORES.              RSA030
341800           MOVE     RS12-LNRENA  TO   6-EW423-LNRENA.             RSA030
341900           MOVE     RS12-TXREI   TO   6-EW423-TXREI.              RSA030
342000 F8423-99. GO TO  F8499.                                          RSA030
342100 F8423-FN. EXIT.                                                  RSA030
342200 F8424.                                                           RSA030
342300           MOVE     RS12-CNREK   TO   6-EW424-CNREK.              RSA030
342400           MOVE     RS12-MKREL   TO   6-EW424-MKREL.              RSA030
342500           MOVE     RS12-CCREM   TO   6-EW424-CCREM.              RSA030
342600 F8424-99. GO TO  F8499.                                          RSA030
342700 F8424-FN. EXIT.                                                  RSA030
342800 F8425.                                                           RSA030
342900           MOVE     RS12-CDREB   TO   6-EW425-CDREB.              RSA030
343000           MOVE     RS12-CTRESS  TO   6-EW425-CTRESS.             RSA030
343100           MOVE     RS12-CDREPS  TO   6-EW425-CDREPS.             RSA030
343200 F8425-99. GO TO  F8499.                                          RSA030
343300 F8425-FN. EXIT.                                                  RSA030
343400 F8426.                                                           RSA030
343500           MOVE     RS12-CNREOS  TO   6-EW426-CNREOS.             RSA030
343600 F8426-99. GO TO  F8499.                                          RSA030
343700 F8426-FN. EXIT.                                                  RSA030
343800 F8427.                                                           RSA030
343900           MOVE     RS13-DDREJ   TO   6-EW427-DDREJ.              RSA030
344000           MOVE     RS13-CDREJ   TO   6-EW427-CDREJ.              RSA030
344100           MOVE     RS13-MOREC   TO   6-EW427-MOREC.              RSA030
344200 F8427-99. GO TO  F8499.                                          RSA030
344300 F8427-FN. EXIT.                                                  RSA030
344400 F8428.                                                           RSA030
344500           MOVE     RS13-DTREO   TO   6-EW428-DTREO.              RSA030
344600           MOVE     RS13-MTPAC   TO   6-EW428-MTPAC.              RSA030
344700           MOVE     RS13-LNRENS  TO   6-EW428-LNRENS.             RSA030
344800 F8428-99. GO TO  F8499.                                          RSA030
344900 F8428-FN. EXIT.                                                  RSA030
345000 F8429.                                                           RSA030
345100           MOVE     RS13-NORES   TO   6-EW429-NORES.              RSA030
345200           MOVE     RS13-LNRENA  TO   6-EW429-LNRENA.             RSA030
345300           MOVE     RS13-TXREI   TO   6-EW429-TXREI.              RSA030
345400 F8429-99. GO TO  F8499.                                          RSA030
345500 F8429-FN. EXIT.                                                  RSA030
345600 F8430.                                                           RSA030
345700           MOVE     RS13-CNREK   TO   6-EW430-CNREK.              RSA030
345800           MOVE     RS13-MKREL   TO   6-EW430-MKREL.              RSA030
345900           MOVE     RS13-MTREAO  TO   6-EW430-MTREAO.             RSA030
346000 F8430-99. GO TO  F8499.                                          RSA030
346100 F8430-FN. EXIT.                                                  RSA030
346200 F8431.                                                           RSA030
346300           MOVE     RS13-CTREDC  TO   6-EW431-CTREDC.             RSA030
346400           MOVE     RS13-CCREM   TO   6-EW431-CCREM.              RSA030
346500           MOVE     RS13-CDREB   TO   6-EW431-CDREB.              RSA030
346600 F8431-99. GO TO  F8499.                                          RSA030
346700 F8431-FN. EXIT.                                                  RSA030
346800 F8432.                                                           RSA030
346900           MOVE     RS13-CTREJ   TO   6-EW432-CTREJ.              RSA030
347000           MOVE     RS13-KMREDC  TO   6-EW432-KMREDC.             RSA030
347100 F8432-99. GO TO  F8499.                                          RSA030
347200 F8432-FN. EXIT.                                                  RSA030
347300 F8433.                                                           RSA030
347400           MOVE     RS13-CDREAT  TO   6-EW433-CDREAT.             RSA030
347500           MOVE     RS13-DTRENV  TO   6-EW433-DTRENV.             RSA030
347600           MOVE     RS13-CDREX   TO   6-EW433-CDREX.              RSA030
347700 F8433-99. GO TO  F8499.                                          RSA030
347800 F8433-FN. EXIT.                                                  RSA030
347900 F8434.                                                           RSA030
348000           MOVE     RS14-NOCRM   TO   6-EW434-NOCRM.              RSA030
348100           MOVE     RS14-NOREMS  TO   6-EW434-NOREMS.             RSA030
348200           MOVE     RS14-CDRETX  TO   6-EW434-CDRETX.             RSA030
348300 F8434-99. GO TO  F8499.                                          RSA030
348400 F8434-FN. EXIT.                                                  RSA030
348500 F8435.                                                           RSA030
348600           MOVE     RS14-TXREAS  TO   6-EW435-TXREAS.             RSA030
348700           MOVE     RS14-NORENR  TO   6-EW435-NORENR.             RSA030
348800 F8435-99. GO TO  F8499.                                          RSA030
348900 F8435-FN. EXIT.                                                  RSA030
349000 F8436.                                                           RSA030
349100           MOVE     RS15-PEREA   TO   6-EW436-PEREA.              RSA030
349200           MOVE     RS15-MTRECR  TO   6-EW436-MTRECR.             RSA030
349300           MOVE     RS15-NORENR  TO   6-EW436-NORENR.             RSA030
349400 F8436-99. GO TO  F8499.                                          RSA030
349500 F8436-FN. EXIT.                                                  RSA030
349600 F8437.                                                           RSA030
349700           MOVE     RS16-DMREN   TO   6-EW437-DMREN.              RSA030
349800           MOVE     RS16-NOTIE   TO   6-EW437-NOTIE.              RSA030
349900 F8437-99. GO TO  F8499.                                          RSA030
350000 F8437-FN. EXIT.                                                  RSA030
350100 F8438.                                                           RSA030
350200           MOVE     RS17-NORCS   TO   6-EW438-NORCS.              RSA030
350300           MOVE     RS17-DMREN   TO   6-EW438-DMREN.              RSA030
350400           MOVE     RS17-CERCS   TO   6-EW438-CERCS.              RSA030
350500 F8438-99. GO TO  F8499.                                          RSA030
350600 F8438-FN. EXIT.                                                  RSA030
350700 F8439.                                                           RSA030
350800           MOVE     RS17-LIRCS   TO   6-EW439-LIRCS.              RSA030
350900           MOVE     RS17-DDRCS   TO   6-EW439-DDRCS.              RSA030
351000 F8439-99. GO TO  F8499.                                          RSA030
351100 F8439-FN. EXIT.                                                  RSA030
351200 F8440.                                                           RSA030
351300           MOVE     RS17-CTRCK   TO   6-EW440-CTRCK.              RSA030
351400           MOVE     RS17-MKRCL   TO   6-EW440-MKRCL.              RSA030
351500           MOVE     RS17-DHRCS   TO   6-EW440-DHRCS.              RSA030
351600 F8440-99. GO TO  F8499.                                          RSA030
351700 F8440-FN. EXIT.                                                  RSA030
351800 F8441.                                                           RSA030
351900           MOVE     RS17-CTRCB   TO   6-EW441-CTRCB.              RSA030
352000           MOVE     RS17-MTRCA   TO   6-EW441-MTRCA.              RSA030
352100           MOVE     RS17-MTRCG   TO   6-EW441-MTRCG.              RSA030
352200 F8441-99. GO TO  F8499.                                          RSA030
352300 F8441-FN. EXIT.                                                  RSA030
352400 F8442.                                                           RSA030
352500           MOVE     RS17-MCRCCR  TO   6-EW442-MCRCCR.             RSA030
352600           MOVE     RS17-DTRCB   TO   6-EW442-DTRCB.              RSA030
352700           MOVE     RS17-MTRCB   TO   6-EW442-MTRCB.              RSA030
352800 F8442-99. GO TO  F8499.                                          RSA030
352900 F8442-FN. EXIT.                                                  RSA030
353000 F8443.                                                           RSA030
353100           MOVE     RS17-LNRCA   TO   6-EW443-LNRCA.              RSA030
353200           MOVE     RS17-MORCS   TO   6-EW443-MORCS.              RSA030
353300 F8443-99. GO TO  F8499.                                          RSA030
353400 F8443-FN. EXIT.                                                  RSA030
353500 F8444.                                                           RSA030
353600           MOVE     RS17-LRRE1   TO   6-EW444-LRRE1.              RSA030
353700           MOVE     RS17-LRRE2   TO   6-EW444-LRRE2.              RSA030
353800 F8444-99. GO TO  F8499.                                          RSA030
353900 F8444-FN. EXIT.                                                  RSA030
354000 F8445.                                                           RSA030
354100           MOVE     RS17-LVRE    TO   6-EW445-LVRE.               RSA030
354200           MOVE     RS17-CPREN   TO   6-EW445-CPREN.              RSA030
354300 F8445-99. GO TO  F8499.                                          RSA030
354400 F8445-FN. EXIT.                                                  RSA030
354500 F8446.                                                           RSA030
354600           MOVE     RS17-LIRCSB  TO   6-EW446-LIRCSB.             RSA030
354700           MOVE     RS17-CDREP   TO   6-EW446-CDREP.              RSA030
354800 F8446-99. GO TO  F8499.                                          RSA030
354900 F8446-FN. EXIT.                                                  RSA030
355000 F8447.                                                           RSA030
355100           MOVE     RS18-DTRCB   TO   6-EW447-DTRCB.              RSA030
355200           MOVE     RS18-MTRCBH  TO   6-EW447-MTRCBH.             RSA030
355300           MOVE     RS18-DMREN   TO   6-EW447-DMREN.              RSA030
355400 F8447-99. GO TO  F8499.                                          RSA030
355500 F8447-FN. EXIT.                                                  RSA030
355600 F8448.                                                           RSA030
355700           MOVE     RS19-NOPAI   TO   6-EW448-NOPAI.              RSA030
355800           MOVE     RS19-DHPAI   TO   6-EW448-DHPAI.              RSA030
355900           MOVE     RS19-DTPAI   TO   6-EW448-DTPAI.              RSA030
356000 F8448-99. GO TO  F8499.                                          RSA030
356100 F8448-FN. EXIT.                                                  RSA030
356200 F8449.                                                           RSA030
356300           MOVE     RS19-MTPAI   TO   6-EW449-MTPAI.              RSA030
356400           MOVE     RS19-MORER   TO   6-EW449-MORER.              RSA030
356500 F8449-99. GO TO  F8499.                                          RSA030
356600 F8449-FN. EXIT.                                                  RSA030
356700 F8450.                                                           RSA030
356800           MOVE     RS19-CDPAD   TO   6-EW450-CDPAD.              RSA030
356900           MOVE     RS19-NOTIE   TO   6-EW450-NOTIE.              RSA030
357000           MOVE     RS19-NOPAM   TO   6-EW450-NOPAM.              RSA030
357100 F8450-99. GO TO  F8499.                                          RSA030
357200 F8450-FN. EXIT.                                                  RSA030
357300 F8451.                                                           RSA030
357400           MOVE     RS19-CDPAM   TO   6-EW451-CDPAM.              RSA030
357500           MOVE     RS19-DTPAIR  TO   6-EW451-DTPAIR.             RSA030
357600           MOVE     RS19-CDPAS   TO   6-EW451-CDPAS.              RSA030
357700 F8451-99. GO TO  F8499.                                          RSA030
357800 F8451-FN. EXIT.                                                  RSA030
357900 F8452.                                                           RSA030
358000           MOVE     RS19-CDPAP   TO   6-EW452-CDPAP.              RSA030
358100           MOVE     RS19-NOPAF   TO   6-EW452-NOPAF.              RSA030
358200           MOVE     RS19-CDPAI   TO   6-EW452-CDPAI.              RSA030
358300 F8452-99. GO TO  F8499.                                          RSA030
358400 F8452-FN. EXIT.                                                  RSA030
358500 F8453.                                                           RSA030
358600           MOVE     RS22-DMREN   TO   6-EW453-DMREN.              RSA030
358700           MOVE     RS22-DHPAI   TO   6-EW453-DHPAI.              RSA030
358800           MOVE     RS22-MTPAIO  TO   6-EW453-MTPAIO.             RSA030
358900 F8453-99. GO TO  F8499.                                          RSA030
359000 F8453-FN. EXIT.                                                  RSA030
359100 F8454.                                                           RSA030
359200           MOVE     RS22-LIPAO   TO   6-EW454-LIPAO.              RSA030
359300           MOVE     RS22-NOTIE   TO   6-EW454-NOTIE.              RSA030
359400 F8454-99. GO TO  F8499.                                          RSA030
359500 F8454-FN. EXIT.                                                  RSA030
359600 F8455.                                                           RSA030
359700           MOVE     RS22-CTPAO   TO   6-EW455-CTPAO.              RSA030
359800           MOVE     RS22-NOPAI   TO   6-EW455-NOPAI.              RSA030
359900           MOVE     RS22-NOREN   TO   6-EW455-NOREN.              RSA030
360000 F8455-99. GO TO  F8499.                                          RSA030
360100 F8455-FN. EXIT.                                                  RSA030
360200 F8456.                                                           RSA030
360300           MOVE     RS22-LIPA2   TO   6-EW456-LIPA2.              RSA030
360400 F8456-99. GO TO  F8499.                                          RSA030
360500 F8456-FN. EXIT.                                                  RSA030
360600 F8457.                                                           RSA030
360700           MOVE     RS23-MTREB   TO   6-EW457-MTREB.              RSA030
360800           MOVE     RS23-PXREF   TO   6-EW457-PXREF.              RSA030
360900 F8457-99. GO TO  F8499.                                          RSA030
361000 F8457-FN. EXIT.                                                  RSA030
361100 F8458.                                                           RSA030
361200           MOVE     RS20-CTREA   TO   6-EW458-CTREA.              RSA030
361300           MOVE     RS20-TXREI   TO   6-EW458-TXREI.              RSA030
361400           MOVE     RS20-DTREO   TO   6-EW458-DTREO.              RSA030
361500 F8458-99. GO TO  F8499.                                          RSA030
361600 F8458-FN. EXIT.                                                  RSA030
361700 F8459.                                                           RSA030
361800           MOVE     RS20-MTREAO  TO   6-EW459-MTREAO.             RSA030
361900           MOVE     RS20-MTREAT  TO   6-EW459-MTREAT.             RSA030
362000 F8459-99. GO TO  F8499.                                          RSA030
362100 F8459-FN. EXIT.                                                  RSA030
362200 F8460.                                                           RSA030
362300           MOVE     RS20-MOREC   TO   6-EW460-MOREC.              RSA030
362400 F8460-99. GO TO  F8499.                                          RSA030
362500 F8460-FN. EXIT.                                                  RSA030
362600 F8499.    MOVE     6-EW00     TO   EW00.                         RSA030
362700           IF ST-ABS = ' '        GO TO F8499-10.                 RSA030
362800           MOVE ' ' TO ST-ABS.                                    RSA030
362900           IF SAUT = '01' MOVE 1    TO 5-EW00-4CL1                RSA030
363000           WRITE EW00    AFTER   ADVANCING SAUTP                  RSA030
363100           GO TO  F8499-20.                                       RSA030
363200           SUBTRACT 5-EW00-4CL1  FROM SAUT.                       RSA030
363300 F8499-10. IF SAUT = '00'                                         RSA030
363400           WRITE EW00    AFTER   ADVANCING SAUT0 ELSE             RSA030
363500           WRITE EW00    AFTER   ADVANCING SAUT                   RSA030
363600           ADD  SAUT                TO 5-EW00-4CL1.               RSA030
363700 F8499-20. ADD 1 TO 5-EW00-4CE.   GO TO F84ZZ-009.                RSA030
363800 F8499-FN. EXIT.                                                  RSA030
363900 F84-FN.   EXIT.                                                  RSA030
364000 F90.      EXIT.                                                  RSA030
364100 F90YU.                                                           P000
364200         GO TO     F90YU-FN.                                      P100
364300 F90YX.                                                           RSA030
364400           WRITE    YX00.                                         RSA030
364500 F90YX-99. ADD 1 TO 5-YX00-CPTENR.                                RSA030
364600 F90YX-FN. EXIT.                                                  RSA030
364700 F90YU-FN. EXIT.                                                  RSA030
364800 F90YZ.         EXIT.                                             P000
364900 F90YZ-FN. EXIT.                                                  P000
365000 F90-FN.   EXIT.                                                  P000
365100 F9099-ITER-FN.  GO TO F05.                                       RSA030
365200 F95-A.                                                           P000
365300     EXEC SQL    WHENEVER SQLWARNING CONTINUE          END-EXEC.  P100
365400     EXEC SQL    WHENEVER NOT FOUND  CONTINUE          END-EXEC.  P200
365500     EXEC SQL    WHENEVER SQLERROR   GO TO F99OR       END-EXEC.  P300
365600 F95-A-FN. EXIT.                                                  P300
365700 F95AA.         EXIT.                                             P000
365800 F95-RS01-NO.                                                     P100
365900     MOVE        'SELECT' TO XO00-XORATY                          P101
366000     MOVE        '95AA' TO XO00-XCDFSF                            P102
366100     MOVE        'RS01' TO XO00-XORATA                            P103
366200     MOVE        RS01 TO XO00-XORACL                              P104
366300     MOVE        ZERO TO XOAA-RS01-CF                             P105
366400     EXEC SQL                                                     P108
366500                 SELECT  NONER                                    P110
366600                   INTO :RS01-NONER                               P210
366700                   FROM  RS01                          END-EXEC.  P300
366800     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
366900           IF    IK = ZERO                                        P485
367000     MOVE        '1' TO XOAA-RS01-CF                              P485
367100     ADD         1 TO XOAA-COUNT                                  P490
367200     MOVE        XP00-XROWID TO RS01-XROWID.                      P495
367300 F95-RS01-NO-FN. EXIT.                                            P499
367400 F95AA-FN. EXIT.                                                  P499
367500 F95BB.         EXIT.                                             P000
367600 F95-RS42-FU.                                                     P100
367700     MOVE        'SELECT' TO XO00-XORATY                          P101
367800     MOVE        '95BB' TO XO00-XCDFSF                            P102
367900     MOVE        'RS42' TO XO00-XORATA                            P103
368000     MOVE        RS42 TO XO00-XORACL                              P104
368100     MOVE        ZERO TO XOBB-RS42-CF                             P105
368200     EXEC SQL                                                     P108
368300                 SELECT  ROWID,                                   P110
368400                         ZCLET                                    P111
368500                      ,  NIRET                                    P112
368600                      ,  ZTA100                                   P113
368700                   INTO :XP00-XROWID,                             P210
368800                        :RS42-ZCLET                               P211
368900                      , :RS42-NIRET                               P212
369000                      , :RS42-ZTA100                              P213
369100                   FROM  RS42                                     P300
369200                  WHERE                                           P310
369300                         ZCLET  =  :RS42-ZCLET                    P311
369400                    AND  NIRET  =  :RS42-NIRET                    P312
369500                    AND  ROWNUM = 1                    END-EXEC.  P410
369600     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
369700           IF    IK = ZERO                                        P485
369800     MOVE        '1' TO XOBB-RS42-CF                              P485
369900     ADD         1 TO XOBB-COUNT                                  P490
370000     MOVE        XP00-XROWID TO RS42-XROWID.                      P495
370100 F95-RS42-FU-FN. EXIT.                                            P499
370200 F95BB-FN. EXIT.                                                  P499
370300 F95CC.         EXIT.                                             P000
370400 F95-RS02-FA.                                                     P100
370500     MOVE        'SELECT' TO XO00-XORATY                          P101
370600     MOVE        '95CC' TO XO00-XCDFSF                            P102
370700     MOVE        'RS02' TO XO00-XORATA                            P103
370800     MOVE        RS02 TO XO00-XORACL                              P104
370900     MOVE        ZERO TO XOCC-RS02-CF                             P105
371000     EXEC SQL                                                     P108
371100                 SELECT  ROWID,                                   P110
371200                         NORER                                    P111
371300                      ,  nvl(CDRES ,' ')                          P112
371400                      ,  nvl(LNRENR,' ')                          P113
371500                      ,  nvl(LNREPR,' ')                          P114
371600                      ,  nvl(DTRENR,' ')                          P115
371700                      ,  nvl(LNRENF,' ')                          P116
371800                      ,  nvl(CDRECR,' ')                          P117
371900                      ,  nvl(CDREXR,' ')                          P118
372000                      ,  nvl(CTREF ,' ')                          P119
372100                      ,  nvl(CEREF ,' ')                          P120
372200                      ,  nvl(DBREF ,' ')                          P121
372300                      ,  nvl(DMREN ,' ')                          P122
372400                   INTO :XP00-XROWID,                             P210
372500                        :RS02-NORER                               P211
372600                      , :RS02-CDRES                               P212
372700                      , :RS02-LNRENR                              P213
372800                      , :RS02-LNREPR                              P214
372900                      , :RS02-DTRENR                              P215
373000                      , :RS02-LNRENF                              P216
373100                      , :RS02-CDRECR                              P217
373200                      , :RS02-CDREXR                              P218
373300                      , :RS02-CTREF                               P219
373400                      , :RS02-CEREF                               P220
373500                      , :RS02-DBREF                               P221
373600                      , :RS02-DMREN                               P222
373700                   FROM  RS02                                     P300
373800                  WHERE                                           P310
373900                         NORER  = :RS02-NORER          END-EXEC.  P311
374000     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
374100           IF    IK = ZERO                                        P485
374200     MOVE        '1' TO XOCC-RS02-CF                              P485
374300     ADD         1 TO XOCC-COUNT                                  P490
374400     MOVE        XP00-XROWID TO RS02-XROWID.                      P495
374500 F95-RS02-FA-FN. EXIT.                                            P499
374600 F95CC-FN. EXIT.                                                  P499
374700 F95DD.         EXIT.                                             P000
374800 F95-RS04-FV.                                                     P100
374900     MOVE        'SELECT' TO XO00-XORATY                          P101
375000     MOVE        '95DD' TO XO00-XCDFSF                            P102
375100     MOVE        'RS04' TO XO00-XORATA                            P103
375200     MOVE        RS04 TO XO00-XORACL                              P104
375300     MOVE        ZERO TO XODD-RS04-CF                             P105
375400     EXEC SQL                                                     P108
375500                 SELECT  ROWID,                                   P110
375600                         CNREN                                    P111
375700                      ,  NORER                                    P112
375800                      ,  IDRPL                                    P113
375900                      ,  nvl(CDRES ,' ')                          P114
376000                      ,  nvl(NOCRA ,' ')                          P115
376100                      ,  nvl(NOTIE ,' ')                          P116
376200                      ,  nvl(CDPAD ,' ')                          P117
376300                      ,  nvl(CDREA ,' ')                          P118
376400                      ,  nvl(DMREA ,' ')                          P119
376500                      ,  nvl(DFREN ,' ')                          P120
376600                      ,  nvl(NOSIN ,' ')                          P121
376700                      ,  nvl(DTREAC,' ')                          P122
376800                      ,  nvl(DTREC ,' ')                          P123
376900                      ,  nvl(CTRETC,' ')                          P124
377000                      ,  nvl(CDPAF ,' ')                          P125
377100                      ,  nvl(CTPAT ,' ')                          P126
377200                      ,  nvl(CQRER ,' ')                          P127
377300                      ,  nvl(CERERC,' ')                          P128
377400                      ,  nvl(NOREG ,' ')                          P129
377500                      ,  nvl(CDREF ,' ')                          P130
377600                      ,  nvl(CDREG ,' ')                          P131
377700                      ,  nvl(LIRE30,' ')                          P132
377800                      ,  nvl(DMRET ,' ')                          P133
377900                      ,  nvl(DDREN ,' ')                          P134
378000                      ,  nvl(MTREA , 0)                           P135
378100                      ,  nvl(MORER , 0)                           P136
378200                      ,  nvl(LIRER ,' ')                          P137
378300                      ,  nvl(MTPARV, 0)                           P138
378400                      ,  nvl(CDRELK,' ')                          P139
378500                      ,  nvl(MKREND, 0)                           P140
378600                      ,  nvl(LIREK ,' ')                          P141
378700                      ,  nvl(CSREE ,' ')                          P142
378800                      ,  nvl(MTRER , 0)                           P143
378900                      ,  nvl(DTPAID,' ')                          P144
379000                      ,  nvl(DHPAID,' ')                          P145
379100                      ,  nvl(DTPARP,' ')                          P146
379200                      ,  nvl(LIPAOD,' ')                          P147
379300                      ,  nvl(MCPAA , 0)                           P148
379400                      ,  nvl(MCPAP , 0)                           P149
379500                      ,  nvl(MKREP , 0)                           P150
379600                      ,  nvl(CDRER ,' ')                          P151
379700                      ,  nvl(NOPAMD, 0)                           P152
379800                      ,  nvl(NOPAID, 0)                           P153
379900                      ,  nvl(MKREC , 0)                           P154
380000                      ,  nvl(CEREK ,' ')                          P155
380100                      ,  nvl(DTPAIS,' ')                          P156
380200                      ,  nvl(NODEP ,' ')                          P157
380300                      ,  nvl(CDREV ,' ')                          P158
380400                      ,  nvl(LIPA2 ,' ')                          P159
380500                      ,  nvl(CDREK ,' ')                          P160
380600                      ,  nvl(DTREC1,' ')                          P161
380700                      ,  nvl(CTREU ,' ')                          P162
380800                      ,  nvl(DFREL ,' ')                          P163
380900                      ,  nvl(LIR30A,' ')                          P164
381000                      ,  nvl(CCGT1 ,' ')                          P165
381100                      ,  nvl(CDGT  ,' ')                          P166
381200                      ,  nvl(CRCGJ ,' ')                          P167
381300                      ,  nvl(CNCOB ,' ')                          P168
381400                      ,  nvl(CRCGE ,' ')                          P169
381500                      ,  nvl(CCREN ,' ')                          P170
381600                      ,  nvl(CRCGD ,' ')                          P171
381700                      ,  nvl(CCCON ,' ')                          P172
381800                      ,  nvl(CDSIN ,' ')                          P173
381900                      ,  nvl(CDEXCS,' ')                          P174
382000                      ,  nvl(NOSINL,' ')                          P175
382100                      ,  nvl(NOSINS,' ')                          P176
382200                      ,  nvl(CCFRN ,' ')                          P177
382300                      ,  nvl(CCFRA ,' ')                          P178
382400                      ,  nvl(MTREB , 0)                           P179
382500                      ,  nvl(LIFOE ,' ')                          P180
382600                      ,  nvl(NIREGS,' ')                          P181
382700                      ,  nvl(CDRES3,' ')                          P182
382800                      ,  nvl(NOCRA3,' ')                          P183
382900                      ,  nvl(CDCAT ,' ')                          P184
383000                      ,  nvl(CDRVA ,' ')                          P185
383100                      ,  nvl(CDFISC,' ')                          P186
383200                      ,  nvl(CDRESO,' ')                          P187
383300                      ,  nvl(CCRER ,' ')                          P188
383400                      ,  nvl(CTRTCA,' ')                          P189
383500                   INTO :XP00-XROWID,                             P210
383600                        :RS04-CNREN                               P211
383700                      , :RS04-NORER                               P212
383800                      , :RS04-IDRPL                               P213
383900                      , :RS04-CDRES                               P214
384000                      , :RS04-NOCRA                               P215
384100                      , :RS04-NOTIE                               P216
384200                      , :RS04-CDPAD                               P217
384300                      , :RS04-CDREA                               P218
384400                      , :RS04-DMREA                               P219
384500                      , :RS04-DFREN                               P220
384600                      , :RS04-NOSIN                               P221
384700                      , :RS04-DTREAC                              P222
384800                      , :RS04-DTREC                               P223
384900                      , :RS04-CTRETC                              P224
385000                      , :RS04-CDPAF                               P225
385100                      , :RS04-CTPAT                               P226
385200                      , :RS04-CQRER                               P227
385300                      , :RS04-CERERC                              P228
385400                      , :RS04-NOREG                               P229
385500                      , :RS04-CDREF                               P230
385600                      , :RS04-CDREG                               P231
385700                      , :RS04-LIRE30                              P232
385800                      , :RS04-DMRET                               P233
385900                      , :RS04-DDREN                               P234
386000                      , :RS04-MTREA                               P235
386100                      , :RS04-MORER                               P236
386200                      , :RS04-LIRER                               P237
386300                      , :RS04-MTPARV                              P238
386400                      , :RS04-CDRELK                              P239
386500                      , :RS04-MKREND                              P240
386600                      , :RS04-LIREK                               P241
386700                      , :RS04-CSREE                               P242
386800                      , :RS04-MTRER                               P243
386900                      , :RS04-DTPAID                              P244
387000                      , :RS04-DHPAID                              P245
387100                      , :RS04-DTPARP                              P246
387200                      , :RS04-LIPAOD                              P247
387300                      , :RS04-MCPAA                               P248
387400                      , :RS04-MCPAP                               P249
387500                      , :RS04-MKREP                               P250
387600                      , :RS04-CDRER                               P251
387700                      , :RS04-NOPAMD                              P252
387800                      , :RS04-NOPAID                              P253
387900                      , :RS04-MKREC                               P254
388000                      , :RS04-CEREK                               P255
388100                      , :RS04-DTPAIS                              P256
388200                      , :RS04-NODEP                               P257
388300                      , :RS04-CDREV                               P258
388400                      , :RS04-LIPA2                               P259
388500                      , :RS04-CDREK                               P260
388600                      , :RS04-DTREC1                              P261
388700                      , :RS04-CTREU                               P262
388800                      , :RS04-DFREL                               P263
388900                      , :RS04-LIR30A                              P264
389000                      , :RS04-CCGT1                               P265
389100                      , :RS04-CDGT                                P266
389200                      , :RS04-CRCGJ                               P267
389300                      , :RS04-CNCOB                               P268
389400                      , :RS04-CRCGE                               P269
389500                      , :RS04-CCREN                               P270
389600                      , :RS04-CRCGD                               P271
389700                      , :RS04-CCCON                               P272
389800                      , :RS04-CDSIN                               P273
389900                      , :RS04-CDEXCS                              P274
390000                      , :RS04-NOSINL                              P275
390100                      , :RS04-NOSINS                              P276
390200                      , :RS04-CCFRN                               P277
390300                      , :RS04-CCFRA                               P278
390400                      , :RS04-MTREB                               P279
390500                      , :RS04-LIFOE                               P280
390600                      , :RS04-NIREGS                              P281
390700                      , :RS04-CDRES3                              P282
390800                      , :RS04-NOCRA3                              P283
390900                      , :RS04-CDCAT                               P284
391000                      , :RS04-CDRVA                               P285
391100                      , :RS04-CDFISC                              P286
391200                      , :RS04-CDRESO                              P287
391300                      , :RS04-CCRER                               P288
391400                      , :RS04-CTRTCA                              P289
391500                   FROM  RS04                                     P300
391600                  WHERE                                           P310
391700                         NORER  = :C-0204-NORER                   P311
391800                    AND                                           P400
391900                         CNREN                                    P401
392000                     ||  IDRPL                                    P402
392100                      = ( SELECT MIN (                            P420
392200                         CNREN                                    P421
392300                     ||  IDRPL                                    P422
392400                                     )                            P440
392500                   FROM  RS04                                     P442
392600                  WHERE                                           P444
392700                         NORER  = :C-0204-NORER                   P445
392800                    AND (CNREN  > :C-0204-CNREN                   P446
392900                     OR (CNREN  = :C-0204-CNREN                   P447
393000                    AND (IDRPL  > :C-0204-IDRPL                   P448
393100                        )))                                       P449
393200                        )                              END-EXEC.  P475
393300     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
393400           IF    IK = ZERO                                        P485
393500     MOVE        '1' TO XODD-RS04-CF                              P485
393600     ADD         1 TO XODD-COUNT                                  P490
393700     MOVE        XP00-XROWID TO RS04-XROWID.                      P495
393800 F95-RS04-FV-FN. EXIT.                                            P499
393900 F95DD-FN. EXIT.                                                  P499
394000 F95EE.         EXIT.                                             P000
394100 F95-RS05-FV.                                                     P100
394200     MOVE        'SELECT' TO XO00-XORATY                          P101
394300     MOVE        '95EE' TO XO00-XCDFSF                            P102
394400     MOVE        'RS05' TO XO00-XORATA                            P103
394500     MOVE        RS05 TO XO00-XORACL                              P104
394600     MOVE        ZERO TO XOEE-RS05-CF                             P105
394700     EXEC SQL                                                     P108
394800                 SELECT  ROWID,                                   P110
394900                         CNREN                                    P111
395000                      ,  NORER                                    P112
395100                      ,  IDRPL                                    P113
395200                      ,  NOPAM                                    P114
395300                      ,  nvl(DMRENB,' ')                          P115
395400                      ,  nvl(NCRENR,' ')                          P116
395500                      ,  nvl(NCREER,' ')                          P117
395600                      ,  nvl(NCREGR,' ')                          P118
395700                      ,  nvl(NCREBR,' ')                          P119
395800                      ,  nvl(NCRECR,' ')                          P120
395900                      ,  nvl(CDPAMR,' ')                          P121
396000                   INTO :XP00-XROWID,                             P210
396100                        :RS05-CNREN                               P211
396200                      , :RS05-NORER                               P212
396300                      , :RS05-IDRPL                               P213
396400                      , :RS05-NOPAM                               P214
396500                      , :RS05-DMRENB                              P215
396600                      , :RS05-NCRENR                              P216
396700                      , :RS05-NCREER                              P217
396800                      , :RS05-NCREGR                              P218
396900                      , :RS05-NCREBR                              P219
397000                      , :RS05-NCRECR                              P220
397100                      , :RS05-CDPAMR                              P221
397200                   FROM  RS05                                     P300
397300                  WHERE                                           P310
397400                         CNREN  = :C-0405-CNREN                   P311
397500                    AND  NORER  = :C-0405-NORER                   P312
397600                    AND  IDRPL  = :C-0405-IDRPL                   P313
397700                    AND                                           P400
397800                         NOPAM                                    P401
397900                      = ( SELECT MIN (                            P420
398000                         NOPAM                                    P421
398100                                     )                            P440
398200                   FROM  RS05                                     P442
398300                  WHERE                                           P444
398400                         CNREN  = :C-0405-CNREN                   P445
398500                    AND  NORER  = :C-0405-NORER                   P446
398600                    AND  IDRPL  = :C-0405-IDRPL                   P447
398700                    AND (NOPAM  > :C-0405-NOPAM                   P448
398800                        )                                         P449
398900                        )                              END-EXEC.  P475
399000     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
399100           IF    IK = ZERO                                        P485
399200     MOVE        '1' TO XOEE-RS05-CF                              P485
399300     ADD         1 TO XOEE-COUNT                                  P490
399400     MOVE        XP00-XROWID TO RS05-XROWID.                      P495
399500 F95-RS05-FV-FN. EXIT.                                            P499
399600 F95EE-FN. EXIT.                                                  P499
399700 F95FF.                                                           P000
399800     EXEC SQL                                                     P100
399900                 DECLARE C_FF_RS06 CURSOR FOR                     P105
400000                 SELECT  ROWID,                                   P110
400100                         CNREN                                    P111
400200                      ,  NORER                                    P112
400300                      ,  IDRPL                                    P113
400400                      ,  XCSEQ                                    P114
400500                      ,  nvl(DMREN ,' ')                          P115
400600                      ,  DDREN                                    P116
400700                      ,  nvl(MTREA , 0)                           P117
400800                      ,  nvl(DDREJ ,' ')                          P118
400900                      ,  nvl(DTREC ,' ')                          P119
401000                      ,  nvl(TXREI , 0)                           P120
401100                      ,  nvl(MTREAO, 0)                           P121
401200                      ,  nvl(BTREAT, 0)                           P122
401300                      ,  nvl(TXREAS, 0)                           P123
401400                   FROM  RS06                                     P300
401500                  WHERE                                           P310
401600                         CNREN  = :C-0406-CNREN                   P311
401700                    AND  NORER  = :C-0406-NORER                   P312
401800                    AND  IDRPL  = :C-0406-IDRPL                   P313
401900                  ORDER  BY                                       P410
402000                         DDREN  DESC                              P411
402100                      ,  XCSEQ                         END-EXEC.  P412
402200 F95-RS06-FV.                                                     P500
402300           IF    XOFF-RS06-OPE = ZERO                             P502
402400     MOVE        'OPEN' TO XO00-XORATY                            P502
402500     MOVE        '95FF' TO XO00-XCDFSF                            P503
402600     MOVE        'RS06' TO XO00-XORATA                            P504
402700     MOVE        RS06 TO XO00-XORACL                              P505
402800     EXEC SQL    OPEN    C_FF_RS06                     END-EXEC.  P510
402900     MOVE        '1' TO XOFF-RS06-OPE.                            P520
403000     MOVE        'FETCH' TO XO00-XORATY                           P601
403100     MOVE        '95FF' TO XO00-XCDFSF                            P602
403200     MOVE        'RS06' TO XO00-XORATA                            P603
403300     MOVE        RS06 TO XO00-XORACL                              P604
403400     MOVE        ZERO TO XOFF-RS06-CF                             P605
403500     EXEC SQL    FETCH   C_FF_RS06                                P610
403600                   INTO :XP00-XROWID,                             P620
403700                        :RS06-CNREN                               P621
403800                      , :RS06-NORER                               P622
403900                      , :RS06-IDRPL                               P623
404000                      , :RS06-XCSEQ                               P624
404100                      , :RS06-DMREN                               P625
404200                      , :RS06-DDREN                               P626
404300                      , :RS06-MTREA                               P627
404400                      , :RS06-DDREJ                               P628
404500                      , :RS06-DTREC                               P629
404600                      , :RS06-TXREI                               P630
404700                      , :RS06-MTREAO                              P631
404800                      , :RS06-BTREAT                              P632
404900                      , :RS06-TXREAS                   END-EXEC.  P633
405000     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P850
405100           IF    IK = ZERO                                        P860
405200     MOVE        '1' TO XOFF-RS06-CF                              P860
405300     ADD         1 TO XOFF-COUNT                                  P870
405400     MOVE        XP00-XROWID TO RS06-XROWID                       P880
405500           ELSE                                                   P885
405600     PERFORM     F95-RS06-FV-CL THRU F95-RS06-FV-CL-FN.           P885
405700 F95-RS06-FV-FN. EXIT.                                            P895
405800 F95-RS06-FV-CL.                                                  P900
405900           IF    XOFF-RS06-OPE = ZERO                             P901
406000     GO TO F95-RS06-FV-CL-FN.                                     P901
406100     MOVE        'CLOSE FF' TO XO00-XORATY                        P902
406200     MOVE        '95FF' TO XO00-XCDFSF                            P903
406300     MOVE        'RS06' TO XO00-XORATA                            P904
406400     MOVE        RS06 TO XO00-XORACL                              P905
406500     EXEC SQL    CLOSE   C_FF_RS06                     END-EXEC.  P910
406600     MOVE        ZERO TO XOFF-RS06-OPE.                           P920
406700 F95-RS06-FV-CL-FN. EXIT.                                         P930
406800 F95FF-FN. EXIT.                                                  P930
406900 F95GG.         EXIT.                                             P000
407000 F95-RS07-FV.                                                     P100
407100     MOVE        'SELECT' TO XO00-XORATY                          P101
407200     MOVE        '95GG' TO XO00-XCDFSF                            P102
407300     MOVE        'RS07' TO XO00-XORATA                            P103
407400     MOVE        RS07 TO XO00-XORACL                              P104
407500     MOVE        ZERO TO XOGG-RS07-CF                             P105
407600     EXEC SQL                                                     P108
407700                 SELECT  ROWID,                                   P110
407800                         CNREN                                    P111
407900                      ,  NORER                                    P112
408000                      ,  IDRPL                                    P113
408100                      ,  XCSEQ                                    P114
408200                      ,  DMREN                                    P115
408300                      ,  nvl(MKREN , 0)                           P116
408400                      ,  nvl(LIREK ,' ')                          P117
408500                      ,  nvl(CSREE ,' ')                          P118
408600                      ,  nvl(MOREC , 0)                           P119
408700                      ,  nvl(TXCAD , 0)                           P120
408800                      ,  nvl(CDTIPS,' ')                          P121
408900                      ,  nvl(CDREGR,' ')                          P122
409000                   INTO :XP00-XROWID,                             P210
409100                        :RS07-CNREN                               P211
409200                      , :RS07-NORER                               P212
409300                      , :RS07-IDRPL                               P213
409400                      , :RS07-XCSEQ                               P214
409500                      , :RS07-DMREN                               P215
409600                      , :RS07-MKREN                               P216
409700                      , :RS07-LIREK                               P217
409800                      , :RS07-CSREE                               P218
409900                      , :RS07-MOREC                               P219
410000                      , :RS07-TXCAD                               P220
410100                      , :RS07-CDTIPS                              P221
410200                      , :RS07-CDREGR                              P222
410300                   FROM  RS07                                     P300
410400                  WHERE                                           P310
410500                         CNREN  = :C-0407-CNREN                   P311
410600                    AND  NORER  = :C-0407-NORER                   P312
410700                    AND  IDRPL  = :C-0407-IDRPL                   P313
410800                    AND                                           P400
410900                         XCSEQ                                    P401
411000                      = ( SELECT MIN (                            P420
411100                         XCSEQ                                    P421
411200                                     )                            P440
411300                   FROM  RS07                                     P442
411400                  WHERE                                           P444
411500                         CNREN  = :C-0407-CNREN                   P445
411600                    AND  NORER  = :C-0407-NORER                   P446
411700                    AND  IDRPL  = :C-0407-IDRPL                   P447
411800                    AND (XCSEQ  > :C-0407-XCSEQ                   P448
411900                        )                                         P449
412000                        )                              END-EXEC.  P475
412100     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
412200           IF    IK = ZERO                                        P485
412300     MOVE        '1' TO XOGG-RS07-CF                              P485
412400     ADD         1 TO XOGG-COUNT                                  P490
412500     MOVE        XP00-XROWID TO RS07-XROWID.                      P495
412600 F95-RS07-FV-FN. EXIT.                                            P499
412700 F95GG-FN. EXIT.                                                  P499
412800 F95H1.         EXIT.                                             P000
412900 F95-RS08-FV.                                                     P100
413000     MOVE        'SELECT' TO XO00-XORATY                          P101
413100     MOVE        '95H1' TO XO00-XCDFSF                            P102
413200     MOVE        'RS08' TO XO00-XORATA                            P103
413300     MOVE        RS08 TO XO00-XORACL                              P104
413400     MOVE        ZERO TO XOH1-RS08-CF                             P105
413500     EXEC SQL                                                     P108
413600                 SELECT  ROWID,                                   P110
413700                         CNREN                                    P111
413800                      ,  NORER                                    P112
413900                      ,  IDRPL                                    P113
414000                      ,  nvl(DDREJ ,' ')                          P114
414100                      ,  nvl(CDREJ ,' ')                          P115
414200                      ,  nvl(CTRER ,' ')                          P116
414300                      ,  nvl(CTREA ,' ')                          P117
414400                      ,  nvl(TXREI , 0)                           P118
414500                      ,  nvl(MTRES , 0)                           P119
414600                      ,  nvl(MOREC , 0)                           P120
414700                      ,  nvl(CERETP,' ')                          P121
414800                   INTO :XP00-XROWID,                             P210
414900                        :RS08-CNREN                               P211
415000                      , :RS08-NORER                               P212
415100                      , :RS08-IDRPL                               P213
415200                      , :RS08-DDREJ                               P214
415300                      , :RS08-CDREJ                               P215
415400                      , :RS08-CTRER                               P216
415500                      , :RS08-CTREA                               P217
415600                      , :RS08-TXREI                               P218
415700                      , :RS08-MTRES                               P219
415800                      , :RS08-MOREC                               P220
415900                      , :RS08-CERETP                              P221
416000                   FROM  RS08                                     P300
416100                  WHERE                                           P310
416200                         CNREN  = :C-04XX-CNREN                   P311
416300                    AND  NORER  = :C-04XX-NORER                   P312
416400                    AND  IDRPL  = :C-04XX-IDRPL        END-EXEC.  P313
416500     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
416600           IF    IK = ZERO                                        P485
416700     MOVE        '1' TO XOH1-RS08-CF                              P485
416800     ADD         1 TO XOH1-COUNT                                  P490
416900     MOVE        XP00-XROWID TO RS08-XROWID.                      P495
417000 F95-RS08-FV-FN. EXIT.                                            P499
417100 F95H1-FN. EXIT.                                                  P499
417200 F95H2.         EXIT.                                             P000
417300 F95-RS09-FV.                                                     P100
417400     MOVE        'SELECT' TO XO00-XORATY                          P101
417500     MOVE        '95H2' TO XO00-XCDFSF                            P102
417600     MOVE        'RS09' TO XO00-XORATA                            P103
417700     MOVE        RS09 TO XO00-XORACL                              P104
417800     MOVE        ZERO TO XOH2-RS09-CF                             P105
417900     EXEC SQL                                                     P108
418000                 SELECT  ROWID,                                   P110
418100                         CNREN                                    P111
418200                      ,  NORER                                    P112
418300                      ,  IDRPL                                    P113
418400                      ,  nvl(TXREO , 0)                           P114
418500                      ,  nvl(CEREO ,' ')                          P115
418600                      ,  nvl(MOREC , 0)                           P116
418700                      ,  nvl(LNRENO,' ')                          P117
418800                      ,  nvl(LNREPO,' ')                          P118
418900                      ,  nvl(CDREXO,' ')                          P119
419000                      ,  nvl(DTRENO,' ')                          P120
419100                      ,  nvl(NORENR,' ')                          P121
419200                   INTO :XP00-XROWID,                             P210
419300                        :RS09-CNREN                               P211
419400                      , :RS09-NORER                               P212
419500                      , :RS09-IDRPL                               P213
419600                      , :RS09-TXREO                               P214
419700                      , :RS09-CEREO                               P215
419800                      , :RS09-MOREC                               P216
419900                      , :RS09-LNRENO                              P217
420000                      , :RS09-LNREPO                              P218
420100                      , :RS09-CDREXO                              P219
420200                      , :RS09-DTRENO                              P220
420300                      , :RS09-NORENR                              P221
420400                   FROM  RS09                                     P300
420500                  WHERE                                           P310
420600                         CNREN  = :C-04XX-CNREN                   P311
420700                    AND  NORER  = :C-04XX-NORER                   P312
420800                    AND  IDRPL  = :C-04XX-IDRPL        END-EXEC.  P313
420900     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
421000           IF    IK = ZERO                                        P485
421100     MOVE        '1' TO XOH2-RS09-CF                              P485
421200     ADD         1 TO XOH2-COUNT                                  P490
421300     MOVE        XP00-XROWID TO RS09-XROWID.                      P495
421400 F95-RS09-FV-FN. EXIT.                                            P499
421500 F95H2-FN. EXIT.                                                  P499
421600 F95H3.         EXIT.                                             P000
421700 F95-RS10-FV.                                                     P100
421800     MOVE        'SELECT' TO XO00-XORATY                          P101
421900     MOVE        '95H3' TO XO00-XCDFSF                            P102
422000     MOVE        'RS10' TO XO00-XORATA                            P103
422100     MOVE        RS10 TO XO00-XORACL                              P104
422200     MOVE        ZERO TO XOH3-RS10-CF                             P105
422300     EXEC SQL                                                     P108
422400                 SELECT  ROWID,                                   P110
422500                         CNREN                                    P111
422600                      ,  NORER                                    P112
422700                      ,  IDRPL                                    P113
422800                      ,  nvl(CTRER ,' ')                          P114
422900                      ,  nvl(CTREA ,' ')                          P115
423000                      ,  nvl(MOREC , 0)                           P116
423100                      ,  nvl(CTREEX,' ')                          P117
423200                      ,  nvl(DTREO ,' ')                          P118
423300                      ,  nvl(MTPAC , 0)                           P119
423400                   INTO :XP00-XROWID,                             P210
423500                        :RS10-CNREN                               P211
423600                      , :RS10-NORER                               P212
423700                      , :RS10-IDRPL                               P213
423800                      , :RS10-CTRER                               P214
423900                      , :RS10-CTREA                               P215
424000                      , :RS10-MOREC                               P216
424100                      , :RS10-CTREEX                              P217
424200                      , :RS10-DTREO                               P218
424300                      , :RS10-MTPAC                               P219
424400                   FROM  RS10                                     P300
424500                  WHERE                                           P310
424600                         CNREN  = :C-04XX-CNREN                   P311
424700                    AND  NORER  = :C-04XX-NORER                   P312
424800                    AND  IDRPL  = :C-04XX-IDRPL        END-EXEC.  P313
424900     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
425000           IF    IK = ZERO                                        P485
425100     MOVE        '1' TO XOH3-RS10-CF                              P485
425200     ADD         1 TO XOH3-COUNT                                  P490
425300     MOVE        XP00-XROWID TO RS10-XROWID.                      P495
425400 F95-RS10-FV-FN. EXIT.                                            P499
425500 F95H3-FN. EXIT.                                                  P499
425600 F95H4.         EXIT.                                             P000
425700 F95-RS11-FV.                                                     P100
425800     MOVE        'SELECT' TO XO00-XORATY                          P101
425900     MOVE        '95H4' TO XO00-XCDFSF                            P102
426000     MOVE        'RS11' TO XO00-XORATA                            P103
426100     MOVE        RS11 TO XO00-XORACL                              P104
426200     MOVE        ZERO TO XOH4-RS11-CF                             P105
426300     EXEC SQL                                                     P108
426400                 SELECT  ROWID,                                   P110
426500                         CNREN                                    P111
426600                      ,  NORER                                    P112
426700                      ,  IDRPL                                    P113
426800                      ,  nvl(CTRER ,' ')                          P114
426900                      ,  nvl(CTREA ,' ')                          P115
427000                      ,  nvl(LIPOT ,' ')                          P116
427100                      ,  nvl(LIPOTB,' ')                          P117
427200                      ,  nvl(TXPOO , 0)                           P118
427300                      ,  nvl(MTPOO , 0)                           P119
427400                      ,  nvl(MTRECR, 0)                           P120
427500                      ,  nvl(CDTIPS,' ')                          P121
427600                      ,  nvl(TXCAD , 0)                           P122
427700                      ,  nvl(CDREGR,' ')                          P123
427800                   INTO :XP00-XROWID,                             P210
427900                        :RS11-CNREN                               P211
428000                      , :RS11-NORER                               P212
428100                      , :RS11-IDRPL                               P213
428200                      , :RS11-CTRER                               P214
428300                      , :RS11-CTREA                               P215
428400                      , :RS11-LIPOT                               P216
428500                      , :RS11-LIPOTB                              P217
428600                      , :RS11-TXPOO                               P218
428700                      , :RS11-MTPOO                               P219
428800                      , :RS11-MTRECR                              P220
428900                      , :RS11-CDTIPS                              P221
429000                      , :RS11-TXCAD                               P222
429100                      , :RS11-CDREGR                              P223
429200                   FROM  RS11                                     P300
429300                  WHERE                                           P310
429400                         CNREN  = :C-04XX-CNREN                   P311
429500                    AND  NORER  = :C-04XX-NORER                   P312
429600                    AND  IDRPL  = :C-04XX-IDRPL        END-EXEC.  P313
429700     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
429800           IF    IK = ZERO                                        P485
429900     MOVE        '1' TO XOH4-RS11-CF                              P485
430000     ADD         1 TO XOH4-COUNT                                  P490
430100     MOVE        XP00-XROWID TO RS11-XROWID.                      P495
430200 F95-RS11-FV-FN. EXIT.                                            P499
430300 F95H4-FN. EXIT.                                                  P499
430400 F95H5.         EXIT.                                             P000
430500 F95-RS12-FV.                                                     P100
430600     MOVE        'SELECT' TO XO00-XORATY                          P101
430700     MOVE        '95H5' TO XO00-XCDFSF                            P102
430800     MOVE        'RS12' TO XO00-XORATA                            P103
430900     MOVE        RS12 TO XO00-XORACL                              P104
431000     MOVE        ZERO TO XOH5-RS12-CF                             P105
431100     EXEC SQL                                                     P108
431200                 SELECT  ROWID,                                   P110
431300                         CNREN                                    P111
431400                      ,  NORER                                    P112
431500                      ,  IDRPL                                    P113
431600                      ,  nvl(DDREJ ,' ')                          P114
431700                      ,  nvl(CDREJ ,' ')                          P115
431800                      ,  nvl(MOREC , 0)                           P116
431900                      ,  nvl(DTREO ,' ')                          P117
432000                      ,  nvl(MTPAC , 0)                           P118
432100                      ,  nvl(LNRENS,' ')                          P119
432200                      ,  nvl(NORES ,' ')                          P120
432300                      ,  nvl(LNRENA,' ')                          P121
432400                      ,  nvl(CTRESS,' ')                          P122
432500                      ,  nvl(TXREI , 0)                           P123
432600                      ,  nvl(CNREK ,' ')                          P124
432700                      ,  nvl(MKREL , 0)                           P125
432800                      ,  nvl(CCREM ,' ')                          P126
432900                      ,  nvl(CDREB ,' ')                          P127
433000                      ,  nvl(CDREPS,' ')                          P128
433100                      ,  nvl(CNREOS,' ')                          P129
433200                      ,  nvl(CTREA ,' ')                          P130
433300                   INTO :XP00-XROWID,                             P210
433400                        :RS12-CNREN                               P211
433500                      , :RS12-NORER                               P212
433600                      , :RS12-IDRPL                               P213
433700                      , :RS12-DDREJ                               P214
433800                      , :RS12-CDREJ                               P215
433900                      , :RS12-MOREC                               P216
434000                      , :RS12-DTREO                               P217
434100                      , :RS12-MTPAC                               P218
434200                      , :RS12-LNRENS                              P219
434300                      , :RS12-NORES                               P220
434400                      , :RS12-LNRENA                              P221
434500                      , :RS12-CTRESS                              P222
434600                      , :RS12-TXREI                               P223
434700                      , :RS12-CNREK                               P224
434800                      , :RS12-MKREL                               P225
434900                      , :RS12-CCREM                               P226
435000                      , :RS12-CDREB                               P227
435100                      , :RS12-CDREPS                              P228
435200                      , :RS12-CNREOS                              P229
435300                      , :RS12-CTREA                               P230
435400                   FROM  RS12                                     P300
435500                  WHERE                                           P310
435600                         CNREN  = :C-04XX-CNREN                   P311
435700                    AND  NORER  = :C-04XX-NORER                   P312
435800                    AND  IDRPL  = :C-04XX-IDRPL        END-EXEC.  P313
435900     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
436000           IF    IK = ZERO                                        P485
436100     MOVE        '1' TO XOH5-RS12-CF                              P485
436200     ADD         1 TO XOH5-COUNT                                  P490
436300     MOVE        XP00-XROWID TO RS12-XROWID.                      P495
436400 F95-RS12-FV-FN. EXIT.                                            P499
436500 F95H5-FN. EXIT.                                                  P499
436600 F95H6.         EXIT.                                             P000
436700 F95-RS13-FV.                                                     P100
436800     MOVE        'SELECT' TO XO00-XORATY                          P101
436900     MOVE        '95H6' TO XO00-XCDFSF                            P102
437000     MOVE        'RS13' TO XO00-XORATA                            P103
437100     MOVE        RS13 TO XO00-XORACL                              P104
437200     MOVE        ZERO TO XOH6-RS13-CF                             P105
437300     EXEC SQL                                                     P108
437400                 SELECT  ROWID,                                   P110
437500                         CNREN                                    P111
437600                      ,  NORER                                    P112
437700                      ,  IDRPL                                    P113
437800                      ,  nvl(DDREJ ,' ')                          P114
437900                      ,  nvl(CDREJ ,' ')                          P115
438000                      ,  nvl(MOREC , 0)                           P116
438100                      ,  nvl(DTREO ,' ')                          P117
438200                      ,  nvl(MTPAC , 0)                           P118
438300                      ,  nvl(LNRENS,' ')                          P119
438400                      ,  nvl(NORES ,' ')                          P120
438500                      ,  nvl(LNRENA,' ')                          P121
438600                      ,  nvl(CTREDC,' ')                          P122
438700                      ,  nvl(CTREJ ,' ')                          P123
438800                      ,  nvl(TXREI , 0)                           P124
438900                      ,  nvl(MTREAO, 0)                           P125
439000                      ,  nvl(KMREDC, 0)                           P126
439100                      ,  nvl(CNREK ,' ')                          P127
439200                      ,  nvl(MKREL , 0)                           P128
439300                      ,  nvl(CCREM ,' ')                          P129
439400                      ,  nvl(CDREB ,' ')                          P130
439500                      ,  nvl(CDREAT,' ')                          P131
439600                      ,  nvl(DTRENV,' ')                          P132
439700                      ,  nvl(CDREX ,' ')                          P133
439800                   INTO :XP00-XROWID,                             P210
439900                        :RS13-CNREN                               P211
440000                      , :RS13-NORER                               P212
440100                      , :RS13-IDRPL                               P213
440200                      , :RS13-DDREJ                               P214
440300                      , :RS13-CDREJ                               P215
440400                      , :RS13-MOREC                               P216
440500                      , :RS13-DTREO                               P217
440600                      , :RS13-MTPAC                               P218
440700                      , :RS13-LNRENS                              P219
440800                      , :RS13-NORES                               P220
440900                      , :RS13-LNRENA                              P221
441000                      , :RS13-CTREDC                              P222
441100                      , :RS13-CTREJ                               P223
441200                      , :RS13-TXREI                               P224
441300                      , :RS13-MTREAO                              P225
441400                      , :RS13-KMREDC                              P226
441500                      , :RS13-CNREK                               P227
441600                      , :RS13-MKREL                               P228
441700                      , :RS13-CCREM                               P229
441800                      , :RS13-CDREB                               P230
441900                      , :RS13-CDREAT                              P231
442000                      , :RS13-DTRENV                              P232
442100                      , :RS13-CDREX                               P233
442200                   FROM  RS13                                     P300
442300                  WHERE                                           P310
442400                         CNREN  = :C-04XX-CNREN                   P311
442500                    AND  NORER  = :C-04XX-NORER                   P312
442600                    AND  IDRPL  = :C-04XX-IDRPL        END-EXEC.  P313
442700     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
442800           IF    IK = ZERO                                        P485
442900     MOVE        '1' TO XOH6-RS13-CF                              P485
443000     ADD         1 TO XOH6-COUNT                                  P490
443100     MOVE        XP00-XROWID TO RS13-XROWID.                      P495
443200 F95-RS13-FV-FN. EXIT.                                            P499
443300 F95H6-FN. EXIT.                                                  P499
443400 F95H7.         EXIT.                                             P000
443500 F95-RS14-FV.                                                     P100
443600     MOVE        'SELECT' TO XO00-XORATY                          P101
443700     MOVE        '95H7' TO XO00-XCDFSF                            P102
443800     MOVE        'RS14' TO XO00-XORATA                            P103
443900     MOVE        RS14 TO XO00-XORACL                              P104
444000     MOVE        ZERO TO XOH7-RS14-CF                             P105
444100     EXEC SQL                                                     P108
444200                 SELECT  ROWID,                                   P110
444300                         CNREN                                    P111
444400                      ,  NORER                                    P112
444500                      ,  IDRPL                                    P113
444600                      ,  nvl(NOCRM ,' ')                          P114
444700                      ,  nvl(NOREMS,' ')                          P115
444800                      ,  nvl(CDRETX,' ')                          P116
444900                      ,  nvl(TXREAS, 0)                           P117
445000                      ,  nvl(NORENR,' ')                          P118
445100                      ,  nvl(BTREAT, 0)                           P119
445200                   INTO :XP00-XROWID,                             P210
445300                        :RS14-CNREN                               P211
445400                      , :RS14-NORER                               P212
445500                      , :RS14-IDRPL                               P213
445600                      , :RS14-NOCRM                               P214
445700                      , :RS14-NOREMS                              P215
445800                      , :RS14-CDRETX                              P216
445900                      , :RS14-TXREAS                              P217
446000                      , :RS14-NORENR                              P218
446100                      , :RS14-BTREAT                              P219
446200                   FROM  RS14                                     P300
446300                  WHERE                                           P310
446400                         CNREN  = :C-04XX-CNREN                   P311
446500                    AND  NORER  = :C-04XX-NORER                   P312
446600                    AND  IDRPL  = :C-04XX-IDRPL        END-EXEC.  P313
446700     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
446800           IF    IK = ZERO                                        P485
446900     MOVE        '1' TO XOH7-RS14-CF                              P485
447000     ADD         1 TO XOH7-COUNT                                  P490
447100     MOVE        XP00-XROWID TO RS14-XROWID.                      P495
447200 F95-RS14-FV-FN. EXIT.                                            P499
447300 F95H7-FN. EXIT.                                                  P499
447400 F95H8.         EXIT.                                             P000
447500 F95-RS15-FV.                                                     P100
447600     MOVE        'SELECT' TO XO00-XORATY                          P101
447700     MOVE        '95H8' TO XO00-XCDFSF                            P102
447800     MOVE        'RS15' TO XO00-XORATA                            P103
447900     MOVE        RS15 TO XO00-XORACL                              P104
448000     MOVE        ZERO TO XOH8-RS15-CF                             P105
448100     EXEC SQL                                                     P108
448200                 SELECT  ROWID,                                   P110
448300                         CNREN                                    P111
448400                      ,  NORER                                    P112
448500                      ,  IDRPL                                    P113
448600                      ,  nvl(PEREA , 0)                           P114
448700                      ,  nvl(MTRECR, 0)                           P115
448800                      ,  nvl(NORENR,' ')                          P116
448900                   INTO :XP00-XROWID,                             P210
449000                        :RS15-CNREN                               P211
449100                      , :RS15-NORER                               P212
449200                      , :RS15-IDRPL                               P213
449300                      , :RS15-PEREA                               P214
449400                      , :RS15-MTRECR                              P215
449500                      , :RS15-NORENR                              P216
449600                   FROM  RS15                                     P300
449700                  WHERE                                           P310
449800                         CNREN  = :C-04XX-CNREN                   P311
449900                    AND  NORER  = :C-04XX-NORER                   P312
450000                    AND  IDRPL  = :C-04XX-IDRPL        END-EXEC.  P313
450100     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
450200           IF    IK = ZERO                                        P485
450300     MOVE        '1' TO XOH8-RS15-CF                              P485
450400     ADD         1 TO XOH8-COUNT                                  P490
450500     MOVE        XP00-XROWID TO RS15-XROWID.                      P495
450600 F95-RS15-FV-FN. EXIT.                                            P499
450700 F95H8-FN. EXIT.                                                  P499
450800 F95H9.         EXIT.                                             P000
450900 F95-RS20-FV.                                                     P100
451000     MOVE        'SELECT' TO XO00-XORATY                          P101
451100     MOVE        '95H9' TO XO00-XCDFSF                            P102
451200     MOVE        'RS20' TO XO00-XORATA                            P103
451300     MOVE        RS20 TO XO00-XORACL                              P104
451400     MOVE        ZERO TO XOH9-RS20-CF                             P105
451500     EXEC SQL                                                     P108
451600                 SELECT  ROWID,                                   P110
451700                         CNREN                                    P111
451800                      ,  NORER                                    P112
451900                      ,  IDRPL                                    P113
452000                      ,  nvl(CTREA ,' ')                          P114
452100                      ,  nvl(TXREI , 0)                           P115
452200                      ,  nvl(DTREO ,' ')                          P116
452300                      ,  nvl(MTREAO, 0)                           P117
452400                      ,  nvl(MTREAT, 0)                           P118
452500                      ,  nvl(MOREC , 0)                           P119
452600                   INTO :XP00-XROWID,                             P210
452700                        :RS20-CNREN                               P211
452800                      , :RS20-NORER                               P212
452900                      , :RS20-IDRPL                               P213
453000                      , :RS20-CTREA                               P214
453100                      , :RS20-TXREI                               P215
453200                      , :RS20-DTREO                               P216
453300                      , :RS20-MTREAO                              P217
453400                      , :RS20-MTREAT                              P218
453500                      , :RS20-MOREC                               P219
453600                   FROM  RS20                                     P300
453700                  WHERE                                           P310
453800                         CNREN  = :C-04XX-CNREN                   P311
453900                    AND  NORER  = :C-04XX-NORER                   P312
454000                    AND  IDRPL  = :C-04XX-IDRPL        END-EXEC.  P313
454100     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
454200           IF    IK = ZERO                                        P485
454300     MOVE        '1' TO XOH9-RS20-CF                              P485
454400     ADD         1 TO XOH9-COUNT                                  P490
454500     MOVE        XP00-XROWID TO RS20-XROWID.                      P495
454600 F95-RS20-FV-FN. EXIT.                                            P499
454700 F95H9-FN. EXIT.                                                  P499
454800 F95I2.                                                           P000
454900           IF    XO00-XORATA = 'RS02'                             P005
455000     MOVE        D-0204 TO C-0204                                 P010
455100     MOVE        RS02-NORER TO C-0204-NORER.                      P015
455200           IF    XO00-XORATA = 'RS04'                             P020
455300     MOVE        D-0204 TO C-0204                                 P025
455400     MOVE        RS04-NORER TO C-0204-NORER                       P030
455500     MOVE        RS04-CNREN TO C-0204-CNREN                       P035
455600     MOVE        RS04-IDRPL TO C-0204-IDRPL.                      P040
455700           IF    XO00-XORATA = 'RS04'                             P045
455800     MOVE        D-0405 TO C-0405                                 P050
455900     MOVE        RS04-CNREN TO C-0405-CNREN                       P055
456000     MOVE        RS04-NORER TO C-0405-NORER                       P060
456100     MOVE        RS04-IDRPL TO C-0405-IDRPL.                      P065
456200           IF    XO00-XORATA = 'RS05'                             P070
456300     MOVE        D-0405 TO C-0405                                 P075
456400     MOVE        RS05-CNREN TO C-0405-CNREN                       P080
456500     MOVE        RS05-NORER TO C-0405-NORER                       P085
456600     MOVE        RS05-IDRPL TO C-0405-IDRPL                       P090
456700     MOVE        RS05-NOPAM TO C-0405-NOPAM.                      P095
456800           IF    XO00-XORATA = 'RS04'                             P100
456900     MOVE        D-0406 TO C-0406                                 P105
457000     MOVE        RS04-CNREN TO C-0406-CNREN                       P110
457100     MOVE        RS04-NORER TO C-0406-NORER                       P115
457200     MOVE        RS04-IDRPL TO C-0406-IDRPL.                      P120
457300           IF    XO00-XORATA = 'RS06'                             P125
457400     MOVE        D-0406 TO C-0406                                 P130
457500     MOVE        RS06-CNREN TO C-0406-CNREN                       P135
457600     MOVE        RS06-NORER TO C-0406-NORER                       P140
457700     MOVE        RS06-IDRPL TO C-0406-IDRPL                       P145
457800     MOVE        RS06-DDREN TO C-0406-DDREN                       P147
457900     MOVE        RS06-XCSEQ TO C-0406-XCSEQ.                      P150
458000           IF    XO00-XORATA = 'RS04'                             P155
458100     MOVE        D-0407 TO C-0407                                 P160
458200     MOVE        RS04-CNREN TO C-0407-CNREN                       P165
458300     MOVE        RS04-NORER TO C-0407-NORER                       P170
458400     MOVE        RS04-IDRPL TO C-0407-IDRPL.                      P175
458500           IF    XO00-XORATA = 'RS07'                             P180
458600     MOVE        D-0407 TO C-0407                                 P185
458700     MOVE        RS07-CNREN TO C-0407-CNREN                       P190
458800     MOVE        RS07-NORER TO C-0407-NORER                       P195
458900     MOVE        RS07-IDRPL TO C-0407-IDRPL                       P200
459000     MOVE        RS07-XCSEQ TO C-0407-XCSEQ.                      P205
459100           IF    XO00-XORATA = 'RS04'                             P210
459200     MOVE        D-0416 TO C-0416                                 P215
459300     MOVE        RS04-CNREN TO C-0416-CNREN                       P220
459400     MOVE        RS04-NORER TO C-0416-NORER                       P225
459500     MOVE        RS04-IDRPL TO C-0416-IDRPL.                      P230
459600           IF    XO00-XORATA = 'RS16'                             P235
459700     MOVE        D-0416 TO C-0416                                 P240
459800     MOVE        RS16-CNREN TO C-0416-CNREN                       P245
459900     MOVE        RS16-NORER TO C-0416-NORER                       P250
460000     MOVE        RS16-IDRPL TO C-0416-IDRPL                       P255
460100     MOVE        RS16-XCSEQ TO C-0416-XCSEQ.                      P260
460200           IF    XO00-XORATA = 'RS04'                             P265
460300     MOVE        D-0417 TO C-0417                                 P270
460400     MOVE        RS04-CNREN TO C-0417-CNREN                       P275
460500     MOVE        RS04-NORER TO C-0417-NORER                       P280
460600     MOVE        RS04-IDRPL TO C-0417-IDRPL.                      P285
460700           IF    XO00-XORATA = 'RS17'                             P290
460800     MOVE        D-0417 TO C-0417                                 P295
460900     MOVE        RS17-CNREN TO C-0417-CNREN                       P300
461000     MOVE        RS17-NORER TO C-0417-NORER                       P305
461100     MOVE        RS17-IDRPL TO C-0417-IDRPL                       P310
461200     MOVE        RS17-NORCS TO C-0417-NORCS.                      P315
461300           IF    XO00-XORATA = 'RS04'                             P320
461400     MOVE        D-0419 TO C-0419                                 P325
461500     MOVE        RS04-CNREN TO C-0419-CNREN                       P330
461600     MOVE        RS04-NORER TO C-0419-NORER                       P335
461700     MOVE        RS04-IDRPL TO C-0419-IDRPL.                      P340
461800           IF    XO00-XORATA = 'RS19'                             P345
461900     MOVE        D-0419 TO C-0419                                 P350
462000     MOVE        RS19-CNREN TO C-0419-CNREN                       P355
462100     MOVE        RS19-NORER TO C-0419-NORER                       P360
462200     MOVE        RS19-IDRPL TO C-0419-IDRPL                       P365
462300     MOVE        RS19-DTPAI TO C-0419-DTPAI                       P370
462400     MOVE        RS19-DTPAIR TO C-0419-DTPAIR                     P375
462500     MOVE        RS19-NOPAI TO C-0419-NOPAI.                      P380
462600           IF    XO00-XORATA = 'RS04'                             P385
462700     MOVE        D-0422 TO C-0422                                 P390
462800     MOVE        RS04-CNREN TO C-0422-CNREN                       P395
462900     MOVE        RS04-NORER TO C-0422-NORER                       P400
463000     MOVE        RS04-IDRPL TO C-0422-IDRPL.                      P405
463100           IF    XO00-XORATA = 'RS22'                             P410
463200     MOVE        D-0422 TO C-0422                                 P415
463300     MOVE        RS22-CNREN TO C-0422-CNREN                       P420
463400     MOVE        RS22-NORER TO C-0422-NORER                       P425
463500     MOVE        RS22-IDRPL TO C-0422-IDRPL                       P430
463600     MOVE        RS22-XCSEQ TO C-0422-XCSEQ.                      P435
463700           IF    XO00-XORATA = 'RS04'                             P440
463800     MOVE        D-0423 TO C-0423                                 P445
463900     MOVE        RS04-CNREN TO C-0423-CNREN                       P450
464000     MOVE        RS04-NORER TO C-0423-NORER                       P455
464100     MOVE        RS04-IDRPL TO C-0423-IDRPL.                      P460
464200           IF    XO00-XORATA = 'RS23'                             P465
464300     MOVE        D-0423 TO C-0423                                 P470
464400     MOVE        RS23-CNREN TO C-0423-CNREN                       P475
464500     MOVE        RS23-NORER TO C-0423-NORER                       P480
464600     MOVE        RS23-IDRPL TO C-0423-IDRPL                       P485
464700     MOVE        RS23-XCSEQ TO C-0423-XCSEQ.                      P490
464800           IF    XO00-XORATA = 'RS04'                             P495
464900     MOVE        D-04XX TO C-04XX                                 P500
465000     MOVE        RS04-CNREN TO C-04XX-CNREN                       P505
465100     MOVE        RS04-NORER TO C-04XX-NORER                       P510
465200     MOVE        RS04-IDRPL TO C-04XX-IDRPL.                      P515
465300           IF    XO00-XORATA = 'RS08'                             P520
465400     MOVE        D-04XX TO C-04XX                                 P525
465500     MOVE        RS08-CNREN TO C-04XX-CNREN                       P530
465600     MOVE        RS08-NORER TO C-04XX-NORER                       P535
465700     MOVE        RS08-IDRPL TO C-04XX-IDRPL.                      P540
465800           IF    XO00-XORATA = 'RS09'                             P545
465900     MOVE        D-04XX TO C-04XX                                 P550
466000     MOVE        RS09-CNREN TO C-04XX-CNREN                       P555
466100     MOVE        RS09-NORER TO C-04XX-NORER                       P560
466200     MOVE        RS09-IDRPL TO C-04XX-IDRPL.                      P565
466300           IF    XO00-XORATA = 'RS10'                             P570
466400     MOVE        D-04XX TO C-04XX                                 P575
466500     MOVE        RS10-CNREN TO C-04XX-CNREN                       P580
466600     MOVE        RS10-NORER TO C-04XX-NORER                       P585
466700     MOVE        RS10-IDRPL TO C-04XX-IDRPL.                      P590
466800           IF    XO00-XORATA = 'RS11'                             P595
466900     MOVE        D-04XX TO C-04XX                                 P600
467000     MOVE        RS11-CNREN TO C-04XX-CNREN                       P605
467100     MOVE        RS11-NORER TO C-04XX-NORER                       P610
467200     MOVE        RS11-IDRPL TO C-04XX-IDRPL.                      P615
467300           IF    XO00-XORATA = 'RS12'                             P620
467400     MOVE        D-04XX TO C-04XX                                 P625
467500     MOVE        RS12-CNREN TO C-04XX-CNREN                       P630
467600     MOVE        RS12-NORER TO C-04XX-NORER                       P635
467700     MOVE        RS12-IDRPL TO C-04XX-IDRPL.                      P640
467800           IF    XO00-XORATA = 'RS13'                             P645
467900     MOVE        D-04XX TO C-04XX                                 P650
468000     MOVE        RS13-CNREN TO C-04XX-CNREN                       P655
468100     MOVE        RS13-NORER TO C-04XX-NORER                       P660
468200     MOVE        RS13-IDRPL TO C-04XX-IDRPL.                      P665
468300           IF    XO00-XORATA = 'RS14'                             P670
468400     MOVE        D-04XX TO C-04XX                                 P675
468500     MOVE        RS14-CNREN TO C-04XX-CNREN                       P680
468600     MOVE        RS14-NORER TO C-04XX-NORER                       P685
468700     MOVE        RS14-IDRPL TO C-04XX-IDRPL.                      P690
468800           IF    XO00-XORATA = 'RS15'                             P695
468900     MOVE        D-04XX TO C-04XX                                 P700
469000     MOVE        RS15-CNREN TO C-04XX-CNREN                       P705
469100     MOVE        RS15-NORER TO C-04XX-NORER                       P710
469200     MOVE        RS15-IDRPL TO C-04XX-IDRPL.                      P715
469300           IF    XO00-XORATA = 'RS20'                             P720
469400     MOVE        D-04XX TO C-04XX                                 P725
469500     MOVE        RS20-CNREN TO C-04XX-CNREN                       P730
469600     MOVE        RS20-NORER TO C-04XX-NORER                       P735
469700     MOVE        RS20-IDRPL TO C-04XX-IDRPL.                      P740
469800           IF    XO00-XORATA = 'RS17'                             P745
469900     MOVE        D-1718 TO C-1718                                 P750
470000     MOVE        RS17-CNREN TO C-1718-CNREN                       P755
470100     MOVE        RS17-NORER TO C-1718-NORER                       P760
470200     MOVE        RS17-IDRPL TO C-1718-IDRPL                       P765
470300     MOVE        RS17-NORCS TO C-1718-NORCS.                      P770
470400           IF    XO00-XORATA = 'RS18'                             P775
470500     MOVE        D-1718 TO C-1718                                 P780
470600     MOVE        RS18-CNREN TO C-1718-CNREN                       P785
470700     MOVE        RS18-NORER TO C-1718-NORER                       P790
470800     MOVE        RS18-IDRPL TO C-1718-IDRPL                       P795
470900     MOVE        RS18-NORCS TO C-1718-NORCS                       P800
471000     MOVE        RS18-XCSEQ TO C-1718-XCSEQ.                      P805
471100 F95I2-FN. EXIT.                                                  P805
471200 F95I3.                                                           P000
471300           IF    XO00-XORATA = 'RS05'                             P005
471400     MOVE        C-0405-CNREN TO RS05-CNREN                       P010
471500     MOVE        C-0405-NORER TO RS05-NORER                       P015
471600     MOVE        C-0405-IDRPL TO RS05-IDRPL.                      P020
471700           IF    XO00-XORATA = 'RS06'                             P025
471800     MOVE        C-0406-CNREN TO RS06-CNREN                       P030
471900     MOVE        C-0406-NORER TO RS06-NORER                       P035
472000     MOVE        C-0406-IDRPL TO RS06-IDRPL                       P040
472100     PERFORM     F95I4 THRU F95I4-FN                              P045
472200     MOVE        XP00-XCSEQ TO RS06-XCSEQ.                        P050
472300           IF    XO00-XORATA = 'RS07'                             P055
472400     MOVE        C-0407-CNREN TO RS07-CNREN                       P060
472500     MOVE        C-0407-NORER TO RS07-NORER                       P065
472600     MOVE        C-0407-IDRPL TO RS07-IDRPL                       P070
472700     PERFORM     F95I4 THRU F95I4-FN                              P075
472800     MOVE        XP00-XCSEQ TO RS07-XCSEQ.                        P080
472900           IF    XO00-XORATA = 'RS08'                             P085
473000     MOVE        C-04XX-CNREN TO RS08-CNREN                       P090
473100     MOVE        C-04XX-NORER TO RS08-NORER                       P095
473200     MOVE        C-04XX-IDRPL TO RS08-IDRPL.                      P100
473300           IF    XO00-XORATA = 'RS09'                             P105
473400     MOVE        C-04XX-CNREN TO RS09-CNREN                       P110
473500     MOVE        C-04XX-NORER TO RS09-NORER                       P115
473600     MOVE        C-04XX-IDRPL TO RS09-IDRPL.                      P120
473700           IF    XO00-XORATA = 'RS10'                             P125
473800     MOVE        C-04XX-CNREN TO RS10-CNREN                       P130
473900     MOVE        C-04XX-NORER TO RS10-NORER                       P135
474000     MOVE        C-04XX-IDRPL TO RS10-IDRPL.                      P140
474100           IF    XO00-XORATA = 'RS11'                             P145
474200     MOVE        C-04XX-CNREN TO RS11-CNREN                       P150
474300     MOVE        C-04XX-NORER TO RS11-NORER                       P155
474400     MOVE        C-04XX-IDRPL TO RS11-IDRPL.                      P160
474500           IF    XO00-XORATA = 'RS12'                             P165
474600     MOVE        C-04XX-CNREN TO RS12-CNREN                       P170
474700     MOVE        C-04XX-NORER TO RS12-NORER                       P175
474800     MOVE        C-04XX-IDRPL TO RS12-IDRPL.                      P180
474900           IF    XO00-XORATA = 'RS13'                             P185
475000     MOVE        C-04XX-CNREN TO RS13-CNREN                       P190
475100     MOVE        C-04XX-NORER TO RS13-NORER                       P195
475200     MOVE        C-04XX-IDRPL TO RS13-IDRPL.                      P200
475300           IF    XO00-XORATA = 'RS14'                             P205
475400     MOVE        C-04XX-CNREN TO RS14-CNREN                       P210
475500     MOVE        C-04XX-NORER TO RS14-NORER                       P215
475600     MOVE        C-04XX-IDRPL TO RS14-IDRPL.                      P220
475700           IF    XO00-XORATA = 'RS15'                             P225
475800     MOVE        C-04XX-CNREN TO RS15-CNREN                       P230
475900     MOVE        C-04XX-NORER TO RS15-NORER                       P235
476000     MOVE        C-04XX-IDRPL TO RS15-IDRPL.                      P240
476100           IF    XO00-XORATA = 'RS16'                             P245
476200     MOVE        C-0416-CNREN TO RS16-CNREN                       P250
476300     MOVE        C-0416-NORER TO RS16-NORER                       P255
476400     MOVE        C-0416-IDRPL TO RS16-IDRPL                       P260
476500     PERFORM     F95I4 THRU F95I4-FN                              P265
476600     MOVE        XP00-XCSEQ TO RS16-XCSEQ.                        P270
476700           IF    XO00-XORATA = 'RS17'                             P275
476800     MOVE        C-0417-CNREN TO RS17-CNREN                       P280
476900     MOVE        C-0417-NORER TO RS17-NORER                       P285
477000     MOVE        C-0417-IDRPL TO RS17-IDRPL.                      P290
477100           IF    XO00-XORATA = 'RS18'                             P295
477200     MOVE        C-1718-CNREN TO RS18-CNREN                       P300
477300     MOVE        C-1718-NORER TO RS18-NORER                       P305
477400     MOVE        C-1718-IDRPL TO RS18-IDRPL                       P310
477500     MOVE        C-1718-NORCS TO RS18-NORCS                       P315
477600     PERFORM     F95I4 THRU F95I4-FN                              P320
477700     MOVE        XP00-XCSEQ TO RS18-XCSEQ.                        P325
477800           IF    XO00-XORATA = 'RS19'                             P330
477900     MOVE        C-0419-CNREN TO RS19-CNREN                       P335
478000     MOVE        C-0419-NORER TO RS19-NORER                       P340
478100     MOVE        C-0419-IDRPL TO RS19-IDRPL.                      P345
478200           IF    XO00-XORATA = 'RS20'                             P350
478300     MOVE        C-04XX-CNREN TO RS20-CNREN                       P355
478400     MOVE        C-04XX-NORER TO RS20-NORER                       P360
478500     MOVE        C-04XX-IDRPL TO RS20-IDRPL.                      P365
478600           IF    XO00-XORATA = 'RS22'                             P370
478700     MOVE        C-0422-CNREN TO RS22-CNREN                       P375
478800     MOVE        C-0422-NORER TO RS22-NORER                       P380
478900     MOVE        C-0422-IDRPL TO RS22-IDRPL                       P385
479000     PERFORM     F95I4 THRU F95I4-FN                              P390
479100     MOVE        XP00-XCSEQ TO RS22-XCSEQ.                        P395
479200           IF    XO00-XORATA = 'RS23'                             P400
479300     MOVE        C-0423-CNREN TO RS23-CNREN                       P405
479400     MOVE        C-0423-NORER TO RS23-NORER                       P410
479500     MOVE        C-0423-IDRPL TO RS23-IDRPL                       P415
479600     PERFORM     F95I4 THRU F95I4-FN                              P420
479700     MOVE        XP00-XCSEQ TO RS23-XCSEQ.                        P425
479800 F95I3-FN. EXIT.                                                  P425
479900 F95I4.                                                           P000
480000     EXEC SQL    SELECT   RS_SET.NEXTVAL                          P100
480100                   INTO  :XP00-XCSEQ                              P110
480200                   FROM   DUAL                         END-EXEC.  P120
480300 F95I4-FN. EXIT.                                                  P120
480400 F95PP.         EXIT.                                             P000
480500 F95-RS16-FV.                                                     P100
480600     MOVE        'SELECT' TO XO00-XORATY                          P101
480700     MOVE        '95PP' TO XO00-XCDFSF                            P102
480800     MOVE        'RS16' TO XO00-XORATA                            P103
480900     MOVE        RS16 TO XO00-XORACL                              P104
481000     MOVE        ZERO TO XOPP-RS16-CF                             P105
481100     EXEC SQL                                                     P108
481200                 SELECT  ROWID,                                   P110
481300                         CNREN                                    P111
481400                      ,  NORER                                    P112
481500                      ,  IDRPL                                    P113
481600                      ,  XCSEQ                                    P114
481700                      ,  nvl(DMREN ,' ')                          P115
481800                      ,  nvl(NOTIE ,' ')                          P116
481900                   INTO :XP00-XROWID,                             P210
482000                        :RS16-CNREN                               P211
482100                      , :RS16-NORER                               P212
482200                      , :RS16-IDRPL                               P213
482300                      , :RS16-XCSEQ                               P214
482400                      , :RS16-DMREN                               P215
482500                      , :RS16-NOTIE                               P216
482600                   FROM  RS16                                     P300
482700                  WHERE                                           P310
482800                         CNREN  = :C-0416-CNREN                   P311
482900                    AND  NORER  = :C-0416-NORER                   P312
483000                    AND  IDRPL  = :C-0416-IDRPL                   P313
483100                    AND                                           P400
483200                         XCSEQ                                    P401
483300                      = ( SELECT MIN (                            P420
483400                         XCSEQ                                    P421
483500                                     )                            P440
483600                   FROM  RS16                                     P442
483700                  WHERE                                           P444
483800                         CNREN  = :C-0416-CNREN                   P445
483900                    AND  NORER  = :C-0416-NORER                   P446
484000                    AND  IDRPL  = :C-0416-IDRPL                   P447
484100                    AND (XCSEQ  > :C-0416-XCSEQ                   P448
484200                        )                                         P449
484300                        )                              END-EXEC.  P475
484400     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
484500           IF    IK = ZERO                                        P485
484600     MOVE        '1' TO XOPP-RS16-CF                              P485
484700     ADD         1 TO XOPP-COUNT                                  P490
484800     MOVE        XP00-XROWID TO RS16-XROWID.                      P495
484900 F95-RS16-FV-FN. EXIT.                                            P499
485000 F95PP-FN. EXIT.                                                  P499
485100 F95QQ.         EXIT.                                             P000
485200 F95-RS17-FV.                                                     P100
485300     MOVE        'SELECT' TO XO00-XORATY                          P101
485400     MOVE        '95QQ' TO XO00-XCDFSF                            P102
485500     MOVE        'RS17' TO XO00-XORATA                            P103
485600     MOVE        RS17 TO XO00-XORACL                              P104
485700     MOVE        ZERO TO XOQQ-RS17-CF                             P105
485800     EXEC SQL                                                     P108
485900                 SELECT  ROWID,                                   P110
486000                         CNREN                                    P111
486100                      ,  NORER                                    P112
486200                      ,  IDRPL                                    P113
486300                      ,  NORCS                                    P114
486400                      ,  nvl(DMREN ,' ')                          P115
486500                      ,  nvl(LIRCS ,' ')                          P116
486600                      ,  nvl(LIRCSB,' ')                          P117
486700                      ,  nvl(CERCS ,' ')                          P118
486800                      ,  nvl(DDRCS ,' ')                          P119
486900                      ,  nvl(CTRCK ,' ')                          P120
487000                      ,  nvl(MKRCL , 0)                           P121
487100                      ,  nvl(DHRCS ,' ')                          P122
487200                      ,  nvl(MORCS , 0)                           P123
487300                      ,  nvl(CTRCB ,' ')                          P124
487400                      ,  nvl(MTRCA , 0)                           P125
487500                      ,  nvl(MTRCG , 0)                           P126
487600                      ,  nvl(MCRCCR, 0)                           P127
487700                      ,  nvl(LNRCA ,' ')                          P128
487800                      ,  nvl(LRRE1 ,' ')                          P129
487900                      ,  nvl(LRRE2 ,' ')                          P130
488000                      ,  nvl(LVRE  ,' ')                          P131
488100                      ,  nvl(CPREN ,' ')                          P132
488200                      ,  nvl(CDREP ,' ')                          P133
488300                      ,  nvl(MTRCB , 0)                           P134
488400                      ,  nvl(DTRCB ,' ')                          P135
488500                   INTO :XP00-XROWID,                             P210
488600                        :RS17-CNREN                               P211
488700                      , :RS17-NORER                               P212
488800                      , :RS17-IDRPL                               P213
488900                      , :RS17-NORCS                               P214
489000                      , :RS17-DMREN                               P215
489100                      , :RS17-LIRCS                               P216
489200                      , :RS17-LIRCSB                              P217
489300                      , :RS17-CERCS                               P218
489400                      , :RS17-DDRCS                               P219
489500                      , :RS17-CTRCK                               P220
489600                      , :RS17-MKRCL                               P221
489700                      , :RS17-DHRCS                               P222
489800                      , :RS17-MORCS                               P223
489900                      , :RS17-CTRCB                               P224
490000                      , :RS17-MTRCA                               P225
490100                      , :RS17-MTRCG                               P226
490200                      , :RS17-MCRCCR                              P227
490300                      , :RS17-LNRCA                               P228
490400                      , :RS17-LRRE1                               P229
490500                      , :RS17-LRRE2                               P230
490600                      , :RS17-LVRE                                P231
490700                      , :RS17-CPREN                               P232
490800                      , :RS17-CDREP                               P233
490900                      , :RS17-MTRCB                               P234
491000                      , :RS17-DTRCB                               P235
491100                   FROM  RS17                                     P300
491200                  WHERE                                           P310
491300                         CNREN  = :C-0417-CNREN                   P311
491400                    AND  NORER  = :C-0417-NORER                   P312
491500                    AND  IDRPL  = :C-0417-IDRPL                   P313
491600                    AND                                           P400
491700                         NORCS                                    P401
491800                      = ( SELECT MIN (                            P420
491900                         NORCS                                    P421
492000                                     )                            P440
492100                   FROM  RS17                                     P442
492200                  WHERE                                           P444
492300                         CNREN  = :C-0417-CNREN                   P445
492400                    AND  NORER  = :C-0417-NORER                   P446
492500                    AND  IDRPL  = :C-0417-IDRPL                   P447
492600                    AND (NORCS  > :C-0417-NORCS                   P448
492700                        )                                         P449
492800                        )                              END-EXEC.  P475
492900     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
493000           IF    IK = ZERO                                        P485
493100     MOVE        '1' TO XOQQ-RS17-CF                              P485
493200     ADD         1 TO XOQQ-COUNT                                  P490
493300     MOVE        XP00-XROWID TO RS17-XROWID.                      P495
493400 F95-RS17-FV-FN. EXIT.                                            P499
493500 F95QQ-FN. EXIT.                                                  P499
493600 F95RR.         EXIT.                                             P000
493700 F95-RS18-FV.                                                     P100
493800     MOVE        'SELECT' TO XO00-XORATY                          P101
493900     MOVE        '95RR' TO XO00-XCDFSF                            P102
494000     MOVE        'RS18' TO XO00-XORATA                            P103
494100     MOVE        RS18 TO XO00-XORACL                              P104
494200     MOVE        ZERO TO XORR-RS18-CF                             P105
494300     EXEC SQL                                                     P108
494400                 SELECT  ROWID,                                   P110
494500                         CNREN                                    P111
494600                      ,  NORER                                    P112
494700                      ,  IDRPL                                    P113
494800                      ,  NORCS                                    P114
494900                      ,  XCSEQ                                    P115
495000                      ,  nvl(DTRCB ,' ')                          P116
495100                      ,  nvl(MTRCBH, 0)                           P117
495200                      ,  nvl(DMREN ,' ')                          P118
495300                   INTO :XP00-XROWID,                             P210
495400                        :RS18-CNREN                               P211
495500                      , :RS18-NORER                               P212
495600                      , :RS18-IDRPL                               P213
495700                      , :RS18-NORCS                               P214
495800                      , :RS18-XCSEQ                               P215
495900                      , :RS18-DTRCB                               P216
496000                      , :RS18-MTRCBH                              P217
496100                      , :RS18-DMREN                               P218
496200                   FROM  RS18                                     P300
496300                  WHERE                                           P310
496400                         CNREN  = :C-1718-CNREN                   P311
496500                    AND  NORER  = :C-1718-NORER                   P312
496600                    AND  IDRPL  = :C-1718-IDRPL                   P313
496700                    AND  NORCS  = :C-1718-NORCS                   P314
496800                    AND                                           P400
496900                         XCSEQ                                    P401
497000                      = ( SELECT MIN (                            P420
497100                         XCSEQ                                    P421
497200                                     )                            P440
497300                   FROM  RS18                                     P442
497400                  WHERE                                           P444
497500                         CNREN  = :C-1718-CNREN                   P445
497600                    AND  NORER  = :C-1718-NORER                   P446
497700                    AND  IDRPL  = :C-1718-IDRPL                   P447
497800                    AND  NORCS  = :C-1718-NORCS                   P448
497900                    AND (XCSEQ  > :C-1718-XCSEQ                   P449
498000                        )                                         P450
498100                        )                              END-EXEC.  P475
498200     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
498300           IF    IK = ZERO                                        P485
498400     MOVE        '1' TO XORR-RS18-CF                              P485
498500     ADD         1 TO XORR-COUNT                                  P490
498600     MOVE        XP00-XROWID TO RS18-XROWID.                      P495
498700 F95-RS18-FV-FN. EXIT.                                            P499
498800 F95RR-FN. EXIT.                                                  P499
498900 F95SS.         EXIT.                                             P000
499000 F95-RS19-FV.                                                     P100
499100     MOVE        'SELECT' TO XO00-XORATY                          P101
499200     MOVE        '95SS' TO XO00-XCDFSF                            P102
499300     MOVE        'RS19' TO XO00-XORATA                            P103
499400     MOVE        RS19 TO XO00-XORACL                              P104
499500     MOVE        ZERO TO XOSS-RS19-CF                             P105
499600     EXEC SQL                                                     P108
499700                 SELECT  ROWID,                                   P110
499800                         CNREN                                    P111
499900                      ,  NORER                                    P112
500000                      ,  IDRPL                                    P113
500100                      ,  NOPAI                                    P114
500200                      ,  nvl(DHPAI ,' ')                          P115
500300                      ,  DTPAI                                    P116
500400                      ,  nvl(MTPAI , 0)                           P117
500500                      ,  nvl(MORER , 0)                           P118
500600                      ,  nvl(CDPAD ,' ')                          P119
500700                      ,  nvl(NOTIE ,' ')                          P120
500800                      ,  nvl(NOPAM , 0)                           P121
500900                      ,  nvl(CDPAM ,' ')                          P122
501000                      ,  DTPAIR                                   P123
501100                      ,  nvl(CDPAS ,' ')                          P124
501200                      ,  nvl(CDPAP ,' ')                          P125
501300                      ,  nvl(NOPAF ,' ')                          P126
501400                      ,  nvl(CDPAI ,' ')                          P127
501500                      ,  nvl(CDTAXE,' ')                          P128
501600                      ,  nvl(CDCAL ,' ')                          P129
501700                      ,  nvl(MTREF , 0)                           P130
501800                      ,  nvl(DDPAI ,' ')                          P131
501900                   INTO :XP00-XROWID,                             P210
502000                        :RS19-CNREN                               P211
502100                      , :RS19-NORER                               P212
502200                      , :RS19-IDRPL                               P213
502300                      , :RS19-NOPAI                               P214
502400                      , :RS19-DHPAI                               P215
502500                      , :RS19-DTPAI                               P216
502600                      , :RS19-MTPAI                               P217
502700                      , :RS19-MORER                               P218
502800                      , :RS19-CDPAD                               P219
502900                      , :RS19-NOTIE                               P220
503000                      , :RS19-NOPAM                               P221
503100                      , :RS19-CDPAM                               P222
503200                      , :RS19-DTPAIR                              P223
503300                      , :RS19-CDPAS                               P224
503400                      , :RS19-CDPAP                               P225
503500                      , :RS19-NOPAF                               P226
503600                      , :RS19-CDPAI                               P227
503700                      , :RS19-CDTAXE                              P228
503800                      , :RS19-CDCAL                               P229
503900                      , :RS19-MTREF                               P230
504000                      , :RS19-DDPAI                               P231
504100                   FROM  RS19                                     P300
504200                  WHERE                                           P310
504300                         CNREN  = :C-0419-CNREN                   P311
504400                    AND  NORER  = :C-0419-NORER                   P312
504500                    AND  IDRPL  = :C-0419-IDRPL                   P313
504600                    AND                                           P400
504700                         DTPAI                                    P401
504800                     ||  DTPAIR                                   P402
504900                     ||  TO_CHAR(NOPAI, '009')                    P403
505000                      = ( SELECT MIN (                            P420
505100                         DTPAI                                    P421
505200                     ||  DTPAIR                                   P422
505300                     ||  TO_CHAR(NOPAI, '009')                    P423
505400                                     )                            P440
505500                   FROM  RS19                                     P442
505600                  WHERE                                           P444
505700                         CNREN  = :C-0419-CNREN                   P445
505800                    AND  NORER  = :C-0419-NORER                   P446
505900                    AND  IDRPL  = :C-0419-IDRPL                   P447
506000                    AND (DTPAI  > :C-0419-DTPAI                   P448
506100                     OR (DTPAI  = :C-0419-DTPAI                   P449
506200                    AND (DTPAIR > :C-0419-DTPAIR                  P450
506300                     OR (DTPAIR = :C-0419-DTPAIR                  P451
506400                    AND (NOPAI  > :C-0419-NOPAI                   P452
506500                        )))))                                     P453
506600                        )                              END-EXEC.  P475
506700     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
506800           IF    IK = ZERO                                        P485
506900     MOVE        '1' TO XOSS-RS19-CF                              P485
507000     ADD         1 TO XOSS-COUNT                                  P490
507100     MOVE        XP00-XROWID TO RS19-XROWID.                      P495
507200 F95-RS19-FV-FN. EXIT.                                            P499
507300 F95SS-FN. EXIT.                                                  P499
507400 F95TT.         EXIT.                                             P000
507500 F95-RS22-FV.                                                     P100
507600     MOVE        'SELECT' TO XO00-XORATY                          P101
507700     MOVE        '95TT' TO XO00-XCDFSF                            P102
507800     MOVE        'RS22' TO XO00-XORATA                            P103
507900     MOVE        RS22 TO XO00-XORACL                              P104
508000     MOVE        ZERO TO XOTT-RS22-CF                             P105
508100     EXEC SQL                                                     P108
508200                 SELECT  ROWID,                                   P110
508300                         CNREN                                    P111
508400                      ,  NORER                                    P112
508500                      ,  IDRPL                                    P113
508600                      ,  XCSEQ                                    P114
508700                      ,  nvl(DMREN ,' ')                          P115
508800                      ,  nvl(DHPAI ,' ')                          P116
508900                      ,  nvl(MTPAIO, 0)                           P117
509000                      ,  nvl(LIPAO ,' ')                          P118
509100                      ,  nvl(NOTIE ,' ')                          P119
509200                      ,  nvl(CTPAO ,' ')                          P120
509300                      ,  nvl(NOPAI , 0)                           P121
509400                      ,  nvl(LIPA2 ,' ')                          P122
509500                      ,  nvl(CDTAXE,' ')                          P123
509600                      ,  CDRES                                    P124
509700                   INTO :XP00-XROWID,                             P210
509800                        :RS22-CNREN                               P211
509900                      , :RS22-NORER                               P212
510000                      , :RS22-IDRPL                               P213
510100                      , :RS22-XCSEQ                               P214
510200                      , :RS22-DMREN                               P215
510300                      , :RS22-DHPAI                               P216
510400                      , :RS22-MTPAIO                              P217
510500                      , :RS22-LIPAO                               P218
510600                      , :RS22-NOTIE                               P219
510700                      , :RS22-CTPAO                               P220
510800                      , :RS22-NOPAI                               P221
510900                      , :RS22-LIPA2                               P222
511000                      , :RS22-CDTAXE                              P223
511100                      , :RS22-CDRES                               P224
511200                   FROM  RS22                                     P300
511300                  WHERE                                           P310
511400                         CNREN  = :C-0422-CNREN                   P311
511500                    AND  NORER  = :C-0422-NORER                   P312
511600                    AND  IDRPL  = :C-0422-IDRPL                   P313
511700                    AND                                           P400
511800                         XCSEQ                                    P401
511900                      = ( SELECT MIN (                            P420
512000                         XCSEQ                                    P421
512100                                     )                            P440
512200                   FROM  RS22                                     P442
512300                  WHERE                                           P444
512400                         CNREN  = :C-0422-CNREN                   P445
512500                    AND  NORER  = :C-0422-NORER                   P446
512600                    AND  IDRPL  = :C-0422-IDRPL                   P447
512700                    AND (XCSEQ  > :C-0422-XCSEQ                   P448
512800                        )                                         P449
512900                        )                              END-EXEC.  P475
513000     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
513100           IF    IK = ZERO                                        P485
513200     MOVE        '1' TO XOTT-RS22-CF                              P485
513300     ADD         1 TO XOTT-COUNT                                  P490
513400     MOVE        XP00-XROWID TO RS22-XROWID.                      P495
513500 F95-RS22-FV-FN. EXIT.                                            P499
513600 F95TT-FN. EXIT.                                                  P499
513700 F95UU.         EXIT.                                             P000
513800 F95-RS23-FV.                                                     P100
513900     MOVE        'SELECT' TO XO00-XORATY                          P101
514000     MOVE        '95UU' TO XO00-XCDFSF                            P102
514100     MOVE        'RS23' TO XO00-XORATA                            P103
514200     MOVE        RS23 TO XO00-XORACL                              P104
514300     MOVE        ZERO TO XOUU-RS23-CF                             P105
514400     EXEC SQL                                                     P108
514500                 SELECT  ROWID,                                   P110
514600                         CNREN                                    P111
514700                      ,  NORER                                    P112
514800                      ,  IDRPL                                    P113
514900                      ,  XCSEQ                                    P114
515000                      ,  nvl(MTREB , 0)                           P115
515100                      ,  nvl(PXREF , 0)                           P116
515200                      ,  nvl(NOCRA ,' ')                          P117
515300                      ,  nvl(NOSIN ,' ')                          P118
515400                      ,  nvl(NIREGS,' ')                          P119
515500                      ,  nvl(ZN04  , 0)                           P120
515600                      ,  nvl(CTRS23,' ')                          P121
515700                   INTO :XP00-XROWID,                             P210
515800                        :RS23-CNREN                               P211
515900                      , :RS23-NORER                               P212
516000                      , :RS23-IDRPL                               P213
516100                      , :RS23-XCSEQ                               P214
516200                      , :RS23-MTREB                               P215
516300                      , :RS23-PXREF                               P216
516400                      , :RS23-NOCRA                               P217
516500                      , :RS23-NOSIN                               P218
516600                      , :RS23-NIREGS                              P219
516700                      , :RS23-ZN04                                P220
516800                      , :RS23-CTRS23                              P221
516900                   FROM  RS23                                     P300
517000                  WHERE                                           P310
517100                         CNREN  = :C-0423-CNREN                   P311
517200                    AND  NORER  = :C-0423-NORER                   P312
517300                    AND  IDRPL  = :C-0423-IDRPL                   P313
517400                    AND                                           P400
517500                         XCSEQ                                    P401
517600                      = ( SELECT MIN (                            P420
517700                         XCSEQ                                    P421
517800                                     )                            P440
517900                   FROM  RS23                                     P442
518000                  WHERE                                           P444
518100                         CNREN  = :C-0423-CNREN                   P445
518200                    AND  NORER  = :C-0423-NORER                   P446
518300                    AND  IDRPL  = :C-0423-IDRPL                   P447
518400                    AND (XCSEQ  > :C-0423-XCSEQ                   P448
518500                        )                                         P449
518600                        )                              END-EXEC.  P475
518700     PERFORM     F95-WORK-OK THRU F95-WORK-OK-FN.                 P480
518800           IF    IK = ZERO                                        P485
518900     MOVE        '1' TO XOUU-RS23-CF                              P485
519000     ADD         1 TO XOUU-COUNT                                  P490
519100     MOVE        XP00-XROWID TO RS23-XROWID.                      P495
519200 F95-RS23-FV-FN. EXIT.                                            P499
519300 F95UU-FN. EXIT.                                                  P499
519400 F9590.         EXIT.                                             P000
519500 F95-WORK-CN.                                                     P100
519600     MOVE        '9590' TO XO00-XCDFSF                            P110
519700     MOVE        'CONNECT' TO XO00-XORATY                         P120
519800     MOVE        SPACE TO XO00-XORATA                             P130
519900     MOVE        USERID TO XO00-XORACL                            P140
520000     EXEC SQL    CONNECT :USERID                       END-EXEC.  P150
520100 F95-WORK-CN-FN.                                                  P199
520200 F9590-FN. EXIT.                                                  P199
520300 F9591.         EXIT.                                             P000
520400 F95-WORK-RBS.                                                    P100
520500     MOVE        '9591' TO XO00-XCDFSF                            P110
520600     MOVE        'RB SEGMT' TO XO00-XORATY                        P120
520700     MOVE        SPACE TO XO00-XORATA                             P130
520800     MOVE        XO00-XORARB TO XO00-XORACL                       P140
520900     EXEC SQL    SET TRANSACTION USE                              P150
521000                 ROLLBACK SEGMENT :XO00-XORARB         END-EXEC.  P160
521100 F95-WORK-RBS-FN.                                                 P199
521200 F9591-FN. EXIT.                                                  P199
521300 F9592.         EXIT.                                             P000
521400 F95-WORK-CO.                                                     P100
521500     MOVE        '9592' TO XO00-XCDFSF                            P110
521600     MOVE        'COMMIT' TO XO00-XORATY                          P120
521700     MOVE        SPACE TO XO00-XORATA                             P130
521800     MOVE        SPACE TO XO00-XORACL                             P140
521900     EXEC SQL    COMMIT WORK                           END-EXEC.  P150
522000     MOVE        '1' TO XO00-XORACO.                              P155
522100           IF    XO00-XORARB NOT = SPACE                          P160
522200     PERFORM     F95-WORK-RBS THRU F95-WORK-RBS-FN.               P165
522300 F95-WORK-CO-FN.                                                  P199
522400 F9592-FN. EXIT.                                                  P199
522500 F9594.         EXIT.                                             P000
522600 F95-WORK-RO.                                                     P100
522700     MOVE        '9594' TO XO00-XCDFSF                            P110
522800     MOVE        'ROLLBACK' TO XO00-XORATY                        P120
522900     MOVE        SPACE TO XO00-XORATA                             P130
523000     MOVE        SPACE TO XO00-XORACL                             P140
523100     EXEC SQL    ROLLBACK WORK                         END-EXEC.  P150
523200 F95-WORK-RO-FN.                                                  P199
523300 F9594-FN. EXIT.                                                  P199
523400 F9596.         EXIT.                                             P000
523500 F95-WORK-RR.                                                     P100
523600     MOVE        '9596' TO XO00-XCDFSF                            P110
523700     MOVE        'RELEASE' TO XO00-XORATY                         P120
523800     MOVE        SPACE TO XO00-XORATA                             P130
523900     MOVE        SPACE TO XO00-XORACL                             P140
524000     EXEC SQL    ROLLBACK WORK RELEASE                 END-EXEC.  P150
524100 F95-WORK-RR-FN.                                                  P199
524200 F9596-FN. EXIT.                                                  P199
524300 F9598.         EXIT.                                             P000
524400 F95-WORK-TR.                                                     P100
524500     MOVE        '9598' TO XO00-XCDFSF                            P110
524600     MOVE        'TRACE' TO XO00-XORATY                           P120
524700     MOVE        SPACE TO XO00-XORATA                             P130
524800     MOVE        SPACE TO XO00-XORACL                             P140
524900     EXEC SQL    ALTER SESSION SET SQL_TRACE TRUE      END-EXEC.  P150
525000 F95-WORK-TR-FN.                                                  P199
525100 F9598-FN. EXIT.                                                  P199
525200 F9599.         EXIT.                                             P000
525300 F95-WORK-OK.                                                     P100
525400           IF    SQLCODE = ZERO                                   P110
525500     MOVE        ZERO TO IK                                       P110
525600     PERFORM     F95I2 THRU F95I2-FN                              P115
525700           ELSE                                                   P120
525800     MOVE        '1' TO IK.                                       P120
525900     MOVE        SQLCODE TO XO00-XORARC.                          P130
526000 F95-WORK-OK-FN.                                                  P199
526100 F9599-FN. EXIT.                                                  P199
526200 F98.           EXIT.                                             P000
526300 F98-D.         EXIT.                                             P000
526400 F98-H.                                                           P000
526500     DISPLAY     '------------------------------'                 P010
526600     '------------------------------'                             P020
526700     '-------------'                                              P030
526800     DISPLAY     '- Programme ' PROGE ' : '                       P100
526900     'RENTES                        '.                            P110
527000     DISPLAY     '-   version ' NUGNA ' generee '                 P200
527100     'le ' DATGN ' a ' TIMGN ' en '                               P210
527200     'bibliotheque ' APPLI.                                       P220
527300     DISPLAY     '-   debut : ' XAED-XDATRT ' '                   P300
527400     XAED-XHETRT.                                                 P310
527500     DISPLAY     '------------------------------'                 P500
527600     '------------------------------'                             P520
527700     '-------------'.                                             P530
527800 F98-H-FN. EXIT.                                                  P530
527900 F98-L.    IF    DATCE = XA30-ENVVAL                              P000
528000           NEXT SENTENCE ELSE GO TO     F98-L-FN.                 P000
528100     DISPLAY                      '- Attention ! La date du jour aP100
528200-                                 ' ete forcee par la variable GCAP110
528300-                '_DATCE'.                                        P120
528400     DISPLAY     '------------------------------'                 P500
528500     '------------------------------'                             P520
528600     '-------------'.                                             P530
528700 F98-L-FN. EXIT.                                                  P530
528800 F98-D-FN. EXIT.                                                  P530
528900 F98-Z.                                                           P000
529000     DISPLAY     '-'.                                             P010
529100 F98AA.                                                           P000
529200     MOVE        XOAA-COUNT TO XA80-XQNENR.                       P100
529300     DISPLAY     '- oracle F95AA ' XA80-XQNENR                    P200
529400     ' enregistrement  lu '                                       P210
529500     ' dans RS01'.                                                P220
529600 F98AA-FN. EXIT.                                                  P220
529700 F98BB.                                                           P000
529800     MOVE        XOBB-COUNT TO XA80-XQNENR.                       P100
529900     DISPLAY     '- oracle F95BB ' XA80-XQNENR                    P200
530000     ' enregistrements lus'                                       P210
530100     ' dans RS42'.                                                P220
530200 F98BB-FN. EXIT.                                                  P220
530300 F98CC.                                                           P000
530400     MOVE        XOCC-COUNT TO XA80-XQNENR.                       P100
530500     DISPLAY     '- oracle F95CC ' XA80-XQNENR                    P200
530600     ' enregistrements lus'                                       P210
530700     ' dans RS02'.                                                P220
530800 F98CC-FN. EXIT.                                                  P220
530900 F98DD.                                                           P000
531000     MOVE        XODD-COUNT TO XA80-XQNENR.                       P100
531100     DISPLAY     '- oracle F95DD ' XA80-XQNENR                    P200
531200     ' enregistrements lus'                                       P210
531300     ' dans RS04'.                                                P220
531400 F98DD-FN. EXIT.                                                  P220
531500 F98EE.                                                           P000
531600     MOVE        XOEE-COUNT TO XA80-XQNENR.                       P100
531700     DISPLAY     '- oracle F95EE ' XA80-XQNENR                    P200
531800     ' enregistrements lus'                                       P210
531900     ' dans RS05'.                                                P220
532000 F98EE-FN. EXIT.                                                  P220
532100 F98FF.                                                           P000
532200     MOVE        XOFF-COUNT TO XA80-XQNENR.                       P100
532300     DISPLAY     '- oracle F95FF ' XA80-XQNENR                    P200
532400     ' enregistrements lus'                                       P210
532500     ' dans RS06 par curseur'.                                    P220
532600 F98FF-FN. EXIT.                                                  P220
532700 F98GG.                                                           P000
532800     MOVE        XOGG-COUNT TO XA80-XQNENR.                       P100
532900     DISPLAY     '- oracle F95GG ' XA80-XQNENR                    P200
533000     ' enregistrements lus'                                       P210
533100     ' dans RS07'.                                                P220
533200 F98GG-FN. EXIT.                                                  P220
533300 F98H1.                                                           P000
533400     MOVE        XOH1-COUNT TO XA80-XQNENR.                       P100
533500     DISPLAY     '- oracle F95H1 ' XA80-XQNENR                    P200
533600     ' enregistrements lus'                                       P210
533700     ' dans RS08'.                                                P220
533800 F98H1-FN. EXIT.                                                  P220
533900 F98H2.                                                           P000
534000     MOVE        XOH2-COUNT TO XA80-XQNENR.                       P100
534100     DISPLAY     '- oracle F95H2 ' XA80-XQNENR                    P200
534200     ' enregistrements lus'                                       P210
534300     ' dans RS09'.                                                P220
534400 F98H2-FN. EXIT.                                                  P220
534500 F98H3.                                                           P000
534600     MOVE        XOH3-COUNT TO XA80-XQNENR.                       P100
534700     DISPLAY     '- oracle F95H3 ' XA80-XQNENR                    P200
534800     ' enregistrements lus'                                       P210
534900     ' dans RS10'.                                                P220
535000 F98H3-FN. EXIT.                                                  P220
535100 F98H4.                                                           P000
535200     MOVE        XOH4-COUNT TO XA80-XQNENR.                       P100
535300     DISPLAY     '- oracle F95H4 ' XA80-XQNENR                    P200
535400     ' enregistrements lus'                                       P210
535500     ' dans RS11'.                                                P220
535600 F98H4-FN. EXIT.                                                  P220
535700 F98H5.                                                           P000
535800     MOVE        XOH5-COUNT TO XA80-XQNENR.                       P100
535900     DISPLAY     '- oracle F95H5 ' XA80-XQNENR                    P200
536000     ' enregistrements lus'                                       P210
536100     ' dans RS12'.                                                P220
536200 F98H5-FN. EXIT.                                                  P220
536300 F98H6.                                                           P000
536400     MOVE        XOH6-COUNT TO XA80-XQNENR.                       P100
536500     DISPLAY     '- oracle F95H6 ' XA80-XQNENR                    P200
536600     ' enregistrements lus'                                       P210
536700     ' dans RS13'.                                                P220
536800 F98H6-FN. EXIT.                                                  P220
536900 F98H7.                                                           P000
537000     MOVE        XOH7-COUNT TO XA80-XQNENR.                       P100
537100     DISPLAY     '- oracle F95H7 ' XA80-XQNENR                    P200
537200     ' enregistrements lus'                                       P210
537300     ' dans RS14'.                                                P220
537400 F98H7-FN. EXIT.                                                  P220
537500 F98H8.                                                           P000
537600     MOVE        XOH8-COUNT TO XA80-XQNENR.                       P100
537700     DISPLAY     '- oracle F95H8 ' XA80-XQNENR                    P200
537800     ' enregistrements lus'                                       P210
537900     ' dans RS15'.                                                P220
538000 F98H8-FN. EXIT.                                                  P220
538100 F98H9.                                                           P000
538200     MOVE        XOH9-COUNT TO XA80-XQNENR.                       P100
538300     DISPLAY     '- oracle F95H9 ' XA80-XQNENR                    P200
538400     ' enregistrements lus'                                       P210
538500     ' dans RS20'.                                                P220
538600 F98H9-FN. EXIT.                                                  P220
538700 F98PP.                                                           P000
538800     MOVE        XOPP-COUNT TO XA80-XQNENR.                       P100
538900     DISPLAY     '- oracle F95PP ' XA80-XQNENR                    P200
539000     ' enregistrements lus'                                       P210
539100     ' dans RS16'.                                                P220
539200 F98PP-FN. EXIT.                                                  P220
539300 F98QQ.                                                           P000
539400     MOVE        XOQQ-COUNT TO XA80-XQNENR.                       P100
539500     DISPLAY     '- oracle F95QQ ' XA80-XQNENR                    P200
539600     ' enregistrements lus'                                       P210
539700     ' dans RS17'.                                                P220
539800 F98QQ-FN. EXIT.                                                  P220
539900 F98RR.                                                           P000
540000     MOVE        XORR-COUNT TO XA80-XQNENR.                       P100
540100     DISPLAY     '- oracle F95RR ' XA80-XQNENR                    P200
540200     ' enregistrements lus'                                       P210
540300     ' dans RS18'.                                                P220
540400 F98RR-FN. EXIT.                                                  P220
540500 F98SS.                                                           P000
540600     MOVE        XOSS-COUNT TO XA80-XQNENR.                       P100
540700     DISPLAY     '- oracle F95SS ' XA80-XQNENR                    P200
540800     ' enregistrements lus'                                       P210
540900     ' dans RS19'.                                                P220
541000 F98SS-FN. EXIT.                                                  P220
541100 F98TT.                                                           P000
541200     MOVE        XOTT-COUNT TO XA80-XQNENR.                       P100
541300     DISPLAY     '- oracle F95TT ' XA80-XQNENR                    P200
541400     ' enregistrements lus'                                       P210
541500     ' dans RS22'.                                                P220
541600 F98TT-FN. EXIT.                                                  P220
541700 F98UU.                                                           P000
541800     MOVE        XOUU-COUNT TO XA80-XQNENR.                       P100
541900     DISPLAY     '- oracle F95UU ' XA80-XQNENR                    P200
542000     ' enregistrements lus'                                       P210
542100     ' dans RS23'.                                                P220
542200 F98UU-FN. EXIT.                                                  P220
542300 F98YX.                                                           P000
542400     MOVE        5-YX00-CPTENR TO XA80-XQNENR.                    P100
542500     DISPLAY     '- fichier ' 'YX   '                             P200
542600     XA80-XQNENR ' enregistrements'                               P210
542700     ' assignation: ' 'YX'                                        P220
542800     ' ouverture: ' 'O'.                                          P230
542900 F98YX-FN. EXIT.                                                  P230
543000 F9899.                                                           P000
543100     DISPLAY     '-     fin : ' XAED-XDATRT ' '                   P300
543200     XAED-XHETRT.                                                 P310
543300     DISPLAY     '------------------------------'                 P500
543400     '------------------------------'                             P520
543500     '-------------'.                                             P530
543600 F9899-FN. EXIT.                                                  P530
543700 F98-Z-FN. EXIT.                                                  P530
543800 F98-FN.   EXIT.                                                  P530
543900 F99OR.                                                           P000
544000     PERFORM     F0BBA THRU F0BBA-FN.                             P100
544100 F99OV.                                                           P000
544200     MOVE        001                      TO J99OVR.              P000
544300                              GO TO     F99OV-B.                  P000
544400 F99OV-A.                                                         P000
544500     ADD         1                        TO J99OVR.              P000
544600 F99OV-B.                                                         P000
544700     IF          IXO00M                   <  J99OVR               P000
544800                              GO TO     F99OV-FN.                 P000
544900           IF    XO00-XORAC1 (J99OVR) < SPACE                     P100
545000           OR    XO00-XORAC1 (J99OVR) > 'Z'                       P110
545100     MOVE        '.' TO XO00-XORAC1 (J99OVR).                     P100
545200 F99OV-900. GO TO F99OV-A.                                        P100
545300 F99OV-FN. EXIT.                                                  P100
545400 F99OZ.                                                           P000
545500     MOVE        '99OR' TO XA60-XCDFSF                            P100
545600     MOVE        'ERREUR ORACLE' TO XA60-XLISUI                   P110
545700     MOVE        XO00-XORAER TO XA60-ZX67A                        P130
545800     MOVE        XO00-XORAE2 TO XA60-ZX67B                        P140
545900     MOVE        SQLERRMC TO XA60-ZX67C.                          P150
546000     PERFORM     F9900 THRU F9900-FN.                             P210
546100 F99OZ-FN. EXIT.                                                  P210
546200 F99OR-FN. EXIT.                                                  P210
546300 F99SW.         EXIT.                                             P000
546400 F99SW-FN. EXIT.                                                  P000
546500 F99SX.         EXIT.                                             P000
546600 F99SX-FN. EXIT.                                                  P000
546700 F99VE.                                                           P000
546800     MOVE        ZERO TO IK                                       P100
546900     DISPLAY     XA30-ENVNAM                                      P110
547000     UPON ENVIRONMENT-NAME                                        P120
547100     ACCEPT      XA30-ENVVAL                                      P200
547200     FROM ENVIRONMENT-VALUE                                       P210
547300     ON EXCEPTION                                                 P220
547400     MOVE        '1' TO IK                                        P230
547500     MOVE        SPACE TO XA30-ENVVAL.                            P240
547600 F99VE-FN. EXIT.                                                  P240
547700 F99VI.                                                           P000
547800     MOVE        ZERO TO IK                                       P100
547900     DISPLAY     XA30-ENVNAM                                      P110
548000     UPON ENVIRONMENT-NAME                                        P120
548100     DISPLAY     XA30-ENVVAL                                      P200
548200     UPON ENVIRONMENT-VALUE                                       P210
548300     ON EXCEPTION                                                 P220
548400     MOVE        '1' TO IK.                                       P230
548500 F99VO.    IF    IK = ZERO                                        P000
548600           NEXT SENTENCE ELSE GO TO     F99VO-FN.                 P000
548700     MOVE        SPACE TO YX00                                    P100
548800     STRING      'setenv ' DELIMITED BY SIZE                      P110
548900     XA30-ENVNAM DELIMITED BY SPACE                               P115
549000     ' ' DELIMITED BY SIZE                                        P120
549100     XA30-ENVVAL DELIMITED BY SPACE                               P125
549200     INTO YX00 ON OVERFLOW                                        P130
549300     MOVE        '1' TO IK.                                       P150
549400     PERFORM     F90YX THRU F90YX-FN.                             P200
549500 F99VO-FN. EXIT.                                                  P200
549600 F99VI-FN. EXIT.                                                  P200
549700 F9900.         EXIT.                                             P000
549800 F9910.                                                           P000
549900     DISPLAY     '***'                                            P100
550000     DISPLAY     '***' XA60-L1                                    P200
550100     DISPLAY     '***' XA60-ZX67A.                                P300
550200           IF    XA60-ZX67B NOT = SPACE                           P400
550300     DISPLAY     '***' XA60-ZX67B.                                P400
550400           IF    XA60-ZX67C NOT = SPACE                           P500
550500     DISPLAY     '***' XA60-ZX67C                                 P500
550600     DISPLAY     '***'.                                           P600
550700 F9910-FN. EXIT.                                                  P600
550800 F9920.                                                           P000
550900     PERFORM     F98-Z THRU F98-Z-FN.                             P100
551000 F9950.    IF    XO00-XORACN NOT = ZERO                           P000
551100           NEXT SENTENCE ELSE GO TO     F9950-FN.                 P000
551200     MOVE        ZERO TO XO00-XORACN                              P100
551300     PERFORM     F95-WORK-RR THRU F95-WORK-RR-FN.                 P110
551400 F9950-FN. EXIT.                                                  P110
551500 F9980.                                                           P000
551600     MOVE        4 TO XA00-XRC.                                   P100
551700           IF    XO00-XORACO NOT = ZERO                           P500
551800     MOVE        8 TO XA00-XRC.                                   P500
551900 F9980-FN. EXIT.                                                  P500
552000 F9990.                                                           P000
552100     MOVE        XA00-XRC TO RETURN-CODE                          P100
552200     STOP RUN.                                                    P500
552300 F9990-FN. EXIT.                                                  P500
552400 F9920-FN. EXIT.                                                  P500
552500 F9900-FN. EXIT.                                                  P500
552600 F9999.                                                           P000
552700     DISPLAY     '******* ABORT PROVOQUE *******'                 P100
552800     ' ' XA60-XCDFSF                                              P150
552900     PERFORM     F9920 THRU F9920-FN.                             P200
553000 F9999-FN. EXIT.                                                  P200
