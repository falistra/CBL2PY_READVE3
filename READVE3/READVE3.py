# -*- coding: utf-8 -*-

import sys
import re
from collections import OrderedDict

import ws_rest
import db_access_core
import lib_dt
import lib_anag_sku
import lib_exception

import get_prezzo_stock

import RAPPRAI3
import db
import common_fun
import logging
from logging import config
from LoggingConfig import log_config      # configurazione del logger
config.dictConfig(log_config)

# solo per compatibilta con python 2.7
input = raw_input

class Program():
    def __init__(self,loglevel='info'):
        if (loglevel == 'debug'):
            logging.getLogger().setLevel(logging.DEBUG)
        elif (loglevel == 'info'):
            logging.getLogger().setLevel(logging.INFO)
        elif (loglevel == 'warning'):
            logging.getLogger().setLevel(logging.WARING)
        else:
            logging.getLogger().setLevel(logging.ERROR)

        self.DESTINO_VALIDO = [73, 90, 94, 27, 28, 29, 34, 22]
        self.TABELLA_NO_GIAC = []

        self.session = db_access_core.mysql_connect('reretis', echo=False)
        engine = db.mysql_engine('reretis', echo=False)
        self.conn = engine.connect()

    def READVE3(self):
        logging.debug("====INIZIO READVE3====")
        self.NTGOCCURS = 10

        self.TABELLA_SINGOLI = [ # 1421
            {
                "C-MAT" : None,
                "CONT" : None,
                "D-MAT" : None,
                "PREZZO" : None,
                "PRIMA-TG" : None,
                "SKU" : None
            }
        ]

        self.TRATTA_DEV() # 1047
        self.CARICA_B2C_NO_DT() # 1054
        self.TRATTA_IMPEGNATO() # 1061
        self.TRATTA_NEG() # 1063


    def TRATTA_DEV(self): # OK	# 1071
        while (True):
            print("Disp. USCITA >> ")
            try:
                DEV_IN = int(input())
            except KeyboardInterrupt:
                sys.exit(1)
            except:
                print("Dest. USCITA Err. ")
                continue
            
            self.DESTINO_VALIDO = [73, 90, 94, 27, 28, 29, 34, 22]
            if DEV_IN not in self.DESTINO_VALIDO:
                print("Dest. USCITA Err. ",DEV_IN)
                continue
            else:
                break
        self.DESTINO_USCITA = DEV_IN

    def CARICA_B2C_NO_DT(self): # 4246
        logging.debug("=== select sql_NEGOZIO_CATEG ===")
        rowset = self.session.execute(db.sql_NEGOZIO_CATEG)
        self.TAB_B2C_NO_DT = [r for r, in rowset]
        logging.debug(str(self.TAB_B2C_NO_DT))

    def TRATTA_IMPEGNATO(self): # 1090
        while (True):
            print("Si vuole eliminare impegnato ? (SI/NO)")
            try:
                self.DISIMPEGNA = input()
            except KeyboardInterrupt:
                sys.exit(1)
            if self.DISIMPEGNA not in ["SI","NO"]:
                print("Risposta errata")
                continue 
            else:
                break

    def TRATTA_NEG(self): # 1106
        while  (self.VERIF_NEG()): # 1110

            self.PREZZO_TOT = 0

            self.VERIF_MAG() # 1117
            self.F_V_INPUT = "V"  # 1121 Fallato o Validoself.VERIF_F_V()
            NEG_OK = self.VERIFICA_SOC() # 1140 OK
            if not NEG_OK:  return

            NEG_OK = self.CARICA_TAB_UNICO_DDT() # 1145 OK
            if not NEG_OK:  return
                        
            if (self.DISIMPEGNA == "SI"): # 1158
                self.CICLO_DISIMPEGNO() # OK

            self.INIZIA_TAB_ART() # 1162 OK
            self.INIZIA_TAB_SING() # 1163 OK
            self.TRATTA_OLD_NEW() # 1164   
            self.TRATTA_LETTI() # 1165

    def VERIF_NEG(self): # 1504
        while(True):
            print("CONTO cliente (8 cifre)")
            print("  (END/end=fine)")
            try:
                self.CONTO_IN = input()
            except KeyboardInterrupt:
                    sys.exit(1)
            if (self.CONTO_IN.lower() == 'end'):
                return False 
            try:
                self.CONTO_IN_R = int(self.CONTO_IN)
                self.NEG_IN = self.CONTO_IN[5:7]
            except:
                print("CODICE non numerico")
                continue
            data = self.session.execute(db.sql_SELECT_ANACON,{"CONTO_IN_R":self.CONTO_IN_R})
            
            # lettura REC-ANACON COPY YANACON
            if not data:
                print("Manca CLIENTE ")
                continue
            for rec in data:    # solo 1 rec e in formato dict
                rec = rec._mapping 
                break
            logging.debug(rec)
            # logging.debug(str(row2dict(rec)))
            
            self.REC_ANACON = {
                "CONTO" : rec['CONTO'], 
                "D-CONTO" : rec['D_CTO'],
                "FLAG-ANA-8" : True if rec['FLAG_8'] == 3 else False,
                "FLAG-ANA-9" : True if rec['FLAG_9'] == "B" else False
            }
            # lettura REC-ANACON COPY YANACON

            self.D_CONTO = self.REC_ANACON["D-CONTO"]
            self.D_CONTO_MEM = self.D_CONTO
            self.FLAG_ANACON = self.REC_ANACON["FLAG-ANA-8"]
            self.FLAG_DT_ESTERO = self.REC_ANACON["FLAG-ANA-9"]

            self.CAMPI_ANAGRAFICI = {
                "CONTO-IN": self.CONTO_IN,
                "D-CONTO-MEM": self.REC_ANACON["CONTO"],
                "D-CONTO-AGG-MEM": None,
                "INDIRIZZO-STD" : None,
                "INDIRIZZO-COM": None,                   
                "LOCALITA-COM": None,                       
                "CAP-COM": None,             
                "PROV-COM": None,                        
                "STATO-COM": None,                       
                "INDIRIZZO-C-COM": None,                   
                "LOCALITA-C-COM": None,                   
                "CAP-C-COM": None,                  
                "PROV-C-COM": None,
                "CONTO-FATTURA-MEM" : None                      
            }
            rec = self.LEGGI_IND() # 1547
            if rec == False:    return False
            self.MUOVI_IND(rec) # 1548
            self.MUOVI_CAP(rec)# 1549
            self.SCEGLI_CONTO_FATTURA(rec) # 1551
            self.CERCA_LISTINO() # 1551
            return True


    def LEGGI_IND(self):
        data = self.session.execute(db.sql_SELECT_INDIRIZ,{"CONTO_IN_R":self.CONTO_IN_R})
        
        # lettura REC-ANACON COPY YANACON
        if not data:
            print("Manca INDIRIZZO ")
            return False
        for rec in data:    # solo 1 rec e in formato dict
            rec = rec._mapping 
        logging.debug(rec)

        # REC-INDIRIZZI  COPY YINDIRIZ
        self.REC_INDIRIZZI = rec 
        # { # 1607
        #         "D-AGG" : " "*24,
        #         "STATO" : " "*4,
        #         "SIGLA-PROV" : [" "*2," "*2],
        #         "INDIRIZZO" : [" "*66," "*66],
        #         "CAP" : [0,0],
        #         "PRIORITA" : 0,
        #         "TELEFONO" : 0,
        #         "TELEX" : 0
        # }
        return rec

    def MUOVI_IND(self,rec):
        self.CAMPI_ANAGRAFICI["INDIRIZZO-STD"] = rec["INDZ_1"]
        try:
            logging.debug("{}".format(rec["INDZ_1"]))
            (INDIRIZZO,LOCALITA) = rec["INDZ_1"].split(";")
            logging.debug("{} {}".format(INDIRIZZO,LOCALITA))
            self.CAMPI_ANAGRAFICI["INDIRIZZO-COM"] = INDIRIZZO
            self.CAMPI_ANAGRAFICI["LOCALITA-COM"] = LOCALITA
            if rec["INDZ_2"]:
                logging.debug("{}".format(rec["INDZ_2"]))
                (INDIRIZZO1,LOCALITA1) = rec["INDZ_2"].split(";")
                logging.debug("{} {}".format(INDIRIZZO1,LOCALITA1))
                self.CAMPI_ANAGRAFICI["INDIRIZZO-C-COM"] = INDIRIZZO1
                self.CAMPI_ANAGRAFICI["LOCALITA-C-COM"] = LOCALITA1
            else:
                self.CAMPI_ANAGRAFICI["INDIRIZZO-C-COM"] = ""
                self.CAMPI_ANAGRAFICI["LOCALITA-C-COM"] = ""

        except Exception as e:
            logging.warning(e)
            logging.warning(str(self.CONTO_IN_R) + " Errore in MUOVI_IND")
        pass

    def MUOVI_CAP(self,rec):
        self.CAMPI_ANAGRAFICI["D-CONTO-AGG-MEM"] = rec["D_AGG"]
        self.CAMPI_ANAGRAFICI["CAP-COM"] = rec["CAP_1"]
        self.CAMPI_ANAGRAFICI["PROV-COM"] = rec["SGL_P_1"]
        self.CAMPI_ANAGRAFICI["STATO-COM"] = rec["ST"]
        self.CAMPI_ANAGRAFICI["CAP-C-COM"] = rec["CAP_2"]
        self.CAMPI_ANAGRAFICI["PROV-C-COM"] = rec["SGL_P_2"]


    def SCEGLI_CONTO_FATTURA(self,rec):
        self.CAMPI_ANAGRAFICI["CONTO-FATTURA-MEM"] = rec["TL"] if not rec["TL"] == 0 else rec["TX"]

    def CERCA_LISTINO(self):
        data = self.session.execute(db.sql_SELECT_LISTINO,{"CONTO_IN_R":self.CONTO_IN_R})
        
        # lettura REC-ANACON COPY YANACON
        if not data:
            print("Manca LISTINO ")
        for rec in data:    # solo 1 rec e in formato dict
            rec = rec._mapping 
            break
        self.LISTINO_MEM = rec["LIST"]
        self.DIVISA_MEM = rec["DVS"]


    def VERIF_MAG(self): # 3893
        while (True):
            print("MAG provenienza (3 cifre)")
            try:
                self.MAG_INPUT = input() # 756
                self.MAG_INPUT_R = int(self.MAG_INPUT)
            except KeyboardInterrupt:
                sys.exit(1)
            except:                
                print("MAG non numerico")
                continue
            if not self.MAG_INPUT_R in [1, 4, 6, 7, 852 , 853]:
                print("accettato MAG 1, 4, 6, 7, 852 o 853")
            else:
                break                 

    def VERIFICA_SOC(self): # 1171
        while (True):
            print("Soc >> (vuoto = tutti) ")
            try:
                self.SOCIETA_INPUT = input()
                if not self.SOCIETA_INPUT == "":
                    self.SOCIETA_INPUT_R = int(self.SOCIETA_INPUT)
                else:
                    self.SOCIETA_INPUT_R = None
            except KeyboardInterrupt:
                sys.exit(1)
            except:                
                print("Soc Err. ")
                continue 
            else:
                break
        return True  

    def CARICA_TAB_UNICO_DDT(self): # 1190
        # 4411
        # CALL "QTABELXL" USING PAR-TAB-UNICO-DDT 
        #                       TAB-UNICO-DDT 
        #                       DEP-TAB-UNICO-DDT
        
        while(True):
            while(True):
                try:
                    print("AS >> (tt=tutti) (elenco separato da ,) (CHIUDI)")
                    AS = input()
                except KeyboardInterrupt:
                    sys.exit(1)
                if AS == "CHIUDI":
                    break
                if AS == 'tt':
                    break
                else:
                    z = re.match(r"^\s*((\d\d)(\s*,\s*\d\d)*)\s*$",AS)
                    if not z:
                        print("Non specificato correttamente")
                        print("deve essere una sequenza di numeri di due cifre separati da una virgola (p.e.: 12,24)") 
                        continue
                    else:
                        break

            if AS == "CHIUDI":
                return False

            if not AS == "tt":
                AS = AS.split(",")
                AS = [_as_.strip() for _as_ in AS]
            else:
                AS = ["TUTTI"]

            while(True):
                try:
                    print("CL >> (elenco separato da ,)")
                    CL = input()
                except KeyboardInterrupt:
                    sys.exit(1)
                z = re.match(r"^\s*((\d\d)(\s*,\s*\d\d)*)\s*$",CL)
                if not z:
                    print("Non specificato correttamente")
                    print("deve essere una sequenza di numeri di due cifre separati da una virgola (p.e.: 01,02)") 
                    continue
                else:
                    break
            CL = CL.split(",")
            CL = [int(_cl_.strip()) for _cl_ in CL]

            while(True):
                try:
                    print("MaxCapi >> (vuoto = tutti)")
                    MaxCapi = input()                 
                except KeyboardInterrupt:
                        sys.exit(1)
                if MaxCapi == "":
                    MaxCapi = sys.maxint
                else:
                    try:
                        MaxCapi = int(MaxCapi)
                    except:                
                        print("MaxCapi non numerico")
                        continue
                break

            self.TAB_UNICO_DDT = OrderedDict()
            
            for _as_ in AS:
                for _cl_ in CL:
                    if _as_ == "TUTTI":
                        self.__TUTTI_AS__ = True
                        key = _cl_
                    else:
                        self.__TUTTI_AS__ = False
                        key = (_as_,_cl_)
                    self.TAB_UNICO_DDT[key] = { # 838
                        "MAX-CAPI" : MaxCapi,
                        "CAPI-LETTI" : 0, 
                        "magazzino" : self.MAG_INPUT,
                        "fornitore" : self.SOCIETA_INPUT
                        }
            print("-------------------------------")
            for k in self.TAB_UNICO_DDT:
                max_capi = self.TAB_UNICO_DDT[k]["MAX-CAPI"] if self.TAB_UNICO_DDT[k]["MAX-CAPI"] < sys.maxint else "Tutti"
                if self.__TUTTI_AS__:
                    print("AS = {} CL = {} MAX-CAPI = {}".format("TUTTI",k,max_capi))            
                else:
                    print("AS = {} CL = {} MAX-CAPI = {}".format(k[0],k[1],max_capi))            
            print("CORRETTI? 'SI' per proseguire 'NO' per rifare elenco AS CL da capo")
            while(True):
                try:
                    risposta = input()
                except KeyboardInterrupt:
                    sys.exit(1)
                except:                
                    if not risposta.lower() in ["si","no"]:
                        print("La risposta deve esser 'SI' o 'NO'")
                    continue 
                else:
                    break  
            if risposta == "SI":
                break
            else:
                continue
        return True

    def CICLO_DISIMPEGNO(self): # 4461
        # session = db_access_core.mysql_connect('reretis', echo=False)

        for _as__cl_ in self.TAB_UNICO_DDT:
            json_object = {
                'anni_stagioni': _as__cl_[0],
                'magazzino': self.MAG_INPUT,
                'fornitore': self.SOCIETA_INPUT,
                'classi': _as__cl_[1]
            }
            print(json_object)

            # 1394
            # i python che vengono chiamati dai cobol sono tutti nel repo intranet3/bin/4cobol/
            # disimpegna_capi.py del quale viene chiamata la functiona elimina_impegnati() 

            call_rest_api = ws_rest.Call_Rest_Api_DT(self.session, logging.getLogger(), 'put', 
                                        'magazzino_pf/gestione_impegni/DisimpegnaListaModelli',
                                        json_object, None)

            response_status = call_rest_api.response_code
            if response_status == 200 or response_status == 204:
                pass
            else:
                print("ERRORE DISIMPEGNO!!!")
                sys.exit(1)


    def INIZIA_TAB_ART(self): # 1402
        self.TABELLA_ARTICOLI_LETTI = {
            # "TAB-ART" : { # S9(15)
            #     "D-MAT" : None, # X(7)
            #     "PRIMA-TG" : None, # S9(4)
            #     "PREZZO" : None, # S9(9)
            #     "CAMBIO" : None, # S9(9)
            #     "QTA-GIAC" : [None]*self.NTGOCCURS, # S9(8)
            #     "QTA-TAGLIE" : [None]*self.NTGOCCURS, # S9(4)
            #     "COSTO" : None #S9(9)
            # }
        }

# 524
# 043200 01 TABELLA-ARTICOLI-LETTI.                                               
# 043300  03 ART-TAB-LETTI   OCCURS 5000.                                          
# 043400   05 TAB-ART        PIC S9(15) COMP-3.                                   
# 043500   05 D-MAT-TAB      PIC X(7).                                            
# 043600*BUDA*                                                                    
# 043700   05 PRIMA-TG-TAB     PIC S*9(4) COMP.                                    
# 043800   05 PREZZO-TAB       PIC S9(9) COMP.                                    
# 043900   05 CAMBIO-TAB       PIC S9(9) COMP.                                    
# 044000   05 TIPO-ANA-TAB  PIC XX.                                               
# 044100   05 QTA-GIAC-TAB.                                                       
# 044200     10 QTA-GIAC-PF-TAB  PIC S9(8) COMP   COPY NTGOCCURS. 
# 044300   05 QTA-TAGLIE-TAB.                                                     
# 044400     10 QTA-TAGLIA-TAB PIC S9(4) COMP    COPY NTGOCCURS. 
#       *VACO*                                                            
#          05 COSTO-TAB       PIC S9(9) COMP.

        self.TABELLA_NO_GIAC = [ # 561 
            {   "C_MAT" : None, 
                "PREZZO" : None,
                "D-MAT" : None,                                      
                "CAUSALE" : None,                                     
                "CAUSALE-NO-PRZ" : None                     
            }
        ]


    def INIZIA_TAB_SING(self): # 1421
# 039600  05 C-MAT-SING        PIC S9(15) COMP-3.
# 039500  05 CONT-SING         PIC S9(4) COMP.  
# 039700  05 D-MAT-SING        PIC X(7).                                          
# 039800  05 PREZZO-SING       PIC S9(9) COMP.                                    
# 039900  05 PRIMA-TG-SING     PIC S9(4) COMP. 
#       *MOVSKU
#         05 SKU-SING          PIC X(13).                                      
        self.ART_TAB_SING = []
        SING = {
           "C-MAT" : '123456789012345', # PIC S9(15) COMP-3
           "CONT" : '1234', 
           "D-MAT" :  '1234567',   # PIC S*9(4) COMP.
           "PREZZO" : 1, # S9(9)
           "PRIMA-TG" : 1, # S9(4)
           "SKU" : '1234567890123' # 13 
        }

    def TRATTA_OLD_NEW(self):
        print(self.D_CONTO_MEM)
        print("dal mag ",self.MAG_INPUT)
        self.TRATTA_SITPF_3()
        print(" S stampa rapportino")
        self.COD_IN = input().lower()
        if (self.COD_IN == 's'):
            rc = self.STAMPA_RAPPORTINO() # 1598
            print("   rapportino stampato")       

    def TRATTA_SITPF_3(self): # 1713
        
        data = self.conn.execute("select * from SITPF where MAG  = {} limit 1;".format(self.MAG_INPUT))
        self.IND_CAPI_LETTI = 0
        for rec_SITPF3 in data:
            # SELEZIONA-SITPF-3 1721 - 1781
            print(rec_SITPF3)

            # 15 C-MAT               PIC S9(15) COMP-3.                   YSITPF  
            # 15 MAGAZZINO           PIC S9(4)  COMP.                     YSITPF  
            # 15 QTA-GIAC.                                                YSITPF  
            #  20  QTA-GIAC-PF        PIC S9(8) COMP COPY NTGOCCURS.      YSITPF  
            # 15 VAL-GIAC            PIC S9(11) COMP-3.                   YSITPF  
            # 15 QTA-INV.                                                 YSITPF  
            #  20 QTA-INV-PF       PIC S9(8) COMP COPY NTGOCCURS.         YSITPF  
            # 15 VAL-INV             PIC S9(11) COMP-3.                   YSITPF  
            # 15 DT-UM               PIC S9(8)  COMP.                     YSITPF  
            # 15 DT-INV              PIC S9(8)  COMP.                     YSITPF  
            # 15 QTA-ORDINATA.                                            YSITPF  
            #  20 QTA-ORD           PIC S9(8) COMP COPY NTGOCCURS.        YSITPF  
            # 15 QTA-ORDINATA-C.                                          YSITPF  
            #  20  QTA-ORD-C        PIC S9(8) COMP COPY NTGOCCURS.        YSITPF  
            # 15 QTA-IMPEGNATA.                                           YSITPF  
            #  20  QTA-IMP          PIC S9(8) COMP COPY NTGOCCURS.        YSITPF  
            # 15 QTA-IMPEGNATA-C.                                         YSITPF  
            #  20  QTA-IMP-C        PIC S9(8) COMP COPY NTGOCCURS.        YSITPF  
            # 15 VAL-REC             PIC XX.                              YSITPF  
            #  88 BOX-SOSPESO   VALUE "S ".  




    #        15 C-MAT-TRANSITO.                                           DANCODMT
    #          20  MODELLO                  PIC 9(7).  0-6                   DANCODMT
    #          20 MOD-RID  REDEFINES MODELLO.                             DANCODMT
    #           25  COLLEZIONE              PIC 9.    0                    DANCODMT
    #           25  CLASSE                  PIC 99.   1-2                    DANCODMT
    #           25  STAGIONE                PIC 9.    3                    DANCODMT
    #           25  PROGR-MOD               PIC 99.   4-5                    DANCODMT
    #           25  ANNO                    PIC 9.    6                    DANCODMT
    #          20  ARTICOLO                 PIC 9(5). 7-11                    DANCODMT
    #          20 ART-RID  REDEFINES ARTICOLO.                            DANCODMT
    #           25 GR-MERC                  PIC 99. 7-8                       DANCODMT
    #           25 FILLER REDEFINES GR-MERC.                              DANCODMT
    #             30 VEST-A                 PIC 9.  7                      DANCODMT
    #             30 PEZZO-A                PIC 9.  8                      DANCODMT
    #           25 PROGR-ART                PIC 999. 9-11                      DANCODMT
    #           25 FILLER REDEFINES PROGR-ART.                            DANCODMT
    #            30 FILLER                  PIC 9.  9                      DANCODMT
    #            30 PREFISSO-V-F            PIC 9.  10                      DANCODMT
    #            30 SOCIETA-MOD             PIC 9.  11                      DANCODMT
    #          20  COLORE                   PIC 999. 12-15                      DANCODMT
    #   *                                                                 DANCODMT
    #        15 C-MAT-TRANS-RID REDEFINES C-MAT-TRANSITO PIC 9(15).       DANCODMT

            # CALCOLA-AS-CL 1753
            self.C_MAT_TRANS_RID = int(rec_SITPF3["C_MAT"])
            #print(rec_SITPF3["C_MAT"])
            #print(self.C_MAT_TRANS_RID)
            C_MAT = str(rec_SITPF3["C_MAT"])
            self.C_MAT = OrderedDict([
                ("MODELLO", int(C_MAT[0:6])),
                ("COLLEZIONE", int(C_MAT[0:1])),
                ("CLASSE"    , int(C_MAT[1:3])),
                ("STAGIONE"  , int(C_MAT[3:4])),
                ("PROGR-MOD" , int(C_MAT[4:6])),
                ("ANNO"      , int(C_MAT[6:7])),
                ("VEST-A"    , int(C_MAT[7:8])),
                ("PEZZO-A"   , int(C_MAT[8:9])),
                ("PROGR-ART"   , int(C_MAT[10:12])),
                ("PREFISSO-V-F"   , int(C_MAT[10:11])),
                ("SOCIETA-MOD"   , int(C_MAT[11:12])),
                ("COLORE"   , int(C_MAT[12:15]))
            ])
            # print(self.C_MAT)

            rec_anamat_ = self.session.execute(db.sql_SELECT_ANAMAT,{'C_MAT':self.C_MAT_TRANS_RID})
            for rec_anamat in rec_anamat_:    # solo 1 rec e in formato dict
                break
            _as_ = "{}{}".format(rec_anamat["ANNO"],rec_anamat["STAG"])
            _cl_ = rec_anamat["CL_GR"]

            try:
                if self.__TUTTI_AS__:   key = _cl_
                else:   key = (_as_,_cl_)
                self.DEP_TAB_UNICO_DDT =  self.TAB_UNICO_DDT[key] 
                print(key,self.DEP_TAB_UNICO_DDT)
            except: # on trovato
                continue
            if (self.DEP_TAB_UNICO_DDT["MAX-CAPI"] == self.DEP_TAB_UNICO_DDT["CAPI-LETTI"]):

                # check se raggiunto il massimo su TUTTI TAB_UNICO_DDT
                raggiunto_max_tutti_AS_CL = True

                for key in self.TAB_UNICO_DDT:
                    raggiunto_max_tutti_AS_CL = self.TAB_UNICO_DDT[key]["MAX-CAPI"] == self.TAB_UNICO_DDT[key]["CAPI-LETTI"]
                    if not raggiunto_max_tutti_AS_CL:    break

                if raggiunto_max_tutti_AS_CL:
                    break # esco da for su SITPF3
                else:
                    continue # raggiunto SOLO MAX per specifico AS CL :continuo for su SITPF3
                

            if not (self.SOCIETA_INPUT_R == None)\
                 and not (self.SOCIETA_INPUT_R == int(self.C_MAT["SOCIETA-MOD"]) ): # SOCIETA-MOD 
                    print("{} {} non uguale societa'".format(self.SOCIETA_INPUT ,self.C_MAT["SOCIETA-MOD"]))
                    continue

            # 1853
            for IT in range(1,self.NTGOCCURS+1):
                if self.DISIMPEGNA == "SI":
                    self.DA_TRASFERIRE = rec_SITPF3["GQF%d" % IT]
                else:
                    self.DA_TRASFERIRE = rec_SITPF3["GQF%d" % IT] + rec_SITPF3["QIF%d" % IT]

                for IC in range(1,self.DA_TRASFERIRE+1):
                    self.C_MAT_TRANS_RID = int(rec_SITPF3["C_MAT"])
                    
                    #    15 C-MAT-A-BARRE.                                            DANCODBC
                    #     20 MODELLO               PIC 9(7).                          DANCODBC
                    #     20 MOD-RID REDEFINES MODELLO.                               DANCODBC
                    #      25 MARCHIO              PIC 9.                             DANCODBC
                    #      25 CLASSE               PIC 99.                            DANCODBC
                    #      25 STAGIONE             PIC 9.                             DANCODBC
                    #      25 PROGR-ART            PIC 99.                            DANCODBC
                    #      25 ANNO                 PIC 9.                             DANCODBC
                    #     20 VESTIBILITA           PIC 9.                             DANCODBC
                    #     20 SOCIETA               PIC 99.                            DANCODBC
                    #     20 FILLER REDEFINES SOCIETA.                                DANCODBC
                    #      25 PREFBC-V-F           PIC 9.                             DANCODBC
                    #      25 SOC-BC-MOD           PIC 9.                             DANCODBC
                    #     20 PEZZO                 PIC 9.                             DANCODBC
                    #     20 VARIANTE-COL          PIC 99.                            DANCODBC
                    #     20 TAGLIA                PIC 9.                             DANCODBC
                    #    15 C-MAT-A-BARRE-RID REDEFINES C-MAT-A-BARRE  PIC 9(14).     DANCODBC

                    self.NTG_MEM = IT
                    self.C_MAT_A_BARRE = lib_dt.conv_trans_barc(rec_SITPF3["C_MAT"], self.NTG_MEM)

                    self.TRATTA_LEGGI(rec_SITPF3)

    def TRATTA_LETTI(self):	# 2413
        while (True):
            self.TOT_CAPI_LETTI_1 = self.IND_CAPI_LETTI
            print("- Tot CAPI - ",self.TOT_CAPI_LETTI_1)
            self.TOT_CAPI_NO_GIAC = self.IND_CAPI_NO_GIAC
            print("- No GIAC./PREZZO - ",self.TOT_CAPI_NO_GIAC)
            print(" ")
            print("Vuoi STORNARE ","(SI-NO)")
            self.CONFERMA_STORNO = input()

            if (self.CONFERMA_STORNO.lower() == "si")\
                and not self.TOT_CAPI_LETTI_1 == 0:
                
                while(True):
                    print("Dammi il CODICE") # CPY DANCODMT
                    print(" . fine lettura")
                    print(" @ storno totale")
                    self.COD_IN = input()
                    if self.COD_IN == ".":  break
                    if not (self.COD_IN == "@"):
                        try:
                            int(self.COD_IN)
                        except:
                            print("COD non num >> RILEGGERE")
                            continue
                        self.TRATTA_STORNO()

                    if not self.TABELLA_SINGOLI:
                        break
                    
                    self.LEGGI_PARAMDT()
                    self.INDIRIZZO_DPARAM = "..." # 1454

                    self.AGG_DPARAM()
                    
                    print("Aggiorno  ")
                    print("BOLLA n.  %s" % self.NUMERO_DDT)   

                    if (len(self.PARTAB_SING) > 0):
                        for self.IND_BARUNI in range(len(self.PARTAB_SING)):
                             self.INSERISCI_MOVSKU()   

                    FILE_VAR_VALUE_TEMPLATE = "%(USER_VAR_VALUE)s_BC_%(AA_MM_GG_DDT)s%(NUMERO_DDT)s_%(MAG_INPUT_R)s%(NEG_IN)s_B"  
                    FILE_VAR_VALUE = FILE_VAR_VALUE_TEMPLATE % {
                        "USER_VAR_VALUE" : "",
                        "AA_MM_GG_DDT" : "",
                        "NUMERO_DDT" : self.NUMERO_DDT,
                        "MAG_INPUT_R" : self.MAG_INPUT,
                        "NEG_IN" : self.NEG_IN
                    }
                    print(FILE_VAR_VALUE)
                    FILE_BC = open(FILE_VAR_VALUE,"w")
                    for rec in self.TABELLA_ARTICOLI_LETTI:
                        self.SCRIVI_RECORD()
    
                    # 2531 
                    if self.REC_INDIRIZZI["PRIORITA"] == "4":
                        self.INTESTA_FILE_BC() # 2533
                        for SING in self.TABELLA_SINGOLI:
                            self.SCORRI_TAB_SING(SING)
                    else:
                        self.SCORRI_TB() # 2542

                    self.SCRIVI_BOLLE() # 2545
                    self.CHIAMA_PRINTDDT() # 2546

                    if self.REC_INDIRIZZI["PRIORITA"] == "4":
                        self.PRTBCEU8() # 2561

                    if self.IND_CAPI_NO_GIAC: # > 0
                        self.STAMPA_NO_GIAC()

                    if not self.FLAG_ANACON in ['0',' ']:
                         self.ALLINEA_BOLLE_ESTERO()

                    self.AVANZA_DDT()
                print("premi un tasto per continuare...")
                input()


    def ALLINEA_BOLLE_ESTERO(self): # 2587
        pass 
    # CALL  "PYTHON"
    #        USING "allinea_bolle_in_estero"
    #              "allinea_estero"

    def AVANZA_DDT(self): # 2606
        pass
        # CALL "PYTHON" USING "avanzamento" 
        #                      "genera_avanzamento" 
        #                        PY-INPUT-REC-B
        #                        PY-OUTPUT-REC-B.


    def INTESTA_FILE_BC(self): # 3333
        pass
    
    def SCORRI_TAB_SING(self,SING): # 3291
        pass
    
    def SCORRI_TB(self): # 2948
        for SING in self.TABELLA_SINGOLI:
            self.SCORRI_TB_SING(SING) # 2971

    def SCORRI_TB_SING(self,SING): # 2982
        pass

    def SCRIVI_BOLLE(self): # 2852
        pass

    def CHIAMA_PRINTDDT(): # 3671 
        pass

    def PRTBCEU8(self):
        pass # Chiamata esterna  modulo python tabprt2single

    def STAMPA_NO_GIAC(self): # 2660
        pass # call cobol QWLPPR 2686



    def SCRIVI_RECORD(self):
        # CPY DANCODMT  C-MAT-TRANSITO
        # 3377-3384
        # INVERTI-QTA 3413
        self.TABELLA_ARTICOLI_LETTI[QTA_TAGLIA] = [ -tg for tg in self.TABELLA_ARTICOLI_LETTI[QTA_TAGLIA]]
        # PREPARA-MOVMAG 3744 , CPY YMOVMAG
        # PERFORM PREPARA-MOVMAG # 404
        self.CREA_MOVMAG_P_3() # 3405
        self.AGGIORNA_SITPF_P_3() # 3407
    
    def CREA_MOVMAG_P_3(self): # 3776
        pass
    
    def AGGIORNA_SITPF_P_3(self): # 3812
        pass


 
    def LEGGI_PARAMDT(self):
        rec_= self.session.execute(db.sql_SELECT_DPARAM,{})
        for rec in rec_:    # solo 1 rec e in formato dict
            rec = rec_._mapping 
            break        
        self.NUMERO_DDT = rec["NUMERO_3"]

    def AGG_DPARAM(self):
        NUOVO_NUMERO_DDT = self.NUMERO_DDT + 1
        self.conn.execute(db.sql_UPDATE_DPARAM,{"NUM-BOLLA-TAGLIO-FODERE":NUOVO_NUMERO_DDT})


    def GETBARUNI(self):
        pard = "%s;%s;READVE3" % (self.C_MAT_A_BARRE,self.RIF_BOLLA_DDT)
        try:
            p = lib_anag_sku.Pop_ANAG_SKU(pard, self.conn)
            p.DEBUG = False 
            p.logger = logging.getLogger()

            if not len(p.list_anag_sku) > 0:
                raise Exception('nessuna lista sku restituita')
            anag_sku = p.list_anag_sku[0]
            MOVSKU_BARUNI = anag_sku.baruni
            return MOVSKU_BARUNI
        except Exception as exc:
            err = str(lib_exception.Build_Traceback(sys.exc_info()))
            logging.getLogger().critical(err)
            sys.exit(1)

    def INSERISCI_MOVSKU(self):
        TAGLIA = self.COD_IN[14]
        MOVSKU_BARUNI = self.GETBARUNI()

        params = {
                      "SQL_MOVSKU-RIF-INTERNO" : self.NUMERO_DDT,
                      "MOVSKU-CMAT" : self.COD_IN,
                      "MOVSKU-TG" : common_fun.idxtg(TAGLIA),
                      "MOVSKU-BARUNI" : MOVSKU_BARUNI,
                      "MOVSKU-CONTO" : self.CONTO_IN,                                                                     
                      "MOVSKU-MAG" : self.MAG_INPUT,
                      "MOVSKU-SKU" : MOVSKU_BARUNI[0:8],
                      "MOVSKU-IS-BARUNI-READ" : 0,
                      "MOVSKU-IS-BARUNI-CERTIFIED" : 0,
                      "MOVSKU-SKU-FATTURAZIONE"  : MOVSKU_BARUNI[0:8]         
        }
        
        data = self.conn.execute(db.sql_INSERT_MOV_SKU,params)

    def TRATTA_STORNO(self):
        TAGLIA = self.COD_IN[14]
        TAGLIA = common_fun.idxtg(TAGLIA)
        
        # SOC_COM =  self.COD_IN[8:10]
        # SOC_COM[0] = "0"
        # self.COD_IN[8:10] = SOC_COM
        self.COD_IN[8] = "0"
        ELEM_ART = self.COD_IN[:len(self.COD_IN)-1] # toglie la taglia in cbl:  / 10
        if not (ELEM_ART in self.TABELLA_ARTICOLI_LETTI):
            print("Manca lettura ")
            return
        self.TABELLA_ARTICOLI_LETTI[ELEM_ART]["QTA_TAGLIA"][TAGLIA] -= 1
        if (self.TABELLA_ARTICOLI_LETTI[ELEM_ART]["QTA_TAGLIA"][TAGLIA] < 0):
            print("Taglia non stornabile" )
            return
        # 2924 PERFORM DELETE-ELEM-SING THRU EX-DELETE-ELEM-SING.
        del self.TABELLA_SINGOLI[self.COD_IN]

        print("ancora %s in %s"  % (self.TABELLA_ARTICOLI_LETTI[ELEM_ART]["QTA_TAGLIA"][TAGLIA], self.COD_IN[8]))

    def TRATTA_LEGGI(self,rec_SITPF3,NTG_MEM):
        # 1945
        self.C_MAT_A_BARRE = lib_dt.conv_trans_barc(rec_SITPF3["C_MAT"], NTG_MEM)
        KEY_ELEM_ART = self.C_MAT_A_BARRE[:len(self.C_MAT_A_BARRE)-1]
        
        # Check esistenza in self.TABELLA_ARTICOLI_LETTI
        QT_STATO_TROVATO = KEY_ELEM_ART in self.TABELLA_ARTICOLI_LETTI
        
        if not QT_STATO_TROVATO: # Non presente in self.TABELLA_ARTICOLI_LETTI
            data = self.session.execute(db.sql_SELECT_ANAMAT,{'C_MAT':self.C_MAT_A_BARRE})
            if not data: # IF B2C-NO-DT
                print("Inesist. %s" % self.C_MAT_A_BARRE) 
                return
            for rec_ in data:    # solo 1 rec e in formato dict
                rec_ANAMAT = rec_._mapping 
                break    

        if not QT_STATO_TROVATO:  # Non presente in self.TABELLA_ARTICOLI_LETTI
            self.D_MAT_MEM  = rec_ANAMAT["D_MAT"]
            self.PTG_MEM = rec_ANAMAT["P_TG"]

            self.VAL_REC_MEM  = rec_ANAMAT["VAL_REC"]
            self.COSTO_MEM = rec_ANAMAT["CST_STD"]

            self.ANACST_C_MAT_COM = rec_ANAMAT["C_MAT"]
            self.ANACST_MAG_COM = self.MAG_INPUT

            self.RIVALUTA_COSTO_ANAMAT() # 2020
            if not self.ANACST_CST_COM == 0:
                self.COSTO_MEM = self.ANACST_CST_COM

        RISP_NO_GIAC =  RISP_NO_PREZZO = ""
        self.PREZZO_MEM = 0
        if not QT_STATO_TROVATO and self.REC_INDIRIZZI["QT_STATO"] == 4:
            rc = self.CERCA_PREZZO_V()
            if not rc: 
               return 
        if not QT_STATO_TROVATO:
            self.PREZZO_MEM = self.PREZZO_ANAMAT = 0
            self.CAMBIO_MEM = 0
            rc = self.CERCA_PREZZO() # 2072
            if not rc: 
               return 
            if self.PREZZO_MEM == 0 or \
                (self.PREZZO_MEM == self.PREZZO_ANAMAT and not self.FLAG_DT_ESTERO == 1):
                self.PREZZO_OK = 0
                # self.TRATTA_NO_PREZZO() # 2084 2299
                RISP_NO_PREZZO = "S"
                self.PREZZO_OK = 1
                if not RISP_NO_PREZZO == "S": # 2086
                    return
                # 2090
                if not self.PREZZO_MEM == 0:
                    self.PREZZO_MEM = 0
        else:
            ELEM_ART = self.TABELLA_ARTICOLI_LETTI[KEY_ELEM_ART]
            self.CAMBIO_MEM = ELEM_ART["CAMBIO"]
            self.D_MAT_MEM = ELEM_ART["D-MAT"]
            self.PREZZO_MEM = ELEM_ART["PREZZO"]

        
        if not QT_STATO_TROVATO:
            QTA_GIAC = []
            for IT in range(1,self.NTGOCCURS+1):
                QTA_GIAC.append(rec_SITPF3["GQF%d" % IT])    

            ELEM_ART = {
                "D-MAT" : self.D_MAT_MEM, # X(7)
                "PRIMA-TG" : self.PTG_MEM, # S9(4)
                "PREZZO" : self.PREZZO_MEM, # S9(9)
                "CAMBIO" : self.CAMBIO_MEM, # S9(9),
                "TIPO-ANA" : self.VAL_REC_MEM,
                "QTA-GIAC" : QTA_GIAC, # S9(8)
                "QTA-TAGLIE" : [None]*self.NTGOCCURS, # S9(4)
                "COSTO" : self.COSTO_MEM #S9(9)
            } 
        else:
            ELEM_ART = self.TABELLA_ARTICOLI_LETTI[KEY_ELEM_ART]
            self.D_MAT_MEM = ELEM_ART["D-MAT"]
            self.PTG_MEM = ELEM_ART["PRIMA-TG"]

        PREZZO_D = self.PREZZO_MEM / 100
        ELEM_ART["QTA-TAGLIE"][self.NTG_MEM] += 1 
        if rec_SITPF3["GQF%d" % IT]  <  ELEM_ART["QTA_TAGLIE"][NTG_MEM] :
            print("Manca giac %s" %  self.C_MAT_A_BARRE)

            RISP_NO_GIAC = self.TRATTA_NO_GIAC(ELEM_ART["D-MAT"],PREZZO_D) # 2142
            if not RISP_NO_GIAC == "S":
                return
            
            if RISP_NO_GIAC == "S" or ( self.RISP_NO_PREZZO == "S" and  self.FLAG_DT_ESTERO == 1) :
                self.INSERISCI_NO_GIAC_PREZZO(RISP_NO_GIAC) # 2154
                return

        self.IND_CAPI_LETTI += 1
        self.PREZZO_TOT +=  self.PREZZO_MEM
        self.PREZZO_D = self.PREZZO_MEM / 100

        self.DEP_TAB_UNICO_DDT["CAPI-LETTI"] += 1 # 2165
        # 2190 RIMETTI-DEP-TAB-UNICO-DDT
        
        # 2334 inserimento in TABELLA_SINGOLI
        ELEMENTO_SINGOLI = {               
            "C-MAT" : self.C_MAT_A_BARRE,
            "CONT" : self.CONTO_IN,
            "D-MAT" : self.D_MAT_MEM,
            "PREZZO" : self.PREZZO_MEM,
            "PRIMA-TG" : self.PTG_MEM,
            "SKU" : None
        }
        self.TABELLA_SINGOLI.append(ELEMENTO_SINGOLI)
    
        if not QT_STATO_TROVATO:
            self.TABELLA_ARTICOLI_LETTI[KEY_ELEM_ART] = ELEM_ART


    def TRATTA_NO_GIAC(self,D_MAT,PREZZO_D): # 2273
        print("%s %s" % (D_MAT,PREZZO_D))
        print("CONFERMI MANCA GIAC ? (S/N)")
        print("N")
        RISP_NO_GIAC = "N"
        return RISP_NO_GIAC


    def INSERISCI_NO_GIAC_PREZZO(self,RISP_NO_GIAC): # 2243
        IND_CAPI_NO_GIAC = {}
        if RISP_NO_GIAC == "S":
            print("INSERITO Manca GIAC.")
            IND_CAPI_NO_GIAC["CAUSALE"] = "MancaGIAC"
        if self.RISP_NO_PREZZO == "S" and  self.FLAG_DT_ESTERO == 1:
            print("INS. Manca PREZZO x ESTERO")
            IND_CAPI_NO_GIAC["CAUSALE-NO-PRZ"] = "MancaPRZ"
        
        IND_CAPI_NO_GIAC["C-MAT"] = self.C_MAT_A_BARRE
        IND_CAPI_NO_GIAC["D-MAT"] = self.D_MAT_MEM
        IND_CAPI_NO_GIAC["PREZZO"] = self.PREZZO_MEM

        self.TABELLA_NO_GIAC.append(IND_CAPI_NO_GIAC)
        
    def CERCA_PREZZO(self):
        # 3420
        C_MAT = '%15d' % self.C_MAT_TRANS_RID
        C_MAT = C_MAT[0:len(C_MAT)-3] + '000'
        data = self.session.execute(db.sql_SELECT_PREZZO,{'C_MAT':C_MAT})
        if not data: 
            print("Inesist. col 0  %s" % C_MAT) 
            return False
        for rec_ in data:    # solo 1 rec e in formato dict
            rec = rec_._mapping 
            break    
        self.PREZZO_ANAMAT = rec["COSTO"]

        if self.MAG_INPUT in [1, 4, 6, 7, 852, 853]:
            self.CHIAMA_DTVALSTK()
        else:
            self.CERCA_PREZZIA()

        return True

    def CHIAMA_DTVALSTK(self):
        # 3442        
        if self.MAG_INPUT_R in [1,6]:
            self.MAG_FALLATO = True
        else:
            self.MAG_FALLATO = False
        if self.MAG_FALLATO:
            VC_NOME = "STF"
        else:
            if self.F_V_INPUT == "F":
                VC_NOME = "STF"
            else:
                VC_NOME = "STV"

        # DTVALSTK 219
        # MOVE VC-NOME(3:1)  TO INPUT-VAL-TIPO-PREZZO.
        INPUT_VAL_TIPO_PREZZO = VC_NOME[2:3]
        #    STRING '100',
        #          VC-NOME(4:5)  INTO INPUT-VAL-CONTO.
        # 3461
        # MOVE CONTO-IN-R TO NOME-IN-5.
        #  
        INPUT_VAL_CONTO = '100' + self.CONTO_IN[3:]
        cobol_input = "%15s%8s%1s" % (self.C_MAT,INPUT_VAL_CONTO,INPUT_VAL_TIPO_PREZZO)
        out = get_prezzo_stock.prezzo_stock({'cobol_input':cobol_input})["cobol_output"]
        self.PREZZO_MEM = int(out[2:11])
        self.CAMBIO_MEM = int(out[11:20])

    def CERCA_PREZZIA(self):
        # 3502
        C_MAT = '%15d' % self.C_MAT_TRANS_RID
        C_MAT = C_MAT[0:len(C_MAT)-3] + '000'
        data = self.session.execute(db.sql_SELECT_PREZZIA,{'C_MAT':C_MAT})
        if not data: 
            print("Inesist. col 0  %s" % C_MAT) 
            return False
        for rec_ in data:    # solo 1 rec e in formato dict
            rec = rec_._mapping 
            break    
        self.PREZZO_MEM = rec["PRZ_V_1"]
        self.CAMBIO_MEM = 0
        return True

    def CERCA_PREZZO_V(self):
        data = self.session.execute(db.sql_SELECT_PREZZO_V,{'C_MAT':self.C_MAT_A_BARRE})
        if not data: 
            print("Inesist. %s" % self.C_MAT_A_BARRE) 
            return False
        for rec_ in data:    # solo 1 rec e in formato dict
            rec = rec_._mapping 
            break    
        self.PREZZO_MEM = rec["PRZ_V_1"]
        return True
    
    def RIVALUTA_COSTO_ANAMAT(self):
        self.CERCA_B2C_NO_DT()
        self.RICERCA_COSTO_ANAMAT()

    def CERCA_B2C_NO_DT(self): # 4309
        if self.ANACST_MAG_COM  in self.TAB_B2C_NO_DT:
            self.FLAG_B2C_NO_DT = True
        else:
            self.FLAG_B2C_NO_DT = False

    def RICERCA_COSTO_ANAMAT(self): # 4320
        ANACST_C_MAT = self.ANACST_C_MAT_COM
        data = self.session.execute(db.sql_SELECT_ANAMAT_CST,{'ANACST_C_MAT':ANACST_C_MAT})
        if not data: # IF B2C-NO-DT
            print("Inesist. %s" % self.C_MAT_A_BARRE) 
            return
        for rec_ in data:    # solo 1 rec e in formato dict
            rec = rec_._mapping 
            break    
        if self.FLAG_B2C_NO_DT:
            self.ANACST_CST_COM = rec["CST_STD_2"]
        else:
            self.ANACST_CST_COM = rec["CST_STD"]

    def STAMPA_RAPPORTINO(self):
        RAPPRAI3.RAPPRAI3(self.TABELLA_ARTICOLI_LETTI,self.TABELLA_NO_GIAC)
          # 3883
#         246100     CALL "RAPPRAI3" USING W-COMMON SQLCA                                 
# 246200                           TABELLA-ARTICOLI-LETTI PARTAB-ART              
# 246300                           CONTO-IN-R D-CONTO-MEM                         
# 246400                           TABELLA-NO-GIAC IND-CAPI-NO-GIAC               
# 246500*MAG6/7*                                                                  
# 246600                           MAG-INPUT-R.      
        
        pass

if __name__ == "__main__":
    program = Program(loglevel='debug')
    program.READVE3()   
