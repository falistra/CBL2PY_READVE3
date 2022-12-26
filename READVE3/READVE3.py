import sys
import re

import ws_rest
import db_access_core
import lib_dt
import lib_anag_sku
import lib_exception

import RAPPRAI3
import db
import common_fun
import logging
from logging import config
from LoggingConfig import log_config      # configurazione del logger
config.dictConfig(log_config)

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

        pass

    def READVE3(self):
        logging.debug("====INIZIO READVE3====")

        self.TABELLA_ARTICOLI_LETTI = {
            "ART" : {
                "QTA_TAGLIA" : [0]*10,
                "PREZZO-ELEM" : 0
            }
        }
        self.TABELLA_SINGOLI = {}

        self.TRATTA_DEV() # OK
        self.CARICA_B2C_NO_DT() # OK
        self.TRATTA_IMPEGNATO() # OK
        self.TRATTA_NEG()


    def TRATTA_DEV(self): # OK	# Linea Source Cobol: 2897
        while (True):
            print("Disp. USCITA >> ")
            try:
                self.DEV_IN = int(input())
            except KeyboardInterrupt:
                sys.exit(1)
            except:
                print("Dest. USCITA Err. ")
                continue
            if self.DEV_IN not in self.DESTINO_VALIDO:
                print("Dest. USCITA Err. ",self.DEV_IN)
                continue
            else:
                break

    def CARICA_B2C_NO_DT(self): # OK
        logging.debug("=== select sql_NEGOZIO_CATEG ===")
        self.TAB_B2C_NO_DT = self.session.execute(db.sql_NEGOZIO_CATEG)
        logging.debug(str(self.TAB_B2C_NO_DT))
        # 01 TAB-B2C-NO-DT.
        #   05 EL-B2C-NO-DT  OCCURS 100.
        #     10 MAG-B2C-NO-DT  PIC S9(4) COMP.

    def TRATTA_IMPEGNATO(self): # OK
        while (True):
            print("Si vuole eliminare impegnato ?(SI/NO)")
            try:
                self.DISIMPEGNA = input()
            except KeyboardInterrupt:
                sys.exit(1)
            if self.DISIMPEGNA not in ["SI","NO"]:
                print("Risposta errata")
                continue 
            else:
                break

    def TRATTA_NEG(self):
        while  (self.VERIF_NEG()): # OK
            self.VERIF_MAG() # OK
            self.F_V_INPUT = "V"  # self.VERIF_F_V()
            self.VERIFICA_SOC() # OK
            self.CARICA_TAB_UNICO_DDT() # OK
            if (self.DISIMPEGNA == "SI"):
                self.CICLO_DISIMPEGNO() # OK
            self.INIZIA_TAB_ART() # OK
            self.INIZIA_TAB_SING() # OK
            self.TRATTA_OLD_NEW()
            self.TRATTA_LETTI()

    def VERIF_NEG(self):
        while (True):
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
                self.NEG = self.CONTO_IN[5:7]
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
            self.FLAG_ANACON = self.REC_ANACON["FLAG-ANA-8"]
            self.FLAG_DT_ESTERO = self.REC_ANACON["FLAG-ANA-9"]

            self.CAMPI_ANAGRAFICI = {
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
            rec = self.LEGGI_IND()
            if rec == False:    return False
            self.MUOVI_IND(rec)
            self.MUOVI_CAP(rec)
            self.SCEGLI_CONTO_FATTURA(rec)
            self.CERCA_LISTINO()
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
        self.REC_INDIRIZZI = { # 1607
                "D-AGG" : " "*24,
                "STATO" : " "*4,
                "SIGLA-PROV" : [" "*2," "*2],
                "INDIRIZZO" : [" "*66," "*66],
                "CAP" : [0,0],
                "PRIORITA" : 0,
                "TELEFONO" : 0,
                "TELEX" : 0
        }
        return rec

    def MUOVI_IND(self,rec):
        self.CAMPI_ANAGRAFICI["INDIRIZZO-STD"] = rec["INDZ_1"]
        try:
            (INDIRIZZO,LOCALITA) = rec["INDZ_1"].split(";")
            self.CAMPI_ANAGRAFICI["INDIRIZZO-COM"] = INDIRIZZO
            self.CAMPI_ANAGRAFICI["LOCALITA-COM"] = LOCALITA
            (INDIRIZZO1,LOCALITA1) = rec["INDZ_2"].split(";")
            self.CAMPI_ANAGRAFICI["INDIRIZZO-C-COM"] = INDIRIZZO1
            self.CAMPI_ANAGRAFICI["LOCALITA-C-COM"] = LOCALITA1
        except:
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


    def VERIF_MAG(self):
        while (True):
            print("MAG provenienza (3 cifre)")
            try:
                self.MAG_INPUT = int(input())
            except KeyboardInterrupt:
                sys.exit(1)
            except:                
                print("MAG non numerico")
                continue
            if not self.MAG_INPUT in [1, 4, 6, 7, 852 , 853]:
                print("accettato MAG 1, 4, 6, 7, 852 o 853")
            else:
                break                 

    def VERIFICA_SOC(self):
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

    def CARICA_TAB_UNICO_DDT(self):
        #   MOVE   16 TO QT-LL-ELEM       OF PAR-TAB-UNICO-DDT.
        #   MOVE    4 TO QT-LL-KEY        OF PAR-TAB-UNICO-DDT.
        #   MOVE    1 TO QT-ADDR-KEY      OF PAR-TAB-UNICO-DDT.
        #   MOVE 1980 TO QT-NUM-ELEM-MAX  OF PAR-TAB-UNICO-DDT.
        #   MOVE    0 TO QT-NUM-ELEM-EFF  OF PAR-TAB-UNICO-DDT.

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
                    continue
                elif AS == 'tt':
                    AS = [None]
                else:
                    z = re.match(AS,r"\s*((\d\d)(\s*,\s*\d\d)*)")
                    if not z:
                        print("Non specificato correttamente")
                        print("deve essere una sequenza di numeri di due cifre separati da una virgola (p.e.: 12,24)") 
                        continue
                    else:
                        break
            AS = AS.split(",")
            AS = [_as_.strip() for _as_ in AS]
                
            while(True):
                try:
                    print("CL >> (elenco separato da ,)")
                    CL = input()
                except KeyboardInterrupt:
                    sys.exit(1)
                z = re.match(CL,r"\s*((\d\d)(\s*,\s*\d\d)*)")
                if not z:
                    print("Non specificato correttamente")
                    print("deve essere una sequenza di numeri di due cifre separati da una virgola (p.e.: 01,02)") 
                    continue
                else:
                    break
            CL = CL.split(",")
            CL = [_cl_.strip() for _cl_ in CL]

            while(True):
                try:
                    print("MaxCapi >> (vuoto = tutti)")
                    MaxCapi = input()                 
                except KeyboardInterrupt:
                        sys.exit(1)
                if MaxCapi == "":
                    MaxCapi = None
                else:
                    try:
                        MaxCapi = int(MaxCapi)
                    except:                
                        print("MaxCapi non numerico")
                        continue
                break

            for _as_ in AS:
                self.TAB_UNICO_DDT[_as_] = {}
                for _cl_ in CL:
                    self.TAB_UNICO_DDT[_as_][_cl_] = {
                        "MAX-CAPI" : MaxCapi,
                        "CAPI-LETTI" : 0, 
                        "magazzino" : self.MAG_INPUT,
                        "fornitore" : self.SOCIETA_INPUT
                        }

            # pretty = json.dumps(self.TAB_UNICO_DDT,indent=4)
            # print(pretty)

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

    def CICLO_DISIMPEGNO(self):
        session = db_access_core.mysql_connect('reretis', echo=False)

        for _as_ in self.TAB_UNICO_DDT:
            for _cl_ in self.TAB_UNICO_DDT[_as_]:
                json_object = {
                    'anni_stagioni': [_as_],
                    'magazzino': self.MAG_INPUT,
                    'fornitore': self.SOCIETA_INPUT,
                    'classi': [_cl_]
                }
                print(json_object)
                call_rest_api = ws_rest.Call_Rest_Api_DT(session, logging.getLogger(), 'put', 
                                            'magazzino_pf/gestione_impegni/DisimpegnaListaModelli', json_object, None)

                response_status = call_rest_api.response_code
                if response_status == 200 or response_status == 204:
                    pass
                else:
                    print("ERRORE DISIMPEGNO!!!")
                    sys.exit(1)


    def INIZIA_TAB_ART(self):
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
        pass

    def INIZIA_TAB_SING(self):
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
            rc = self.STAMPA_RAPPORTINO()
            print("   rapportino stampato")       

    def TRATTA_SITPF_3(self):
        data = self.conn.execute(db.sql_SITPF3,{"MAG_INPUT":self.MAG_INPUT})
        for rec in data:
            rec = rec._mapping
            self.C_MAT_TRANS_RID = rec["C_MAT"]
            rec_anamat_ = self.session.execute(db.sql_SELECT_ANAMAT,{'C_MAT':self.C_MAT_TRANS_RID})
            for rec_anamat in rec_anamat_:    # solo 1 rec e in formato dict
                rec_anamat = rec_anamat._mapping 
                break
            _as_ = "%s%s" % rec_anamat["ANNO"],rec_anamat["STAG"]
            _cl_ = rec_anamat["CL_GR"]
            try:
                capi =  self.TAB_UNICO_DDT[_as_][_cl_] 
            except: # on trovato
                continue
            if (capi["MAX_CAPI"] == capi["CAPI-LETTI"]):
                raggiunto_max_tutti_AS_CL = True
                for _as_ in self.TAB_UNICO_DDT:
                    for _cl_ in self.TAB_UNICO_DDT[_as_]:
                        raggiunto_max_tutti_AS_CL = self.TAB_UNICO_DDT[_as_][_cl_]["MAX_CAPI"] == self.TAB_UNICO_DDT[_as_][_cl_]["CAPI-LETTI"]
                        if not raggiunto_max_tutti_AS_CL:    break
                    if not raggiunto_max_tutti_AS_CL:    break
                if raggiunto_max_tutti_AS_CL:
                    break # esco da for su SITPF3
                else:
                    continue # raggiunto SOLO MAX per specifico AS CL :continuo for su SITPF3

            if not (self.SOCIETA_INPUT_R == None): # None = TUTTI
                if not (self.SOCIETA_INPUT_R == str(rec["C_MAT"][11])): # SOCIETA-MOD 
                    continue

            for IT in range(1,11):
                if self.DISIMPEGNA == "SI":
                    self.DA_TRASFERIRE = rec["GQF%d" % IT]
                else:
                    self.DA_TRASFERIRE = rec["GQF%d" % IT] + rec["QICF%d" % IT]

                for IC in range(1,self.DA_TRASFERIRE+1):
                    self.C_MAT_A_BARRE = lib_dt.conv_trans_barc(rec["C_MAT"], IT)
                    self.TRATTA_LEGGI()

    def TRATTA_LETTI(self):	# Linea Source Cobol: 4328
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
                    # 2466 LEGGI-PARAMDT
                    self.INDIRIZZO_DPARAM = "..."
                    self.NUMERO_DDT = None
                    # 2467 AGG-DPARAM
                    self.NUMERO_DDT += 1


                    print("Aggiorno  ")
                    print("BOLLA n.  %s" % self.NUMERO_DDT)   

                    if (len(self.PARTAB_SING) > 0):
                        for self.IND_BARUNI in range(len(self.PARTAB_SING)):
                             self.INSERISCI_MOVSKU()   

        # self.DIR_VAR_NAME = "RETIS_DIRECTORY"
        # print(self.DIR_VAR_NAME)
        # self.DIR_VAR_VALUE = input()
        # self.USER_VAR_NAME = "RETIS_UTENTE"
        # print(self.USER_VAR_NAME)
        # self.USER_VAR_VALUE = input()
        # self.FILE_VAR_NAME = "dd_BARCNEG"
        # print(self.FILE_VAR_NAME)
        # print(self.FILE_VAR_VALUE)
        # self.SCRIVI_RECORD()
        # self.QT_ADDR_KEYOFPARTAB_SING = 1
        # self.QT_LL_KEYOFPARTAB_SING = 10
        # self.INTESTA_FILE_BC()
        # self.SCORRI_TAB_SING()
        # self.CALL_COMMAND()
        # self.SCORRI_TB()
        # self.CALL_COMMAND_2()
        # self.SCRIVI_BOLLE()
        # self.CHIAMA_PRINTDDT()
        # self.DIVISA_PRIMO_LETTO = self.DIVISAOFREC_CONFATT
        # self.STAMPA_NO_GIAC()
        # self.TTUNLOCK()
        # self.ALLINEA_BOLLA_ESTERO()
        # self.AVANZA_DDT()
        # print("premi un tasto per continuare...")
        # self.TASTO_INP = input()


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
                      "SQL_MOVSKU-RIF-INTERNO" : None,
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

    def TRATTA_LEGGI(self):
        self.ANACST_MAG_COM = None # self.MAG_INPUT_R # 2015
        self.ANACST_C_MAT_COM = None # 2016 MOVE C-MAT OF REC-ANAMAT TO ANACST-C-MAT-COM

        self.RIVALUTA_COSTO_ANAMAT()
    
    def RIVALUTA_COSTO_ANAMAT(self):
        self.CERCA_B2C_NO_DT()
        self.RICERCA_COSTO_ANAMAT()

    def CERCA_B2C_NO_DT(self):
        if self.ANACST_MAG_COM  in self.TAB_B2C_NO_DT:
            self.FLAG_B2C_NO_DT = 'S'
        else:
            self.FLAG_B2C_NO_DT = 'N'

    def RICERCA_COSTO_ANAMAT(self):
        ANACST_C_MAT = self.ANACST_C_MAT_COM
        data = self.session.execute(db.sql_SELECT_ANAMAT_CST,{'ANACST_C_MAT':ANACST_C_MAT})
        #if not data: # IF B2C-NO-DT
            

    def STAMPA_RAPPORTINO(self):
        RAPPRAI3.RAPPRAI3(self.ART_TAB_LETTI,self.TABELLA_NO_GIAC)
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
