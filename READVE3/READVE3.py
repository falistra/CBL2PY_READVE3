import sys
if (sys.version_info.major == 2): # Python 2
    input = raw_input

import RAPPRAI3

class Program():
    def __init__(self):
        self.DESTINO_VALIDO = [73, 90, 94, 27, 28, 29, 34, 22]
        self.TABELLA_NO_GIAC = []
        pass

    def READVE3(self):
        self.TRATTA_DEV()
        self.CARICA_B2C_NO_DT()
        self.TRATTA_IMPEGNATO()
        self.TRATTA_NEG()


    def TRATTA_DEV(self):	# Linea Source Cobol: 2897
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

    def CARICA_B2C_NO_DT(self):
        SQL = """
        SELECT NEGOZIO                                               
        FROM NEGOZIO_ANAG_CATEGORIA 
        JOIN NEGOZIO_CATEGORIA USING (ID_CATEGORIA)
        where DESC_CATEGORIA = 'NEGOZI_ITALIA_B2C_SOC' 
        order by NEGOZIO
        """
        # 01 TAB-B2C-NO-DT.
        #   05 EL-B2C-NO-DT  OCCURS 100.
        #     10 MAG-B2C-NO-DT  PIC S9(4) COMP.

    def TRATTA_IMPEGNATO(self):
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
        self.VERIF_NEG()
        self.VERIF_MAG()
        self.F_V_INPUT = "V"  # self.VERIF_F_V()
        self.VERIFICA_SOC()
        self.CARICA_TAB_UNICO_DDT()
        self.CICLO_DISIMPEGNO()
        self.INIZIA_TAB_ART()
        self.INIZIA_TAB_SING()
        self.TRATTA_OLD_NEW()
        self.TRATTA_LETTI()

    def VERIF_NEG(self):
        print("CONTO cliente (8 cifre)")
        print("  (END/end=fine)")
        self.CONTO_IN = input()
        try:
            if (self.CONTO_IN.lower() == 'end'):
                return 
            self.CONTO_IN_R = int(self.CONTO_IN)
            self.NEG = self.CONTO_IN[5:7]
        except:
            print("CODICE non numerico")
            return
        # lettura REC-ANACON COPY YANACON
        REC_ANACON = {
            "CONTO" : "12345678", 
            "D-CONTO" : "DESCRIZIONE CONTO",
            "FLAG-ANA-8" : "3", # "3", "2"
            "FLAG-ANA-9" : "B", # "B", "S"
        }
        self.D_CONTO = REC_ANACON["D-CONTO"]
        self.FLAG_ANACON = REC_ANACON["FLAG-ANA-8"]
        self.FLAG_DT_ESTERO = True if REC_ANACON["FLAG-ANA-9"] == "B" else False

        self.LEGGI_IND()
        self.MUOVI_IND()
        self.MUOVI_CAP()
        self.SCEGLI_CONTO_FATTURA()
        self.CERCA_LISTINO()


    def LEGGI_IND(self):
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

    def MUOVI_IND(self):
        pass

    def MUOVI_CAP(self):
        pass

    def SCEGLI_CONTO_FATTURA(self):
        pass

    def CERCA_LISTINO(self):
        pass




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
            else:
                break                 

    def VERIFICA_SOC(self):
        while (True):
            print("Soc >> (vuoto = tutti) ")
            try:
                self.SOCIETA_INPUT = input()
                if not self.SOCIETA_INPUT == "":
                    int(self.SOCIETA_INPUT)
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


        self.TAB_UNICO_DDT = {
            "AS-CL" : {
                "MAX_CAPI" : 0,
                "CAPI_LETTI" : 0
            }
        }

    def CICLO_DISIMPEGNO(self):
        for as_cl in self.TAB_UNICO_DDT:
            DEP_TAB_UNICO_DDT = self.TAB_UNICO_DDT[as_cl]
            self.MAG_DISIMPEGNA = self.MAG_INPUT
            self.FORN_DISIMPEGNA = self.SOCIETA_INPUT
            self.AS_DISIMPEGNA = as_cl[0:2]
            self.CLASSE_DISIMPEGNA = as_cl[2:4]
            self.DISIMPEGNA_MAG() 

    def DISIMPEGNA_MAG(self):
        pass # 1394
            # CALL "PYTHON" USING "disimpegna_capi"
            #                     "elimina_impegnati"
            #                      PY-INPUT-REC-DISIMPEGNA
            #                      PY-OUTPUT-DISIMPEGNO.        
            # 01 PY-INPUT-REC-DISIMPEGNA.
            #  05 LISTA-AS               OCCURS 20.
            #     10 AS-DISIMPEGNA.
            #         15 ANNO-DISIMPEGNA        PIC X.
            #         15 STAG-DISIMPEGNA        PIC X.
            # 05 MAG-DISIMPEGNA         PIC XXX.
            # 05 FORN-DISIMPEGNA        PIC X.
            # 05 LISTA-CLASSE-DISIMPEGNA.
            #     10 CLASSE-DISIMPEGNA       PIC XX OCCURS 99.

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
        self.ART_TAB_LETTI = []
        ART = {
           "TAB_ART" : '123456789012345', # PIC S9(15) COMP-3
           "D_MAT" : '1234567', 
           "PRIMA_TG" :  1,   # PIC S*9(4) COMP.
           "PREZZO" : 1,
           "TIPO_ANA" : 'XX',
           "QTA_GIAC" : [],
           "QTA_TAGLIE" : [],
           "COSTO" : 1
        }
        pass

    def INIZIA_TAB_SING(self):
        pass 

    def TRATTA_OLD_NEW(self):
        # print(self.D_CONTO_MEM)
        print("dal mag ",self.MAG_INPUT)
        self.TRATTA_SITPF_3()
        print(" S stampa rapportino")
        self.COD_IN = input().lower()
        if (self.COD_IN == 's'):
            self.STAMPA_RAPPORTINO()
            print("   rapportino stampato")       
        pass

    def TRATTA_LETTI(self):
        pass

    def TRATTA_SITPF_3(self):
        pass

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
    program = Program()
    program.READVE3()   
