import sys
from QTABEL import QTABEL
from collections import OrderedDict
import logging


class RAPPRAI3:
    def __init__(self,TABELLA_ARTICOLI_LETTI,TABELLA_NO_GIAC):

        logging.debug("INIT RAPPRAI3")
        self.TABELLA_ARTICOLI_LETTI = TABELLA_ARTICOLI_LETTI
        self.TABELLA_NO_GIAC = TABELLA_NO_GIAC

        self.TABELLA_CLASSI_LETTE = OrderedDict()
        self.CARICA_TABELLA_CLASSI()
        for ART in self.TABELLA_ARTICOLI_LETTI:
            self.CARICA_QTA_CLASSE(ART)
        for CMAT in self.TABELLA_NO_GIAC:
            self.CARICA_QTA_NO_GIAC(CMAT)

    
    def CARICA_TABELLA_CLASSI(self):
        self.CC_SOCIETA = 'MM'        
        SQL = """
        SELECT  classe,                                             
                desc_classe                                               
        FROM anagrafica_classi_dbg                                                  
        WHERE societa = % -- {CC_SOCIETA}                                   
        ORDER BY classe   
        """ 
        # 085900          INTO        :TABELLA-CLASSI
        #   
        data = [] # dati classi lette
        for classe_riga in data:
             self.TABELLA_CLASSI_LETTE[classe_riga["classe"]] = {
                "CLASSE" : classe_riga["classe"],
                "QTA-TOT" : 0,
                "PREZZO-TOT" : 0,
                "QTA-NO-GIAC" : 0,
                "PREZZO-NO-GIAC" : 0
             }
                           

    def CARICA_QTA_CLASSE(self,ART):
        CLASSE = ART["CMAT"][1:3] # DANCODBC
        self.TABELLA_CLASSI_LETTE[CLASSE]["QTA-TOT"] += sum(ART["QTA-TAGLIA"])
        self.TABELLA_CLASSI_LETTE[CLASSE]["PREZZO-TOT"] += sum([qta * ART["PREZZO"] for qta in ART["QTA-TAGLIA"]])

    def CARICA_QTA_NO_GIAC(self,ELEM_NO_GIAC):
        CLASSE = ELEM_NO_GIAC["C-MAT"][1:3] # DANCODBC
        self.TABELLA_CLASSI_LETTE[CLASSE]["PREZZO-NO-GIAC"] += ELEM_NO_GIAC["C-MAT"]
