# -*- coding: utf-8 -*-

import sys
import re
import json
import itertools
from sqlalchemy.sql import text
from sqlalchemy import insert, update

import logging
from logging import config as log_config
from LoggingConfig import log_config as  myconfig      # configurazione del logger
log_config.dictConfig(myconfig)
import lib_exception

from ws_rest import Call_Rest_Api_DT
import ws_rest
import db_access_core

import db

import config
sys.path.append(r'/home/app/intranet3/bin/4cobol')
import get_param
import disimpegna_capi

sys.path.append(r'/home/app/intranet3/bin/Common/models/db_sources')
from Movmag import Movmag
from Sitpf import Sitpf



# solo per compatibilta con python 2.7
input = raw_input

class Program():
    def __init__(self,loglevel='info',MAG='CDEP',AZIONE='USCITA'):
        if (loglevel == 'debug'):
            logging.getLogger().setLevel(logging.DEBUG)
        elif (loglevel == 'info'):
            logging.getLogger().setLevel(logging.INFO)
        elif (loglevel == 'warning'):
            logging.getLogger().setLevel(logging.WARING)
        else:
            logging.getLogger().setLevel(logging.ERROR)

        self.NTGOCCURS = 10 # 104

        self.MAG_PAR= MAG
        self.AZIONE = AZIONE
        self.USCITA = True if self.AZIONE == "USCITA" else False
        self.RIENTRO = True if self.AZIONE == "RIENTRO" else False

        self.session = db_access_core.mysql_connect('reretis', echo=False)
        engine = db.mysql_engine('reretis', echo=False)
        self.conn = engine.connect()

    def READTR3(self):
        logging.debug("====INIZIO READTR3====")

        parms = {
            "AGGIORNA" : True,
            "DISIMPEGNA" : False,
            "MAGAZZINO_PARTENZA" : 5,
            "MAGAZZINO_DESTINAZIONE" : "06",
            "SOCIETA" : "2",
            "CLASSI" : ["01","02"],
            "ANNO" : "3",
            "STAG" : "4"    
        }

        listaRecords = self.get_rowset_SITPF(parms)
        self.output(listaRecords,parms)


    def get_rowset_SITPF(self,parms):
        where_clauses = [
            " s.MAG = :MAGAZZINO_PARTENZA",
            " (SUBSTRING(CAST(s.C_MAT AS CHAR), 2, 2) IN :CLASSI)" if parms.get("CLASSI",False) else "TRUE",
            " (SUBSTRING(CAST(s.C_MAT AS CHAR), -4, 1) = :SOCIETA)"  if parms.get("SOCIETA",False) else "TRUE",
            " a.ANNO = :ANNO" if parms.get("ANNO",False) else "TRUE",
            " a.STAG = :STAG" if parms.get("STAG",False) else "TRUE",
            " (s.GQF1+s.GQF2+s.GQF3+s.GQF4+s.GQF5+s.GQF6+s.GQF7+s.GQF8+s.GQF9+s.GQF10 > 0)"
        ]

        sql = """
            select s.*,
            a.ANNO,
            a.STAG,
            SUBSTRING(CAST(s.C_MAT AS CHAR), 2, 2) AS CLASSE,
            SUBSTRING(CAST(s.C_MAT AS CHAR), -4, 1) AS SOCIETA
            
            from SITPF s
            INNER JOIN ANAMAT a USING (C_MAT) 

            where 
            {}

            order by CLASSE,SOCIETA,C_MAT
            limit 1
            ;
        """.format(" AND ".join(where_clauses)) 
        s = text( sql )
        return self.conn.execute(s,parms)

    def LEGGI_IND(self):
        data = self.session.execute("select * from INDIRIZ where conto = :CONTO_IN_R;",{"CONTO_IN_R":self.CONTO_IN_R})
        
        # lettura REC-ANACON COPY YANACON
        if not data:
            # print("Manca INDIRIZZO ")
            return False

        self.REC_INDIRIZZI = rec = data.fetchone()
        self.CAMPI_ANAGRAFICI["INDIRIZZO-STD"] = rec["INDZ_1"]
        try:
            #logging.debug("{}".format(rec["INDZ_1"]))
            (INDIRIZZO,LOCALITA) = rec["INDZ_1"].split(";")
            #logging.debug("{} {}".format(INDIRIZZO,LOCALITA))
            self.CAMPI_ANAGRAFICI["INDIRIZZO-COM"] = INDIRIZZO
            self.CAMPI_ANAGRAFICI["LOCALITA-COM"] = LOCALITA
            if rec["INDZ_2"]:
                #logging.debug("{}".format(rec["INDZ_2"]))
                (INDIRIZZO1,LOCALITA1) = rec["INDZ_2"].split(";")
                #logging.debug("{} {}".format(INDIRIZZO1,LOCALITA1))
                self.CAMPI_ANAGRAFICI["INDIRIZZO-C-COM"] = INDIRIZZO1
                self.CAMPI_ANAGRAFICI["LOCALITA-C-COM"] = LOCALITA1
            else:
                self.CAMPI_ANAGRAFICI["INDIRIZZO-C-COM"] = ""
                self.CAMPI_ANAGRAFICI["LOCALITA-C-COM"] = ""

            self.CAMPI_ANAGRAFICI["D-CONTO-AGG-MEM"] = rec["D_AGG"]
            self.CAMPI_ANAGRAFICI["CAP-COM"] = rec["CAP_1"]
            self.CAMPI_ANAGRAFICI["PROV-COM"] = rec["SGL_P_1"]
            self.CAMPI_ANAGRAFICI["STATO-COM"] = rec["ST"]
            self.CAMPI_ANAGRAFICI["CAP-C-COM"] = rec["CAP_2"]
            self.CAMPI_ANAGRAFICI["PROV-C-COM"] = rec["SGL_P_2"]
        except Exception as e:
            logging.warning(e)
            logging.warning(str(self.CONTO_IN_R) + " Errore in MUOVI_IND")
        pass


    def output(self,listaRecords,parms):

        # RITORNA 930

        # RICAVA-ANAGRAFICA 1130

       CONTO_IN = "10000000{}".format(self.parms["MAGAZZINO_DESTINAZIONE"] )

       self.CAMPI_ANAGRAFICI = {
                "CONTO-IN": CONTO_IN,
                "D-CONTO-MEM": None,
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

       sql = """
       select CONTO,D_CTO,FLAG_8,FLAG_9 from ANACON where conto = :CONTO_IN_R;
       """
       s = text( sql )
       data_from_db = self.conn.execute(s,{'CONTO_IN_R': int(CONTO_IN)})
       self.CAMPI_ANAGRAFICI["D-CONTO-MEM"] = data_from_db.fetchone()["D_CTO"]
       self.LEGGI_IND() # 1547

       QTA_GIAC_CLASSE = {}
       for classe, gruppo_classe in itertools.groupby(listaRecords, lambda x: x["CLASSE"]):
           QTA_GIAC_CLASSE[classe] = [0]*self.NTGOCCURS
           QTA_GIAC_SOCIETA = {}     
           for societa, gruppo_societa in itertools.groupby(gruppo_classe, lambda x: x["SOCIETA"]):
               # print(societa)
               QTA_GIAC_SOCIETA[societa] = [0]*self.NTGOCCURS
               for c_mat_rec in gruppo_societa:
                   # print(c_mat_rec)

                   gqf = [c_mat_rec["GQF{}".format(tg)] for tg in range(1,self.NTGOCCURS+1)]
                   if parms["DISIMPEGNA"] :
                       # se si DISIMPEGNA allora GQF = gqf di sitpf
                       QTA_GIAC_PF = gqf
                   else: # altrimenti
                       # array impegnato
                       qif = [c_mat_rec["QIF{}".format(tg)] for tg in range(1,self.NTGOCCURS+1)]
                       # se (giacenza + impegnato) > 0 allora GQF = giacenza + impegnato altrimenti GQF = 0
                       f = lambda giac,imp : giac + imp if giac + imp > 0 else 0
                       QTA_GIAC_PF = [f(giac,imp) for (giac,imp) in zip(gqf,qif)]
                   
                   QTA_GIAC_SOCIETA[societa] = [sum(x) for x in zip( QTA_GIAC_PF, QTA_GIAC_SOCIETA[societa])]

                   # MOVMAG 1244
                   stmt = (
                    insert(Movmag)
                    .values()
                   )
                   self.conn.execute(stmt) 

                   # AGGIONA SITPF 1317
                   stmt = (
                    update(Sitpf)
                    .where(Sitpf.c.cmat == "patrick")
                    .values(fullname="Patrick the Star") # ....
                   )
                   self.conn.execute(stmt) 


               QTA_GIAC_CLASSE[classe] =  [sum(x) for x in zip( QTA_GIAC_SOCIETA[societa], QTA_GIAC_CLASSE[classe])]    


           # 954
           #        IF SI-AGGIORNAMENTO AND
           #           NUMERO-RIGA-COM > 0    
           #          PERFORM CHIAMA-STRAPCAR THRU EX-CHIAMA-STRAPCAR
           #          PERFORM CHIAMA-PRINTDDT THRU EX-CHIAMA-PRINTDDT
           #   *MAGCDEP*                                                         inizio
           #          PERFORM GENERA-EXCEL-PREBOLLE THRU 
           #               EX-GENERA-EXCEL-PREBOLLE





    def LEGGI_PARAMDT(self):
        q = text("select NUMERO_5 from DPARAM where T_P = 3;")
        rec = self.session.execute(q,{}).fetchone()
        self.NUMERO_DDT = rec["NUMERO_3"]

    def AGG_DPARAM(self):
        NUOVO_NUMERO_DDT = self.NUMERO_DDT + 1
        q = text("UPDATE DPARAM set NUMERO_5  = :NUOVO_NUMERO_DDT where T_P = 3;")
        self.conn.execute(q,{"NUOVO_NUMERO_DDT":NUOVO_NUMERO_DDT})


    def genera_excel_prebolle(self,rec):
        url = 'prebolla/genera_excel'
        method = 'post'

        try:
            request_data = {}
            request_data['rifIntr'] = list_input[0]
            request_data['conto'] = '10000{}'.format()  
            request_data['mag'] = list_input[2]
            call_rest_api_DT = Call_Rest_Api_DT(None, logging, method, url, json.dumps(request_data), 15)
            data = call_rest_api_DT.response_data
            if data['status'] == 1:
                logging.error('Call_Rest_Api_DT.invio.{0}'.format(data['msg']))
                return False
        except:
            logging.error(str(lib_exception.Build_Traceback(sys.exc_info())))
            return False
        return True


    def CALL_DISIMPEGNA_MAG(self):
        pard = config.configura_batch()
        pard['cobol_input'] = '{:<}{:<}{:<}{:<}'.format(
			self.ANNO_DISIMPEGNA,# args.anni_stagioni.rjust(40)+
			self.MAG_DISIMPEGNA, # str(args.magazzino.rjust(3)).zfill(3)+
			self.FORN_DISIMPEGNA, # fornitore.rjust(1)+
			"".join(self.CLASSI_IN) # classi.rjust(198)
        )
        disimpegna_capi.elimina_impegnati(pard, verbose=True)
        success = pard['cobol_output'][0:2]
        return  success == "OK"

    def CHIAMA_PARAMETRI_CDEP(self):
        pard = config.configura_batch()
        pard['cobol_input'] = '{:<}{:<}{:02d}'.format(
            'MAG_CONTO_DEP'.ljust(50),
            'deposito_pf'.ljust(50),
            3
        )
        get_param.get_param_multi(pard,verbose=True)
        self.NUM_VALUES_CDEP = int(pard['cobol_output'][2:4])
        MAG_CDEP = pard['cobol_output'][4:]

        def gen_intervalli(inizio,passo,volte):
            step = 0
            while(step < volte):
                yield inizio+step*passo,inizio+step*passo + passo
                step += 1

        self.MAG_CDEP = [int(MAG_CDEP[da:a]) for (da,a) in list(gen_intervalli(0,3,self.NUM_VALUES_CDEP))]


if __name__ == "__main__":
    program = Program(loglevel='debug',MAG='CDEP',AZIONE='USCITA')
    program.READTR3() 