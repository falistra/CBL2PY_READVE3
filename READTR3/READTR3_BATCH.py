# -*- coding: utf-8 -*-

import sys
import re
from collections import OrderedDict

import ws_rest
import db_access_core
import lib_dt
import lib_exception


import db
# import common_fun
import logging
from logging import config as log_config
from LoggingConfig import log_config as  myconfig      # configurazione del logger
log_config.dictConfig(myconfig)

import re

import config
sys.path.append(r'/home/app/intranet3/bin/4cobol')
import get_param
import disimpegna_capi

sys.path.append(r'/home/app/intranet3/bin/Common/models/db_sources')
from Sitpf import Sitpf 
from Anamat import Anamat


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

        self.MAG_PAR= MAG
        self.AZIONE = AZIONE
        self.USCITA = True if self.AZIONE == "USCITA" else False
        self.RIENTRO = True if self.AZIONE == "RIENTRO" else False

        self.session = db_access_core.mysql_connect('reretis', echo=False)
        engine = db.mysql_engine('reretis', echo=False)
        self.conn = engine.connect()

    def READTR3(self):
        logging.debug("====INIZIO READTR3====")
        self.NTGOCCURS = 10 # 104

        self.CHIAMA_PARAMETRI_CDEP()
        continua = self.VERIF_INP()
        if not continua:    return # 425


        if self.AGGIORNA_IN == "SI":
            if self.DISIMPEGNA == "SI":
                self.ANNO_DISIMPEGNA = self.AS_IN[0]
                self.STAG_DISIMPEGNA = self.AS_IN[1]
                self.MAG_DISIMPEGNA = self.MAG_IN
                self.FORN_DISIMPEGNA = self.FORN_IN
                for classe in self.CLASSI_IN:
                    retcode = self.CALL_DISIMPEGNA_MAG()

        PARMS = {
            "MAG" : self.MAGAZZINO_P,
            "SOCIETA" : self.FORN_IN,
            "CLASSI" : self.CLASSI_IN,
            "ANNO" : self.ANNO,
            "STAG" : self.STAGIONE    
        }

    def get_rowset_SITPF(self,parms):
        where_clauses = [
            " (SUBSTRING(CAST(s.C_MAT AS CHAR), 2, 2) IN :CLASSI)" if CLASSI else "TRUE",
            " (SUBSTRING(CAST(s.C_MAT AS CHAR), -4, 1) = :FORN_IN)"  if FORN_IN else "TRUE",
            " a.ANNO = :A_IN" if A_IN else "TRUE",
            " a.STAG = :S_IN" if A_IN else "TRUE"
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
            
            limit 1;
        """.format(" AND ".join(where_clauses)) 







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

    def VERIF_INP(self):
        while(True):
            while (True):
                    print("Si vuole eliminare impegnato ?(SI/NO) (//=fine)")
                    try:
                        self.DISIMPEGNA = input()
                    except KeyboardInterrupt:
                        sys.exit(1)
                    if self.DISIMPEGNA == "//":
                        return True
                    if self.DISIMPEGNA not in ["SI","NO"]:
                        print("Risposta errata")
                        continue 
                    else:
                        break
                
            while (True):
                    print("Anno (1 car) (//=fine) ")
                    try:
                        self.AS_IN = input()
                    except KeyboardInterrupt:
                        sys.exit(1)
                    if self.AS_IN == "//":
                        return True
                    if (re.match("^\d$",self.AS_IN)):
                        self.AS_IN = int(self.AS_IN)
                        break
                    else:
                        print("Non numerico di una cifra")
                        continue

            while (True):
                    print("Stagione (1 car)")
                    try:
                        self.S_IN = input()
                    except KeyboardInterrupt:
                        sys.exit(1)
                    if (re.match("^[2,4]$",self.S_IN)):
                        self.S_IN = int(self.S_IN)
                        break
                    else:
                        print("Stag errata; non '2' o '4'")
                        continue

            if (self.MAG_PAR== "CDP"):
                    while (True):
                        if self.USCITA:
                            print("Magazzino destinazione c/dep ",)
                        else:
                            print("Magazzino provenienza c/dep ",)
                        try:
                            self.MAG_IN = input()
                        except KeyboardInterrupt:
                            sys.exit(1)
                        
                        if (re.match("^\d+$",self.AS_IN)):
                            self.MAG_IN = int(self.MAG_IN)
                            if not (self.MAG_IN in self.MAG_CDEP):
                                print("Magazzino NON C/DEP errato: ")
                                continue
                        else:
                            print("Non numerico ")
                            continue

                        trovato = self.VERIFICA_CDEP_MAG()
                        if not trovato:
                            print("Magazzino NON C/DEP errato ")
                            continue
                        else:
                            self.MAG_PAR = self.MAG_IN
                            self.MAG_DEST_CDEP = 1


                        if self.USCITA:
                            pard = config.configura_batch()
                            pard['cobol_input'] = '{:<}{:<}{:02d}'.format(
                                'magazzini'.ljust(50),                
                                "magazzini_sede_pf".ljust(50),
                                3
                            )
                            MAGS = []
                            for T in range(1,16):
                                get_param.get_param_multi(pard,verbose=True)
                                self.NUM_VALUES_CDEP = int(pard['cobol_output'][2:4])
                                MAG_SEDE = pard['cobol_output'][4:]

                                def gen_intervalli(inizio,passo,volte):
                                    step = 0
                                    while(step < volte):
                                        yield inizio+step*passo,inizio+step*passo + passo
                                        step += 1

                                MAG_SEDE = [int(MAG_SEDE[da:a]) for (da,a) in list(gen_intervalli(0,3,self.NUM_VALUES_CDEP))]
                                if T in MAG_SEDE:
                                    MAGS.append(T)
                            
                            
                            
                            MESSAGGIO_MAG = "Magazzino provenienza ({} Cdep)".format(",".join(MAGS)) 
                            while (True):
                                print("MESSAGGIO-MAG = {}".format(MESSAGGIO_MAG))
                                try:
                                    self.MAG_IN = input() # 756
                                    self.MAG_IN_R = int(self.MAG_IN)
                                except KeyboardInterrupt:
                                    sys.exit(1)
                                except:                
                                    print("MAG non numerico")
                                    continue
                                if not self.MAG_IN in MAGS]:
                                    print("accettato MAG IN {}".format(",".join(MAGS)))
                                else:
                                    continue                
                                if self.MAG_PAR == "007":
                                    if self.MAG_IN_R == 2:
                                        print("Magazzino errato")
                                        continue

                                if self.MAG_IN_R in [4,1] and (self.MAG_PAR in [ '006', '008', '003'] and self.MAG_DEST_CDEP == 0) :
                                        print("Magazzino errato")
                                        continue

                                if self.MAG_IN == self.mag_par:
                                    print("Magazzino Part/Dest = !!")
                                    continue
                        
                        self.MAGAZZINO_P = self.MAG_IN_R
                        self.MAGAZZINO_D = self.MAG_PAR

                        if self.RIENTRO:
                            while(True):
                                print("Magazzino destinazione  (6,7)",)
                                try:
                                    self.MAG_IN = input()
                                except KeyboardInterrupt:
                                    sys.exit(1)
                                if (re.match("^\[6,7]$",self.MAG_IN)):
                                    self.MAG_IN_R = int(self.MAG_IN)
                                    self.MAGAZZINO_D = self.MAG_IN_R
                                    break
                                else:
                                    print("Magazzino errato ")
                                    continue
                        self.MAGAZZINO_P = self.MAG_PAR

            print("MAG provenienza   : {}".format(self.MAGAZZINO_P))
            print("MAG destinazione   : {}".format(self.MAGAZZINO_D))

            # FORN 637
            while (True):
                    print("Forn (1 cifra)   (vuoto=tutti) ")
                    try:
                        self.FORN_IN = input()
                    except KeyboardInterrupt:
                        sys.exit(1)
                    if (re.match("^\d$",self.FORN_IN)):
                        self.FORN_IN_R = int(self.FORN_IN)
                    else:
                        print("Forn non numerico")
                        continue
                    if self.FORN_IN_R == 0:
                        print("Forn errato")
                        continue
                    else:
                        break   

            # CLASSI-IN 651  
            self.CLASSI_IN = []   
            while (True):
                    print("Classe (2 cifre)(vuoto=fine) ")
                    try:
                        classe = input()
                    except KeyboardInterrupt:
                        sys.exit(1)
                    if classe == "":
                        break
                    if (re.match("^\d\d$",classe)):
                        classe = int(classe)
                        self.CLASSI_IN.append(classe)
                    else:
                        print("Classe non numerica")
                        continue
            # 673
            while (True):
                    print("Aggiorni? (SI/NO)")
                    try:
                        self.AGGIORNA_IN = input()
                    except KeyboardInterrupt:
                        sys.exit(1)
                    if self.AGGIORNA_IN not in ["SI","NO"]:
                        print("Risposta errata")
                        continue 
                    else:
                        break

            print("AnnoStag: ".format(self.AS_IN))
            print("Tutti i Fronitori" if self.FORN_IN == "" else "Forn: {}".format(self.FORN_IN) )
            print("Tutte le classi" if self.CLASSI_IN == [] else "Classi: {}".format(",".join(self.CLASSI_IN) ))
            print("Con aggiornamento" if self.AGGIORNA_IN == "SI" else "Senza aggiornamento")

            while (True):
                print("Confermi? SI/NO)")
                try:
                    conferma = input()
                except KeyboardInterrupt:
                    sys.exit(1)
                if conferma not in ["SI","NO"]:
                    print("Risposta errata")
                    continue 
                break

            if conferma == "SI":
                return True
            else:
                return False

    def VERIFICA_CDEP_MAG(self):
        return True


if __name__ == "__main__":
    program = Program(loglevel='debug',MAG='CDEP',AZIONE='USCITA')
    program.READTR3() 