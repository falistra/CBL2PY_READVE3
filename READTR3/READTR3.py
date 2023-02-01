# -*- coding: utf-8 -*-
import logging
from datetime import datetime
import itertools
from operator import add,sub
import json
import re
import lib_exception

from sqlalchemy import insert, update
from sqlalchemy.sql import text
from sqlalchemy import create_engine
from sqlalchemy.sql.expression import or_


from ws_rest import Call_Rest_Api_DT
import db_access_core
import config

from LoggingConfig import log_config as myconfig      # configurazione del logger

import sys

from Common.models.Utils import Utils
from stampaDdt.controller import stampaDdtController
from stampaDdt.locator import locator
from Common.models.db_sources.Movmag import Movmag
from Common.models.db_sources.Sitpf import Sitpf
from Common.models.db_sources.Anacon import Anacon
from Common.models.db_sources.Anamat import Anamat
from Common.models.db_sources.Sitpez import Sitpez
from Common.models.db_sources.Dparam import Dparam
from Common.models.db_sources.Indiriz import Indiriz
from Common.models.db_sources.Anacon import Anacon


try:
    import get_param
    import disimpegna_capi
except:
    sys.path.append(r'/home/app/intranet3/bin/4cobol')
    import get_param
    import disimpegna_capi


logging.config.dictConfig(myconfig)


# solo per compatibilta con python 2.7
input = raw_input

__LIMIT__ = 10
# numero di righe di sitpf da leggere per test. None per tutte


class SitpfZ(Sitpf):

    def __repr__(self):
        return "C_MAT : {}, MAG : {}".format(self.cmat,self.mag)

    @property
    def gqf(self):
        return (self.gqf1, self.gqf2, self.gqf3, self.gqf4, self.gqf5, self.gqf6, self.gqf7, self.gqf8, self.gqf9, self.gqf10)

    @property
    def qif(self):
        return (self.qif1, self.qif2, self.qif3, self.qif4, self.qif5, self.qif6, self.qif7, self.qif8, self.qif9, self.qif10)


class Program():
    def __init__(self, loglevel='info', MAG='CDEP', AZIONE='USCITA'):
        if (loglevel == 'debug'):
            logging.getLogger().setLevel(logging.DEBUG)
        elif (loglevel == 'info'):
            logging.getLogger().setLevel(logging.INFO)
        elif (loglevel == 'warning'):
            logging.getLogger().setLevel(logging.WARING)
        else:
            logging.getLogger().setLevel(logging.ERROR)

        self.NTGOCCURS = 10  # 104
        self.AZIONE = AZIONE
        self.session = db_access_core.mysql_connect('reretis', echo=False)
        self.QWPRINT = open('READTR3.out', "w")

    def READTR3(self):
        logging.debug("====INIZIO READTR3====")

        parms = {
            "AGGIORNA": False,
            "DISIMPEGNA": False,
            'AZIONE': 'USCITA',
            "MAGAZZINO_PARTENZA": 5,
            "MAGAZZINO_DESTINAZIONE": 6,
            "SOCIETA": "2",
            "CLASSI": ["16", "02"],
            "ANNO": "3",
            "STAG": "4"
        }

        # from getMagazzino import getMagazzino
        # MAG_PAR = parms["MAGAZZINO_PARTENZA"] # CDP
        # AZIONE = 'USCITA'
        # getMagazzino(parms,MAG_PAR,AZIONE)

        if parms["AGGIORNA"] and parms["DISIMPEGNA"]:
            ok = self.CALL_DISIMPEGNA_MAG(parms) # 437
            if not ok:
                logging.fatal("ERRORE DISIMPEGNO!!!")
                sys.exit(1)

        listaRecords = self.get_rowset_SITPF(parms)
        self.output(listaRecords, parms)

    def CALL_DISIMPEGNA_MAG(self, parms):
        anno_stag = "{}{}{}".format(parms["ANNO"], parms["STAG"], " "*38)
        mag = parms["MAGAZZINO_DESTINAZIONE"] if parms["AZIONE"] == "USCITA" else parms["MAGAZZINO_PARTENZA"]
        pard = config.configura_batch()
        pard['cobol_input'] = '{:<}{:<}{:<}{:<}'.format(
            anno_stag,
            str(mag).zfill(3),
            parms["SOCIETA"],
            "".join(parms["CLASSI"])  # classi.rjust(198)
        )
        pard['cobol_output'] = 'KO'
        logging.debug(pard['cobol_input'])
        # disimpegna_capi.elimina_impegnati(pard, verbose=True)
        success = pard['cobol_output'][0:2]
        return success == "OK"

    def get_rowset_SITPF(self, parms):

        rec_sitpf = self.session.query(SitpfZ,Anamat)\
               .join(Anamat,SitpfZ.cmat == Anamat.cmat)\
               .filter(SitpfZ.mag == parms["MAGAZZINO_PARTENZA"])\
               .filter(SitpfZ.gqf_tot > 0)


        if "CLASSI" in parms:
            rec_sitpf = rec_sitpf.filter(or_(*[Anamat.cmat.regexp_match('^..{}'.format(classe)) for classe in parms["CLASSI"]]))

        if "SOCIETA" in parms:
            rec_sitpf = rec_sitpf.filter(Anamat.cmat.regexp_match('{}...$'.format(parms["SOCIETA"])) )

        if "ANNO" in parms:
            rec_sitpf = rec_sitpf.filter(Anamat.anno == int(parms["ANNO"]))

        if "STAG" in parms:
            rec_sitpf = rec_sitpf.filter(Anamat.stag == int(parms["STAG"]))

        logging.debug(str(rec_sitpf))
        logging.debug( str(rec_sitpf.statement.compile().params))

        # logging.debug(str(rec_sitpf.statement.compile(dialect=mysql.dialect(),compile_kwargs={"literal_binds": True})))

        if __LIMIT__:
            listaRecords = rec_sitpf.limit(__LIMIT__) # all()
        else:
            listaRecords = rec_sitpf.all()
        return listaRecords


    def output(self, listaRecords, parms):
        # RITORNA 930
        # RICAVA-ANAGRAFICA 1130
        self.CONTO_IN = '100{}'.format(
            str(parms["MAGAZZINO_DESTINAZIONE"]).zfill(5))
        
        self.CAMPI_ANAGRAFICI = {"CONTO-IN": self.CONTO_IN}

        (rec_anacon,rec_Indiriz) = self.session.query(Anacon,Indiriz)\
            .filter(Anacon.conto == self.CONTO_IN)\
            .filter(Anacon.conto == Indiriz.conto)\
            .one()


        self.LEGGI_PARAMDT()
        # self.AGG_DPARAM()

        self.NUMERO_RIGA_COM = 0

        NUMERO_TAGLIE = self.NTGOCCURS
        QTA_GIAC_TOT = [0]*NUMERO_TAGLIE
        QTA_GIAC_CLASSE = {}
        for classe, gruppo_classe in itertools.groupby(listaRecords, lambda item: str(item[0].cmat)[2:2]): # classe
            QTA_GIAC_CLASSE[classe] = [0]*NUMERO_TAGLIE
            QTA_GIAC_SOCIETA = {}
            for societa, gruppo_societa in itertools.groupby(gruppo_classe, lambda item: str(item[0].cmat)[-4:1]): # societa
                # logging.debug(societa)
                QTA_GIAC_SOCIETA[societa] = [0]*NUMERO_TAGLIE
                for item in gruppo_societa:
                    # logging.debug(item)
                    rec_sitpf = item[0]
                    gqf = rec_sitpf.gqf
                    qif = rec_sitpf.qif
                    if parms["DISIMPEGNA"]:
                            # se si DISIMPEGNA allora GQF = gqf di sitpf
                            QTA_GIAC_PF = gqf
                    else:
                        # se (giacenza + impegnato) > 0 allora GQF = giacenza + impegnato altrimenti GQF = 0
                        def f(giac, imp): return giac + imp if giac + imp > 0 else 0
                        QTA_GIAC_PF = [f(giac, imp) for (giac, imp) in zip(gqf, qif)]
                        
                        QTA_GIAC_SOCIETA[societa] = [sum(x) for x in zip(QTA_GIAC_PF, QTA_GIAC_SOCIETA[societa])]
                        QTA_GIAC_TOT = [sum(x) for x in zip(QTA_GIAC_PF, QTA_GIAC_TOT)]


                    # MOVMAG 1244
                    values_common = {
                        'RIF_INTR': self.riferimentoInterno,
                        "FD": int(datetime.today().strftime("%V")),
                        "C_MAT_A": 0,
                        "RIF_ORD": 0,
                        "Q": 0,
                        "RIF_BLL_F": 0,
                        "PRZ": 0,
                        "DVS": 'EUR',
                        "VAL_REC": '  ',
                        "U_MIS": 'NR',
                        "C_MAT": rec_sitpf.cmat,
                    }
                    self.NUMERO_RIGA_COM = + 1
                    values_MOVMAG_P = {
                        "NR_R": self.NUMERO_RIGA_COM,
                        "MAG": int(parms["MAGAZZINO_PARTENZA"]),
                        "C_OP": 'TRAS',
                        "CONTO": self.CONTO_IN
                    }

                    values = values_common.copy()
                    values.update(values_MOVMAG_P)
                    # -v : qta cambiate di segno
                    qta_taglia_p = {"QT{}".format(it): -v for (it, v) in enumerate(QTA_GIAC_PF,1)}
                    values.update(qta_taglia_p)

                    stmt = insert(Movmag).values(values)
                    logging.debug(str(stmt) + '\n' + str(stmt.compile().params))

                    self.NUMERO_RIGA_COM = + 1
                    values_MOVMAG_D = {
                        "NR_R": self.NUMERO_RIGA_COM,
                        "MAG": parms["MAGAZZINO_DESTINAZIONE"],
                        "C_OP": 'TRA1',
                        "CONTO": self.CONTO_IN
                    }
                    values = values_common.copy()
                    values.update(values_MOVMAG_D)
                    qta_taglia_p = {"QT{}".format(it): v for (it, v) in enumerate(QTA_GIAC_PF,1)}
                    values.update(qta_taglia_p)

                    stmt = insert(Movmag).values(values)
                    logging.debug(str(stmt) + '\n' + str(stmt.compile().params))
                    # self.conn.execute(stmt)

                    self.AGGIORNA_SITPF(parms, rec_sitpf, QTA_GIAC_PF)

                QTA_GIAC_CLASSE[classe] = [sum(x) for x in zip(
                    QTA_GIAC_SOCIETA[societa], QTA_GIAC_CLASSE[classe])]

        if parms["AGGIORNA"]:  # 954
            self.STRAPCAR()
            self.PRINTDDT()
            self.GENERA_EXCEL_PREBOLLE(parms)

    def AGGIORNA_SITPF(self, parms, rec_sitpf, QTA_GIAC_PF):
        PARAGGPF = {
            'C-MAT': rec_sitpf.cmat,
            'MAGAZZINO': parms["MAGAZZINO_DESTINAZIONE"],
            'VALORE': -1,
            'F-GIAC': 1,
            'SOMMA-QTA': True,
            'QTA': QTA_GIAC_PF
        }
        self.AGSITPFW(PARAGGPF)
        
        PARAGGPF = {
            'C-MAT' : rec_sitpf.cmat,
            'MAGAZZINO' : parms["MAGAZZINO_PARTENZA"],
            'VALORE' : -1,
            'F-GIAC' : -1,
            'SOMMA-QTA': False,
            'QTA' : QTA_GIAC_PF
        }
        self.AGSITPFW(PARAGGPF)


    def AGSITPFW(self,PARAGGPF):
        rec_sitpf = self.session\
            .query(SitpfZ)\
            .filter(SitpfZ.cmat == PARAGGPF['C-MAT'])\
            .filter(SitpfZ.mag == PARAGGPF['MAGAZZINO'])\
            .first()
        if not rec_sitpf: # insert
            new_rec = SitpfZ()
            new_rec.cmat = PARAGGPF['C-MAT']
            new_rec.mag = PARAGGPF['MAGAZZINO']
            new_rec.valrec = '  '
            [setattr(new_rec, 'gqf{}'.format(i), 0) for i in  range(1,self.NTGOCCURS+1)]
            [setattr(new_rec, 'invqpf{}'.format(i), 0) for i in  range(1,self.NTGOCCURS+1)]
            new_rec.gc_v = 0
            self.MUOVI_QUANTITA(new_rec,PARAGGPF)
            logging.debug("INSERT " + repr(new_rec))
#            self.session.add(new_rec)
        else: # update
            pass
            self.MUOVI_QUANTITA(rec_sitpf,PARAGGPF)
            logging.debug("UPDATE " + repr(rec_sitpf))
#        self.session.commit()


    def MUOVI_QUANTITA(self,rec,PARAGGPF):
        if PARAGGPF["VALORE"] < 0:
            # CALCOLA-VALORE-MEDIO 136
            somma_qta = sum(rec.gqf)
            if somma_qta == 0:
                PARAGGPF["VALORE"] = 0
            elif rec.gc_v == 0:
                PARAGGPF["VALORE"] = 0
            else:
                PARAGGPF["VALORE"] = somma_qta / rec.gc_v

        if not PARAGGPF['F-GIAC'] == 0:
            rec.dt_uv =  datetime.today().strftime('%y%m%d')
            QTA_8_COM =  rec.gqf # [getattr(rec, 'gqf{}'.format(i)) for i in  range(1,self.NTGOCCURS+1)]
            # TRATTA-VALORE
            somma_qta = sum(QTA_8_COM)
            valore = somma_qta * PARAGGPF["VALORE"]

            if PARAGGPF['SOMMA-QTA']: # Alias 
                rec.gc_v  += valore
                QTA_8_COM =  list( map(add, QTA_8_COM, PARAGGPF["QTA"]) ) # VARIA_QTA
            else:
                rec.gc_v  -= valore
                QTA_8_COM = list( map(sub, QTA_8_COM, PARAGGPF["QTA"]) ) # VARIA_QTA
            [setattr(rec, 'gqf{}'.format(i), max(0,q)) for (i,q) in  enumerate(QTA_8_COM,1)]

 
    def STRAPCAR(self,parms):  # 1339
        movMagQuery = self.session\
        .query(Movmag,Anacon)\
        .filter(Movmag.conto == Anacon.conto)\
        .filter(Movmag.rif_intr == self.riferimentoInterno)

        if parms["MAGAZZINO_DESTINAZIONE"] == 3:
            movMagQuery.filter(Movmag.mag == 3)

        movMagList = movMagQuery.all()
        logging.debug(Utils.statement(movMagQuery))

        fincatura = \
        "|  |                 |                                       |    "\
        "  |      |             |               |   |           |         |"

        TOT_PREZZO = TOT_CAPI = 0
        for rec_Movmag in movMagList:
            if ((not rec_Movmag.mag > 10 ) or (901 <= rec_Movmag.mag < 909))\
                and (not rec_Movmag.c_op == "SPED"):
                rec_Anamat = self.session.query(Anamat).filter(Anamat.cmat == rec_Movmag.cmat).first()
                if rec_Anamat.flag2 == 1: # GESIONE A PEZZE
                    SitpezQuery = self.session.query(Sitpez)\
                        .filter(Sitpez.cmat == rec_Anamat.cmat)\
                        .filter(Sitpez.rifintr == rec_Movmag.rif_intr)\
                        .filter(Sitpez.nr_r == rec_Movmag.nr_r)
                    for rec_Sitpez in SitpezQuery.all():
                        cmat = str(rec_Movmag.cmat)
                        rigaSTP = "|  |{}{}{}|{}|{}|{}|{}|{}|\n".format(
                            cmat[0:7].ljust(7), # modello
                            cmat[7:13].ljust(6), # articolo
                            cmat[13:].ljust(4), # colore
                            " "*39,
                            str(rec_Sitpez.nr_pezza).ljust(7),
                            str(rec_Sitpez.nr_pezza_fornitore).ljust(7),
                            rec_Movmag.u_mis.ljust(4),
                            "{:.2f}".format(rec_Sitpez.lunghezza_pezza / 100).ljust(9)
                        )
                        self.QWPRINT.write(rigaSTP)
                # TRATTA-RIGA 523
                pass


# 01  RIGA-SPEZZATA  REDEFINES  RIGA-TESTATA.                              
# 028400  05 PRIMO-PEZZO          PIC X(66).                                      
# 028500  05 PRIMO-PEZZO-RID  REDEFINES  PRIMO-PEZZO.                             
# 028600   10 FINC1               PIC X.                                          
# 028700   10 FILLER              PIC X(65).                                      
# 028800  05 SECONDO-PEZZO        PIC X(66).                                      
# 028900  05 SECONDO-PEZZO-RID  REDEFINES  SECONDO-PEZZO.                         
# 029000   10 FILLER              PIC X(23).                                      
# 029100   10 FINC2               PIC X.                                          
# 029200   10 FILLER              PIC X(31).                                      
# 029300   10 FINC3               PIC X.                                          
# 029400   10 FILLER              PIC X(9).                                       
# 029500   10 FINC4               PIC X.       


# 050300 STAMPA-PEZZA.                                                            
# 050400     PERFORM TEST-CONTARIGA THRU EX-TEST-CONTARIGA.                       
# 050500     MOVE RIGA-FINCATA-1 TO PRIMO-PEZZO.                                  
# 050600     MOVE RIGA-FINCATA-2 TO SECONDO-PEZZO.                                
# 050700     MOVE MODELLO OF C-MAT-TRANSITO TO MODELLO-DETT.                      
# 050800     MOVE ARTICOLO OF C-MAT-TRANSITO TO ARTICOLO-DETT.                    
# 050900     MOVE COLORE OF C-MAT-TRANSITO TO COLORE-DETT.                        
# 051000     MOVE NR-PEZZA OF REC-SITPEZ TO NR-PEZZA-DETT.                        
# 051100     MOVE NR-PEZZA-F OF REC-SITPEZ TO NR-PEZZA-FOR-DETT.                  
# 051200     MOVE UN-MIS-FATT OF REC-MOVMAG TO UN-MIS-DETT.                       
# 051300     COMPUTE COMODO-QTA = LUNGH-DICHIARATA OF REC-SITPEZ / 100.           
# 051400     MOVE COMODO-QTA TO QTA-DETT.                                         
# 051500     PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT.                           
# 051600     IF CONTARIGA = MAX-RIGHE-TABULATO                                    
# 051700             PERFORM CHIUDI-PAGINA THRU EX-CHIUDI-PAGINA.                 
# 051800 EX-STAMPA-PEZZA.                               



            
            # IF (MAGAZZINO OF REC-MOVMAG NOT > 10 OR                              
# 044800          (MAGAZZINO OF REC-MOVMAG >=901 AND <= 909))                     
# 044900            AND C-OPE OF REC-MOVMAG NOT = "SPED"                          
# 045000        PERFORM CERCA-GESTIONE THRU EX-CERCA-GESTIONE                     
# 045100        IF GESTIONE-A-PEZZE                                               
# 045200           PERFORM TRATTA-PEZZE THRU EX-TRATTA-PEZZE                      
# 045300        END-IF                                                            
            

    def PRINTDDT(self):
        options = {
            'archivio': 'MOVMAG',
            'conto': self.CONTO_IN,
            'magazzino': '4',
            'riferimentoInterno': self.riferimentoInterno,
            'causale': 'TRAS',
            'tipoDocumento': '2',
            'tipoCausale': '2'
        }
        testDdt = stampaDdtController(locator=locator)
        retCodeStampa = testDdt.stampa(options)
        if not retCodeStampa:
            logging.fatal("PRINTDDT: stampa non riuscita")
        return retCodeStampa

    def LEGGI_PARAMDT(self):
        self.rec_Dparam = self.session\
            .query(Dparam)\
            .filter(Dparam.t_p == 3)\
            .first()
        self.NUMERO_DDT = self.rec_Dparam.numero_5
        self.riferimentoInterno = '{}{}'.format(datetime.today().strftime('%y%m%d'), self.NUMERO_DDT)
        return self.rec_Dparam

    def AGG_DPARAM(self,rec_Dparam):
        self.rec_Dparam = self.session\
            .query(Dparam)\
            .filter(Dparam.t_p == 3)\
            .one()
        self.rec_Dparam.numero_5 += 1
        self.session.commit()


    def GENERA_EXCEL_PREBOLLE(self, rec, parms):
        url = 'prebolla/genera_excel'
        method = 'post'
        try:
            request_data = {}
            request_data['rifIntr'] = self.riferimentoInterno
            request_data['conto'] = '100{}'.format(
                str(parms["MAGAZZINO_PROVENIENZA"]).zfill(3))
            request_data['mag'] = str(
                parms["MAGAZZINO_DESTINAZIONE"]).zfill(3)
            call_rest_api_DT = Call_Rest_Api_DT(
                None, logging, method, url, json.dumps(request_data), 15)
            data = call_rest_api_DT.response_data
            if data['status'] == 1:
                logging.error(
                    'Call_Rest_Api_DT.invio.{0}'.format(data['msg']))
                return False
        except:
            logging.error(str(lib_exception.Build_Traceback(sys.exc_info())))
            return False
        return True

if __name__ == "__main__":
    program = Program(loglevel='debug', MAG='CDEP', AZIONE='USCITA')
    program.READTR3()
