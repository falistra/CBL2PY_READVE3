import itertools
import db_access_core

from sqlalchemy.sql.expression import or_

from Common.models.db_sources.Sitpf import Sitpf
from Common.models.db_sources.Anamat import Anamat

parms = {
    "AGGIORNA": False,
    "DISIMPEGNA": False,
    "MAGAZZINO_PARTENZA": 5,
    "MAGAZZINO_DESTINAZIONE": 6,
    "SOCIETA": "2",
    "CLASSI": ["16", "02"],
    "ANNO": "3",
    "STAG": "4"
}


class SitpfZ(Sitpf):

    @property
    def gqf(self):
        return (self.gqf1, self.gqf2, self.gqf3, self.gqf4, self.gqf5, self.gqf6, self.gqf7, self.gqf8, self.gqf9, self.gqf10)

    @property
    def qif(self):
        return (self.qif1, self.qif2, self.qif3, self.qif4, self.qif5, self.qif6, self.qif7, self.qif8, self.qif9, self.qif10)


session = db_access_core.mysql_connect('reretis', echo=False)

rec_sitpf = session.query(SitpfZ,Anamat)\
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

# rec_sitpf,rec_anamat = rec_sitpf.first()
# print(rec_sitpf.cmat,rec_sitpf.mag,rec_anamat.cmat,rec_anamat.anno)

# rowsset = rec_sitpf.limit(10)
# for (rec_sitpf,rec_anamat) in rowsset:
#     print(rec_sitpf.cmat,rec_sitpf.mag,rec_anamat.cmat,rec_anamat.anno)

listaRecords = rec_sitpf.limit(10)
# (sitpf,_) = listaRecords[0]
# print(sitpf.cmat,sitpf.gqf,sitpf.qif)


NUMERO_TAGLIE = 10
QTA_GIAC_TOT = [0]*NUMERO_TAGLIE
QTA_GIAC_CLASSE = {}
for classe, gruppo_classe in itertools.groupby(listaRecords, lambda rec: str(rec[0].cmat)[2:2]): # classe
    QTA_GIAC_CLASSE[classe] = [0]*NUMERO_TAGLIE
    QTA_GIAC_SOCIETA = {}
    for societa, gruppo_societa in itertools.groupby(gruppo_classe, lambda rec: str(rec[0].cmat)[-4:1]): # societa
        # logging.debug(societa)
        QTA_GIAC_SOCIETA[societa] = [0]*NUMERO_TAGLIE
        for (sitpf,anamat) in gruppo_societa:
            # logging.debug(c_mat_rec)

            if parms["DISIMPEGNA"]:
                # se si DISIMPEGNA allora GQF = gqf di sitpf
                QTA_GIAC_PF = gqf
            else:
                # se (giacenza + impegnato) > 0 allora GQF = giacenza + impegnato altrimenti GQF = 0
                def f(giac, imp): return giac + imp if giac + imp > 0 else 0
                QTA_GIAC_PF = [f(giac, imp)
                                for (giac, imp) in zip(sitpf.gqf, sitpf.qif)]
                
                QTA_GIAC_SOCIETA[societa] = [sum(x) for x in zip(QTA_GIAC_PF, QTA_GIAC_SOCIETA[societa])]
                QTA_GIAC_TOT = [sum(x) for x in zip(QTA_GIAC_PF, QTA_GIAC_TOT)]
