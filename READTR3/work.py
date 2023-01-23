# def gen_intervalli(inizio,passo,volte):
#     step = 0
#     while(step < volte):
#         yield inizio+step*passo,inizio+step*passo + passo
#         step += 1

# s = '123123123123'

# print([s[da:a] for (da,a) in list(gen_intervalli(0,3,4))])

import db_access_core

import db_access_core
from config import MYSQL_HOST
from config import MYSQL_USER
from config import MYSQL_PASSWORD
from config import MYSQL_SOCKET
from config import MYSQL_PORT


from sqlalchemy import create_engine

def mysql_engine(database_name, echo=False):
	mysql_user = MYSQL_USER
	mysql_password = MYSQL_PASSWORD
	mysql_host = MYSQL_HOST
	mysql_port = MYSQL_PORT
	# mysql_socket = MYSQL_SOCKET
	engine = create_engine('mysql://%(mysql_user)s:%(mysql_password)s@%(mysql_host)s:%(mysql_port)s/%(database_name)s' % vars(), echo=echo)
	return engine

session = db_access_core.mysql_connect('reretis', echo=False)
engine = mysql_engine('reretis', echo=False)
conn = engine.connect()

import sys
#sys.path.append(r'/home/app/intranet3/bin/Common/models/db_sources')
#from Sitpf import Sitpf 
#from Anamat import Anamat


from sqlalchemy.ext.hybrid import hybrid_property
import sqlalchemy as sa
from sqlalchemy.sql import select

from sqlalchemy.sql import text

parms = {
     "DISIMPEGNA" : False,
     "MAGAZZINO_PARTENZA" : 5,
     "MAGAZZINO_DESTINAZIONE" : 6,
#     "SOCIETA" : "2",
#     "CLASSI" : ["01","02"],
     "ANNO" : "3",
     "STAG" : "4"    
}

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
     limit 100
     ;
""".format(" AND ".join(where_clauses)) 

print(sql)

s = text( sql )

result = conn.execute(s,parms)
# for row in result:
#      print(Sitpf.cmat.name, row[Sitpf.cmat.name])
#      print('MAG', row['MAG'])
#      print('CLASSE', row['CLASSE'])
#      print('SOCIETA', row['SOCIETA'])
#      print('ANNO', row['ANNO'])
#      print('STAG', row['STAG'])
#      # array giacenza
#      gqf = [row["GQF{}".format(tg)] for tg in range(1,11)]
#      if parms["DISIMPEGNA"] :
#           # se si DISIMPEGNA allora GQF = gqf di sitpf
#           GQF = gqf
#           print("GQF",GQF) 
#      else: # altrimenti
#           # array impegnato
#           qif = [row["QIF{}".format(tg)] for tg in range(1,11)]
#           # se (giacenza + impegnato) > 0 allora GQF = giacenza + impegnato altrimenti GQF = 0
#           f = lambda giac,imp : giac + imp if giac + imp > 0 else 0
#           GQF = [f(giac,imp) for (giac,imp) in zip(gqf,qif)]
#           print("QIF",qif)
#           print("gqf",gqf)
#           print("GQF",GQF)
#      break



import itertools
  
# Key function
key_func = lambda x: x["CLASSE"]
  
for classe, gruppo_classe in itertools.groupby(result, lambda x: x["CLASSE"]):
#    print(key + " :", list(group))
    print(classe)
    for societa, gruppo_societa in itertools.groupby(gruppo_classe, lambda x: x["SOCIETA"]):
          print(societa)
          for c_mat_rec in gruppo_societa:
               print(c_mat_rec)


# s = select(Sitpf)
# print(s)

# result = conn.execute(s)
# for row in result.first():
#      print(row)
#      break


# class mySitpf(Sitpf):

#      @hybrid_property
#      def CLASSE(self):
#         return self.cmat



# rec = (
#      session.query(mySitpf)
#      .order_by(mySitpf.CLASSE)
#      .first()
# )

# print(rec.cmat,rec.CLASSE)