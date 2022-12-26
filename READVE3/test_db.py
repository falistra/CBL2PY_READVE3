import db_access_core
session = db_access_core.mysql_connect('reretis', echo=False)

from sqlalchemy.sql import text

query = """
    SELECT NEGOZIO                                               
    FROM NEGOZIO_ANAG_CATEGORIA 
    JOIN NEGOZIO_CATEGORIA USING (ID_CATEGORIA)
         where DESC_CATEGORIA = 'NEGOZI_ITALIA_B2C_SOC' 
         order by NEGOZIO 
"""
query = """
select * from INDIRIZ where conto = 10010101;
"""

query = """
select * from CONFATT where conto = 10010101;
"""

query = """
select * from ANAMAT where C_MAT = 101001080006088;
"""

rowset = session.execute(text(query))
for r in rowset:
    for k in r._mapping:
        print(k,r._mapping[k])
    print(r)

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


# engine = mysql_engine('reretis', echo=False)
# conn = engine.connect()
# result = conn.execute(text("select * from SITPF where MAG  = '007' limit 1"))
# for row in result:
# 	for k in row._mapping:
# 		print(k,row._mapping[k])
# 	print(row)

# for k in data._mapping:
#     print(k,data._mapping[k])
# print(r)
# print(data)

# (row,rc) = mysql_cursor(text(query), session)
# rowset = db_access_core.mysql_dict_conn(text(query), session)

# query = """
# select * from ANAMAT where C_MAT = 101001080006088;
# """
# rowset = session.execute(text(query))
# for r in rowset:
#     for k in r._mapping:
#         print(k,r._mapping[k])
#     print(r)
