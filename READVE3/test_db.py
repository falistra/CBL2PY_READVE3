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
rowset = session.execute(text(query))
for r in rowset:
    print(r)

# rowset = db_access_core.mysql_dict_conn(text(query), session)