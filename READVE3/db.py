import db_access_core
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


from sqlalchemy.sql import text

sql_NEGOZIO_CATEG = text("""
    SELECT NEGOZIO                                               
    FROM NEGOZIO_ANAG_CATEGORIA 
    JOIN NEGOZIO_CATEGORIA USING (ID_CATEGORIA)
         where DESC_CATEGORIA = 'NEGOZI_ITALIA_B2C_SOC' 
         order by NEGOZIO 
""")

sql_SELECT_ANAMAT_CST = text("""
SELECT CST_STD, CST_STD_2                                                      
    FROM ANAMAT_CST                                               
    WHERE C_MAT   = :ANACST_C_MAT 
""")

sql_SELECT_ANACON = text("""
select CONTO,D_CTO,FLAG_8,FLAG_9 from ANACON where conto = :CONTO_IN_R;
""")

sql_SELECT_INDIRIZ = text("""
select * from INDIRIZ where conto = :CONTO_IN_R;
""")

sql_SELECT_LISTINO = text("""
select LIST,DVS from CONFATT where conto = :CONTO_IN_R;
""")

sql_SITPF3 = """
select * from SITPF where MAG  = :MAG_INPUT
"""

sql_SELECT_ANAMAT = text("""
select * from ANAMAT where C_MAT = :C_MAT;
""")

sql_SELECT_ANAMAT = text("""
select * from DPARAM where C-AZIENDA = 0;
""")

sql_INSERT_MOV_SKU = text("""
               INSERT INTO MOV_SKU                                          
               VALUES(
                       NULL,
                      :MOVSKU-RIF-INTERNO,
                      :MOVSKU-CMAT,
                      :MOVSKU-TG,
                      :MOVSKU-BARUNI,
                      :MOVSKU-CONTO,                                                                     
                      :MOVSKU-MAG,
                      :MOVSKU-SKU,
                      :MOVSKU-IS-BARUNI-READ,
                      :MOVSKU-IS-BARUNI-CERTIFIED,
                      :MOVSKU-SKU-FATTURAZIONE
                      )                                     
""")