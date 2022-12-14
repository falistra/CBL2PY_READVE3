
class Program():
    def __init__(self):
        self.REC_BC = None
        self.C_MAT_S = None
        self.T_1 = None
        self.TAGLIA_S = None
        self.T_2 = None
        self.MAG_S = None
        self.T_3 = None
        self.SETTORE_S = None
        self.T_4 = None
        self.NOME_S = None
        self.T_5 = None
        self.PREZZO_S = None
        self.T_6 = None
        self.NOME_F_S = None
        self.T_7 = None
        self.COL_F_S = None
        self.T_8 = None
        self.TG_OUT_S = None
        self.T_9 = None
        self.BARUNI_S = None
        self.REC_PEND = None
        self.NTG_NTG = 10
        self.NTG_IN = None
        self.NTG_OUT = None
        self.NTG_MEM = None
        self.ERR_DISP = None
        self.DISP_4 = None
        self.DISP_8 = None
        self.STATO_DISPLAY = None
        self.FLAG_DT_ESTERO = None
        self.PREZZO_MEM = None
        self.PREZZO_TOT = None
        self.IR = None
        self.JRUNC = 0
        self.IT = None
        self.IC = None
        self.CAMPI_ANAGRAFICI = None
        self.INDIRIZZO_STD = None
        self.INDIRIZZO_COM = " "*60
        self.LOCALITA_COM = " "*60
        self.CAP_COM = 0
        self.PROV_COM = " "*None
        self.STATO_COM = " "*None
        self.INDIRIZZO_C_COM = " "*60
        self.LOCALITA_C_COM = " "*60
        self.CAP_C_COM = 0
        self.PROV_C_COM = " "*None
        self.D_CONTO_MEM = None
        self.D_CONTO_AGG_MEM = None
        self.D_CONTO_VET = None
        self.INDIRIZZO_C_VET = " "*60
        self.LOCALITA_C_VET = " "*60
        self.CAP_C_VET = 0
        self.PROV_C_VET = " "*None
        self.CAMPI_COMODO = None
        self.RIF_BOLLA_DDT = None
        self.None = None
        self.AA_MM_GG_DDT = None
        self.NUMERO_DDT = None
        self.CLIENTE_DDT = None
        self.MAGAZZINO_DDT = None
        self.CAUSALE_DDT = None
        self.TIPO_DOC_DDT = None
        self.TIPO_MOVIMENTO_DDT = None
        self.TIPO_STAMPA_DDT = None
        self.LOC_PART_DDT = None
        self.NOTE_DDT = None
        self.TIPO_DATA_SET_DDT = None
        self.IMPORTO_X_PL = None
        self.FILE_FAT_DDT = None
        self.RIGA_1_DDT = None
        self.RIGA_2_DDT = None
        self.PAR_INDIRIZZO = None
        self.STATO_IND = None
        self.FUNZIONE_IND = None
        self.LL_STRINGA_IND = None
        self.LL_SUBSTRINGA_IND = None
        self.None = None
        self.TIPO_SEP = " ;"
        self.AREA_REC_SET = None
        self.None = None
        self.REC_ANACON = None
        self.CONTO = None
        self.D_CONTO = None
        self.FLAGS = None
        self.FLAG_ANA_1 = None
        self.FLAG_ANA_2 = None
        self.FLAG_ANA_3 = None
        self.FLAG_ANA_4 = None
        self.FLAG_ANA_5 = None
        self.FLAG_ANA_6 = None
        self.FLAG_ANA_7 = None
        self.FLAG_ANA_8 = None
        self.FLAG_ANA_9 = None
        self.FLAG_ANA_10 = None
        self.None = None
        self.FLAG = None
        self.P_DARE = None
        self.P_AVERE = None
        self.P_DARE_C = None
        self.P_AVERE_C = None
        self.C_RAGG = None
        self.FIDO = None
        self.DATA_FIDO = None
        self.DATA_NA = None
        self.DATA_UM = None
        self.DATA_UV = None
        self.TIPO_CONTO = None
        self.VALIDITA_REC = None
        self.None = None
        self.REC_BOLLE = None
        self.CONTO = None
        self.RIF_INTERNO = None
        self.DATA_NASCITA = None
        self.NUM_PRE_FATT = None
        self.NUMERO = None
        self.VAL_REC = None
        self.MAGAZZINO = None
        self.None = None
        self.REC_ANAMAT = None
        self.C_MAT = None
        self.D_MAT = None
        self.DT_NA = None
        self.DT_UV = None
        self.P_STOCK = None
        self.CTO_RICAVI = None
        self.CTO_COSTI = None
        self.FLAGS_A = None
        self.FLAG = None
        self.FLAGS_RID = None
        self.FLAG1 = None
        self.FLAG2 = None
        self.FLAG3 = None
        self.FLAG4 = None
        self.FLAG5 = None
        self.FLAG6 = None
        self.FLAG7 = None
        self.FLAG8 = None
        self.PERC_MAGG = None
        self.ALIQ_IVA = None
        self.CL_GR = None
        self.COLLEZIONE = None
        self.ANNO = None
        self.STAGIONE = None
        self.COSTO = None
        self.UN_MIS = None
        self.CAT_TAGLIO = None
        self.MATER_MAX = None
        self.CATEG_GHELDA = None
        self.TG_BASE = None
        self.PRIMA_TG = None
        self.ULTIMA_TG = None
        self.SCORTA_IND = None
        self.SCORTA = None
        self.IND = None
        self.TIPO_MAT = None
        self.VALID_REC = None
        self.REC_CONFATT = None
        self.CONTO = None
        self.COND_PAG = None
        self.COND_PAGAMENTO = None
        self.CPAG = None
        self.SCPAG = None
        self.GGPAG = None
        self.SC1 = None
        self.SC2 = None
        self.COD_FISCALE = None
        self.COD_IVA = None
        self.BANCA_APP = None
        self.CAUSALE_IVA = None
        self.DIVISA = None
        self.MERCATO = None
        self.NR_COPIE_FATT = None
        self.LISTINO = None
        self.VALIDITA_REC = None
        self.REC_PREZZI = None
        self.C_MAT = None
        self.MERCATO = None
        self.DIVISA = None
        self.MAGAZZINO = None
        self.PREZZO_VENDITA_SUP = None
        self.PREZZO_VENDITA = None
        self.DATA_ULT_AGG = None
        self.VALID_REC = None
        self.REC_PREZZI_PUB = None
        self.C_MAT = None
        self.MERCATO = None
        self.DIVISA = None
        self.MAGAZZINO = None
        self.PREZZO_VENDITA_SUP = None
        self.PREZZO_VENDITA = None
        self.DATA_ULT_AGG = None
        self.VALID_REC = None
        self.REC_INDIRIZZI = None
        self.CONTO = None
        self.D_AGG = None
        self.D_BANCA = None
        self.D_AGG_BANCA = None
        self.CC_BANCA = None
        self.INDIRIZZO = None
        self.TIPO_INDIRIZZO = None
        self.CAP = None
        self.SIGLA_PROV = None
        self.STATO = None
        self.TELEX = None
        self.TELEFONO = None
        self.CONTO_FATTURA = None
        self.INF_COMM_INDIRIZ = None
        self.INF_COMM_ABB = None
        self.PRIORITA = None
        self.PROVINCIA = None
        self.CAMPO_AGE = None
        self.CONTO_PROVV = None
        self.PERC_PROVV = None
        self.COLLEZIONI = None
        self.COLL_VENDITA = None
        self.VALIDITA_REC = None
        self.REC_SITPF = None
        self.C_MAT = None
        self.MAGAZZINO = None
        self.QTA_GIAC = None
        self.QTA_GIAC_PF = None
        self.VAL_GIAC = None
        self.QTA_INV = None
        self.QTA_INV_PF = None
        self.VAL_INV = None
        self.DT_UM = None
        self.DT_INV = None
        self.QTA_ORDINATA = None
        self.QTA_ORD = None
        self.QTA_ORDINATA_C = None
        self.QTA_ORD_C = None
        self.QTA_IMPEGNATA = None
        self.QTA_IMP = None
        self.QTA_IMPEGNATA_C = None
        self.QTA_IMP_C = None
        self.VAL_REC = None
        self.MOVMAG = None
        self.D_MOVMAG = None
        self.RIF_INTERNO = None
        self.NUMERO_RIGA = None
        self.RIF_ORDINE = None
        self.RIF_BOLLA_FORN = None
        self.CLI_FINALE = None
        self.C_MAT = None
        self.CONTO = None
        self.C_OPE = None
        self.PREZZO = None
        self.COSTO_STD = None
        self.SETTIMANA = None
        self.FILLER_MOVMAG = None
        self.CATEG_GHELDA = None
        self.MOD_IMPUTAZ = None
        self.DATA_AGG_TRASF_MOV = None
        self.MAGAZZINO = None
        self.DIVISA = None
        self.UN_MIS_FATT = None
        self.QTA_TAGLIE = None
        self.QTA_TAGLIA = None
        self.QUANTITA = None
        self.VAL_REC = None
        self.REC_PARAMETRI = None
        self.ENTRY_PARAM_DETAIL = None
        self.C_AZIENDA = None
        self.TIPO_PARAMETRI = None
        self.STRINGA_PARAMETRI = None
        self.VALIDITA_REC = None
        self.REC_PARAM_RID = None
        self.None = None
        self.PARAM_MAG = None
        self.CAMPI_FLAG_13 = None
        self.FLAG_1 = None
        self.FLAG_2 = None
        self.FLAG_3 = None
        self.FLAG_4 = None
        self.FLAG_5 = None
        self.FLAG_6 = None
        self.FLAG_7 = None
        self.FLAG_8 = None
        self.FILLER_FLAG = None
        self.CAMPI_FLAG_RID = None
        self.CAMPO_FLAG = None
        self.CAMPI_DATE_13 = None
        self.DATA_BOLLA_TAGLIO_TESSUTO = None
        self.DATA_BOLLA_RESI_PF = None
        self.DATA_BOLLA_TAGLIO_INTERNI = None
        self.DATA_BOLLA_TAGLIO_FODERE = None
        self.DATA_BOLLA_MATERASSO = None
        self.DATA_TRAVASO = None
        self.DATA_RIEP_MAG = None
        self.DATA_6 = None
        self.DATA_7 = None
        self.DATA_8 = None
        self.CAMPI_DATE_RID = None
        self.CAMPO_DATA = None
        self.CAMPI_NUMERI_13 = None
        self.NUM_BOLLA_RESI_PF = None
        self.NUM_BOLLA_TAGLIO_INTERNI = None
        self.NUM_BOLLA_TAGLIO_FODERE = None
        self.NUM_BOLLA_MATERASSO = None
        self.NUM_TRAVASO = None
        self.NUM_BUONO_PRELIEVO = None
        self.NUM_CONSEGNA_NO_BOLLA = None
        self.NUM_CONSEGNA_BOLLA = None
        self.NUMERO_PEZZA = None
        self.CAMPI_NUMERI_RID = None
        self.CAMPO_NUMERO = None
        self.CAMPI_CHIAVE_13 = None
        self.KEY_1 = None
        self.KEY_2 = None
        self.KEY_3 = None
        self.KEY_4 = None
        self.CAMPI_CHIAVE_RID = None
        self.CAMPO_CHIAVE = None
        self.PARAMETRI_VARI = None
        self.REC_PARAM_FITTIZI = None
        self.ENTRY_PARAM_DETAIL = None
        self.C_AZIENDA = None
        self.TIPO_PARAMETRI = None
        self.STRINGA_PARAMETRI = None
        self.VALIDITA_REC = None
        self.REC_PARAM_FITTIZ_R = None
        self.None = None
        self.PARAM_RIPRADI = None
        self.CAMPI_FLAG_55 = None
        self.FLAG_1 = None
        self.FLAG_2 = None
        self.FLAG_3 = None
        self.FLAG_4 = None
        self.FLAG_5 = None
        self.FLAG_6 = None
        self.FLAG_7 = None
        self.FLAG_8 = None
        self.FILLER_FLAG = None
        self.CAMPI_FLAG_RID = None
        self.CAMPO_FLAG = None
        self.CAMPI_DATE_55 = None
        self.None = None
        self.None = None
        self.None = None
        self.None = None
        self.None = None
        self.DATA_6 = None
        self.DATA_7 = None
        self.DATA_8 = None
        self.CAMPI_DATE_RID = None
        self.CAMPO_DATA = None
        self.CAMPI_NUMERI_55 = None
        self.PAR_PROGR_SESS = None
        self.PAR_PROGR_SORTER = None
        self.PAR_PROGR_PRESPED = None
        self.PAR_FITTIZIO_1 = None
        self.NUM_TRASF_DA_MAG_4 = None
        self.None = None
        self.PAR_RESI_SS = None
        self.None = None
        self.CAMPI_NUMERI_RID = None
        self.CAMPO_NUMERO = None
        self.CAMPI_CHIAVE_55 = None
        self.KEY_1 = None
        self.KEY_2 = None
        self.KEY_3 = None
        self.KEY_4 = None
        self.CAMPI_CHIAVE_RID = None
        self.CAMPO_CHIAVE = None
        self.PARAMETRI_VARI = None
        self.RIGA_PENDENTI = None
        self.ANTE_PRIMA_RIGA = None
        self.None = None
        self.NUMERO_P = None
        self.None = None
        self.PRIMA_RIGA = None
        self.None = None
        self.CODICE_P = None
        self.None = None
        self.SECONDA_RIGA = None
        self.None = None
        self.NOME_P = None
        self.None = None
        self.TERZA_RIGA = None
        self.DIV_LIT = None
        self.PREZZO_P = None
        self.None = None
        self.QUARTA_RIGA = None
        self.DIV_EUR = None
        self.PREZZO_P_E = None
        self.None = None
        self.CAMPI_X_WRITE = None
        self.RIF_INTR_WR = None
        self.CONTO_CLI_WR = None
        self.CONTO_DEST_WR = None
        self.MAGAZZINO_WR = None
        self.DEST_WR = None
        self.DIVISA_WR = None
        self.LISTINO_WR = None
        self.CAUSALE_WR = None
        self.DATA_CARICO_WR = None
        self.LISTINO_MEM = None
        self.DIVISA_MEM = None
        self.CAMBIO_MEM = None
        self.CAMPI_UTILI = None
        self.OK_GIAC = None
        self.OK_PREZZO = None
        self.CONTA_PAGINE = None
        self.TOT_CAPI_NO_GIAC = None
        self.CONTA_RIGHE = None
        self.USCITA_PROGRAMMA = None
        self.RISP_NO_GIAC = None
        self.RISP_NO_PREZZO = None
        self.FLAG_ANACON = None
        self.SOC_COM = None
        self.None = None
        self.PRE_SOC = None
        self.SOC_REALE = None
        self.TROVATO_GIAC = None
        self.NUM_BOLLE = None
        self.IND_CAPI_LETTI = None
        self.TOT_BOLLA_C = None
        self.TOT_CAPI_RIGA_C = None
        self.IND_PAG = None
        self.ITB_T = None
        self.ITB_T_5 = None
        self.IND_CL = None
        self.VARIAZIONE = None
        self.INDIRIZZO_DPARAM = None
        self.D_MAT_MEM = None
        self.VAL_REC_MEM = None
        self.IND_RIGA = None
        self.CNTR_ANAMAT = None
        self.DISP_15 = None
        self.DISP_3 = None
        self.TG_CAL = None
        self.PTG_MEM = None
        self.TAGLIO_MEM = None
        self.IND_FILE = None
        self.COD_REC = None
        self.ESIST_VECCHIO = None
        self.OK_O_N = None
        self.OK_NEG = None
        self.CONT = None
        self.CONT_D = None
        self.PREZZO_D = None
        self.PREZZO_TOT_D = None
        self.FINITO = None
        self.IND_4 = None
        self.CONFERMA_STORNO = None
        self.C_MAT_MEM = None
        self.USCITA_DEVICE = None
        self.COL_COM = None
        self.PRE_COL = None
        self.COL_VAR = None
        self.COL_COM_R = None
        self.PARQDATA = None
        self.Q_PARAMETRI_DATA = None
        self.Q_DATA_E = None
        self.Q_DATA = None
        self.Q_GG = None
        self.Q_MM = None
        self.Q_AA = None
        self.Q_DATA_9 = None
        self.None = None
        self.None = None
        self.Q_DATA_EE = None
        self.Q_GG_S = None
        self.Q_MM_S = None
        self.Q_AA_S = None
        self.None = None
        self.Q_DATA_I = None
        self.Q_SETTIMANA = None
        self.PARGEN = None
        self.Q_PARAMETRI_GEN = None
        self.Q_STATO = None
        self.Q_FUNZIONE = None
        self.Q_PARAMETRO_2 = None
        self.Q_PARAMETRO_3 = None
        self.Q_PARAMETRO_4 = None
        self.Q_PARAMETRO_5 = None
        self.Q_PARAMETRO_6 = None
        self.Q_PARAMETRO_7 = None
        self.None = None
        self.PARAGGPF = None
        self.C_MAT = None
        self.MAGAZZINO = None
        self.VALORE = None
        self.QTA_8 = None
        self.QTA = None
        self.FLAGS_AGGIORNAMENTO = None
        self.F_GIAC = None
        self.F_QTA_ORD = None
        self.F_QTA_ORD_C = None
        self.F_QTA_IMP = None
        self.F_QTA_IMP_C = None
        self.CODICE_CONTO = None
        self.CODICE_CONTO_R = None
        self.CAPO_CONTO = None
        self.SOTTO_CONTO = None
        self.SOTTO_CONTO_R = None
        self.SOTTO_CONTO_3 = None
        self.SOTTO_CONTO_2 = None
        self.TABELLA_SINGOLI = None
        self.ELEM_TAB_SING = None
        self.ELEMENTO_SINGOLI = None
        self.C_MAT_SING = None
        self.CONT_SING = None
        self.D_MAT_SING = None
        self.PREZZO_SING = None
        self.PRIMA_TG_SING = None
        self.SKU_SING = None
        self.PARTAB_SING = None
        self.QT_PARAMETRI = None
        self.QT_STATO = None
        self.QT_NUM_ELEM_EFF = None
        self.QT_NUM_ELEM_MAX = None
        self.QT_LL_ELEM = None
        self.QT_ADDR_KEY = None
        self.QT_LL_KEY = None
        self.QT_INDEX_ELEM = None
        self.QT_FUNZIONE = None
        self.DATI_INPUT = None
        self.CONTO_IN = None
        self.CONTO_IN_R = None
        self.CONTO_FINE = None
        self.None = None
        self.None = None
        self.None = None
        self.NEG_IN = None
        self.COD_IN = None
        self.C_MAT_A_BARRE = None
        self.MODELLO = None
        self.MOD_RID = None
        self.MARCHIO = None
        self.CLASSE = None
        self.STAGIONE = None
        self.PROGR_ART = None
        self.ANNO = None
        self.VESTIBILITA = None
        self.SOCIETA = None
        self.None = None
        self.PREFBC_V_F = None
        self.SOC_BC_MOD = None
        self.PEZZO = None
        self.VARIANTE_COL = None
        self.TAGLIA = None
        self.C_MAT_A_BARRE_RID = None
        self.COD_IN_RID = None
        self.F_V_INPUT = None
        self.MEM_COD_IN = None
        self.DEV_IN = None
        self.DISIMPEGNA = None
        self.DA_TRASFERIRE = None
        self.C_MAT_COM = None
        self.C_MAT_TRANSITO = None
        self.MODELLO = None
        self.MOD_RID = None
        self.COLLEZIONE = None
        self.CLASSE = None
        self.STAGIONE = None
        self.PROGR_MOD = None
        self.ANNO = None
        self.ARTICOLO = None
        self.ART_RID = None
        self.GR_MERC = None
        self.None = None
        self.VEST_A = None
        self.PEZZO_A = None
        self.PROGR_ART = None
        self.None = None
        self.None = None
        self.PREFISSO_V_F = None
        self.SOCIETA_MOD = None
        self.COLORE = None
        self.C_MAT_TRANS_RID = None
        self.COM_QTA_DISP = None
        self.TABELLA_ARTICOLI_LETTI = None
        self.ART_TAB_LETTI = None
        self.TAB_ART = None
        self.D_MAT_TAB = None
        self.PRIMA_TG_TAB = None
        self.PREZZO_TAB = None
        self.CAMBIO_TAB = None
        self.TIPO_ANA_TAB = None
        self.QTA_GIAC_TAB = None
        self.QTA_GIAC_PF_TAB = None
        self.QTA_TAGLIE_TAB = None
        self.QTA_TAGLIA_TAB = None
        self.COSTO_TAB = None
        self.ART_ELEM_LETTI = None
        self.ELEM_ART = None
        self.D_MAT_ELEM = None
        self.PRIMA_TG_ELEM = None
        self.PREZZO_ELEM = None
        self.CAMBIO_ELEM = None
        self.TIPO_ANA_ELEM = None
        self.QTA_GIAC_ELEM = None
        self.QTA_GIAC_PF_ELEM = None
        self.QTA_TAGLIE_ELEM = None
        self.QTA_TAGLIA_ELEM = None
        self.COSTO_ELEM = None
        self.QTA_TAGLIE_NEG = None
        self.QTA_TAGLIA_NEG = None
        self.NUM_ELEM_MAX_ART = 5000
        self.PARTAB_ART = None
        self.QT_PARAMETRI = None
        self.QT_STATO = None
        self.QT_NUM_ELEM_EFF = None
        self.QT_NUM_ELEM_MAX = None
        self.QT_LL_ELEM = None
        self.QT_ADDR_KEY = None
        self.QT_LL_KEY = None
        self.QT_INDEX_ELEM = None
        self.QT_FUNZIONE = None
        self.TABELLA_NO_GIAC = None
        self.ELEM_NO_GIAC = None
        self.C_MAT_NO_GIAC = None
        self.PREZZO_NO_GIAC = None
        self.D_MAT_NO_GIAC = None
        self.CAUSALE_NO_GIAC = None
        self.CAUSALE_NO_PRZ = None
        self.IND_CAPI_NO_GIAC = None
        self.COMANDO_BUILD = None
        self.None = "BUILD P"
        self.BUILD_N_DDT = None
        self.None = ";REC=-132,3,F,ASCII;DISC=20000"
        self.CARRIAGE_RETURN = X"13"
        self.COMANDO_FILE = None
        self.None = "FILE PEND=P"
        self.FILE_N_DDT = None
        self.CARRIAGE_RETURN = X"13"
        self.COMANDO_FILE_2 = None
        self.None = "FILE P"
        self.FILE_N_DDT_2 = None
        self.None = ";DEV=91,5"
        self.CARRIAGE_RETURN = X"13"
        self.COMANDO_PURGE = None
        self.None = "PURGE P"
        self.PURGE_N_DDT = None
        self.CARRIAGE_RETURN = X"13"
        self.COMANDO_PRINT = None
        self.None = "PRINT P"
        self.PRINT_N_DDT = None
        self.None = ";OUT=*P"
        self.PRINT_N_DDT_2 = None
        self.CARRIAGE_RETURN = X"13"
        self.COMANDO_LPR_LINUX = None
        self.None = "lpr -P p1"
        self.LPR_NUM_STAMPANTE = None
        self.None = " "
        self.LPR_NOME_FILE = None
        self.None = X"00"
        self.WK_VAR_NAME = None
        self.WK_VAR_VALUE = None
        self.DIR_VAR_NAME = None
        self.DIR_VAR_VALUE = None
        self.USER_VAR_NAME = None
        self.USER_VAR_VALUE = None
        self.FILE_VAR_NAME = None
        self.FILE_VAR_VALUE = None
        self.ERR = 0
        self.ERR_PARM = 0
        self.LOCALITA_PART_STR = None
        self.IND_LOC = None
        self.MAX_LOC = 4
        self.TAB_LOC_EXP = None
        self.None = 2
        self.None = "Magazzino Via Santi 8, Cavriago (R.E.) "
        self.None = 3
        self.None = "Magazzino Via Santi 5, Cavriago (R.E.) "
        self.None = 6
        self.None = "Magazzino Via Santi 8, Cavriago (R.E.) "
        self.None = "Magazzino Via Santi 8, Cavriago (R.E.) "
        self.None = 7
        self.None = "Magazzino Via Santi 8, Cavriago (R.E.) "
        self.TAB_LOC = None
        self.None = None
        self.COD_LOC = None
        self.DESC_LOC = None
        self.RIGA_DISP = None
        self.DATI_DISP_4 = None
        self.None = None
        self.DISP_ART = None
        self.None = None
        self.PARE1 = None
        self.QTA_DISP = None
        self.PARE2 = None
        self.TOT_CAPI_LETTI_1 = None
        self.TAB_LOCK = None
        self.None = 4
        self.None = 17
        self.None = "DPARAM;"
        self.None = "@"
        self.None = 17
        self.None = "SITPF;"
        self.None = "@"
        self.None = 17
        self.None = "MOVMAG;"
        self.None = "@"
        self.None = 17
        self.None = "BOLLE;"
        self.None = "@"
        self.STK_NOME = None
        self.STK_C_MAT = None
        self.STK_STAGIONE = None
        self.STK_SCO = None
        self.STK_COLL = None
        self.STK_PRZ_SCO = None
        self.STK_PRZ_LORDO = None
        self.STK_MSG = None
        self.STK_MSG_1 = None
        self.STK_MSG_2 = None
        self.STK_PRIMA_VOLTA = None
        self.STK_CAMBIO = None
        self.NOME_IN = "TABSTK.TABELLE"
        self.NOME_IN_B = "TABSTB.TABELLE"
        self.PAR_PRINT = None
        self.STATO = None
        self.LL_RIGA = None
        self.N_MAX_RIGHE = None
        self.FLAG_ROUTINE = None
        self.NUM_FILE_ID = None
        self.NOME_FILE = None
        self.PRE_NOME_FILE = None
        self.TERM_N_FILE = None
        self.None = ".ST"
        self.RIGA = None
        self.N_STAMPANTE = None
        self.COMANDO = None
        self.N_RIGA_STAMPA = None
        self.DATI_RIGA = None
        self.RIGA_INTESTA = None
        self.None = None
        self.CONTO_T = None
        self.None = None
        self.D_CONTO_T = None
        self.None = None
        self.DATA_T = None
        self.None = None
        self.D_PAG_T = None
        self.None = None
        self.NUM_PAG_T = None
        self.RIGA_DETTAGLIO = None
        self.None = None
        self.C_MAT_ST = None
        self.None = None
        self.TAGLIA_ST = None
        self.None = None
        self.NOME_MOD_ST = None
        self.None = None
        self.PREZZO_ST = None
        self.PREZZO_ST_EU = None
        self.None = None
        self.NOGIAC_ST = None
        self.None = None
        self.NOPRZ_ST = None
        self.BUFFER = None
        self.N_BUF = 37
        self.None = None
        self.None = None
        self.MAG_INPUT = None
        self.MAG_INPUT_R = None
        self.SUGG_MAG_DISP = "accettato MAG 1, 4, 6, 7, 852 o 853"
        self.CLASSE_INPUT = None
        self.CLASSE_INPUT_R = None
        self.SOCIETA_INPUT = None
        self.SOCIETA_INPUT_R = None
        self.AS_INPUT = None
        self.A_INPUT_R = None
        self.S_INPUT_R = None
        self.None = None
        self.AS_INPUT_R = None
        self.STRINGA_TUTTI_AS = None
        self.None = "02.04.12.14.22.24.32.34.42.44."
        self.None = "52.54.62.64.72.74.82.84.92.94."
        self.ELENCO_AS = None
        self.None = None
        self.AS_IN = None
        self.AS_IN_R = None
        self.None = None
        self.ELENCO_CL = None
        self.None = None
        self.CL_IN = None
        self.CL_IN_R = None
        self.None = None
        self.I_APP = None
        self.I_AS = None
        self.I_CL = None
        self.PRO = None
        self.SINO = None
        self.APP_AS = None
        self.None = None
        self.APP_A = None
        self.APP_S = None
        self.SW_AS_ERR = None
        self.SW_CL_ERR = None
        self.SW_MAX_CAPI_RAGGIUNTO = None
        self.SW_ERR_AS_CL = None
        self.SW_FINE_CARICA_TAB = None
        self.XD = "N"
        self.PAR_TAB_UNICO_DDT = None
        self.QT_PARAMETRI = None
        self.QT_STATO = None
        self.QT_NUM_ELEM_EFF = None
        self.QT_NUM_ELEM_MAX = None
        self.QT_LL_ELEM = None
        self.QT_ADDR_KEY = None
        self.QT_LL_KEY = None
        self.QT_INDEX_ELEM = None
        self.QT_FUNZIONE = None
        self.TAB_UNICO_DDT = None
        self.ELE_TAB_UNICO_DDT = None
        self.DEP_TAB_UNICO_DDT = None
        self.KEY_TAB_UNICO_DDT = None
        self.TAB_AS = None
        self.TAB_CL = None
        self.DATI_TAB_UNICO_DDT = None
        self.TAB_MAX_CAPI = None
        self.TAB_CAPI_LETTI = None
        self.NOME_IN_35 = None
        self.NOME_IN_3 = None
        self.NOME_IN_5 = None
        self.None = ".TABELLE"
        self.PREZZO_ANAMAT = None
        self.PARDEED = None
        self.QD_PARAMETRI = None
        self.QD_STATO = None
        self.QD_NR_DEC = None
        self.QD_LL_A = None
        self.QD_LL_B = None
        self.None = None
        self.PAR_INEU = None
        self.IE_PARAMETRI = None
        self.IE_STATO = None
        self.IE_IMPORTO_IN = None
        self.IE_IMPORTO_IN_V = None
        self.IE_IMPORTO_OU = None
        self.IE_IMPORTO_OU_V = None
        self.IE_DIVISA_IN = None
        self.IE_DIVISA_OU = None
        self.IE_MSG = None
        self.DIVISA_PRIMO_LETTO = None
        self.DESTINO_USCITA = None
        self.CONTO_FATTURA_MEM = None
        self.TAB_RIGHE_SOCIETA = None
        self.TAB_MAPPA_SOCIETA = None
        self.None = 9
        self.SOC_1 = "CA"
        self.COD_1 = 1
        self.SOC_2 = "MA"
        self.COD_2 = 2
        self.SOC_3 = "MN"
        self.COD_3 = 3
        self.SOC_4 = "MR"
        self.COD_4 = 4
        self.SOC_5 = "DT"
        self.COD_5 = 5
        self.SOC_6 = "MM"
        self.COD_6 = 6
        self.SOC_7 = "YE"
        self.COD_7 = 7
        self.SOC_8 = "MH"
        self.COD_8 = 8
        self.SOC_9 = "IM"
        self.COD_9 = 9
        self.TAB_MAPPA_SOCIETA_RID = None
        self.NUMERO_MAX_SOCIETA = None
        self.EL_TAB_SOC = None
        self.SOCIETA_SIGLA = None
        self.SOCIETA_CODICE = None
        self.PREZZO_PUBBL = None
        self.CC_C_MAT = None
        self.CC_SOCIETA = None
        self.CC_LISTINO = None
        self.CC_TIPO_PREZZO = None
        self.CC_PREZZO_DBG = None
        self.MOVSKU_RIF_INTERNO = None
        self.MOVSKU_CMAT = None
        self.MOVSKU_TG = None
        self.MOVSKU_BARUNI = None
        self.MOVSKU_CONTO = None
        self.MOVSKU_MAG = None
        self.MOVSKU_SKU = None
        self.MOVSKU_IS_BARUNI_READ = None
        self.MOVSKU_IS_BARUNI_CERTIFIED = None
        self.MOVSKU_SKU_FATTURAZIONE = None
        self.ANACST_C_MAT = None
        self.ANACST_CST_STD = None
        self.ANACST_CST_STD_2 = None
        self.ANACST_TS_CST = None
        self.TAB_B2C_NO_DT = None
        self.EL_B2C_NO_DT = None
        self.MAG_B2C_NO_DT = None
        self.SQL_CONST = None
        self.COSTANTI_SQL = None
        self.DEADLOCK = -14024
        self.NO_MEMORY = -4008
        self.MULTIPLE_ROWS = -2112
        self.NOT_FOUND = 100
        self.MODULE_NOT_FOUND = -2216
        self.CONSTR_VIOLATED_DF = -2091
        self.CONSTR_VIOLATED = -2292
        self.OK = +0
        self.SQL_STATUS = None
        self.PAR_ERR = None
        self.ER_NUM = None
        self.ER_IND = None
        self.ER_DISP = None
        self.ER_DESCRIZIONE = None
        self.AREA_HL = None
        self.HL_COMMON_AREA = None
        self.HL_SEND_PAR = None
        self.HL_ENABLE_INPUT = None
        self.HL_WINDOW_ENH = None
        self.HL_BYPASS_FEATURE = None
        self.HL_GLOBAL_PAR = None
        self.HL_EXPECTED_VUF = None
        self.HL_CALL_PROTOCOL = None
        self.HL_COMAREA_LEN = None
        self.HL_COMAREA = None
        self.HL_RETURN_PAR = None
        self.HL_STATUS = None
        self.HL_RETURN_SUBSTATUS = None
        self.HL_RETURN_MSGLENGTH = None
        self.HL_RETURN_MSG = None
        self.HL_LAST_ITEMTYPE = None
        self.HL_LAST_ITEMNUM = None
        self.HL_LAST_ITEMNAME = None
        self.HL_NUM_DATAERRS = None
        self.HL_NUM_CHANGEFIELDS = None
        self.HL_TERM_PAR = None
        self.HL_TERM_FILE = None
        self.HL_BYPASS_FEATURE_2 = None
        self.HL_FORM_PAR = None
        self.HL_FORM_NAME = None
        self.HL_FORM_POSITION = None
        self.HL_CHANGE_LISTTYPE = None
        self.HL_LIST_COUNT = None
        self.HL_CHANGE_ENTRY = None
        self.HL_FIELD_ID = None
        self.HL_CHANGE_TYPE = None
        self.HL_CHANGE_SPEC = None
        self.HL_PROMPT_PAR = None
        self.HL_PROMPT_REPAINT = None
        self.HL_PROMPT_WINDENH = None
        self.HL_PROMPT_RESET = None
        self.HL_READ_PAR = None
        self.HL_READ_TIME = None
        self.HL_ENABLE_FOR = None
        self.HL_DOREREAD = None
        self.HL_FILLER = None
        self.HL_READ_ITEMS = None
        self.HL_READITEM_COUNT = None
        self.HL_READITEM_ENTRY = None
        self.HL_READITEM_TYPE = None
        self.HL_READITEM_ID = None
        self.HL_READITEM_OPTION = None
        self.HL_CURSOR_POSITION = None
        self.HL_CURSOR_POS_NUM = None
        self.HL_CURSOR_POS_NAME = None
        self.HL_PROMPT_CURSOR_POSITION = None
        self.HL_FIL_PROMPT = None
        self.HL_ERR_CURSOR_POS = None
        self.HL_DATA_DESC = None
        self.HL_DESC_TYPE = None
        self.HL_BUFF_LENGTH = None
        self.HL_RET_BUFLEN = None
        self.HL_LABEL_DESC = None
        self.HL_LABEL_NUM = None
        self.HL_LABEL_ELEM = None
        self.HL_LABEL_ID = None
        self.HL_LABEL_ENH = None
        self.HL_LABEL_BUFF = None
        self.HL_LABEL_ENTRY = None
        self.HL_LABEL_TEXT = None
        self.HL_MSG_WINDOW = None
        self.HL_MSG_LENGTH = None
        self.HL_WINDOW = None
        self.HL_FORMCNTRL_PAR = None
        self.HL_FC_NAME = None
        self.HL_FC_UNDLCNTR = None
        self.HL_FC_PAGECNTR = None
        self.HL_FC_ENREF = None
        self.HL_FILLCNTRL_PAR = None
        self.HL_FC_DESC = None
        self.HL_FC_ENTRYCNT = None
        self.HL_FC_FIELDID = None
        self.HL_PRINT_DEV = None
        self.HL_FORMS_FILE = None
        self.HL_UNUSED_PAR = None
        self.HL_ERROR_PAR = None
        self.HL_ERR_LIST_TYPE = None
        self.HL_ERR_LIST_ALLOC = None
        self.HL_ERR_LIST_ACTUAL = None
        self.HL_ERR_FIELD = None
        self.HL_CALL = None
        self.TP_PARAM = None
        self.TP_ERRORI = None
        self.TP_FLAG = None
        self.TP_FLAG_SEND = None
        self.TP_FLAG_READ = None
        self.TP_FLAG_AGG_FASE = None
        self.TP_NUMERO_FASI = None
        self.TP_FASE_ATTUALE = None
        self.TP_SET_KEYF = None
        self.TP_RIGHE = None
        self.TP_RIGHE_FASE = None
        self.TP_PASSO_ROLL = None
        self.TP_RIGA_END = None
        self.TP_RIGA_START = None
        self.TP_RIGHE_EFF = None
        self.TP_IND_RIGA = None
        self.TP_IND_TERM = None
        self.TP_COMANDO = None
        self.TP_FIL1 = None
        self.TP_RESTO_COMANDO = None
        self.TP_RIGA1 = None
        self.TP_START1 = None
        self.None = None
        self.None = None
        self.TP_RIGA2 = None
        self.TP_START2 = None
        self.None = None
        self.None = None
        self.TP_RIGA3 = None
        self.TP_START3 = None
        self.None = None
        self.None = None
        self.None = None
        self.None = None
        self.FILLER_COM = None
        self.TP_RIGA4 = None
        self.TP_START4 = None
        self.TP_RIGA41 = None
        self.TP_START41 = None
        self.TP_RIGA42 = None
        self.TP_START42 = None
        self.TP_RIGA43 = None
        self.TP_START43 = None
        self.TP_INFO = None
        self.TP_DISAB_FKEY = None
        self.TP_NUM_DISAB = None
        self.TP_TABELLA_DISAB = None
        self.TP_COM_DISAB = None
        self.TP_INFO_2 = None
        self.H_COMAREA = None
        self.H_COM_STATUS = None
        self.H_COM_LANGUAGE = None
        self.H_COM_COMAREALEN = None
        self.None = None
        self.H_COM_MODE = None
        self.H_COM_LASTKEY = None
        self.H_COM_NUMERRS = None
        self.None = None
        self.None = None
        self.H_COM_KEYLABOPT = None
        self.H_COM_CFNAME = None
        self.None = None
        self.H_COM_NFNAME = None
        self.None = None
        self.H_COM_REPEATOPT = None
        self.H_COM_NFOPT = None
        self.None = None
        self.H_COM_DBUFLEN = None
        self.None = None
        self.None = None
        self.H_COM_DELETEFLAG = None
        self.H_COM_SHOWCONTROL = None
        self.None = None
        self.None = None
        self.None = None
        self.None = None
        self.H_COM_FRMSTORSIZE = None
        self.None = None
        self.None = None
        self.None = None
        self.H_COM_NUMRECS = None
        self.H_COM_RECNUM = None
        self.None = None
        self.None = None
        self.H_COM_TERMFILENUM = None
        self.None = None
        self.None = None
        self.None = None
        self.None = None
        self.None = None
        self.None = None
        self.H_COM_TERMOPTIONS = None
        self.None = None
        self.None = None
        self.None = None
        self.None = None
        self.AREA_SI = None
        self.SI_AREA_SISTEMA = None
        self.SI_DATI_UTENTE = None
        self.SI_SOTTOSISTEMA = None
        self.SI_TIMEOUT = None
        self.SI_STAMPANTE = None
        self.SI_STFORM = None
        self.SI_CODICE_AZIENDA = None
        self.SI_DATA_DEL_GIORNO = None
        self.SI_FORMATO_INTERNO = None
        self.None = None
        self.SI_FORMATO_GG_MM_AA = None
        self.SI_FORMATO_GG_MMM_AAAA = None
        self.SI_FORMATO_GGMMAAAA = None
        self.None = None
        self.None = None
        self.SI_AAAA = None
        self.SI_FORMATO_GGMMAA = None
        self.SI_GG = None
        self.SI_MM = None
        self.SI_AA = None
        self.SI_FORMATO_GMA_9 = None
        self.SI_PARAMETRI_UTE = None
        self.SI_TIPO = None
        self.SI_DATI_PROCESSO = None
        self.SI_DATI_PROG = None
        self.SI_NOME_PROG = None
        self.SI_NOME_PROC = None
        self.None = None
        self.SI_NOME_GROUP_PROG = None
        self.SI_NOME_ACCT_PROG = None
        self.SI_GROUP = None
        self.SI_USER = None
        self.SI_ACCOUNT = None
        self.SI_SESSION_NAME = None
        self.SI_TERM = None
        self.SI_NOME_FUNZIONE = None
        self.SI_CHIAVE_FUNZIONE = None
        self.SI_NOME_CHIAMANTE = None
        self.None = None
        self.SI_PRIMI4_CHIAMANTE = None
        self.None = None
        self.SI_PAR_SISTEMA = None
        self.SI_AREA_UTENTE = None
        self.SI_FLAG_1 = None
        self.SI_FLAG_2 = None
        self.SI_DEV_OUTCLASS = None
        self.SI_PRI_OUTCLASS = None
        self.SI_PRIORITA_OUT = None
        self.SI_PRIORITA_JOB = None
        self.SI_DATA_VALUTA = None
        self.SI_DATA_RIFERIMENTO = None
        self.SI_FLAG_COMMESSA = None
        self.FLAG_NUM_EFFETTI = None
        self.SI_DIVISA_DEFAULT = None
        self.SI_PREVIS_OLTRE = None
        self.None = None
        self.SI_PAR_IVA = None
        self.SI_CODA_DEFAULT = None
        self.SI_DIVISA_EURO = None
        self.SI_DATA_INIZIO_EURO = None
        self.SI_DATA_INIZIO_EURO_NUM = None
        self.SI_DATA_CONTAB_EURO = None
        self.SI_DATA_CONTAB_EURO_NUM = None
        self.SI_DIVISA_ITL = None
        self.None = None
        self.SI_AREE = None
        self.SI_DESCR_FUNZIONE = None
        self.SI_NR_FUNZIONE = None
        self.SI_FLAG_DISABLETERM = None
        self.SI_AREA_MSG = None
        self.SI_FLAG_BATCH = None
        self.SI_INFO_AREA_LIBERA = None
        self.None = None
        self.SI_LINGUA_USER = None
        self.SI_CAMBIO_EURO_LIRA = None
        self.SI_DB_CONNESSO = None
        self.SI_PASSWORD = None
        self.SI_DIRECTORY = None
        self.SI_AREE_PARAMETRI = None
        self.SI_PAR_SOTTOSISTEMA = None
        self.SI_PAR_FUNZIONE = None
        self.None = None
        self.SI_FLAG_UPD = None
        self.SI_FLAG_DEL = None
        self.SI_RESTO_PAR_FUN = None
        self.FLAG_CURSORE = None
        self.SQLCODE_MEM = None
        self.TASTO_INP = None
        self.COM_IGP_STANDARD = None
        self.COMIGP_PTXSETUP = "!PTX_SETUP"
        self.COMIGP_PTXCFG2 = "CONFIG-LOAD;2"
        self.COMIGP_PTXEND = "PTX_END"
        self.COMIGP_CREATE = "^CREATE;LAB;144"
        self.COMIGP_EXECUTE = "^EXECUTE;LAB;1"
        self.COMIGP_SCALEDOT = "SCALE;DOT;300;300"
        self.COMIGP_ALPHA = "ALPHA"
        self.COMIGP_STOP = "STOP"
        self.COMIGP_END = "END"
        self.COMIGP_LISTEN = "^LISTEN"
        self.COMIGP_QUIET = "^QUIET"
        self.ROW_IGP = None
        self.ROW_X_IGP = None
        self.COL_IGP = None
        self.COL_X_IGP = None
        self.VERT_EXP_IGP = None
        self.ORIZ_EXP_IGP = None
        self.TEMP_X_IN = None
        self.TEMP_EL_X = None
        self.TEMP_X_OUT = None
        self.PRIMO_NONZERO = None
        self.K = None
        self.RIGA_PEND_COM = None
        self.IND_PEND = None
        self.MAX_CAPI_INPUT = None
        self.MAX_CAPI_INPUT_R = None
        self.COSTO_MEM = None
        self.PY_INPUT_REC = None
        self.INPUT_VAL = None
        self.None = ";"
        self.INPUT_VAL_B = None
        self.None = ";"
        self.INPUT_VAL_C = None
        self.PY_OUTPUT_REC = None
        self.OUTPUT_VAL_A = None
        self.OUTPUT_VAL_B = None
        self.OUTPUT_VAL_B_OK = None
        self.T_TAB = X"9"
        self.IND_BARUNI = None
        self.PY_INPUT_TRASF = None
        self.INPUT_RIF_TRASF = None
        self.INPUT_CONTO_TRASF = None
        self.PY_OUTPUT_TRASF = None
        self.OUTPUT_STATO_TRASF = None
        self.OUTPUT_ERR_TRASF = None
        self.PY_INPUT_REC_DISIMPEGNA = None
        self.LISTA_AS = None
        self.AS_DISIMPEGNA = None
        self.ANNO_DISIMPEGNA = None
        self.STAG_DISIMPEGNA = None
        self.MAG_DISIMPEGNA = None
        self.FORN_DISIMPEGNA = None
        self.LISTA_CLASSE_DISIMPEGNA = None
        self.CLASSE_DISIMPEGNA = None
        self.PY_OUTPUT_DISIMPEGNO = None
        self.FLAG_B2C_NO_DT = None
        self.NUM_B2C_NO_DT = None
        self.IND_B2C_NO_DT = None
        self.ANACST_MAG_COM = None
        self.ANACST_C_MAT_COM = None
        self.ANACST_CST_COM = None
        self.FLAG_ANACST = None
        self.PY_INPUT_REC_B = None
        self.INPUT_RIF_INTR = None
        self.INPUT_FLAG = None
        self.INPUT_CONTO = None
        self.INPUT_MAG = None
        self.PY_OUTPUT_REC_B = None
        self.OUTPUT_VAL = None
        self.W_COMMON = None
        self.W_SOTTOSISTEMA = None
        self.W_DATI_W_IMAGE = None
        self.W_NOME_DATA_BASE_1 = None
        self.W_TAB_DB = None
        self.W_NOME_DB = None
        self.None = None
        self.W_NOME_DB_1 = None
        self.W_NOME_DB_2 = None
        self.W_NOME_DB_3 = None
        self.W_NOME_DB_4 = None
        self.W_NOME_DB_5 = None
        self.W_NOME_DB_6 = None
        self.W_NOME_DB_7 = None
        self.W_NOME_DB_8 = None
        self.W_MODO = None
        self.W_NOME_CAMPO = None
        self.W_VALORE_CAMPO = None
        self.W_DB_KEY_P16 = None
        self.W_VAL_1 = None
        self.W_VALORE_CAMPO_W = None
        self.W_DB_KEY_W = None
        self.None = None
        self.W_VAL_2 = None
        self.W_VALORE_CAMPO_HW = None
        self.W_DB_KEY_HW = None
        self.None = None
        self.W_VAL_3 = None
        self.W_VALORE_CAMPO_X4 = None
        self.W_DB_KEY_X4 = None
        self.None = None
        self.W_VAL_4 = None
        self.W_VALORE_CAMPO_12 = None
        self.None = None
        self.W_VAL_5 = None
        self.W_VALORE_CAMPO_15 = None
        self.W_NOME_DATA_SET = None
        self.W_PAROLA_CHIAVE_1 = None
        self.W_TUTTO_RECORD = None
        self.W_CHIUSURA_TOTALE = None
        self.W_CA_IMAGE = None
        self.W_STATUS_WORD_IMAGE = None
        self.W_WORD_L_BUFF = None
        self.W_WORD_ATT = None
        self.W_WORD_SIN = None
        self.W_WORD_PREC = None
        self.W_WORD_SEG = None
        self.W_DATI_W_DEL = None
        self.W_CA_DEL = None
        self.W_STATUS_WORD_DEL = None
        self.W_SW = None
        self.W_SW_1 = None
        self.W_SW_2 = None
        self.W_SW_3 = None
        self.W_SW_4 = None
        self.W_SW_5 = None
        self.W_SW_6 = None
        self.W_SW_7 = None
        self.W_SW_8 = None
        self.W_SW_9 = None
        self.W_SW_10 = None
        self.None = None
        self.W_NOME_MODULO = None
        self.W_LL_MODULO = None
        self.W_NOME_PROX_MODULO = None
        self.W_CAMPO_ERRATO = None
        self.W_RIGA = None
        self.W_COLONNA = None
        self.None = None
        self.W_SYSMODULI = None
        self.W_PROX_MODULO = None
        self.W_TERMINALE = None
        self.W_DATI_VIEW = None
        self.W_CA_VIEW = None
        self.W_CSTATUS = None
        self.W_LINGUAGGIO = None
        self.W_LL_CA_VIEW = None
        self.W_EXT_BASIC = None
        self.W_COLLECT_BROWSE = None
        self.W_ULT_TASTO_FUNZ = None
        self.W_NUMERO_ERRORI = None
        self.W_WIND_EN = None
        self.None = None
        self.W_FILLER = None
        self.W_WINDOW_ENH = None
        self.W_MULTIUSAGE = None
        self.W_LABEL_OPTION = None
        self.W_NOME_CFORM = None
        self.W_FILLER = None
        self.W_NOME_NFORM = None
        self.W_FILLER = None
        self.W_FLAG_REPEAT = None
        self.W_FLAG_FREEZE = None
        self.W_NR_RIGHE_CFORM = None
        self.W_LL_BUFFER = None
        self.None = None
        self.W_LOOK_AHEAD = None
        self.W_DELETE_FLAG = None
        self.W_SHOWCONTROL = None
        self.W_FILLER = None
        self.W_PRINTFILE_NUM = None
        self.W_FILERRNUM = None
        self.W_ERRFILNUM = None
        self.W_FORM_STORE_SIZE = None
        self.None = None
        self.None = None
        self.None = None
        self.W_NUM_RECS = None
        self.W_RECNUM = None
        self.None = None
        self.None = None
        self.W_TERMFILENUM = None
        self.None = None
        self.None = None
        self.None = None
        self.None = None
        self.None = None
        self.W_RETRIES = None
        self.W_OPTIONS = None
        self.W_ENVIRON = None
        self.W_USER_TIME = None
        self.W_TERM_IDENTIFIER = None
        self.W_LAB_INFO = None
        self.W_NOME_FORMSFILE = None
        self.W_NOME_TERMINALE = None
        self.W_NOME_FORM_TEST = None
        self.W_NOME_FORM_DETT = None
        self.W_LL_FIELD = None
        self.W_FLAGS_STD = None
        self.W_FLAG_ERRORI_STD = None
        self.W_FLAG_COMANDO = None
        self.W_FLAG_RICERCA = None
        self.W_NR_FIELD = None
        self.W_LL_FIELD_MOSSO = None
        self.W_NR_PROX_FIELD = None
        self.W_PRINT_C = None
        self.W_PAGE_C = None
        self.W_DATI_WINDOW = None
        self.W_LL_MAX_WINDOW = None
        self.W_LL_WINDOW = None
        self.W_MESS_X_WINDOW = None
        self.W_FIL_WINDOW = None
        self.W_RESTO_WINDOW = None
        self.W_CAMPO_ERR = None
        self.W_FIL_ERR = None
        self.W_NUM_ERR = None
        self.W_NUM_ERR_X = None
        self.W_FILLER = None
        self.W_LL_CAMPO_ERR = None
        self.W_LL_CAMPO_ERR_2 = None
        self.W_CAMPI_FLAG_STD = None
        self.W_FLAG_FINISH_TEST = None
        self.W_FLAG_FINISH_DETT = None
        self.W_NUMERO_FORMS = None
        self.W_FLAG_LOOP_TEST = None
        self.W_FLAG_TIPO_AGG = None
        self.W_FLAG_CHIUDI_TEST = None
        self.W_FLAG_CHIUDI_DETT = None
        self.W_LL_DISPLAY = None
        self.W_CTL_DISPLAY = None
        self.W_CAMPO_SIGLA = None
        self.W_PAR_TESTP = None
        self.W_LL_MAX_SIGLA = None
        self.W_LL_SIGLA = None
        self.W_SIGLA_OUT = None
        self.W_POS_RC = None
        self.None = None
        self.W_SIGLA_AZ = None
        self.None = None
        self.None = None
        self.W_CAMPO_COMANDO = None
        self.W_CAMPO_PUNTO = None
        self.W_FILLER = None
        self.W_FIL_COM_SPACE = None
        self.W_COMAND_SPACE_1 = None
        self.W_COMAND_SPACE_2 = None
        self.None = None
        self.W_PAR_SOPTV = None
        self.W_FUNZ_SOPTV = None
        self.W_STATO_SOPTV = None
        self.W_TIPO_SOPTV = None
        self.None = None
        self.W_RIGA_V = None
        self.None = None
        self.W_COLONNA_V = None
        self.W_MESSAGGI_COMANDO = None
        self.W_ULT_MESS_COMANDO = None
        self.None = None
        self.W_ULT_MESS_1 = None
        self.W_NR_PAGINA_10_99 = None
        self.None = None
        self.W_NR_PAGINA_1_9 = None
        self.None = None
        self.None = None
        self.W_ULT_MESS_2 = None
        self.None = None
        self.None = None
        self.None = None
        self.W_ZONA_MESSAGGI_FISSI = None
        self.W_ZONA_MESSAGGI_PUNTO = None
        self.W_NR_MESS_PUNTO = None
        self.W_PUNTO_CS = None
        self.W_PUNTO = None
        self.None = None
        self.W_PUNTO_CF = None
        self.W_PUNTO_CO = None
        self.W_PUNTO_A = None
        self.W_PUNTO_P = None
        self.W_PUNTO_V = None
        self.W_PUNTO_PP = None
        self.W_PUNTO_PS = None
        self.W_PUNTO_I = None
        self.W_PUNTO_AR = None
        self.None = None
        self.None = None
        self.W_MESS_FISSO = None
        self.W_FLAG_LAVORO = None
        self.W_DATI_GENERALI = None
        self.W_SIGLA_TERMINALE = None
        self.W_SIGLA_OUT_ASSOCIATO = None
        self.W_TIPO_TER = None
        self.W_NR_OUT_ASSOCIATO = None
        self.W_TIPO_ASSOCIAZIONE = None
        self.W_INDICI_W_COMMON = None
        self.W_INDICE_1 = None
        self.W_INDICE_2 = None
        self.W_INDICE_3 = None
        self.W_INDICE_4 = None
        self.W_INDICE_5 = None
        self.W_INDICE_6 = None
        self.W_INDICE_7 = None
        self.W_INDICE_8 = None
        self.W_INDICI_W_COMMON_RID = None
        self.W_INDICE = None
        self.W_SIGLA_UTENTE = None
        self.W_DATA_DEL_GIORNO = None
        self.W_FORMATO_INTERNO = None
        self.W_FORMATO_ESTERNO_1 = None
        self.W_FORMATO_GG_MM_AA = None
        self.W_FORMATO_ESTERNO_2 = None
        self.W_FORMATO_GG_MMM_AAAA = None
        self.W_FORMATO_GGMMAA = None
        self.W_GG = None
        self.W_MM = None
        self.W_AA = None
        self.W_PARAMETRI_UTE = None
        self.W_CODICE_FISCALE = None
        self.W_CONTI_BASE = None
        self.W_CLIENTI_ITA = None
        self.W_CLIENTI_EXP = None
        self.W_FORNITORI = None
        self.W_AGENTI_ITA = None
        self.W_AGENTI_EXP = None
        self.W_PORTAF_ITA = None
        self.W_PORTAF_EXP = None
        self.W_I_V_A = None
        self.W_RICAVI_ITA_INV = None
        self.W_RICAVI_ITA_PE = None
        self.W_RICAVI_EXP_INV = None
        self.W_RICAVI_EXP_PE = None
        self.W_RICAVI_MP = None
        self.W_TRASPORTI = None
        self.W_IMBALLI = None
        self.W_SCONTI = None
        self.W_BANCHE = None
        self.W_DEBIT_DIV = None
        self.W_CREDIT_DIV = None
        self.W_EFFETTI_SCONTO = None
        self.W_EFFETTI_SBF = None
        self.W_EFFETTI_DOPO_INCASSO = None
        self.W_SPESE_VARIE_BOLLI = None
        self.W_RICAVI_CAUZ = None
        self.W_COSTI = None
        self.W_FORN_ANTICIPI = None
        self.W_CLIENTI_ANTICIPI = None
        self.W_CONTO_RAG_1 = None
        self.W_CONTO_RAG_2 = None
        self.W_PERDITE_PROFITTI = None
        self.W_BILANCIO_CHIUSURA = None
        self.W_BILANCIO_APERTURA = None
        self.W_CONTI_BASE_RID = None
        self.W_CONTO_BASE = None
        self.W_TIPO_LANCIO = None
        self.W_NUM_TERM = None
        self.W_UTENTE = None
        self.W_GRUPPO = None
        self.W_ACCOUNT = None
        self.W_IND_REC_PARAM = None
        self.AREE_VARIE_MENUTREE = None
        self.W_DIRECTORY = None
        self.W_PARAMETRI_FUNZIONE = None
        self.W_PAR_FUNZ_RID = None
        self.W_FLAGS = None
        self.W_FLAG_1 = None
        self.W_FLAG_2 = None
        self.W_FLAG_3 = None
        self.W_FLAG_4 = None
        self.W_FLAG_5 = None
        self.W_FLAG_6 = None
        self.W_FLAG_7 = None
        self.W_FLAG_8 = None
        self.None = None
        self.SALTO = None

        
    def READVE3(self):	# Linea Source Cobol: non definita
        pass

    def INIZIO(self):	# Linea Source Cobol: 2898
        self.USCITA_PROGRAMMA = 0
        self.CONT = 0
        self.USCITA_DEVICE = 0
        self.AA_MM_GG_DDT = self.W_FORMATO_INTERNO
        self.TRATTA_DEV()
        self.S_SET_1()
        self.CARICA_B2C_NO_DT()
        self.TRATTA_IMPEGNATO()
        self.TRATTA_NEG()

    def FINE(self):	# Linea Source Cobol: 2921
        pass

    def TRATTA_DEV(self):	# Linea Source Cobol: 2927
        self.DEV_IN = SPACES
        print("Disp. USCITA >> ")
        self.DEV_IN = input()
        self.QD_LL_A = 2
        self.QD_LL_B = 2
        self.QD_STATOOFPARDEED = 0
        self.QD_NR_DEC = 0
        print("Dest. USCITA Err. ",self.DEV_IN)
        self.USCITA_DEVICE = 1

    def TRATTA_IMPEGNATO(self):	# Linea Source Cobol: 2946
        self.DISIMPEGNA = SPACES
        print("Si vuole eliminare impegnato ?(SI/NO)")
        self.DISIMPEGNA = input()
        self.DISIMPEGNA = "SI"
        self.DISIMPEGNA = "NO"
        print("Input non valido.")

    def TRATTA_NEG(self):	# Linea Source Cobol: 2962
        self.IND_RIGA = 100
        self.OK_NEG = 0
        self.IND_PAG = 0
        self.PREZZO_TOT = 0
        self.VERIF_NEG()
        self.MAG_INPUT = SPACE
        self.VERIF_MAG()
        self.F_V_INPUT = SPACE
        self.VERIF_F_V()
        self.OK_NEG = 0
        self.VERIFICA_SOC()
        self.CARICA_TAB_UNICO_DDT()
        self.OK_NEG = 0
        self.CICLO_DISIMPEGNO()
        self.INIZIA_TAB_ART()
        self.INIZIA_TAB_SING()
        self.TRATTA_OLD_NEW()
        self.TRATTA_LETTI()

    def VERIFICA_SOC(self):	# Linea Source Cobol: 3027
        self.SOCIETA_INPUT = SPACE
        print("Soc >> (vuoto = tutti) ")
        self.SOCIETA_INPUT = input()
        self.QD_LL_A = 1
        self.QD_LL_B = 1
        self.QD_STATOOFPARDEED = 0
        self.QD_NR_DEC = 0
        print("Soc Err. ",self.SOCIETA_INPUT)
        self.OK_NEG = 1

    def CARICA_TAB_UNICO_DDT(self):	# Linea Source Cobol: 3046
        self.INIT_PAR_TAB_UNICO_DDT()
        self.SW_FINE_CARICA_TAB = 0
        self.VERIFICA_AS()
        self.VERIFICA_CL()
        self.VERIFICA_MAX_CAPI()
        self.TAB_ASOFDEP_TAB_UNICO_DDT = self.AS_IN_R(I_AS)
        self.TAB_CLOFDEP_TAB_UNICO_DDT = self.CL_IN_R(I_CL)
        self.TAB_MAX_CAPI = self.MAX_CAPI_INPUT_R
        self.TAB_CAPI_LETTI = 0
        self.INS_TAB_UNICO_DDT()
        print("---------------------------")
        print("AS CL MAX-CAPI INSERITI")
        self.MOSTRA_TAB_UNICO_DDT()
        print("CORRETTI? 'SI' per proseguire ","'NO' per rifare elenco AS CL da capo")
        self.SINO = "NO"
        self.SINO = input()
        print("sono dopo mostra-tab-unico")

    def VERIFICA_AS(self):	# Linea Source Cobol: 3101
        print("AS >> (tt=tutti) (elenco separato da ,) (CHIUDI)")
        self.ELENCO_AS = SPACES
        self.ELENCO_AS = input()
        self.SW_FINE_CARICA_TAB = 1
        print("Tratto tutti gli AS")
        self.ELENCO_AS = self.STRINGA_TUTTI_AS
        self.SW_AS_ERR = 0
        self.QD_LL_A = 2
        self.QD_LL_B = 2
        self.QD_STATOOFPARDEED = 0
        self.QD_NR_DEC = 0
        print("AS Err. ",self.AS_IN(I_AS))
        self.SW_AS_ERR = 1

    def VERIFICA_CL(self):	# Linea Source Cobol: 3135
        self.ELENCO_CL = SPACES
        print("CL >> (elenco separato da ,)")
        self.ELENCO_CL = input()
        print("Tratto tutti le CL")
        self.ELENCO_CL = SPACES
        self.CL_IN_R(I_APP) = self.I_APP
        self.SW_CL_ERR = 0
        self.QD_LL_A = 2
        self.QD_LL_B = 2
        self.QD_STATOOFPARDEED = 0
        self.QD_NR_DEC = 0
        print("Cl Err. ",self.CL_IN(I_CL))
        self.SW_CL_ERR = 1

    def VERIFICA_MAX_CAPI(self):	# Linea Source Cobol: 3169
        print("MaxCapi >> (vuoto = tutti) ")
        self.MAX_CAPI_INPUT = input()
        self.QD_LL_A = 6
        self.QD_LL_B = 6
        self.QD_STATOOFPARDEED = 0
        self.QD_NR_DEC = 0
        print("MaxCapi Err. ")

    def CALL_DISIMPEGNA_MAG(self):	# Linea Source Cobol: 3249
        pass

    def INIZIA_TAB_ART(self):	# Linea Source Cobol: 3258
        self.QT_STATOOFPARTAB_ART = 0
        self.QT_NUM_ELEM_EFFOFPARTAB_ART = 0
        self.QT_INDEX_ELEMOFPARTAB_ART = 0
        self.QT_NUM_ELEM_MAXOFPARTAB_ART = self.NUM_ELEM_MAX_ART
        self.QT_FUNZIONEOFPARTAB_ART = "K1"
        self.QT_ADDR_KEYOFPARTAB_ART = 1
        self.QT_LL_KEYOFPARTAB_ART = 8

    def INIZIA_TAB_SING(self):	# Linea Source Cobol: 3277
        self.QT_STATOOFPARTAB_SING = 0
        self.QT_NUM_ELEM_EFFOFPARTAB_SING = 0
        self.QT_INDEX_ELEMOFPARTAB_SING = 0
        self.QT_NUM_ELEM_MAXOFPARTAB_SING = 9999
        self.QT_FUNZIONEOFPARTAB_SING = "K1"
        self.QT_LL_ELEMOFPARTAB_SING = 36
        self.QT_ADDR_KEYOFPARTAB_SING = 1
        self.QT_LL_KEYOFPARTAB_SING = 10

    def LEGGI_PARAMDT(self):	# Linea Source Cobol: 3291
        self.W_NOME_DATA_SET = "DPARAM;"
        self.W_NOME_CAMPO = "C-AZIENDA;"
        self.W_VALORE_CAMPO_HW = 0
        self.TTDBFIND()
        self.W_MODO = 5
        self.TTDBGET()
        self.REC_PARAM_RID = self.AREA_REC_SET
        self.DBGET_PARAMDT()
        self.INDIRIZZO_DPARAM = self.W_WORD_ATT
        self.NUMERO_DDT = self.NUM_BOLLA_TAGLIO_FODERE

    def DBGET_PARAMDT(self):	# Linea Source Cobol: 3316
        self.TTDBGET()
        self.REC_PARAM_RID = self.AREA_REC_SET

    def LEGGI_PARAMDT_FITTIZI(self):	# Linea Source Cobol: 3324
        self.W_NOME_DATA_SET = "DPARAM;"
        self.W_NOME_CAMPO = "C-AZIENDA;"
        self.W_VALORE_CAMPO_HW = 0
        self.TTDBFIND()
        self.W_MODO = 5
        self.TTDBGET()
        self.REC_PARAM_FITTIZ_R = self.AREA_REC_SET
        self.DBGET_PARAMDT_FTZ()
        self.INDIRIZZO_DPARAM = self.W_WORD_ATT
        self.NUMERO_DDT = self.PAR_FITTIZIO_1

    def DBGET_PARAMDT_FTZ(self):	# Linea Source Cobol: 3349
        self.TTDBGET()
        self.REC_PARAM_FITTIZ_R = self.AREA_REC_SET

    def VERIF_NEG(self):	# Linea Source Cobol: 3360
        self.D_CONTO_MEM = SPACES
        self.INDIRIZZO_COM = SPACES
        self.INDIRIZZO_C_COM = SPACES
        self.LOCALITA_COM = SPACES
        self.LOCALITA_C_COM = SPACES
        self.CONTO_IN = SPACES
        self.PROV_COM = SPACES
        self.PROV_C_COM = SPACES
        self.CAP_COM = 0
        self.CAP_C_COM = 0
        print(SPACE)
        print("CONTO cliente (8 cifre)")
        print("  (END/end=fine)")
        self.CONTO_IN = input()
        self.OK_NEG = 1
        self.USCITA_PROGRAMMA = 1
        print("CODICE non numerico")
        self.W_NOME_DATA_SET = "ANACON;"
        self.W_NOME_CAMPO = "CONTO;"
        self.W_VALORE_CAMPO_W = self.CONTO_IN_R
        self.W_MODO = 7
        self.TTDBGET()
        print("Manca CLIENTE ",self.CONTO_IN_R)
        self.D_CONTO_MEM = self.D_CONTOOFREC_ANACON
        self.FLAG_ANACON = self.FLAG_ANA_8
        self.FLAG_DT_ESTERO = 1
        self.FLAG_DT_ESTERO = 0
        self.LEGGI_IND()
        self.MUOVI_IND()
        self.MUOVI_CAP()
        self.SCEGLI_CONTO_FATTURA()
        self.CERCA_LISTINO()
        self.OK_NEG = 1

    def TRATTA_OLD_NEW(self):	# Linea Source Cobol: 3420
        print(SPACE)
        print(self.D_CONTO_MEM)
        print("dal mag ",self.MAG_INPUT)
        self.IND_CL = 0
        self.TOT_BOLLA_C = 0
        self.IND_CAPI_LETTI = 0
        self.IND_CAPI_NO_GIAC = 0
        self.TABELLA_ARTICOLI_LETTI = LOW-VALUE
        self.COD_IN = SPACES
        self.MEM_COD_IN = SPACES
        self.TRATTA_SITPF_3()
        print(" S stampa rapportino")
        self.COD_IN = SPACES
        self.COD_IN = input()
        self.STAMPA_RAPPORTINO()
        print("   rapportino stampato")

    def LEGGI_IND(self):	# Linea Source Cobol: 3463
        self.W_NOME_CAMPO = "CONTO;"
        self.W_NOME_DATA_SET = "INDIRIZ;"
        self.TTDBFIND()
        self.W_MODO = 5
        self.TTDBGET()
        self.REC_INDIRIZZI = self.AREA_REC_SET
        print("ERR INDIRIZZI",self.CONTO_IN_R)
        self.AZZERA_CAMPI_INDIRIZ()

    def AZZERA_CAMPI_INDIRIZ(self):	# Linea Source Cobol: 3478
        self.D_AGG = SPACE
        self.STATOOFREC_INDIRIZZI = SPACE
        self.SIGLA_PROVOFREC_INDIRIZZI(1) = SPACE
        self.SIGLA_PROVOFREC_INDIRIZZI(2) = SPACE
        self.INDIRIZZOOFREC_INDIRIZZI(1) = SPACE
        self.INDIRIZZOOFREC_INDIRIZZI(2) = SPACE
        self.STATOOFREC_INDIRIZZI = SPACE
        self.CAPOFREC_INDIRIZZI(1) = 0
        self.CAPOFREC_INDIRIZZI(2) = 0
        self.PRIORITAOFREC_INDIRIZZI = 0
        self.TELEFONOOFREC_INDIRIZZI = 0
        self.TELEXOFREC_INDIRIZZI = 0

    def MUOVI_IND(self):	# Linea Source Cobol: 3497
        self.INDIRIZZO_STD = self.INDIRIZZOOFREC_INDIRIZZI(1)
        self.LL_STRINGA_IND = 66
        self.LL_SUBSTRINGA_IND = 60
        self.FUNZIONE_IND = 1
        self.FUNZIONE_IND = 2
        self.INDIRIZZO_STD = self.INDIRIZZOOFREC_INDIRIZZI(2)
        self.FUNZIONE_IND = 1
        self.FUNZIONE_IND = 2

    def MUOVI_CAP(self):	# Linea Source Cobol: 3527
        self.D_CONTO_AGG_MEM = self.D_AGGOFREC_INDIRIZZI
        self.STATO_COM = self.STATOOFREC_INDIRIZZI
        self.CAP_COM = self.CAPOFREC_INDIRIZZI(1)
        self.PROV_COM = self.SIGLA_PROVOFREC_INDIRIZZI(1)
        self.PROV_C_COM = self.SIGLA_PROVOFREC_INDIRIZZI(2)
        self.CAP_C_COM = self.CAPOFREC_INDIRIZZI(2)

    def CERCA_LISTINO(self):	# Linea Source Cobol: 3541
        self.W_NOME_CAMPO = "CONTO;"
        self.W_NOME_DATA_SET = "CONFATT;"
        self.W_VALORE_CAMPO_W = self.CONTO_IN_R
        self.TTDBFIND()
        self.W_MODO = 5
        self.TTDBGET()
        self.REC_CONFATT = self.AREA_REC_SET
        self.LISTINO_MEM = self.LISTINOOFREC_CONFATT
        self.DIVISA_MEM = self.DIVISAOFREC_CONFATT

    def SCEGLI_CONTO_FATTURA(self):	# Linea Source Cobol: 3556
        self.CONTO_FATTURA_MEM = 0
        self.CONTO_FATTURA_MEM = self.TELEXOFREC_INDIRIZZI

    def TRATTA_SITPF_3(self):	# Linea Source Cobol: 3569
        self.W_NOME_DATA_SET = "SITPF"
        self.W_NOME_CAMPO = "MAG"
        self.W_VALORE_CAMPO_HW = self.MAG_INPUT_R
        self.TTDBFIND()
        self.W_MODO = 5
        self.TTDBGET_S()
        self.SELEZIONA_SITPF_3()
        print(SPACE)
        print("TABELLA-ARTICOLI-LETTI ")
        print(" da allargare")
        print(SPACE)
        print("TABELLA-SINGOLI ")
        print(" da allargare")
        self.COD_IN_RID = "."

    def CALCOLA_AS_CL(self):	# Linea Source Cobol: 3609
        self.SW_ERR_AS_CL = 0
        self.W_VALORE_CAMPO = self.C_MAT_TRANS_RID
        self.W_NOME_DATA_SET = "ANAMAT;"
        self.W_NOME_CAMPO = "C-MAT;"
        self.W_MODO = 7
        self.TTDBGET()
        print("Inesist. ",self.C_MAT_A_BARRE_RID)
        self.SW_ERR_AS_CL = 1
        self.APP_A = self.ANNOOFREC_ANAMAT
        self.APP_S = self.STAGIONEOFREC_ANAMAT
        self.TAB_AS = self.APP_AS
        self.TAB_CL = self.CL_GROFREC_ANAMAT
        self.LEGGI_TAB_UNICO_DDT()
        self.SW_ERR_AS_CL = 1
        self.W_NOME_DATA_SET = "SITPF"
        self.W_MODO = 5

    def SELEZIONA_SITPF_3(self):	# Linea Source Cobol: 3637
        self.TTDBGET_S()
        self.C_MAT_TRANS_RID = self.C_MATOFREC_SITPF
        self.CALCOLA_AS_CL()
        self.TTDBGET_S()
        print("sono dopo calcola-as-cl ",self.C_MAT_TRANS_RID)
        print(self.dep_tab_unico_ddt)
        self.SW_MAX_CAPI_RAGGIUNTO = 1
        self.DEP_TAB_UNICO_DDT = self.ELE_TAB_UNICO_DDT(I_APP)
        self.SW_MAX_CAPI_RAGGIUNTO = 0
        print("sono dopo calcolo max capi raggiunto = ",self.SW_MAX_CAPI_RAGGIUNTO)
        self.TTDBGET_S()
        self.TAB_AS = self.APP_AS
        self.TAB_CL = self.CL_GROFREC_ANAMAT
        self.LEGGI_TAB_UNICO_DDT()
        self.TTDBGET_S()
        self.DA_TRASFERIRE = self.QTA_GIAC_PFOFREC_SITPF(IT)
        self.C_MAT_TRANS_RID = self.C_MATOFREC_SITPF
        self.MODELLOOFC_MAT_A_BARRE = self.MODELLOOFC_MAT_TRANSITO
        self.VESTIBILITAOFC_MAT_A_BARRE = self.VEST_AOFC_MAT_TRANSITO
        self.PEZZOOFC_MAT_A_BARRE = self.PEZZO_AOFC_MAT_TRANSITO
        self.SOCIETAOFC_MAT_A_BARRE = self.PROGR_ARTOFC_MAT_TRANSITO
        self.VARIANTE_COLOFC_MAT_A_BARRE = self.COLOREOFC_MAT_TRANSITO
        self.TAGLIAOFC_MAT_A_BARRE = self.FUNCTIONtgxid(IT)
        self.NTG_MEM = self.IT
        self.TRATTA_LEGGI()
        self.W_NOME_DATA_SET = "SITPF"
        self.W_MODO = 5
        self.TTDBGET_S()

    def TRATTA_LEGGI(self):	# Linea Source Cobol: 3770
        print("sono in tratta-leggi ")
        print(self.dep_tab_unico_ddt)
        self.MEM_COD_IN = SPACES
        self.SOC_COM = self.SOCIETAOFC_MAT_A_BARRE
        self.PRE_SOC = 0
        self.SOCIETAOFC_MAT_A_BARRE = self.SOC_COM
        self.QT_FUNZIONEOFPARTAB_ART = "K2"
        self.C_MAT_TRANS_RID = 0
        self.MODELLOOFC_MAT_TRANSITO = self.MODELLOOFC_MAT_A_BARRE
        self.VEST_AOFC_MAT_TRANSITO = self.VESTIBILITAOFC_MAT_A_BARRE
        self.PROGR_ARTOFC_MAT_TRANSITO = self.SOCIETAOFC_MAT_A_BARRE
        self.PEZZO_AOFC_MAT_TRANSITO = self.PEZZOOFC_MAT_A_BARRE
        self.COLOREOFC_MAT_TRANSITO = self.VARIANTE_COLOFC_MAT_A_BARRE
        self.W_VALORE_CAMPO = self.C_MAT_TRANS_RID
        self.W_NOME_DATA_SET = "ANAMAT;"
        self.W_NOME_CAMPO = "C-MAT;"
        self.W_MODO = 7
        self.TTDBGET()
        print("Inesist. ",self.C_MAT_A_BARRE_RID)
        self.D_MAT_MEM = self.D_MATOFREC_ANAMAT
        self.PTG_MEM = self.PRIMA_TGOFREC_ANAMAT
        self.VAL_REC_MEM = self.VALID_RECOFREC_ANAMAT
        self.COSTO_MEM = self.COSTOOFREC_ANAMAT
        self.ANACST_C_MAT_COM = self.C_MATOFREC_ANAMAT
        self.ANACST_MAG_COM = self.MAG_INPUT_R
        self.RIVALUTA_COSTO_ANAMAT()
        self.COSTO_MEM = self.ANACST_CST_COM
        self.RISP_NO_GIAC = SPACE
        self.RISP_NO_PREZZO = SPACE
        self.PREZZO_MEM = 0
        self.CERCA_PREZZO_V()
        print("Manca prezzo al pubblico ")
        print("    ",self.C_MAT_A_BARRE_RID)
        self.PREZZO_MEM = 0
        self.PREZZO_ANAMAT = 0
        self.CAMBIO_MEM = 0
        self.CERCA_PREZZO()
        self.OK_PREZZO = 0
        self.TRATTA_NO_PREZZO()
        self.prezzo_mem = 0
        self.CAMBIO_MEM = self.CAMBIO_TAB(QT_INDEX_ELEMOFPARTAB_ART)
        self.D_MAT_MEM = self.D_MAT_TAB(QT_INDEX_ELEMOFPARTAB_ART)
        self.PREZZO_MEM = self.PREZZO_TAB(QT_INDEX_ELEMOFPARTAB_ART)
        self.D_MAT_ELEM = self.D_MAT_MEM
        self.PRIMA_TG_ELEM = self.PTG_MEM
        self.PREZZO_ELEM = self.PREZZO_MEM
        self.COSTO_ELEM = self.COSTO_MEM
        self.CAMBIO_ELEM = self.CAMBIO_MEM
        self.TIPO_ANA_ELEM = self.VAL_REC_MEM
        self.QTA_TAGLIE_ELEM = LOW-VALUE
        self.QTA_GIAC_ELEM = self.QTA_GIACOFREC_SITPF
        self.ART_ELEM_LETTI = self.ART_TAB_LETTI(QT_INDEX_ELEMOFPARTAB_ART)
        self.D_MAT_MEM = self.D_MAT_ELEM
        self.PTG_MEM = self.PRIMA_TG_ELEM
        print("Manca giac ",self.C_MAT_A_BARRE_RID)
        self.OK_GIAC = 0
        self.TRATTA_NO_GIAC()
        self.INSERISCI_NO_GIAC_PREZZO()
        self.CONT_D = self.IND_CAPI_LETTI
        print(self.CONT_D)
        print("sono in add 1 to ind-capi-letti ")
        print(self.dep_tab_unico_ddt)
        self.IE_IMPORTO_IN = self.PREZZO_MEM
        self.PRZ_INLIT()
        print(self.CONT_D," ",self.D_MAT_ELEM,SPACE,self.PREZZO_D)
        self.RIMETTI_DEP_TAB_UNICO_DDT()
        print("sono in tratta-leggi DOPO RIMETTI-DEP-TAB")
        print("AS=",self.TAB_AS," CL=",self.TAB_CL," MAX-CAPI=",self.TAB_MAX_CAPI," CAPI-LETTI=",self.TAB_CAPI_LETTI)
        self.IE_IMPORTO_IN = self.PREZZO_TOT
        self.PRZ_INLIT()
        self.INSERT_ELEM_SING()
        self.ART_TAB_LETTI(QT_INDEX_ELEMOFPARTAB_ART) = self.ART_ELEM_LETTI
        self.QT_FUNZIONEOFPARTAB_ART = "K1"
        self.ERR_DISP = self.QT_STATOOFPARTAB_ART
        print("ERR INSERIM QTABEL ",self.ERR_DISP," - TRATTA-LEGGI")
        self.MEM_COD_IN = self.COD_IN

    def INSERISCI_NO_GIAC_PREZZO(self):	# Linea Source Cobol: 4099
        print("INSERITO Manca GIAC.")
        self.CAUSALE_NO_GIAC(IND_CAPI_NO_GIAC) = "MancaGIAC"
        print("INS. Manca PREZZO x ESTERO")
        self.CAUSALE_NO_PRZ(IND_CAPI_NO_GIAC) = "MancaPRZ"
        self.C_MAT_NO_GIAC(IND_CAPI_NO_GIAC) = self.C_MAT_A_BARRE_RID
        self.D_MAT_NO_GIAC(IND_CAPI_NO_GIAC) = self.D_MAT_MEM
        self.PREZZO_NO_GIAC(IND_CAPI_NO_GIAC) = self.PREZZO_MEM

    def TTDBGET_S(self):	# Linea Source Cobol: 4120
        pass

    def TTDBGET(self):	# Linea Source Cobol: 4124
        self.W_INDICE_8 = 4

    def TRATTA_NO_GIAC(self):	# Linea Source Cobol: 4151
        self.RISP_NO_GIAC = SPACE
        print("     ",self.D_MAT_ELEM,SPACE,self.PREZZO_D)
        print("CONFERMI MANCA GIAC ? (S/N)")
        self.RISP_NO_GIAC = "N"
        print(self.RISP_NO_GIAC)
        self.OK_GIAC = 1

    def TRATTA_NO_PREZZO(self):	# Linea Source Cobol: 4177
        self.RISP_NO_PREZZO = SPACE
        self.RISP_NO_PREZZO = "S"
        self.OK_PREZZO = 1

    def TTDBFIND(self):	# Linea Source Cobol: 4195
        self.W_MODO = 1
        self.W_INDICE_8 = 3

    def INSERT_ELEM_SING(self):	# Linea Source Cobol: 4221
        self.QT_FUNZIONEOFPARTAB_SING = "K1"
        self.CONT_SING = self.IND_CAPI_LETTI
        self.C_MAT_SING = self.C_MAT_A_BARRE_RID
        self.D_MAT_SING = self.D_MAT_MEM
        self.PRIMA_TG_SING = self.PTG_MEM
        self.PREZZO_SING = self.PREZZO_MEM
        self.SKU_SING = SPACES

    def PRZ_INLIT(self):	# Linea Source Cobol: 4288
        print(self.IE_MSG)

    def DELETE_ELEM_SING(self):	# Linea Source Cobol: 4307
        self.QT_FUNZIONEOFPARTAB_SING = "K3"
        self.CONT_SING = self.IND_CAPI_LETTI
        self.C_MAT_SING = self.C_MAT_A_BARRE_RID
        self.ERR_DISP = self.QT_STATOOFPARTAB_SING
        print("ERR QTABEL SING  ",self.ERR_DISP)
        print("ANNULLO PREC")
        print("PER ELEMENTO     ",self.C_MAT_A_BARRE_RID)

    def TRATTA_LETTI(self):	# Linea Source Cobol: 4328
        print(SPACE)
        self.TOT_CAPI_LETTI_1 = self.IND_CAPI_LETTI
        print("- Tot CAPI - ",self.TOT_CAPI_LETTI_1)
        self.TOT_CAPI_NO_GIAC = self.IND_CAPI_NO_GIAC
        print("- No GIAC./PREZZO - ",self.TOT_CAPI_NO_GIAC)
        print(" ")
        print("Vuoi STORNARE ","(SI-NO)")
        self.CONFERMA_STORNO = SPACES
        self.CONFERMA_STORNO = input()
        print(SPACE)
        print("Dammi il CODICE")
        self.COD_IN = SPACES
        self.MEM_COD_IN = SPACES
        print(" . fine lettura")
        print(" @ storno totale")
        self.QT_ADDR_KEYOFPARTAB_SING = 1
        self.QT_LL_KEYOFPARTAB_SING = 8
        self.TRATTA_STORNO()
        self.W_MODO = 5
        self.TTLOCK_T()
        self.LEGGI_PARAMDT()
        self.AGG_DPARAM()
        self.AA_MM_GG_DDT = self.W_FORMATO_INTERNO
        print("Aggiorno  ")
        print("BOLLA n.  ",self.NUMERO_DDT)
        self.INSERISCI_MOVSKU()
        self.S_S_COMMIT()
        self.DIR_VAR_VALUE = SPACE
        self.DIR_VAR_NAME = "RETIS_DIRECTORY"
        print(self.DIR_VAR_NAME)
        self.DIR_VAR_VALUE = input()
        self.USER_VAR_VALUE = SPACE
        self.USER_VAR_NAME = "RETIS_UTENTE"
        print(self.USER_VAR_NAME)
        self.USER_VAR_VALUE = input()
        self.FILE_VAR_NAME = "dd_BARCNEG"
        self.FILE_VAR_VALUE = SPACES
        print(self.FILE_VAR_NAME)
        print(self.FILE_VAR_VALUE)
        self.SCRIVI_RECORD()
        self.QT_ADDR_KEYOFPARTAB_SING = 1
        self.QT_LL_KEYOFPARTAB_SING = 10
        self.INTESTA_FILE_BC()
        self.SCORRI_TAB_SING()
        self.CALL_COMMAND()
        self.SCORRI_TB()
        self.CALL_COMMAND_2()
        self.SCRIVI_BOLLE()
        self.CHIAMA_PRINTDDT()
        self.DIVISA_PRIMO_LETTO = self.DIVISAOFREC_CONFATT
        self.STAMPA_NO_GIAC()
        self.TTUNLOCK()
        self.ALLINEA_BOLLA_ESTERO()
        self.AVANZA_DDT()
        print("premi un tasto per continuare...")
        self.TASTO_INP = input()

    def ALLINEA_BOLLA_ESTERO(self):	# Linea Source Cobol: 4502
        self.INPUT_RIF_TRASF = self.RIF_BOLLA_DDT
        self.INPUT_CONTO_TRASF = self.CONTO_FATTURA_MEM
        self.INPUT_CONTO_TRASF = self.CONTO_IN_R
        print('ERRORE!')
        print(self.OUTPUT_ERR_TRASF)

    def AVANZA_DDT(self):	# Linea Source Cobol: 4521
        self.INPUT_RIF_INTR = self.RIF_BOLLA_DDT
        self.INPUT_CONTO = self.CONTO_FATTURA_MEM
        self.INPUT_CONTO = self.CONTO_IN_R
        self.INPUT_MAG = self.MAG_INPUT_R
        self.INPUT_FLAG = 'E'
        print('AVANZAMENTO NON RIUSCITO')

    def CALL_COMMAND(self):	# Linea Source Cobol: 4543
        self.BUILD_N_DDT = self.NUMERO_DDT
        self.PURGE_N_DDT = self.NUMERO_DDT
        self.FILE_N_DDT = self.NUMERO_DDT
        self.FILE_N_DDT_2 = self.NUMERO_DDT
        self.PRINT_N_DDT = self.NUMERO_DDT
        self.PRINT_N_DDT_2 = self.NUMERO_DDT
        self.ERR_DISP = self.ERR
        print("Errore COMMAND BUILD - P",self.BUILD_N_DDT,SPACE,"-  ",self.ERR_DISP)
        self.ERR_DISP = self.ERR
        print("Errore COMMAND FILE ECQ - P",self.BUILD_N_DDT,SPACE,"-  ",self.ERR_DISP)
        self.JRUNC = 1

    def STAMPA_NO_GIAC(self):	# Linea Source Cobol: 4575
        self.CONTA_RIGHE = 100
        self.CONTA_PAGINE = 1
        self.APRI_STAMPA()
        self.STAMPA_DETT_N_G()
        self.CHIUDI_STAMPA()

    def APRI_STAMPA(self):	# Linea Source Cobol: 4588
        self.LL_RIGA = 136
        self.N_MAX_RIGHE = 9999
        self.PRE_NOME_FILE = "N"
        self.TERM_N_FILE = self.NUMERO_DDT
        self.N_STAMPANTE = 0
        self.COMANDO = "M"
        self.N_RIGA_STAMPA = 66
        self.DATI_RIGA = " MANCA GIAC./PREZZO SU VENDITA "
        self.DATI_RIGA = SPACES

    def SCRIVI(self):	# Linea Source Cobol: 4608
        self.STATO_DISPLAY = self.STATOOFPAR_PRINT
        print("ERRORE QPRINT CON STATO : ",self.STATO_DISPLAY)
        self.DATI_RIGA = SPACES
        self.COMANDO = "S"
        self.N_RIGA_STAMPA = 0

    def INTESTA_PAGINA(self):	# Linea Source Cobol: 4623
        self.DATI_RIGA = SPACE
        self.DATA_T = self.W_FORMATO_GG_MMM_AAAA
        self.NUM_PAG_T = self.W_NUM_TERM
        self.D_PAG_T = "term "
        self.COMANDO = "P"
        self.N_RIGA_STAMPA = 2
        self.N_STAMPANTE = 0
        self.SCRIVI()
        self.CONTO_T = self.CONTO_IN_R
        self.D_CONTO_T = self.D_CONTO_MEM
        self.D_PAG_T = "pag. "
        self.NUM_PAG_T = self.CONTA_PAGINE
        self.COMANDO = "P"
        self.N_RIGA_STAMPA = 3
        self.N_STAMPANTE = 0
        self.SCRIVI()
        self.DATI_RIGA = "     ELENCO MODELLI SENZA GIAC./PREZZO "
        self.N_RIGA_STAMPA = 2
        self.SCRIVI()
        self.N_RIGA_STAMPA = 1
        self.CONTA_RIGHE = 5

    def STAMPA_DETT_N_G(self):	# Linea Source Cobol: 4657
        self.INTESTA_PAGINA()
        self.C_MAT_A_BARRE_RID = self.C_MAT_NO_GIAC(W_INDICE_3)
        self.C_MAT_TRANS_RID = 0
        self.MODELLOOFC_MAT_TRANSITO = self.MODELLOOFC_MAT_A_BARRE
        self.VEST_AOFC_MAT_TRANSITO = self.VESTIBILITAOFC_MAT_A_BARRE
        self.PROGR_ARTOFC_MAT_TRANSITO = self.SOCIETAOFC_MAT_A_BARRE
        self.PEZZO_AOFC_MAT_TRANSITO = self.PEZZOOFC_MAT_A_BARRE
        self.COLOREOFC_MAT_TRANSITO = self.VARIANTE_COLOFC_MAT_A_BARRE
        self.C_MAT_ST = self.C_MAT_TRANS_RID
        self.NOME_MOD_ST = self.D_MAT_NO_GIAC(W_INDICE_3)
        self.NTG_IN = self.TAGLIAOFC_MAT_A_BARRE
        self.TAGLIA_ST = self.FUNCTIONidxtg(NTG_IN)
        self.NOGIAC_ST = self.CAUSALE_NO_GIAC(W_INDICE_3)
        self.NOPRZ_ST = self.CAUSALE_NO_PRZ(W_INDICE_3)
        self.SCRIVI()

    def CHIUDI_STAMPA(self):	# Linea Source Cobol: 4690
        pass

    def CALL_COMMAND_2(self):	# Linea Source Cobol: 4697
        self.WK_VAR_NAME = "dd_PEND"
        self.WK_VAR_VALUE = SPACES
        print(self.WK_VAR_NAME)
        self.WK_VAR_VALUE = input()
        self.LPR_NOME_FILE = self.WK_VAR_VALUE
        self.LPR_NUM_STAMPANTE = self.DESTINO_USCITA

    def CHIAMA_WRITERES(self):	# Linea Source Cobol: 4745
        self.CAPO_CONTO = 0
        self.SOTTO_CONTO = self.NEG_IN
        self.RIF_INTR_WR = self.RIF_BOLLA_DDT
        self.MAGAZZINO_WR = self.MAG_INPUT_R
        self.DEST_WR = self.FLAG_ANACON
        self.CONTO_CLI_WR = self.CONTO_FATTURA_MEM
        self.CONTO_CLI_WR = self.CONTO_IN_R
        self.CONTO_DEST_WR = self.CODICE_CONTO
        self.DIVISA_WR = self.DIVISA_MEM
        self.LISTINO_WR = self.LISTINO_MEM
        self.CAUSALE_WR = "VESD"
        self.DATA_CARICO_WR = 0

    def SCRIVI_BOLLE(self):	# Linea Source Cobol: 4774
        self.W_NOME_DATA_SET = "BOLLE;"
        self.CONTOOFREC_BOLLE = self.CONTO_FATTURA_MEM
        self.CONTOOFREC_BOLLE = self.CONTO_IN_R
        self.RIF_INTERNOOFREC_BOLLE = self.RIF_BOLLA_DDT
        self.MAGAZZINOOFREC_BOLLE = self.MAG_INPUT_R
        self.DATA_NASCITAOFREC_BOLLE = 0
        self.NUM_PRE_FATTOFREC_BOLLE = 0
        self.NUMEROOFREC_BOLLE = 1
        self.VAL_RECOFREC_BOLLE = SPACES
        self.TTDBPUT()
        self.ERR_DISP = self.W_STATUS_WORD_IMAGE
        print("Err. PUT BOLLE  ",self.ERR_DISP)
        print("Per CONTO  ",self.CONTO_IN_R,"   RIF-INTERNO  ",self.RIF_BOLLA_DDT)

    def TRATTA_STORNO(self):	# Linea Source Cobol: 4804
        self.COD_IN = SPACES
        self.COD_IN = input()
        self.IND_CAPI_LETTI = 0
        self.COD_IN_RID = "."
        print("COD non num >> RILEGGERE")
        self.NTG_IN = self.TAGLIAOFC_MAT_A_BARRE
        self.NTG_OUT = self.FUNCTIONidxtg(NTG_IN)
        self.SOC_COM = self.SOCIETAOFC_MAT_A_BARRE
        self.PRE_SOC = 0
        self.SOCIETAOFC_MAT_A_BARRE = self.SOC_COM
        self.QT_FUNZIONEOFPARTAB_ART = "K2"
        print("Manca lettura ")
        self.ART_ELEM_LETTI = self.ART_TAB_LETTI(QT_INDEX_ELEMOFPARTAB_ART)
        print("Taglia non stornabile")
        self.DELETE_ELEM_SING()
        self.DISP_4 = self.QTA_TAGLIA_ELEM(NTG_OUT)
        print("ancora ",self.DISP_4,self.C_MAT_A_BARRE_RID)
        self.IE_IMPORTO_IN = self.PREZZO_TOT
        self.PRZ_INLIT()
        self.ART_TAB_LETTI(QT_INDEX_ELEMOFPARTAB_ART) = self.ART_ELEM_LETTI

    def SCORRI_TB(self):	# Linea Source Cobol: 4863
        self.COMANDI_IGP_TESTA()
        self.VERT_EXP_IGP = 0
        self.ORIZ_EXP_IGP = 0
        self.ROW_IGP = 80
        self.COL_IGP = 100
        self.COMANDI_IGP_TESTA_1()
        self.RIGA_PEND_COM = SPACES
        self.DISP_4 = self.QT_NUM_ELEM_EFFOFPARTAB_SING
        self.SCRIVI_RECORD_IGP()
        self.COMANDI_IGP_FINE_1()
        self.IND_PEND = 0
        self.SCORRI_TB_SING()
        self.SCRIVI_ETICH_VUOTA()

    def SCORRI_TB_SING(self):	# Linea Source Cobol: 4897
        self.COMANDI_IGP_TESTA_1()
        self.RIGA_PENDENTI = SPACES
        self.METTI_VALORI_PEND()
        self.ROW_IGP = 80
        self.RIGA_PEND_COM = self.ANTE_PRIMA_RIGA
        self.SCRIVI_RECORD_IGP()
        self.RIGA_PEND_COM = self.PRIMA_RIGA
        self.SCRIVI_RECORD_IGP()
        self.RIGA_PEND_COM = self.SECONDA_RIGA
        self.SCRIVI_RECORD_IGP()
        self.RIGA_PEND_COM = self.TERZA_RIGA
        self.SCRIVI_RECORD_IGP()
        self.RIGA_PEND_COM = self.QUARTA_RIGA
        self.SCRIVI_RECORD_IGP()
        self.COMANDI_IGP_FINE_1()
        self.IND_PEND = 1
        self.IND_PEND = 0

    def SCRIVI_ETICH_VUOTA(self):	# Linea Source Cobol: 4942
        self.ROW_IGP = 80
        self.COL_IGP = 100
        self.COMANDI_IGP_TESTA_1()
        self.RIGA_PEND_COM = SPACES
        self.SCRIVI_RECORD_IGP()
        self.COMANDI_IGP_FINE_1()

    def METTI_VALORI_PEND(self):	# Linea Source Cobol: 4954
        self.NUMERO_P = self.NUMERO_DDT
        self.ELEMENTO_SINGOLI = self.ELEM_TAB_SING(W_INDICE_3)
        self.CODICE_P = self.C_MAT_SING
        self.NOME_P = self.D_MAT_SING
        print(self.IE_MSG)
        self.DIV_EUR = self.IE_DIVISA_OU
        self.PREZZO_P_E = 0
        self.IE_IMPORTO_IN = self.PREZZO_SING
        print(self.IE_MSG)
        self.DIV_LIT = self.IE_DIVISA_OU
        self.PREZZO_P = 0

    def CERCA_PREZZO_PUBBL(self):	# Linea Source Cobol: 5011
        self.PREZZO_PUBBL = 0
        self.C_MAT_A_BARRE_RID = self.C_MAT_SING
        self.C_MAT_TRANS_RIDOFC_MAT_COM = 0
        self.MODELLOOFC_MAT_TRANSITO = self.MODELLOOFC_MAT_A_BARRE
        self.VEST_AOFC_MAT_TRANSITO = self.VESTIBILITAOFC_MAT_A_BARRE
        self.PROGR_ARTOFC_MAT_TRANSITO = self.SOCIETAOFC_MAT_A_BARRE
        self.PEZZO_AOFC_MAT_TRANSITO = self.PEZZOOFC_MAT_A_BARRE
        self.COLOREOFC_MAT_TRANSITO = 0
        self.TRATTA_MODELLI_DBG()
        self.CERCA_PREZZO_PUBBL_2()

    def TRATTA_MODELLI_DBG(self):	# Linea Source Cobol: 5068
        self.CC_SOCIETA = self.SOCIETA_SIGLA(SOCIETA_MODOFC_MAT_TRANSITO)
        self.CC_C_MAT = self.C_MAT_TRANS_RIDOFC_MAT_COM
        self.CC_LISTINo = 1
        self.CC_TIPO_PREZZO = "V"
        self.SELEZIONA_PREZZO_DBG()
        print("SQLCODE ",self.SQLCODE)
        self.TEST_ERR()
        self.PREZZO_PUBBL = self.CC_PREZZO_DBG

    def SELEZIONA_PREZZO_DBG(self):	# Linea Source Cobol: 5093
        self.BEGIN_RC()
        self.SQLCODE_MEM = self.SQLCODE
        self.S_S_COMMIT()

    def CERCA_PREZZO_PUBBL_2(self):	# Linea Source Cobol: 5189
        self.PREZZO_PUBBL = 0
        self.W_NOME_DATA_SET = "PREZZI;"
        self.W_NOME_CAMPO = "C-MAT;"
        self.TTDBFIND()
        self.W_MODO = 5
        self.TTDBGET_P_PUB()
        self.TTDBGET_P_PUB()
        self.PREZZO_PUBBL = self.PREZZO_VENDITAOFREC_PREZZI_PUB(1)

    def SCORRI_TAB_SING(self):	# Linea Source Cobol: 5215
        self.ELEMENTO_SINGOLI = self.ELEM_TAB_SING(W_INDICE_3)
        print("SCORRI-TAB-SING")
        print(self.C_MAT_SING,SPACE,self.CONT_SING,SPACE,self.D_MAT_SING)
        self.C_MAT_A_BARRE_RID = self.C_MAT_SING
        self.C_MAT_TRANS_RID = 0
        self.MODELLOOFC_MAT_TRANSITO = self.MODELLOOFC_MAT_A_BARRE
        self.VEST_AOFC_MAT_TRANSITO = self.VESTIBILITAOFC_MAT_A_BARRE
        self.PROGR_ARTOFC_MAT_TRANSITO = self.SOCIETAOFC_MAT_A_BARRE
        self.PEZZO_AOFC_MAT_TRANSITO = self.PEZZOOFC_MAT_A_BARRE
        self.MAG_S = self.NEG_IN
        self.NOME_S = self.D_MAT_SING
        self.NOME_F_S = SPACES
        self.COL_F_S = SPACES
        self.SETTORE_S = SPACES
        self.PREZZO_S = 0
        self.BARUNI_S = self.SKU_SING
        self.T_1 = self.T_TAB
        self.T_2 = self.T_TAB
        self.T_3 = self.T_TAB
        self.T_4 = self.T_TAB
        self.T_5 = self.T_TAB
        self.T_6 = self.T_TAB
        self.T_7 = self.T_TAB
        self.T_8 = self.T_TAB
        self.T_9 = self.T_TAB
        self.CERCA_PREZZO_V()
        self.TAGLIA_S = self.TAGLIAOFC_MAT_A_BARRE
        self.NTG_IN = self.TAGLIAOFC_MAT_A_BARRE
        self.W_INDICE_4 = self.FUNCTIONidxtg(NTG_IN)
        self.ESPLODI_TG()

    def INTESTA_FILE_BC(self):	# Linea Source Cobol: 5257
        self.REC_BC = SPACE

    def CERCA_PREZZO_V(self):	# Linea Source Cobol: 5270
        self.W_NOME_DATA_SET = "PREZZI;"
        self.W_NOME_CAMPO = "C-MAT;"
        self.TTDBFIND()
        self.W_MODO = 5
        self.TTDBGET_P()
        self.TTDBGET_P()
        self.PREZZO_MEM = self.PREZZO_VENDITAOFREC_PREZZI(1)

    def SCRIVI_RECORD(self):	# Linea Source Cobol: 5290
        self.ART_ELEM_LETTI = self.ART_TAB_LETTI(W_INDICE_3)
        self.C_MAT_TRANS_RID = 0
        self.MODELLOOFC_MAT_TRANSITO = self.MODELLOOFC_MAT_A_BARRE
        self.VEST_AOFC_MAT_TRANSITO = self.VESTIBILITAOFC_MAT_A_BARRE
        self.PROGR_ARTOFC_MAT_TRANSITO = self.SOCIETAOFC_MAT_A_BARRE
        self.PEZZO_AOFC_MAT_TRANSITO = self.PEZZOOFC_MAT_A_BARRE
        self.COLOREOFC_MAT_TRANSITO = self.VARIANTE_COLOFC_MAT_A_BARRE
        self.INVERTI_QTA()
        self.PREPARA_MOVMAG()
        self.CREA_MOVMAG_P_3()
        self.AGGIORNA_SITPF_P_3()

    def INVERTI_QTA(self):	# Linea Source Cobol: 5337
        pass

    def CERCA_PREZZO(self):	# Linea Source Cobol: 5344
        self.W_NOME_DATA_SET = "ANAMAT;"
        self.W_MODO = 7
        self.TTDBGET()
        print("Inesist. col 0  ",self.C_MAT_A_BARRE_RID)
        self.PREZZO_ANAMAT = self.COSTOOFREC_ANAMAT
        self.CHIAMA_DTVALSTK()
        self.CERCA_PREZZIA()

    def CHIAMA_DTVALSTK(self):	# Linea Source Cobol: 5366
        self.STK_C_MAT = self.C_MAT_TRANS_RID
        self.STK_COLL = self.COLLEZIONEOFREC_ANAMAT
        self.STK_STAGIONE = self.STAGIONEOFREC_ANAMAT
        self.STK_PRZ_LORDO = self.COSTOOFREC_ANAMAT
        self.NOME_IN_3 = "STF"
        self.NOME_IN_3 = "STF"
        self.NOME_IN_3 = "STV"
        self.NOME_IN_5 = self.CONTO_IN_R
        self.STK_NOME = self.NOME_IN_35
        self.PREZZO_MEM = self.STK_PRZ_SCO
        self.CAMBIO_MEM = self.STK_CAMBIO

    def CERCA_PREZZIA(self):	# Linea Source Cobol: 5426
        self.W_NOME_DATA_SET = "PREZZIA;"
        self.W_NOME_CAMPO = "C-MAT;"
        self.TTDBFIND()
        self.W_MODO = 5
        self.TTDBGET_P()
        self.TTDBGET_P()
        self.PREZZO_MEM = self.PREZZO_VENDITAOFREC_PREZZI(1)
        self.CAMBIO_MEM = 0

    def ESPLODI_8_TG(self):	# Linea Source Cobol: 5447
        self.ESPLODI_TG()

    def ESPLODI_TG(self):	# Linea Source Cobol: 5457
        self.TAGLIA_S = self.W_INDICE_4
        self.T_TG()

    def T_TG(self):	# Linea Source Cobol: 5468
        self.TG_OUT_S = self.TG_CAL
        self.TG_OUT_S = "XS"
        self.TG_OUT_S = " S"
        self.TG_OUT_S = " M"
        self.TG_OUT_S = " L"
        self.TG_OUT_S = "XL"

    def TTUNLOCK(self):	# Linea Source Cobol: 5487
        self.W_MODO = 1
        self.W_INDICE_8 = 8

    def TTLOCK_T(self):	# Linea Source Cobol: 5512
        pass

    def TTLOCK(self):	# Linea Source Cobol: 5514
        self.W_INDICE_8 = 5

    def TTDBPUT(self):	# Linea Source Cobol: 5538
        self.W_MODO = 1
        self.W_INDICE_8 = 7

    def TTUPDATE(self):	# Linea Source Cobol: 5566
        self.W_MODO = 1
        self.W_INDICE_8 = 9

    def TTDBGET_P(self):	# Linea Source Cobol: 5593
        pass

    def TTDBGET_P_PUB(self):	# Linea Source Cobol: 5597
        pass

    def DISP_C_MAT(self):	# Linea Source Cobol: 5604
        self.RIGA_DISP = SPACES
        self.IND_4 = 1
        self.METTI_4()
        print(self.RIGA_DISP)

    def METTI_4(self):	# Linea Source Cobol: 5615
        self.DISP_ART(IND_4) = self.TAB_ART(W_INDICE_3)
        self.PARE1(IND_4) = "("
        self.PARE2(IND_4) = ")"
        self.ART_ELEM_LETTI = self.ART_TAB_LETTI(W_INDICE_3)
        self.COM_QTA_DISP = 0
        self.ACCUMULA_QTA()
        self.QTA_DISP(IND_4) = self.COM_QTA_DISP

    def ACCUMULA_QTA(self):	# Linea Source Cobol: 5635
        pass

    def AGG_DPARAM(self):	# Linea Source Cobol: 5643
        self.W_NOME_DATA_SET = "DPARAM;"
        self.AREA_REC_SET = self.REC_PARAM_RID
        self.TTUPDATE()

    def AGG_DPARAM_FITTIZI(self):	# Linea Source Cobol: 5652
        self.W_NOME_DATA_SET = "DPARAM;"
        self.AREA_REC_SET = self.REC_PARAM_FITTIZ_R
        self.TTUPDATE()

    def GET_LOCALITA(self):	# Linea Source Cobol: 5662
        self.LOCALITA_PART_STR = SPACES
        self.LOCALITA_PART_STR = self.DESC_LOC(IND_LOC)

    def CHIAMA_PRINTDDT(self):	# Linea Source Cobol: 5676
        self.MAGAZZINO_DDT = self.MAG_INPUT_R
        self.TIPO_DOC_DDT = 1
        self.TIPO_MOVIMENTO_DDT = 11
        self.CAUSALE_DDT = "VESD"
        self.TIPO_STAMPA_DDT = 1
        self.CLIENTE_DDT = self.CONTO_FATTURA_MEM
        self.CLIENTE_DDT = self.CONTO_IN_R
        self.GET_LOCALITA()
        self.LOC_PART_DDT = self.LOCALITA_PART_STR
        self.TIPO_DATA_SET_DDT = SPACES
        self.D_CONTO_VET = SPACES
        self.NOTE_DDT(1) = SPACES
        self.NOTE_DDT(2) = SPACES
        self.IMPORTO_X_PL = 0
        self.FILE_FAT_DDT = "N"
        self.RIGA_1_DDT = "MITTENTE DEPOSITARIO:MAXIMA SPA C/O DEPOSITO DIFFUSIONE TESSILE"
        self.RIGA_2_DDT = "SRL - VIA SANTI,8 42025 CAVRIAGO (RE) (SCARICO CONTO DEPOSITO)"
        self.RIGA_1_DDT = SPACE
        self.RIGA_2_DDT = SPACE

    def PREPARA_MOVMAG(self):	# Linea Source Cobol: 5749
        self.MOVMAG = LOW-VALUE
        self.VAL_RECOFMOVMAG = SPACES
        self.NUMERO_RIGAOFMOVMAG = 1
        self.Q_DATA_I = self.W_FORMATO_INTERNO
        self.Q_FUNZIONEOFPARGEN = 2
        self.SETTIMANAOFMOVMAG = self.Q_SETTIMANA
        self.RIF_INTERNOOFMOVMAG = self.RIF_BOLLA_DDT
        self.RIF_BOLLA_FORNOFMOVMAG = 0
        self.RIF_ORDINEOFMOVMAG = 0
        self.MOD_IMPUTAZOFMOVMAG = 0
        self.QUANTITAOFMOVMAG = 0
        self.DIVISAOFMOVMAG = SPACES
        self.VAL_RECOFMOVMAG = SPACES
        self.UN_MIS_FATTOFMOVMAG = "NR"
        self.DIVISAOFMOVMAG = "EUR "
        self.C_MATOFMOVMAG = self.C_MAT_TRANS_RID

    def CREA_MOVMAG_P_3(self):	# Linea Source Cobol: 5781
        self.PREZZOOFMOVMAG = self.COSTO_ELEM
        self.COSTO_STDOFMOVMAG = self.PREZZO_ELEM
        self.MOD_IMPUTAZOFMOVMAG = self.CAMBIO_ELEM
        self.C_OPEOFMOVMAG = "VESD"
        self.QTA_TAGLIEOFMOVMAG = self.QTA_TAGLIE_NEG
        self.MAGAZZINOOFMOVMAG = self.MAG_INPUT_R
        self.CONTOOFMOVMAG = self.CONTO_FATTURA_MEM
        self.MOD_IMPUTAZOFMOVMAG = self.CONTO_IN_R
        self.CONTOOFMOVMAG = self.CONTO_IN_R
        self.AREA_REC_SET = self.MOVMAG
        self.W_NOME_DATA_SET = "MOVMAG"
        self.TTDBPUT()
        self.STATO_DISPLAY = self.W_STATUS_WORD_IMAGE
        print("ERR PUT MOVMAG-P3- ",self.STATO_DISPLAY)
        print("PER C-MAT ",self.C_MAT_TRANSITO)

    def AGGIORNA_SITPF_P_3(self):	# Linea Source Cobol: 5817
        self.PARAGGPF = LOW-VALUE
        self.C_MATOFPARAGGPF = self.C_MAT_TRANS_RID
        self.MAGAZZINOOFPARAGGPF = self.MAG_INPUT_R
        self.VALOREOFPARAGGPF = -1
        self.QTA_8OFPARAGGPF = self.QTA_TAGLIE_NEG
        self.F_GIACOFPARAGGPF = 1

    def CARICA_TABELLA(self):	# Linea Source Cobol: 5839
        self.STK_C_MAT = 0
        self.STK_STAGIONE = 0
        self.STK_COLL = 0
        self.STK_PRZ_LORDO = 0
        self.STK_PRIMA_VOLTA = 0
        self.NOME_IN_3 = "STF"
        self.NOME_IN_3 = "STF"
        self.NOME_IN_3 = "STV"
        self.NOME_IN_5 = self.CONTO_IN_R
        self.STK_NOME = self.NOME_IN_35

    def STAMPA_RAPPORTINO(self):	# Linea Source Cobol: 5882
        print('M',self.TAB_ARTOFART_TAB_LETTI(W_INDICE_3))

    def VERIF_MAG(self):	# Linea Source Cobol: 5898
        print("MAG provenienza (3 cifre)")
        self.MAG_INPUT = input()
        print("MAG non numerico")
        print(self.SUGG_MAG_DISP)
        self.MAG_INPUT = SPACE

    def VERIF_F_V(self):	# Linea Source Cobol: 5919
        self.F_V_INPUT = "V"

    def TEST_ERR(self):	# Linea Source Cobol: 5940
        self.SQL_STATUS = self.SQLCODE

    def TEST_ERR_EX(self):	# Linea Source Cobol: 5947
        pass

    def BEGIN_RC(self):	# Linea Source Cobol: 5951
        self.ER_DESCRIZIONE = "BEGIN WORK RC"
        self.TEST_ERR()

    def BEGIN_RC_EX(self):	# Linea Source Cobol: 5955
        pass

    def S_S_COMMIT(self):	# Linea Source Cobol: 5959
        self.ER_DESCRIZIONE = "COMMIT WORK"
        self.TEST_ERR()

    def S_S_COMMIT_EX(self):	# Linea Source Cobol: 5963
        pass

    def COMANDI_IGP_TESTA(self):	# Linea Source Cobol: 6022
        self.REC_PEND = self.COMIGP_PTXSETUP
        self.REC_PEND = self.COMIGP_PTXCFG2
        self.REC_PEND = self.COMIGP_PTXEND

    def COMANDI_IGP_TESTA_1(self):	# Linea Source Cobol: 6033
        self.REC_PEND = self.COMIGP_LISTEN
        self.REC_PEND = self.COMIGP_CREATE
        self.REC_PEND = self.COMIGP_SCALEDOT
        self.REC_PEND = self.COMIGP_ALPHA

    def COMANDI_IGP_FINE_1(self):	# Linea Source Cobol: 6046
        self.REC_PEND = self.COMIGP_STOP
        self.REC_PEND = self.COMIGP_END
        self.REC_PEND = self.COMIGP_EXECUTE
        self.REC_PEND = self.COMIGP_QUIET

    def SCRIVI_RECORD_IGP(self):	# Linea Source Cobol: 6059
        self.REC_PEND = SPACES
        self.TEMP_X_IN = self.ROW_IGP
        self.TOGLI_ZERI()
        self.ROW_X_IGP = self.TEMP_X_OUT
        self.TEMP_X_IN = self.COL_IGP
        self.TOGLI_ZERI()
        self.COL_X_IGP = self.TEMP_X_OUT

    def TOGLI_ZERI(self):	# Linea Source Cobol: 6088
        self.PRIMO_NONZERO = 0
        self.PRIMO_NONZERO = self.K
        self.TEMP_X_OUT = self.TEMP_X_IN

    def INSERISCI_MOVSKU(self):	# Linea Source Cobol: 6108
        self.ELEMENTO_SINGOLI = self.ELEM_TAB_SING(IND_BARUNI)
        self.OUTPUT_VAL_A = '1'
        self.OUTPUT_VAL_B = SPACES
        self.CHIAMA_GETBARUNI()
        self.CONVERTI_BARCODE()
        self.MOVSKU_CMAT = self.C_MAT_TRANS_RID
        self.NTG_IN = self.TAGLIAOFC_MAT_A_BARRE
        self.MOVSKU_TG = self.FUNCTIONidxtg(NTG_IN)
        self.MOVSKU_BARUNI = self.OUTPUT_VAL_B_OK
        self.INSERT_SKU_E_SING()
        self.MOVSKU_SKU = self.MOVSKU_BARUNI(1:8)
        self.MOVSKU_RIF_INTERNO = self.RIF_BOLLA_DDT
        self.MOVSKU_CONTO = self.CONTO_IN_R
        self.MOVSKU_MAG = self.MAG_INPUT_R
        self.MOVSKU_IS_BARUNI_READ = 0
        self.MOVSKU_IS_BARUNI_CERTIFIED = 0
        self.MOVSKU_SKU_FATTURAZIONE = self.MOVSKU_BARUNI(1:8)
        self.ER_DESCRIZIONE = "Insert MOV-SKU "
        self.TEST_ERR()

    def INSERISCI_MOVSKU_EX(self):	# Linea Source Cobol: 6147
        pass

    def CONVERTI_BARCODE(self):	# Linea Source Cobol: 6150
        self.C_MAT_COM = ZEROS
        self.C_MAT_A_BARRE_RID = self.C_MAT_SING
        self.MODELLOOFC_MAT_COM = self.MODELLOOFC_MAT_A_BARRE
        self.VEST_AOFC_MAT_COM = self.VESTIBILITAOFC_MAT_A_BARRE
        self.PEZZO_AOFC_MAT_COM = self.PEZZOOFC_MAT_A_BARRE
        self.PREFISSO_V_FOFC_MAT_COM = self.PREFBC_V_FOFC_MAT_A_BARRE
        self.SOCIETA_MODOFC_MAT_COM = self.SOC_BC_MODOFC_MAT_A_BARRE
        self.COLOREOFC_MAT_COM = self.VARIANTE_COLOFC_MAT_A_BARRE

    def CHIAMA_GETBARUNI(self):	# Linea Source Cobol: 6172
        self.INPUT_VAL = self.C_MAT_SING
        self.INPUT_VAL_B = self.RIF_BOLLA_DDT
        self.INPUT_VAL_C = "READVE3"
        print(self.OUTPUT_VAL_B)

    def CHIAMA_GETBARUNI_EX(self):	# Linea Source Cobol: 6184
        pass

    def INSERT_SKU_E_SING(self):	# Linea Source Cobol: 6187
        self.QT_FUNZIONEOFPARTAB_SING = "K2"
        self.SKU_SING = self.OUTPUT_VAL_B_OK
        self.ELEM_TAB_SING(QT_INDEX_ELEMOFPARTAB_SING)(24:13) = self.SKU_SING
        self.ERR_DISP = self.QT_STATOOFPARTAB_SING
        print("ERR UPDATE QTABEL ",self.ERR_DISP," - INSERISCI-SKU")

    def CARICA_B2C_NO_DT(self):	# Linea Source Cobol: 6232
        self.BEGIN_RC()
        self.SE_SELECT_NEGOZIO_CATEG()
        print("NESSUNA NEGOZIO CATEGORIA TROVATA")
        self.NUM_B2C_NO_DT = 0
        self.NUM_B2C_NO_DT = self.SQLERRD(3)
        self.S_S_COMMIT()

    def SE_SELECT_NEGOZIO_CATEG(self):	# Linea Source Cobol: 6264
        self.ER_DESCRIZIONE = "SELECT NEGOZIO-CATEG  "
        self.TEST_ERR()

    def SE_SELECT_NEGOZIO_CATEG_EX(self):	# Linea Source Cobol: 6269
        pass

    def RIVALUTA_COSTO_ANAMAT(self):	# Linea Source Cobol: 6278
        self.CERCA_B2C_NO_DT()
        self.RICERCA_COSTO_ANAMAT()

    def CERCA_B2C_NO_DT(self):	# Linea Source Cobol: 6285
        self.FLAG_B2C_NO_DT = 'N'
        self.FLAG_B2C_NO_DT = 'S'

    def RICERCA_COSTO_ANAMAT(self):	# Linea Source Cobol: 6299
        self.ANACST_C_MAT = self.ANACST_C_MAT_COM
        self.ANACST_CST_COM = 0
        self.BEGIN_RC()
        self.SE_SELECT_ANAMAT_CST()
        self.ANACST_CST_COM = self.ANACST_CST_STD_2
        self.ANACST_CST_COM = self.ANACST_CST_STD
        self.S_S_COMMIT()

    def SE_SELECT_ANAMAT_CST(self):	# Linea Source Cobol: 6326
        self.ER_DESCRIZIONE = "SELECT ANAMAT_CST   "
        self.TEST_ERR()

    def SE_SELECT_ANAMAT_CST_EX(self):	# Linea Source Cobol: 6331
        pass

    def INIT_PAR_TAB_UNICO_DDT(self):	# Linea Source Cobol: 6340
        self.QT_LL_ELEMOFPAR_TAB_UNICO_DDT = 16
        self.QT_LL_KEYOFPAR_TAB_UNICO_DDT = 4
        self.QT_ADDR_KEYOFPAR_TAB_UNICO_DDT = 1
        self.QT_NUM_ELEM_MAXOFPAR_TAB_UNICO_DDT = 1980
        self.QT_NUM_ELEM_EFFOFPAR_TAB_UNICO_DDT = 0

    def LEGGI_TAB_UNICO_DDT(self):	# Linea Source Cobol: 6350
        self.QT_FUNZIONEOFPAR_TAB_UNICO_DDT = "K2"
        self.PRENDI_DEP_TAB_UNICO_DDT()

    def PRENDI_DEP_TAB_UNICO_DDT(self):	# Linea Source Cobol: 6363
        self.DEP_TAB_UNICO_DDT = self.ELE_TAB_UNICO_DDT(QT_INDEX_ELEMOFPAR_TAB_UNICO_DDT)

    def AGG_TAB_UNICO_DDT(self):	# Linea Source Cobol: 6368
        self.RIMETTI_DEP_TAB_UNICO_DDT()
        self.INS_TAB_UNICO_DDT()

    def RIMETTI_DEP_TAB_UNICO_DDT(self):	# Linea Source Cobol: 6376
        self.ELE_TAB_UNICO_DDT(QT_INDEX_ELEMOFPAR_TAB_UNICO_DDT) = self.DEP_TAB_UNICO_DDT

    def INS_TAB_UNICO_DDT(self):	# Linea Source Cobol: 6381
        self.QT_FUNZIONEOFPAR_TAB_UNICO_DDT = "K1"
        print("AS=",self.TAB_AS," CL=",self.TAB_CL," GIA' INSERITI CON CAPI ",self.TAB_MAX_CAPI)
        print("TAB-UNICO-DDT PIENA >> ALLARGARE")
        self.ERR_DISP = self.QT_STATOOFPAR_TAB_UNICO_DDT
        print("ERR k1 QTABELXL ",self.ERR_DISP," TAB-TAB-UNICO-DDT")

    def MOSTRA_TAB_UNICO_DDT(self):	# Linea Source Cobol: 6407
        self.M_DEP_TAB_UNICO_DDT()
        print(" ")

    def M_DEP_TAB_UNICO_DDT(self):	# Linea Source Cobol: 6418
        self.PRENDI_DEP_TAB_UNICO_DDT()
        print(self.DEP_TAB_UNICO_DDT)
        print("AS = ",self.TAB_ASOFDEP_TAB_UNICO_DDT,"  ","CL = ",self.TAB_CLOFDEP_TAB_UNICO_DDT,"  ","MAX-CAPI = ",self.TAB_MAX_CAPIOFDEP_TAB_UNICO_DDT,"  ")

    def CICLO_DISIMPEGNO(self):	# Linea Source Cobol: 6435
        self.PRENDI_DEP_TAB_UNICO_DDT()
        print(self.DEP_TAB_UNICO_DDT)
        self.MAG_DISIMPEGNA = self.MAG_INPUT
        self.FORN_DISIMPEGNA = self.SOCIETA_INPUT
        self.AS_DISIMPEGNA(1) = self.TAB_AS
        self.CLASSE_DISIMPEGNA(1) = self.TAB_CL
        self.CALL_DISIMPEGNA_MAG()
        print("ERRORE DISIMPEGNO!!!")

if __name__ == "__main__":
    program = Program()
    program.READVE3()   
        