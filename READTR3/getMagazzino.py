import sys
import re
import config

sys.path.append(r'/home/app/intranet3/bin/4cobol')
import get_param
# solo per compatibilta con python 2.7
input = raw_input

def getMagazzino(parms):
 
            MAG_DEST_CDEP = False
            MAG_PAR = parms['MAG_PAR']
            if (MAG_PAR == "CDP"):
                    while (True):
                        if parms['AZIONE'] == "USCITA":
                            print("Magazzino destinazione c/dep ")
                        else:
                            print("Magazzino provenienza c/dep ")
                        MAG_INPUT = input()
                        if not (re.match(r"^\d+$",MAG_INPUT)):
                            print("Magazzino non numerico")
                            continue
                        MAG_IN = int(MAG_INPUT)

                        # 529 PERFORM VERIFICA-CDEP-MAG THRU EX-VERIFICA-CDEP-MAG
                        MAGS = VERIFICA_CDEP_MAG()
                        if not (MAG_IN in MAGS):
                            print("Magazzino NON C/DEP errato: ")
                            continue
                        MAG_PAR = MAG_INPUT
                        MAG_DEST_CDEP = True
                        break
            
            if parms['AZIONE'] == 'USCITA':
                MAGS = LISTA_MAG()
                while(True):
                    MESSAGGIO_MAG = "Magazzino provenienza ({} Cdep)".format(",".join(MAGS))
                    print("MESSAGGIO-MAG = {}".format(MESSAGGIO_MAG))
                    MAG_IN = input()
                    if not (re.match(r"^\d+$",MAG_IN)):
                        print("Magazzino non numerico")
                        continue
                    
                    MAG_IN = int(MAG_IN)

                    # if not (MAG_IN in VERIFICA_CDEP_MAG()):
                    #     print("1.Magazzino errato: ")
                    #     continue
               
                    if MAG_PAR == 7 and MAG_IN == 2:
                        print("2.Magazzino errato")
                        continue

                    if MAG_IN in [4,1] and (MAG_PAR in [ 6, 8, 3] and MAG_DEST_CDEP == False) :
                        print("3.Magazzino errato")
                        continue

                    if MAG_IN == MAG_PAR:
                        print("4.Magazzino Part/Dest Uguali !!")
                        continue
                    break

                parms["MAGAZZINO_PARTENZA"] = MAG_IN
                parms["MAGAZZINO_DESTINAZIONE"] = int(MAG_PAR)

            if AZIONE == 'RIENTRO':
                while(True):
                    print("Magazzino destinazione  (6,7)",)
                    MAG_IN = input()
                    if not (re.match(r"^\[6,7]$",MAG_IN)):
                        print("Magazzino errato ")
                        continue
                    parms["MAGAZZINO_DESTINAZIONE"] = int(MAG_IN)
                    parms["MAGAZZINO_PARTENZA"] = int(MAG_PAR)
                    


def LISTA_MAG():
    pard = config.configura_batch()
    pard['cobol_input'] = '{:<}{:<}{:02d}'.format(
        'magazzini'.ljust(50),                
        "magazzini_sede_pf".ljust(50),
        3
    )
    MAGS = []
    for T in range(1,16):
        get_param.get_param_multi(pard,verbose=True)
        NUM_VALUES_CDEP = int(pard['cobol_output'][2:4])
        MAG_SEDE = pard['cobol_output'][4:]

        def gen_intervalli(inizio,passo,volte):
            step = 0
            while(step < volte):
                yield inizio+step*passo,inizio+step*passo + passo
                step += 1

        MAG_SEDE = [int(MAG_SEDE[da:a]) for (da,a) in list(gen_intervalli(0,3,NUM_VALUES_CDEP))]
        if T in MAG_SEDE:
            MAGS.append(str(T))
    return MAGS



def VERIFICA_CDEP_MAG():
    pard = config.configura_batch()
    pard['cobol_input'] = '{:<}{:<}{:02d}'.format(
        'MAG_CONTO_DEP'.ljust(50),
        'deposito_pf'.ljust(50),
        3
    )
    get_param.get_param_multi(pard,verbose=True)
    NUM_VALUES_CDEP = int(pard['cobol_output'][2:4])
    MAG_CDEP = pard['cobol_output'][4:]

    def gen_intervalli(inizio,passo,volte):
        step = 0
        while(step < volte):
            yield inizio+step*passo,inizio+step*passo + passo
            step += 1

    MAG_CDEP = [int(MAG_CDEP[da:a]) for (da,a) in list(gen_intervalli(0,3,NUM_VALUES_CDEP))]
    return MAG_CDEP



if __name__ == "__main__":
    MAG_CDEP = VERIFICA_CDEP_MAG()
    print(MAG_CDEP)

    MAGS = LISTA_MAG()
    print(MAGS)
    AZIONE = 'USCITA'
    MAG_PAR="CDP"
    

    MAG_PAR= 3 # 'CDP'
    parms = {}
    getMagazzino(parms,MAG_PAR,AZIONE)
    print(parms)
