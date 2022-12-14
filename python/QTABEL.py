class QTABEL:
    def __init__(self,par,tab,elem):
        self.PAR = par
        self.TAB = tab
        self.ELEM = elem

        if not self.PAR["FUNZ"] in ["K1","K2","K3","K3","K4","P1"]:
            self.PAR["STATO"] = -4
            return

        if self.PAR["FUNZ"] == "K1" and self.PAR["N-EL-EFF"] < self.PAR["N-EL-MAX"]:
            self.PAR["STATO"] = -1
            return

        if self.PAR["FUNZ"][0] == "K":
            self.TRATTA_K()
        else:
            self.TRATTA_P()

    def TRATTA_K(self):
        self.SW1 = 0
        self.IMIN = 1
        self.IMAX = self.PAR["N-EL-EFF"]
        if self.IMAX == 0 and not self.PAR["FUNZ"][1] == "1":
            self.PAR["STATO"] = -3
            return        
        if self.IMAX > 0:
            self.RICERCA()
        
        if self.PAR["FUNZ"][1] == "1":
            self.INSERISCI()
        else:
            if not self.SW1 == 0:
                self.PAR["STATO"] = -3
                return        
            else:
                if self.PAR["FUNZ"][1] == "3":
                    self.CANCELLA()
                else:
                    self.PAR["IND-EL"] = self.IT

    def RICERCA(self):
        self.PMIN = (self.IMIN - 1) * self.PAR["EL-EL"] + self.PAR["ADDR-K"]
        self.PMAX = (self.IMAX - 1) * self.PAR["EL-EL"] + self.PAR["ADDR-K"]
        if self.ELEM[self.PAR["ADDR-K"]:self.PAR["LL-K"]] == self.TAB[self.PMIN:self.PAR["LL-K"]]:
            self.IT = self.IMIN
            return
        if self.ELEM[self.PAR["ADDR-K"]:self.PAR["LL-K"]] == self.TAB[self.PMAX:self.PAR["LL-K"]]:
            self.IT = self.IMAX
            return
        if self.ELEM[self.PAR["ADDR-K"]:self.PAR["LL-K"]] < self.TAB[self.PMIN:self.PAR["LL-K"]]:
            self.IT = self.IMIN
            self.SW1 = 2
            return
        if self.ELEM[self.PAR["ADDR-K"]:self.PAR["LL-K"]] < self.TAB[self.PMAX:self.PAR["LL-K"]]:
            self.IT = self.IMAX
            self.SW1 = 3
            return
        
        self.IT = (self.IMAX + self.IMIN) / 2
        self.IT1 = 0
        self.POS = (self.IT -1) * self.PAR["EL-EL"] + self.PAR["ADDR-K"]
        while (True):
            if ( self.ELEM[self.PAR["ADDR-K"]:self.PAR["LL-K"]] == self.TAB[self.POS:self.PAR["LL-K"]]) or self.IT == self.IT1 :
                break
            self.CERCA()
        if not (self.ELEM[self.PAR["ADDR-K"]:self.PAR["LL-K"]] == self.TAB[self.POS:self.PAR["LL-K"]] ):
            self.SW1  = 1

    def CERCA(self):
        if self.ELEM[self.PAR["ADDR-K"]:self.PAR["LL-K"]] > self.TAB[self.POS:self.PAR["LL-K"]]:
            self.IMIN = self.IT
        else:
            self.IMAX = self.IT
        self.IT1 = self.IT
        self.IT = (self.IMAX + self.IMIN) / 2
        self.POS = (self.IT -1) * self.PAR["EL-EL"] + self.PAR["ADDR-K"]


    def INSERISCI(self):
        if self.SW1 == 0 and not self.PAR["N-EL-EFF"] == 0:
            self.PAR["STATO"] = -2
            return            
        if self.PAR["N-EL-EFF"] == 0:
            self.TAB[0:self.PAR["EL-EL"]] = self.ELEM[0:self.PAR["EL-EL"]]
            self.PAR["IND-EL"] = 1
        else:
            if self.SW1 == 2:
                for J in range(self.PAR["N-EL-EFF"],0,-1):
                    self.POS = (J - 1) * self.PAR["LL-EL"] + 1
                    self.POS1 = J * self.PAR["LL-EL"] + 1
                    self.TAB[self.POS1:self.PAR["LL-EL"]] = self.TAB[self.POS:self.PAR["LL-EL"]]                
                self.TAB[0:self.PAR["LL-EL"]]   = self.ELEM[0:self.PAR["LL-EL"]]  
                self.PAR["IND-EL"] = 1
            elif self.SW1 == 3:
                self.POS = self.PAR["N-LL-EL"] * self.PAR["LL-EL"] + 1
                self.TAB[self.POS:self.PAR["LL-EL"]]   = self.ELEM[0:self.PAR["LL-EL"]]
                self.PAR["IND-EL"] = self.PAR["N-EL-EFF"] + 1
            else:
                for J in range(self.PAR["N-EL-EFF"],self.IT,-1):
                    self.POS = (J - 1) * self.PAR["LL-EL"] + 1
                    self.POS1 = J * self.PAR["LL-EL"] + 1
                    self.TAB[self.POS1:self.PAR["LL-EL"]] = self.TAB[self.POS:self.PAR["LL-EL"]]                
                self.TAB[self.POS:self.PAR["LL-EL"]]   = self.ELEM[0:self.PAR["LL-EL"]]  
                self.PAR["IND-EL"] = self.IT + 1


    def CANCELLA(self):
        if not self.IT == self.PAR["N-EL-EFF"]:
            self.LL_TAB = self.PAR["LL-EL"] * (self.PAR["N-EL-EFF"] - self.IT)
            self.POS = self.IT * self.PAR["LL-EL"] + 1
            self.POS1 = (self.IT - 1) * self.PAR["LL-EL"] + 1
            self.TAB[self.POS1:self.LL_TAB] = self.TAB[self.POS:self.LL_TAB]
        self.PAR["N-EL-EFF"] = self.PAR["N-EL-EFF"] - 1 

    def TRATTA_P(self):
        if self.PAR["IND-EL"] < 1 or self.PAR["IND-EL"] > self.PAR["N-EL-EFF"]:
            self.PAR["STATO"] = -1
            return
        self.IT = self.PAR["IND-EL"]
        self.CANCELLA()


