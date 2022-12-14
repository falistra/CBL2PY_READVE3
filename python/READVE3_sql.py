
# Cobol source line :  3155
SQL0 = """                                                  
       
               SELECT 
                   P.prezzo
                INTO :CC-PREZZO-DBG
                FROM prezzi_modelli_dbg P
                JOIN anagrafica_modelli_dbg M
                    ON  (M.SOCIETA = P.SOCIETA)
                    AND (P.MODELLO = M.MODELLO)
                JOIN anagrafica_modelli_barcode_negozio_dbg B
                    ON  (B.SOCIETA = P.SOCIETA) 
                    AND (P.MODELLO = B.MODELLO)
            
                WHERE
                    P.modello = :CC-C-MAT 
                    AND M.societa = :CC-SOCIETA                         
                    AND P.f_listino_rif = :CC-LISTINO                   
                    AND P.tipo_prezzo = :CC-TIPO-PREZZO                 
       
               """
# Cobol source line :  3947
SQL1 = """                                                      
              BEGIN WORK RC                                             
           """
# Cobol source line :  3957
SQL2 = """                                                      
              COMMIT WORK                                               
           ."""
# Cobol source line :  4142
SQL3 = """                                                      
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
           """
# Cobol source line :  4279
SQL4 = """   
                BULK SELECT NEGOZIO                                     
                INTO :TAB-B2C-NO-DT                                     
                FROM NEGOZIO_ANAG_CATEGORIA 
                   JOIN NEGOZIO_CATEGORIA USING (ID_CATEGORIA)
                   where DESC_CATEGORIA = 'NEGOZI_ITALIA_B2C_SOC' 
                   order by NEGOZIO                                     
           """
# Cobol source line :  4348
SQL5 = """                                                      
                SELECT CST_STD, CST_STD_2                               
                INTO :ANACST-CST-STD, :ANACST-CST-STD-2                 
                FROM ANAMAT_CST                                         
                WHERE C_MAT   = :ANACST-C-MAT                           
           """