type Patente = String
type Desgaste = Float
type Fecha = (Int, Int, Int)



data Auto = UnAuto { patente :: Patente,
                     desgasteDeLlantas :: [Desgaste],
                     rpm :: Int,
                     temperaturaAgua :: Int,
                     ultimoArreglo :: Fecha
                   } deriving (Show)


costoDeReparacion :: Patente -> Auto -> Int
costoDeReparacion unaPatente unAuto 
 | patenteTieneSieteDigitos unaPatente unAuto = 12500
 | patenteEstaEntreDJyNB unaPatente unAuto && patenteTerminaEn4 unaPatente unAuto = 3 * (length unaPatente)
 | patenteEstaEntreDJyNB unaPatente unAuto = 20000
 | otherwise = 15000

patenteTieneSieteDigitos :: Patente -> Bool
patenteTieneSieteDigitos unaPatente = length unaPatente == 7

patenteTieneSieteDigitos :: Patente -> Bool
patenteEstaEntreDJyNB unaPatente = estaEntreDyN (head unaPatente) && estaEntreJyB (tail (take 2 unaPatente))

estaEntreDyN :: String -> Bool
estaEntreDyN unaLetra = elem unaLetra entreDyN

estaEntreJyB :: String -> Bool
estaEntreJyB unaLetra = elem unaLetra entreJyB

entreDyN :: [char]
entreDyN = ['D'..'N']

entreJyB :: [char]
entreJyB = ['J'..'B']

patenteTerminaEn4 :: Patente -> Bool
patenteTerminaEn4 unaPatente =  drop ((length unaPatente) - 1) unaPatente == 4